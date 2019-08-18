package ifc;

import addr.addrEmpty;
import addr.addrType;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
import pack.packHolder;
import user.userHelping;
import util.bits;
import util.cmds;
import util.counter;
import util.debugger;
import util.logger;
import util.state;

/**
 * itu q931 (isdn) encapsulation handler
 *
 * @author matecsaba
 */
public class ifcIsdn implements ifcUp, ifcDn {

    /**
     * size of header
     */
    public final static int size = 4;

    /**
     * terminal mode
     */
    public int dataMode = dataMoDTE;

    /**
     * dte terminal
     */
    public final static int dataMoDTE = 0x02;

    /**
     * dce terminal
     */
    public final static int dataMoDCE = 0x00;

    /**
     * current keepalive interval (0=disabled)
     */
    public int keepaliveInterval = 5;

    /**
     * last known state
     */
    public state.states lastState = state.states.down;

    /**
     * time when last keepalive received
     */
    public long lastRxKeep = 0;

    /**
     * transmit sequence number
     */
    public int sequenceTx = 0;

    /**
     * receive sequence number
     */
    public int sequenceRx = 0;

    /**
     * counter of this interface
     */
    public counter cntr = new counter();

    /**
     * server that handler received packets
     */
    public ifcUp upper = new ifcNull();

    /**
     * server that sends our packets
     */
    public ifcDn lower = new ifcNull();

    private Timer keepTimer;

    /**
     * get counter
     *
     * @return counter
     */
    public counter getCounter() {
        return cntr;
    }

    /**
     * set parent
     *
     * @param parent parent
     */
    public void setParent(ifcDn parent) {
        lower = parent;
    }

    /**
     * close interface
     */
    public void closeUp() {
        setState(state.states.close);
        upper.closeUp();
    }

    /**
     * close interface
     */
    public void closeDn() {
        setState(state.states.close);
        lower.closeDn();
    }

    /**
     * flap interface
     */
    public void flapped() {
    }

    /**
     * set upper layer
     *
     * @param server upper layer
     */
    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
        setState(lastState);
    }

    /**
     * get state
     *
     * @return state
     */
    public state.states getState() {
        return lastState;
    }

    /**
     * set filter
     *
     * @param promisc promiscous mode
     */
    public void setFilter(boolean promisc) {
    }

    /**
     * get hw address
     *
     * @return hw address
     */
    public addrType getHwAddr() {
        return new addrEmpty();
    }

    /**
     * create new instance
     */
    public ifcIsdn() {
        restartTimer(false);
    }

    /**
     * set state
     *
     * @param stat state
     */
    public void setState(state.states stat) {
        stat = state.toForceable(stat);
        restartTimer(false);
    }

    /**
     * get mtu size
     *
     * @return mtu size
     */
    public int getMTUsize() {
        return lower.getMTUsize() - size;
    }

    /**
     * get bandwidth
     *
     * @return bandwidth
     */
    public long getBandwidth() {
        return lower.getBandwidth();
    }

    public String toString() {
        return "isdn on " + lower;
    }

    /**
     * get help text
     *
     * @param l storage
     */
    public static void getHelp(userHelping l) {
        l.add("2 3     keepalive                   keepalive timer");
        l.add("3 .       <num>                     time in seconds");
        l.add("2 3     mode                        my line mode");
        l.add("3 .       dce                       this side is dce");
        l.add("3 .       dte                       this side is dte");
    }

    /**
     * get configuration
     *
     * @param l storage
     * @param beg beginning
     */
    public void getConfig(List<String> l, String beg) {
        l.add(beg + "keepalive " + keepaliveInterval);
        String a;
        switch (dataMode) {
            case dataMoDCE:
                a = "dce";
                break;
            case dataMoDTE:
                a = "dte";
                break;
            default:
                a = "unknown=" + dataMode;
                break;
        }
        l.add(beg + "mode " + a);
    }

    /**
     * do configuration
     *
     * @param cmd command
     */
    public void doConfig(cmds cmd) {
        String a = cmd.word();
        if (a.equals("keepalive")) {
            keepaliveInterval = bits.str2num(cmd.word());
            restartTimer(false);
            return;
        }
        if (a.equals("mode")) {
            a = cmd.word();
            int i = 0;
            if (a.equals("dce")) {
                i = dataMoDCE;
            }
            if (a.equals("dte")) {
                i = dataMoDTE;
            }
            dataMode = i;
            return;
        }
        cmd.badCmd();
    }

    /**
     * undo configuration
     *
     * @param cmd command
     */
    public void unConfig(cmds cmd) {
        cmd.badCmd();
    }

    /**
     * setup timer thread
     *
     * @param shutdown set true to shut down
     */
    public void restartTimer(boolean shutdown) {
        try {
            keepTimer.cancel();
        } catch (Exception e) {
        }
        keepTimer = null;
        if (lastState == state.states.admin) {
            return;
        }
        if (shutdown) {
            return;
        }
        if (keepaliveInterval < 1) {
            return;
        }
        keepTimer = new Timer();
        ifcIsdnTxKeep task = new ifcIsdnTxKeep(this);
        keepTimer.schedule(task, 500, keepaliveInterval * 1000);
    }

    private void clearState() {
        lastRxKeep = 0;
        sequenceRx = 0;
        sequenceTx = 0;
    }

    /**
     * check peer state from timers
     *
     * @param force try to force line protocol
     * @return true if state changed
     */
    public boolean checkPeerState(state.states force) {
        state.states stat = state.states.down;
        if (bits.getTime() - lastRxKeep < keepaliveInterval * 6000) {
            stat = state.states.up;
        }
        if (keepaliveInterval < 1) {
            stat = state.states.up;
        }
        if (state.toForceable(force) == state.states.admin) {
            stat = state.states.admin;
        }
        if (lastState == stat) {
            return false;
        }
        if (stat != state.states.up) {
            clearState();
        }
        lastState = stat;
        if (debugger.ifcIsdnEvnt) {
            logger.debug("line proto=" + state.conv2string(stat));
        }
        if (stat != state.states.up) {
            lower.flapped();
        }
        cntr.stateChange(stat);
        upper.setState(stat);
        return true;
    }

    /**
     * received packet
     *
     * @param pck packet
     */
    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        int i = pck.msbGetW(0); // sapi tei
        if ((i & 0x001) != 0x0001) {
            cntr.drop(pck, counter.reasons.badAddr);
            return;
        }
        int o = pck.getByte(2); // command
        if ((o & 1) == 0) {
            i = pck.getByte(3);
            sequenceTx = o >>> 1;
            sequenceRx = ((i >>> 1) + 1) & 0x7f;
            if ((i & 1) != 0) {
                sendKeepalive();
            }
            if (debugger.ifcIsdnEvnt) {
                logger.debug("rx i nr=" + sequenceRx + " ns=" + sequenceTx);
            }
            pck.getSkip(4);
            if (recvCmd(pck)) {
                sendAck();
            }
            return;
        }
        switch (o & 0xef) {
            case 0x01: // rr
                sequenceTx = pck.getByte(3) >>> 1;
                lastRxKeep = bits.getTime();
                checkPeerState(state.states.up);
                if (debugger.ifcIsdnEvnt) {
                    logger.debug("rx rr nr=" + sequenceTx);
                }
                break;
            case 0x05: // rnr
                sequenceTx = pck.getByte(3) >>> 1;
                lastRxKeep = bits.getTime();
                if (debugger.ifcIsdnEvnt) {
                    logger.debug("rx rnr nr=" + sequenceTx);
                }
                break;
            case 0x09: // rej
                sequenceTx = pck.getByte(3) >>> 1;
                if (debugger.ifcIsdnEvnt) {
                    logger.debug("rx rej nr=" + sequenceTx);
                }
                break;
            case 0x03: // ui
                pck.getSkip(3);
                upper.recvPack(pck);
                break;
            case 0x6f: // sabme
                clearState();
                lastRxKeep = bits.getTime();
                checkPeerState(state.states.up);
                if (debugger.ifcIsdnEvnt) {
                    logger.debug("rx sabme");
                }
                sendAck();
                break;
            case 0x0f: // dm
                clearState();
                checkPeerState(state.states.down);
                if (debugger.ifcIsdnEvnt) {
                    logger.debug("rx dm");
                }
                break;
            case 0x43: // disc
                clearState();
                checkPeerState(state.states.down);
                if (debugger.ifcIsdnEvnt) {
                    logger.debug("rx disc");
                }
                break;
            case 0x63: // ua
                lastRxKeep = bits.getTime();
                checkPeerState(state.states.up);
                if (debugger.ifcIsdnEvnt) {
                    logger.debug("rx ua");
                }
                break;
            case 0x87: // frmr
                if (debugger.ifcIsdnEvnt) {
                    logger.debug("rx frmr");
                }
                break;
            case 0xaf: // xid
                if (debugger.ifcIsdnEvnt) {
                    logger.debug("rx xid");
                }
                break;
            default:
                break;
        }
    }

    private boolean recvCmd(packHolder pck) {
        int pi = pck.getByte(0); // prot
        int i = pck.getByte(1); // ref len
        pck.getSkip(2);
        int cr = -1;
        switch (i) {
            case 0:
                cr = -1;
                break;
            case 1:
                cr = pck.getByte(0);
                break;
            case 2:
                cr = pck.msbGetW(0);
                break;
            case 3:
                cr = pck.msbGetD(0) >>> 8;
                break;
            case 4:
                cr = pck.msbGetD(0);
                break;
        }
        pck.getSkip(i);
        int typ = pck.getByte(0); // type
        pck.getSkip(1);
        int ch = -1;
        for (;;) {
            if (pck.dataSize() < 1) {
                break;
            }
            i = pck.getByte(0);
            pck.getSkip(1);
            int len = 0;
            if ((i & 0x80) == 0) {
                len = pck.getByte(0);
                pck.getSkip(1);
            }
            switch (i) {
                case 24:
                    ch = pck.getByte(2) & 0x1f;
                    break;
            }
            pck.getSkip(len);
        }
        if (debugger.ifcIsdnEvnt) {
            logger.debug("rx i prt=" + pi + " ref=" + cr + " chan=" + ch
                    + " type=" + type2string(typ));
        }
        if (typ != 5) {
            return false;
        }
        pck.clear();
        putChan(pck, ch);
        putHead(pck, cr, 2);
        sendInfo(pck);
        pck.clear();
        putChan(pck, ch);
        putHead(pck, cr, 7);
        sendInfo(pck);
        return false;
    }

    private static void putHead(packHolder pck, int r, int t) {
        pck.putByte(0, 8); // prot
        pck.putByte(1, 2); // len
        pck.msbPutW(2, r);
        pck.putByte(4, t);
        pck.putSkip(5);
        pck.merge2beg();
        if (debugger.ifcIsdnEvnt) {
            logger.debug("tx i ref=" + r + " type=" + type2string(t));
        }
    }

    private static void putChan(packHolder pck, int i) {
        pck.putByte(0, 24); // type
        pck.putByte(1, 3); // len
        pck.msbPutW(2, 0xa983); // encoding
        pck.putByte(4, i | 0x80); // chan
        pck.putSkip(5);
        pck.merge2beg();
    }

    private static String type2string(int i) {
        switch (i) {
            case 0x00:
                return "national message";
            case 0x01:
                return "alerting";
            case 0x02:
                return "call proceeding";
            case 0x07:
                return "connect";
            case 0x0f:
                return "connect ack";
            case 0x03:
                return "progress";
            case 0x05:
                return "setup";
            case 0x0d:
                return "setup ack";
            case 0x26:
                return "resume";
            case 0x2e:
                return "resume ack";
            case 0x22:
                return "resume rej";
            case 0x25:
                return "suspend";
            case 0x2d:
                return "suspend ack";
            case 0x21:
                return "suspend rej";
            case 0x20:
                return "user info";
            case 0x45:
                return "disconnect";
            case 0x4d:
                return "release";
            case 0x5a:
                return "release done";
            case 0x46:
                return "restart";
            case 0x4e:
                return "restart ack";
            case 0x60:
                return "segment";
            case 0x79:
                return "congestion control";
            case 0x7b:
                return "information";
            case 0x6e:
                return "notify";
            case 0x7d:
                return "status";
            case 0x75:
                return "status enquery";
            default:
                return "unknown=" + i;
        }
    }

    private void sendInfo(packHolder pck) {
        pck.putByte(0, dataMode ^ 2); // addr
        pck.putByte(1, 0x01); // addr
        pck.putByte(2, sequenceTx << 1); // seq
        pck.putByte(3, sequenceRx << 1); // seq
        pck.putSkip(4);
        if (debugger.ifcIsdnEvnt) {
            logger.debug("tx ua");
        }
        pck.merge2beg();
        cntr.tx(pck);
        lower.sendPack(pck);
        sequenceTx++;
    }

    /**
     * send packet
     *
     * @param pck packet
     */
    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        if (lastState != state.states.up) {
            cntr.drop(pck, counter.reasons.notUp);
            return;
        }
        pck.putStart();
        pck.putByte(0, dataMode); // addr
        pck.putByte(1, 0x01); // addr
        pck.putByte(2, 0x03); // ui
        pck.putSkip(3);
        pck.merge2beg();
        lower.sendPack(pck);
    }

    /**
     * send keepalive packet
     */
    protected void sendAck() {
        packHolder pck = new packHolder(true, true);
        pck.clear();
        pck.putStart();
        pck.putByte(0, dataMode); // addr
        pck.putByte(1, 0x01); // addr
        pck.putByte(2, 0x73); // ua
        pck.putSkip(3);
        if (debugger.ifcIsdnEvnt) {
            logger.debug("tx ua");
        }
        pck.merge2beg();
        cntr.tx(pck);
        lower.sendPack(pck);
    }

    /**
     * send keepalive packet
     */
    protected void sendKeepalive() {
        packHolder pck = new packHolder(true, true);
        pck.clear();
        pck.putStart();
        pck.putByte(0, dataMode); // addr
        pck.putByte(1, 0x01); // addr
        if (lastState != state.states.up) {
            pck.putByte(2, 0x7f); // sabme
            pck.putSkip(3);
            if (debugger.ifcIsdnEvnt) {
                logger.debug("tx sabme");
            }
        } else {
            pck.putByte(2, 0x01); // rr
            pck.putByte(3, sequenceRx << 1); // seq
            pck.putSkip(4);
            if (debugger.ifcIsdnEvnt) {
                logger.debug("tx rr nr=" + sequenceRx);
            }
        }
        pck.merge2beg();
        cntr.tx(pck);
        lower.sendPack(pck);
    }

}

class ifcIsdnTxKeep extends TimerTask {

    private ifcIsdn lower;

    public ifcIsdnTxKeep(ifcIsdn parent) {
        lower = parent;
    }

    public void run() {
        try {
            if (state.toForceable(lower.lastState) != state.states.up) {
                cancel();
                return;
            }
            lower.sendKeepalive();
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}
