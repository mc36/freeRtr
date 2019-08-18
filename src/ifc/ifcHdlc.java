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
 * cisco hdlc encapsulation handler
 *
 * @author matecsaba
 */
public class ifcHdlc implements ifcUp, ifcDn {

    /**
     * size of header
     */
    public final static int size = 2;

    private final static int keepSize = 22;

    /**
     * ethertype of keepalives
     */
    public final static int typeKeep = 0x8035;

    /**
     * current keepalive interval (0=disabled)
     */
    public int keepaliveInterval = 5;

    /**
     * last known state
     */
    public state.states lastState = state.states.down;

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

    /**
     * transmit sequence number
     */
    public int sequenceTx = 0;

    /**
     * receive sequence number
     */
    public int sequenceRx = 0;

    /**
     * time when last keepalive received
     */
    public long lastRxKeep = 0;

    /**
     * time when last keepalive sent
     */
    public int lastTxKeep = 0;

    private Timer keepTimer;

    public counter getCounter() {
        return cntr;
    }

    public void setParent(ifcDn parent) {
        lower = parent;
    }

    public void closeUp() {
        setState(state.states.close);
        upper.closeUp();
    }

    public void closeDn() {
        setState(state.states.close);
        lower.closeDn();
    }

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

    public state.states getState() {
        return lastState;
    }

    public void setFilter(boolean promisc) {
    }

    public addrType getHwAddr() {
        return new addrEmpty();
    }

    /**
     * create new instance
     */
    public ifcHdlc() {
        restartTimer(false);
    }

    public void setState(state.states stat) {
        stat = state.toForceable(stat);
        if (checkPeerState(stat)) {
            return;
        }
        restartTimer(false);
    }

    public int getMTUsize() {
        return lower.getMTUsize() - size;
    }

    public long getBandwidth() {
        return lower.getBandwidth();
    }

    public String toString() {
        return "hdlc on " + lower;
    }

    /**
     * get help text
     *
     * @param l storage
     */
    public static void getHelp(userHelping l) {
        l.add("2 3     keepalive                   keepalive timer");
        l.add("3 .       <num>                     time in seconds");
        l.add("2 3     addrctrl                    address and control field");
        l.add("3 .       <num>                     value in hex");
    }

    /**
     * get configuration
     *
     * @param l storage
     * @param beg beginning
     */
    public void getConfig(List<String> l, String beg) {
        l.add(beg + "keepalive " + keepaliveInterval);
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
            checkPeerState(state.states.up);
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
            lastRxKeep = 0;
            sequenceRx = 0;
        }
        lastState = stat;
        if (debugger.ifcHdlcEvnt) {
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
        ifcHdlcTxKeep task = new ifcHdlcTxKeep(this);
        keepTimer.schedule(task, 500, keepaliveInterval * 1000);
    }

    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        if (pck.dataSize() < 2) {
            cntr.drop(pck, counter.reasons.tooSmall);
            return;
        }
        if (pck.msbGetW(0x02) != typeKeep) {
            if (lastState != state.states.up) {
                cntr.drop(pck, counter.reasons.notUp);
                return;
            }
            pck.getSkip(size);
            upper.recvPack(pck);
            return;
        }
        int gotCmd = pck.msbGetD(0x04); // command
        int gotTsq = pck.msbGetD(0x08); // tx sequence
        int gotRsq = pck.msbGetD(0x0c); // rx sequence
        int gotTim = pck.msbGetD(0x12); // time
        if (debugger.ifcHdlcEvnt) {
            logger.debug("rx keepalive cmd=" + gotCmd + " txSeq=" + gotTsq + " rxSeq=" + gotRsq + " tim=" + gotTim);
        }
        if (gotCmd != 2) {
            cntr.drop(pck, counter.reasons.badCmd);
            return;
        }
        sequenceRx = gotTsq;
        if (gotRsq != sequenceTx) {
            cntr.drop(pck, counter.reasons.badRxSeq);
            return;
        }
        lastRxKeep = bits.getTime();
        checkPeerState(state.states.up);
    }

    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        if (lastState != state.states.up) {
            cntr.drop(pck, counter.reasons.notUp);
            return;
        }
        pck.putStart();
        pck.msbPutW(0x00, 0x0f00); // address
        pck.putSkip(size);
        pck.merge2beg();
        lower.sendPack(pck);
    }

    /**
     * send keepalive packet
     */
    protected void sendKeepalive() {
        sequenceTx++;
        lastTxKeep = ((int) bits.getTime()) & 0x7fffffff;
        checkPeerState(state.states.up);
        packHolder pck = new packHolder(true, true);
        pck.clear();
        pck.putStart();
        pck.putFill(0, keepSize, 0);
        pck.msbPutW(0x00, 0x8f00); // address
        pck.msbPutW(0x02, ifcHdlc.typeKeep); // type
        pck.msbPutD(0x04, 2); // command
        pck.msbPutD(0x08, sequenceTx); // tx sequence
        pck.msbPutD(0x0c, sequenceRx); // rx sequence
        pck.msbPutW(0x10, 0xffff); // rel
        pck.msbPutD(0x12, lastTxKeep); // time
        pck.putSkip(keepSize);
        pck.merge2beg();
        cntr.tx(pck);
        lower.sendPack(pck);
        if (debugger.ifcHdlcEvnt) {
            logger.debug("tx keepalive txSeq=" + sequenceTx + " rxSeq=" + sequenceRx + " tim=" + lastTxKeep);
        }
    }

}

class ifcHdlcTxKeep extends TimerTask {

    private ifcHdlc lower;

    public ifcHdlcTxKeep(ifcHdlc parent) {
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
