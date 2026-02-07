package org.freertr.ifc;

import java.util.List;
import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrType;
import org.freertr.pack.packHolder;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.state;

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
    public int keepaliveInterval = 5000;

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

    /**
     * keepalive
     */
    protected ifcHdlcTxKeep keepTimer;

    public counter getCounter() {
        return cntr;
    }

    public void setParent(ifcDn parent) {
        lower = parent;
    }

    public void closeUp() {
        setState(state.states.close);
        upper.closeUp();
        restartTimer(true);
    }

    public void closeDn() {
        setState(state.states.close);
        lower.closeDn();
        restartTimer(true);
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
    public static void getHelp(userHelp l) {
        l.add(null, false, 2, new int[]{3}, "keepalive", "keepalive timer");
        l.add(null, false, 3, new int[]{-1}, "<num>", "time in ms");
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
        if (bits.getTime() - lastRxKeep < keepaliveInterval * 6) {
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
        keepTimer = new ifcHdlcTxKeep(this);
        keepTimer.start();
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
        if ((gotRsq != sequenceTx) && (gotRsq != (sequenceTx - 1))) {
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

class ifcHdlcTxKeep implements Runnable {

    private ifcHdlc lower;

    public ifcHdlcTxKeep(ifcHdlc parent) {
        lower = parent;
    }

    public void start() {
        new Thread(this).start();
    }

    public void run() {
        try {
            for (;;) {
                if (lower.keepTimer != this) {
                    break;
                }
                lower.sendKeepalive();
                bits.sleep(lower.keepaliveInterval);
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}
