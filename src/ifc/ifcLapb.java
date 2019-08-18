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
 * lapb (itu x25) encapsulation handler
 *
 * @author matecsaba
 */
public class ifcLapb implements ifcUp, ifcDn {

    /**
     * terminal mode
     */
    public int dataMode = dataMoDTE;

    /**
     * terminal modulo value
     */
    public int dataModulo = dataMod8;

    /**
     * current keepalive interval (0=disabled)
     */
    public int keepaliveInterval = 5;

    /**
     * dte terminal
     */
    public final static int dataMoDTE = 0x01;

    /**
     * dce terminal
     */
    public final static int dataMoDCE = 0x03;

    /**
     * modulo 8
     */
    public final static int dataMod8 = 1;

    /**
     * modulo 128
     */
    public final static int dataMod128 = 2;

    /**
     * modulo 32768
     */
    public final static int dataMod32768 = 3;

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
     * Information
     */
    public final static int commandI = 0x00;

    /**
     * Receiver Ready
     */
    public final static int commandRR = 0x01;

    /**
     * Receiver Not Ready
     */
    public final static int commandRNR = 0x05;

    /**
     * Reject
     */
    public final static int commandREJ = 0x09;

    /**
     * Sejective Reject
     */
    public final static int commandSREJ = 0x0d;

    /**
     * Set Asynchronous Balanced Mode
     */
    public final static int commandSABM = 0x2f;

    /**
     * Set Asynchronous Balanced Mode Extended
     */
    public final static int commandSABME = 0x6f;

    /**
     * Set Asynchronous Balanced Mode Super
     */
    public final static int commandSABMS = 0xc3;

    /**
     * Disconnect
     */
    public final static int commandDISC = 0x43;

    /**
     * Disconnect Mode
     */
    public final static int commandDM = 0x0f;

    /**
     * Unnumbered Acknowledgement
     */
    public final static int commandUA = 0x63;

    /**
     * Frame Reject
     */
    public final static int commandFRMR = 0x87;

    private Timer keepTimer;

    private final static int bufferMax = 6;

    private final packHolder bufferDat[] = new packHolder[bufferMax];

    private int bufferCur = 0;

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
        clearState();
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

    public int getMTUsize() {
        return lower.getMTUsize() - getHeaderSize();
    }

    public long getBandwidth() {
        return lower.getBandwidth();
    }

    public void setState(state.states stat) {
        stat = state.toForceable(stat);
        if (checkPeerState(stat)) {
            return;
        }
        restartTimer(false);
    }

    /**
     * create new instance
     */
    public ifcLapb() {
        clearState();
        restartTimer(false);
    }

    public String toString() {
        return "lapb on " + lower;
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
        l.add("2 3     modulus                     modulus");
        l.add("3 .       8                         use 3 bit modulus");
        l.add("3 .       128                       use 7 bit modulus");
        l.add("3 .       32768                     use 15 bit modulus");
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
        switch (dataModulo) {
            case dataMod8:
                a = "8";
                break;
            case dataMod128:
                a = "128";
                break;
            case dataMod32768:
                a = "32768";
                break;
            default:
                a = "unknown=" + dataModulo;
                break;
        }
        l.add(beg + "modulus " + a);
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
        if (a.equals("modulus")) {
            a = cmd.word();
            int i = 0;
            if (a.equals("8")) {
                i = dataMod8;
            }
            if (a.equals("128")) {
                i = dataMod128;
            }
            if (a.equals("32768")) {
                i = dataMod32768;
            }
            dataModulo = i;
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
     * get size of header
     *
     * @return size of header
     */
    public int getHeaderSize() {
        switch (dataModulo) {
            case dataMod8:
                return 2;
            case dataMod128:
                return 3;
            case dataMod32768:
                return 5;
            default:
                return 1;
        }
    }

    /**
     * decode command word
     *
     * @param i command word
     * @return string showing command
     */
    public String decodeCommand(int i) {
        switch (i) {
            case commandI:
                return "I";
            case commandRR:
                return "RR";
            case commandRNR:
                return "RNR";
            case commandREJ:
                return "REJ";
            case commandSREJ:
                return "SREJ";
            case commandSABM:
                return "SABM";
            case commandSABME:
                return "SABME";
            case commandSABMS:
                return "SABMS";
            case commandDISC:
                return "DISC";
            case commandDM:
                return "DM";
            case commandUA:
                return "UA";
            case commandFRMR:
                return "FRMR";
            default:
                return "unknown:" + i;
        }
    }

    /**
     * get sabm command value
     *
     * @return sabm command
     */
    public int getSABMcommand() {
        switch (dataModulo) {
            case ifcLapb.dataMod8:
                return commandSABM;
            case ifcLapb.dataMod128:
                return commandSABME;
            case ifcLapb.dataMod32768:
                return commandSABMS;
            default:
                return 0;
        }
    }

    private void clearState() {
        lastRxKeep = 0;
        sequenceRx = 0;
        sequenceTx = 0;
        bufferCur = 0;
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
        if (debugger.ifcLapbEvnt) {
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
        ifcLapbTxKeep task = new ifcLapbTxKeep(this);
        keepTimer.schedule(task, 500, keepaliveInterval * 1000);
    }

    /**
     * get truncater value
     *
     * @return modulo ander
     */
    public int getModMax() {
        switch (dataModulo) {
            case dataMod8:
                return 0x7;
            case dataMod128:
                return 0x7f;
            case dataMod32768:
                return 0x7fff;
            default:
                return 0;
        }
    }

    /**
     * ack some packets
     *
     * @param nr nr value
     */
    public void ackPacks(int nr) {
        int trnc = getModMax();
        nr = (nr - sequenceTx) & trnc;
        if (nr < 1) {
            return;
        }
        if (nr > bufferCur) {
            return;
        }
        for (int i = nr; i < bufferCur; i++) {
            bufferDat[i - nr] = bufferDat[i];
        }
        bufferCur -= nr;
        sequenceTx = (sequenceTx + nr) & trnc;
    }

    /**
     * send buffered packet
     *
     * @param i number in buffer
     * @return false if successful, true if error happened
     */
    public boolean sendBufferedPack(int i) {
        if (i < 0) {
            return true;
        }
        if (i >= bufferCur) {
            return true;
        }
        packHolder pck = bufferDat[i].copyBytes(true, true);
        putPacketHeader(pck, commandI, sequenceTx + i, sequenceRx, 0, 1);
        lower.sendPack(pck);
        return false;
    }

    /**
     * create packet header
     *
     * @param pck packet to update
     * @param cmd command word
     * @param ns next send value
     * @param nr next receive value
     * @param rp set 1 for response
     * @param pf poll/final bit
     */
    public void putPacketHeader(packHolder pck, int cmd, int ns, int nr,
            int rp, int pf) {
        pf &= 1;
        cmd &= 0xef;
        if (debugger.ifcLapbEvnt) {
            logger.debug("tx " + decodeCommand(cmd) + " rp=" + rp + " pf=" + pf
                    + " ns=" + ns + " nr=" + nr);
        }
        if (rp != 0) {
            rp = dataMoDCE ^ dataMoDTE;
        }
        rp ^= dataMode;
        pck.putByte(0, rp);
        int siz = getHeaderSize();
        switch (cmd & 0x3) {
            case 0:
            case 2:
                break;
            case 1:
                ns = 0;
                break;
            case 3:
                cmd |= pf << 4;
                ns = 0;
                nr = 0;
                pf = 0;
                siz = 2;
                break;
        }
        switch (dataModulo) {
            case dataMod8:
                cmd |= (ns & 0x7) << 1;
                cmd |= (nr & 0x7) << 5;
                cmd |= pf << 4;
                pck.putByte(1, cmd);
                break;
            case dataMod128:
                cmd |= (ns & 0x7f) << 1;
                cmd |= (nr & 0x7f) << 9;
                cmd |= pf << 8;
                pck.lsbPutW(1, cmd);
                break;
            case dataMod32768:
                cmd |= (ns & 0x7fff) << 1;
                cmd |= (nr & 0x7fff) << 17;
                cmd |= pf << 16;
                pck.lsbPutD(1, cmd);
                break;
            default:
                cntr.drop(pck, counter.reasons.badVal);
                return;
        }
        pck.putSkip(siz);
        pck.merge2beg();
    }

    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        if (pck.dataSize() < 2) {
            cntr.drop(pck, counter.reasons.tooSmall);
            return;
        }
        int rp = pck.getByte(0);
        switch (rp) {
            case dataMoDCE:
            case dataMoDTE:
                break;
            default:
                cntr.drop(pck, counter.reasons.badAddr);
                return;
        }
        if (rp == dataMode) {
            rp = 1;
        } else {
            rp = 0;
        }
        int cmd = 0;
        int ns = 0;
        int nr = 0;
        int pf = 0;
        switch (dataModulo) {
            case dataMod8:
                cmd = pck.getByte(1);
                ns = (cmd >>> 1) & 0x7;
                nr = (cmd >>> 5) & 0x7;
                pf = (cmd >>> 4) & 0x1;
                break;
            case dataMod128:
                cmd = pck.lsbGetW(1);
                ns = (cmd >>> 1) & 0x7f;
                nr = (cmd >>> 9) & 0x7f;
                pf = (cmd >>> 8) & 0x1;
                break;
            case dataMod32768:
                cmd = pck.lsbGetD(1);
                ns = (cmd >>> 1) & 0x7fff;
                nr = (cmd >>> 17) & 0x7fff;
                pf = (cmd >>> 16) & 0x1;
                break;
            default:
                cntr.drop(pck, counter.reasons.badVal);
                return;
        }
        int siz = getHeaderSize();
        switch (cmd & 0x3) {
            case 0x0:
            case 0x2:
                cmd = 0;
                break;
            case 0x1:
                cmd &= 0xf;
                ns = 0;
                break;
            case 0x3:
                pf = (cmd >>> 4) & 1;
                cmd &= 0xef;
                ns = 0;
                nr = 0;
                siz = 2;
                break;
        }
        pck.getSkip(siz);
        if (debugger.ifcLapbEvnt) {
            logger.debug("rx " + decodeCommand(cmd) + " rp=" + rp + " pf=" + pf + " ns=" + ns + " nr=" + nr);
        }
        switch (cmd) {
            case commandI:
                sequenceRx &= getModMax();
                if (ns != sequenceRx) {
                    cntr.drop(pck, counter.reasons.badRxSeq);
                    return;
                }
                sequenceRx += 1;
                ackPacks(nr);
                int i = ifcEther.guessEtherType(pck);
                if (i < 0) {
                    cntr.drop(pck, counter.reasons.badVer);
                    break;
                }
                pck.msbPutW(0, i);
                i = pck.headSize();
                pck.putSkip(2);
                pck.mergeHeader(-1, i);
                upper.recvPack(pck);
                break;
            case commandRR:
            case commandRNR:
            case commandREJ:
            case commandSREJ:
                ackPacks(nr);
                lastRxKeep = bits.getTime();
                checkPeerState(state.states.up);
                break;
            case commandUA:
                lastRxKeep = bits.getTime();
                checkPeerState(state.states.up);
                break;
            case commandDISC:
            case commandDM:
                checkPeerState(state.states.down);
                break;
            case commandSABM:
            case commandSABME:
            case commandSABMS:
                if (cmd != getSABMcommand()) {
                    cntr.drop(pck, counter.reasons.badHdr);
                    break;
                }
                pck.clear();
                pck.putStart();
                putPacketHeader(pck, commandUA, 0, sequenceRx, 1, 1);
                lower.sendPack(pck);
                clearState();
                lastRxKeep = bits.getTime();
                checkPeerState(state.states.up);
                pf = 0;
                break;
            default:
                cntr.drop(pck, counter.reasons.badCmd);
        }
        if ((pf != 0) && (rp == 0)) {
            pck.clear();
            pck.putStart();
            putPacketHeader(pck, commandRR, 0, sequenceRx, 1, 1);
            lower.sendPack(pck);
        }
    }

    public void sendPack(packHolder orig) {
        cntr.tx(orig);
        if (lastState != state.states.up) {
            cntr.drop(orig, counter.reasons.notUp);
            return;
        }
        if (bufferCur >= bufferMax) {
            cntr.drop(orig, counter.reasons.noBuffer);
            return;
        }
        if (ifcEther.stripEtherType(orig)) {
            cntr.drop(orig, counter.reasons.badProto);
            return;
        }
        packHolder pck = orig.copyBytes(true, true);
        bufferDat[bufferCur] = pck;
        bufferCur++;
        sendBufferedPack(bufferCur - 1);
    }

    /**
     * send keepalive packet
     */
    protected void sendKeepalive() {
        checkPeerState(state.states.up);
        int i = getSABMcommand();
        if (lastState == state.states.up) {
            if (!sendBufferedPack(0)) {
                return;
            }
            i = ifcLapb.commandRR;
        }
        packHolder pck = new packHolder(true, true);
        pck.clear();
        pck.putStart();
        putPacketHeader(pck, i, 0, sequenceRx, 0, 1);
        lower.sendPack(pck);
    }

}

class ifcLapbTxKeep extends TimerTask {

    private ifcLapb lower;

    public ifcLapbTxKeep(ifcLapb parent) {
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
