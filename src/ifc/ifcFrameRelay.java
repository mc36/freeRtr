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
 * frame relay encapsulation handler
 *
 * @author matecsaba
 */
public class ifcFrameRelay implements ifcUp, ifcDn {

    /**
     * terminal mode
     */
    public int dataMode = dataMoDTE;

    /**
     * lmi type
     */
    public int lmiType = lmiAnsi;

    /**
     * dlci number
     */
    public int dlciNum = 1;

    /**
     * max payload size
     */
    public int fragLen = 0;

    /**
     * max payload delay
     */
    public int fragGap = 0;

    /**
     * tx sequence
     */
    public int fragSeqTx = 0;

    /**
     * rx sequence
     */
    public int fragSeqRx = -1;

    /**
     * reassembly buffer
     */
    public packHolder fragReasm = new packHolder(true, true);

    /**
     * ansi lmi type
     */
    public final static int lmiAnsi = 1;

    /**
     * cisco lmi type
     */
    public final static int lmiCisco = 2;

    /**
     * q933a lmi type
     */
    public final static int lmiQ933a = 3;

    /**
     * dte terminal
     */
    public final static int dataMoDTE = 1;

    /**
     * dce terminal
     */
    public final static int dataMoDCE = 2;

    /**
     * size of header
     */
    public final static int size = 2;

    /**
     * type of fragment header
     */
    public final static int fragType = 0x3b1;

    /**
     * first fragment
     */
    public final static int fragBeg = 0x8000;

    /**
     * last fragment
     */
    public final static int fragEnd = 0x4000;

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
    public long lastTxKeep = 0;

    /**
     * need full update
     */
    public boolean needFull;

    /**
     * enquery
     */
    public final static int opcodeEnquery = 0x75;

    /**
     * status
     */
    public final static int opcodeStatus = 0x7d;

    private Timer keepTimer;

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

    public void closeDn() {
        setState(state.states.close);
        lower.closeDn();
    }

    public void flapped() {
        lower.flapped();
    }

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
     * set state
     *
     * @param stat state
     */
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
        return "frameRelay on " + lower;
    }

    /**
     * create new instance
     */
    public ifcFrameRelay() {
        restartTimer(false);
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
        l.add("2 3     lmi                         lmi type");
        l.add("3 .       ansi                      set to ansi");
        l.add("3 .       cisco                     set to cisco");
        l.add("3 .       q933a                     set to itu q933a");
        l.add("2 3     dlci                        set dlci number");
        l.add("3 .       <num>                     dlci number");
        l.add("2 3     fragment                    set end2end payload size");
        l.add("3 .       <num>                     number of bytes");
        l.add("2 3     frgap                       inter fragment gap");
        l.add("3 .       <num>                     milliseconds");
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
        switch (lmiType) {
            case lmiAnsi:
                a = "ansi";
                break;
            case lmiCisco:
                a = "cisco";
                break;
            case lmiQ933a:
                a = "q933a";
                break;
            default:
                a = "unknown=" + lmiType;
                break;
        }
        l.add(beg + "lmi " + a);
        l.add(beg + "dlci " + dlciNum);
        l.add(beg + "fragment " + fragLen);
        l.add(beg + "frgap " + fragGap);
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
        if (a.equals("lmi")) {
            a = cmd.word();
            int i = 0;
            if (a.equals("ansi")) {
                i = lmiAnsi;
            }
            if (a.equals("cisco")) {
                i = lmiCisco;
            }
            if (a.equals("q933a")) {
                i = lmiQ933a;
            }
            lmiType = i;
            return;
        }
        if (a.equals("dlci")) {
            dlciNum = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("fragment")) {
            fragLen = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("frgap")) {
            fragGap = bits.str2num(cmd.word());
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
        String a = cmd.word();
        if (a.equals("fragment")) {
            fragLen = 0;
            return;
        }
        if (a.equals("frgap")) {
            fragGap = 0;
            return;
        }
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
        if (debugger.ifcFrmRlyEvnt) {
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
        ifcFrameRelayTxKeep task = new ifcFrameRelayTxKeep(this);
        keepTimer.schedule(task, 500, keepaliveInterval * 1000);
    }

    private void putDLCI(packHolder pck, int dlci) {
        pck.putByte(0, (dlci >>> 4) << 2);
        pck.putByte(1, (dlci << 4) | 1);
        pck.putSkip(size);
    }

    private void putFrag(packHolder pck, int seq, boolean frst, boolean last) {
        pck.msbPutW(0, fragType);
        pck.putSkip(size);
        int i = (seq & 0xf00) << 1;
        if (frst) {
            i |= fragBeg;
        }
        if (last) {
            i |= fragEnd;
        }
        i |= seq & 0xff;
        pck.msbPutW(0, i);
        pck.putSkip(size);
    }

    /**
     * received packet
     *
     * @param pck packet
     */
    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        if (pck.dataSize() < size) {
            cntr.drop(pck, counter.reasons.tooSmall);
            return;
        }
        int b0 = pck.getByte(0);
        int b1 = pck.getByte(1);
        pck.getSkip(size);
        // boolean cr = (b0 & 0x2) != 0;
        // boolean de = (b1 & 0x2) != 0;
        // boolean becn = (b1 & 0x4) != 0;
        // boolean fecn = (b1 & 0x8) != 0;
        int dlci = ((b0 >>> 2) << 4) | (b1 >>> 4);
        if (dlci != dlciNum) {
            if (dlci != getLMIdlci()) {
                cntr.drop(pck, counter.reasons.badVlan);
                return;
            }
            recvKeepalive(pck);
            checkPeerState(state.states.up);
            return;
        }
        if ((fragLen < 1) || (pck.msbGetW(0) != fragType)) {
            upper.recvPack(pck);
            return;
        }
        pck.getSkip(size);
        int i = pck.msbGetW(0);
        pck.getSkip(size);
        int seq = ((i >>> 1) & 0xf00) | (i & 0xff);
        if ((i & fragBeg) != 0) {
            fragSeqRx = seq;
            fragReasm.clear();
        }
        if (fragSeqRx != seq) {
            cntr.drop(pck, counter.reasons.badRxSeq);
            return;
        }
        fragSeqRx = (fragSeqRx + 1) & 0xfff;
        byte[] buf = pck.getCopy();
        fragReasm.putCopy(buf, 0, 0, buf.length);
        fragReasm.putSkip(buf.length);
        fragReasm.merge2end();
        if ((i & fragEnd) == 0) {
            return;
        }
        fragSeqRx = -1;
        upper.recvPack(fragReasm);
    }

    public synchronized void sendPack(packHolder pck) {
        cntr.tx(pck);
        if ((fragLen < 1) || (pck.dataSize() < fragLen)) {
            putDLCI(pck, dlciNum);
            pck.merge2beg();
            lower.sendPack(pck);
            return;
        }
        byte[] buf = pck.getCopy();
        for (int ofs = 0; ofs < buf.length;) {
            int len = buf.length - ofs;
            if (len > fragLen) {
                len = fragLen;
            }
            fragSeqTx++;
            pck.clear();
            putDLCI(pck, dlciNum);
            putFrag(pck, fragSeqTx, ofs == 0, (ofs + len) >= buf.length);
            pck.msbPutW(ofs, len);
            pck.putCopy(buf, ofs, 0, len);
            pck.putSkip(len);
            pck.merge2beg();
            lower.sendPack(pck);
            ofs += len;
            if (fragGap < 1) {
                continue;
            }
            bits.sleep(fragGap);
        }
    }

    private int getLMIdlci() {
        switch (lmiType) {
            case lmiAnsi:
                return 0;
            case lmiCisco:
                return 1023;
            case lmiQ933a:
                return 0;
            default:
                return -1;
        }
    }

    private int getNLPIDvalue() {
        switch (lmiType) {
            case lmiAnsi:
                return 0x0308;
            case lmiCisco:
                return 0x0309;
            case lmiQ933a:
                return 0x0308;
            default:
                return -1;
        }
    }

    private int getORer(int i) {
        if (lmiType == lmiQ933a) {
            return i | 0x50;
        } else {
            return i;
        }
    }

    private void putPVCstat(packHolder pck, int dlci) {
        pck.putByte(0, getORer(7)); // type of pvcstat
        switch (lmiType) {
            case lmiAnsi:
            case lmiQ933a:
                pck.putByte(1, 3); // length of pvcstat
                pck.putByte(2, (dlci >>> 4) & 0x3f);
                pck.putByte(3, (dlci << 3) | 0x80);
                pck.putByte(4, 0x82);
                pck.putSkip(5);
                break;
            case lmiCisco:
                pck.putByte(1, 6); // length of pvcstat
                pck.msbPutW(2, dlci); // dlci number
                pck.putByte(4, 2);
                pck.putByte(5, 0);
                pck.putByte(6, 0);
                pck.putByte(7, 0);
                pck.putSkip(8);
                break;
        }
    }

    /**
     * send keepalive packet
     */
    protected void sendKeepalive() {
        lastTxKeep = bits.getTime();
        sequenceTx++;
        if (debugger.ifcFrmRlyEvnt) {
            logger.debug("tx keepalive mySeq=" + sequenceTx + " peerSeq=" + sequenceRx + " full=" + needFull);
        }
        packHolder pck = new packHolder(true, true);
        int i;
        pck.msbPutW(0, getNLPIDvalue()); // nlpid value
        pck.putByte(2, 0); // call reference
        if (dataMode == dataMoDTE) {
            i = opcodeEnquery;
        } else {
            i = opcodeStatus;
        }
        pck.putByte(3, i); // opcode
        pck.putSkip(4);
        if (lmiType == lmiAnsi) {
            pck.putByte(0, 0x95); // ansi lockshift
            pck.putSkip(1);
        }
        pck.putByte(0, getORer(1)); // type of report
        pck.putByte(1, 1); // length of report
        if (needFull) {
            i = 0;
        } else {
            i = 1;
        }
        pck.putByte(2, i); // type of listing
        pck.putSkip(3);
        pck.putByte(0, getORer(3)); // type of alive
        pck.putByte(1, 2); // length of alive
        pck.putByte(2, sequenceTx);
        pck.putByte(3, sequenceRx);
        pck.putSkip(4);
        if (needFull) {
            putPVCstat(pck, dlciNum);
        }
        pck.merge2beg();
        putDLCI(pck, getLMIdlci());
        pck.merge2beg();
        lower.sendPack(pck);
        needFull = false;
    }

    /**
     * received keepalive packet
     *
     * @param pck
     */
    protected void recvKeepalive(packHolder pck) {
        lastRxKeep = bits.getTime();
        needFull = false;
        int i = pck.msbGetW(0); // nlpid
        if (i != getNLPIDvalue()) {
            cntr.drop(pck, counter.reasons.badHdr);
            return;
        }
        i = pck.getByte(2); // call reference
        if (i != 0) {
            return;
        }
        i = pck.getByte(3); // opcode
        pck.getSkip(4);
        switch (i) {
            case opcodeEnquery:
                break;
            case opcodeStatus:
                break;
            default:
                return;
        }
        if (lmiType == lmiAnsi) {
            i = pck.getByte(0); // ansi lockshift
            pck.getSkip(1);
            if (i != 0x95) {
                return;
            }
        }
        i = pck.getByte(0); // report type
        if (i != getORer(1)) {
            return;
        }
        i = pck.getByte(1); // report size
        if (i != 1) {
            return;
        }
        i = pck.getByte(2); // report value
        pck.getSkip(3);
        needFull = (i == 0);
        i = pck.getByte(0); // alive
        pck.getSkip(1);
        if (i != getORer(3)) {
            return;
        }
        i = pck.getByte(0); // size of sequences
        int seqPeer = pck.getByte(1); // peer sequence
        int seqMine = pck.getByte(2); // peer sequence
        pck.getSkip(3);
        if (i != 2) {
            return;
        }
        sequenceRx = seqPeer;
        if (debugger.ifcFrmRlyEvnt) {
            logger.debug("rx keepalive mySeq=" + seqMine + " peerSeq=" + seqPeer + " full=" + needFull);
        }
    }

}

class ifcFrameRelayTxKeep extends TimerTask {

    private ifcFrameRelay lower;

    public ifcFrameRelayTxKeep(ifcFrameRelay parent) {
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
