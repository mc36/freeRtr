package org.freertr.ifc;

import java.util.List;
import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrType;
import org.freertr.cry.cryHashCrc32;
import org.freertr.pack.packHolder;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.counter;
import org.freertr.util.state;

/**
 * dvb mpe encapsulation handler
 *
 * @author matecsaba
 */
public class ifcDvbMpe implements ifcUp, ifcDn {

    /**
     * fixed header
     */
    public final static int head = 4;

    /**
     * fixed payload
     */
    public final static int size = 184;

    /**
     * packet id
     */
    public final static int flagPid = 0x1fff;

    /**
     * continuity
     */
    public final static int flagStr = 0x4000;

    /**
     * continuity
     */
    public final static int flagCnt = 0xf;

    /**
     * mpe header
     */
    public final static int flagHdr = 13;

    /**
     * mpe length
     */
    public final static int flagLen = 0xfff;

    /**
     * packet identifier
     */
    public int pid = 0;

    /**
     * rx continuity count
     */
    public int contRx = 0;

    /**
     * tx continuity count
     */
    public int contTx = 0;

    /**
     * reassembly buffer
     */
    public packHolder reasm = new packHolder(true, true);

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
        setState(lower.getState());
    }

    public state.states getState() {
        return lower.getState();
    }

    public void setFilter(boolean promisc) {
    }

    public addrType getHwAddr() {
        return new addrEmpty();
    }

    public void setState(state.states stat) {
        stat = state.toForceable(stat);
        upper.setState(stat);
    }

    public int getMTUsize() {
        return lower.getMTUsize();
    }

    public long getBandwidth() {
        return lower.getBandwidth();
    }

    public String toString() {
        return "dvbmpe on " + lower;
    }

    /**
     * create new instance
     */
    public ifcDvbMpe() {
    }

    /**
     * get help text
     *
     * @param l storage
     */
    public static void getHelp(userHelp l) {
        l.add(null, false, 2, new int[]{3}, "pid", "set pid");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value");
    }

    /**
     * get configuration
     *
     * @param l storage
     * @param beg beginning
     */
    public void getConfig(List<String> l, String beg) {
        l.add(beg + "pid " + pid);
    }

    /**
     * do configuration
     *
     * @param cmd command
     */
    public void doConfig(cmds cmd) {
        String a = cmd.word();
        if (a.equals("pid")) {
            pid = bits.str2num(cmd.word());
            return;
        }
        cmd.badCmd();
    }

    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        byte[] buf = new byte[size];
        for (;;) {
            if (pck.dataSize() < buf.length) {
                break;
            }
            if (pck.getByte(0) != 0x47) {
                cntr.drop(pck, counter.reasons.badHdr);
                pck.getSkip(1);
                continue;
            }
            int i = pck.msbGetW(1);
            if ((i & flagPid) != pid) {
                cntr.drop(pck, counter.reasons.badID);
                pck.getSkip(head);
                continue;
            }
            if ((pck.getByte(3) & flagCnt) != contRx) {
                cntr.drop(pck, counter.reasons.badRxSeq);
                pck.getSkip(head);
                continue;
            }
            contRx = (contRx + 1) & flagCnt;
            pck.getSkip(head);
            pck.getCopy(buf, 0, 0, buf.length);
            pck.getSkip(buf.length);
            if ((i & flagStr) != 0) {
                reasm.clear();
            }
            reasm.putCopy(buf, 0, 0, buf.length);
            reasm.putSkip(buf.length);
            reasm.merge2end();
            i = (reasm.msbGetW(2) & flagLen) + 4;
            if (i < flagHdr) {
                cntr.drop(reasm, counter.reasons.tooSmall);
                reasm.clear();
                continue;
            }
            if (reasm.msbGetW(0) != 0x003e) {
                cntr.drop(reasm, counter.reasons.badCod);
                reasm.clear();
                continue;
            }
            if (i > reasm.dataSize()) {
                continue;
            }
            i -= 4;
            reasm.setDataSize(i);
            cryHashCrc32 crc = new cryHashCrc32(cryHashCrc32.polyCrc32i);
            crc.init();
            reasm.hashData(crc, 1, i - 1);
            buf = new byte[crc.getHashSize()];
            reasm.getCopy(buf, 0, i, buf.length);
            if (bits.byteComp(buf, 0, crc.finish(), 0, buf.length) != 0) {
                cntr.drop(pck, counter.reasons.badSum);
                return;
            }
            reasm.getSkip(flagHdr);
            i = ifcEther.guessEtherType(reasm);
            if (i < 0) {
                cntr.drop(reasm, counter.reasons.badVal);
                reasm.clear();
                continue;
            }
            reasm.msbPutW(0, i);
            reasm.putSkip(2);
            reasm.merge2beg();
            upper.recvPack(reasm);
            reasm.clear();
        }
    }

    public void sendPack(packHolder pck) {
        if (ifcEther.stripEtherType(pck)) {
            return;
        }
        int len = pck.dataSize() + flagHdr;
        cntr.tx(pck);
        pck.msbPutW(0, 0x003e);
        pck.msbPutW(2, 0xb000 | len); // total length
        pck.msbPutW(4, 0xffff); // dst mac
        pck.putByte(6, 0xc1); // no scrambling
        pck.msbPutW(7, 0); // section numbers
        pck.msbPutD(9, 0xffffffff); // dst mac
        pck.putSkip(flagHdr);
        pck.merge2beg();
        cryHashCrc32 crc = new cryHashCrc32(cryHashCrc32.polyCrc32i);
        crc.init();
        pck.hashData(crc, 1, pck.dataSize() - 1);
        byte[] buf = crc.finish();
        pck.putCopy(buf, 0, 0, buf.length);
        pck.putSkip(buf.length);
        pck.merge2end();
        buf = pck.getCopy();
        pck.clear();
        for (int ofs = 0; ofs < buf.length;) {
            len = buf.length - ofs;
            if (len > size) {
                len = size;
            }
            pck.putByte(0, 0x47); // indicator
            int flg = pid;
            if (ofs == 0) {
                flg |= flagStr;
            }
            pck.msbPutW(1, flg); // packet id
            pck.putByte(3, 0x10 | contTx); // continuity
            pck.putSkip(4);
            pck.merge2end();
            pck.putCopy(buf, ofs, 0, len);
            pck.putSkip(len);
            pck.merge2end();
            if (len < size) {
                int i = size - len;
                pck.putFill(0, i, 0xff);
                pck.putSkip(i);
                pck.merge2end();
            }
            contTx = (contTx + 1) & flagCnt;
            ofs += len;
        }
        lower.sendPack(pck);
    }

}
