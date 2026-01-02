package org.freertr.ifc;

import java.util.List;
import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrType;
import org.freertr.cry.cryHashCrc32;
import org.freertr.cry.cryHashCrc8;
import org.freertr.pack.packHolder;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.counter;
import org.freertr.util.state;

/**
 * dvb s2 gse encapsulation handler
 *
 * @author matecsaba
 */
public class ifcDvbGse implements ifcUp, ifcDn {

    /**
     * fixed header
     */
    public final static int size = 9;

    /**
     * first fragment
     */
    public final static int fragBeg = 0x8000;

    /**
     * last fragment
     */
    public final static int fragEnd = 0x4000;

    /**
     * no label mode
     */
    public final static int fragAdr = 0x2000;

    /**
     * max payload size
     */
    public int fragLen = 0;

    /**
     * max payload delay
     */
    public int fragGap = 0;

    /**
     * use baseband header
     */
    public boolean baseBand = true;

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
        return "dvbgse on " + lower;
    }

    /**
     * create new instance
     */
    public ifcDvbGse() {
    }

    /**
     * get help text
     *
     * @param l storage
     */
    public static void getHelp(userHelp l) {
        l.add(null, false, 2, new int[]{-1}, "baseband", "use baseband header");
        l.add(null, false, 2, new int[]{3}, "fragment", "set end2end payload size");
        l.add(null, false, 3, new int[]{-1}, "<num>", "number of bytes");
        l.add(null, false, 2, new int[]{3}, "frgap", "inter fragment gap");
        l.add(null, false, 3, new int[]{-1}, "<num>", "milliseconds");
    }

    /**
     * get configuration
     *
     * @param l storage
     * @param beg beginning
     */
    public void getConfig(List<String> l, String beg) {
        if (baseBand) {
            l.add(beg + "baseband");
        } else {
            l.add(cmds.tabulator + cmds.negated + beg + "baseband");
        }
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
        if (a.equals("fragment")) {
            fragLen = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("frgap")) {
            fragGap = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("baseband")) {
            baseBand = true;
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
        if (a.equals("baseband")) {
            baseBand = false;
            return;
        }
        cmd.badCmd();
    }

    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        if (baseBand) {
            if (pck.dataSize() < size) {
                cntr.drop(pck, counter.reasons.tooSmall);
                return;
            }
            cryHashCrc8 sum = new cryHashCrc8(cryHashCrc8.polyCrc8d);
            sum.init();
            pck.hashData(sum, 0, size);
            byte[] buf = new byte[sum.getHashSize()];
            pck.getCopy(buf, 0, size, buf.length);
            if (bits.byteComp(buf, 0, sum.finish(), 0, buf.length) != 0) {
                cntr.drop(pck, counter.reasons.badSum);
                return;
            }
            int i = pck.msbGetW(4) / 8; // length
            pck.getSkip(size + buf.length);
            if (i > pck.dataSize()) {
                cntr.drop(pck, counter.reasons.badLen);
                return;
            }
            pck.setDataSize(i);
        }
        int i = pck.msbGetW(0); // gse
        pck.getSkip(2);
        boolean beg = (i & fragBeg) != 0;
        boolean end = (i & fragEnd) != 0;
        if (beg && end) { // full
            upper.recvPack(pck);
            return;
        }
        i = pck.getByte(0); // frag id
        pck.getSkip(1);
        if (beg) {
            fragReasm.clear();
            fragSeqRx = i;
        }
        if ((i & fragBeg) != 0) {
            fragSeqRx = i;
            fragReasm.clear();
        }
        if (fragSeqRx != i) {
            cntr.drop(pck, counter.reasons.badRxSeq);
            return;
        }
        byte[] buf = pck.getCopy();
        fragReasm.putCopy(buf, 0, 0, buf.length);
        fragReasm.putSkip(buf.length);
        fragReasm.merge2end();
        if (!end) {
            return;
        }
        fragSeqRx = -1;
        i = fragReasm.msbGetW(0) + 2;
        if (i > fragReasm.dataSize()) {
            cntr.drop(fragReasm, counter.reasons.badLen);
            return;
        }
        cryHashCrc32 crc = new cryHashCrc32(cryHashCrc32.polyCrc32i);
        crc.init();
        fragReasm.hashData(crc, 0, i);
        buf = new byte[crc.getHashSize()];
        fragReasm.getCopy(buf, 0, i, buf.length);
        if (bits.byteComp(buf, 0, crc.finish(), 0, buf.length) != 0) {
            cntr.drop(pck, counter.reasons.badSum);
            return;
        }
        fragReasm.setDataSize(i);
        fragReasm.getSkip(2);
        upper.recvPack(fragReasm);
    }

    private void putBaseband(packHolder pck) {
        if (!baseBand) {
            return;
        }
        pck.putByte(0, 0x72); // mtype: gse, sis, ccm
        pck.putByte(1, 0); // mtype2: none
        pck.msbPutW(2, 0); // upl
        pck.msbPutW(4, pck.dataSize() * 8); // dfl
        pck.putByte(6, 0); // sync
        pck.msbPutW(7, 0); // syncd
        cryHashCrc8 sum = new cryHashCrc8(cryHashCrc8.polyCrc8d);
        sum.init();
        pck.hashHead(sum, 0, size);
        byte[] buf = sum.finish();
        pck.putSkip(size);
        pck.putCopy(buf, 0, 0, buf.length);
        pck.putSkip(buf.length);
        pck.merge2beg();
    }

    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        if ((fragLen < 1) || (pck.dataSize() < fragLen)) {
            pck.msbPutW(0, pck.dataSize() | fragBeg | fragEnd | fragAdr); // gse complete
            pck.putSkip(2);
            pck.merge2beg();
            putBaseband(pck);
            lower.sendPack(pck);
            return;
        }
        pck.msbPutW(0, pck.dataSize()); // total length
        pck.putSkip(2);
        pck.merge2beg();
        cryHashCrc32 crc = new cryHashCrc32(cryHashCrc32.polyCrc32i);
        crc.init();
        pck.hashData(crc, 0, pck.dataSize());
        byte[] buf = crc.finish();
        pck.putCopy(buf, 0, 0, buf.length);
        pck.putSkip(buf.length);
        pck.merge2end();
        fragSeqTx++;
        buf = pck.getCopy();
        for (int ofs = 0; ofs < buf.length;) {
            int len = buf.length - ofs;
            if (len > fragLen) {
                len = fragLen;
            }
            pck.clear();
            pck.putCopy(buf, ofs, 0, len);
            pck.putSkip(len);
            pck.merge2beg();
            int flg = fragAdr + 1 + pck.dataSize(); // gse
            if (ofs == 0) {
                flg |= fragBeg; // start
            }
            if ((ofs + len) >= buf.length) {
                flg |= fragEnd; // end
            }
            pck.msbPutW(0, flg);
            pck.putByte(2, fragSeqTx); // frag id
            pck.putSkip(3);
            pck.merge2beg();
            putBaseband(pck);
            lower.sendPack(pck);
            ofs += len;
            if (fragGap < 1) {
                continue;
            }
            bits.sleep(fragGap);
        }
    }

}
