package org.freertr.tab;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.util.bits;

/**
 * bier forwarder
 *
 * @author matecsaba
 */
public class tabLabelBier {

    /**
     * label base
     */
    public final int base;

    /**
     * bit string length
     */
    public final int bsl;

    /**
     * own index
     */
    public int idx;

    /**
     * second own index
     */
    public int idx2;

    /**
     * forwarder peers
     */
    public final tabGen<tabLabelBierN> peers = new tabGen<tabLabelBierN>();

    /**
     * create new instance
     *
     * @param bas base
     * @param len bsl
     */
    public tabLabelBier(int bas, int len) {
        base = bas;
        bsl = len;
    }

    /**
     * copy bytee
     *
     * @return copy
     */
    public tabLabelBier copyBytes() {
        tabLabelBier n = new tabLabelBier(base, bsl);
        n.idx = idx;
        n.idx2 = idx2;
        for (int i = 0; i < peers.size(); i++) {
            n.peers.add(peers.get(i).copyBytes());
        }
        return n;
    }

    /**
     * check if differs
     *
     * @param o other
     * @return true if yes, false if not
     */
    public boolean differs(tabLabelBier o) {
        if (o == null) {
            return true;
        }
        if (base != o.base) {
            return true;
        }
        if (bsl != o.bsl) {
            return true;
        }
        if (idx != o.idx) {
            return true;
        }
        if (idx2 != o.idx2) {
            return true;
        }
        if (peers.size() != o.peers.size()) {
            return true;
        }
        for (int i = 0; i < peers.size(); i++) {
            if (peers.get(i).differs(o.peers.get(i))) {
                return true;
            }
        }
        return false;
    }

    /**
     * encode bit string length
     *
     * @param i bsl
     * @return encoded value
     */
    public static int num2bsl(int i) {
        int o = Integer.bitCount(Integer.highestOneBit(i) - 1) - 5;
        if (o < 0) {
            o = 0;
        }
        if (o > 7) {
            o = 7;
        }
        return o;
    }

    /**
     * decode bit string length
     *
     * @param i encoded value
     * @return bsl
     */
    public static int bsl2num(int i) {
        return 1 << (i + 5);
    }

    /**
     * normalize bit string length
     *
     * @param i bsl
     * @return bsl
     */
    public static int normalizeBsl(int i) {
        return bsl2num(num2bsl(i));
    }

    /**
     * get si mask
     *
     * @param i bsl
     * @return mask
     */
    public static byte[] bsl2msk(int i) {
        byte[] res = new byte[bsl2num(i) / 8];
        bits.byteFill(res, 0, res.length, 0xff);
        return res;
    }

    /**
     * unset and test own bit
     *
     * @param src bit string
     * @param bsl bs length
     * @param idx index to check
     * @return true if bit was set
     */
    public static boolean untestMine(byte[] src, int bsl, int idx) {
        if (idx < 0) {
            return false;
        }
        if (idx >= bsl) {
            return false;
        }
        int ofs = src.length - 1 - (idx / 8);
        int bit = idx & 7;
        if ((src[ofs] & bits.bitVals[bit]) == 0) {
            return false;
        }
        src[ofs] &= (byte) (~bits.bitVals[bit]);
        return true;
    }

    /**
     * merge from other
     *
     * @param src source
     */
    public void mergeFrom(tabLabelBier src) {
        if (src == null) {
            return;
        }
        for (int i = 0; i < src.peers.size(); i++) {
            tabLabelBierN ntry = src.peers.get(i);
            if (ntry == null) {
                continue;
            }
            peers.add(ntry);
        }
        if (idx == 0) {
            idx = src.idx;
        }
        if (idx2 == 0) {
            idx2 = src.idx2;
        }
    }

    /**
     * get mask for index
     *
     * @return mask
     */
    public tabLabelBierN getIdxMask() {
        tabLabelBierN n = new tabLabelBierN(null, null, new addrIP(), 0, 0);
        if (idx > 0) {
            n.setBit(idx - 1);
        }
        if (idx2 > 0) {
            n.setBit(idx2 - 1);
        }
        return n;
    }

    /**
     * dump this entry
     *
     * @param prnt parent entry
     * @return dump
     */
    public List<String> getShow(tabLabelEntry prnt) {
        int bits = bsl2num(bsl);
        int si = prnt.label - base;
        int sis = bits * si;
        List<String> lst = new ArrayList<String>();
        lst.add("bier base|" + base);
        lst.add("bier bsl|" + bsl + "-" + bits);
        lst.add("bier si|" + si);
        lst.add("bier sis|" + sis);
        lst.add("bier idx|" + idx);
        lst.add("bier idx2|" + idx2);
        tabLabelBierN ntry = getIdxMask();
        byte[] full = bsl2msk(bsl);
        lst.add("bier local bs|" + ntry.dumpBits(full, sis));
        for (int i = 0; i < peers.size(); i++) {
            ntry = peers.get(i);
            lst.add("bier peer|" + ntry.hop + " " + ntry.iface + " lab=" + ntry.label + " bs=" + ntry.dumpBits(full, sis));
        }
        return lst;
    }

}
