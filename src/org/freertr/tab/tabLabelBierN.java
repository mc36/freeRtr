package org.freertr.tab;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.ip.ipFwdIface;
import org.freertr.util.bits;

/**
 * bier peer
 *
 * @author matecsaba
 */
public class tabLabelBierN implements Comparator<tabLabelBierN> {

    /**
     * interface
     */
    public final ipFwdIface iface;

    /**
     * next hop
     */
    public final addrIP hop;

    /**
     * remote label
     */
    public final int label;

    /**
     * bsl value
     */
    public int bsl;

    /**
     * needed indexes
     */
    public List<Byte> ned = new ArrayList<Byte>();

    /**
     * create new bier peer
     *
     * @param ifa interface
     * @param nxtHop next hop
     * @param lab labels
     */
    public tabLabelBierN(tabRouteIface ifa, addrIP nxtHop, int lab) {
        iface = (ipFwdIface) ifa;
        hop = nxtHop.copyBytes();
        label = lab;
    }

    /**
     * copy bytee
     *
     * @return copy
     */
    public tabLabelBierN copyBytes() {
        tabLabelBierN n = new tabLabelBierN(iface, hop, label);
        n.bsl = bsl;
        for (int i = 0; i < ned.size(); i++) {
            n.ned.add(ned.get(i));
        }
        return n;
    }

    /**
     * check if differs
     *
     * @param o other
     * @return true if yes, false if not
     */
    public boolean differs(tabLabelBierN o) {
        if (o == null) {
            return true;
        }
        if (iface != o.iface) {
            return true;
        }
        if (label != o.label) {
            return true;
        }
        if (bsl != o.bsl) {
            return true;
        }
        if (hop.compare(hop, o.hop) != 0) {
            return true;
        }
        if (ned.size() != o.ned.size()) {
            return true;
        }
        for (int i = 0; i < ned.size(); i++) {
            if (ned.get(i).compareTo(o.ned.get(i)) != 0) {
                return true;
            }
        }
        return false;
    }

    /**
     * set one bit
     *
     * @param idx bit index
     */
    public void setBit(int idx) {
        int ofs = idx / 8;
        int bit = idx & 7;
        for (; ned.size() <= ofs;) {
            ned.add((byte) 0);
        }
        ned.set(ofs, (byte) (ned.get(ofs) | bits.bitVals[bit]));
    }

    /**
     * get anded, shifted value
     *
     * @param src source
     * @param shr shift value
     * @return result, null if over, empty if no bits;
     */
    public byte[] getAndShr(byte[] src, int shr) {
        byte[] res = new byte[src.length];
        shr = shr / 8;
        if (shr >= ned.size()) {
            return null;
        }
        int saw = 0;
        for (int i = res.length - 1; i >= 0; i--) {
            if (shr >= ned.size()) {
                break;
            }
            byte b = ned.get(shr);
            shr++;
            b &= src[i];
            res[i] = b;
            saw += b & 0xff;
        }
        if (saw < 1) {
            return new byte[0];
        }
        return res;
    }

    /**
     * merge two bytes list
     *
     * @param src other list
     */
    public void mergeNed(List<Byte> src) {
        int got = src.size();
        int siz = ned.size();
        for (int i = 0; i < got; i++) {
            byte b = src.get(i);
            if (i >= siz) {
                ned.add(b);
                continue;
            }
            b |= ned.get(i);
            ned.set(i, b);
        }
    }

    public int compare(tabLabelBierN o1, tabLabelBierN o2) {
        if (o1.iface.ifwNum < o2.iface.ifwNum) {
            return -1;
        }
        if (o1.iface.ifwNum > o2.iface.ifwNum) {
            return +1;
        }
        return o1.hop.compare(o1.hop, o2.hop);
    }

    /**
     * dump bits
     *
     * @param full interested bits
     * @param shr shift value
     * @return dump
     */
    public String dumpBits(byte[] full, int shr) {
        return bits.byteDump(getAndShr(full, shr), 0, -1);
    }

    public String toString() {
        return hop + "," + iface + "," + label;
    }

}
