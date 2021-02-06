package tab;

import addr.addrIP;
import ip.ipFwdIface;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import util.bits;

/**
 * bier peer
 *
 * @author matecsaba
 */
public class tabLabelBierN implements Comparator<tabLabelBierN> {

    /**
     * interface
     */
    public final ipFwdIface ifc;

    /**
     * next hop
     */
    public final addrIP hop;

    /**
     * remote label
     */
    public final int lab;

    /**
     * bsl value
     */
    public int len;

    /**
     * needed indexes
     */
    public List<Byte> ned = new ArrayList<Byte>();

    /**
     * create new bier peer
     *
     * @param iface interface
     * @param nxtHop next hop
     * @param label labels
     */
    public tabLabelBierN(tabRouteIface iface, addrIP nxtHop, int label) {
        ifc = (ipFwdIface) iface;
        hop = nxtHop.copyBytes();
        lab = label;
    }

    /**
     * copy bytee
     *
     * @return copy
     */
    public tabLabelBierN copyBytes() {
        tabLabelBierN n = new tabLabelBierN(ifc, hop, lab);
        n.len = len;
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
        if (ifc != o.ifc) {
            return true;
        }
        if (lab != o.lab) {
            return true;
        }
        if (len != o.len) {
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

    public int compare(tabLabelBierN o1, tabLabelBierN o2) {
        if (o1.ifc.ifwNum < o2.ifc.ifwNum) {
            return -1;
        }
        if (o1.ifc.ifwNum > o2.ifc.ifwNum) {
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
        return hop + "," + ifc + "," + lab;
    }

}
