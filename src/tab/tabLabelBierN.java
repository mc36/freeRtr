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

    public String toString() {
        String a = "";
        for (int i = ned.size() - 1; i >= 0; i--) {
            a += bits.toHexB(ned.get(i));
        }
        return hop + " " + ifc + " lab=" + lab + " len=" + (ned.size() * 8) + " bit=" + a;
    }

}
