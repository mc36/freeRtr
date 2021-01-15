package tab;

import addr.addrIP;
import ip.ipFwdIface;
import java.math.BigInteger;
import java.util.Comparator;

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
    public BigInteger ned;

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
        return hop + " " + ifc + " lab=" + lab + " bit=" + ned.bitCount() + "/" + ned.bitLength() + ":" + ned.toString(16);
    }

}
