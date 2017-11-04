package tab;

import addr.addrIP;
import ip.ipFwdIface;
import java.util.Comparator;
import java.util.List;

/**
 * represents one label duplicating entry
 *
 * @author matecsaba
 */
public class tabLabelDup implements Comparator<tabLabelDup> {

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
    public final List<Integer> lab;

    /**
     * create new duplicator
     *
     * @param iface interface
     * @param nxtHop next hop
     * @param labels labels
     */
    public tabLabelDup(ipFwdIface iface, addrIP nxtHop, List<Integer> labels) {
        ifc = iface;
        hop = nxtHop.copyBytes();
        if (labels == null) {
            lab = null;
        } else {
            lab = tabLabel.copyLabels(labels);
        }
    }

    public int compare(tabLabelDup o1, tabLabelDup o2) {
        return o1.hop.compare(o1.hop, o2.hop);
    }

    public String toString() {
        String s = "";
        if (lab != null) {
            for (int i = 0; i < lab.size(); i++) {
                s += " " + lab.get(i);
            }
        }
        return "duplicate|" + hop + " " + ifc + s;
    }

}
