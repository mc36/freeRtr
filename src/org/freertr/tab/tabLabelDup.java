package org.freertr.tab;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.ip.ipFwdIface;

/**
 * represents one label duplicating entry
 *
 * @author matecsaba
 */
public class tabLabelDup implements Comparable<tabLabelDup> {

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
    public final List<Integer> label;

    /**
     * create new duplicator
     *
     * @param ifa interface
     * @param nxtHop next hop
     * @param lab labels
     */
    public tabLabelDup(ipFwdIface ifa, addrIP nxtHop, List<Integer> lab) {
        iface = ifa;
        hop = nxtHop.copyBytes();
        label = tabLabel.copyLabels(lab);
    }

    /**
     * compare this entry
     *
     * @param o other
     * @return false if equals, true if differs
     */
    public boolean differs(tabLabelDup o) {
        if (o == null) {
            return true;
        }
        if (iface != o.iface) {
            return true;
        }
        if (hop.compareTo(o.hop) != 0) {
            return true;
        }
        return tabRouteUtil.diffIntList(label, o.label);
    }

    public int compareTo(tabLabelDup o) {
        return hop.compareTo(o.hop);
    }

    public String toString() {
        String s = "";
        if (label != null) {
            for (int i = 0; i < label.size(); i++) {
                s += " " + label.get(i);
            }
        }
        return "duplicate|" + hop + " " + iface + s;
    }

}
