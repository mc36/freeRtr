package org.freertr.ip;

import java.util.Comparator;
import org.freertr.addr.addrIP;
import org.freertr.tab.tabLabelEntry;

/**
 * stores one multipoint neighbor
 *
 * @author matecsaba
 */
public class ipFwdMpNe implements Comparator<ipFwdMpNe> {

    /**
     * peer address
     */
    public final addrIP addr;

    /**
     * interface to use
     */
    public ipFwdIface iface;

    /**
     * remote label
     */
    public int labelR;

    /**
     * local label
     */
    public tabLabelEntry labelL;

    /**
     * create instance
     *
     * @param per peer address
     */
    public ipFwdMpNe(addrIP per) {
        addr = per.copyBytes();
    }

    /**
     * copy bytes
     *
     * @return copy of record
     */
    public ipFwdMpNe copyBytes() {
        ipFwdMpNe n = new ipFwdMpNe(addr);
        n.iface = iface;
        if (labelL != null) {
            n.labelL = labelL.copyBytes();
        }
        n.labelR = labelR;
        return n;
    }

    /**
     * compare this entry
     *
     * @param o other
     * @return false if equals, true if differs
     */
    public boolean differs(ipFwdMpNe o) {
        if (o == null) {
            return true;
        }
        if (iface != o.iface) {
            return true;
        }
        if (labelR != o.labelR) {
            return true;
        }
        if (labelL == null) {
            if (o.labelL != null) {
                return true;
            }
        } else {
            if (labelL.differs(o.labelL)) {
                return true;
            }
        }
        if (addr.compare(addr, o.addr) != 0) {
            return true;
        }
        return false;
    }

    public int compare(ipFwdMpNe o1, ipFwdMpNe o2) {
        return o1.addr.compare(o1.addr, o2.addr);
    }

}
