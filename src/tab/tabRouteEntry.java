package tab;

import addr.addrIP;
import addr.addrPrefix;
import addr.addrType;
import ip.ipFwd;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import util.bits;
import util.counter;

/**
 * represents one route table entry
 *
 * @param <T> class of address
 * @author matecsaba
 */
public class tabRouteEntry<T extends addrType> implements Comparator<tabRouteEntry<T>> {

    /**
     * route distinguisher
     */
    public long rouDst;

    /**
     * prefix
     */
    public addrPrefix<T> prefix;

    /**
     * best path
     */
    public tabRouteAttr<T> best;

    /**
     * alternate instances
     */
    public List<tabRouteAttr<T>> alts;

    /**
     * counter
     */
    public counter cntr;

    /**
     * hardware counter
     */
    public counter hwCntr;

    /**
     * create instance
     */
    public tabRouteEntry() {
        best = new tabRouteAttr<T>();
        alts = new ArrayList<tabRouteAttr<T>>();
        alts.add(best);
    }

    public String toString() {
        return "" + prefix;
    }

    /**
     * clone this table entry
     *
     * @return new instance containing same data
     */
    public tabRouteEntry<T> copyBytes() {
        tabRouteEntry<T> prf = new tabRouteEntry<T>();
        prf.rouDst = rouDst;
        if (prefix != null) {
            prf.prefix = prefix.copyBytes();
        }
        best.copyBytes(prf.best, true);
        prf.cntr = cntr;
        prf.hwCntr = hwCntr;
        return prf;
    }

    /**
     * select best route
     */
    public void selectBest() {
        best = alts.get(0);
        for (int i = 1; i < alts.size(); i++) {
            tabRouteAttr<T> ntry = alts.get(i);
            if (best.isOtherBetter(ntry, true)) {
                best = ntry;
            }
        }
    }

    /**
     * check if differs from other
     *
     * @param other other to test
     * @return false on equals, true on differs
     */
    public boolean differs(tabRouteEntry<T> other) {
        if (other == null) {
            return true;
        }
        if (compare(this, other) != 0) {
            return true;
        }
        return best.differs(other.best);
    }

    /**
     * need to update with this prefix
     *
     * @param imp new prefix
     * @return true if yes, false if not
     */
    public boolean isOtherBetter(tabRouteEntry<T> imp) {
        return best.isOtherBetter(imp.best, true);
    }

    /**
     * convert to route format
     *
     * @param prf entry to dump
     * @return converted
     */
    public static String toShSrRoute(tabRouteEntry<addrIP> prf) {
        if (prf.best.segrouIdx < 1) {
            return null;
        }
        return addrPrefix.ip2str(prf.prefix) + "|" + prf.best.toShSrRoute();
    }

    /**
     * convert to route format
     *
     * @param prf entry to dump
     * @return converted
     */
    public static String toShBrRoute(tabRouteEntry<addrIP> prf) {
        if (prf.best.bierIdx < 1) {
            return null;
        }
        return addrPrefix.ip2str(prf.prefix) + "|" + prf.best.toShBrRoute();
    }

    /**
     * convert to route format
     *
     * @param prf entry to dump
     * @return converted
     */
    public static String toShRoute(tabRouteEntry<addrIP> prf) {
        return tabRouteAttr.rouTyp2string(prf.best) + "|" + addrPrefix.ip2str(prf.prefix) + "|" + prf.best.toShRoute();
    }

    /**
     * convert to bgp format
     *
     * @param prf entry to dump
     * @return converted
     */
    public static String toShBgp(tabRouteEntry<addrIP> prf) {
        return toShBgpFirst(prf) + prf.best.toShBgpLast();
    }

    /**
     * convert to evpn format
     *
     * @param prf entry to dump
     * @return converted
     */
    public static String toShEvpn(tabRouteEntry<addrIP> prf) {
        return addrPrefix.ip2evpn(prf.prefix) + " " + tabRtrmapN.rd2string(prf.rouDst) + prf.best.toShBgpLast();
    }

    /**
     * convert to bgp format
     *
     * @param prf entry to dump
     * @param evpn evpn
     * @return converted
     */
    public static String toShBgpLabels(tabRouteEntry<addrIP> prf, boolean evpn) {
        String a;
        if (evpn) {
            a = addrPrefix.ip2evpn(prf.prefix);
        } else {
            a = toShBgpFirst(prf);
        }
        return a + "|" + prf.best.toShBgpLabels();
    }

    /**
     * convert to counter format
     *
     * @param prf entry to dump
     * @return converted
     */
    public static String toShCntr(tabRouteEntry<addrIP> prf) {
        if (prf.cntr == null) {
            return null;
        }
        String a = "";
        String s = "";
        if (prf.hwCntr != null) {
            a = "+" + prf.hwCntr.packTx;
            s = "+" + prf.hwCntr.byteTx;
        }
        return addrPrefix.ip2str(prf.prefix) + "|" + prf.cntr.packTx + a + "|" + prf.cntr.byteTx + s + "|" + bits.timePast(prf.best.time);
    }

    /**
     * convert to bgp format
     *
     * @param prf entry to dump
     * @return converted
     */
    public static String toShBgpFirst(tabRouteEntry<addrIP> prf) {
        String s = "";
        if (prf.rouDst != 0) {
            s = " " + tabRtrmapN.rd2string(prf.rouDst);
        }
        return addrPrefix.ip2str(prf.prefix) + s;
    }

    /**
     * convert to ldp format
     *
     * @param prf entry to dump
     * @return converted
     */
    public static String toShLdp(tabRouteEntry<addrIP> prf) {
        return addrPrefix.ip2str(prf.prefix) + "|" + prf.best.toShLdp();
    }

    /**
     * convert to rpki format
     *
     * @param prf entry to dump
     * @return converted
     */
    public static String toShRpki(tabRouteEntry<addrIP> prf) {
        return addrPrefix.ip2str(prf.prefix) + "|" + prf.best.toShRpki();
    }

    /**
     * full dump of this prefix
     *
     * @param fwd forwarding core to use
     * @return list describes this prefix
     */
    public List<String> fullDump(ipFwd fwd) {
        List<String> l = new ArrayList<String>();
        if (fwd != null) {
            l.add("vrf = " + fwd.vrfName);
            l.add("ipver = " + fwd.ipVersion);
        }
        l.add("rd = " + tabRtrmapN.rd2string(rouDst));
        l.add("prefix = " + prefix);
        l.add("prefix network = " + prefix.network);
        l.add("prefix broadcast = " + prefix.broadcast);
        l.add("prefix wildcard = " + prefix.wildcard);
        l.add("prefix netmask = " + prefix.mask);
        best.fullDump(l);
        l.add("counter = " + counter.getShStat(cntr));
        l.add("hardware counter = " + counter.getShStat(hwCntr));
        return l;
    }

    public int compare(tabRouteEntry<T> o1, tabRouteEntry<T> o2) {
        if (o1.rouDst < o2.rouDst) {
            return -1;
        }
        if (o1.rouDst > o2.rouDst) {
            return +1;
        }
        return prefix.compare(o1.prefix, o2.prefix);
    }

}
