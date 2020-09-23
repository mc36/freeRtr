package tab;

import addr.addrIP;
import addr.addrPrefix;
import addr.addrType;
import ip.ipFwd;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import user.userFormat;
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
     * clone this table entry
     *
     * @param mod mode to use
     * @return new instance containing same data
     */
    public tabRouteEntry<T> copyBytes(tabRoute.addType mod) {
        tabRouteEntry<T> prf = new tabRouteEntry<T>();
        prf.rouDst = rouDst;
        prf.cntr = cntr;
        prf.hwCntr = hwCntr;
        if (prefix != null) {
            prf.prefix = prefix.copyBytes();
        }
        best.copyBytes(prf.best, true);
        switch (mod) {
            case ecmp:
                for (int i = 0; i < alts.size(); i++) {
                    tabRouteAttr<T> ntry = alts.get(i);
                    if (ntry.isOtherBetter(best, false)) {
                        continue;
                    }
                    tabRouteAttr<T> attr = new tabRouteAttr<T>();
                    ntry.copyBytes(attr, true);
                    prf.alts.add(attr);
                }
                prf.selectBest();
                return prf;
            case alters:
                for (int i = 0; i < alts.size(); i++) {
                    tabRouteAttr<T> ntry = alts.get(i);
                    tabRouteAttr<T> attr = new tabRouteAttr<T>();
                    ntry.copyBytes(attr, true);
                    prf.alts.add(attr);
                }
                prf.selectBest();
                return prf;
            default:
                return prf;
        }
    }

    /**
     * check if differs from other
     *
     * @param mod mode to use
     * @param other other to test
     * @return false on equals, true on differs
     */
    public boolean differs(tabRoute.addType mod, tabRouteEntry<T> other) {
        if (other == null) {
            return true;
        }
        if (compare(this, other) != 0) {
            return true;
        }
        switch (mod) {
            case alters:
                if (alts.size() != other.alts.size()) {
                    return true;
                }
                for (int i = 0; i < alts.size(); i++) {
                    if (alts.get(i).differs(other.alts.get(i))) {
                        return true;
                    }
                }
                return false;
            default:
                return best.differs(other.best);
        }
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
     * @param l list to append
     * @param prf entry to dump
     */
    public static void toShSrRoute(userFormat l, tabRouteEntry<addrIP> prf) {
        String a = addrPrefix.ip2str(prf.prefix);
        for (int i = 0; i < prf.alts.size(); i++) {
            tabRouteAttr<addrIP> attr = prf.alts.get(i);
            if (attr.segrouIdx < 1) {
                continue;
            }
            l.add(a + "|" + attr.toShSrRoute());
        }
    }

    /**
     * convert to route format
     *
     * @param l list to append
     * @param prf entry to dump
     */
    public static void toShBrRoute(userFormat l, tabRouteEntry<addrIP> prf) {
        String a = addrPrefix.ip2str(prf.prefix);
        for (int i = 0; i < prf.alts.size(); i++) {
            tabRouteAttr<addrIP> attr = prf.alts.get(i);
            if (attr.bierIdx < 1) {
                continue;
            }
            l.add(a + "|" + attr.toShBrRoute());
        }
    }

    /**
     * convert to route format
     *
     * @param l list to append
     * @param prf entry to dump
     */
    public static void toShRoute(userFormat l, tabRouteEntry<addrIP> prf) {
        String a = addrPrefix.ip2str(prf.prefix);
        for (int i = 0; i < prf.alts.size(); i++) {
            tabRouteAttr<addrIP> attr = prf.alts.get(i);
            l.add(tabRouteAttr.rouTyp2string(attr) + "|" + a + "|" + attr.toShRoute());
        }
    }

    /**
     * convert to bgp format
     *
     * @param l list to append
     * @param prf entry to dump
     */
    public static void toShBgp(userFormat l, tabRouteEntry<addrIP> prf) {
        String a = toShBgpFirst(prf);
        for (int i = 0; i < prf.alts.size(); i++) {
            tabRouteAttr<addrIP> attr = prf.alts.get(i);
            l.add(a + attr.toShBgpLast());
        }
    }

    /**
     * convert to evpn format
     *
     * @param l list to append
     * @param prf entry to dump
     */
    public static void toShEvpn(userFormat l, tabRouteEntry<addrIP> prf) {
        String a = addrPrefix.ip2evpn(prf.prefix) + " " + tabRtrmapN.rd2string(prf.rouDst);
        for (int i = 0; i < prf.alts.size(); i++) {
            tabRouteAttr<addrIP> attr = prf.alts.get(i);
            l.add(a + attr.toShBgpLast());
        }
    }

    /**
     * convert to bgp format
     *
     * @param l list to append
     * @param prf entry to dump
     * @param evpn evpn
     */
    public static void toShBgpLabels(userFormat l, tabRouteEntry<addrIP> prf, boolean evpn) {
        String a;
        if (evpn) {
            a = addrPrefix.ip2evpn(prf.prefix);
        } else {
            a = toShBgpFirst(prf);
        }
        for (int i = 0; i < prf.alts.size(); i++) {
            tabRouteAttr<addrIP> attr = prf.alts.get(i);
            l.add(a + "|" + attr.toShBgpLabels());
        }
    }

    /**
     * convert to counter format
     *
     * @param l list to append
     * @param prf entry to dump
     */
    public static void toShCntr(userFormat l, tabRouteEntry<addrIP> prf) {
        if (prf.cntr == null) {
            return;
        }
        String a = "";
        String s = "";
        if (prf.hwCntr != null) {
            a = "+" + prf.hwCntr.packTx;
            s = "+" + prf.hwCntr.byteTx;
        }
        l.add(addrPrefix.ip2str(prf.prefix) + "|" + prf.cntr.packTx + a + "|" + prf.cntr.byteTx + s + "|" + bits.timePast(prf.best.time));
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
     * @param l list to append
     * @param prf entry to dump
     */
    public static void toShLdp(userFormat l, tabRouteEntry<addrIP> prf) {
        String a = addrPrefix.ip2str(prf.prefix);
        for (int i = 0; i < prf.alts.size(); i++) {
            tabRouteAttr<addrIP> attr = prf.alts.get(i);
            l.add(a + "|" + attr.toShLdp());
        }
    }

    /**
     * convert to rpki format
     *
     * @param l list to append
     * @param prf entry to dump
     */
    public static void toShRpki(userFormat l, tabRouteEntry<addrIP> prf) {
        String a = addrPrefix.ip2str(prf.prefix);
        for (int i = 0; i < prf.alts.size(); i++) {
            tabRouteAttr<addrIP> attr = prf.alts.get(i);
            l.add(a + "|" + attr.toShRpki());
        }
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
        l.add("alternates = " + alts.size());
        for (int i = 0; i < alts.size(); i++) {
            l.add("alternate #" + i + " attributes:");
            alts.get(i).fullDump(l);
        }
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
