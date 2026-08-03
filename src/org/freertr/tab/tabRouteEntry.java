package org.freertr.tab;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrPrefix;
import org.freertr.addr.addrType;
import org.freertr.ip.ipFwd;
import org.freertr.user.userFormat;
import org.freertr.util.bits;
import org.freertr.util.counter;

/**
 * represents one route table entry
 *
 * @param <T> class of address
 * @author matecsaba
 */
public class tabRouteEntry<T extends addrType> implements Comparable<tabRouteEntry<T>> {

    /**
     * route distinguisher
     */
    public long rouDst;

    /**
     * old route distinguisher
     */
    public long oldDst;

    /**
     * prefix
     */
    public addrPrefix<T> prefix;

    /**
     * nlri
     */
    public byte[] nlri;

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
        cntr = new counter();
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
            if (best.isOtherBetter(ntry)) {
                best = ntry;
            }
        }
    }

    /**
     * select best route
     */
    public void hashBest() {
        best = alts.get(prefix.network.getHashB() % alts.size());
    }

    /**
     * find alternate by id
     *
     * @param id id to find
     * @return entry number, -1 if not found
     */
    public int findId(int id) {
        for (int i = 0; i < alts.size(); i++) {
            if (alts.get(i).ident == id) {
                return i;
            }
        }
        return -1;
    }

    /**
     * reduce to best route
     */
    public void reduce2best() {
        alts.clear();
        alts.add(best);
    }

    /**
     * find same forwarder
     *
     * @param other attribute to find
     * @return instance, null if not found
     */
    public tabRouteAttr<T> sameFwder(tabRouteAttr<T> other) {
        if (other.nextHop == null) {
            return null;
        }
        for (int i = 0; i < alts.size(); i++) {
            tabRouteAttr<T> ntry = alts.get(i);
            if (ntry.iface != other.iface) {
                continue;
            }
            if (ntry.rouTab != other.rouTab) {
                continue;
            }
            if (ntry.nextHop == null) {
                continue;
            }
            if (other.nextHop.getSize() != ntry.nextHop.getSize()) {
                continue;
            }
            if (other.nextHop.compareTo(ntry.nextHop) == 0) {
                return ntry;
            }
        }
        return null;
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
        prf.oldDst = oldDst;
        prf.cntr = cntr;
        prf.hwCntr = hwCntr;
        if (nlri != null) {
            prf.nlri = new byte[nlri.length];
            bits.byteCopy(nlri, 0, prf.nlri, 0, nlri.length);
        }
        if (prefix != null) {
            prf.prefix = prefix.copyBytes();
        }
        switch (mod) {
            case altEcmp:
                prf.alts.clear();
                for (int i = 0; i < alts.size(); i++) {
                    tabRouteAttr<T> ntry = alts.get(i);
                    tabRouteAttr<T> attr = new tabRouteAttr<T>();
                    ntry.copyBytes(attr, true);
                    prf.alts.add(attr);
                }
                prf.hashBest();
                return prf;
            case ecmp:
                prf.alts.clear();
                for (int i = 0; i < alts.size(); i++) {
                    tabRouteAttr<T> ntry = alts.get(i);
                    if (ntry.isOtherBetter(best)) {
                        continue;
                    }
                    tabRouteAttr<T> attr = new tabRouteAttr<T>();
                    ntry.copyBytes(attr, true);
                    prf.alts.add(attr);
                }
                prf.hashBest();
                return prf;
            case alters:
                prf.alts.clear();
                for (int i = 0; i < alts.size(); i++) {
                    tabRouteAttr<T> ntry = alts.get(i);
                    tabRouteAttr<T> attr = new tabRouteAttr<T>();
                    ntry.copyBytes(attr, true);
                    prf.alts.add(attr);
                }
                prf.selectBest();
                return prf;
            case lnkEcmp:
            case lnkBcmp:
                prf.alts.clear();
                for (int i = 0; i < alts.size(); i++) {
                    tabRouteAttr<T> ntry = alts.get(i);
                    if (ntry.isOtherBetter(best)) {
                        continue;
                    }
                    prf.alts.add(ntry);
                }
                if (mod == tabRoute.addType.lnkBcmp) {
                    prf.selectBest();
                } else {
                    prf.hashBest();
                }
                return prf;
            case lnkAlters:
                prf.alts.clear();
                for (int i = 0; i < alts.size(); i++) {
                    prf.alts.add(alts.get(i));
                }
                prf.selectBest();
                return prf;
            default:
                best.copyBytes(prf.best, true);
                return prf;
        }
    }

    /**
     * add attribute
     *
     * @param other attribute to add
     */
    public void addAlt(tabRouteAttr<T> other) {
        alts.add(other);
    }

    /**
     * add attribute
     *
     * @param idx index
     * @param other attribute to add
     */
    public void setAlt(int idx, tabRouteAttr<T> other) {
        alts.set(idx, other);
    }

    /**
     * add attribute
     *
     * @param others attributes to add
     */
    public void addAlt(List<tabRouteAttr<T>> others) {
        alts.addAll(others);
    }

    /**
     * add attribute
     *
     * @param idx index
     */
    public void delAlt(int idx) {
        alts.remove(idx);
    }

    /**
     * check if differs from other
     *
     * @param mod mode to use
     * @param other other to test
     * @return 0 if equals, greater than 0 if differs
     */
    public int differs(tabRoute.addType mod, tabRouteEntry<T> other) {
        if (other == null) {
            return 1001;
        }
        if (compareTo(other) != 0) {
            return 1002;
        }
        switch (mod) {
            case alters:
                if (alts.size() != other.alts.size()) {
                    return 1003;
                }
                for (int i = 0; i < alts.size(); i++) {
                    int o = alts.get(i).differs(other.alts.get(i));
                    if (o != 0) {
                        return o;
                    }
                }
                return 0;
            case never:
                return 0;
            default:
                return best.differs(other.best);
        }
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
        String a = toShBgpFirst(prf);
        for (int i = 0; i < prf.alts.size(); i++) {
            tabRouteAttr<addrIP> attr = prf.alts.get(i);
            l.add(tabRouteAttr.rouTyp2string(attr) + "|" + a + "|" + attr.toShRoute());
        }
    }

    /**
     * convert to asname format
     *
     * @param l list to append
     * @param prf entry to dump
     */
    public static void toShAsName(userFormat l, tabRouteEntry<addrIP> prf) {
        String a = toShBgpFirst(prf);
        for (int i = 0; i < prf.alts.size(); i++) {
            tabRouteAttr<addrIP> attr = prf.alts.get(i);
            l.add(a + attr.toShAsName());
        }
    }

    /**
     * convert to asinfo format
     *
     * @param l list to append
     * @param prf entry to dump
     */
    public static void toShAsMixed(userFormat l, tabRouteEntry<addrIP> prf) {
        String a = toShBgpFirst(prf);
        for (int i = 0; i < prf.alts.size(); i++) {
            tabRouteAttr<addrIP> attr = prf.alts.get(i);
            l.add(a + attr.toShAsMixed());
        }
    }

    /**
     * convert to asinfo format
     *
     * @param l list to append
     * @param prf entry to dump
     */
    public static void toShAsInfo(userFormat l, tabRouteEntry<addrIP> prf) {
        String a = toShBgpFirst(prf);
        for (int i = 0; i < prf.alts.size(); i++) {
            tabRouteAttr<addrIP> attr = prf.alts.get(i);
            l.add(a + attr.toShAsInfo());
        }
    }

    /**
     * convert to changed format
     *
     * @param l list to append
     * @param prf entry to dump
     */
    public static void toShChgRoute(userFormat l, tabRouteEntry<addrIP> prf) {
        String a = toShBgpFirst(prf);
        for (int i = 0; i < prf.alts.size(); i++) {
            tabRouteAttr<addrIP> attr = prf.alts.get(i);
            l.add(a + "|" + attr.toShChgRoute());
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
        String a = addrPrefix.ip2evpn(prf.prefix) + " " + tabRouteUtil.rd2string(prf.rouDst);
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
        String tp = "";
        String tb = "";
        String rp = "";
        String rb = "";
        if (prf.hwCntr != null) {
            tp = "+" + prf.hwCntr.packTx;
            tb = "+" + prf.hwCntr.byteTx;
            rp = "+" + prf.hwCntr.packRx;
            rb = "+" + prf.hwCntr.byteRx;
        }
        l.add(addrPrefix.ip2str(prf.prefix) + "|" + prf.cntr.packTx + tp + "|" + prf.cntr.byteTx + tb + "|" + prf.cntr.packRx + rp + "|" + prf.cntr.byteRx + rb + "|" + bits.timePast(prf.best.time));
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
            s = " " + tabRouteUtil.rd2string(prf.rouDst);
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
        String a = toShBgpFirst(prf);
        for (int i = 0; i < prf.alts.size(); i++) {
            tabRouteAttr<addrIP> attr = prf.alts.get(i);
            l.add(a + "|" + attr.toShRpki());
        }
    }

    /**
     * convert to ecmp format
     *
     * @param l list to append
     * @param prf entry to dump
     * @param evpn evpn
     */
    public static void toShEcmp(userFormat l, tabRouteEntry<addrIP> prf, boolean evpn) {
        String a;
        if (evpn) {
            a = addrPrefix.ip2evpn(prf.prefix);
        } else {
            a = toShBgpFirst(prf);
        }
        for (int i = 0; i < prf.alts.size(); i++) {
            tabRouteAttr<addrIP> attr = prf.alts.get(i);
            l.add(a + "|" + prf.alts.size() + "|" + (!attr.isOtherBetter(prf.best)) + "|" + (attr == prf.best) + "|" + attr.toShEcmp());
        }
    }

    /**
     * full dump of this prefix
     *
     * @param beg beginning
     * @param fwd forwarding core to use
     * @return list describes this prefix
     */
    @SuppressWarnings("unchecked")
    public userFormat fullDump(String beg, ipFwd fwd) {
        userFormat lst = new userFormat("|", "id|category|value");
        if (fwd != null) {
            lst.add(beg + "|vrf|" + fwd.vrfName);
            lst.add(beg + "|ipver|" + fwd.ipVersion);
        }
        lst.add(beg + "|rd|" + tabRouteUtil.rd2string(rouDst));
        lst.add(beg + "|original rd|" + tabRouteUtil.rd2string(oldDst));
        try {
            lst.add(beg + "|prefix|" + addrPrefix.ip2str((addrPrefix<addrIP>) prefix));
        } catch (Exception e) {
            lst.add(beg + "|prefix|" + prefix);
        }
        lst.add(beg + "|prefix network|" + addrType.any2str(prefix.network));
        lst.add(beg + "|prefix broadcast|" + addrType.any2str(prefix.broadcast));
        lst.add(beg + "|prefix wildcard|" + addrType.any2str(prefix.wildcard));
        lst.add(beg + "|prefix netmask|" + addrType.any2str(prefix.mask));
        lst.add(beg + "|nlri|" + bits.byteDump(nlri, 0, -1));
        lst.add(beg + "|alternates|" + alts.size());
        for (int i = 0; i < alts.size(); i++) {
            tabRouteAttr<T> ntry = alts.get(i);
            lst.add(beg + "|alternate #" + i + "|candidate=" + (!ntry.isOtherBetter(best)) + " best=" + (ntry == best));
            ntry.fullDump(lst, beg + " alt" + i + "|");
        }
        lst.add(beg + "|counter|" + cntr.getShStat());
        lst.add(beg + "|lastio|" + cntr.getShTraff());
        lst.add(beg + "|hardware counter|" + counter.getShStat(hwCntr));
        return lst;
    }

    public int compareTo(tabRouteEntry<T> o) {
        if (rouDst < o.rouDst) {
            return -1;
        }
        if (rouDst > o.rouDst) {
            return +1;
        }
        return prefix.compareTo(o.prefix);
    }

}
