package net.freertr.tab;

import java.util.ArrayList;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrPrefix;
import net.freertr.addr.addrType;
import net.freertr.ip.ipMpls;
import net.freertr.util.bits;
import net.freertr.util.debugger;
import net.freertr.util.logger;

/**
 * represents one route table
 *
 * @param <T> class of address
 * @author matecsaba
 */
public class tabRoute<T extends addrType> {

    /**
     * add type
     */
    public enum addType {
        /**
         * add if not exists yet
         */
        notyet,
        /**
         * add if better
         */
        better,
        /**
         * add as ecmp if not better, overwrite otherwise
         */
        ecmp,
        /**
         * add always
         */
        always,
        /**
         * add as alternatives
         */
        alters,
        /**
         * ecmp but with linking copy mode
         */
        lnkEcmp,
        /**
         * alternatives but with linking copy mode
         */
        lnkAlters,
        /**
         * ecmp but assume ecmp on copy
         */
        altEcmp,
    }

    /**
     * default distance of prefixes
     */
    public int defDist = 255;

    /**
     * default metric of prefixes
     */
    public int defMetr = 0;

    /**
     * default protocol identifier number
     */
    public int defProto = -1;

    /**
     * default route type
     */
    public tabRouteAttr.routeType defRouTyp = null;

    /**
     * list of prefixes
     */
    protected final tabGen<tabRouteEntry<T>> prefixes;

    /**
     * version of this table
     */
    public long version = 1;

    /**
     * name of routing table
     */
    public final String tabName;

    public String toString() {
        return tabName + " table";
    }

    /**
     * create empty table
     *
     * @param nam name of table
     */
    public tabRoute(String nam) {
        tabName = nam;
        prefixes = new tabGen<tabRouteEntry<T>>();
    }

    /**
     * copy the list (thread safely)
     *
     * @param orig source
     */
    public tabRoute(tabRoute<T> orig) {
        tabName = orig.tabName;
        defDist = orig.defDist;
        defMetr = orig.defMetr;
        defProto = orig.defProto;
        defRouTyp = orig.defRouTyp;
        prefixes = new tabGen<tabRouteEntry<T>>(orig.prefixes);
    }

    /**
     * optimize for lookup
     */
    public void optimize4lookup() {
        prefixes.optimize4lookup();
    }

    /**
     * get table info
     *
     * @return info
     */
    public String tableInfo() {
        return prefixes.tableInfo();
    }

    /**
     * check consistency of table
     *
     * @return -1 on success, failing index otherwise
     */
    public int checkConsistency() {
        if (size() < 2) {
            return -1;
        }
        tabRouteEntry<T> lst = get(0);
        for (int i = 1; i < size(); i++) {
            tabRouteEntry<T> cur = get(i);
            if (lst.compare(lst, cur) >= 0) {
                return i;
            }
            lst = cur;
        }
        return -1;
    }

    /**
     * clear all prefixes from table
     */
    public void clear() {
        if (debugger.tabRouteEvnt) {
            logger.debug("clear");
        }
        prefixes.clear();
        version++;
    }

    /**
     * add one table entry with preset values
     *
     * @param mod mode to use
     * @param prefix entry to add
     * @param copy set true to add just a copy of this prefix
     * @param newTime set true to set time, false to keep original time
     */
    public void add(addType mod, tabRouteEntry<T> prefix, boolean copy, boolean newTime) {
        if (debugger.tabRouteEvnt) {
            logger.debug("add " + prefix);
        }
        if (copy) {
            prefix = prefix.copyBytes(mod);
        }
        if (newTime) {
            long tim = bits.getTime();
            for (int i = 0; i < prefix.alts.size(); i++) {
                prefix.alts.get(i).time = tim;
            }
        }
        switch (mod) {
            case altEcmp:
                tabRouteEntry<T> own = prefixes.add(prefix);
                if (own == null) {
                    version++;
                    return;
                }
                if (own.best.isOtherBetter(prefix.best, false)) {
                    prefixes.put(prefix);
                    version++;
                    return;
                }
                if (prefix.best.isOtherBetter(own.best, false)) {
                    return;
                }
                for (int i = 0; i < prefix.alts.size(); i++) {
                    tabRouteAttr<T> ntry = prefix.alts.get(i);
                    own.alts.add(ntry);
                }
                own.hashBest();
                version++;
                return;
            case lnkEcmp:
            case ecmp:
                own = prefixes.add(prefix);
                if (own == null) {
                    version++;
                    return;
                }
                if (own.best.isOtherBetter(prefix.best, false)) {
                    prefixes.put(prefix);
                    version++;
                    return;
                }
                if (prefix.best.isOtherBetter(own.best, false)) {
                    return;
                }
                for (int i = 0; i < prefix.alts.size(); i++) {
                    tabRouteAttr<T> ntry = prefix.alts.get(i);
                    if (ntry.isOtherBetter(own.best, false)) {
                        continue;
                    }
                    own.alts.add(ntry);
                }
                own.hashBest();
                version++;
                return;
            case better:
                if (prefix.alts.size() > 1) {
                    prefix = prefix.copyBytes(mod);
                }
                own = prefixes.add(prefix);
                if (own == null) {
                    version++;
                    return;
                }
                if (!own.isOtherBetter(prefix)) {
                    return;
                }
                prefixes.put(prefix);
                version++;
                return;
            case always:
                prefixes.put(prefix);
                version++;
                return;
            case notyet:
                if (prefix.alts.size() > 1) {
                    prefix = prefix.copyBytes(mod);
                }
                if (prefixes.add(prefix) != null) {
                    return;
                }
                version++;
                return;
            case lnkAlters:
            case alters:
                own = prefixes.add(prefix);
                if (own == null) {
                    version++;
                    return;
                }
                for (int i = 0; i < prefix.alts.size(); i++) {
                    own.alts.add(prefix.alts.get(i));
                }
                own.selectBest();
                version++;
                return;
            default:
                return;
        }
    }

    /**
     * add one table entry with default values
     *
     * @param mod mode to use
     * @param prefix prefix to store
     * @param nextHop address of nexthop
     * @return the newly added prefix
     */
    @SuppressWarnings("unchecked")
    public tabRouteEntry<T> add(addType mod, addrPrefix<T> prefix, T nextHop) {
        tabRouteEntry<T> prf = new tabRouteEntry<T>();
        prf.prefix = prefix.copyBytes();
        if (nextHop != null) {
            prf.best.nextHop = (T) nextHop.copyBytes();
        }
        prf.best.distance = defDist;
        prf.best.metric = defMetr;
        prf.best.protoNum = defProto;
        prf.best.rouTyp = defRouTyp;
        add(mod, prf, false, true);
        return prf;
    }

    /**
     * delete one table entry
     *
     * @param prefix prefix to delete
     * @return false if deleted, true if not found
     */
    public boolean del(addrPrefix<T> prefix) {
        if (debugger.tabRouteEvnt) {
            logger.debug("del " + prefix);
        }
        tabRouteEntry<T> prf = new tabRouteEntry<T>();
        prf.prefix = prefix;
        if (prefixes.del(prf) == null) {
            return true;
        }
        version++;
        return false;
    }

    /**
     * delete one table entry
     *
     * @param prf prefix to delete
     * @return false if deleted, true if not found
     */
    public boolean del(tabRouteEntry<T> prf) {
        if (debugger.tabRouteEvnt) {
            logger.debug("del " + prf);
        }
        if (prefixes.del(prf) == null) {
            return true;
        }
        version++;
        return false;
    }

    /**
     * find one prefix in table
     *
     * @param prefix prefix to find
     * @return null if not found, the entry from table if found
     */
    public tabRouteEntry<T> find(addrPrefix<T> prefix) {
        tabRouteEntry<T> prf = new tabRouteEntry<T>();
        prf.prefix = prefix;
        return prefixes.find(prf);
    }

    /**
     * find one prefix in table
     *
     * @param prf prefix to find
     * @return null if not found, the entry from table if found
     */
    public tabRouteEntry<T> find(tabRouteEntry<T> prf) {
        return prefixes.find(prf);
    }

    /**
     * count entries in table
     *
     * @return number of entries
     */
    public int size() {
        return prefixes.size();
    }

    /**
     * read table entry
     *
     * @param idx sequence number 0..size-1
     * @return null if not found, the entry from table if found
     */
    public tabRouteEntry<T> get(int idx) {
        return prefixes.get(idx);
    }

    /**
     * delete too distant entries
     *
     * @param distan lower distance (inclusive)
     * @return number of entries imported
     */
    public int delDistance(int distan) {
        int cnt = 0;
        for (int i = prefixes.size() - 1; i >= 0; i--) {
            tabRouteEntry<T> prf = prefixes.get(i);
            if (prf == null) {
                continue;
            }
            if (prf.best.distance < distan) {
                continue;
            }
            if (debugger.tabRouteEvnt) {
                logger.debug("deldst " + prf);
            }
            prefixes.del(prf);
            cnt++;
        }
        if (cnt > 0) {
            version++;
        }
        return cnt;
    }

    /**
     * delete too far entries
     *
     * @param metric lower metric (inclusive)
     * @return number of entries imported
     */
    public int delMetric(int metric) {
        int cnt = 0;
        for (int i = prefixes.size() - 1; i >= 0; i--) {
            tabRouteEntry<T> prf = prefixes.get(i);
            if (prf == null) {
                continue;
            }
            if (prf.best.metric < metric) {
                continue;
            }
            if (debugger.tabRouteEvnt) {
                logger.debug("delmet " + prf);
            }
            prefixes.del(prf);
            cnt++;
        }
        if (cnt > 0) {
            version++;
        }
        return cnt;
    }

    /**
     * delete all entries of a protocol from table
     *
     * @param proto protocol id
     * @return number of entries removed
     */
    public int delProto(int proto) {
        int cnt = 0;
        for (int i = prefixes.size() - 1; i >= 0; i--) {
            tabRouteEntry<T> prf = prefixes.get(i);
            if (prf == null) {
                continue;
            }
            if (prf.best.protoNum != proto) {
                continue;
            }
            if (debugger.tabRouteEvnt) {
                logger.debug("delprt " + prf);
            }
            prefixes.del(prf);
            cnt++;
        }
        if (cnt > 0) {
            version++;
        }
        return cnt;
    }

    /**
     * delete all entries of an interface from table
     *
     * @param iface protocol id
     * @return number of entries removed
     */
    public int delIface(tabRouteIface iface) {
        int cnt = 0;
        for (int i = prefixes.size() - 1; i >= 0; i--) {
            tabRouteEntry<T> prf = prefixes.get(i);
            if (prf == null) {
                continue;
            }
            if (prf.best.iface != iface) {
                continue;
            }
            if (debugger.tabRouteEvnt) {
                logger.debug("delifc " + prf);
            }
            prefixes.del(prf);
            cnt++;
        }
        if (cnt > 0) {
            version++;
        }
        return cnt;
    }

    /**
     * preserve time info
     *
     * @param src source
     */
    public void preserveTime(tabRoute<T> src) {
        for (int i = 0; i < prefixes.size(); i++) {
            tabRouteEntry<T> prf = prefixes.get(i);
            if (prf == null) {
                continue;
            }
            tabRouteEntry<T> old = src.find(prf);
            if (old == null) {
                continue;
            }
            if (prf.differs(addType.notyet, old)) {
                continue;
            }
            for (int o = 0; o < prf.alts.size(); o++) {
                prf.alts.get(o).time = old.best.time;
            }
        }
    }

    /**
     * set protocol info
     *
     * @param typ protocol type
     * @param num protocol number
     */
    public void setProto(tabRouteAttr.routeType typ, int num) {
        for (int i = 0; i < prefixes.size(); i++) {
            tabRouteEntry<T> prf = prefixes.get(i);
            if (prf == null) {
                continue;
            }
            for (int o = 0; o < prf.alts.size(); o++) {
                tabRouteAttr<T> attr = prf.alts.get(o);
                attr.rouTyp = typ;
                attr.protoNum = num;
            }
        }
    }

    /**
     * import all the entries from another table
     *
     * @param mod mode to use
     * @param other table to import
     * @param distan highest allowed distance
     */
    public void mergeFrom(addType mod, tabRoute<T> other, int distan) {
        for (int i = 0; i < other.prefixes.size(); i++) {
            tabRouteEntry<T> imp = other.prefixes.get(i);
            if (imp == null) {
                continue;
            }
            if (imp.best.distance >= distan) {
                continue;
            }
            imp = imp.copyBytes(mod);
            add(mod, imp, false, false);
        }
        if (debugger.tabRouteEvnt) {
            logger.debug("merged prefixes from " + other.defRouTyp);
        }
    }

    /**
     * import all the entries from another table
     *
     * @param mod mode to use
     * @param other table to import
     * @param nexthops table where look up nexthops
     * @param recur maximum recursion depth
     * @param distan highest allowed distance
     */
    public void mergeFrom(addType mod, tabRoute<T> other, tabRoute<T> nexthops, int recur, int distan) {
        for (int i = 0; i < other.prefixes.size(); i++) {
            tabRouteEntry<T> imp = other.prefixes.get(i);
            if (imp == null) {
                continue;
            }
            if (imp.best.distance >= distan) {
                continue;
            }
            imp = imp.copyBytes(mod);
            for (int o = imp.alts.size() - 1; o >= 0; o--) {
                tabRouteAttr<T> attr = imp.alts.get(o);
                if (!doNexthopFix(attr, other, recur, nexthops)) {
                    continue;
                }
                imp.delAlt(o);
            }
            if (imp.alts.size() < 1) {
                continue;
            }
            imp.hashBest();
            add(mod, imp, false, false);
        }
        if (debugger.tabRouteEvnt) {
            logger.debug("merged prefixes from " + other.defRouTyp);
        }
    }

    @SuppressWarnings("unchecked")
    private boolean doNexthopFix(tabRouteAttr<T> attr, tabRoute<T> recurs, int recurn, tabRoute<T> nexthops) {
        T hop = attr.nextHop;
        T orig = hop;
        if (hop == null) {
            return true;
        }
        for (int i = 0; i < recurn; i++) {
            tabRouteEntry<T> nhr = nexthops.route(hop);
            if (nhr == null) {
                nhr = recurs.route(hop);
                if (nhr == null) {
                    return true;
                }
                hop = nhr.best.nextHop;
                if (hop == null) {
                    return true;
                }
                attr.oldHop = orig;
                attr.nextHop = (T) hop.copyBytes();
                continue;
            }
            if (nhr.best.nextHop != null) {
                attr.oldHop = orig;
                attr.nextHop = (T) nhr.best.nextHop.copyBytes();
            }
            attr.iface = nhr.best.iface;
            return false;
        }
        return true;
    }

    /**
     * route lookup one destination address
     *
     * @param addr address to look up
     * @return route table entry matching the route, null if not found
     */
    public tabRouteEntry<T> route(T addr) {
        tabRouteEntry<T> prf = new tabRouteEntry<T>();
        prf.prefix = new addrPrefix<T>(addr, addr.maxBits());
        for (int o = prf.prefix.maskLen; o >= 0; o--) {
            prf.prefix.setMask(o);
            tabRouteEntry<T> res = prefixes.find(prf);
            if (res != null) {
                return res;
            }
        }
        return null;
    }

    /**
     * test if this table differs from the other
     *
     * @param mod mode to use
     * @param other to compare with
     * @return false if identical, true if differs
     */
    public boolean differs(addType mod, tabRoute<T> other) {
        if (other == null) {
            return true;
        }
        if (prefixes.size() != other.prefixes.size()) {
            return true;
        }
        for (int i = 0; i < prefixes.size(); i++) {
            tabRouteEntry<T> prf = prefixes.get(i);
            if (prf == null) {
                continue;
            }
            if (prf.differs(mod, other.prefixes.get(i))) {
                return true;
            }
        }
        return false;
    }

    /**
     * dump part of list
     *
     * @return list of entries
     */
    public List<String> dump() {
        List<String> l = new ArrayList<String>();
        for (int i = 0; i < prefixes.size(); i++) {
            l.add("" + prefixes.get(i));
        }
        return l;
    }

    /**
     * null labeled routes
     *
     * @param lst source
     * @return result
     */
    public static tabRoute<addrIP> nullLabeled(tabRoute<addrIP> lst) {
        tabRoute<addrIP> res = new tabRoute<addrIP>("rx");
        for (int i = 0; i < lst.size(); i++) {
            tabRouteEntry<addrIP> ntry = lst.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.best.labelRem == null) {
                continue;
            }
            if (ntry.best.labelRem.size() != 1) {
                continue;
            }
            int o = ntry.best.labelRem.get(0);
            if ((o != ipMpls.labelImp) && (o != ipMpls.labelExp4) && (o != ipMpls.labelExp6)) {
                continue;
            }
            res.add(tabRoute.addType.always, ntry, false, false);
        }
        return res;
    }

    /**
     * update entry
     *
     * @param afi address family
     * @param asn as number
     * @param ntry entry to add
     * @param rouMap route map to apply, null=permit
     * @param rouPlc route policy to apply, null=permit
     * @param prfLst prefix list to apply, null=permit
     * @return updated entry, null if denied
     */
    public static tabRouteEntry<addrIP> doUpdateEntry(int afi, int asn, tabRouteEntry<addrIP> ntry, tabListing<tabRtrmapN, addrIP> rouMap, tabListing<tabRtrplcN, addrIP> rouPlc, tabListing<tabPrfxlstN, addrIP> prfLst) {
        if (ntry == null) {
            return null;
        }
        if (prfLst != null) {
            if (!prfLst.matches(afi, asn, ntry.prefix)) {
                return null;
            }
        }
        if ((rouMap == null) && (rouPlc == null)) {
            return ntry.copyBytes(addType.ecmp);
        }
        if (rouMap != null) {
            ntry = rouMap.update(afi, asn, ntry, true);
            if (ntry == null) {
                return null;
            }
        }
        if (rouPlc != null) {
            ntry = tabRtrplc.doRpl(afi, asn, ntry, rouPlc, true);
            if (ntry == null) {
                return null;
            }
        }
        return ntry;
    }

    /**
     * add updated entry to route table
     *
     * @param mod mode to use
     * @param rouTab route table to add
     * @param afi address family
     * @param asn as number
     * @param ntry entry to add
     * @param newTime set true to set time, false to keep original time
     * @param rouMap route map to apply, null=permit
     * @param rouPlc route policy to apply, null=permit
     * @param prfLst prefix list to apply, null=permit
     * @return number of entries added
     */
    public static int addUpdatedEntry(addType mod, tabRoute<addrIP> rouTab, int afi, int asn, tabRouteEntry<addrIP> ntry, boolean newTime, tabListing<tabRtrmapN, addrIP> rouMap, tabListing<tabRtrplcN, addrIP> rouPlc, tabListing<tabPrfxlstN, addrIP> prfLst) {
        ntry = doUpdateEntry(afi, asn, ntry, rouMap, rouPlc, prfLst);
        if (ntry == null) {
            return 0;
        }
        rouTab.add(mod, ntry, false, newTime);
        return 1;
    }

    /**
     * delete updated entry from route table
     *
     * @param rouTab route table to add
     * @param afi address family
     * @param asn as number
     * @param ntry entry to add
     * @param rouMap route map to apply, null=permit
     * @param rouPlc route policy to apply, null=permit
     * @param prfLst prefix list to apply, null=permit
     * @return number of entries added
     */
    public static int delUpdatedEntry(tabRoute<addrIP> rouTab, int afi, int asn, tabRouteEntry<addrIP> ntry, tabListing<tabRtrmapN, addrIP> rouMap, tabListing<tabRtrplcN, addrIP> rouPlc, tabListing<tabPrfxlstN, addrIP> prfLst) {
        ntry = doUpdateEntry(afi, asn, ntry, rouMap, rouPlc, prfLst);
        if (ntry == null) {
            return 0;
        }
        rouTab.del(ntry.prefix);
        return 1;
    }

    /**
     * add updated table to route table
     *
     * @param mod mode to use
     * @param afi address family
     * @param asn as number
     * @param trg route table to add to
     * @param src route table to add from
     * @param newTime set true to set time, false to keep original time
     * @param rouMap route map to apply, null=permit
     * @param rouPlc route policy to apply, null=permit
     * @param prfLst prefix list to apply, null=permit
     * @return number of entries added
     */
    public static int addUpdatedTable(addType mod, int afi, int asn, tabRoute<addrIP> trg, tabRoute<addrIP> src, boolean newTime, tabListing<tabRtrmapN, addrIP> rouMap, tabListing<tabRtrplcN, addrIP> rouPlc, tabListing<tabPrfxlstN, addrIP> prfLst) {
        int added = 0;
        for (int i = 0; i < src.size(); i++) {
            tabRouteEntry<addrIP> ntry = src.get(i);
            if (ntry == null) {
                continue;
            }
            added += addUpdatedEntry(mod, trg, afi, asn, ntry, newTime, rouMap, rouPlc, prfLst);
        }
        return added;
    }

    /**
     * delete updated table to route table
     *
     * @param afi address family
     * @param asn as number
     * @param trg route table to add to
     * @param src route table to add from
     * @param rouMap route map to apply, null=permit
     * @param rouPlc route policy to apply, null=permit
     * @param prfLst prefix list to apply, null=permit
     * @return number of entries added
     */
    public static int delUpdatedTable(int afi, int asn, tabRoute<addrIP> trg, tabRoute<addrIP> src, tabListing<tabRtrmapN, addrIP> rouMap, tabListing<tabRtrplcN, addrIP> rouPlc, tabListing<tabPrfxlstN, addrIP> prfLst) {
        int deled = 0;
        for (int i = 0; i < src.size(); i++) {
            tabRouteEntry<addrIP> ntry = src.get(i);
            if (ntry == null) {
                continue;
            }
            deled += delUpdatedEntry(trg, afi, asn, ntry, rouMap, rouPlc, prfLst);
        }
        return deled;
    }

    /**
     * filter one table
     *
     * @param afi address family
     * @param asn as number
     * @param tab table to filter
     * @param flt filter to use
     * @return number of entries deleted
     */
    public static int filterTable(int afi, int asn, tabRoute<addrIP> tab, tabListing<tabPrfxlstN, addrIP> flt) {
        if (flt == null) {
            return 0;
        }
        int deled = 0;
        for (int i = tab.size() - 1; i >= 0; i--) {
            tabRouteEntry<addrIP> ntry = tab.get(i);
            if (ntry == null) {
                continue;
            }
            if (flt.matches(afi, asn, ntry.prefix)) {
                continue;
            }
            tab.del(ntry.prefix);
            deled++;
        }
        return deled;
    }

}
