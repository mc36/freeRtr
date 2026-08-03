package org.freertr.tab;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrPrefix;
import org.freertr.addr.addrType;
import org.freertr.user.userFormat;
import org.freertr.util.bits;
import org.freertr.util.debugger;
import org.freertr.util.logger;

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
         * add never
         */
        never,
        /**
         * add as alternatives
         */
        alters,
        /**
         * ecmp but with linking copy mode
         */
        lnkEcmp,
        /**
         * ecmp but with linking copy mode and strict best selection
         */
        lnkBcmp,
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

    private tabGep<T> lookupTrie = null;

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
        if (orig.lookupTrie != null) {
            optimize4lookup();
        }
    }

    /**
     * optimize for lookup
     */
    public void optimize4lookup() {
        tabGep<T> res = new tabGep<T>();
        for (int i = 0; i < prefixes.size(); i++) {
            res.add(prefixes.get(i));
        }
        lookupTrie = res;
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
            if (lst.compareTo(cur) >= 0) {
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
        if (lookupTrie != null) {
            optimize4lookup();
        }
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
                    if (lookupTrie != null) {
                        lookupTrie.add(prefix);
                    }
                    version++;
                    return;
                }
                if (own.best.isOtherBetter(prefix.best)) {
                    prefixes.put(prefix);
                    if (lookupTrie != null) {
                        lookupTrie.add(prefix);
                    }
                    version++;
                    return;
                }
                if (prefix.best.isOtherBetter(own.best)) {
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
            case lnkBcmp:
            case ecmp:
                own = prefixes.add(prefix);
                if (own == null) {
                    if (lookupTrie != null) {
                        lookupTrie.add(prefix);
                    }
                    version++;
                    return;
                }
                if (own.best.isOtherBetter(prefix.best)) {
                    prefixes.put(prefix);
                    if (lookupTrie != null) {
                        lookupTrie.add(prefix);
                    }
                    version++;
                    return;
                }
                if (prefix.best.isOtherBetter(own.best)) {
                    return;
                }
                for (int i = 0; i < prefix.alts.size(); i++) {
                    tabRouteAttr<T> ntry = prefix.alts.get(i);
                    if (ntry.isOtherBetter(own.best)) {
                        continue;
                    }
                    own.alts.add(ntry);
                }
                if (mod == addType.lnkBcmp) {
                    own.selectBest();
                } else {
                    own.hashBest();
                }
                version++;
                return;
            case better:
                if (prefix.alts.size() > 1) {
                    prefix = prefix.copyBytes(mod);
                }
                own = prefixes.add(prefix);
                if (own == null) {
                    if (lookupTrie != null) {
                        lookupTrie.add(prefix);
                    }
                    version++;
                    return;
                }
                if (!own.best.isOtherBetter(prefix.best)) {
                    return;
                }
                prefixes.put(prefix);
                if (lookupTrie != null) {
                    lookupTrie.add(prefix);
                }
                version++;
                return;
            case always:
                prefixes.put(prefix);
                if (lookupTrie != null) {
                    lookupTrie.add(prefix);
                }
                version++;
                return;
            case notyet:
                if (prefix.alts.size() > 1) {
                    prefix = prefix.copyBytes(mod);
                }
                if (prefixes.add(prefix) != null) {
                    return;
                }
                if (lookupTrie != null) {
                    lookupTrie.add(prefix);
                }
                version++;
                return;
            case lnkAlters:
            case alters:
                own = prefixes.add(prefix);
                if (own == null) {
                    if (lookupTrie != null) {
                        lookupTrie.add(prefix);
                    }
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
        if (lookupTrie != null) {
            lookupTrie.del(prf);
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
        if (lookupTrie != null) {
            lookupTrie.del(prf);
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
     * get part of the table
     *
     * @param beg first index
     * @param end last index
     * @return subtable
     */
    public tabRoute<T> getSubset(int beg, int end) {
        tabRoute<T> res = new tabRoute<T>(tabName);
        int siz = size();
        if (end > siz) {
            end = siz;
        }
        if (beg < 0) {
            beg = 0;
        }
        for (int i = beg; i < end; i++) {
            tabRouteEntry<T> ntry = prefixes.get(i);
            if (ntry == null) {
                continue;
            }
            res.prefixes.put(ntry);
        }
        res.version = end - beg;
        return res;
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
            if (lookupTrie != null) {
                lookupTrie.del(prf);
            }
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
            if (lookupTrie != null) {
                lookupTrie.del(prf);
            }
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
            if (lookupTrie != null) {
                lookupTrie.del(prf);
            }
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
            if (lookupTrie != null) {
                lookupTrie.del(prf);
            }
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
     * @return true if the same, false if differs
     */
    public boolean preserveTime(tabRoute<T> src) {
        boolean res = prefixes.size() == src.prefixes.size();
        for (int i = 0; i < prefixes.size(); i++) {
            tabRouteEntry<T> prf = prefixes.get(i);
            if (prf == null) {
                continue;
            }
            tabRouteEntry<T> old = src.find(prf);
            if (old == null) {
                continue;
            }
            if (prf.differs(addType.notyet, old) != 0) {
                res = false;
                continue;
            }
            for (int o = 0; o < prf.alts.size(); o++) {
                prf.alts.get(o).time = old.best.time;
            }
        }
        return res;
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
     * @param nexthops table where look up resolved nexthops
     * @param othrhops table where look up other nexthops
     * @param recur maximum recursion depth
     * @param distan highest allowed distance
     */
    public void mergeFrom(addType mod, tabRoute<T> other, tabRoute<T> nexthops, tabRoute<T> othrhops, int recur, int distan) {
        for (int i = 0; i < other.prefixes.size(); i++) {
            tabRouteEntry<T> imp = other.prefixes.get(i);
            if (imp == null) {
                continue;
            }
            if (imp.best.distance >= distan) {
                continue;
            }
            imp = imp.copyBytes(mod);
            if (tabRouteUtil.doNexthopFix(imp, other, nexthops, othrhops, recur)) {
                continue;
            }
            add(mod, imp, false, false);
        }
        if (debugger.tabRouteEvnt) {
            logger.debug("merged prefixes from " + other.defRouTyp);
        }
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
        if (lookupTrie != null) {
            return lookupTrie.search(prf);
        }
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
            if (prf.differs(mod, other.prefixes.get(i)) != 0) {
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
     * convert table header
     *
     * @param typ type to format
     * @return header
     */
    public final static userFormat convertTableHead(int typ) {
        switch (typ) {
            case 1:
                return new userFormat("|", "typ|prefix|metric|iface|hop|time");
            case 2:
            case 5:
                return new userFormat("|", "prefix|hop|metric|aspath");
            case 1002:
            case 1005:
                return new userFormat("|", "prefix|local|evpn*16|pmsi*16|remote|hop");
            case 3:
                return new userFormat("|", "prefix|local|remote|hop");
            case 4:
                return new userFormat("|", "prefix|asnum|asnam|calc|encod|calc|encod|ago|since", "3|2roa|2aspa|2");
            case 6:
                return new userFormat("|", "prefix|pack|byte|pack|byte|time", "1|2transmit|2receive|1");
            case 7:
                return new userFormat("|", "prefix|index|base|oldbase|srv6|behav");
            case 8:
                return new userFormat("|", "prefix|index|subdom|base|oldbase|size");
            case 2002:
            case 2005:
            case 9:
                return new userFormat("|", "prefix|alts|candid|best|proto|source");
            case 3002:
            case 3005:
            case 10:
                return new userFormat("|", "prefix|hop|ago|last");
            case 11:
                return new userFormat("|", "prefix|hop|metric|asname");
            case 12:
                return new userFormat("|", "prefix|hop|metric|asinfo");
            case 13:
                return new userFormat("|", "prefix|hop|metric|asmixed");
            default:
                return null;
        }
    }

    /**
     * convert table body
     *
     * @param lst string to update
     * @param tab table to convert
     * @param typ type to format
     */
    public final static void convertTableBody(userFormat lst, tabRoute<addrIP> tab, int typ) {
        lst.clear();
        for (int i = 0; i < tab.size(); i++) {
            tabRouteEntry<addrIP> prf = tab.get(i);
            if (prf == null) {
                continue;
            }
            switch (typ) {
                case 1:
                    tabRouteEntry.toShRoute(lst, prf);
                    break;
                case 2:
                    tabRouteEntry.toShBgp(lst, prf);
                    break;
                case 3:
                    tabRouteEntry.toShLdp(lst, prf);
                    break;
                case 1002:
                case 1005:
                    tabRouteEntry.toShBgpLabels(lst, prf, typ == 1005);
                    break;
                case 4:
                    tabRouteEntry.toShRpki(lst, prf);
                    break;
                case 5:
                    tabRouteEntry.toShEvpn(lst, prf);
                    break;
                case 6:
                    tabRouteEntry.toShCntr(lst, prf);
                    break;
                case 7:
                    tabRouteEntry.toShSrRoute(lst, prf);
                    break;
                case 8:
                    tabRouteEntry.toShBrRoute(lst, prf);
                    break;
                case 2002:
                case 2005:
                case 9:
                    tabRouteEntry.toShEcmp(lst, prf, typ == 2005);
                    break;
                case 3002:
                case 3005:
                case 10:
                    tabRouteEntry.toShChgRoute(lst, prf);
                    break;
                case 11:
                    tabRouteEntry.toShAsName(lst, prf);
                    break;
                case 12:
                    tabRouteEntry.toShAsInfo(lst, prf);
                    break;
                case 13:
                    tabRouteEntry.toShAsMixed(lst, prf);
                    break;
            }
        }
    }

    /**
     * convert the full table
     *
     * @param tab table to convert
     * @param typ type to format
     * @return converted table
     */
    public final static userFormat convertTableFull(tabRoute<addrIP> tab, int typ) {
        userFormat res = convertTableHead(typ);
        if (res == null) {
            return null;
        }
        convertTableBody(res, tab, typ);
        return res;
    }

}
