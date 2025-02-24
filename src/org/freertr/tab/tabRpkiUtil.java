package org.freertr.tab;

import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrPrefix;
import org.freertr.cfg.cfgAll;
import org.freertr.user.userFormat;

/**
 * route authorization utils
 *
 * @author matecsaba
 */
public class tabRpkiUtil {

    /**
     * create instance
     */
    private tabRpkiUtil() {
    }

    /**
     * merge two tables
     *
     * @param trg target
     * @param src source
     * @return entries changed
     */
    public final static int mergeTwoRoa(tabGen<tabRpkiRoa> trg, tabGen<tabRpkiRoa> src) {
        int c = 0;
        for (int i = 0; i < src.size(); i++) {
            tabRpkiRoa n = src.get(i);
            if (n == null) {
                continue;
            }
            tabRpkiRoa o = trg.find(n);
            if (o == null) {
                trg.put(n);
                c++;
                continue;
            }
            if (n.isOtherBetter(o)) {
                continue;
            }
            trg.put(n);
            c++;
        }
        return c;
    }

    /**
     * merge two tables
     *
     * @param trg target
     * @param src source
     * @return entries changed
     */
    public final static int mergeTwoAspa(tabGen<tabRpkiAspa> trg, tabGen<tabRpkiAspa> src) {
        int c = 0;
        for (int i = 0; i < src.size(); i++) {
            tabRpkiAspa n = src.get(i);
            if (n == null) {
                continue;
            }
            tabRpkiAspa o = trg.find(n);
            if (o == null) {
                trg.put(n);
                c++;
                continue;
            }
            if (n.isOtherBetter(o)) {
                continue;
            }
            trg.put(n);
            c++;
        }
        return c;
    }

    /**
     * filter to an asn
     *
     * @param src table to filter
     * @param asn asn to find
     * @return matching prefixes
     */
    public final static tabGen<tabRpkiRoa> allowedRoa(tabGen<tabRpkiRoa> src, int asn) {
        tabGen<tabRpkiRoa> res = new tabGen<tabRpkiRoa>();
        for (int i = 0; i < src.size(); i++) {
            tabRpkiRoa ntry = src.get(i);
            if (ntry.asns.indexOf(asn) < 0) {
                continue;
            }
            res.add(ntry);
        }
        return res;
    }

    /**
     * filter to an asn
     *
     * @param src table to filter
     * @param asn asn to find
     * @return matching prefixes
     */
    public final static tabGen<tabRpkiAspa> allowedAspa(tabGen<tabRpkiAspa> src, int asn) {
        tabGen<tabRpkiAspa> res = new tabGen<tabRpkiAspa>();
        for (int i = 0; i < src.size(); i++) {
            tabRpkiAspa ntry = src.get(i);
            if (ntry.provs.indexOf(asn) < 0) {
                continue;
            }
            res.add(ntry);
        }
        return res;
    }

    /**
     * check if two tables are identical
     *
     * @param t1 first table
     * @param t2 second table
     * @return false if differs, true if identical
     */
    public final static boolean compareTwoRoa(tabGen<tabRpkiRoa> t1, tabGen<tabRpkiRoa> t2) {
        int s1 = t1.size();
        int s2 = t2.size();
        if (s1 != s2) {
            return false;
        }
        for (int i = 0; i < s1; i++) {
            tabRpkiRoa d1 = t1.get(i);
            tabRpkiRoa d2 = t2.get(i);
            if (d1 == null) {
                return false;
            }
            if (d2 == null) {
                return false;
            }
            int o = d1.compareTo(d2);
            if (o != 0) {
                return false;
            }
        }
        return true;
    }

    /**
     * check if two tables are identical
     *
     * @param t1 first table
     * @param t2 second table
     * @return false if differs, true if identical
     */
    public final static boolean compareTwoAspa(tabGen<tabRpkiAspa> t1, tabGen<tabRpkiAspa> t2) {
        int s1 = t1.size();
        int s2 = t2.size();
        if (s1 != s2) {
            return false;
        }
        for (int i = 0; i < s1; i++) {
            tabRpkiAspa d1 = t1.get(i);
            tabRpkiAspa d2 = t2.get(i);
            if (d1 == null) {
                return false;
            }
            if (d2 == null) {
                return false;
            }
            int o = d1.compareTo(d2);
            if (o != 0) {
                return false;
            }
        }
        return true;
    }

    /**
     * check if two tables are identical
     *
     * @param uniq unique to first neighbor
     * @param diff attributes differ
     * @param nei1 first neighbor
     * @param nei2 second neighbor
     */
    public final static void diffTwoRoa(tabGen<tabRpkiRoa> uniq, tabGen<tabRpkiRoa> diff, tabGen<tabRpkiRoa> nei1, tabGen<tabRpkiRoa> nei2) {
        for (int o = 0; o < nei1.size(); o++) {
            tabRpkiRoa prf1 = nei1.get(o);
            if (prf1 == null) {
                continue;
            }
            prf1 = prf1.copyBytes();
            tabRpkiRoa prf2 = nei2.find(prf1);
            if (prf2 == null) {
                uniq.add(prf1);
                continue;
            }
            prf2 = prf2.copyBytes();
            if (prf1.differs(prf2) == 0) {
                continue;
            }
            diff.add(prf1);
        }
    }

    /**
     * check if two tables are identical
     *
     * @param uniq unique to first neighbor
     * @param diff attributes differ
     * @param nei1 first neighbor
     * @param nei2 second neighbor
     */
    public final static void diffTwoAspa(tabGen<tabRpkiAspa> uniq, tabGen<tabRpkiAspa> diff, tabGen<tabRpkiAspa> nei1, tabGen<tabRpkiAspa> nei2) {
        for (int o = 0; o < nei1.size(); o++) {
            tabRpkiAspa prf1 = nei1.get(o);
            if (prf1 == null) {
                continue;
            }
            prf1 = prf1.copyBytes();
            tabRpkiAspa prf2 = nei2.find(prf1);
            if (prf2 == null) {
                uniq.add(prf1);
                continue;
            }
            prf2 = prf2.copyBytes();
            if (prf1.differs(prf2) == 0) {
                continue;
            }
            diff.add(prf1);
        }
    }

    /**
     * lookup a customer
     *
     * @param tab table to use
     * @param pfx prefix to lookup
     * @return roa if found, null if not
     */
    public final static tabRpkiAspa lookupAspa(tabGen<tabRpkiAspa> tab, int asn) {
        tabRpkiAspa ntry = new tabRpkiAspa();
        ntry.cust = asn;
        ntry = tab.find(ntry);
        return ntry;
    }

    /**
     * lookup a prefix
     *
     * @param tab table to use
     * @param pfx prefix to lookup
     * @return roa if found, null if not
     */
    public final static tabRpkiRoa lookupRoa(tabGen<tabRpkiRoa> tab, addrPrefix<addrIP> pfx) {
        if (tab.size() < 1) {
            return null;
        }
        boolean is4 = pfx.network.isIPv4();
        int end;
        if (is4) {
            end = cfgAll.accessSupnet4;
        } else {
            end = cfgAll.accessSupnet6;
        }
        tabRpkiRoa ntry = new tabRpkiRoa();
        ntry.prefix = pfx.copyBytes();
        for (int i = pfx.maskLen; i > end; i--) {
            ntry.prefix.setMask(i);
            tabRpkiRoa old = tab.find(ntry);
            if (old == null) {
                continue;
            }
            old.hits++;
            return old;
        }
        return null;
    }

    /**
     * get part of the table
     *
     * @param tab table to read
     * @param beg first index
     * @param end last index
     * @return subtable
     */
    public final static tabGen<tabRpkiRoa> getSubsetRoa(tabGen<tabRpkiRoa> tab, int beg, int end) {
        tabGen<tabRpkiRoa> res = new tabGen<tabRpkiRoa>();
        int siz = tab.size();
        if (end > siz) {
            end = siz;
        }
        if (beg < 0) {
            beg = 0;
        }
        for (int i = beg; i < end; i++) {
            tabRpkiRoa ntry = tab.get(i);
            if (ntry == null) {
                continue;
            }
            res.put(ntry);
        }
        return res;
    }

    /**
     * get part of the table
     *
     * @param tab table to read
     * @param beg first index
     * @param end last index
     * @return subtable
     */
    public final static tabGen<tabRpkiAspa> getSubsetAspa(tabGen<tabRpkiAspa> tab, int beg, int end) {
        tabGen<tabRpkiAspa> res = new tabGen<tabRpkiAspa>();
        int siz = tab.size();
        if (end > siz) {
            end = siz;
        }
        if (beg < 0) {
            beg = 0;
        }
        for (int i = beg; i < end; i++) {
            tabRpkiAspa ntry = tab.get(i);
            if (ntry == null) {
                continue;
            }
            res.put(ntry);
        }
        return res;
    }

    /**
     * convert table header
     *
     * @param typ type to format
     * @return header
     */
    public final static userFormat convertRoaHead(int typ) {
        switch (typ) {
            case 1:
                return new userFormat("|", "prefix|max|ases|ago");
            default:
                return null;
        }
    }

    /**
     * convert table header
     *
     * @param typ type to format
     * @return header
     */
    public final static userFormat convertAspaHead(int typ) {
        switch (typ) {
            case 1:
                return new userFormat("|", "prefix|ases|ago");
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
    public final static void convertRoaBody(userFormat lst, tabGen<tabRpkiRoa> tab, int typ) {
        lst.clear();
        for (int i = 0; i < tab.size(); i++) {
            tabRpkiRoa prf = tab.get(i);
            if (prf == null) {
                continue;
            }
            switch (typ) {
                case 1:
                    lst.add(prf.toShRoute());
                    break;
            }
        }
    }

    /**
     * convert table body
     *
     * @param lst string to update
     * @param tab table to convert
     * @param typ type to format
     */
    public final static void convertAspaBody(userFormat lst, tabGen<tabRpkiAspa> tab, int typ) {
        lst.clear();
        for (int i = 0; i < tab.size(); i++) {
            tabRpkiAspa prf = tab.get(i);
            if (prf == null) {
                continue;
            }
            switch (typ) {
                case 1:
                    lst.add(prf.toShRoute());
                    break;
            }
        }
    }

    /**
     * validity to string
     *
     * @param i validity
     * @return string
     */
    public final static String validity2string(int i) {
        switch (i) {
            case 0:
                return "nothing";
            case 1:
                return "correct";
            case 2:
                return "unknown";
            case 3:
                return "invalid";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * set validity
     *
     * @param ntry entry to update
     * @param val result code
     */
    protected static void setValidityFixed(tabRouteEntry<addrIP> ntry, int val) {
        for (int i = 0; i < ntry.alts.size(); i++) {
            setValidityFixed(ntry.alts.get(i), val);
        }
        ntry.selectBest();
    }

    /**
     * set validity
     *
     * @param tab table to update
     * @param val result code
     */
    protected static void setValidityFixed(tabRoute<addrIP> tab, int val) {
        for (int i = 0; i < tab.size(); i++) {
            setValidityFixed(tab.get(i), val);
        }
    }

    /**
     * set validity
     *
     * @param ntry entry to update
     * @param val result code
     */
    public static void updateJustValidity(tabRouteEntry<addrIP> ntry, int val) {
        for (int i = 0; i < ntry.alts.size(); i++) {
            ntry.alts.get(i).validity = val;
        }
        ntry.selectBest();
    }

    /**
     * set validity
     *
     * @param prfx prefix to check
     * @param attr attribute to set
     * @param res route authorization
     * @return calculated value
     */
    public final static int calcValidityValue(addrPrefix<addrIP> prfx, tabRouteAttr<addrIP> attr, tabRpkiRoa res) {
        if (res == null) {
            return 2;
        }
        int i = prfx.maskLen;
        if (prfx.network.isIPv4()) {
            i -= (addrIP.size - addrIPv4.size) * 8;
        }
        if (i > res.max) {
            return 3;
        }
        int asn = attr.asPathEnd();
        if (res.asns.indexOf(asn) < 0) {
            return 3;
        }
        return 1;
    }

    /**
     * set validity
     *
     * @param attr entry to update
     * @param val result code
     */
    protected static void setValidityFixed(tabRouteAttr<addrIP> attr, int val) {
        attr.validity = val;
        attr.extComm = tabRouteUtil.setValidExtCommRoa(attr.extComm, val);
    }

    /**
     * set validity
     *
     * @param ntry entry to update
     * @param roas route authorizations
     * @param mod mode to use
     */
    public static void setValidityRoute(tabRouteEntry<addrIP> ntry, tabGen<tabRpkiRoa> roas, int mod) {
        switch (mod) {
            case 0:
                return;
            case 1:
            case 2:
            case 3:
                break;
            case 4:
                setValidityFixed(ntry, 0);
                return;
            case 5:
                setValidityFixed(ntry, 1);
                return;
            case 6:
                setValidityFixed(ntry, 3);
                return;
            case 7:
                setValidityFixed(ntry, 2);
                return;
            default:
                return;
        }
        boolean lok = false;
        tabRpkiRoa res = null;
        for (int i = 0; i < ntry.alts.size(); i++) {
            tabRouteAttr<addrIP> attr = ntry.alts.get(i);
            int o;
            switch (mod) {
                case 1:
                    o = tabRouteUtil.getValidExtCommRoa(attr.extComm);
                    setValidityFixed(attr, o);
                    break;
                case 2:
                    if (!lok) {
                        res = lookupRoa(roas, ntry.prefix);
                        lok = true;
                    }
                    o = calcValidityValue(ntry.prefix, attr, res);
                    setValidityFixed(attr, o);
                    break;
                case 3:
                    o = tabRouteUtil.getValidExtCommRoa(attr.extComm);
                    if (o > 0) {
                        setValidityFixed(attr, o);
                        continue;
                    }
                    if (!lok) {
                        res = lookupRoa(roas, ntry.prefix);
                        lok = true;
                    }
                    o = calcValidityValue(ntry.prefix, attr, res);
                    setValidityFixed(attr, o);
                    break;
                default:
                    continue;
            }
        }
        ntry.selectBest();
    }

    /**
     * set validity
     *
     * @param tab table to update
     * @param roas route authorizations
     * @param mod rpki mode to use
     */
    public static void setValidityTable(tabRoute<addrIP> tab, tabGen<tabRpkiRoa> roas, int mod) {
        switch (mod) {
            case 0:
                return;
            case 1:
            case 2:
            case 3:
                break;
            case 4:
                setValidityFixed(tab, 0);
                return;
            case 5:
                setValidityFixed(tab, 1);
                return;
            case 6:
                setValidityFixed(tab, 3);
                return;
            case 7:
                setValidityFixed(tab, 2);
                return;
            default:
                return;
        }
        for (int i = 0; i < tab.size(); i++) {
            tabRouteEntry<addrIP> ntry = tab.get(i);
            if (ntry == null) {
                continue;
            }
            setValidityRoute(ntry, roas, mod);
        }
    }

}
