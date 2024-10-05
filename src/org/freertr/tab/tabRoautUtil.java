package org.freertr.tab;

import org.freertr.addr.addrIP;
import org.freertr.addr.addrPrefix;
import org.freertr.cfg.cfgAll;
import org.freertr.user.userFormat;

/**
 * route authorization utils
 *
 * @author matecsaba
 */
public class tabRoautUtil {

    /**
     * create instance
     */
    private tabRoautUtil() {
    }

    /**
     * merge two tables
     *
     * @param trg target
     * @param src source
     * @return entries changed
     */
    public final static int mergeTwo(tabGen<tabRoautNtry> trg, tabGen<tabRoautNtry> src) {
        int c = 0;
        for (int i = 0; i < src.size(); i++) {
            tabRoautNtry n = src.get(i);
            if (n == null) {
                continue;
            }
            tabRoautNtry o = trg.find(n);
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
     * check if two tables are identical
     *
     * @param t1 first table
     * @param t2 second table
     * @return false if differs, true if identical
     */
    public final static boolean compareTwo(tabGen<tabRoautNtry> t1, tabGen<tabRoautNtry> t2) {
        int s1 = t1.size();
        int s2 = t2.size();
        if (s1 != s2) {
            return false;
        }
        for (int i = 0; i < s1; i++) {
            tabRoautNtry d1 = t1.get(i);
            tabRoautNtry d2 = t2.get(i);
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
    public final static void diffTwo(tabGen<tabRoautNtry> uniq, tabGen<tabRoautNtry> diff, tabGen<tabRoautNtry> nei1, tabGen<tabRoautNtry> nei2) {
        for (int o = 0; o < nei1.size(); o++) {
            tabRoautNtry prf1 = nei1.get(o);
            if (prf1 == null) {
                continue;
            }
            prf1 = prf1.copyBytes();
            tabRoautNtry prf2 = nei2.find(prf1);
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
     * lookup a prefix
     *
     * @param tab table to use
     * @param pfx prefix to lookup
     * @return roa if found, null if not
     */
    public final static tabRoautNtry lookup(tabGen<tabRoautNtry> tab, addrPrefix<addrIP> pfx) {
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
        tabRoautNtry ntry = new tabRoautNtry();
        for (int i = pfx.maskLen; i > end; i--) {
            ntry.prefix = pfx.copyBytes();
            ntry.prefix.setMask(i);
            tabRoautNtry old = tab.find(ntry);
            if (old == null) {
                continue;
            }
            if (pfx.maskLen - end > old.max) {
                return null;
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
    public final static tabGen<tabRoautNtry> getSubset(tabGen<tabRoautNtry> tab, int beg, int end) {
        tabGen<tabRoautNtry> res = new tabGen<tabRoautNtry>();
        int siz = tab.size();
        if (end > siz) {
            end = siz;
        }
        if (beg < 0) {
            beg = 0;
        }
        for (int i = beg; i < end; i++) {
            tabRoautNtry ntry = tab.get(i);
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
    public final static userFormat convertTableHead(int typ) {
        switch (typ) {
            case 1:
                return new userFormat("|", "prefix|max|asnum|asnam|ago");
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
    public final static void convertTableBody(userFormat lst, tabGen<tabRoautNtry> tab, int typ) {
        lst.clear();
        for (int i = 0; i < tab.size(); i++) {
            tabRoautNtry prf = tab.get(i);
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
     * convert the full table
     *
     * @param tab table to convert
     * @param typ type to format
     * @return converted table
     */
    public final static userFormat convertTableFull(tabGen<tabRoautNtry> tab, int typ) {
        userFormat res = convertTableHead(typ);
        if (res == null) {
            return null;
        }
        convertTableBody(res, tab, typ);
        return res;
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
                return "unset";
            case 1:
                return "valid";
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
     * @param attr attribute to set
     * @param res route authorization
     * @return calculated value
     */
    public final static int calcValidityValue(tabRouteAttr<addrIP> attr, tabRoautNtry res) {
        if (res == null) {
            return 2;
        }
        int asn = attr.asPathEnd();
        if (asn != res.asn) {
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
        attr.extComm = tabRouteUtil.setValidityExtComm(attr.extComm, val);
    }

    /**
     * set validity
     *
     * @param ntry entry to update
     * @param roas route authorizations
     * @param mod mode to use
     */
    public static void setValidityRoute(tabRouteEntry<addrIP> ntry, tabGen<tabRoautNtry> roas, int mod) {
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
        tabRoautNtry res = null;
        for (int i = 0; i < ntry.alts.size(); i++) {
            tabRouteAttr<addrIP> attr = ntry.alts.get(i);
            int o;
            switch (mod) {
                case 1:
                    o = tabRouteUtil.getValidityExtComm(attr.extComm);
                    setValidityFixed(attr, o);
                    break;
                case 2:
                    if (!lok) {
                        res = lookup(roas, ntry.prefix);
                        lok = true;
                    }
                    o = calcValidityValue(attr, res);
                    setValidityFixed(attr, o);
                    break;
                case 3:
                    o = tabRouteUtil.getValidityExtComm(attr.extComm);
                    if (o > 0) {
                        setValidityFixed(attr, o);
                        continue;
                    }
                    if (!lok) {
                        res = lookup(roas, ntry.prefix);
                        lok = true;
                    }
                    o = calcValidityValue(attr, res);
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
    public static void setValidityTable(tabRoute<addrIP> tab, tabGen<tabRoautNtry> roas, int mod) {
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
