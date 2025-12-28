package org.freertr.tab;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrPrefix;
import org.freertr.cfg.cfgAll;
import org.freertr.clnt.clntWhois;
import org.freertr.spf.spfCalc;
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
     * merge two tables
     *
     * @param trg target
     * @param src source
     * @return entries changed
     */
    public final static int mergeTwoKey(tabGen<tabRpkiKey> trg, tabGen<tabRpkiKey> src) {
        int c = 0;
        for (int i = 0; i < src.size(); i++) {
            tabRpkiKey n = src.get(i);
            if (n == null) {
                continue;
            }
            tabRpkiKey o = trg.find(n);
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
            if (Collections.binarySearch(ntry.asns, asn) < 0) {
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
            if (Collections.binarySearch(ntry.provs, asn) < 0) {
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
    public final static tabGen<tabRpkiKey> allowedKey(tabGen<tabRpkiKey> src, int asn) {
        tabGen<tabRpkiKey> res = new tabGen<tabRpkiKey>();
        for (int i = 0; i < src.size(); i++) {
            tabRpkiKey ntry = src.get(i);
            if (ntry.asn != asn) {
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
     * @param t1 first table
     * @param t2 second table
     * @return false if differs, true if identical
     */
    public final static boolean compareTwoKey(tabGen<tabRpkiKey> t1, tabGen<tabRpkiKey> t2) {
        int s1 = t1.size();
        int s2 = t2.size();
        if (s1 != s2) {
            return false;
        }
        for (int i = 0; i < s1; i++) {
            tabRpkiKey d1 = t1.get(i);
            tabRpkiKey d2 = t2.get(i);
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
     * check if two tables are identical
     *
     * @param uniq unique to first neighbor
     * @param diff attributes differ
     * @param nei1 first neighbor
     * @param nei2 second neighbor
     */
    public final static void diffTwoKey(tabGen<tabRpkiKey> uniq, tabGen<tabRpkiKey> diff, tabGen<tabRpkiKey> nei1, tabGen<tabRpkiKey> nei2) {
        for (int o = 0; o < nei1.size(); o++) {
            tabRpkiKey prf1 = nei1.get(o);
            if (prf1 == null) {
                continue;
            }
            prf1 = prf1.copyBytes();
            tabRpkiKey prf2 = nei2.find(prf1);
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
     * @param asn asn to lookup
     * @return roa if found, null if not
     */
    public final static tabRpkiAspa lookupAspa(tabGen<tabRpkiAspa> tab, int asn) {
        tabRpkiAspa ntry = new tabRpkiAspa();
        ntry.cust = asn;
        ntry = tab.find(ntry);
        if (ntry == null) {
            return null;
        }
        ntry.hits++;
        return ntry;
    }

    /**
     * lookup a customer
     *
     * @param tab table to use
     * @param asn asn to lookup
     * @param ski ski to lookup
     * @return roa if found, null if not
     */
    public final static tabRpkiKey lookupKey(tabGen<tabRpkiKey> tab, int asn, byte[] ski) {
        tabRpkiKey ntry = new tabRpkiKey();
        ntry.asn = asn;
        ntry.ski = ski;
        ntry = tab.find(ntry);
        if (ntry == null) {
            return null;
        }
        ntry.hits++;
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
     * get part of the table
     *
     * @param tab table to read
     * @param beg first index
     * @param end last index
     * @return subtable
     */
    public final static tabGen<tabRpkiKey> getSubsetKey(tabGen<tabRpkiKey> tab, int beg, int end) {
        tabGen<tabRpkiKey> res = new tabGen<tabRpkiKey>();
        int siz = tab.size();
        if (end > siz) {
            end = siz;
        }
        if (beg < 0) {
            beg = 0;
        }
        for (int i = beg; i < end; i++) {
            tabRpkiKey ntry = tab.get(i);
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
                return new userFormat("|", "asn|ases|ago");
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
    public final static userFormat convertKeyHead(int typ) {
        switch (typ) {
            case 1:
                return new userFormat("|", "asn|ski|ago");
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
    public final static void convertKeyBody(userFormat lst, tabGen<tabRpkiKey> tab, int typ) {
        lst.clear();
        for (int i = 0; i < tab.size(); i++) {
            tabRpkiKey prf = tab.get(i);
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
     * aspa path graph
     *
     * @param tab table to convert
     * @return text
     */
    public final static List<String> getAspaGraph(tabGen<tabRpkiAspa> tab) {
        List<String> res = new ArrayList<String>();
        res.add(spfCalc.graphBeg1);
        res.add(spfCalc.graphBeg2);
        res.add(spfCalc.graphBeg3);
        for (int o = 0; o < tab.size(); o++) {
            tabRpkiAspa ntry = tab.get(o);
            String a = clntWhois.asn2mixed(ntry.cust, true);
            for (int i = 0; i < ntry.provs.size(); i++) {
                res.add("\"" + a + "\" -- \"" + clntWhois.asn2mixed(ntry.provs.get(i), true) + "\"");
            }
        }
        res.add(spfCalc.graphEnd1);
        res.add(spfCalc.graphEnd2);
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
                return "nothing";
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
     * @param roa result code
     * @param aspa result code
     */
    protected static void setValidityFixed(tabRouteEntry<addrIP> ntry, int roa, int aspa) {
        for (int i = 0; i < ntry.alts.size(); i++) {
            setValidityFixed(ntry.alts.get(i), roa, aspa);
        }
        ntry.selectBest();
    }

    /**
     * set validity
     *
     * @param tab table to update
     * @param roa result code
     * @param aspa result code
     */
    protected static void setValidityFixed(tabRoute<addrIP> tab, int roa, int aspa) {
        for (int i = 0; i < tab.size(); i++) {
            setValidityFixed(tab.get(i), roa, aspa);
        }
    }

    /**
     * set validity
     *
     * @param ntry entry to update
     * @param roa result code
     * @param aspa result code
     */
    public static void updateJustValidity(tabRouteEntry<addrIP> ntry, int roa, int aspa) {
        for (int i = 0; i < ntry.alts.size(); i++) {
            tabRouteAttr<addrIP> attr = ntry.alts.get(i);
            attr.validRoa = roa;
            attr.validAspa = aspa;
        }
        ntry.selectBest();
    }

    /**
     * compute aspa validity
     *
     * @param attr attribute to set
     * @param tab provider authorization
     * @param numP local asn
     * @return calculated value
     */
    public final static int calcValidityAspa(tabRouteAttr<addrIP> attr, tabGen<tabRpkiAspa> tab, int numP) {
        if (attr.pathSeq == null) {
            return 2;
        }
        tabRpkiAspa resP = lookupAspa(tab, numP);
        int len = attr.pathSeq.size();
        int maxUp = len;
        int minUp = len;
        int maxDn = len;
        int minDn = len;
        for (int i = 0; i < len; i++) {
            int numC = attr.pathSeq.get(i);
            if (numC == numP) {
                continue;
            }
            tabRpkiAspa resC = lookupAspa(tab, numC);
            if (resC == null) {
                minUp = len - i;
            } else {
                if (Collections.binarySearch(resC.provs, numP) < 0) {
                    minUp = len - i;
                    maxUp = len - i;
                }
            }
            if (resP == null) {
                if (minDn == len) {
                    minDn = i;
                }
            } else {
                if (Collections.binarySearch(resP.provs, numC) < 0) {
                    if (minDn == len) {
                        minDn = i;
                    }
                    if (maxDn == len) {
                        maxDn = i;
                    }
                }
            }
            numP = numC;
            resP = resC;
        }
        if ((maxUp + maxDn) < len) {
            return 3;
        }
        if ((minUp + minDn) < len) {
            return 2;
        }
        return 1;
    }

    /**
     * compute roa validity
     *
     * @param prfx prefix to check
     * @param attr attribute to set
     * @param res route authorization
     * @return calculated value
     */
    public final static int calcValidityRoa(addrPrefix<addrIP> prfx, tabRouteAttr<addrIP> attr, tabRpkiRoa res) {
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
        if (Collections.binarySearch(res.asns, asn) < 0) {
            return 3;
        }
        return 1;
    }

    /**
     * set validity
     *
     * @param attr entry to update
     * @param roa result code
     * @param aspa result code
     */
    protected static void setValidityFixed(tabRouteAttr<addrIP> attr, int roa, int aspa) {
        attr.validRoa = roa;
        attr.validAspa = aspa;
        attr.extComm = tabRouteUtil.setValidExtCommRoa(attr.extComm, roa);
        attr.extComm = tabRouteUtil.setValidExtCommAspa(attr.extComm, aspa);
    }

    /**
     * set validity
     *
     * @param asn local asn
     * @param ntry entry to update
     * @param roas route authorizations
     * @param aspas provider authorizations
     * @param mod mode to use
     */
    public static void setValidityRoute(int asn, tabRouteEntry<addrIP> ntry, tabGen<tabRpkiRoa> roas, tabGen<tabRpkiAspa> aspas, int mod) {
        switch (mod) {
            case 0:
                return;
            case 1:
                for (int i = 0; i < ntry.alts.size(); i++) {
                    tabRouteAttr<addrIP> attr = ntry.alts.get(i);
                    int o = tabRouteUtil.getValidExtCommRoa(attr.extComm);
                    int p = tabRouteUtil.getValidExtCommAspa(attr.extComm);
                    setValidityFixed(attr, o, p);
                }
                ntry.selectBest();
                return;
            case 2:
            case 3:
                break;
            case 4:
                setValidityFixed(ntry, 0, 0);
                return;
            case 5:
                setValidityFixed(ntry, 1, 1);
                return;
            case 6:
                setValidityFixed(ntry, 3, 3);
                return;
            case 7:
                setValidityFixed(ntry, 2, 2);
                return;
            default:
                return;
        }
        tabRpkiRoa res = lookupRoa(roas, ntry.prefix);
        for (int i = 0; i < ntry.alts.size(); i++) {
            tabRouteAttr<addrIP> attr = ntry.alts.get(i);
            switch (mod) {
                case 2:
                    int o = calcValidityRoa(ntry.prefix, attr, res);
                    int p = calcValidityAspa(attr, aspas, asn);
                    setValidityFixed(attr, o, p);
                    break;
                case 3:
                    o = tabRouteUtil.getValidExtCommRoa(attr.extComm);
                    p = tabRouteUtil.getValidExtCommAspa(attr.extComm);
                    if (o < 1) {
                        o = calcValidityRoa(ntry.prefix, attr, res);
                    }
                    if (p < 1) {
                        p = calcValidityAspa(attr, aspas, asn);
                    }
                    setValidityFixed(attr, o, p);
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
     * @param asn local asn
     * @param tab table to update
     * @param roas route authorizations
     * @param aspas provider authorizations
     * @param mod rpki mode to use
     */
    public static void setValidityTable(int asn, tabRoute<addrIP> tab, tabGen<tabRpkiRoa> roas, tabGen<tabRpkiAspa> aspas, int mod) {
        switch (mod) {
            case 0:
                return;
            case 1:
            case 2:
            case 3:
                break;
            case 4:
                setValidityFixed(tab, 0, 0);
                return;
            case 5:
                setValidityFixed(tab, 1, 1);
                return;
            case 6:
                setValidityFixed(tab, 3, 3);
                return;
            case 7:
                setValidityFixed(tab, 2, 2);
                return;
            default:
                return;
        }
        for (int i = 0; i < tab.size(); i++) {
            tabRouteEntry<addrIP> ntry = tab.get(i);
            if (ntry == null) {
                continue;
            }
            setValidityRoute(asn, ntry, roas, aspas, mod);
        }
    }

}
