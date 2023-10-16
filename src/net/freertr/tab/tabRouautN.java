package net.freertr.tab;

import java.util.Comparator;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrPrefix;
import net.freertr.cfg.cfgAll;
import net.freertr.clnt.clntWhois;
import net.freertr.user.userFormat;
import net.freertr.util.bits;
import net.freertr.util.cmds;

/**
 * route authorization entry
 *
 * @author matecsaba
 */
public class tabRouautN implements Comparator<tabRouautN> {

    /**
     * create instance
     */
    public tabRouautN() {
    }

    /**
     * prefix base
     */
    public addrPrefix<addrIP> prefix;

    /**
     * max length
     */
    public int max;

    /**
     * allowed asn
     */
    public int asn;

    /**
     * preference
     */
    public int distan;

    /**
     * information source
     */
    public addrIP srcIP;

    /**
     * information source
     */
    public tabRouteAttr.routeType srcRtr;

    /**
     * information source
     */
    public int srcNum;

    /**
     * information time
     */
    public long time;

    public int compare(tabRouautN o1, tabRouautN o2) {
        return o1.prefix.compare(o1.prefix, o2.prefix);
    }

    public String toString() {
        return addrPrefix.ip2str(prefix) + "-" + max;
    }

    /**
     * copy bytes
     *
     * @param o other
     * @return copy
     */
    public tabRouautN copyBytes(tabRouautN o) {
        tabRouautN n = new tabRouautN();
        n.prefix = o.prefix.copyBytes();
        n.max = o.max;
        n.asn = o.asn;
        n.distan = o.distan;
        n.srcIP = o.srcIP.copyBytes();
        n.srcRtr = o.srcRtr;
        n.srcNum = o.srcNum;
        n.time = o.time;
        return n;
    }

    /**
     * check of other is better
     *
     * @param o other
     * @return true if yes, false if not
     */
    public boolean isOtherBetter(tabRouautN o) {
        if (distan < o.distan) {
            return true;
        }
        if (distan > o.distan) {
            return false;
        }
        if (time > o.time) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * convert from string
     *
     * @param cmd commands to read
     * @return true on error, false on success
     */
    public boolean fromString(cmds cmd) {
        prefix = addrPrefix.str2ip(cmd.word());
        if (prefix == null) {
            return true;
        }
        max = bits.str2num(cmd.word());
        asn = bits.str2num(cmd.word());
        distan = bits.str2num(cmd.word());
        if (distan < 1) {
            distan = 100;
        }
        srcIP = new addrIP();
        srcRtr = tabRouteAttr.routeType.staticRoute;
        return false;
    }

    /**
     * get config string
     *
     * @return string
     */
    public String toConfig() {
        String a = "";
        if (distan != 100) {
            a = " " + distan;
        }
        return addrPrefix.ip2str(prefix) + " " + max + " " + asn + a;
    }

    /**
     * convert to route
     *
     * @return converted
     */
    public String toShRoute() {
        return addrPrefix.ip2str(prefix) + "|" + max + "|" + bits.num2str(asn) + "|" + clntWhois.asn2name(asn, true) + "|" + bits.timePast(time) + "|" + bits.time2str(cfgAll.timeZoneName, time + cfgAll.timeServerOffset, 3);
    }

    /**
     * merge two tables
     *
     * @param trg target
     * @param src source
     * @return entries changed
     */
    public final static int mergeTwo(tabGen<tabRouautN> trg, tabGen<tabRouautN> src) {
        int c = 0;
        for (int i = 0; i < src.size(); i++) {
            tabRouautN n = src.get(i);
            if (n == null) {
                continue;
            }
            tabRouautN o = trg.find(n);
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
    public final static boolean compareTwo(tabGen<tabRouautN> t1, tabGen<tabRouautN> t2) {
        int s1 = t1.size();
        int s2 = t2.size();
        if (s1 != s2) {
            return false;
        }
        for (int i = 0; i < s1; i++) {
            tabRouautN d1 = t1.get(i);
            tabRouautN d2 = t2.get(i);
            if (d1 == null) {
                return false;
            }
            if (d2 == null) {
                return false;
            }
            int o = d1.compare(d1, d2);
            if (o != 0) {
                return false;
            }
        }
        return true;
    }

    /**
     * lookup a prefix
     *
     * @param tab table to use
     * @param pfx prefix to lookup
     * @return roa if found, null if not
     */
    public final static tabRouautN lookup(tabGen<tabRouautN> tab, addrPrefix<addrIP> pfx) {
        boolean is4 = pfx.network.isIPv4();
        int end;
        if (is4) {
            end = cfgAll.accessSupnet4;
        } else {
            end = cfgAll.accessSupnet6;
        }
        tabRouautN ntry = new tabRouautN();
        for (int i = pfx.maskLen; i > end; i--) {
            ntry.prefix = pfx.copyBytes();
            ntry.prefix.setMask(i);
            tabRouautN old = tab.find(ntry);
            if (old == null) {
                continue;
            }
            if (pfx.maskLen - end > old.max) {
                return null;
            }
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
    public final static tabGen<tabRouautN> getSubset(tabGen<tabRouautN> tab, int beg, int end) {
        tabGen<tabRouautN> res = new tabGen<tabRouautN>();
        int siz = tab.size();
        if (end > siz) {
            end = siz;
        }
        if (beg < 0) {
            beg = 0;
        }
        for (int i = beg; i < end; i++) {
            tabRouautN ntry = tab.get(i);
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
                return new userFormat("|", "prefix|max|asnum|asnam|ago|since");
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
    public final static void convertTableBody(userFormat lst, tabGen<tabRouautN> tab, int typ) {
        lst.clear();
        for (int i = 0; i < tab.size(); i++) {
            tabRouautN prf = tab.get(i);
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
    public final static userFormat convertTableFull(tabGen<tabRouautN> tab, int typ) {
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
    public final static String valid2str(int i) {
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
    public static void setValidityRoute(tabRouteEntry<addrIP> ntry, int val) {
        for (int o = 0; o < ntry.alts.size(); o++) {
            tabRouteAttr<addrIP> attr = ntry.alts.get(o);
            attr.validity = val;
        }
        ntry.selectBest();
    }

    /**
     * set validity
     *
     * @param tab table to update
     * @param val result code
     */
    public static void setValidityTable(tabRoute<addrIP> tab, int val) {
        for (int i = 0; i < tab.size(); i++) {
            setValidityRoute(tab.get(i), val);
        }
    }

    /**
     * set validity
     *
     * @param attr attribute to set
     * @param res route authorization
     */
    public final static void setValidityAttr(tabRouteAttr<addrIP> attr, tabRouautN res) {
        if (res == null) {
            attr.validity = 2;
            return;
        }
        int i = attr.asPathEnd();
        if (i != res.asn) {
            attr.validity = 3;
            return;
        }
        attr.validity = 1;
    }

    /**
     * set validity
     *
     * @param ntry entry to update
     * @param roas route authorizations
     */
    public static void setValidityRoute(tabRouteEntry<addrIP> ntry, tabGen<tabRouautN> roas) {
        tabRouautN res = tabRouautN.lookup(roas, ntry.prefix);
        for (int o = 0; o < ntry.alts.size(); o++) {
            tabRouteAttr<addrIP> attr = ntry.alts.get(o);
            setValidityAttr(attr, res);
        }
        ntry.selectBest();
    }

    /**
     * set validity
     *
     * @param tab table to update
     * @param roas route authorizations
     */
    public static void setValidityTable(tabRoute<addrIP> tab, tabGen<tabRouautN> roas) {
        for (int i = 0; i < tab.size(); i++) {
            setValidityRoute(tab.get(i), roas);
        }
    }

}
