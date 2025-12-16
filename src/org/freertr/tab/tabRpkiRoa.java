package org.freertr.tab;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrPrefix;
import org.freertr.cfg.cfgAll;
import org.freertr.clnt.clntWhois;
import org.freertr.enc.encJson;
import org.freertr.user.userFormat;
import org.freertr.util.bits;
import org.freertr.util.cmds;

/**
 * route authorization entry
 *
 * @author matecsaba
 */
public class tabRpkiRoa implements Comparable<tabRpkiRoa> {

    /**
     * create instance
     */
    public tabRpkiRoa() {
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
    public List<Integer> asns;

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

    /**
     * hit counter
     */
    public int hits;

    public int compareTo(tabRpkiRoa o) {
        return prefix.compareTo(o.prefix);
    }

    public String toString() {
        return addrPrefix.ip2str(prefix) + "-" + max;
    }

    /**
     * copy bytes
     *
     * @return copy
     */
    public tabRpkiRoa copyBytes() {
        tabRpkiRoa n = new tabRpkiRoa();
        n.prefix = prefix.copyBytes();
        n.max = max;
        if (asns != null) {
            n.asns = new ArrayList<Integer>(asns);
        }
        n.distan = distan;
        n.srcIP = srcIP.copyBytes();
        n.srcRtr = srcRtr;
        n.srcNum = srcNum;
        n.time = time;
        n.hits = hits;
        return n;
    }

    /**
     * print roa details
     *
     * @return text
     */
    public userFormat fullDump() {
        userFormat res = new userFormat("|", "category|value");
        res.add("prefix|" + addrPrefix.ip2str(prefix));
        res.add("maximum length|" + max);
        for (int i = 0; i < asns.size(); i++) {
            res.add("as number|" + clntWhois.asn2mixed(asns.get(i), true));
        }
        res.add("preference|" + distan);
        res.add("source|" + srcIP);
        res.add("type|" + srcRtr + " " + srcNum);
        res.add("updated|" + bits.time2str(cfgAll.timeZoneName, time + cfgAll.timeServerOffset, 3) + " (" + bits.timePast(time) + " ago)");
        res.add("hits|" + hits);
        return res;
    }

    /**
     * check if atrtibutes differs
     *
     * @param o other to compare to
     * @return numerical value if differred
     */
    public int differs(tabRpkiRoa o) {
        if (o == null) {
            return 1;
        }
        if (max != o.max) {
            return 2;
        }
        if (asns.size() != o.asns.size()) {
            return 3;
        }
        if (o.compareTo(this) != 0) {
            return 4;
        }
        for (int i = 0; i < asns.size(); i++) {
            if (asns.get(i) != o.asns.get(i)) {
                return 5;
            }
        }
        return 0;
    }

    /**
     * check of other is better
     *
     * @param o other
     * @return true if yes, false if not
     */
    public boolean isOtherBetter(tabRpkiRoa o) {
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
        asns = new ArrayList<Integer>();
        for (;;) {
            String a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            asns.add(bits.str2num(a));
        }
        distan = 100;
        srcIP = new addrIP();
        srcRtr = tabRouteAttr.routeType.staticRoute;
        return false;
    }

    /**
     * convert from json
     *
     * @param jsn json to read
     * @return true on error, false on success
     */
    public boolean fromJson(encJson jsn) {
        int i = jsn.findValue("maxLength");
        if (i < 0) {
            return true;
        }
        String a = jsn.getValue(i + 1);
        if (a == null) {
            return true;
        }
        max = bits.str2num(a);
        i = jsn.findValue("prefix");
        if (i < 0) {
            return true;
        }
        a = jsn.getValue(i + 1);
        if (a == null) {
            return true;
        }
        prefix = addrPrefix.str2ip(a);
        if (prefix == null) {
            return true;
        }
        i = jsn.findValue("asn");
        if (i < 0) {
            return true;
        }
        a = jsn.getValue(i + 1);
        if (a == null) {
            return true;
        }
        distan = bits.str2num(a);
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
        for (int i = 0; i < asns.size(); i++) {
            a += " " + asns.get(i);
        }
        return addrPrefix.ip2str(prefix) + " " + max + a;
    }

    /**
     * convert to route
     *
     * @return converted
     */
    public String toShRoute() {
        String a = "";
        for (int i = 0; i < asns.size(); i++) {
            a += " " + clntWhois.asn2mixed(asns.get(i), true);
        }
        a = a.trim();
        return addrPrefix.ip2str(prefix) + "|" + max + "|" + a + "|" + bits.timePast(time);
    }

}
