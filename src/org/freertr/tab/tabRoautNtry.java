package org.freertr.tab;

import org.freertr.addr.addrIP;
import org.freertr.addr.addrPrefix;
import org.freertr.cfg.cfgAll;
import org.freertr.clnt.clntWhois;
import org.freertr.user.userFormat;
import org.freertr.util.bits;
import org.freertr.util.cmds;

/**
 * route authorization entry
 *
 * @author matecsaba
 */
public class tabRoautNtry implements Comparable<tabRoautNtry> {

    /**
     * create instance
     */
    public tabRoautNtry() {
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

    /**
     * hit counter
     */
    public int hits;

    public int compareTo(tabRoautNtry o) {
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
    public tabRoautNtry copyBytes() {
        tabRoautNtry n = new tabRoautNtry();
        n.prefix = prefix.copyBytes();
        n.max = max;
        n.asn = asn;
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
        res.add("as number|" + asn);
        res.add("as name|" + clntWhois.asn2name(asn, true));
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
    public int differs(tabRoautNtry o) {
        if (o == null) {
            return 1001;
        }
        if (max != o.max) {
            return 1;
        }
        if (asn != o.asn) {
            return 1;
        }
        if (o.compareTo(this) != 0) {
            return 2;
        }
        return 0;
    }

    /**
     * check of other is better
     *
     * @param o other
     * @return true if yes, false if not
     */
    public boolean isOtherBetter(tabRoautNtry o) {
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
        return addrPrefix.ip2str(prefix) + "|" + max + "|" + bits.num2str(asn) + "|" + clntWhois.asn2name(asn, true) + "|" + bits.timePast(time);
    }

}
