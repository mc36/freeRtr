package net.freertr.tab;

import java.util.Comparator;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrPrefix;
import net.freertr.clnt.clntWhois;
import net.freertr.util.bits;
import net.freertr.util.cmds;

/**
 * route authorization entry
 *
 * @author matecsaba
 */
public class tabRouautNtry implements Comparator<tabRouautNtry> {

    /**
     * create instance
     */
    public tabRouautNtry() {
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

    public int compare(tabRouautNtry o1, tabRouautNtry o2) {
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
    public tabRouautNtry copyBytes() {
        tabRouautNtry n = new tabRouautNtry();
        n.prefix = prefix.copyBytes();
        n.max = max;
        n.asn = asn;
        n.distan = distan;
        n.srcIP = srcIP.copyBytes();
        n.srcRtr = srcRtr;
        n.srcNum = srcNum;
        n.time = time;
        return n;
    }

    /**
     * check if atrtibutes differs
     *
     * @param o other to compare to
     * @return numerical value if differred
     */
    public int differs(tabRouautNtry o) {
        if (o == null) {
            return 1001;
        }
        if (max != o.max) {
            return 1;
        }
        if (asn != o.asn) {
            return 1;
        }
        return 0;
    }

    /**
     * check of other is better
     *
     * @param o other
     * @return true if yes, false if not
     */
    public boolean isOtherBetter(tabRouautNtry o) {
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
