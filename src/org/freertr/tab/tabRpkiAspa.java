package org.freertr.tab;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.cfg.cfgAll;
import org.freertr.clnt.clntWhois;
import org.freertr.enc.encJson;
import org.freertr.enc.encJsonEntry;
import org.freertr.user.userFormat;
import org.freertr.util.bits;
import org.freertr.util.cmds;

/**
 * provider authorization entry
 *
 * @author matecsaba
 */
public class tabRpkiAspa implements Comparable<tabRpkiAspa> {

    /**
     * create instance
     */
    public tabRpkiAspa() {
    }

    /**
     * customer asn
     */
    public int cust;

    /**
     * allowed providers
     */
    public List<Integer> provs;

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

    public int compareTo(tabRpkiAspa o) {
        if (cust < o.cust) {
            return -1;
        }
        if (cust > o.cust) {
            return +1;
        }
        return 0;
    }

    public String toString() {
        return "" + cust;
    }

    /**
     * copy bytes
     *
     * @return copy
     */
    public tabRpkiAspa copyBytes() {
        tabRpkiAspa n = new tabRpkiAspa();
        n.cust = cust;
        if (provs != null) {
            n.provs = new ArrayList<Integer>(provs);
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
     * print aspa details
     *
     * @return text
     */
    public userFormat fullDump() {
        userFormat res = new userFormat("|", "category|value");
        res.add("customer|" + clntWhois.asn2mixed(cust, true));
        for (int i = 0; i < provs.size(); i++) {
            res.add("as number|" + clntWhois.asn2mixed(provs.get(i), true));
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
    public int differs(tabRpkiAspa o) {
        if (o == null) {
            return 1;
        }
        if (cust != o.cust) {
            return 2;
        }
        if (provs.size() != o.provs.size()) {
            return 3;
        }
        if (o.compareTo(this) != 0) {
            return 4;
        }
        for (int i = 0; i < provs.size(); i++) {
            if (provs.get(i) != o.provs.get(i)) {
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
    public boolean isOtherBetter(tabRpkiAspa o) {
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
        cust = bits.str2num(cmd.word());
        provs = new ArrayList<Integer>();
        for (;;) {
            String a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            provs.add(bits.str2num(a));
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
        int i = jsn.findValue("customer_asid");
        if (i < 0) {
            return true;
        }
        String a = jsn.getValue(i + 1);
        if (a == null) {
            return true;
        }
        cust = bits.str2num(a);
        i = jsn.findValue("providers");
        if (i < 0) {
            return true;
        }
        encJsonEntry jnt = jsn.getData(i);
        if (jnt == null) {
            return true;
        }
        provs = new ArrayList<Integer>();
        int o = jnt.level + 1;
        for (;;) {
            i++;
            jnt = jsn.getData(i);
            if (jnt == null) {
                break;
            }
            if (jnt.level != o) {
                break;
            }
            provs.add(bits.str2num(jnt.data));
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
        String a = "" + cust;
        for (int i = 0; i < provs.size(); i++) {
            a += " " + provs.get(i);
        }
        return a;
    }

    /**
     * convert to route
     *
     * @return converted
     */
    public String toShRoute() {
        String a = "";
        for (int i = 0; i < provs.size(); i++) {
            a += " " + clntWhois.asn2mixed(provs.get(i), true);
        }
        a = a.trim();
        return clntWhois.asn2mixed(cust, true) + "|" + a + "|" + bits.timePast(time);
    }

}
