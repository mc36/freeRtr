package org.freertr.tab;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.cfg.cfgAll;
import org.freertr.clnt.clntWhois;
import org.freertr.enc.encBase64;
import org.freertr.enc.encJson;
import org.freertr.user.userFormat;
import org.freertr.util.bits;
import org.freertr.util.cmds;

/**
 * subject key identifier
 *
 * @author matecsaba
 */
public class tabRpkiSki implements Comparable<tabRpkiSki> {

    /**
     * create instance
     */
    public tabRpkiSki() {
    }

    /**
     * customer asn
     */
    public int asn;

    /**
     * ski value
     */
    public byte ski[];

    /**
     * public key
     */
    public byte key[];

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

    public int compareTo(tabRpkiSki o) {
        if (asn < o.asn) {
            return -1;
        }
        if (asn > o.asn) {
            return +1;
        }
        if (ski.length < o.ski.length) {
            return -1;
        }
        if (ski.length > o.ski.length) {
            return +1;
        }
        return bits.byteComp(ski, 0, o.ski, 0, ski.length);
    }

    public String toString() {
        return asn + " " + bits.byteDump(ski, 0, ski.length);
    }

    /**
     * copy bytes
     *
     * @return copy
     */
    public tabRpkiSki copyBytes() {
        tabRpkiSki n = new tabRpkiSki();
        n.asn = asn;
        n.ski = bits.byteConcat(new byte[0], ski);
        n.key = bits.byteConcat(new byte[0], key);
        n.srcIP = srcIP.copyBytes();
        n.srcRtr = srcRtr;
        n.srcNum = srcNum;
        n.time = time;
        n.hits = hits;
        return n;
    }

    /**
     * print ski details
     *
     * @return text
     */
    public userFormat fullDump() {
        userFormat res = new userFormat("|", "category|value");
        res.add("asn|" + clntWhois.asn2mixed(asn, true));
        res.add("ski|" + bits.byteDump(ski, 0, ski.length));
        res.add("key|" + bits.byteDump(key, 0, key.length));
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
    public int differs(tabRpkiSki o) {
        if (o == null) {
            return 1;
        }
        if (asn != o.asn) {
            return 2;
        }
        if (ski.length != o.ski.length) {
            return 3;
        }
        if (key.length != o.key.length) {
            return 4;
        }
        if (o.compareTo(this) != 0) {
            return 5;
        }
        if (bits.byteComp(key, 0, o.key, 0, key.length) != 0) {
            return 6;
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

    private static byte[] hex2ski(String s) {
        List<Integer> arr = new ArrayList<Integer>();
        for (; s.length() > 1;) {
            String a = s.substring(0, 2);
            s = s.substring(2, s.length());
            arr.add(bits.fromHex(a));
        }
        byte[] res = new byte[20];
        for (; arr.size() < res.length;) {
            arr.add(0);
        }
        for (int i = 0; i < res.length; i++) {
            int o = arr.get(i);
            res[i] = (byte) o;
        }
        return res;
    }

    /**
     * convert from string
     *
     * @param cmd commands to read
     * @return true on error, false on success
     */
    public boolean fromString(cmds cmd) {
        asn = bits.str2num(cmd.word());
        ski = hex2ski(cmd.word());
        key = encBase64.decodeBytes(cmd.word());
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
        int i = jsn.findValue("pubkey");
        if (i < 0) {
            return true;
        }
        String a = jsn.getValue(i + 1);
        if (a == null) {
            return true;
        }
        key = encBase64.decodeBytes(a);
        i = jsn.findValue("ski");
        if (i < 0) {
            return true;
        }
        a = jsn.getValue(i + 1);
        if (a == null) {
            return true;
        }
        ski = hex2ski(a);
        i = jsn.findValue("asn");
        if (i < 0) {
            return true;
        }
        a = jsn.getValue(i + 1);
        if (a == null) {
            return true;
        }
        asn = bits.str2num(a);
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
        String a = asn + " ";
        for (int i = 0; i < ski.length; i++) {
            a += "" + bits.toHexB(ski[i]);
        }
        return a + " " + encBase64.encodeBytes(key);
    }

    /**
     * convert to route
     *
     * @return converted
     */
    public String toShRoute() {
        return clntWhois.asn2mixed(asn, true) + "|" + bits.toHex(ski) + "|" + bits.timePast(time);
    }

}
