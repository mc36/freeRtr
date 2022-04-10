package net.freertr.tab;

import java.util.ArrayList;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrPrefix;
import net.freertr.addr.addrType;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.rtr.rtrBgpUtil;
import net.freertr.user.userScript;
import net.freertr.util.bits;
import net.freertr.util.cmds;

/**
 * route utilities
 *
 * @author matecsaba
 */
public class tabRouteUtil {

    private tabRouteUtil() {
    }

    /**
     * execute script
     *
     * @param afi address family
     * @param asn as number
     * @param attr attribute to update
     * @param net prefix to update
     * @param scr updater script
     */
    public static void doTcl(int afi, int asn, tabRouteAttr<addrIP> attr, tabRouteEntry<addrIP> net, List<String> scr) {
        pipeLine pl = new pipeLine(32768, false);
        pipeSide pip = pl.getSide();
        pip.setTime(10000);
        pip.lineRx = pipeSide.modTyp.modeCRorLF;
        pip.lineTx = pipeSide.modTyp.modeCRLF;
        userScript t = new userScript(pip, "");
        t.allowExec = true;
        t.addLine("set seconds " + (bits.getTime() / 1000));
        t.addLine("set afi " + (afi & rtrBgpUtil.afiMask));
        t.addLine("set safi " + (afi & rtrBgpUtil.sfiMask));
        t.addLine("set peerasn " + asn);
        t.addLine("set prefix " + addrPrefix.ip2str(net.prefix));
        t.addLine("set network " + net.prefix.network);
        t.addLine("set masklen " + net.prefix.maskLen);
        t.addLine("set netmask " + net.prefix.mask);
        t.addLine("set wildcard " + net.prefix.wildcard);
        t.addLine("set broadcast " + net.prefix.broadcast);
        t.addLine("set rd " + rd2string(net.rouDst));
        t.addLine("set oldrd " + rd2string(net.oldDst));
        t.addLine("set nexthop " + attr.nextHop);
        t.addLine("set oldhop " + attr.oldHop);
        t.addLine("set distance " + attr.distance);
        t.addLine("set validity " + attr.validity);
        t.addLine("set locpref " + attr.locPref);
        t.addLine("set aigp " + attr.accIgp);
        t.addLine("set bandwidth " + attr.bandwidth);
        t.addLine("set origin " + attr.origin);
        t.addLine("set metric " + attr.metric);
        t.addLine("set tag " + attr.tag);
        t.addLine("set segrout " + attr.segrouIdx);
        t.addLine("set bier " + attr.bierIdx);
        t.addLine("set aspath \"" + attr.asPathStr() + "\"");
        t.addLine("set asend \"" + attr.asPathEnd() + "\"");
        t.addLine("set asbeg \"" + attr.asPathBeg() + "\"");
        t.addLine("set pathlen \"" + attr.asPathLen() + "\"");
        t.addLine("set stdcomm \"" + stdComms2string(attr.stdComm) + "\"");
        t.addLine("set extcomm \"" + extComms2string(attr.extComm) + "\"");
        t.addLine("set lrgcomm \"" + lrgComms2string(attr.lrgComm) + "\"");
        t.addLines(scr);
        pip = pl.getSide();
        pip.lineRx = pipeSide.modTyp.modeCRorLF;
        pip.lineTx = pipeSide.modTyp.modeCR;
        t.cmdAll();
        pl.setClose();
        for (;;) {
            if (pip.ready2rx() < 1) {
                return;
            }
            String a = pip.lineGet(1);
            if (a == null) {
                return;
            }
            if (a.length() < 1) {
                continue;
            }
            cmds cmd = new cmds("tcl", a);
            a = cmd.word();
            if (a.equals("nexthop")) {
                attr.nextHop.fromString(cmd.word());
                continue;
            }
            if (a.equals("distance")) {
                attr.distance = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("locpref")) {
                attr.locPref = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("aigp")) {
                attr.accIgp = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("bandwidth")) {
                attr.bandwidth = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("validity")) {
                attr.validity = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("origin")) {
                attr.origin = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("metric")) {
                attr.metric = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("tag")) {
                attr.tag = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("segrout")) {
                attr.segrouIdx = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("bier")) {
                attr.bierIdx = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("aspath")) {
                List<Integer> lst = string2intList(cmd.getRemaining());
                attr.pathSeq = tabLabel.prependLabels(attr.pathSeq, lst);
                continue;
            }
            if (a.equals("stdcomm")) {
                attr.stdComm = string2stdComms(cmd.getRemaining());
                continue;
            }
            if (a.equals("extcomm")) {
                attr.extComm = string2extComms(cmd.getRemaining());
                continue;
            }
            if (a.equals("lrgcomm")) {
                attr.lrgComm = string2lrgComms(cmd.getRemaining());
                continue;
            }
        }
    }

    /**
     * convert tunnel type to extended community
     *
     * @param i tunnel type
     * @return extended community
     */
    public static long tunTyp2comm(long i) {
        return 219550481834311680L | (i & 281474976710655L);
    }

    /**
     * convert community to string
     *
     * @param i community to convert
     * @return converted string
     */
    public static String stdComm2string(int i) {
        switch (i) {
            case rtrBgpUtil.commNoExport:
                return "noexport";
            case rtrBgpUtil.commNoAdvertise:
                return "noadvertise";
            case rtrBgpUtil.commNoConfed:
                return "localas";
            case rtrBgpUtil.commNoPeer:
                return "nopeer";
            case rtrBgpUtil.commAcceptOwn:
                return "acceptown";
            case rtrBgpUtil.commBlackhole:
                return "blackhole";
            case rtrBgpUtil.commGraceShut:
                return "graceshut";
            case rtrBgpUtil.commLlgrStale:
                return "llgrstale";
            case rtrBgpUtil.commNoLlgr:
                return "nollgr";
            case rtrBgpUtil.commAcceptHop:
                return "accepthop";
        }
        return (i >>> 16) + ":" + (i & 65535);
    }

    /**
     * generate flowspec rate
     *
     * @param as as number
     * @param bw flow rate
     * @return extended community
     */
    public static long rate2comm(long as, long bw) {
        return (((as & 65535) | -2147090432) << 32) | Float.floatToIntBits(bw);
    }

    /**
     * convert string to int list
     *
     * @param s string
     * @return int list
     */
    public static List<Integer> string2intList(String s) {
        List<Integer> l = new ArrayList<Integer>();
        cmds cmd = new cmds("", s);
        for (;;) {
            String a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            int i = bits.str2num(a);
            l.add(i);
        }
        return l;
    }

    /**
     * convert community to integer
     *
     * @param s string to convert
     * @return converted
     */
    public static long string2extComm(String s) {
        s = s.trim();
        int i = s.indexOf(":");
        if (i < 0) {
            return bits.str2num(s);
        }
        long o = (long) bits.str2num(s.substring(0, i)) << 48;
        s = s.substring(i + 1, s.length());
        i = s.indexOf(":");
        if (i < 0) {
            return o | bits.str2num(s);
        }
        return o | ((long) bits.str2num(s.substring(0, i)) << 32) | bits.str2num(s.substring(i + 1, s.length()));
    }

    /**
     * convert string to large community list
     *
     * @param a string
     * @return list
     */
    public static List<tabLargeComm> string2lrgComms(String a) {
        cmds cmd = new cmds("", a);
        List<tabLargeComm> l = new ArrayList<tabLargeComm>();
        for (;;) {
            a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            tabLargeComm d = new tabLargeComm();
            if (d.fromString(a)) {
                continue;
            }
            l.add(d);
        }
        return l;
    }

    /**
     * convert large community list to string
     *
     * @param l list
     * @return string
     */
    public static String lrgComms2string(List<tabLargeComm> l) {
        if (l == null) {
            return "";
        }
        if (l.size() < 1) {
            return "";
        }
        String a = "";
        for (int i = 0; i < l.size(); i++) {
            a += " " + l.get(i);
        }
        return a.substring(1, a.length());
    }

    /**
     * convert community to string
     *
     * @param i community to convert
     * @return converted string
     */
    public static String extComm2string(long i) {
        return (i >>> 48) + ":" + ((i >>> 32) & 65535) + ":" + (int) (i & 4294967295L);
    }

    /**
     * decode flowspec rate
     *
     * @param comm extended community
     * @return -1 on error, rate if success
     */
    public static long comm2rate(long comm) {
        if (((comm >>> 48) & 65535) != 32774) {
            return -1;
        }
        float rate = Float.intBitsToFloat((int) comm);
        if (rate < 1) {
            return 0;
        }
        if (rate > Long.MAX_VALUE) {
            return Long.MAX_VALUE;
        }
        return (long) rate;
    }

    /**
     * convert community list to string
     *
     * @param l community list
     * @return string
     */
    public static String stdComms2string(List<Integer> l) {
        if (l == null) {
            return "";
        }
        if (l.size() < 1) {
            return "";
        }
        String s = "";
        for (int i = 0; i < l.size(); i++) {
            s += " " + stdComm2string(l.get(i));
        }
        return s.substring(1, s.length());
    }

    /**
     * stdcomm with asn
     *
     * @param comm community
     * @param asn asn
     * @return merged
     */
    public static int stdCommAsn(int comm, int asn) {
        return (comm & -65536) | (asn & 65535);
    }

    /**
     * generate dmz link bandwidth
     *
     * @param as as number
     * @param bw bandwodth
     * @return extended community
     */
    public static long dmzBw2comm(long as, long bw) {
        return (((as & 65535) | 1074003968) << 32) | bw;
    }

    /**
     * convert layer2 info to extended community
     *
     * @param enc encapsulation (19=vpls)
     * @param flg flags (bit0=sequence, bit1=controlword)
     * @param mtu mtu
     * @return extended community
     */
    public static long l2info2comm(int enc, int flg, int mtu) {
        long i = -2146828288 | ((enc & 255) << 8) | (flg & 255);
        return (i << 32) | ((mtu & 65535) << 16);
    }

    /**
     * convert string to community list
     *
     * @param s string
     * @return community list
     */
    public static List<Integer> string2stdComms(String s) {
        List<Integer> l = new ArrayList<Integer>();
        cmds cmd = new cmds("", s);
        for (;;) {
            s = cmd.word();
            if (s.length() < 1) {
                break;
            }
            int i = string2stdComm(s);
            if (i == 0) {
                continue;
            }
            l.add(i);
        }
        return l;
    }

    /**
     * convert agi to extended community
     *
     * @param i agi
     * @return extended community
     */
    public static long agi2comm(long i) {
        long as = (i >>> 32) & 65535;
        long id = i & 4294967295L;
        return ((as | 655360) << 32) | id;
    }

    /**
     * convert string to community list
     *
     * @param s string
     * @return community list
     */
    public static List<Long> string2extComms(String s) {
        List<Long> l = new ArrayList<Long>();
        cmds cmd = new cmds("", s);
        for (;;) {
            s = cmd.word();
            if (s.length() < 1) {
                break;
            }
            long i = string2extComm(s);
            if (i == 0) {
                continue;
            }
            l.add(i);
        }
        return l;
    }

    /**
     * convert route target to extended community
     *
     * @param i route target
     * @return extended community
     */
    public static long rt2comm(long i) {
        long as = (i >>> 32) & 65535;
        long id = i & 4294967295L;
        return ((as | 131072) << 32) | id;
    }

    /**
     * convert rd to integer
     *
     * @param s string to convert
     * @return converted
     */
    public static long string2rd(String s) {
        int i = s.indexOf(":");
        if (i < 0) {
            s = "0:0" + s;
            i = 1;
        }
        long asn = bits.str2num(s.substring(0, i));
        long num = bits.str2num(s.substring(i + 1, s.length()));
        return (asn << 32) | (num & 4294967295L);
    }

    /**
     * convert community to integer
     *
     * @param s string to convert
     * @return converted
     */
    public static int string2stdComm(String s) {
        s = s.trim().toLowerCase();
        if (s.equals("noexport")) {
            return rtrBgpUtil.commNoExport;
        }
        if (s.equals("noadvertise")) {
            return rtrBgpUtil.commNoAdvertise;
        }
        if (s.equals("localas")) {
            return rtrBgpUtil.commNoConfed;
        }
        if (s.equals("nopeer")) {
            return rtrBgpUtil.commNoPeer;
        }
        if (s.equals("acceptown")) {
            return rtrBgpUtil.commAcceptOwn;
        }
        if (s.equals("blackhole")) {
            return rtrBgpUtil.commBlackhole;
        }
        if (s.equals("graceshut")) {
            return rtrBgpUtil.commGraceShut;
        }
        if (s.equals("llgrstale")) {
            return rtrBgpUtil.commLlgrStale;
        }
        if (s.equals("nollgr")) {
            return rtrBgpUtil.commNoLlgr;
        }
        if (s.equals("accepthop")) {
            return rtrBgpUtil.commAcceptHop;
        }
        int i = s.indexOf(":");
        if (i < 0) {
            return bits.str2num(s);
        }
        return (bits.str2num(s.substring(0, i)) << 16) | bits.str2num(s.substring(i + 1, s.length()));
    }

    /**
     * convert community list to string
     *
     * @param l community list
     * @return string
     */
    public static String extComms2string(List<Long> l) {
        if (l == null) {
            return "";
        }
        if (l.size() < 1) {
            return "";
        }
        String s = "";
        for (int i = 0; i < l.size(); i++) {
            s += " " + extComm2string(l.get(i));
        }
        return s.substring(1, s.length());
    }

    /**
     * convert rd to string
     *
     * @param i rd to convert
     * @return converted string
     */
    public static String rd2string(long i) {
        return (i >>> 32) + ":" + (int) (i & 4294967295L);
    }

    /**
     * remove from standard community
     *
     * @param <T> class of address
     * @param attr attribute to update
     * @param mtch match to remove
     * @return number of occurences removed
     */
    public static <T extends addrType> int removeStdComm(tabRouteAttr<T> attr, String mtch) {
        if (attr.stdComm == null) {
            return 0;
        }
        int o = 0;
        for (int i = attr.stdComm.size() - 1; i >= 0; i--) {
            if (!tabRouteUtil.stdComm2string(attr.stdComm.get(i)).matches(mtch)) {
                continue;
            }
            attr.stdComm.remove(i);
            o++;
        }
        return o;
    }

    /**
     * remove from cluster list
     *
     * @param <T> class of address
     * @param attr attribute to update
     * @param mtch match to remove
     * @return number of occurences removed
     */
    public static <T extends addrType> int removeClstLst(tabRouteAttr<T> attr, String mtch) {
        if (attr.clustList == null) {
            return 0;
        }
        int o = 0;
        for (int i = attr.clustList.size() - 1; i >= 0; i--) {
            if (!("" + attr.clustList.get(i)).matches(mtch)) {
                continue;
            }
            attr.clustList.remove(i);
            o++;
        }
        return o;
    }

    /**
     * replace on integer list
     *
     * @param lst list to use
     * @param src source to replace
     * @param trg target to replace
     */
    public static void replaceIntList(List<Integer> lst, int src, int trg) {
        if (lst == null) {
            return;
        }
        for (int i = 0; i < lst.size(); i++) {
            if (lst.get(i) == src) {
                lst.set(i, trg);
            }
        }
    }

    /**
     * first on integer list
     *
     * @param lst list to use
     * @param val value to check
     * @return false if yes, true if not
     */
    public static boolean firstIntList(List<Integer> lst, int val) {
        if (lst == null) {
            return true;
        }
        if (lst.size() < 1) {
            return true;
        }
        if (lst.get(0) != val) {
            return true;
        }
        return false;
    }

    /**
     * remove from extended community
     *
     * @param <T> class of address
     * @param attr attribute to update
     * @param mtch match to remove
     * @return number of occurences removed
     */
    public static <T extends addrType> int removeExtComm(tabRouteAttr<T> attr, String mtch) {
        if (attr.extComm == null) {
            return 0;
        }
        int o = 0;
        for (int i = attr.extComm.size() - 1; i >= 0; i--) {
            if (!tabRouteUtil.extComm2string(attr.extComm.get(i)).matches(mtch)) {
                continue;
            }
            attr.extComm.remove(i);
            o++;
        }
        return o;
    }

    /**
     * find on integer list
     *
     * @param lst list to use
     * @param val value to find
     * @return position, -1 if not found
     */
    public static int findIntList(List<Integer> lst, int val) {
        if (lst == null) {
            return -1;
        }
        for (int i = 0; i < lst.size(); i++) {
            if (lst.get(i) == val) {
                return i;
            }
        }
        return -1;
    }

    /**
     * convert as number to 16 bits
     *
     * @param i as number to convert
     * @return converted
     */
    public static int asNum16bit(int i) {
        if ((i & 65535) == i) {
            return i;
        }
        return 23456;
    }

    /**
     * test if documentary as number
     *
     * @param i as number to test
     * @return false if not, true if yes
     */
    public static boolean asNumDocumentary(int i) {
        if ((i >= 64496) && (i <= 64511)) {
            return true;
        }
        if ((i >= 65536) && (i <= 65551)) {
            return true;
        }
        return false;
    }

    /**
     * remove from large community
     *
     * @param <T> class of address
     * @param attr attribute to update
     * @param mtch match to remove
     * @return number of occurences removed
     */
    public static <T extends addrType> int removeLrgComm(tabRouteAttr<T> attr, String mtch) {
        if (attr.lrgComm == null) {
            return 0;
        }
        int o = 0;
        for (int i = attr.lrgComm.size() - 1; i >= 0; i--) {
            if (!("" + attr.lrgComm.get(i)).matches(mtch)) {
                continue;
            }
            attr.lrgComm.remove(i);
            o++;
        }
        return o;
    }

    /**
     * find on long list
     *
     * @param lst list to use
     * @param val value to find
     * @return position, -1 if not found
     */
    public static int findLongList(List<Long> lst, long val) {
        if (lst == null) {
            return -1;
        }
        for (int i = 0; i < lst.size(); i++) {
            if (lst.get(i) == val) {
                return i;
            }
        }
        return -1;
    }

    /**
     * find in address list
     *
     * @param <T> address type
     * @param lst list of addresses
     * @param adr address to find
     * @return index of entry, -1 if not found
     */
    public static <T extends addrType> int findAddrList(List<T> lst, T adr) {
        for (int i = 0; i < lst.size(); i++) {
            if (adr.compare(lst.get(i), adr) == 0) {
                return i;
            }
        }
        return -1;
    }

    /**
     * remove first as numbers
     *
     * @param <T> class of address
     * @param attr attribute to update
     * @return number of occurences removed
     */
    public static <T extends addrType> int removeFirstAs(tabRouteAttr<T> attr) {
        if (attr.pathSeq == null) {
            return 0;
        }
        if (attr.pathSeq.size() < 1) {
            return 0;
        }
        int o = attr.pathSeq.get(0);
        int i = removeIntList(attr.pathSeq, o);
        i += removeIntList(attr.pathSet, o);
        return i;
    }

    /**
     * find on large list
     *
     * @param lst list to use
     * @param val value to find
     * @return position, -1 if not found
     */
    public static int findLrgList(List<tabLargeComm> lst, tabLargeComm val) {
        if (lst == null) {
            return -1;
        }
        for (int i = 0; i < lst.size(); i++) {
            if (val.compare(val, lst.get(i)) == 0) {
                return i;
            }
        }
        return -1;
    }

    /**
     * replace on integer list
     *
     * @param lst list to use
     * @param src source to replace
     * @return number of occurences removed
     */
    public static int removeIntList(List<Integer> lst, int src) {
        if (lst == null) {
            return 0;
        }
        int o = 0;
        for (int i = lst.size() - 1; i >= 0; i--) {
            if (lst.get(i) == src) {
                lst.remove(i);
                o++;
            }
        }
        return o;
    }

    /**
     * test if private as number
     *
     * @param i as number to test
     * @return false if not, true if yes
     */
    public static boolean asNumPrivate(int i) {
        if ((i >= 64512) && (i <= 65534)) {
            return true;
        }
        if ((i >= -94967296) && (i < -2)) {
            // 4200000000 - 4294967294
            return true;
        }
        return false;
    }

    /**
     * remove private as numbers
     *
     * @param lst list to use
     * @return number of occurences removed
     */
    public static int removePrivateAs(List<Integer> lst) {
        if (lst == null) {
            return 0;
        }
        int o = 0;
        for (int i = lst.size() - 1; i >= 0; i--) {
            if (asNumPrivate(lst.get(i))) {
                lst.remove(i);
                o++;
            }
        }
        return o;
    }

    /**
     * compare two integer lists
     *
     * @param l1 first list
     * @param l2 second list
     * @return true if differs
     */
    public static boolean diffIntList(List<Integer> l1, List<Integer> l2) {
        if (l1 == null) {
            return l2 != null;
        }
        if (l2 == null) {
            return true;
        }
        if (l1.size() != l2.size()) {
            return true;
        }
        for (int i = 0; i < l1.size(); i++) {
            if (l1.get(i).compareTo(l2.get(i)) != 0) {
                return true;
            }
        }
        return false;
    }

    /**
     * dump integer list
     *
     * @param l list to dump
     * @param beg beginning
     * @param end ending
     * @return dumped list
     */
    public static String dumpIntList(List<Integer> l, String beg, String end) {
        if (l == null) {
            return "";
        }
        if (l.size() < 1) {
            return "";
        }
        String s = "";
        for (int i = 0; i < l.size(); i++) {
            s += " " + bits.num2str(l.get(i));
        }
        return beg + s.substring(1, s.length()) + end;
    }

    /**
     * size of list
     *
     * @param lst list to check
     * @return size of list
     */
    public static int listLen(List<?> lst) {
        if (lst == null) {
            return 0;
        }
        return lst.size();
    }

    /**
     * dump address list
     *
     * @param <T> type of address
     * @param l address list
     * @return dumped list
     */
    public static <T extends addrType> String dumpAddrList(List<T> l) {
        if (l == null) {
            return "";
        }
        if (l.size() < 1) {
            return "";
        }
        String s = "";
        for (int i = 0; i < l.size(); i++) {
            s += " " + l.get(i);
        }
        return s.substring(1, s.length());
    }

    /**
     * null empty list
     *
     * @param <E> type of list
     * @param l list
     * @return maybe nulled list
     */
    public static <E extends Object> List<E> nullEmptyList(List<E> l) {
        if (l == null) {
            return null;
        }
        if (l.size() < 1) {
            return null;
        }
        return l;
    }

}
