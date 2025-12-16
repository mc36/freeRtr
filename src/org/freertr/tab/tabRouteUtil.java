package org.freertr.tab;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrIPv6;
import org.freertr.addr.addrPrefix;
import org.freertr.addr.addrType;
import org.freertr.cfg.cfgIfc;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipMpls;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.rtr.rtrBgpUtil;
import org.freertr.user.userFormat;
import org.freertr.user.userScript;
import org.freertr.util.bits;
import org.freertr.util.cmds;

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
        t.addLine("set validroa " + attr.validRoa);
        t.addLine("set validaspa " + attr.validAspa);
        t.addLine("set aggras " + attr.aggrAs);
        t.addLine("set aggrtr " + attr.aggrRtr);
        t.addLine("set customer " + attr.onlyCust);
        t.addLine("set locpref " + attr.locPref);
        t.addLine("set aigp " + attr.accIgp);
        t.addLine("set bandwidth " + attr.bandwidth);
        t.addLine("set origin " + attr.origin);
        t.addLine("set metric " + attr.metric);
        t.addLine("set tag " + attr.tag);
        t.addLine("set segrout " + attr.segrouIdx);
        t.addLine("set bier " + attr.bierIdx + " " + attr.bierSub);
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
            if (a.equals("validroa")) {
                attr.validRoa = bits.str2num(cmd.word());
                attr.extComm = tabRouteUtil.setValidExtCommRoa(attr.extComm, attr.validRoa);
                continue;
            }
            if (a.equals("validaspa")) {
                attr.validAspa = bits.str2num(cmd.word());
                attr.extComm = tabRouteUtil.setValidExtCommAspa(attr.extComm, attr.validAspa);
                continue;
            }
            if (a.equals("aggras")) {
                attr.aggrAs = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("aggrrtr")) {
                attr.aggrRtr = new addrIP();
                attr.aggrRtr.fromString(cmd.word());
                continue;
            }
            if (a.equals("customer")) {
                attr.onlyCust = bits.str2num(cmd.word());
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
                attr.bierSub = bits.str2num(cmd.word());
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
     * generate flowspec divert
     *
     * @param rt route target
     * @return extended community
     */
    public static long divert2comm(long rt) {
        return (0x8008L << 48) | (rt & 0xffffffffffffL);
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
     * decode flowspec divert
     *
     * @param comm extended community
     * @return 0 on error, rd if success
     */
    public static long comm2divert(long comm) {
        if (((comm >>> 48) & 65535) != 0x8008) {
            return 0;
        }
        return comm & 0xffffffffffffL;
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
     * convert color to extended community
     *
     * @param i color
     * @return extended community
     */
    public static long clr2comm(int i) {
        return (167903232L << 32) | i;
    }

    /**
     * convert extended community to rtfilter entry
     *
     * @param a as number to use
     * @param i extended community
     * @return rtfilter route
     */
    public static addrPrefix<addrIP> extcomm2rtfilter(int a, long i) {
        addrIP adr = new addrIP();
        byte[] buf = adr.getBytes();
        bits.msbPutD(buf, 0, a);
        bits.msbPutQ(buf, 4, i);
        return new addrPrefix<addrIP>(adr, 96);
    }

    /**
     * find long in rtfilter table
     *
     * @param ext exteneded communities to look up
     * @param asn as number to use
     * @param rtf rtfilter to use
     * @param nhz nexthop must be zero
     * @return false if found, true if not
     */
    public static boolean findRtfilterTab(List<Long> ext, int asn, tabRoute<addrIP> rtf, boolean nhz) {
        if (ext == null) {
            return true;
        }
        for (int i = 0; i < ext.size(); i++) {
            Long cur = ext.get(i);
            if (cur == null) {
                continue;
            }
            addrPrefix<addrIP> prf = extcomm2rtfilter(asn, cur);
            tabRouteEntry<addrIP> rou = rtf.route(prf.network);
            if (rou == null) {
                continue;
            }
            if (!nhz) {
                return false;
            }
            if (rou.best.nextHop == null) {
                return false;
            }
        }
        return true;
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
            if (!stdComm2string(attr.stdComm.get(i)).matches(mtch)) {
                continue;
            }
            attr.stdComm.remove(i);
            o++;
        }
        return o;
    }

    /**
     * remove from unknown attributes
     *
     * @param <T> class of address
     * @param attr attribute to update
     * @param mtch match to keep
     * @return number of occurences removed
     */
    public static <T extends addrType> int removeUnknowns(tabRouteAttr<T> attr, tabIntMatcher mtch) {
        if (attr.unknown == null) {
            return 0;
        }
        if (mtch == null) {
            int i = attr.unknown.size();
            attr.unknown = null;
            return i;
        }
        int o = 0;
        for (int i = attr.unknown.size() - 1; i >= 0; i--) {
            tabRouteBlob cur = attr.unknown.get(i);
            if (mtch.matches(cur.type)) {
                continue;
            }
            attr.unknown.remove(i);
            o++;
        }
        return o;
    }

    /**
     * remove from originator
     *
     * @param <T> class of address
     * @param attr attribute to update
     * @param mtch match to remove
     * @return number of occurences removed
     */
    public static <T extends addrType> int removeOrgntr(tabRouteAttr<T> attr, String mtch) {
        if (attr.originator == null) {
            return 0;
        }
        if (!("" + attr.originator).matches(mtch)) {
            return 0;
        }
        attr.originator = null;
        return 1;
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
            if (!extComm2string(attr.extComm.get(i)).matches(mtch)) {
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
            if (lst.get(i).compareTo(adr) == 0) {
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
            if (val.compareTo(lst.get(i)) == 0) {
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
     * prepends in list
     *
     * @param lst list to check
     * @return prepends founds
     */
    public static int countPrepends(List<Integer> lst) {
        if (lst == null) {
            return 0;
        }
        if (lst.size() < 1) {
            return 0;
        }
        int p = lst.get(0);
        int r = 0;
        for (int i = 1; i < lst.size(); i++) {
            int o = lst.get(i);
            if (o == p) {
                r++;
            }
            p = o;
        }
        return r;
    }

    /**
     * loops in list
     *
     * @param lst list to check
     * @return loops founds
     */
    public static int countLoops(List<Integer> lst) {
        if (lst == null) {
            return 0;
        }
        int r = 0;
        for (int i = 1; i < lst.size(); i++) {
            int p = lst.get(i - 1);
            if (lst.get(i) == p) {
                continue;
            }
            for (int o = i + 1; o < lst.size(); o++) {
                if (lst.get(o) == p) {
                    r++;
                }
            }
        }
        return r;
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

    /**
     * update local label
     *
     * @param <T> class of address
     * @param attr attribute to update
     * @param setter updater
     */
    public static <T extends addrType> void updateLabloc(tabRouteAttr<T> attr, tabIntUpdater setter) {
        if (setter.action == tabIntUpdater.actionType.nothing) {
            return;
        }
        int lab = -1;
        if (attr.labelLoc != null) {
            lab = attr.labelLoc.label;
        }
        lab = setter.update(lab);
        attr.labelLoc = tabLabel.find(lab);
    }

    /**
     * update remote label
     *
     * @param <T> class of address
     * @param attr attribute to update
     * @param setter updater
     */
    public static <T extends addrType> void updateLabrem(tabRouteAttr<T> attr, tabIntUpdater setter) {
        switch (setter.action) {
            case nothing:
                break;
            case setter:
                attr.labelRem = tabLabel.prependLabel(attr.labelRem, setter.val);
                if (attr.labelRem.size() > 1) {
                    attr.labelRem.remove(1);
                }
                break;
            case adder:
                attr.labelRem = tabLabel.prependLabel(attr.labelRem, setter.val);
                break;
            case suber:
                removeIntList(attr.labelRem, setter.val);
                break;
            default:
                break;
        }
    }

    /**
     * append extended community list
     *
     * @param trg target list
     * @param src what to append
     * @return updated list
     */
    public static List<Long> appendLongList(List<Long> trg, List<Long> src) {
        if (trg == null) {
            trg = new ArrayList<Long>();
        }
        for (int i = 0; i < src.size(); i++) {
            Long rt = src.get(i);
            if (trg.indexOf(rt) >= 0) {
                continue;
            }
            trg.add(rt);
        }
        return trg;
    }

    /**
     * get validity extended community
     *
     * @param lst list to read
     * @return found value plus one
     */
    public static int getValidExtCommRoa(List<Long> lst) {
        if (lst == null) {
            return 0;
        }
        for (int i = 0; i < lst.size(); i++) {
            long o = lst.get(i);
            int p = (((int) o) & 0xff);
            o >>>= 32;
            if (o != rtrBgpUtil.commValidRoa) {
                continue;
            }
            if (p > 2) {
                p = 2;
            }
            return p + 1;
        }
        return 0;
    }

    /**
     * get validity extended community
     *
     * @param lst list to read
     * @return found value plus one
     */
    public static int getValidExtCommAspa(List<Long> lst) {
        if (lst == null) {
            return 0;
        }
        for (int i = 0; i < lst.size(); i++) {
            long o = lst.get(i);
            int p = (((int) o) & 0xff);
            o >>>= 32;
            if (o != rtrBgpUtil.commValidAspa) {
                continue;
            }
            if (p > 2) {
                p = 2;
            }
            return p + 1;
        }
        return 0;
    }

    /**
     * set validity extended community
     *
     * @param lst list to read
     * @param val value to set plus one
     * @return updated list
     */
    public static List<Long> setValidExtCommRoa(List<Long> lst, int val) {
        if (lst == null) {
            if (val == 0) {
                return null;
            }
            lst = new ArrayList<Long>();
        }
        for (int i = lst.size() - 1; i >= 0; i--) {
            long o = lst.get(i);
            if ((o >>> 32) != rtrBgpUtil.commValidRoa) {
                continue;
            }
            lst.remove(i);
        }
        if (val == 0) {
            return lst;
        }
        long l = rtrBgpUtil.commValidRoa;
        l <<= 32;
        l |= val - 1;
        lst.add(0, l);
        return lst;
    }

    /**
     * set validity extended community
     *
     * @param lst list to read
     * @param val value to set plus one
     * @return updated list
     */
    public static List<Long> setValidExtCommAspa(List<Long> lst, int val) {
        if (lst == null) {
            if (val == 0) {
                return null;
            }
            lst = new ArrayList<Long>();
        }
        for (int i = lst.size() - 1; i >= 0; i--) {
            long o = lst.get(i);
            if ((o >>> 32) != rtrBgpUtil.commValidAspa) {
                continue;
            }
            lst.remove(i);
        }
        if (val == 0) {
            return lst;
        }
        long l = rtrBgpUtil.commValidAspa;
        l <<= 32;
        l |= val - 1;
        lst.add(0, l);
        return lst;
    }

    /**
     * convert evpn layer3 route to prefix
     *
     * @param prefix prefix to convert
     * @return converted prefix, null if error happened
     */
    public static addrPrefix<addrIP> convertL3evpn(addrPrefix<addrIP> prefix) {
        if (prefix == null) {
            return null;
        }
        byte[] buf = new byte[addrIP.size];
        prefix.network.toBuffer(buf, 0);
        if (buf[0] != 5) {
            return null;
        }
        if (prefix.broadcast.isIPv4()) {
            int i = prefix.mask.toIPv4().toNetmask();
            addrIPv4 a = prefix.broadcast.toIPv4();
            return addrPrefix.ip4toIP(new addrPrefix<addrIPv4>(a, i));
        } else {
            int i = prefix.mask.toIPv6().toNetmask();
            addrIPv6 a = prefix.broadcast.toIPv6();
            return addrPrefix.ip6toIP(new addrPrefix<addrIPv6>(a, i));
        }
    }

    /**
     * put srv6 prefix
     *
     * @param ntry route entry
     * @param ifc srv6 interface
     * @param lab label entry
     * @return false if success, true if error
     */
    public static boolean generateSrv6pfx(tabRouteEntry<addrIP> ntry, cfgIfc ifc, tabLabelEntry lab) {
        if (ifc == null) {
            return true;
        }
        if (ifc.addr6 == null) {
            return true;
        }
        addrIP adr = new addrIP();
        adr.fromIPv6addr(ifc.addr6);
        bits.msbPutD(adr.getBytes(), 12, lab.label);
        for (int i = 0; i < ntry.alts.size(); i++) {
            tabRouteAttr<addrIP> attr = ntry.alts.get(i);
            attr.segrouPrf = adr.copyBytes();
            attr.labelLoc = new tabLabelEntry(ipMpls.labelImp);
        }
        return false;
    }

    /**
     * list unused prefixes
     *
     * @param src source table
     * @param trg target table
     */
    public static void unusedPrefixes(tabRoute<addrIP> src, List<String> trg) {
        addrIP nxt = new addrIP();
        addrIP one = new addrIP();
        one.fromString("::1");
        for (int i = 0; i < src.prefixes.size(); i++) {
            tabRouteEntry<addrIP> ntry = src.prefixes.get(i);
            if (nxt.compareTo(ntry.prefix.broadcast) >= 0) {
                continue;
            }
            addrIP adr = new addrIP();
            adr.setSub(ntry.prefix.network, one);
            unusedPrefixes(trg, nxt, adr, one);
            nxt.setAddr(ntry.prefix.broadcast);
        }
        addrIP adr = new addrIP();
        adr.fillBytes(255);
        unusedPrefixes(trg, nxt, adr, one);
    }

    private static void unusedPrefixes(List<String> lst, addrIP beg, addrIP end, addrIP one) {
        if (beg.compareTo(end) >= 0) {
            return;
        }
        addrIP adr = new addrIP();
        adr.setAdd(one, beg);
        lst.add(adr + " - " + end);
    }

    /**
     * compare diffs
     *
     * @param equ result
     * @param dif1 first diff
     * @param dif2 second diff
     */
    public static void compareDiffs(tabRoute<addrIP> equ, tabRoute<addrIP> dif1, tabRoute<addrIP> dif2) {
        for (int i = 0; i < dif1.size(); i++) {
            tabRouteEntry<addrIP> prf1 = dif1.get(i);
            tabRouteEntry<addrIP> prf2 = dif2.find(prf1);
            if (prf2 == null) {
                continue;
            }
            equ.add(tabRoute.addType.always, prf1, false, false);
        }
    }

    /**
     * compare tables
     *
     * @param uniq unique prefixes
     * @param diff differring prefixes
     * @param nei1 first feed
     * @param nei2 second feed
     * @param ign ignore flags
     * @param flt filter
     * @param safi safi
     * @param asn1 first asn
     * @param asn2 second asn
     * @param upd updater
     */
    public static void compareTables(tabRoute<addrIP> uniq, tabRoute<addrIP> diff, tabRoute<addrIP> nei1, tabRoute<addrIP> nei2, long ign, tabListing<tabRtrmapN, addrIP> flt, int safi, int asn1, int asn2, tabListing<tabRtrmapN, addrIP> upd) {
        for (int o = 0; o < nei1.size(); o++) {
            tabRouteEntry<addrIP> prf1 = nei1.get(o);
            if (prf1 == null) {
                continue;
            }
            if (flt != null) {
                if (flt.matches(safi, 0, prf1)) {
                    continue;
                }
            }
            prf1 = prf1.copyBytes(tabRoute.addType.alters);
            tabRouteEntry<addrIP> prf2 = nei2.find(prf1);
            if (prf2 == null) {
                uniq.add(tabRoute.addType.always, prf1, false, false);
                continue;
            }
            if (flt != null) {
                if (flt.matches(safi, 0, prf2)) {
                    continue;
                }
            }
            prf2 = prf2.copyBytes(tabRoute.addType.alters);
            for (int i = 0; i < prf1.alts.size(); i++) {
                tabRouteAttr.ignoreAttribs(prf1.alts.get(i), ign);
            }
            for (int i = 0; i < prf2.alts.size(); i++) {
                tabRouteAttr.ignoreAttribs(prf2.alts.get(i), ign);
            }
            if (upd != null) {
                upd.update(safi, asn1, prf1, false);
                upd.update(safi, asn2, prf2, false);
            }
            if (prf1.differs(tabRoute.addType.alters, prf2) == 0) {
                continue;
            }
            diff.add(tabRoute.addType.alters, prf1, false, false);
            diff.add(tabRoute.addType.alters, prf2, false, false);
        }
    }

    /**
     * null labeled routes
     *
     * @param lst source
     * @return result
     */
    public static tabRoute<addrIP> nullLabeled(tabRoute<addrIP> lst) {
        tabRoute<addrIP> res = new tabRoute<addrIP>("rx");
        for (int i = 0; i < lst.size(); i++) {
            tabRouteEntry<addrIP> ntry = lst.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.best.labelRem == null) {
                continue;
            }
            if (ntry.best.labelRem.size() != 1) {
                continue;
            }
            int o = ntry.best.labelRem.get(0);
            if ((o != ipMpls.labelImp) && (o != ipMpls.labelExp4) && (o != ipMpls.labelExp6)) {
                continue;
            }
            res.add(tabRoute.addType.always, ntry, false, false);
        }
        return res;
    }

    /**
     * filter one table
     *
     * @param afi address family
     * @param asn as number
     * @param tab table to filter
     * @param flt filter to use
     * @return number of entries deleted
     */
    public static int filterTable(int afi, int asn, tabRoute<addrIP> tab, tabListing<tabPrfxlstN, addrIP> flt) {
        if (flt == null) {
            return 0;
        }
        int deled = 0;
        for (int i = tab.size() - 1; i >= 0; i--) {
            tabRouteEntry<addrIP> ntry = tab.get(i);
            if (ntry == null) {
                continue;
            }
            if (flt.matches(afi, asn, ntry.prefix)) {
                continue;
            }
            tab.del(ntry.prefix);
            deled++;
        }
        return deled;
    }

    @SuppressWarnings("unchecked")
    private static <T extends addrType> boolean doNexthopFix(tabRouteAttr<T> attr, tabRoute<T> recurs, int recurn, tabRoute<T> nexthops, tabRoute<T> othrhops) {
        T hop = attr.nextHop;
        T orig = hop;
        if (hop == null) {
            return true;
        }
        for (int i = 0; i < recurn; i++) {
            tabRouteEntry<T> nhr = nexthops.route(hop);
            if (nhr != null) {
                if (nhr.best.nextHop != null) {
                    attr.oldHop = orig;
                    attr.nextHop = (T) nhr.best.nextHop.copyBytes();
                }
                attr.iface = nhr.best.iface;
                return false;
            }
            nhr = othrhops.route(hop);
            if (nhr != null) {
                if (nhr.best.nextHop != null) {
                    attr.oldHop = orig;
                    attr.nextHop = (T) nhr.best.nextHop.copyBytes();
                }
                ipFwdIface ifc = (ipFwdIface) nhr.best.iface;
                if (ifc == null) {
                    return true;
                }
                ifc = ifc.otherHandler;
                attr.iface = ifc;
                return false;
            }
            nhr = recurs.route(hop);
            if (nhr == null) {
                return true;
            }
            hop = nhr.best.nextHop;
            if (hop == null) {
                return true;
            }
            attr.oldHop = orig;
            attr.nextHop = (T) hop.copyBytes();
        }
        return true;
    }

    /**
     * fix nexthops on a route entry
     *
     * @param <T> class of address
     * @param imp route entry to update
     * @param recurs where to look up nexthops recursively
     * @param nexthops table where look up resolved nexthops
     * @param othrhops table where look up other nexthops
     * @param recurn maximum recursion depth
     * @return true if failed, false if ready
     */
    public static <T extends addrType> boolean doNexthopFix(tabRouteEntry<T> imp, tabRoute<T> recurs, tabRoute<T> nexthops, tabRoute<T> othrhops, int recurn) {
        for (int o = imp.alts.size() - 1; o >= 0; o--) {
            tabRouteAttr<T> attr = imp.alts.get(o);
            if (!doNexthopFix(attr, recurs, recurn, nexthops, othrhops)) {
                continue;
            }
            imp.delAlt(o);
        }
        if (imp.alts.size() < 1) {
            return true;
        }
        imp.hashBest();
        return false;
    }

    /**
     * list looping path entries
     *
     * @param lst table to update
     * @param pre loop matched
     * @return matching routes
     */
    public static tabRoute<addrIP> loopsFound(tabRoute<addrIP> lst, tabIntMatcher pre) {
        tabRoute<addrIP> res = new tabRoute<addrIP>("loop");
        for (int i = 0; i < lst.prefixes.size(); i++) {
            tabRouteEntry<addrIP> ntry = lst.prefixes.get(i);
            if (ntry == null) {
                continue;
            }
            if (!pre.matches(ntry.best.asPathLoop())) {
                continue;
            }
            res.add(tabRoute.addType.ecmp, ntry, false, false);
        }
        return res;
    }

    /**
     * list prepended path entries
     *
     * @param lst table to find
     * @param pre prepend matched
     * @return matching routes
     */
    public static tabRoute<addrIP> prependsUsed(tabRoute<addrIP> lst, tabIntMatcher pre) {
        tabRoute<addrIP> res = new tabRoute<addrIP>("prep");
        for (int i = 0; i < lst.prefixes.size(); i++) {
            tabRouteEntry<addrIP> ntry = lst.prefixes.get(i);
            if (ntry == null) {
                continue;
            }
            if (!pre.matches(ntry.best.asPathPrep())) {
                continue;
            }
            res.add(tabRoute.addType.ecmp, ntry, false, false);
        }
        return res;
    }

    /**
     * list unusual path attributes
     *
     * @param lst table to find
     * @param ign attributes to filter
     * @return matching routes
     */
    public static tabRoute<addrIP> unusualAttribs(tabRoute<addrIP> lst, long ign) {
        tabRoute<addrIP> res = new tabRoute<addrIP>("prep");
        for (int i = 0; i < lst.prefixes.size(); i++) {
            tabRouteEntry<addrIP> ntry = lst.prefixes.get(i);
            if (ntry == null) {
                continue;
            }
            tabRouteEntry<addrIP> oth = ntry.copyBytes(tabRoute.addType.alters);
            for (int o = 0; o < oth.alts.size(); o++) {
                tabRouteAttr.ignoreAttribs(ntry.alts.get(o), 0);
                tabRouteAttr.ignoreAttribs(oth.alts.get(o), ign);
            }
            if (ntry.differs(tabRoute.addType.alters, oth) == 0) {
                continue;
            }
            res.add(tabRoute.addType.ecmp, ntry, false, false);
        }
        return res;
    }

    /**
     * list deaggregated path entries
     *
     * @param lst table to find
     * @return matching routes
     */
    public static tabRoute<addrIP> deaggregatedPaths(tabRoute<addrIP> lst) {
        tabRoute<addrIP> res = new tabRoute<addrIP>("prep");
        for (int i = 0; i < lst.prefixes.size(); i++) {
            tabRouteEntry<addrIP> ntry = lst.prefixes.get(i);
            if (ntry == null) {
                continue;
            }
            int o = ntry.best.asPathEnd();
            tabRouteEntry<addrIP> oth = findConsecutiveRoute(lst, ntry);
            if (oth != null) {
                if (o == oth.best.asPathEnd()) {
                    res.add(tabRoute.addType.ecmp, ntry, false, false);
                    continue;
                }
            }
            oth = findSupernetRoute(lst, ntry);
            if (oth != null) {
                if (o == oth.best.asPathEnd()) {
                    res.add(tabRoute.addType.ecmp, ntry, false, false);
                    continue;
                }
            }
        }
        return res;
    }

    private static tabRouteEntry<addrIP> findSupernetRoute(tabRoute<addrIP> lst, tabRouteEntry<addrIP> ntry) {
        tabRouteEntry<addrIP> pfx = ntry.copyBytes(tabRoute.addType.better);
        for (int o = ntry.prefix.maskLen - 1; o >= 0; o--) {
            pfx.prefix.setMask(o);
            tabRouteEntry<addrIP> oth = lst.prefixes.find(pfx);
            if (oth == null) {
                continue;
            }
            return oth;
        }
        return null;
    }

    private static tabRouteEntry<addrIP> findConsecutiveRoute(tabRoute<addrIP> lst, tabRouteEntry<addrIP> ntry) {
        final int bit = ntry.prefix.maskLen - 1;
        if (bit < 0) {
            return null;
        }
        tabRouteEntry<addrIP> pfx = ntry.copyBytes(tabRoute.addType.better);
        if (ntry.prefix.network.bitValue(bit)) {
            pfx.prefix.network.bitClear(bit);
        } else {
            pfx.prefix.network.bitSet(bit);
        }
        pfx.prefix.setMask(bit + 1);
        return lst.prefixes.find(pfx);
    }

    private static boolean compressTable1(tabRoute<addrIP> lst, tabRouteEntry<addrIP> ntry) { // consecutives
        tabRouteEntry<addrIP> pfx = findConsecutiveRoute(lst, ntry);
        if (pfx == null) {
            return false;
        }
        if (pfx.sameFwder(ntry.best) == null) {
            return false;
        }
        tabRouteEntry<addrIP> res = ntry.copyBytes(tabRoute.addType.ecmp);
        res.prefix.setMask(ntry.prefix.maskLen - 1);
        tabRouteEntry<addrIP> oth = lst.prefixes.find(res);
        if (oth != null) {
            return false;
        }
        lst.prefixes.del(ntry);
        lst.prefixes.del(pfx);
        lst.prefixes.add(res);
        return true;
    }

    private static boolean compressTable2(tabRoute<addrIP> lst, tabRouteEntry<addrIP> ntry) { // supernet
        tabRouteEntry<addrIP> oth = findSupernetRoute(lst, ntry);
        if (oth == null) {
            return false;
        }
        if (oth.sameFwder(ntry.best) == null) {
            return false;
        }
        lst.prefixes.del(ntry);
        return true;
    }

    private static boolean compressTable3(tabRoute<addrIP> lst, tabRouteEntry<addrIP> ntry) { // subnets
        final int bit = ntry.prefix.maskLen + 1;
        if (bit >= (addrIP.size * 8)) {
            return false;
        }
        tabRouteEntry<addrIP> pfx = ntry.copyBytes(tabRoute.addType.better);
        pfx.prefix.setMask(bit);
        tabRouteEntry<addrIP> oth = lst.prefixes.find(pfx);
        if (oth == null) {
            return false;
        }
        pfx.prefix.network.bitSet(bit - 1);
        pfx.prefix.setMask(bit);
        oth = lst.prefixes.find(pfx);
        if (oth == null) {
            return false;
        }
        lst.prefixes.del(ntry);
        return true;
    }

    /**
     * compress consecutive or subnetted entries
     *
     * @param afi address family
     * @param lst table to update
     * @param pfx prefix list
     * @return number of entries removed
     */
    public static int compressTable(int afi, tabRoute<addrIP> lst, tabListing<tabPrfxlstN, addrIP> pfx) {
        int done = 0;
        for (int i = lst.prefixes.size() - 1; i >= 0; i--) {
            tabRouteEntry<addrIP> ntry = lst.prefixes.get(i);
            if (ntry == null) {
                continue;
            }
            if (pfx != null) {
                if (!pfx.matches(afi, 0, ntry)) {
                    continue;
                }
            }
            if (compressTable1(lst, ntry)) {
                done++;
                continue;
            }
            if (compressTable2(lst, ntry)) {
                done++;
                continue;
            }
            if (compressTable3(lst, ntry)) {
                done++;
                continue;
            }
        }
        return done;
    }

    /**
     * list overlapping prefixes
     *
     * @param lst table to scan
     * @param mtch matcher
     * @return overlapping prefixes
     */
    public static userFormat overlapTable(tabRoute<addrIP> lst, tabIntMatcher mtch) {
        userFormat res = new userFormat("|", "prefix|supernets");
        for (int i = 0; i < lst.prefixes.size(); i++) {
            tabRouteEntry<addrIP> ntry = lst.prefixes.get(i);
            if (ntry == null) {
                continue;
            }
            ntry = ntry.copyBytes(tabRoute.addType.better);
            String a = addrPrefix.ip2str(ntry.prefix) + " " + rd2string(ntry.rouDst) + "|";
            int fnd = 0;
            ntry.prefix.maskLen--;
            for (; ntry.prefix.maskLen >= 0; ntry.prefix.maskLen--) {
                ntry.prefix.setMask(ntry.prefix.maskLen);
                if (lst.find(ntry) == null) {
                    continue;
                }
                fnd++;
                a += " " + addrPrefix.ip2str(ntry.prefix);
            }
            if (!mtch.matches(fnd)) {
                continue;
            }
            res.add(a);
        }
        return res;
    }

}
