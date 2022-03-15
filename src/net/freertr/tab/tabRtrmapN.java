package net.freertr.tab;

import java.util.ArrayList;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrPrefix;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgIfc;
import net.freertr.cfg.cfgRtr;
import net.freertr.cfg.cfgTrack;
import net.freertr.pack.packHolder;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.rtr.rtrBgpUtil;
import net.freertr.user.userScript;
import net.freertr.util.bits;
import net.freertr.util.cmds;

/**
 * represents one route map entry
 *
 * @author matecsaba
 */
public class tabRtrmapN extends tabListingEntry<addrIP> {

    /**
     * create instance
     */
    public tabRtrmapN() {
    }

    /**
     * route distinguisher matcher
     */
    public long rouDstMatch;

    /**
     * afi matcher
     */
    public tabIntMatcher afiMatch = new tabIntMatcher();

    /**
     * safi matcher
     */
    public tabIntMatcher safiMatch = new tabIntMatcher();

    /**
     * network matcher
     */
    public tabPrfxlstN networkMatch;

    /**
     * prefix list matcher
     */
    public tabListing<tabPrfxlstN, addrIP> prfxlstMatch;

    /**
     * peer asn matcher
     */
    public tabIntMatcher peerasnMatch = new tabIntMatcher();

    /**
     * distance matcher
     */
    public tabIntMatcher distanceMatch = new tabIntMatcher();

    /**
     * distance updater
     */
    public tabIntUpdater distanceSet = new tabIntUpdater();

    /**
     * locPref matcher
     */
    public tabIntMatcher locPrefMatch = new tabIntMatcher();

    /**
     * locPref updater
     */
    public tabIntUpdater locPrefSet = new tabIntUpdater();

    /**
     * aigp matcher
     */
    public tabIntMatcher accIgpMatch = new tabIntMatcher();

    /**
     * bandwidth matcher
     */
    public tabIntMatcher bandwidthMatch = new tabIntMatcher();

    /**
     * validity matcher
     */
    public tabIntMatcher validityMatch = new tabIntMatcher();

    /**
     * aspath length matched
     */
    public tabIntMatcher pathlenMatch = new tabIntMatcher();

    /**
     * aspath end matched
     */
    public tabIntMatcher asendMatch = new tabIntMatcher();

    /**
     * aspath begin matched
     */
    public tabIntMatcher asbegMatch = new tabIntMatcher();

    /**
     * aspath mid matched
     */
    public tabIntMatcher asmidMatch = new tabIntMatcher();

    /**
     * aigp updater
     */
    public tabIntUpdater accIgpSet = new tabIntUpdater();

    /**
     * aigp updater
     */
    public tabIntUpdater bandwidthSet = new tabIntUpdater();

    /**
     * origin matcher
     */
    public tabIntMatcher originMatch = new tabIntMatcher();

    /**
     * origin updater
     */
    public tabIntUpdater originSet = new tabIntUpdater();

    /**
     * metric matcher
     */
    public tabIntMatcher metricMatch = new tabIntMatcher();

    /**
     * metric updater
     */
    public tabIntUpdater metricSet = new tabIntUpdater();

    /**
     * tag matcher
     */
    public tabIntMatcher tagMatch = new tabIntMatcher();

    /**
     * tag updater
     */
    public tabIntUpdater tagSet = new tabIntUpdater();

    /**
     * segrou matcher
     */
    public tabIntMatcher segrouMatch = new tabIntMatcher();

    /**
     * segrou updater
     */
    public tabIntUpdater segrouSet = new tabIntUpdater();

    /**
     * bier matcher
     */
    public tabIntMatcher bierMatch = new tabIntMatcher();

    /**
     * bier updater
     */
    public tabIntUpdater bierSet = new tabIntUpdater();

    /**
     * as path matcher
     */
    public String aspathMatch = null;

    /**
     * as path updater
     */
    public List<Integer> aspathSet;

    /**
     * as path updater
     */
    public List<Integer> aspathCnf;

    /**
     * next hop matcher
     */
    public addrIP nexthopMatch;

    /**
     * next hop updater
     */
    public addrIP nexthopSet;

    /**
     * old hop matcher
     */
    public addrIP oldhopMatch;

    /**
     * protocol type matcher
     */
    public tabRouteAttr.routeType protoTypMatch;

    /**
     * protocol number matcher
     */
    public int protoNumMatch;

    /**
     * interface matcher
     */
    public cfgIfc ifaceMatch;

    /**
     * tracker matcher
     */
    public String trackMatch;

    /**
     * private as matcher
     */
    public boolean privasMatch;

    /**
     * private as updates
     */
    public boolean privasClear;

    /**
     * peer as updates
     */
    public boolean peerasClear;

    /**
     * exact as updates
     */
    public int exactasClear;

    /**
     * first as updates
     */
    public boolean firstasClear;

    /**
     * community matcher
     */
    public int peerStdMatch;

    /**
     * community matcher
     */
    public tabLargeComm peerLrgMatch;

    /**
     * community matcher
     */
    public List<Integer> stdCommMatch;

    /**
     * community updater
     */
    public boolean stdCommClear;

    /**
     * community updater
     */
    public List<Integer> stdCommSet;

    /**
     * community matcher
     */
    public List<Long> extCommMatch;

    /**
     * community updater
     */
    public boolean extCommClear;

    /**
     * community updater
     */
    public List<Long> extCommSet;

    /**
     * community matcher
     */
    public List<tabLargeComm> lrgCommMatch;

    /**
     * community updater
     */
    public boolean lrgCommClear;

    /**
     * community updater
     */
    public List<tabLargeComm> lrgCommSet;

    /**
     * cluster list updater
     */
    public boolean clstLstClear;

    /**
     * community matcher
     */
    public boolean noStdComm;

    /**
     * community matcher
     */
    public boolean noExtComm;

    /**
     * community matcher
     */
    public boolean noLrgComm;

    /**
     * script to execute
     */
    public List<String> script;

    /**
     * list of routemaps
     */
    public tabListing<tabRtrmapN, addrIP> roumapMatch;

    /**
     * list of routemaps
     */
    public tabListing<tabRtrmapN, addrIP> roumapSet;

    /**
     * list of routemaps
     */
    public tabListing<tabRtrplcN, addrIP> rouplcMatch;

    /**
     * list of routemaps
     */
    public tabListing<tabRtrplcN, addrIP> rouplcSet;

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
        return (bits.str2num(s.substring(0, i)) << 16)
                | bits.str2num(s.substring(i + 1, s.length()));
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
        return (i >>> 16) + ":" + (i & 0xffff);
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
        return (asn << 32) | (num & 0xffffffffL);
    }

    /**
     * convert rd to string
     *
     * @param i rd to convert
     * @return converted string
     */
    public static String rd2string(long i) {
        return (i >>> 32) + ":" + (int) (i & 0xffffffffL);
    }

    /**
     * convert community to string
     *
     * @param i community to convert
     * @return converted string
     */
    public static String extComm2string(long i) {
        return (i >>> 48) + ":" + ((i >>> 32) & 0xffff) + ":" + (int) (i & 0xffffffffL);
    }

    /**
     * convert route target to extended community
     *
     * @param i route target
     * @return extended community
     */
    public static long rt2comm(long i) {
        long as = (i >>> 32) & 0xffff;
        long id = i & 0xffffffffL;
        return ((as | 0x20000) << 32) | id;
    }

    /**
     * convert tunnel type to extended community
     *
     * @param i tunnel type
     * @return extended community
     */
    public static long tuntyp2comm(long i) {
        return 0x030c000000000000L | (i & 0xffffffffffffL);
    }

    /**
     * generate flowspec rate
     *
     * @param as as number
     * @param bw flow rate
     * @return extended community
     */
    public static long rate2comm(long as, long bw) {
        return (((as & 0xffff) | 0x80060000) << 32) | Float.floatToIntBits(bw);
    }

    /**
     * decode flowspec rate
     *
     * @param comm extended community
     * @return -1 on error, rate if success
     */
    public static long comm2rate(long comm) {
        if (((comm >>> 48) & 0xffff) != 0x8006) {
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
     * generate dmz link bandwidth
     *
     * @param as as number
     * @param bw bandwodth
     * @return extended community
     */
    public static long dmzbw2comm(long as, long bw) {
        return (((as & 0xffff) | 0x40040000) << 32) | bw;
    }

    /**
     * convert agi to extended community
     *
     * @param i agi
     * @return extended community
     */
    public static long agi2comm(long i) {
        long as = (i >>> 32) & 0xffff;
        long id = i & 0xffffffffL;
        return ((as | 0xa0000) << 32) | id;
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
        long i = 0x800a0000 | ((enc & 0xff) << 8) | (flg & 0xff);
        return (i << 32) | ((mtu & 0xffff) << 16);
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
        return o | ((long) bits.str2num(s.substring(0, i)) << 32)
                | bits.str2num(s.substring(i + 1, s.length()));
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
     * stdcomm with asn
     *
     * @param comm community
     * @param asn asn
     * @return merged
     */
    public static int stdcommAsn(int comm, int asn) {
        return (comm & 0xffff0000) | (asn & 0xffff);
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

    public String toString() {
        return description;
    }

    /**
     * get the config
     *
     * @param beg beginning
     * @return the config
     */
    public List<String> usrString(String beg) {
        beg += "sequence " + sequence + " ";
        List<String> l = new ArrayList<String>();
        l.add(beg + "description " + description);
        l.add(beg + "action " + action2string(action));
        if (script == null) {
            l.add(beg + "tcldel");
        } else {
            for (int i = 0; i < script.size(); i++) {
                l.add(beg + "tcladd " + script.get(i));
            }
        }
        cmds.cfgLine(l, prfxlstMatch == null, beg, "match prefix-list", "" + prfxlstMatch);
        cmds.cfgLine(l, roumapMatch == null, beg, "match route-map", "" + roumapMatch);
        cmds.cfgLine(l, rouplcMatch == null, beg, "match route-policy", "" + rouplcMatch);
        l.add(beg + "match afi " + afiMatch);
        l.add(beg + "match safi " + safiMatch);
        if (rouDstMatch == 0) {
            l.add(beg + "no match rd");
        } else {
            l.add(beg + "match rd " + rd2string(rouDstMatch));
        }
        if (networkMatch == null) {
            l.add(beg + "no match network");
        } else {
            l.add(beg + "match network " + networkMatch);
        }
        if (ifaceMatch == null) {
            l.add(beg + "no match interface");
        } else {
            l.add(beg + "match interface " + ifaceMatch.name);
        }
        if (nexthopMatch == null) {
            l.add(beg + "no match nexthop");
        } else {
            l.add(beg + "match nexthop " + nexthopMatch);
        }
        if (oldhopMatch == null) {
            l.add(beg + "no match recursive");
        } else {
            l.add(beg + "match recursive " + oldhopMatch);
        }
        if (protoTypMatch == null) {
            l.add(beg + "no match protocol");
        } else {
            String a = "" + protoTypMatch;
            if (cfgRtr.num2proc(protoTypMatch)) {
                a += " " + protoNumMatch;
            }
            l.add(beg + "match protocol " + a);
        }
        if (aspathMatch == null) {
            l.add(beg + "no match aspath");
        } else {
            l.add(beg + "match aspath " + aspathMatch);
        }
        cmds.cfgLine(l, peerStdMatch == 0, beg, "match peerstd", stdComm2string(peerStdMatch));
        cmds.cfgLine(l, peerLrgMatch == null, beg, "match peerlrg", "" + peerLrgMatch);
        if (stdCommMatch == null) {
            l.add(beg + "no match stdcomm");
        } else {
            l.add(beg + "match stdcomm " + stdComms2string(stdCommMatch));
        }
        if (extCommMatch == null) {
            l.add(beg + "no match extcomm");
        } else {
            l.add(beg + "match extcomm " + extComms2string(extCommMatch));
        }
        if (lrgCommMatch == null) {
            l.add(beg + "no match lrgcomm");
        } else {
            l.add(beg + "match lrgcomm " + lrgComms2string(lrgCommMatch));
        }
        l.add(beg + "match peerasn " + peerasnMatch);
        l.add(beg + "match distance " + distanceMatch);
        l.add(beg + "match locpref " + locPrefMatch);
        l.add(beg + "match validity " + validityMatch);
        l.add(beg + "match pathlen " + pathlenMatch);
        l.add(beg + "match asend " + asendMatch);
        l.add(beg + "match asbeg " + asbegMatch);
        l.add(beg + "match asmid " + asmidMatch);
        l.add(beg + "match aigp " + accIgpMatch);
        l.add(beg + "match bandwidth " + bandwidthMatch);
        l.add(beg + "match origin " + originMatch);
        l.add(beg + "match metric " + metricMatch);
        l.add(beg + "match tag " + tagMatch);
        l.add(beg + "match segrout " + segrouMatch);
        l.add(beg + "match bier " + bierMatch);
        cmds.cfgLine(l, !noStdComm, beg, "match nostdcomm", "");
        cmds.cfgLine(l, !noExtComm, beg, "match noextcomm", "");
        cmds.cfgLine(l, !noLrgComm, beg, "match nolrgcomm", "");
        cmds.cfgLine(l, trackMatch == null, beg, "match tracker", "" + trackMatch);
        cmds.cfgLine(l, !privasMatch, beg, "match privateas", "");
        cmds.cfgLine(l, !logMatch, beg, "log", "");
        cmds.cfgLine(l, !stdCommClear, beg, "clear stdcomm", "");
        cmds.cfgLine(l, !extCommClear, beg, "clear extcomm", "");
        cmds.cfgLine(l, !lrgCommClear, beg, "clear lrgcomm", "");
        cmds.cfgLine(l, !clstLstClear, beg, "clear clustlist", "");
        cmds.cfgLine(l, !privasClear, beg, "clear privateas", "");
        cmds.cfgLine(l, !peerasClear, beg, "clear peeras", "");
        cmds.cfgLine(l, exactasClear == 0, beg, "clear exactas", "" + bits.num2str(exactasClear));
        cmds.cfgLine(l, !firstasClear, beg, "clear firstas", "");
        cmds.cfgLine(l, roumapSet == null, beg, "set route-map", "" + roumapSet);
        cmds.cfgLine(l, rouplcSet == null, beg, "set route-policy", "" + rouplcSet);
        if (aspathSet == null) {
            l.add(beg + "no set aspath");
        } else {
            l.add(beg + "set aspath " + tabRouteAttr.dumpIntList(aspathSet, "", ""));
        }
        if (aspathCnf == null) {
            l.add(beg + "no set asconfed");
        } else {
            l.add(beg + "set asconfed " + tabRouteAttr.dumpIntList(aspathCnf, "", ""));
        }
        if (stdCommSet == null) {
            l.add(beg + "no set stdcomm");
        } else {
            l.add(beg + "set stdcomm " + stdComms2string(stdCommSet));
        }
        if (extCommSet == null) {
            l.add(beg + "no set extcomm");
        } else {
            l.add(beg + "set extcomm " + extComms2string(extCommSet));
        }
        if (lrgCommSet == null) {
            l.add(beg + "no set lrgcomm");
        } else {
            l.add(beg + "set lrgcomm " + lrgComms2string(lrgCommSet));
        }
        if (nexthopSet == null) {
            l.add(beg + "no set nexthop");
        } else {
            l.add(beg + "set nexthop " + nexthopSet);
        }
        l.add(beg + "set distance " + distanceSet);
        l.add(beg + "set locpref " + locPrefSet);
        l.add(beg + "set aigp " + accIgpSet);
        l.add(beg + "set bandwidth " + bandwidthSet);
        l.add(beg + "set origin " + originSet);
        l.add(beg + "set metric " + metricSet);
        l.add(beg + "set tag " + tagSet);
        l.add(beg + "set segrout " + segrouSet);
        l.add(beg + "set bier " + bierSet);
        return l;
    }

    public boolean matches(int afi, int asn, addrPrefix<addrIP> net) {
        tabRouteEntry<addrIP> nt = new tabRouteEntry<addrIP>();
        nt.prefix = net;
        return matches(afi, asn, nt);
    }

    public boolean matches(int afi, int asn, tabRouteEntry<addrIP> net) {
        if (rouDstMatch != 0) {
            if (rouDstMatch != net.rouDst) {
                return false;
            }
        }
        if (!afiMatch.matches(afi & rtrBgpUtil.afiMask)) {
            return false;
        }
        if (!safiMatch.matches(afi & rtrBgpUtil.sfiMask)) {
            return false;
        }
        if (!peerasnMatch.matches(asn)) {
            return false;
        }
        if (!distanceMatch.matches(net.best.distance)) {
            return false;
        }
        if (!locPrefMatch.matches(net.best.locPref)) {
            return false;
        }
        if (!accIgpMatch.matches(net.best.accIgp)) {
            return false;
        }
        if (!bandwidthMatch.matches(net.best.bandwidth)) {
            return false;
        }
        if (!originMatch.matches(net.best.origin)) {
            return false;
        }
        if (!metricMatch.matches(net.best.metric)) {
            return false;
        }
        if (!tagMatch.matches(net.best.tag)) {
            return false;
        }
        if (!segrouMatch.matches(net.best.segrouIdx)) {
            return false;
        }
        if (!bierMatch.matches(net.best.bierIdx)) {
            return false;
        }
        if (!validityMatch.matches(net.best.validity)) {
            return false;
        }
        if (!pathlenMatch.matches(net.best.asPathLen())) {
            return false;
        }
        if (!asendMatch.matches(net.best.asPathEnd())) {
            return false;
        }
        if (!asbegMatch.matches(net.best.asPathBeg())) {
            return false;
        }
        if (!net.best.asPathMid(asmidMatch)) {
            return false;
        }
        if (noStdComm) {
            if (net.best.stdComm != null) {
                if (net.best.stdComm.size() > 0) {
                    return false;
                }
            }
        }
        if (noExtComm) {
            if (net.best.extComm != null) {
                if (net.best.extComm.size() > 0) {
                    return false;
                }
            }
        }
        if (noLrgComm) {
            if (net.best.lrgComm != null) {
                if (net.best.lrgComm.size() > 0) {
                    return false;
                }
            }
        }
        if (trackMatch != null) {
            cfgTrack res = cfgAll.trackFind(trackMatch, false);
            if (res == null) {
                return false;
            }
            if (!res.worker.getStatus()) {
                return false;
            }
        }
        if (privasMatch) {
            int i = rtrBgpUtil.removePrivateAs(tabLabel.copyLabels(net.best.pathSeq));
            i += rtrBgpUtil.removePrivateAs(tabLabel.copyLabels(net.best.pathSet));
            if (i < 1) {
                return false;
            }
        }
        if (ifaceMatch != null) {
            if (net.best.iface == null) {
                return false;
            }
            if ((net.best.iface != ifaceMatch.fwdIf4) && (net.best.iface != ifaceMatch.fwdIf6)) {
                return false;
            }
        }
        if (nexthopMatch != null) {
            if (net.best.nextHop == null) {
                return false;
            }
            if (nexthopMatch.compare(nexthopMatch, net.best.nextHop) != 0) {
                return false;
            }
        }
        if (oldhopMatch != null) {
            if (net.best.oldHop == null) {
                return false;
            }
            if (oldhopMatch.compare(oldhopMatch, net.best.oldHop) != 0) {
                return false;
            }
        }
        if (protoTypMatch != null) {
            if (net.best.rouTyp != protoTypMatch) {
                return false;
            }
            if (net.best.protoNum != protoNumMatch) {
                return false;
            }
        }
        if (networkMatch != null) {
            if (!networkMatch.matches(afi, asn, net.prefix)) {
                return false;
            }
        }
        if (aspathMatch != null) {
            if (!net.best.asPathStr().matches(aspathMatch)) {
                return false;
            }
        }
        if (peerStdMatch != 0) {
            int i = stdcommAsn(peerStdMatch, asn);
            if (rtrBgpUtil.findIntList(net.best.stdComm, i) < 0) {
                return false;
            }
        }
        if (peerLrgMatch != null) {
            tabLargeComm lrg = peerLrgMatch.copyBytes();
            lrg.d2 = asn;
            if (rtrBgpUtil.findLrgList(net.best.lrgComm, lrg) < 0) {
                return false;
            }
        }
        if (stdCommMatch != null) {
            for (int i = 0; i < stdCommMatch.size(); i++) {
                if (rtrBgpUtil.findIntList(net.best.stdComm, stdCommMatch.get(i)) < 0) {
                    return false;
                }
            }
        }
        if (extCommMatch != null) {
            for (int i = 0; i < extCommMatch.size(); i++) {
                if (rtrBgpUtil.findLongList(net.best.extComm, extCommMatch.get(i)) < 0) {
                    return false;
                }
            }
        }
        if (lrgCommMatch != null) {
            for (int i = 0; i < lrgCommMatch.size(); i++) {
                if (rtrBgpUtil.findLrgList(net.best.lrgComm, lrgCommMatch.get(i)) < 0) {
                    return false;
                }
            }
        }
        if (prfxlstMatch != null) {
            if (!prfxlstMatch.matches(afi, asn, net.prefix)) {
                return false;
            }
        }
        if (roumapMatch != null) {
            if (!roumapMatch.matches(afi, asn, net)) {
                return false;
            }
        }
        if (rouplcMatch != null) {
            if (tabRtrplc.doRpl(afi, asn, net, rouplcMatch, true) == null) {
                return false;
            }
        }
        return true;
    }

    public boolean matches(packHolder pck) {
        return matches(rtrBgpUtil.sfiUnicast, 0, new addrPrefix<addrIP>(pck.IPsrc, new addrIP().maxBits()));
    }

    private void doUpdate(tabRouteAttr<addrIP> attr, int asn) {
        attr.distance = distanceSet.update(attr.distance);
        attr.locPref = locPrefSet.update(attr.locPref);
        attr.accIgp = accIgpSet.update(attr.accIgp);
        attr.bandwidth = bandwidthSet.update(attr.bandwidth);
        attr.origin = originSet.update(attr.origin);
        attr.metric = metricSet.update(attr.metric);
        attr.tag = tagSet.update(attr.tag);
        attr.segrouIdx = segrouSet.update(attr.segrouIdx);
        attr.bierIdx = bierSet.update(attr.bierIdx);
        attr.pathSeq = tabLabel.prependLabels(attr.pathSeq, aspathSet);
        attr.confSeq = tabLabel.prependLabels(attr.confSeq, aspathCnf);
        if (stdCommClear) {
            attr.stdComm = null;
        }
        if (extCommClear) {
            attr.extComm = null;
        }
        if (lrgCommClear) {
            attr.lrgComm = null;
        }
        if (clstLstClear) {
            attr.clustList = null;
        }
        if (privasClear) {
            rtrBgpUtil.removePrivateAs(attr.pathSeq);
            rtrBgpUtil.removePrivateAs(attr.pathSet);
        }
        if (peerasClear) {
            rtrBgpUtil.removeIntList(attr.pathSeq, asn);
            rtrBgpUtil.removeIntList(attr.pathSet, asn);
        }
        if (exactasClear != 0) {
            rtrBgpUtil.removeIntList(attr.pathSeq, exactasClear);
            rtrBgpUtil.removeIntList(attr.pathSet, exactasClear);
        }
        if (firstasClear) {
            rtrBgpUtil.removeFirstAs(attr);
        }
        attr.stdComm = tabLabel.prependLabels(attr.stdComm, stdCommSet);
        if (nexthopSet != null) {
            attr.nextHop = nexthopSet.copyBytes();
        }
        if (extCommSet != null) {
            if (attr.extComm == null) {
                attr.extComm = new ArrayList<Long>();
            }
            attr.extComm.addAll(extCommSet);
        }
        if (lrgCommSet != null) {
            if (attr.lrgComm == null) {
                attr.lrgComm = new ArrayList<tabLargeComm>();
            }
            attr.lrgComm.addAll(lrgCommSet);
        }
    }

    public void update(int afi, int asn, tabRouteEntry<addrIP> net) {
        for (int i = 0; i < net.alts.size(); i++) {
            doUpdate(net.alts.get(i), asn);
        }
        net.selectBest();
        if (roumapSet != null) {
            roumapSet.update(afi, asn, net, false);
        }
        if (rouplcSet != null) {
            tabRtrplc.doRpl(afi, asn, net, rouplcSet, false);
        }
        if (script != null) {
            doTcl(afi, asn, net.best, net, script);
        }
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
    protected static void doTcl(int afi, int asn, tabRouteAttr<addrIP> attr, tabRouteEntry<addrIP> net, List<String> scr) {
        pipeLine pl = new pipeLine(32768, false);
        pipeSide pip = pl.getSide();
        pip.setTime(10000);
        pip.lineRx = pipeSide.modTyp.modeCRorLF;
        pip.lineTx = pipeSide.modTyp.modeCRLF;
        userScript t = new userScript(pip, "");
        t.allowExec = true;
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

}
