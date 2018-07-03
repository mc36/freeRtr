package tab;

import addr.addrIP;
import addr.addrPrefix;
import java.util.ArrayList;
import java.util.List;
import pack.packHolder;
import pipe.pipeLine;
import pipe.pipeSide;
import rtr.rtrBgpUtil;
import user.userScript;
import util.bits;
import util.cmds;

/**
 * represents one route map entry
 *
 * @author matecsaba
 */
public class tabRtrmapN extends tabListingEntry<addrIP> {

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
     * next hop matcher
     */
    public addrIP nexthopMatch;

    /**
     * next hop updater
     */
    public addrIP nexthopSet;

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
        return (asn << 32) | (num & 0xffffffffl);
    }

    /**
     * convert rd to string
     *
     * @param i rd to convert
     * @return converted string
     */
    public static String rd2string(long i) {
        return (i >>> 32) + ":" + (int) (i & 0xffffffffl);
    }

    /**
     * convert community to string
     *
     * @param i community to convert
     * @return converted string
     */
    public static String extComm2string(long i) {
        return (i >>> 48) + ":" + ((i >>> 32) & 0xffff) + ":" + (int) (i & 0xffffffffl);
    }

    /**
     * convert route target to extended community
     *
     * @param i route target
     * @return extended community
     */
    public static long rt2comm(long i) {
        long as = (i >>> 32) & 0xffff;
        long id = i & 0xffffffffl;
        return ((as | 0x20000) << 32) | id;
    }

    /**
     * convert tunnel type to extended community
     *
     * @param i tunnel type
     * @return extended community
     */
    public static long tuntyp2comm(long i) {
        return 0x030c000000000000l | (i & 0xffffffffffffl);
    }

    /**
     * generate flowspec rate
     *
     * @param as as number
     * @param bw flow rate
     * @return extended community
     */
    public static long rate2comm(long as, int bw) {
        return (((as & 0xffff) | 0x80060000) << 32) | Float.floatToIntBits(bw);
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
        long id = i & 0xffffffffl;
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
            d.fromString(a);
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
        if (nexthopMatch == null) {
            l.add(beg + "no match nexthop");
        } else {
            l.add(beg + "match nexthop " + nexthopMatch);
        }
        if (aspathMatch == null) {
            l.add(beg + "no match aspath");
        } else {
            l.add(beg + "match aspath " + aspathMatch);
        }
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
        l.add(beg + "match distance " + distanceMatch);
        l.add(beg + "match locpref " + locPrefMatch);
        l.add(beg + "match validity " + validityMatch);
        l.add(beg + "match pathlen " + pathlenMatch);
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
        cmds.cfgLine(l, !logMatch, beg, "log", "");
        cmds.cfgLine(l, !stdCommClear, beg, "clear stdcomm", "");
        cmds.cfgLine(l, !extCommClear, beg, "clear extcomm", "");
        cmds.cfgLine(l, !extCommClear, beg, "clear lrgcomm", "");
        cmds.cfgLine(l, roumapSet == null, beg, "set route-map", "" + roumapSet);
        cmds.cfgLine(l, rouplcSet == null, beg, "set route-policy", "" + rouplcSet);
        if (aspathSet == null) {
            l.add(beg + "no set aspath");
        } else {
            l.add(beg + "set aspath " + tabRouteEntry.dumpIntList(aspathSet, "", ""));
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

    public boolean matches(int afi, addrPrefix<addrIP> net) {
        tabRouteEntry<addrIP> nt = new tabRouteEntry<addrIP>();
        nt.prefix = net;
        return matches(afi, nt);
    }

    public boolean matches(int afi, tabRouteEntry<addrIP> net) {
        if (rouDstMatch != 0) {
            if (rouDstMatch != net.rouDst) {
                return false;
            }
        }
        if (!afiMatch.matches(afi & rtrBgpUtil.afiMask)) {
            return false;
        }
        if (!safiMatch.matches(afi & rtrBgpUtil.safiMask)) {
            return false;
        }
        if (!distanceMatch.matches(net.distance)) {
            return false;
        }
        if (!locPrefMatch.matches(net.locPref)) {
            return false;
        }
        if (!accIgpMatch.matches(net.accIgp)) {
            return false;
        }
        if (!bandwidthMatch.matches(net.bandwidth)) {
            return false;
        }
        if (!originMatch.matches(net.origin)) {
            return false;
        }
        if (!metricMatch.matches(net.metric)) {
            return false;
        }
        if (!tagMatch.matches(net.tag)) {
            return false;
        }
        if (!segrouMatch.matches(net.segrouIdx)) {
            return false;
        }
        if (!bierMatch.matches(net.bierIdx)) {
            return false;
        }
        if (!validityMatch.matches(net.validity)) {
            return false;
        }
        if (!pathlenMatch.matches(net.asPathLen())) {
            return false;
        }
        if (noStdComm) {
            if (net.stdComm != null) {
                if (net.stdComm.size() > 0) {
                    return false;
                }
            }
        }
        if (noExtComm) {
            if (net.extComm != null) {
                if (net.extComm.size() > 0) {
                    return false;
                }
            }
        }
        if (noLrgComm) {
            if (net.lrgComm != null) {
                if (net.lrgComm.size() > 0) {
                    return false;
                }
            }
        }
        if (nexthopMatch != null) {
            if (net.nextHop == null) {
                return false;
            }
            if (nexthopMatch.compare(nexthopMatch, net.nextHop) != 0) {
                return false;
            }
        }
        if (networkMatch != null) {
            if (!networkMatch.matches(afi, net.prefix)) {
                return false;
            }
        }
        if (aspathMatch != null) {
            if (!net.asPathStr().matches(aspathMatch)) {
                return false;
            }
        }
        if (stdCommMatch != null) {
            for (int i = 0; i < stdCommMatch.size(); i++) {
                if (rtrBgpUtil.findIntList(net.stdComm, stdCommMatch.get(i)) < 0) {
                    return false;
                }
            }
        }
        if (extCommMatch != null) {
            for (int i = 0; i < extCommMatch.size(); i++) {
                if (rtrBgpUtil.findLongList(net.extComm, extCommMatch.get(i)) < 0) {
                    return false;
                }
            }
        }
        if (lrgCommMatch != null) {
            for (int i = 0; i < lrgCommMatch.size(); i++) {
                if (rtrBgpUtil.findLrgList(net.lrgComm, lrgCommMatch.get(i)) < 0) {
                    return false;
                }
            }
        }
        if (prfxlstMatch != null) {
            if (!prfxlstMatch.matches(afi, net.prefix)) {
                return false;
            }
        }
        if (roumapMatch != null) {
            tabRtrmapN ntry = roumapMatch.find(afi, net);
            if (ntry == null) {
                return false;
            }
            if (ntry.action == tabPlcmapN.actionType.actDeny) {
                return false;
            }
        }
        if (rouplcMatch != null) {
            if (tabRtrplc.doRpl(afi, net, rouplcMatch, true) == null) {
                return false;
            }
        }
        return true;
    }

    public boolean matches(packHolder pck) {
        return matches(rtrBgpUtil.safiUnicast, new addrPrefix<addrIP>(pck.IPsrc, new addrIP().maxBits()));
    }

    public void update(int afi, tabRouteEntry<addrIP> net) {
        net.distance = distanceSet.update(net.distance);
        net.locPref = locPrefSet.update(net.locPref);
        net.accIgp = accIgpSet.update(net.accIgp);
        net.bandwidth = bandwidthSet.update(net.bandwidth);
        net.origin = originSet.update(net.origin);
        net.metric = metricSet.update(net.metric);
        net.tag = tagSet.update(net.tag);
        net.segrouIdx = segrouSet.update(net.segrouIdx);
        net.bierIdx = bierSet.update(net.bierIdx);
        net.pathSeq = tabLabel.prependLabels(net.pathSeq, aspathSet);
        if (stdCommClear) {
            net.stdComm = null;
        }
        if (extCommClear) {
            net.extComm = null;
        }
        if (lrgCommClear) {
            net.lrgComm = null;
        }
        net.stdComm = tabLabel.prependLabels(net.stdComm, stdCommSet);
        if (nexthopSet != null) {
            net.nextHop = nexthopSet.copyBytes();
        }
        if (extCommSet != null) {
            if (net.extComm == null) {
                net.extComm = new ArrayList<Long>();
            }
            net.extComm.addAll(extCommSet);
        }
        if (lrgCommSet != null) {
            if (net.lrgComm == null) {
                net.lrgComm = new ArrayList<tabLargeComm>();
            }
            net.lrgComm.addAll(lrgCommSet);
        }
        if (roumapSet != null) {
            roumapSet.update(afi, net, false);
        }
        if (rouplcSet != null) {
            tabRtrplc.doRpl(afi, net, rouplcSet, false);
        }
        if (script != null) {
            doTcl(afi, net, script);
        }
    }

    /**
     * execute script
     *
     * @param afi address family
     * @param net prefix to update
     * @param scr updater script
     */
    protected static void doTcl(int afi, tabRouteEntry<addrIP> net, List<String> scr) {
        pipeLine pl = new pipeLine(32768, false);
        pipeSide pip = pl.getSide();
        pip.timeout = 10000;
        pip.lineRx = pipeSide.modTyp.modeCRorLF;
        pip.lineTx = pipeSide.modTyp.modeCRLF;
        userScript t = new userScript(pip, "");
        t.allowExec = true;
        t.addLine("set afi " + (afi & rtrBgpUtil.afiMask));
        t.addLine("set safi " + (afi & rtrBgpUtil.safiMask));
        t.addLine("set prefix " + addrPrefix.ip2str(net.prefix));
        t.addLine("set network " + net.prefix.network);
        t.addLine("set masklen " + net.prefix.maskLen);
        t.addLine("set netmask " + net.prefix.mask);
        t.addLine("set wildcard " + net.prefix.wildcard);
        t.addLine("set broadcast " + net.prefix.broadcast);
        t.addLine("set rd " + rd2string(net.rouDst));
        t.addLine("set nexthop " + net.nextHop);
        t.addLine("set distance " + net.distance);
        t.addLine("set validity " + net.validity);
        t.addLine("set locpref " + net.locPref);
        t.addLine("set accigp " + net.accIgp);
        t.addLine("set bandwidth " + net.bandwidth);
        t.addLine("set origin " + net.origin);
        t.addLine("set metric " + net.metric);
        t.addLine("set tag " + net.tag);
        t.addLine("set segrout " + net.segrouIdx);
        t.addLine("set bier " + net.bierIdx);
        t.addLine("set aspath \"" + net.asPathStr() + "\"");
        t.addLine("set pathlen \"" + net.asPathLen() + "\"");
        t.addLine("set stdcomm \"" + stdComms2string(net.stdComm) + "\"");
        t.addLine("set extcomm \"" + extComms2string(net.extComm) + "\"");
        t.addLine("set lrgcomm \"" + lrgComms2string(net.lrgComm) + "\"");
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
                net.nextHop.fromString(cmd.word());
                continue;
            }
            if (a.equals("distance")) {
                net.distance = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("locpref")) {
                net.locPref = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("accigp")) {
                net.accIgp = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("bandwidth")) {
                net.bandwidth = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("validity")) {
                net.validity = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("origin")) {
                net.origin = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("metric")) {
                net.metric = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("tag")) {
                net.tag = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("segrout")) {
                net.segrouIdx = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("bier")) {
                net.bierIdx = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("aspath")) {
                List<Integer> lst = string2intList(cmd.getRemaining());
                net.pathSeq = tabLabel.prependLabels(net.pathSeq, lst);
                continue;
            }
            if (a.equals("stdcomm")) {
                net.stdComm = string2stdComms(cmd.getRemaining());
                continue;
            }
            if (a.equals("extcomm")) {
                net.extComm = string2extComms(cmd.getRemaining());
                continue;
            }
            if (a.equals("lrgcomm")) {
                net.lrgComm = string2lrgComms(cmd.getRemaining());
                continue;
            }
        }
    }

}
