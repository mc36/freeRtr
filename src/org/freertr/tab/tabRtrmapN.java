package org.freertr.tab;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrPrefix;
import org.freertr.cfg.cfgAceslst;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgPrfxlst;
import org.freertr.cfg.cfgRoump;
import org.freertr.cfg.cfgRouplc;
import org.freertr.cfg.cfgRtr;
import org.freertr.cfg.cfgTrack;
import org.freertr.cfg.cfgVrf;
import org.freertr.ip.ipFwd;
import org.freertr.pack.packHolder;
import org.freertr.rtr.rtrBgpUtil;
import org.freertr.util.bits;
import org.freertr.util.cmds;

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
    public long rouDstMatch = -1L;

    /**
     * route distinguisher matcher
     */
    public long rouDstSet = -1L;

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
     * access list matcher
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> aceslstMatch;

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
    public tabIntMatcher validRoaMatch = new tabIntMatcher();

    /**
     * validity matcher
     */
    public tabIntMatcher validAspaMatch = new tabIntMatcher();

    /**
     * aggregator matcher
     */
    public tabIntMatcher aggregatorMatch = new tabIntMatcher();

    /**
     * customer matcher
     */
    public tabIntMatcher customerMatch = new tabIntMatcher();

    /**
     * aspath length matched
     */
    public tabIntMatcher pathlenMatch = new tabIntMatcher();

    /**
     * unknown attribute matched
     */
    public tabIntMatcher unknownMatch = new tabIntMatcher();

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
     * validity updater
     */
    public tabIntUpdater validRoaSet = new tabIntUpdater();

    /**
     * validity updater
     */
    public tabIntUpdater validAspaSet = new tabIntUpdater();

    /**
     * aggregator updater
     */
    public tabIntUpdater aggregatorSet = new tabIntUpdater();

    /**
     * aggregator updater
     */
    public addrIP aggregatorRtr = null;

    /**
     * connector updater
     */
    public addrIP connectorSet = null;

    /**
     * path limit updater
     */
    public tabIntUpdater pathLimSet = new tabIntUpdater();

    /**
     * path limit updater
     */
    public tabIntUpdater pathAsnSet = new tabIntUpdater();

    /**
     * customer updater
     */
    public tabIntUpdater customerSet = new tabIntUpdater();

    /**
     * bandwidth updater
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
     * local label matcher
     */
    public tabIntMatcher lablocMatch = new tabIntMatcher();

    /**
     * remote label matcher
     */
    public tabIntMatcher labremMatch = new tabIntMatcher();

    /**
     * segrou matcher
     */
    public tabIntMatcher segrouMatch = new tabIntMatcher();

    /**
     * local label updater
     */
    public tabIntUpdater lablocSet = new tabIntUpdater();

    /**
     * remote label updater
     */
    public tabIntUpdater labremSet = new tabIntUpdater();

    /**
     * segrou updater
     */
    public tabIntUpdater segrouSet = new tabIntUpdater();

    /**
     * bier matcher
     */
    public tabIntMatcher bierMatch = new tabIntMatcher();

    /**
     * bier index updater
     */
    public tabIntUpdater bierIdxSet = new tabIntUpdater();

    /**
     * bier subdomain updater
     */
    public tabIntUpdater bierSubSet = new tabIntUpdater();

    /**
     * srv6 matcher
     */
    public addrIP srv6match = null;

    /**
     * srv6 updater
     */
    public addrIP srv6set = null;

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
     * vrf forwarder updater
     */
    public ipFwd vrfSetF;

    /**
     * vrf afi type updater
     */
    public boolean vrfSetT;

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
     * entropy label matcher
     */
    public boolean entropyMatch;

    /**
     * private as updates
     */
    public boolean privasClear;

    /**
     * entropys updates
     */
    public boolean entropyClear;

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
    public String stdCommClear;

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
    public String extCommClear;

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
    public String lrgCommClear;

    /**
     * community updater
     */
    public List<tabLargeComm> lrgCommSet;

    /**
     * originator updater
     */
    public String orgntrClear;

    /**
     * cluster list updater
     */
    public String clstLstClear;

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

    public String toString() {
        return description;
    }

    /**
     * convert to string
     *
     * @param beg beginning
     * @param filter filter mode
     * @return string
     */
    public List<String> usrString(String beg, int filter) {
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
        cmds.cfgLine(l, aceslstMatch == null, beg, "match access-list", "" + aceslstMatch);
        cmds.cfgLine(l, prfxlstMatch == null, beg, "match prefix-list", "" + prfxlstMatch);
        cmds.cfgLine(l, roumapMatch == null, beg, "match route-map", "" + roumapMatch);
        cmds.cfgLine(l, rouplcMatch == null, beg, "match route-policy", "" + rouplcMatch);
        l.add(beg + "match afi " + afiMatch);
        l.add(beg + "match safi " + safiMatch);
        if (rouDstMatch == -1L) {
            l.add(beg + "no match rd");
        } else {
            l.add(beg + "match rd " + tabRouteUtil.rd2string(rouDstMatch));
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
        if (srv6match == null) {
            l.add(beg + "no match srv6");
        } else {
            l.add(beg + "match srv6 " + srv6match);
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
        cmds.cfgLine(l, peerStdMatch == 0, beg, "match peerstd", tabRouteUtil.stdComm2string(peerStdMatch));
        cmds.cfgLine(l, peerLrgMatch == null, beg, "match peerlrg", "" + peerLrgMatch);
        if (stdCommMatch == null) {
            l.add(beg + "no match stdcomm");
        } else {
            l.add(beg + "match stdcomm " + tabRouteUtil.stdComms2string(stdCommMatch));
        }
        if (extCommMatch == null) {
            l.add(beg + "no match extcomm");
        } else {
            l.add(beg + "match extcomm " + tabRouteUtil.extComms2string(extCommMatch));
        }
        if (lrgCommMatch == null) {
            l.add(beg + "no match lrgcomm");
        } else {
            l.add(beg + "match lrgcomm " + tabRouteUtil.lrgComms2string(lrgCommMatch));
        }
        l.add(beg + "match peerasn " + peerasnMatch);
        l.add(beg + "match distance " + distanceMatch);
        l.add(beg + "match locpref " + locPrefMatch);
        l.add(beg + "match validroa " + validRoaMatch);
        l.add(beg + "match validaspa " + validAspaMatch);
        l.add(beg + "match aggregator " + aggregatorMatch);
        l.add(beg + "match customer " + customerMatch);
        l.add(beg + "match pathlen " + pathlenMatch);
        l.add(beg + "match unknowns " + unknownMatch);
        l.add(beg + "match asend " + asendMatch);
        l.add(beg + "match asbeg " + asbegMatch);
        l.add(beg + "match asmid " + asmidMatch);
        l.add(beg + "match aigp " + accIgpMatch);
        l.add(beg + "match bandwidth " + bandwidthMatch);
        l.add(beg + "match origin " + originMatch);
        l.add(beg + "match metric " + metricMatch);
        l.add(beg + "match tag " + tagMatch);
        l.add(beg + "match label-local " + lablocMatch);
        l.add(beg + "match label-remote " + labremMatch);
        l.add(beg + "match segrout " + segrouMatch);
        l.add(beg + "match bier " + bierMatch);
        cmds.cfgLine(l, !noStdComm, beg, "match nostdcomm", "");
        cmds.cfgLine(l, !noExtComm, beg, "match noextcomm", "");
        cmds.cfgLine(l, !noLrgComm, beg, "match nolrgcomm", "");
        cmds.cfgLine(l, trackMatch == null, beg, "match tracker", "" + trackMatch);
        cmds.cfgLine(l, !privasMatch, beg, "match privateas", "");
        cmds.cfgLine(l, !entropyMatch, beg, "match entropy", "");
        cmds.cfgLine(l, !logMatch, beg, "log", "");
        cmds.cfgLine(l, stdCommClear == null, beg, "clear stdcomm", stdCommClear);
        cmds.cfgLine(l, extCommClear == null, beg, "clear extcomm", extCommClear);
        cmds.cfgLine(l, lrgCommClear == null, beg, "clear lrgcomm", lrgCommClear);
        cmds.cfgLine(l, orgntrClear == null, beg, "clear originator", orgntrClear);
        cmds.cfgLine(l, clstLstClear == null, beg, "clear clustlist", clstLstClear);
        cmds.cfgLine(l, !privasClear, beg, "clear privateas", "");
        cmds.cfgLine(l, !entropyClear, beg, "clear entropy", "");
        cmds.cfgLine(l, !peerasClear, beg, "clear peeras", "");
        cmds.cfgLine(l, exactasClear == 0, beg, "clear exactas", "" + bits.num2str(exactasClear));
        cmds.cfgLine(l, !firstasClear, beg, "clear firstas", "");
        if (rouDstSet == -1L) {
            l.add(beg + "no set rd");
        } else {
            l.add(beg + "set rd " + tabRouteUtil.rd2string(rouDstSet));
        }
        cmds.cfgLine(l, roumapSet == null, beg, "set route-map", "" + roumapSet);
        cmds.cfgLine(l, rouplcSet == null, beg, "set route-policy", "" + rouplcSet);
        if (aspathSet == null) {
            l.add(beg + "no set aspath");
        } else {
            l.add(beg + "set aspath " + tabRouteUtil.dumpIntList(aspathSet, "", ""));
        }
        if (aspathCnf == null) {
            l.add(beg + "no set asconfed");
        } else {
            l.add(beg + "set asconfed " + tabRouteUtil.dumpIntList(aspathCnf, "", ""));
        }
        if (stdCommSet == null) {
            l.add(beg + "no set stdcomm");
        } else {
            l.add(beg + "set stdcomm " + tabRouteUtil.stdComms2string(stdCommSet));
        }
        if (extCommSet == null) {
            l.add(beg + "no set extcomm");
        } else {
            l.add(beg + "set extcomm " + tabRouteUtil.extComms2string(extCommSet));
        }
        if (lrgCommSet == null) {
            l.add(beg + "no set lrgcomm");
        } else {
            l.add(beg + "set lrgcomm " + tabRouteUtil.lrgComms2string(lrgCommSet));
        }
        if (vrfSetF == null) {
            l.add(beg + "no set vrf");
        } else {
            l.add(beg + "set vrf " + vrfSetF.cfgName + " " + (vrfSetT ? "ipv4" : "ipv6"));
        }
        if (nexthopSet == null) {
            l.add(beg + "no set nexthop");
        } else {
            l.add(beg + "set nexthop " + nexthopSet);
        }
        if (srv6set == null) {
            l.add(beg + "no set srv6");
        } else {
            l.add(beg + "set srv6 " + srv6set);
        }
        l.add(beg + "set distance " + distanceSet);
        l.add(beg + "set locpref " + locPrefSet);
        l.add(beg + "set aigp " + accIgpSet);
        l.add(beg + "set validroa " + validRoaSet);
        l.add(beg + "set validaspa " + validAspaSet);
        l.add(beg + "set aggregator " + aggregatorSet + " " + aggregatorRtr);
        l.add(beg + "set connector " + connectorSet);
        l.add(beg + "set aslimit " + pathLimSet + " " + pathAsnSet);
        l.add(beg + "set customer " + customerSet);
        l.add(beg + "set bandwidth " + bandwidthSet);
        l.add(beg + "set origin " + originSet);
        l.add(beg + "set metric " + metricSet);
        l.add(beg + "set tag " + tagSet);
        l.add(beg + "set label-local " + lablocSet);
        l.add(beg + "set label-remote " + labremSet);
        l.add(beg + "set segrout " + segrouSet);
        l.add(beg + "set bier " + bierIdxSet + " " + bierSubSet);
        return l;
    }

    /**
     * do clear configuratoin
     *
     * @param a command
     * @param cmd parameters
     * @return true on error, false on success
     */
    public boolean cfgDoClear(String a, cmds cmd) {
        if (a.equals("stdcomm")) {
            stdCommClear = cmd.getRemaining();
            cmd.clear();
            return false;
        }
        if (a.equals("extcomm")) {
            extCommClear = cmd.getRemaining();
            cmd.clear();
            return false;
        }
        if (a.equals("lrgcomm")) {
            lrgCommClear = cmd.getRemaining();
            cmd.clear();
            return false;
        }
        if (a.equals("originator")) {
            orgntrClear = cmd.getRemaining();
            cmd.clear();
            return false;
        }
        if (a.equals("clustlist")) {
            clstLstClear = cmd.getRemaining();
            cmd.clear();
            return false;
        }
        if (a.equals("privateas")) {
            privasClear = true;
            return false;
        }
        if (a.equals("entropy")) {
            entropyClear = true;
            return false;
        }
        if (a.equals("peeras")) {
            peerasClear = true;
            return false;
        }
        if (a.equals("exactas")) {
            exactasClear = bits.str2num(cmd.word());
            return false;
        }
        if (a.equals("firstas")) {
            firstasClear = true;
            return false;
        }
        cmd.badCmd();
        return true;
    }

    /**
     * do clear configuratoin
     *
     * @param a command
     * @param cmd parameters
     * @return true on error, false on success
     */
    public boolean cfgNoClear(String a, cmds cmd) {
        if (a.equals("stdcomm")) {
            stdCommClear = null;
            return false;
        }
        if (a.equals("extcomm")) {
            extCommClear = null;
            return false;
        }
        if (a.equals("lrgcomm")) {
            lrgCommClear = null;
            return false;
        }
        if (a.equals("originator")) {
            orgntrClear = null;
            return false;
        }
        if (a.equals("clustlist")) {
            clstLstClear = null;
            return false;
        }
        if (a.equals("privateas")) {
            privasClear = false;
            return false;
        }
        if (a.equals("entropy")) {
            entropyClear = false;
            return false;
        }
        if (a.equals("peeras")) {
            peerasClear = false;
            return false;
        }
        if (a.equals("exactas")) {
            exactasClear = 0;
            return false;
        }
        if (a.equals("firstas")) {
            firstasClear = false;
            return false;
        }
        cmd.badCmd();
        return true;
    }

    /**
     * do set configuratoin
     *
     * @param a command
     * @param cmd parameters
     * @return true on error, false on success
     */
    public boolean cfgDoSet(String a, cmds cmd) {
        if (a.equals("rd")) {
            rouDstSet = tabRouteUtil.string2rd(cmd.word());
            return false;
        }
        if (a.equals("stdcomm")) {
            stdCommSet = tabRouteUtil.string2stdComms(cmd.getRemaining());
            cmd.clear();
            return false;
        }
        if (a.equals("extcomm")) {
            extCommSet = tabRouteUtil.string2extComms(cmd.getRemaining());
            cmd.clear();
            return false;
        }
        if (a.equals("lrgcomm")) {
            lrgCommSet = tabRouteUtil.string2lrgComms(cmd.getRemaining());
            cmd.clear();
            return false;
        }
        if (a.equals("vrf")) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                cmd.error("no such vrf");
                return true;
            }
            a = cmd.word();
            if (a.equals("ipv4")) {
                vrfSetF = vrf.fwd4;
                vrfSetT = true;
            } else {
                vrfSetF = vrf.fwd6;
                vrfSetT = false;
            }
            return false;
        }
        if (a.equals("nexthop")) {
            nexthopSet = new addrIP();
            nexthopSet.fromString(cmd.word());
            return false;
        }
        if (a.equals("aspath")) {
            aspathSet = tabRouteUtil.string2intList(cmd.getRemaining());
            cmd.clear();
            return false;
        }
        if (a.equals("asconfed")) {
            aspathCnf = tabRouteUtil.string2intList(cmd.getRemaining());
            cmd.clear();
            return false;
        }
        if (a.equals("distance")) {
            if (distanceSet.fromString(cmd.word())) {
                cmd.error("invalid action");
                return true;
            }
            return false;
        }
        if (a.equals("metric")) {
            if (metricSet.fromString(cmd.word())) {
                cmd.error("invalid action");
                return true;
            }
            return false;
        }
        if (a.equals("origin")) {
            if (originSet.fromString(cmd.word())) {
                cmd.error("invalid action");
                return true;
            }
            return false;
        }
        if (a.equals("locpref")) {
            if (locPrefSet.fromString(cmd.word())) {
                cmd.error("invalid action");
                return true;
            }
            return false;
        }
        if (a.equals("aigp")) {
            if (accIgpSet.fromString(cmd.word())) {
                cmd.error("invalid action");
                return true;
            }
            return false;
        }
        if (a.equals("validroa")) {
            if (validRoaSet.fromString(cmd.word())) {
                cmd.error("invalid action");
                return true;
            }
            return false;
        }
        if (a.equals("validaspa")) {
            if (validAspaSet.fromString(cmd.word())) {
                cmd.error("invalid action");
                return true;
            }
            return false;
        }
        if (a.equals("aggregator")) {
            a = cmd.word();
            if (aggregatorSet.fromString(a)) {
                cmd.error("invalid action");
                return true;
            }
            aggregatorRtr = new addrIP();
            a = cmd.word();
            if (aggregatorRtr.fromString(a)) {
                cmd.error("bad address");
                return true;
            }
            return false;
        }
        if (a.equals("aslimit")) {
            if (pathLimSet.fromString(cmd.word())) {
                cmd.error("invalid action");
                return true;
            }
            if (pathAsnSet.fromString(cmd.word())) {
                cmd.error("invalid action");
                return true;
            }
            return false;
        }
        if (a.equals("connector")) {
            connectorSet = new addrIP();
            a = cmd.word();
            if (connectorSet.fromString(a)) {
                cmd.error("bad address");
                return true;
            }
            return false;
        }
        if (a.equals("customer")) {
            if (customerSet.fromString(cmd.word())) {
                cmd.error("invalid action");
                return true;
            }
            return false;
        }
        if (a.equals("bandwidth")) {
            if (bandwidthSet.fromString(cmd.word())) {
                cmd.error("invalid action");
                return true;
            }
            return false;
        }
        if (a.equals("tag")) {
            if (tagSet.fromString(cmd.word())) {
                cmd.error("invalid action");
                return true;
            }
            return false;
        }
        if (a.equals("label-local")) {
            if (lablocSet.fromString(cmd.word())) {
                cmd.error("invalid action");
                return true;
            }
            return false;
        }
        if (a.equals("label-remote")) {
            if (labremSet.fromString(cmd.word())) {
                cmd.error("invalid action");
                return true;
            }
            return false;
        }
        if (a.equals("segrout")) {
            if (segrouSet.fromString(cmd.word())) {
                cmd.error("invalid action");
                return true;
            }
            return false;
        }
        if (a.equals("bier")) {
            if (bierIdxSet.fromString(cmd.word())) {
                cmd.error("invalid action");
                return true;
            }
            if (bierSubSet.fromString(cmd.word())) {
                cmd.error("invalid action");
                return true;
            }
            return false;
        }
        if (a.equals("srv6")) {
            srv6set = new addrIP();
            if (srv6set.fromString(cmd.word())) {
                cmd.error("invalid address");
                return true;
            }
            return false;
        }
        if (a.equals("route-map")) {
            cfgRoump roumap = cfgAll.rtmpFind(cmd.word(), false);
            if (roumap == null) {
                cmd.error("no such route map");
                return true;
            }
            roumapSet = roumap.roumap;
            return false;
        }
        if (a.equals("route-policy")) {
            cfgRouplc roumap = cfgAll.rtplFind(cmd.word(), false);
            if (roumap == null) {
                cmd.error("no such route policy");
                return true;
            }
            rouplcSet = roumap.rouplc;
            return false;
        }
        cmd.badCmd();
        return true;
    }

    /**
     * do set configuratoin
     *
     * @param a command
     * @param cmd parameters
     * @return true on error, false on success
     */
    public boolean cfgNoSet(String a, cmds cmd) {
        if (a.equals("rd")) {
            rouDstSet = -1L;
            return false;
        }
        if (a.equals("stdcomm")) {
            stdCommSet = null;
            return false;
        }
        if (a.equals("vrf")) {
            vrfSetF = null;
            vrfSetT = false;
            return false;
        }
        if (a.equals("nexthop")) {
            nexthopSet = null;
            return false;
        }
        if (a.equals("extcomm")) {
            extCommSet = null;
            return false;
        }
        if (a.equals("lrgcomm")) {
            lrgCommSet = null;
            return false;
        }
        if (a.equals("aspath")) {
            aspathSet = null;
            return false;
        }
        if (a.equals("asconfed")) {
            aspathCnf = null;
            return false;
        }
        if (a.equals("distance")) {
            distanceSet.set2unchange();
            return false;
        }
        if (a.equals("metric")) {
            metricSet.set2unchange();
            return false;
        }
        if (a.equals("origin")) {
            originSet.set2unchange();
            return false;
        }
        if (a.equals("locpref")) {
            locPrefSet.set2unchange();
            return false;
        }
        if (a.equals("aigp")) {
            accIgpSet.set2unchange();
            return false;
        }
        if (a.equals("validroa")) {
            validRoaSet.set2unchange();
            return false;
        }
        if (a.equals("validaspa")) {
            validAspaSet.set2unchange();
            return false;
        }
        if (a.equals("aggregator")) {
            aggregatorSet.set2unchange();
            aggregatorRtr = null;
            return false;
        }
        if (a.equals("aslimit")) {
            pathLimSet.set2unchange();
            pathAsnSet.set2unchange();
            return false;
        }
        if (a.equals("connector")) {
            connectorSet = null;
            return false;
        }
        if (a.equals("customer")) {
            customerSet.set2unchange();
            return false;
        }
        if (a.equals("bandwidth")) {
            bandwidthSet.set2unchange();
            return false;
        }
        if (a.equals("tag")) {
            tagSet.set2unchange();
            return false;
        }
        if (a.equals("label-local")) {
            lablocSet.set2unchange();
            return false;
        }
        if (a.equals("label-remote")) {
            labremSet.set2unchange();
            return false;
        }
        if (a.equals("segrout")) {
            segrouSet.set2unchange();
            return false;
        }
        if (a.equals("bier")) {
            bierIdxSet.set2unchange();
            bierSubSet.set2unchange();
            return false;
        }
        if (a.equals("srv6")) {
            srv6set = null;
            return false;
        }
        if (a.equals("route-map")) {
            roumapSet = null;
            return false;
        }
        if (a.equals("route-policy")) {
            rouplcSet = null;
            return false;
        }
        cmd.badCmd();
        return false;
    }

    /**
     * do match configuratoin
     *
     * @param a command
     * @param cmd parameters
     * @return true on error, false on success
     */
    public boolean cfgDoMatch(String a, cmds cmd) {
        if (a.equals("interface")) {
            ifaceMatch = cfgAll.ifcFind(cmd.word(), 0);
            return false;
        }
        if (a.equals("nexthop")) {
            nexthopMatch = new addrIP();
            nexthopMatch.fromString(cmd.word());
            return false;
        }
        if (a.equals("recursive")) {
            oldhopMatch = new addrIP();
            oldhopMatch.fromString(cmd.word());
            return false;
        }
        if (a.equals("protocol")) {
            protoTypMatch = cfgRtr.name2num(cmd.word());
            if (protoTypMatch == null) {
                cmd.error("invalid protocol");
                return true;
            }
            protoNumMatch = bits.str2num(cmd.word());
            if (!cfgRtr.num2proc(protoTypMatch)) {
                protoNumMatch = -1;
            }
            return false;
        }
        if (a.equals("aspath")) {
            aspathMatch = cmd.getRemaining();
            cmd.clear();
            return false;
        }
        if (a.equals("peerstd")) {
            peerStdMatch = tabRouteUtil.string2stdComm(cmd.word());
            return false;
        }
        if (a.equals("peerlrg")) {
            tabLargeComm d = new tabLargeComm();
            if (d.fromString(cmd.word())) {
                return true;
            }
            peerLrgMatch = d;
            return false;
        }
        if (a.equals("stdcomm")) {
            stdCommMatch = tabRouteUtil.string2stdComms(cmd.getRemaining());
            cmd.clear();
            return false;
        }
        if (a.equals("extcomm")) {
            extCommMatch = tabRouteUtil.string2extComms(cmd.getRemaining());
            cmd.clear();
            return false;
        }
        if (a.equals("lrgcomm")) {
            lrgCommMatch = tabRouteUtil.string2lrgComms(cmd.getRemaining());
            cmd.clear();
            return false;
        }
        if (a.equals("rd")) {
            rouDstMatch = tabRouteUtil.string2rd(cmd.word());
            return false;
        }
        if (a.equals("network")) {
            networkMatch = new tabPrfxlstN();
            networkMatch.action = tabListingEntry.actionType.actPermit;
            if (networkMatch.fromString(cmd.getRemaining())) {
                networkMatch = null;
                cmd.error("invalid prefix");
                return true;
            }
            cmd.clear();
            return false;
        }
        if (a.equals("nostdcomm")) {
            noStdComm = true;
            return false;
        }
        if (a.equals("noextcomm")) {
            noExtComm = true;
            return false;
        }
        if (a.equals("nolrgcomm")) {
            noLrgComm = true;
            return false;
        }
        if (a.equals("privateas")) {
            privasMatch = true;
            return false;
        }
        if (a.equals("entropy")) {
            entropyMatch = true;
            return false;
        }
        if (a.equals("tracker")) {
            trackMatch = cmd.word();
            return false;
        }
        if (a.equals("access-list")) {
            cfgAceslst acl = cfgAll.aclsFind(cmd.word(), false);
            if (acl == null) {
                cmd.error("no such access list");
                return true;
            }
            aceslstMatch = acl.aceslst;
            return false;
        }
        if (a.equals("prefix-list")) {
            cfgPrfxlst prfxlst = cfgAll.prfxFind(cmd.word(), false);
            if (prfxlst == null) {
                cmd.error("no such prefix list");
                return true;
            }
            prfxlstMatch = prfxlst.prflst;
            return false;
        }
        if (a.equals("route-map")) {
            cfgRoump roumap = cfgAll.rtmpFind(cmd.word(), false);
            if (roumap == null) {
                cmd.error("no such route map");
                return true;
            }
            roumapMatch = roumap.roumap;
            return false;
        }
        if (a.equals("route-policy")) {
            cfgRouplc roumap = cfgAll.rtplFind(cmd.word(), false);
            if (roumap == null) {
                cmd.error("no such route policy");
                return true;
            }
            rouplcMatch = roumap.rouplc;
            return false;
        }
        if (a.equals("peerasn")) {
            if (peerasnMatch.fromString(cmd.word())) {
                cmd.error("invalid action");
                return true;
            }
            return false;
        }
        if (a.equals("distance")) {
            if (distanceMatch.fromString(cmd.word())) {
                cmd.error("invalid action");
                return true;
            }
            return false;
        }
        if (a.equals("metric")) {
            if (metricMatch.fromString(cmd.word())) {
                cmd.error("invalid action");
                return true;
            }
            return false;
        }
        if (a.equals("origin")) {
            if (originMatch.fromString(cmd.word())) {
                cmd.error("invalid action");
                return true;
            }
            return false;
        }
        if (a.equals("locpref")) {
            if (locPrefMatch.fromString(cmd.word())) {
                cmd.error("invalid action");
                return true;
            }
            return false;
        }
        if (a.equals("aigp")) {
            if (accIgpMatch.fromString(cmd.word())) {
                cmd.error("invalid action");
                return true;
            }
            return false;
        }
        if (a.equals("validroa")) {
            if (validRoaMatch.fromString(cmd.word())) {
                cmd.error("invalid action");
                return true;
            }
            return false;
        }
        if (a.equals("validaspa")) {
            if (validAspaMatch.fromString(cmd.word())) {
                cmd.error("invalid action");
                return true;
            }
            return false;
        }
        if (a.equals("aggregator")) {
            if (aggregatorMatch.fromString(cmd.word())) {
                cmd.error("invalid action");
                return true;
            }
            return false;
        }
        if (a.equals("customer")) {
            if (customerMatch.fromString(cmd.word())) {
                cmd.error("invalid action");
                return true;
            }
            return false;
        }
        if (a.equals("pathlen")) {
            if (pathlenMatch.fromString(cmd.word())) {
                cmd.error("invalid action");
                return true;
            }
            return false;
        }
        if (a.equals("unknowns")) {
            if (unknownMatch.fromString(cmd.word())) {
                cmd.error("invalid action");
                return true;
            }
            return false;
        }
        if (a.equals("asend")) {
            if (asendMatch.fromString(cmd.word())) {
                cmd.error("invalid action");
                return true;
            }
            return false;
        }
        if (a.equals("asbeg")) {
            if (asbegMatch.fromString(cmd.word())) {
                cmd.error("invalid action");
                return true;
            }
            return false;
        }
        if (a.equals("asmid")) {
            if (asmidMatch.fromString(cmd.word())) {
                cmd.error("invalid action");
                return true;
            }
            return false;
        }
        if (a.equals("bandwidth")) {
            if (bandwidthMatch.fromString(cmd.word())) {
                cmd.error("invalid action");
                return true;
            }
            return false;
        }
        if (a.equals("tag")) {
            if (tagMatch.fromString(cmd.word())) {
                cmd.error("invalid action");
                return true;
            }
            return false;
        }
        if (a.equals("label-local")) {
            if (lablocMatch.fromString(cmd.word())) {
                cmd.error("invalid action");
                return true;
            }
            return false;
        }
        if (a.equals("label-remote")) {
            if (labremMatch.fromString(cmd.word())) {
                cmd.error("invalid action");
                return true;
            }
            return false;
        }
        if (a.equals("segrout")) {
            if (segrouMatch.fromString(cmd.word())) {
                cmd.error("invalid action");
                return true;
            }
            return false;
        }
        if (a.equals("bier")) {
            if (bierMatch.fromString(cmd.word())) {
                cmd.error("invalid action");
                return true;
            }
            return false;
        }
        if (a.equals("srv6")) {
            srv6match = new addrIP();
            if (srv6match.fromString(cmd.word())) {
                cmd.error("invalid address");
                return true;
            }
            return false;
        }
        if (a.equals("afi")) {
            if (afiMatch.fromString(cmd.word())) {
                cmd.error("invalid action");
                return true;
            }
            return false;
        }
        if (a.equals("safi")) {
            if (safiMatch.fromString(cmd.word())) {
                cmd.error("invalid action");
                return true;
            }
            return false;
        }
        cmd.badCmd();
        return true;
    }

    /**
     * do match configuratoin
     *
     * @param a command
     * @param cmd parameters
     * @return true on error, false on success
     */
    public boolean cfgNoMatch(String a, cmds cmd) {
        if (a.equals("interface")) {
            ifaceMatch = null;
            return false;
        }
        if (a.equals("nexthop")) {
            nexthopMatch = null;
            return false;
        }
        if (a.equals("recursive")) {
            oldhopMatch = null;
            return false;
        }
        if (a.equals("protocol")) {
            protoTypMatch = null;
            protoNumMatch = 0;
            return false;
        }
        if (a.equals("aspath")) {
            aspathMatch = null;
            return false;
        }
        if (a.equals("peerstd")) {
            peerStdMatch = 0;
            return false;
        }
        if (a.equals("peerlrg")) {
            peerLrgMatch = null;
            return false;
        }
        if (a.equals("stdcomm")) {
            stdCommMatch = null;
            return false;
        }
        if (a.equals("extcomm")) {
            extCommMatch = null;
            return false;
        }
        if (a.equals("lrgcomm")) {
            lrgCommMatch = null;
            return false;
        }
        if (a.equals("rd")) {
            rouDstMatch = -1L;
            return false;
        }
        if (a.equals("network")) {
            networkMatch = null;
            return false;
        }
        if (a.equals("access-list")) {
            aceslstMatch = null;
            return false;
        }
        if (a.equals("prefix-list")) {
            prfxlstMatch = null;
            return false;
        }
        if (a.equals("route-map")) {
            roumapMatch = null;
            return false;
        }
        if (a.equals("route-policy")) {
            rouplcMatch = null;
            return false;
        }
        if (a.equals("nostdcomm")) {
            noStdComm = false;
            return false;
        }
        if (a.equals("noextcomm")) {
            noExtComm = false;
            return false;
        }
        if (a.equals("nolrgcomm")) {
            noLrgComm = false;
            return false;
        }
        if (a.equals("privateas")) {
            privasMatch = false;
            return false;
        }
        if (a.equals("entropy")) {
            entropyMatch = false;
            return false;
        }
        if (a.equals("tracker")) {
            trackMatch = null;
            return false;
        }
        if (a.equals("peerasn")) {
            peerasnMatch.set2always();
            return false;
        }
        if (a.equals("distance")) {
            distanceMatch.set2always();
            return false;
        }
        if (a.equals("metric")) {
            metricMatch.set2always();
            return false;
        }
        if (a.equals("origin")) {
            originMatch.set2always();
            return false;
        }
        if (a.equals("locpref")) {
            locPrefMatch.set2always();
            return false;
        }
        if (a.equals("aigp")) {
            accIgpMatch.set2always();
            return false;
        }
        if (a.equals("validroa")) {
            validRoaMatch.set2always();
            return false;
        }
        if (a.equals("validaspa")) {
            validAspaMatch.set2always();
            return false;
        }
        if (a.equals("aggregator")) {
            aggregatorMatch.set2always();
            return false;
        }
        if (a.equals("customer")) {
            customerMatch.set2always();
            return false;
        }
        if (a.equals("pathlen")) {
            pathlenMatch.set2always();
            return false;
        }
        if (a.equals("unknowns")) {
            unknownMatch.set2always();
            return false;
        }
        if (a.equals("asend")) {
            asendMatch.set2always();
            return false;
        }
        if (a.equals("asbeg")) {
            asbegMatch.set2always();
            return false;
        }
        if (a.equals("asmid")) {
            asmidMatch.set2always();
            return false;
        }
        if (a.equals("bandwidth")) {
            bandwidthMatch.set2always();
            return false;
        }
        if (a.equals("tag")) {
            tagMatch.set2always();
            return false;
        }
        if (a.equals("label-local")) {
            lablocMatch.set2always();
            return false;
        }
        if (a.equals("label-remote")) {
            labremMatch.set2always();
            return false;
        }
        if (a.equals("segrout")) {
            segrouMatch.set2always();
            return false;
        }
        if (a.equals("bier")) {
            bierMatch.set2always();
            return false;
        }
        if (a.equals("srv6")) {
            srv6match = null;
            return false;
        }
        if (a.equals("afi")) {
            afiMatch.set2always();
            return false;
        }
        if (a.equals("safi")) {
            safiMatch.set2always();
            return false;
        }
        cmd.badCmd();
        return true;
    }

    public boolean matches(int afi, int asn, addrPrefix<addrIP> net) {
        tabRouteEntry<addrIP> nt = new tabRouteEntry<addrIP>();
        nt.prefix = net;
        return matches(afi, asn, nt);
    }

    public boolean matches(int afi, int asn, tabRouteEntry<addrIP> net) {
        if (rouDstMatch != -1L) {
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
        if (lablocMatch.action != tabIntMatcher.actionType.always) {
            if (net.best.labelLoc == null) {
                return false;
            }
            if (!lablocMatch.matches(net.best.labelLoc.label)) {
                return false;
            }
        }
        if (labremMatch.action != tabIntMatcher.actionType.always) {
            if (net.best.labelRem == null) {
                return false;
            }
            if (net.best.labelRem.size() < 1) {
                return false;
            }
            if (!labremMatch.matches(net.best.labelRem.get(0))) {
                return false;
            }
        }
        if (!segrouMatch.matches(net.best.segrouIdx)) {
            return false;
        }
        if (!bierMatch.matches(net.best.bierIdx)) {
            return false;
        }
        if (!validRoaMatch.matches(net.best.validRoa)) {
            return false;
        }
        if (!validAspaMatch.matches(net.best.validAspa)) {
            return false;
        }
        if (!aggregatorMatch.matches(net.best.aggrAs)) {
            return false;
        }
        if (!customerMatch.matches(net.best.onlyCust)) {
            return false;
        }
        if (!pathlenMatch.matches(net.best.asPathLen())) {
            return false;
        }
        if (!unknownMatch.matches(net.best.unkAttrCnt())) {
            return false;
        }
        if (!asendMatch.matches(net.best.asPathEnd())) {
            return false;
        }
        if (!asbegMatch.matches(net.best.asPathBeg())) {
            return false;
        }
        if (!net.best.asPathMid(asmidMatch, 0, 1)) {
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
            int i = tabRouteUtil.removePrivateAs(tabLabel.copyLabels(net.best.pathSeq));
            i += tabRouteUtil.removePrivateAs(tabLabel.copyLabels(net.best.pathSet));
            if (i < 1) {
                return false;
            }
        }
        if (entropyMatch) {
            if (net.best.entropyLabel == null) {
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
            if (nexthopMatch.compareTo(net.best.nextHop) != 0) {
                return false;
            }
        }
        if (oldhopMatch != null) {
            if (net.best.oldHop == null) {
                return false;
            }
            if (oldhopMatch.compareTo(net.best.oldHop) != 0) {
                return false;
            }
        }
        if (srv6match != null) {
            if (net.best.segrouPrf == null) {
                return false;
            }
            if (srv6match.compareTo(net.best.segrouPrf) != 0) {
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
            int i = tabRouteUtil.stdCommAsn(peerStdMatch, asn);
            if (tabRouteUtil.findIntList(net.best.stdComm, i) < 0) {
                return false;
            }
        }
        if (peerLrgMatch != null) {
            tabLargeComm lrg = peerLrgMatch.copyBytes();
            lrg.d2 = asn;
            if (tabRouteUtil.findLrgList(net.best.lrgComm, lrg) < 0) {
                return false;
            }
        }
        if (stdCommMatch != null) {
            for (int i = 0; i < stdCommMatch.size(); i++) {
                if (tabRouteUtil.findIntList(net.best.stdComm, stdCommMatch.get(i)) < 0) {
                    return false;
                }
            }
        }
        if (extCommMatch != null) {
            for (int i = 0; i < extCommMatch.size(); i++) {
                if (tabRouteUtil.findLongList(net.best.extComm, extCommMatch.get(i)) < 0) {
                    return false;
                }
            }
        }
        if (lrgCommMatch != null) {
            for (int i = 0; i < lrgCommMatch.size(); i++) {
                if (tabRouteUtil.findLrgList(net.best.lrgComm, lrgCommMatch.get(i)) < 0) {
                    return false;
                }
            }
        }
        if (aceslstMatch != null) {
            packHolder pck = new packHolder(false, false);
            if ((afi & rtrBgpUtil.sfiMask) != rtrBgpUtil.sfiEthVpn) {
                pck.IPsrc.setAddr(net.prefix.network);
                pck.IPtrg.setAddr(net.prefix.mask);
            } else {
                addrPrefix<addrIP> pfx = tabRouteUtil.convertL3evpn(net.prefix);
                if (pfx == null) {
                    return false;
                }
                pck.IPsrc.setAddr(pfx.network);
                pck.IPtrg.setAddr(pfx.mask);
            }
            if (!aceslstMatch.matches(false, false, pck)) {
                return false;
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
        if (validRoaSet.action != tabIntUpdater.actionType.nothing) {
            attr.validRoa = validRoaSet.update(attr.validRoa);
            attr.extComm = tabRouteUtil.setValidExtCommRoa(attr.extComm, attr.validRoa);
        }
        if (validAspaSet.action != tabIntUpdater.actionType.nothing) {
            attr.validAspa = validAspaSet.update(attr.validAspa);
            attr.extComm = tabRouteUtil.setValidExtCommAspa(attr.extComm, attr.validAspa);
        }
        if (aggregatorSet.action != tabIntUpdater.actionType.nothing) {
            attr.aggrAs = aggregatorSet.update(attr.aggrAs);
            attr.aggrRtr = aggregatorRtr.copyBytes();
        }
        if (connectorSet != null) {
            attr.connRtr = connectorSet.copyBytes();
        }
        attr.pathLim = pathLimSet.update(attr.pathLim);
        attr.pathAsn = pathAsnSet.update(attr.pathAsn);
        attr.onlyCust = customerSet.update(attr.onlyCust);
        attr.bandwidth = bandwidthSet.update(attr.bandwidth);
        attr.origin = originSet.update(attr.origin);
        attr.metric = metricSet.update(attr.metric);
        attr.tag = tagSet.update(attr.tag);
        tabRouteUtil.updateLabloc(attr, lablocSet);
        tabRouteUtil.updateLabrem(attr, labremSet);
        attr.segrouIdx = segrouSet.update(attr.segrouIdx);
        attr.bierIdx = bierIdxSet.update(attr.bierIdx);
        attr.bierSub = bierSubSet.update(attr.bierSub);
        attr.pathSeq = tabLabel.prependLabels(attr.pathSeq, aspathSet);
        attr.confSeq = tabLabel.prependLabels(attr.confSeq, aspathCnf);
        if (stdCommClear != null) {
            tabRouteUtil.removeStdComm(attr, stdCommClear);
        }
        if (extCommClear != null) {
            tabRouteUtil.removeExtComm(attr, extCommClear);
        }
        if (lrgCommClear != null) {
            tabRouteUtil.removeLrgComm(attr, lrgCommClear);
        }
        if (orgntrClear != null) {
            tabRouteUtil.removeOrgntr(attr, orgntrClear);
        }
        if (clstLstClear != null) {
            tabRouteUtil.removeClstLst(attr, clstLstClear);
        }
        if (privasClear) {
            tabRouteUtil.removePrivateAs(attr.pathSeq);
            tabRouteUtil.removePrivateAs(attr.pathSet);
        }
        if (entropyClear) {
            attr.entropyLabel = null;
        }
        if (peerasClear) {
            tabRouteUtil.removeIntList(attr.pathSeq, asn);
            tabRouteUtil.removeIntList(attr.pathSet, asn);
        }
        if (exactasClear != 0) {
            tabRouteUtil.removeIntList(attr.pathSeq, exactasClear);
            tabRouteUtil.removeIntList(attr.pathSet, exactasClear);
        }
        if (firstasClear) {
            tabRouteUtil.removeFirstAs(attr);
        }
        attr.stdComm = tabLabel.prependLabels(attr.stdComm, stdCommSet);
        if (vrfSetF != null) {
            attr.rouTab = vrfSetF;
        }
        if (nexthopSet != null) {
            attr.nextHop = nexthopSet.copyBytes();
        }
        if (srv6set != null) {
            attr.segrouPrf = srv6set.copyBytes();
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
        if (rouDstSet != -1L) {
            net.rouDst = rouDstSet;
        }
        if (roumapSet != null) {
            roumapSet.update(afi, asn, net, false);
        }
        if (rouplcSet != null) {
            tabRtrplc.doRpl(afi, asn, net, rouplcSet, false);
        }
        if (script != null) {
            tabRouteUtil.doTcl(afi, asn, net.best, net, script);
        }
    }

}
