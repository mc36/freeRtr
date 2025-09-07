package org.freertr.rtr;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrIPv6;
import org.freertr.addr.addrPrefix;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgPrfxlst;
import org.freertr.cfg.cfgRoump;
import org.freertr.cfg.cfgRouplc;
import org.freertr.cfg.cfgVrf;
import org.freertr.enc.enc7bit;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipRtr;
import org.freertr.pack.packHolder;
import org.freertr.prt.prtUdp;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabIndex;
import org.freertr.tab.tabIntMatcher;
import org.freertr.tab.tabLabel;
import org.freertr.tab.tabLabelBier;
import org.freertr.tab.tabLabelEntry;
import org.freertr.tab.tabRoute;
import org.freertr.tab.tabRouteAttr;
import org.freertr.tab.tabRouteEntry;
import org.freertr.user.userFormat;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.spf.spfCalc;
import org.freertr.spf.spfLnkst;
import org.freertr.util.state;

/**
 * open shortest path first (rfc5340) protocol v3
 *
 * @author matecsaba
 */
public class rtrOspf6 extends ipRtr {

    /**
     * protocol number
     */
    public final static int protoNum = 89;

    /**
     * protocol version number
     */
    public final static int verNum = 3;

    /**
     * protocol header size
     */
    public final static int sizeHead = 16;

    /**
     * router id
     */
    public addrIPv4 routerID;

    /**
     * traffic engineering id
     */
    public addrIPv6 traffEngID;

    /**
     * segment routing maximum
     */
    public int segrouMax = 0;

    /**
     * segment routing base
     */
    public int segrouBase = 0;

    /**
     * bier length
     */
    public int bierLen = 0;

    /**
     * bier maximum
     */
    public int bierMax = 0;

    /**
     * external distance
     */
    public int distantExt;

    /**
     * intra-area distance
     */
    public int distantInt;

    /**
     * inter-area distance
     */
    public int distantSum;

    /**
     * forwarding core
     */
    public final ipFwd fwdCore;

    /**
     * udp core
     */
    public final prtUdp udpCore;

    /**
     * list of flexalgos
     */
    protected tabGen<rtrAlgo> algos;

    /**
     * list of interfaces
     */
    protected tabGen<rtrOspf6iface> ifaces;

    /**
     * list of srv6 advertisements
     */
    protected tabGen<cfgIfc> srv6;

    /**
     * list of areas
     */
    protected tabGen<rtrOspf6area> areas;

    /**
     * segment routing labels
     */
    protected tabLabelEntry[] segrouLab;

    /**
     * bier labels
     */
    protected tabLabelEntry[] bierLab;

    private int intIds;

    /**
     * create one ospf process
     *
     * @param forwarder the ip protocol
     * @param udp the udp protocol
     * @param id process id
     */
    public rtrOspf6(ipFwd forwarder, prtUdp udp, int id) {
        fwdCore = forwarder;
        udpCore = udp;
        algos = new tabGen<rtrAlgo>();
        ifaces = new tabGen<rtrOspf6iface>();
        srv6 = new tabGen<cfgIfc>();
        areas = new tabGen<rtrOspf6area>();
        routerID = new addrIPv4();
        traffEngID = new addrIPv6();
        distantExt = 110;
        distantInt = 110;
        distantSum = 110;
        intIds = 1;
        routerCreateComputed();
        fwdCore.routerAdd(this, tabRouteAttr.routeType.ospf6, id);
    }

    /**
     * convert to string
     *
     * @return string
     */
    public String toString() {
        return "ospf on " + fwdCore;
    }

    /**
     * create computed
     */
    public synchronized void routerCreateComputed() {
        if (debugger.rtrOspf6evnt) {
            logger.debug("create table");
        }
        tabRoute<addrIP> tab1 = new tabRoute<addrIP>("ospf");
        tabGen<tabIndex<addrIP>> tab2 = new tabGen<tabIndex<addrIP>>();
        for (int i = 0; i < areas.size(); i++) {
            rtrOspf6area ntry = areas.get(i);
            if (ntry == null) {
                continue;
            }
            tab1.mergeFrom(tabRoute.addType.ecmp, ntry.routes, tabRouteAttr.distanLim);
        }
        if (segrouLab != null) {
            for (int i = 0; i < areas.size(); i++) {
                rtrOspf6area ntry = areas.get(i);
                if (ntry == null) {
                    continue;
                }
                tabIndex.mergeTable(tab2, ntry.segrouUsd);
            }
            for (int i = 0; i < segrouLab.length; i++) {
                if (tab2.find(new tabIndex<addrIP>(i, null)) != null) {
                    continue;
                }
                segrouLab[i].setFwdDrop(tabLabelEntry.owner.ospf6srgb);
            }
        }
        if (bierLab != null) {
            int o = 0;
            for (int i = 0; i < ifaces.size(); i++) {
                rtrOspf6iface ifc = ifaces.get(i);
                if (ifc == null) {
                    continue;
                }
                if (ifc.brIndex < 1) {
                    continue;
                }
                o = ifc.brIndex;
                break;
            }
            tabLabelBier res = new tabLabelBier(bierLab[0].label, tabLabelBier.num2bsl(bierLen));
            res.idx = o;
            for (int i = 0; i < areas.size(); i++) {
                rtrOspf6area ntry = areas.get(i);
                if (ntry == null) {
                    continue;
                }
                res.mergeFrom(ntry.bierRes);
            }
            for (int i = 0; i < bierLab.length; i++) {
                bierLab[i].setBierMpls(tabLabelEntry.owner.ospf6bier, fwdCore, res);
            }
        }
        tab1.setProto(routerProtoTyp, routerProcNum);
        boolean same = tab1.preserveTime(routerComputedU);
        same &= !tabIndex.compareTables(routerComputedI, tab2);
        if (same) {
            return;
        }
        routerComputedU = tab1;
        routerComputedM = tab1;
        routerComputedF = new tabRoute<addrIP>("rx");
        routerComputedI = tab2;
        fwdCore.routerChg(this, false);
        for (int o = 0; o < algos.size(); o++) {
            rtrAlgo alg = algos.get(o);
            if (alg == null) {
                continue;
            }
            tab1 = new tabRoute<addrIP>("ospf");
            for (int i = 0; i < areas.size(); i++) {
                rtrOspf6area ntry = areas.get(i);
                if (ntry == null) {
                    continue;
                }
                tabRoute<addrIP> tab3;
                try {
                    tab3 = ntry.algos.get(o);
                } catch (Exception e) {
                    continue;
                }
                if (tab3 == null) {
                    continue;
                }
                tab1.mergeFrom(tabRoute.addType.ecmp, tab3, tabRouteAttr.distanLim);
            }
            alg.vrf.update2ip(tab1);
        }
    }

    /**
     * redistribution changed
     */
    public void routerRedistChanged() {
        genLsas(3);
    }

    /**
     * others changed
     */
    public void routerOthersChanged() {
    }

    /**
     * get help
     *
     * @param l list
     */
    public void routerGetHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "router-id", "specify router id");
        l.add(null, false, 2, new int[]{-1}, "<addr>", "router id");
        l.add(null, false, 1, new int[]{2}, "traffeng-id", "specify traffic engineering id");
        l.add(null, false, 2, new int[]{-1}, "<addr>", "te id");
        l.add(null, false, 1, new int[]{2}, "srv6", "advertise srv6 locator");
        l.add(null, false, 2, new int[]{-1}, "<name:ifc>", "name of interface");
        l.add(null, false, 1, new int[]{2}, "segrout", "segment routing parameters");
        l.add(null, false, 2, new int[]{3, -1}, "<num>", "maximum index");
        l.add(null, false, 3, new int[]{4}, "base", "specify base");
        l.add(null, false, 4, new int[]{3, -1}, "<num>", "label base");
        l.add(null, false, 1, new int[]{2}, "bier", "bier parameters");
        l.add(null, false, 2, new int[]{3}, "<num>", "bitstring length");
        l.add(null, false, 3, new int[]{-1}, "<num>", "maximum index");
        l.add(null, false, 1, new int[]{2}, "area", "configure one area");
        l.add(null, false, 2, new int[]{3}, "<num>", "area number");
        l.add(null, false, 3, new int[]{-1}, "enable", "create this area");
        l.add(null, false, 3, new int[]{-1}, "ha-mode", "save state");
        l.add(null, false, 3, new int[]{-1}, "spf-bidir", "spf bidir check");
        l.add(null, false, 3, new int[]{4, -1}, "spf-topolog", "spf topology logging");
        l.add(null, false, 4, new int[]{4, -1}, "noappear", "exclude node (dis)appearance");
        l.add(null, false, 4, new int[]{4, -1}, "noconnect", "exclude link (dis)connection");
        l.add(null, false, 4, new int[]{4, -1}, "noforward", "exclude forward (un)willingness");
        l.add(null, false, 4, new int[]{4, -1}, "noreachable", "exclude node (un)reachable");
        l.add(null, false, 4, new int[]{4, -1}, "nometric", "exclude link metric change");
        l.add(null, false, 4, new int[]{4, -1}, "noprefix", "exclude prefix change");
        l.add(null, false, 3, new int[]{-1}, "spf-hops", "spf hops disallow");
        l.add(null, false, 3, new int[]{-1}, "spf-ecmp", "spf ecmp allow");
        l.add(null, false, 3, new int[]{4}, "spf-log", "spf log size");
        l.add(null, false, 4, new int[]{-1}, "<num>", "number of entries");
        l.add(null, false, 3, new int[]{-1}, "max-metric", "configure as maximum metric");
        l.add(null, false, 3, new int[]{-1}, "stub", "configure as stub");
        l.add(null, false, 3, new int[]{-1}, "nssa", "configure as nssa");
        l.add(null, false, 3, new int[]{-1}, "traffeng", "configure for traffic engineering");
        l.add(null, false, 3, new int[]{-1}, "segrout", "configure for segment routing");
        l.add(null, false, 3, new int[]{-1}, "srv6", "configure for segment routing v6");
        l.add(null, false, 3, new int[]{-1}, "bier", "configure for bier");
        l.add(null, false, 3, new int[]{-1}, "suppress-prefix", "do not advertise interfaces");
        l.add(null, false, 3, new int[]{-1}, "hostname", "advertise hostname");
        l.add(null, false, 3, new int[]{-1}, "default-originate", "advertise default route");
        l.add(null, false, 3, new int[]{4}, "route-map-from", "process prefixes from this area");
        l.add(null, false, 4, new int[]{-1}, "<name:rm>", "name of route map");
        l.add(null, false, 3, new int[]{4}, "route-map-into", "process prefixes into this area");
        l.add(null, false, 4, new int[]{-1}, "<name:rm>", "name of route map");
        l.add(null, false, 3, new int[]{4}, "route-policy-from", "process prefixes from this area");
        l.add(null, false, 4, new int[]{-1}, "<name:rpl>", "name of route map");
        l.add(null, false, 3, new int[]{4}, "route-policy-into", "process prefixes into this area");
        l.add(null, false, 4, new int[]{-1}, "<name:rpl>", "name of route map");
        l.add(null, false, 3, new int[]{4}, "prefix-list-from", "filter prefixes from this area");
        l.add(null, false, 4, new int[]{-1}, "<name:pl>", "name of prefix list");
        l.add(null, false, 3, new int[]{4}, "prefix-list-into", "filter prefixes into this area");
        l.add(null, false, 4, new int[]{-1}, "<name:pl>", "name of prefix list");
        l.add(null, false, 1, new int[]{2}, "distance", "specify default distance");
        l.add(null, false, 2, new int[]{3}, "<num>", "intra-area distance");
        l.add(null, false, 3, new int[]{4}, "<num>", "inter-area distance");
        l.add(null, false, 4, new int[]{-1}, "<num>", "external distance");
        l.add(null, false, 1, new int[]{2}, "flexalgo", "flexalgo parameters");
        l.add(null, false, 2, new int[]{3}, "<num>", "algorithm id");
        l.add(null, false, 3, new int[]{-1}, "<name:vrf>", "vrf to use");
    }

    /**
     * get config
     *
     * @param l list
     * @param beg beginning
     * @param filter filter
     */
    public void routerGetConfig(List<String> l, String beg, int filter) {
        l.add(beg + "router-id " + routerID);
        l.add(beg + "traffeng-id " + traffEngID);
        String a = "";
        if (segrouBase != 0) {
            a += " base " + segrouBase;
        }
        cmds.cfgLine(l, segrouMax < 1, beg, "segrout", "" + segrouMax + a);
        cmds.cfgLine(l, bierMax < 1, beg, "bier", bierLen + " " + bierMax);
        for (int i = 0; i < areas.size(); i++) {
            rtrOspf6area ntry = areas.get(i);
            String s = "area " + ntry.area + " ";
            l.add(beg + s + "enable");
            cmds.cfgLine(l, !ntry.haMode, beg, s + "ha-mode", "");
            l.add(beg + s + "spf-log " + ntry.lastSpf.logSize);
            cmds.cfgLine(l, ntry.lastSpf.topoLog.get() == 0, beg, s + "spf-topolog", ntry.lastSpf.getTopoLogMode());
            cmds.cfgLine(l, ntry.lastSpf.bidir.get() == 0, beg, s + "spf-bidir", "");
            cmds.cfgLine(l, ntry.lastSpf.hops.get() == 0, beg, s + "spf-hops", "");
            cmds.cfgLine(l, ntry.lastSpf.ecmp.get() == 0, beg, s + "spf-ecmp", "");
            cmds.cfgLine(l, !ntry.maxMetric, beg, s + "max-metric", "");
            cmds.cfgLine(l, !ntry.stub, beg, s + "stub", "");
            cmds.cfgLine(l, !ntry.nssa, beg, s + "nssa", "");
            cmds.cfgLine(l, !ntry.traffEng, beg, s + "traffeng", "");
            cmds.cfgLine(l, !ntry.segrouEna, beg, s + "segrout", "");
            cmds.cfgLine(l, !ntry.srv6ena, beg, s + "srv6", "");
            cmds.cfgLine(l, !ntry.bierEna, beg, s + "bier", "");
            cmds.cfgLine(l, !ntry.suppressAddr, beg, s + "suppress-prefix", "");
            cmds.cfgLine(l, !ntry.hostname, beg, s + "hostname", "");
            cmds.cfgLine(l, !ntry.defOrigin, beg, s + "default-originate", "");
            cmds.cfgLine(l, ntry.prflstFrom == null, beg, s + "prefix-list-from", "" + ntry.prflstFrom);
            cmds.cfgLine(l, ntry.prflstInto == null, beg, s + "prefix-list-into", "" + ntry.prflstInto);
            cmds.cfgLine(l, ntry.roumapFrom == null, beg, s + "route-map-from", "" + ntry.roumapFrom);
            cmds.cfgLine(l, ntry.roumapInto == null, beg, s + "route-map-into", "" + ntry.roumapInto);
            cmds.cfgLine(l, ntry.roupolFrom == null, beg, s + "route-policy-from", "" + ntry.roupolFrom);
            cmds.cfgLine(l, ntry.roupolInto == null, beg, s + "route-policy-into", "" + ntry.roupolInto);
        }
        for (int i = 0; i < srv6.size(); i++) {
            l.add(beg + "srv6 " + srv6.get(i).name);
        }
        l.add(beg + "distance " + distantInt + " " + distantSum + " " + distantExt);
        for (int i = 0; i < algos.size(); i++) {
            l.add(beg + "flexalgo " + algos.get(i));
        }
    }

    /**
     * configure
     *
     * @param cmd command
     * @return false if success, true if error
     */
    public boolean routerConfigure(cmds cmd) {
        String s = cmd.word();
        if (s.equals("router-id")) {
            s = cmd.word();
            routerID.fromString(s);
            cfgIfc ifc = cfgAll.ifcFind(s, 0);
            if (ifc != null) {
                if (ifc.addr4 != null) {
                    routerID.setAddr(ifc.addr4);
                }
            }
            genLsas(3);
            return false;
        }
        if (s.equals("flexalgo")) {
            int i = bits.str2num(cmd.word());
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                cmd.error("no such vrf");
                return false;
            }
            rtrAlgo alg = new rtrAlgo(i, vrf.fwd6, routerProtoTyp, routerProcNum);
            algos.add(alg);
            alg.vrf.register2ip();
            genLsas(3);
            return false;
        }
        if (s.equals("traffeng-id")) {
            traffEngID.fromString(cmd.word());
            genLsas(3);
            return false;
        }
        if (s.equals("srv6")) {
            cfgIfc ntry = cfgAll.ifcFind(cmd.word(), 0);
            if (ntry == null) {
                cmd.error("no such interface");
                return false;
            }
            srv6.put(ntry);
            genLsas(3);
            return false;
        }
        if (s.equals("segrout")) {
            tabLabel.release(segrouLab, tabLabelEntry.owner.ospf6srgb);
            segrouMax = bits.str2num(cmd.word());
            segrouBase = 0;
            for (;;) {
                s = cmd.word();
                if (s.length() < 1) {
                    break;
                }
                if (s.equals("base")) {
                    segrouBase = bits.str2num(cmd.word());
                    continue;
                }
            }
            segrouLab = tabLabel.allocate(tabLabelEntry.owner.ospf6srgb, segrouBase, segrouMax);
            genLsas(3);
            return false;
        }
        if (s.equals("bier")) {
            tabLabel.release(bierLab, tabLabelEntry.owner.ospf6bier);
            bierLen = tabLabelBier.normalizeBsl(bits.str2num(cmd.word()));
            bierMax = bits.str2num(cmd.word());
            bierLab = tabLabel.allocate(tabLabelEntry.owner.ospf6bier, (bierMax + bierLen - 1) / bierLen);
            genLsas(3);
            return false;
        }
        if (s.equals("distance")) {
            distantInt = bits.str2num(cmd.word());
            distantSum = bits.str2num(cmd.word());
            distantExt = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("area")) {
            rtrOspf6area dat = new rtrOspf6area(this, bits.str2num(cmd.word()));
            s = cmd.word();
            if (s.equals("enable")) {
                rtrOspf6area old = areas.add(dat);
                if (old != null) {
                    cmd.error("area already exists");
                    return false;
                }
                dat.startNow();
                dat.schedWork(7);
                return false;
            }
            dat = areas.find(dat);
            if (dat == null) {
                cmd.error("area not exists");
                return false;
            }
            if (s.equals("ha-mode")) {
                dat.haMode = true;
                return false;
            }
            if (s.equals("spf-log")) {
                dat.lastSpf.logSize.set(bits.str2num(cmd.word()));
                return false;
            }
            if (s.equals("spf-topolog")) {
                dat.lastSpf.setTopoLogMode(cmd);
                return false;
            }
            if (s.equals("spf-bidir")) {
                dat.lastSpf.bidir.set(1);
                dat.schedWork(3);
                return false;
            }
            if (s.equals("spf-hops")) {
                dat.lastSpf.hops.set(1);
                dat.schedWork(3);
                return false;
            }
            if (s.equals("spf-ecmp")) {
                dat.lastSpf.ecmp.set(1);
                dat.schedWork(3);
                return false;
            }
            if (s.equals("max-metric")) {
                dat.maxMetric = true;
                dat.schedWork(3);
                return false;
            }
            if (s.equals("stub")) {
                dat.stub = true;
                dat.nssa = false;
                dat.schedWork(3);
                return false;
            }
            if (s.equals("nssa")) {
                dat.stub = false;
                dat.nssa = true;
                dat.schedWork(3);
                return false;
            }
            if (s.equals("traffeng")) {
                dat.traffEng = true;
                dat.schedWork(3);
                return false;
            }
            if (s.equals("segrout")) {
                dat.segrouEna = true;
                dat.schedWork(3);
                return false;
            }
            if (s.equals("srv6")) {
                dat.srv6ena = true;
                dat.schedWork(3);
                return false;
            }
            if (s.equals("bier")) {
                dat.bierEna = true;
                dat.schedWork(3);
                return false;
            }
            if (s.equals("suppress-prefix")) {
                dat.suppressAddr = true;
                dat.schedWork(3);
                return false;
            }
            if (s.equals("hostname")) {
                dat.hostname = true;
                dat.schedWork(3);
                return false;
            }
            if (s.equals("default-originate")) {
                dat.defOrigin = true;
                dat.schedWork(3);
                return false;
            }
            if (s.equals("prefix-list-from")) {
                cfgPrfxlst ntry = cfgAll.prfxFind(cmd.word(), false);
                if (ntry == null) {
                    cmd.error("no such prefix list");
                    return false;
                }
                dat.prflstFrom = ntry.prflst;
                dat.schedWork(7);
                return false;
            }
            if (s.equals("prefix-list-into")) {
                cfgPrfxlst ntry = cfgAll.prfxFind(cmd.word(), false);
                if (ntry == null) {
                    cmd.error("no such prefix list");
                    return false;
                }
                dat.prflstInto = ntry.prflst;
                dat.schedWork(3);
                return false;
            }
            if (s.equals("route-map-from")) {
                cfgRoump ntry = cfgAll.rtmpFind(cmd.word(), false);
                if (ntry == null) {
                    cmd.error("no such route map");
                    return false;
                }
                dat.roumapFrom = ntry.roumap;
                dat.schedWork(7);
                return false;
            }
            if (s.equals("route-map-into")) {
                cfgRoump ntry = cfgAll.rtmpFind(cmd.word(), false);
                if (ntry == null) {
                    cmd.error("no such route map");
                    return false;
                }
                dat.roumapInto = ntry.roumap;
                dat.schedWork(3);
                return false;
            }
            if (s.equals("route-policy-from")) {
                cfgRouplc ntry = cfgAll.rtplFind(cmd.word(), false);
                if (ntry == null) {
                    cmd.error("no such route policy");
                    return false;
                }
                dat.roupolFrom = ntry.rouplc;
                dat.schedWork(7);
                return false;
            }
            if (s.equals("route-policy-into")) {
                cfgRouplc ntry = cfgAll.rtplFind(cmd.word(), false);
                if (ntry == null) {
                    cmd.error("no such route policy");
                    return false;
                }
                dat.roupolInto = ntry.rouplc;
                dat.schedWork(3);
                return false;
            }
            return false;
        }
        if (!s.equals(cmds.negated)) {
            return true;
        }
        s = cmd.word();
        if (s.equals("flexalgo")) {
            int i = bits.str2num(cmd.word());
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                cmd.error("no such vrf");
                return false;
            }
            rtrAlgo alg = new rtrAlgo(i, vrf.fwd6, routerProtoTyp, routerProcNum);
            alg = algos.del(alg);
            if (alg == null) {
                return false;
            }
            alg.vrf.unregister2ip();
            genLsas(3);
            return false;
        }
        if (s.equals("srv6")) {
            cfgIfc ntry = cfgAll.ifcFind(cmd.word(), 0);
            if (ntry == null) {
                cmd.error("no such interface");
                return false;
            }
            srv6.del(ntry);
            genLsas(3);
            return false;
        }
        if (s.equals("segrout")) {
            tabLabel.release(segrouLab, tabLabelEntry.owner.ospf6srgb);
            segrouLab = null;
            segrouMax = 0;
            segrouBase = 0;
            genLsas(3);
            return false;
        }
        if (s.equals("bier")) {
            tabLabel.release(bierLab, tabLabelEntry.owner.ospf6bier);
            bierLab = null;
            bierLen = 0;
            bierMax = 0;
            genLsas(3);
            return false;
        }
        if (s.equals("area")) {
            rtrOspf6area dat = new rtrOspf6area(this, bits.str2num(cmd.word()));
            dat = areas.find(dat);
            if (dat == null) {
                cmd.error("area not exists");
                return false;
            }
            s = cmd.word();
            if (s.equals("enable")) {
                dat.stopNow();
                areas.del(dat);
                genLsas(3);
                return false;
            }
            if (s.equals("ha-mode")) {
                dat.haMode = false;
                return false;
            }
            if (s.equals("spf-log")) {
                dat.lastSpf.logSize.set(0);
                return false;
            }
            if (s.equals("spf-topolog")) {
                dat.lastSpf.topoLog.set(0);
                return false;
            }
            if (s.equals("spf-bidir")) {
                dat.lastSpf.bidir.set(0);
                dat.schedWork(3);
                return false;
            }
            if (s.equals("spf-hops")) {
                dat.lastSpf.hops.set(0);
                dat.schedWork(3);
                return false;
            }
            if (s.equals("spf-ecmp")) {
                dat.lastSpf.ecmp.set(0);
                dat.schedWork(3);
                return false;
            }
            if (s.equals("max-metric")) {
                dat.maxMetric = false;
                dat.schedWork(3);
                return false;
            }
            if (s.equals("stub")) {
                dat.stub = false;
                dat.schedWork(3);
                return false;
            }
            if (s.equals("nssa")) {
                dat.nssa = false;
                dat.schedWork(3);
                return false;
            }
            if (s.equals("traffeng")) {
                dat.traffEng = false;
                dat.schedWork(3);
                return false;
            }
            if (s.equals("segrout")) {
                dat.segrouEna = false;
                dat.schedWork(3);
                return false;
            }
            if (s.equals("srv6")) {
                dat.srv6ena = false;
                dat.schedWork(3);
                return false;
            }
            if (s.equals("bier")) {
                dat.bierEna = false;
                dat.schedWork(3);
                return false;
            }
            if (s.equals("suppress-prefix")) {
                dat.suppressAddr = false;
                dat.schedWork(3);
                return false;
            }
            if (s.equals("hostname")) {
                dat.hostname = false;
                dat.schedWork(3);
                return false;
            }
            if (s.equals("default-originate")) {
                dat.defOrigin = false;
                dat.schedWork(3);
                return false;
            }
            if (s.equals("prefix-list-from")) {
                dat.prflstFrom = null;
                dat.schedWork(7);
                return false;
            }
            if (s.equals("prefix-list-into")) {
                dat.prflstInto = null;
                dat.schedWork(3);
                return false;
            }
            if (s.equals("route-map-from")) {
                dat.roumapFrom = null;
                dat.schedWork(7);
                return false;
            }
            if (s.equals("route-map-into")) {
                dat.roumapInto = null;
                dat.schedWork(3);
                return false;
            }
            if (s.equals("route-policy-from")) {
                dat.roupolFrom = null;
                dat.schedWork(7);
                return false;
            }
            if (s.equals("route-policy-into")) {
                dat.roupolInto = null;
                dat.schedWork(3);
                return false;
            }
            return false;
        }
        return true;
    }

    /**
     * stop work
     */
    public void routerCloseNow() {
        for (int i = 0; i < areas.size(); i++) {
            rtrOspf6area ntry = areas.get(i);
            if (ntry == null) {
                continue;
            }
            ntry.stopNow();
        }
        for (int i = 0; i < ifaces.size(); i++) {
            rtrOspf6iface ntry = ifaces.get(i);
            if (ntry == null) {
                continue;
            }
            ntry.restartTimer(true);
            ntry.unregister2ip();
            ntry.closeNeighbors(true);
        }
        tabLabel.release(segrouLab, tabLabelEntry.owner.ospf6srgb);
        tabLabel.release(bierLab, tabLabelEntry.owner.ospf6bier);
    }

    /**
     * generate lsas in all areas
     *
     * @param todo todo to pass
     */
    protected void genLsas(int todo) {
        todo &= 3;
        for (int i = 0; i < areas.size(); i++) {
            rtrOspf6area ntry = areas.get(i);
            if (ntry == null) {
                continue;
            }
            ntry.schedWork(todo);
        }
    }

    /**
     * add ospf interface
     *
     * @param iface forwarding interface
     * @return interface handler
     */
    public rtrOspf6iface addInterface(ipFwdIface iface) {
        if (iface == null) {
            return null;
        }
        rtrOspf6area ara = areas.get(0);
        if (ara == null) {
            return null;
        }
        rtrOspf6iface ifc = new rtrOspf6iface(this, ara, iface);
        ifc.locInt = intIds++;
        rtrOspf6iface old = ifaces.add(ifc);
        if (old != null) {
            return old;
        }
        ifc.register2ip();
        ifc.restartTimer(false);
        ara.schedWork(7);
        return ifc;
    }

    /**
     * delete ospf interface
     *
     * @param iface forwarding interface
     */
    public void delInterface(ipFwdIface iface) {
        rtrOspf6iface ifc = new rtrOspf6iface(this, null, iface);
        ifc = ifaces.del(ifc);
        if (ifc == null) {
            return;
        }
        ifc.closeUp(ifc.iface);
        ifc.schedWork(7);
    }

    /**
     * check if i am area border
     *
     * @return true if yes, false if no
     */
    protected boolean amIabr() {
        return areas.size() > 1;
    }

    /**
     * list neighbors
     *
     * @param brief only briefly
     * @return list of neighbors
     */
    public userFormat showNeighs(boolean brief) {
        userFormat l;
        if (brief) {
            l = new userFormat("|", "area|routerid|state|uptime");
        } else {
            l = new userFormat("|", "interface|area|address|routerid|state|uptime");
        }
        for (int o = 0; o < ifaces.size(); o++) {
            rtrOspf6iface ifc = ifaces.get(o);
            if (ifc == null) {
                continue;
            }
            for (int i = 0; i < ifc.neighs.size(); i++) {
                rtrOspf6neigh nei = ifc.neighs.get(i);
                if (nei == null) {
                    continue;
                }
                if (brief) {
                    l.add(nei.area.area + "|" + nei.rtrID + "|" + nei.status2string() + "|" + bits.timePast(nei.upTime));
                } else {
                    l.add(ifc.iface + "|" + nei.area.area + "|" + nei.peer + "|" + nei.rtrID + "|" + nei.status2string() + "|" + bits.timePast(nei.upTime));
                }
            }
        }
        return l;
    }

    /**
     * list of neighbors
     *
     * @return list
     */
    public userFormat showMetrics() {
        userFormat l = new userFormat("|", "interface|area|address|routerid|metric|delay");
        for (int o = 0; o < ifaces.size(); o++) {
            rtrOspf6iface ifc = ifaces.get(o);
            if (ifc == null) {
                continue;
            }
            for (int i = 0; i < ifc.neighs.size(); i++) {
                rtrOspf6neigh nei = ifc.neighs.get(i);
                if (nei == null) {
                    continue;
                }
                l.add(ifc.iface + "|" + nei.area.area + "|" + nei.peer + "|" + nei.rtrID + "|" + nei.getMetric() + "|" + nei.echoCalc);
            }
        }
        return l;
    }

    /**
     * list interfaces
     *
     * @return list of interfaces
     */
    public userFormat showIfaces() {
        userFormat l = new userFormat("|", "interface|neighbors");
        for (int i = 0; i < ifaces.size(); i++) {
            rtrOspf6iface ifc = ifaces.get(i);
            l.add(ifc.iface + "|" + ifc.neighs.size());
        }
        return l;
    }

    /**
     * list of algorithms
     *
     * @param area area number
     * @return list
     */
    public userFormat showAlgorithms(int area) {
        rtrOspf6area ara = new rtrOspf6area(this, area);
        ara = areas.find(ara);
        if (ara == null) {
            return null;
        }
        return ara.lastSpf.listAlgorithm();
    }

    /**
     * list database
     *
     * @param area area number
     * @param cmd entry to find
     * @return list of entry
     */
    public List<String> showDatabase(int area, cmds cmd) {
        List<String> l = new ArrayList<String>();
        rtrOspf6area ara = new rtrOspf6area(this, area);
        ara = areas.find(ara);
        if (ara == null) {
            return l;
        }
        addrIPv4 ned1 = new addrIPv4();
        ned1.fromString(cmd.word());
        int ned2 = bits.str2num(cmd.word());
        for (int i = 0; i < ara.lsas.size(); i++) {
            rtrOspf6lsa ntry = ara.lsas.get(i);
            if (ntry == null) {
                continue;
            }
            if (ned2 != ntry.lsaID) {
                continue;
            }
            if (ned1.compareTo(ntry.rtrID) != 0) {
                continue;
            }
            l.add("" + ntry);
            packHolder pck = new packHolder(true, true);
            pck.putSkip(ntry.writeData(pck, 0, true));
            pck.merge2beg();
            enc7bit.buf2hex(l, pck.getCopy(), 0, "");
            rtrOspfDump.dump6lsa(l, pck, ntry);
        }
        return l;
    }

    /**
     * list database
     *
     * @param area area number
     * @return list of database
     */
    public userFormat showDatabase(int area) {
        userFormat l = new userFormat("|", "routerid|lsaid|sequence|type|len|time");
        rtrOspf6area ara = new rtrOspf6area(this, area);
        ara = areas.find(ara);
        if (ara == null) {
            return l;
        }
        for (int i = 0; i < ara.lsas.size(); i++) {
            rtrOspf6lsa ntry = ara.lsas.get(i);
            if (ntry == null) {
                continue;
            }
            l.add("" + ntry);
        }
        return l;
    }

    /**
     * list routes
     *
     * @param area area number
     * @return list of routes
     */
    public tabRoute<addrIP> showRoute(int area) {
        rtrOspf6area ara = new rtrOspf6area(this, area);
        ara = areas.find(ara);
        if (ara == null) {
            return new tabRoute<addrIP>("empty");
        }
        return ara.routes;
    }

    /**
     * show spf
     *
     * @param area area number
     * @return log of spf
     */
    public userFormat showSpfStat(int area) {
        rtrOspf6area ara = new rtrOspf6area(this, area);
        ara = areas.find(ara);
        if (ara == null) {
            return null;
        }
        return ara.lastSpf.listStatistics();
    }

    /**
     * show spf
     *
     * @param area area number
     * @param cmd entry to find
     * @return log of spf
     */
    public userFormat showSpfTopo(int area, cmds cmd) {
        rtrOspf6area ara = new rtrOspf6area(this, area);
        ara = areas.find(ara);
        if (ara == null) {
            return null;
        }
        return ara.lastSpf.listTopology(new rtrOspf6areaSpf(new addrIPv4(), 0), cmd);
    }

    /**
     * show log
     *
     * @param area area number
     * @return log of spf
     */
    public userFormat showSpfLog(int area) {
        rtrOspf6area ara = new rtrOspf6area(this, area);
        ara = areas.find(ara);
        if (ara == null) {
            return null;
        }
        return ara.lastSpf.listUsages();
    }

    /**
     * show tree
     *
     * @param area area number
     * @param cmd entry to find
     * @return tree of spf
     */
    public List<String> showSpfTree(int area, cmds cmd) {
        rtrOspf6area ara = new rtrOspf6area(this, area);
        ara = areas.find(ara);
        if (ara == null) {
            return new ArrayList<String>();
        }
        return ara.lastSpf.listTree(cmd);
    }

    /**
     * show tree
     *
     * @param area area number
     * @param cmd entry to find
     * @return tree of spf
     */
    public List<String> showSpfOtherTree(int area, cmds cmd) {
        rtrOspf6area ara = new rtrOspf6area(this, area);
        ara = areas.find(ara);
        if (ara == null) {
            return new ArrayList<String>();
        }
        spfCalc<rtrOspf6areaSpf> spf = ara.lastSpf.copyBytes();
        rtrOspf6areaSpf ned = new rtrOspf6areaSpf(new addrIPv4(), 0);
        ned.fromString(cmd.word());
        spf.doWork(ned);
        return spf.listTree(cmd);
    }

    /**
     * show topology
     *
     * @param area area number
     * @param cmd entry to find
     * @return log of spf
     */
    public userFormat showSpfOtherTopo(int area, cmds cmd) {
        rtrOspf6area ara = new rtrOspf6area(this, area);
        ara = areas.find(ara);
        if (ara == null) {
            return null;
        }
        spfCalc<rtrOspf6areaSpf> spf = ara.lastSpf.copyBytes();
        rtrOspf6areaSpf ned = new rtrOspf6areaSpf(new addrIPv4(), 0);
        ned.fromString(cmd.word());
        spf.doWork(ned);
        return spf.listTopology(new rtrOspf6areaSpf(new addrIPv4(), 0), cmd);
    }

    /**
     * show graph
     *
     * @param area area number
     * @param cmd masks
     * @return graph of spf
     */
    public List<String> showSpfGraph(int area, cmds cmd) {
        rtrOspf6area ara = new rtrOspf6area(this, area);
        ara = areas.find(ara);
        if (ara == null) {
            return new ArrayList<String>();
        }
        return ara.lastSpf.listGraphviz(cmd);
    }

    /**
     * show nh inconsistency
     *
     * @param area area number
     * @param mtch matcher
     * @return inconsistency list
     */
    public userFormat showNhIncons(int area, tabIntMatcher mtch) {
        rtrOspf6area ara = new rtrOspf6area(this, area);
        ara = areas.find(ara);
        if (ara == null) {
            return null;
        }
        return ara.lastSpf.listNhIncons(mtch);
    }

    /**
     * show met inconsistency
     *
     * @param area area number
     * @param mtch matcher
     * @return inconsistency list
     */
    public userFormat showMetIncons(int area, tabIntMatcher mtch) {
        rtrOspf6area ara = new rtrOspf6area(this, area);
        ara = areas.find(ara);
        if (ara == null) {
            return null;
        }
        return ara.lastSpf.listMetIncons(mtch);
    }

    /**
     * show non redundant nodes
     *
     * @param area area number
     * @return necessity list
     */
    public userFormat showNonRedundant(int area) {
        rtrOspf6area ara = new rtrOspf6area(this, area);
        ara = areas.find(ara);
        if (ara == null) {
            return null;
        }
        return ara.lastSpf.listNonRedundant();
    }

    /**
     * show hostnames
     *
     * @param area area number
     * @return names list
     */
    public userFormat showHostnames(int area) {
        rtrOspf6area ara = new rtrOspf6area(this, area);
        ara = areas.find(ara);
        if (ara == null) {
            return null;
        }
        return ara.lastSpf.listHostnames();
    }

    /**
     * find neighbor
     *
     * @param area area
     * @param adr address
     * @return neighbor, null if not found
     */
    public rtrOspf6neigh findPeer(int area, addrIP adr) {
        rtrOspf6area ara = new rtrOspf6area(this, area);
        ara = areas.find(ara);
        if (ara == null) {
            return null;
        }
        for (int i = 0; i < ifaces.size(); i++) {
            rtrOspf6iface ifc = ifaces.get(i);
            rtrOspf6neigh nei = new rtrOspf6neigh(this, ara, ifc, adr.toIPv6());
            nei = ifc.neighs.find(nei);
            if (nei != null) {
                return nei;
            }
        }
        return null;
    }

    /**
     * get neighbor count
     *
     * @return count
     */
    public int routerNeighCount() {
        int o = 0;
        for (int i = 0; i < ifaces.size(); i++) {
            o += ifaces.get(i).neighs.size();
        }
        return o;
    }

    /**
     * list neighbors
     *
     * @param tab list
     */
    public void routerNeighList(tabRoute<addrIP> tab) {
        for (int o = 0; o < ifaces.size(); o++) {
            rtrOspf6iface ifc = ifaces.get(o);
            if (ifc == null) {
                continue;
            }
            if (ifc.iface.lower.getState() != state.states.up) {
                continue;
            }
            for (int i = 0; i < ifc.neighs.size(); i++) {
                rtrOspf6neigh nei = ifc.neighs.get(i);
                if (nei == null) {
                    continue;
                }
                addrIP adr = new addrIP();
                adr.fromIPv6addr(nei.peer);
                tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
                ntry.prefix = new addrPrefix<addrIP>(adr, addrIP.size * 8);
                tabRoute.addUpdatedEntry(tabRoute.addType.better, tab, rtrBgpUtil.sfiUnicast, 0, ntry, true, null, null, routerAutoMesh);
            }
        }
    }

    /**
     * get interface count
     *
     * @return count
     */
    public int routerIfaceCount() {
        return ifaces.size();
    }

    /**
     * maximum recursion depth
     *
     * @return allowed number
     */
    public int routerRecursions() {
        return 1;
    }

    /**
     * get list of link states
     *
     * @param tab table to update
     * @param area area number
     * @param asn asn
     * @param adv advertiser
     */
    public void routerLinkStates(tabRoute<addrIP> tab, int area, int asn, addrIPv4 adv) {
        rtrOspf6area ara = new rtrOspf6area(this, area);
        ara = areas.find(ara);
        if (ara == null) {
            return;
        }
        ara.lastSpf.listLinkStates(tab, spfLnkst.protoOspfV3, ara.area, asn, adv, addrIPv4.size, 2);
    }

    /**
     * get state information
     *
     * @param lst list to append
     */
    public void routerStateGet(List<String> lst) {
        String a = routerGetName() + " ";
        for (int i = 0; i < areas.size(); i++) {
            rtrOspf6area dat = areas.get(i);
            if (dat == null) {
                continue;
            }
            if (!dat.haMode) {
                continue;
            }
            dat.stateGet(lst, a);
        }
    }

    /**
     * set state information
     *
     * @param cmd string to append
     * @return true on error, false on success
     */
    public boolean routerStateSet(cmds cmd) {
        rtrOspf6area ara = new rtrOspf6area(this, bits.str2num(cmd.word()));
        ara = areas.find(ara);
        if (ara == null) {
            return true;
        }
        return ara.stateSet(cmd);
    }

}
