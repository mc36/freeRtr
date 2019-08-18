package rtr;

import addr.addrIP;
import addr.addrIPv4;
import addr.addrPrefix;
import cfg.cfgAll;
import cfg.cfgPrfxlst;
import cfg.cfgRoump;
import cfg.cfgRouplc;
import ip.ipFwd;
import ip.ipFwdIface;
import ip.ipRtr;
import java.util.ArrayList;
import java.util.List;
import pack.packHolder;
import tab.tabGen;
import tab.tabLabel;
import tab.tabLabelBier;
import tab.tabLabelNtry;
import tab.tabRoute;
import tab.tabRouteEntry;
import user.userFlash;
import user.userFormat;
import user.userHelping;
import util.bits;
import util.cmds;
import util.debugger;
import util.logger;

/**
 * open shortest path first (rfc2328) protocol v2
 *
 * @author matecsaba
 */
public class rtrOspf4 extends ipRtr {

    /**
     * protocol number
     */
    public final static int protoNum = 89;

    /**
     * protocol version number
     */
    public final static int verNum = 2;

    /**
     * protocol header size
     */
    public final static int sizeHead = 24;

    /**
     * router id
     */
    public addrIPv4 routerID;

    /**
     * traffic engineering id
     */
    public addrIPv4 traffEngID;

    /**
     * segment routing maximum
     */
    public int segrouMax = 0;

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
     * list of interfaces
     */
    protected tabGen<rtrOspf4iface> ifaces;

    /**
     * list of areas
     */
    protected tabGen<rtrOspf4area> areas;

    /**
     * segment routing labels
     */
    protected tabLabelNtry[] segrouLab;

    /**
     * bier labels
     */
    protected tabLabelNtry[] bierLab;

    /**
     * create one ospf process
     *
     * @param forwarder the ip protocol
     * @param id process id
     */
    public rtrOspf4(ipFwd forwarder, int id) {
        fwdCore = forwarder;
        ifaces = new tabGen<rtrOspf4iface>();
        areas = new tabGen<rtrOspf4area>();
        routerID = new addrIPv4();
        traffEngID = new addrIPv4();
        distantExt = 110;
        distantInt = 110;
        distantSum = 110;
        routerCreateComputed();
        fwdCore.routerAdd(this, tabRouteEntry.routeType.ospf4, id);
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
        if (debugger.rtrOspf4evnt) {
            logger.debug("create table");
        }
        tabRoute<addrIP> tab = new tabRoute<addrIP>("ospf");
        for (int i = 0; i < areas.size(); i++) {
            rtrOspf4area ntry = areas.get(i);
            if (ntry == null) {
                continue;
            }
            tab.mergeFrom(tabRoute.addType.better, ntry.routes, null, true, tabRouteEntry.distanLim);
        }
        if (segrouLab != null) {
            for (int o = 0; o < segrouLab.length; o++) {
                boolean b = false;
                for (int i = 0; i < areas.size(); i++) {
                    rtrOspf4area ntry = areas.get(i);
                    if (ntry == null) {
                        continue;
                    }
                    if (ntry.segrouUsd == null) {
                        continue;
                    }
                    b |= ntry.segrouUsd[o];
                }
                if (!b) {
                    segrouLab[o].setFwdDrop(8);
                }
            }
        }
        if (bierLab != null) {
            int o = 0;
            for (int i = 0; i < ifaces.size(); i++) {
                rtrOspf4iface ifc = ifaces.get(i);
                if (ifc == null) {
                    continue;
                }
                if (ifc.brIndex < 1) {
                    continue;
                }
                o = ifc.brIndex;
                break;
            }
            tabLabelBier res = new tabLabelBier();
            res.base = bierLab[0].getValue();
            res.fwdr = fwdCore;
            res.bsl = tabLabelBier.num2bsl(bierLen);
            res.idx = o;
            for (int i = 0; i < areas.size(); i++) {
                rtrOspf4area ntry = areas.get(i);
                if (ntry == null) {
                    continue;
                }
                res.mergeFrom(ntry.bierRes);
            }
            for (int i = 0; i < bierLab.length; i++) {
                bierLab[i].setBierMpls(20, fwdCore, res);
            }
        }
        routerComputedU = tab;
        fwdCore.routerChg(this);
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
    public void routerGetHelp(userHelping l) {
        l.add("1 2   router-id                   specify router id");
        l.add("2 .     <addr>                    router id");
        l.add("1 2   traffeng-id                 specify traffic engineering id");
        l.add("2 .     <addr>                    te id");
        l.add("1 2   segrout                     segment routing parameters");
        l.add("2 .     <num>                     maximum index");
        l.add("1 2   bier                        bier parameters");
        l.add("2 3     <num>                     bitstring length");
        l.add("3 .       <num>                   maximum index");
        l.add("1 2   area                        configure one area");
        l.add("2 3     <num>                     area number");
        l.add("3 .       enable                  create this area");
        l.add("3 .       stub                    configure as stub");
        l.add("3 .       nssa                    configure as nssa");
        l.add("3 .       traffeng                configure for traffic engineering");
        l.add("3 .       segrout                 configure for segment routing");
        l.add("3 .       bier                    configure for bier");
        l.add("3 .       hostname                advertise hostname");
        l.add("3 .       default-originate       advertise default route");
        l.add("3 4       route-map-from          process prefixes from this area");
        l.add("4 .         <name>                name of route map");
        l.add("3 4       route-map-into          process prefixes into this area");
        l.add("4 .         <name>                name of route map");
        l.add("3 4       route-policy-from       process prefixes from this area");
        l.add("4 .         <name>                name of route policy");
        l.add("3 4       route-policy-into       process prefixes into this area");
        l.add("4 .         <name>                name of route policy");
        l.add("3 4       prefix-list-from        filter prefixes from this area");
        l.add("4 .         <name>                name of prefix list");
        l.add("3 4       prefix-list-into        filter prefixes into this area");
        l.add("4 .         <name>                name of prefix list");
        l.add("1 2   distance                    specify default distance");
        l.add("2 3     <num>                     intra-area distance");
        l.add("3 4       <num>                   inter-area distance");
        l.add("4 .         <num>                 external distance");
    }

    /**
     * get config
     *
     * @param l list
     * @param beg beginning
     * @param filter filter
     */
    public void routerGetConfig(List<String> l, String beg, boolean filter) {
        l.add(beg + "router-id " + routerID);
        l.add(beg + "traffeng-id " + traffEngID);
        cmds.cfgLine(l, segrouMax < 1, beg, "segrout", "" + segrouMax);
        cmds.cfgLine(l, bierMax < 1, beg, "bier", bierLen + " " + bierMax);
        for (int i = 0; i < areas.size(); i++) {
            rtrOspf4area ntry = areas.get(i);
            String s = "area " + ntry.area + " ";
            l.add(beg + s + "enable");
            cmds.cfgLine(l, !ntry.stub, beg, s + "stub", "");
            cmds.cfgLine(l, !ntry.nssa, beg, s + "nssa", "");
            cmds.cfgLine(l, !ntry.traffEng, beg, s + "traffeng", "");
            cmds.cfgLine(l, !ntry.segrouEna, beg, s + "segrout", "");
            cmds.cfgLine(l, !ntry.bierEna, beg, s + "bier", "");
            cmds.cfgLine(l, !ntry.hostname, beg, s + "hostname", "");
            cmds.cfgLine(l, !ntry.defOrigin, beg, s + "default-originate", "");
            cmds.cfgLine(l, ntry.prflstFrom == null, beg, s + "prefix-list-from", "" + ntry.prflstFrom);
            cmds.cfgLine(l, ntry.prflstInto == null, beg, s + "prefix-list-into", "" + ntry.prflstInto);
            cmds.cfgLine(l, ntry.roumapFrom == null, beg, s + "route-map-from", "" + ntry.roumapFrom);
            cmds.cfgLine(l, ntry.roumapInto == null, beg, s + "route-map-into", "" + ntry.roumapInto);
            cmds.cfgLine(l, ntry.roupolFrom == null, beg, s + "route-policy-from", "" + ntry.roupolFrom);
            cmds.cfgLine(l, ntry.roupolInto == null, beg, s + "route-policy-into", "" + ntry.roupolInto);
        }
        l.add(beg + "distance " + distantInt + " " + distantSum + " " + distantExt);
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
            routerID.fromString(cmd.word());
            genLsas(3);
            return false;
        }
        if (s.equals("traffeng-id")) {
            traffEngID.fromString(cmd.word());
            genLsas(3);
            return false;
        }
        if (s.equals("segrout")) {
            tabLabel.release(segrouLab, 8);
            segrouMax = bits.str2num(cmd.word());
            segrouLab = tabLabel.allocate(8, segrouMax);
            genLsas(3);
            return false;
        }
        if (s.equals("bier")) {
            tabLabel.release(bierLab, 20);
            bierLen = tabLabelBier.normalizeBsl(bits.str2num(cmd.word()));
            bierMax = bits.str2num(cmd.word());
            bierLab = tabLabel.allocate(20, (bierMax + bierLen - 1) / bierLen);
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
            rtrOspf4area dat = new rtrOspf4area(this, bits.str2num(cmd.word()));
            s = cmd.word();
            if (s.equals("enable")) {
                rtrOspf4area old = areas.add(dat);
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
            if (s.equals("bier")) {
                dat.bierEna = true;
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
        if (!s.equals("no")) {
            return true;
        }
        s = cmd.word();
        if (s.equals("segrout")) {
            tabLabel.release(segrouLab, 8);
            segrouLab = null;
            segrouMax = 0;
            genLsas(3);
            return false;
        }
        if (s.equals("bier")) {
            tabLabel.release(bierLab, 20);
            bierLab = null;
            bierLen = 0;
            bierMax = 0;
            genLsas(3);
            return false;
        }
        if (s.equals("area")) {
            rtrOspf4area dat = new rtrOspf4area(this, bits.str2num(cmd.word()));
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
            if (s.equals("bier")) {
                dat.bierEna = false;
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
            rtrOspf4area ntry = areas.get(i);
            if (ntry == null) {
                continue;
            }
            ntry.stopNow();
        }
        for (int i = 0; i < ifaces.size(); i++) {
            rtrOspf4iface ntry = ifaces.get(i);
            if (ntry == null) {
                continue;
            }
            ntry.restartTimer(true);
            ntry.unregister2ip();
            ntry.closeNeighbors(true);
        }
        tabLabel.release(segrouLab, 8);
        tabLabel.release(bierLab, 20);
    }

    /**
     * generate lsas in all areas
     *
     * @param todo todo to pass
     */
    protected void genLsas(int todo) {
        todo &= 3;
        for (int i = 0; i < areas.size(); i++) {
            rtrOspf4area ntry = areas.get(i);
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
    public rtrOspf4iface addInterface(ipFwdIface iface) {
        if (iface == null) {
            return null;
        }
        rtrOspf4area ara = areas.get(0);
        if (ara == null) {
            return null;
        }
        rtrOspf4iface ifc = new rtrOspf4iface(this, ara, iface);
        rtrOspf4iface old = ifaces.add(ifc);
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
        rtrOspf4iface ifc = new rtrOspf4iface(this, null, iface);
        ifc = ifaces.del(ifc);
        if (ifc == null) {
            return;
        }
        ifc.closeUp(ifc.iface);
        ifc.area.schedWork(7);
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
     * @return list of neighbors
     */
    public userFormat showNeighs() {
        userFormat l = new userFormat("|", "interface|address|routerid|uptime");
        for (int o = 0; o < ifaces.size(); o++) {
            rtrOspf4iface ifc = ifaces.get(o);
            if (ifc == null) {
                continue;
            }
            for (int i = 0; i < ifc.neighs.size(); i++) {
                rtrOspf4neigh nei = ifc.neighs.get(i);
                if (nei == null) {
                    continue;
                }
                l.add(ifc + "|" + nei.peer + "|" + nei.rtrID + "|" + bits.timePast(nei.upTime));
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
            rtrOspf4iface ifc = ifaces.get(i);
            l.add(ifc.iface + "|" + ifc.neighs.size());
        }
        return l;
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
        rtrOspf4area ara = new rtrOspf4area(this, area);
        ara = areas.find(ara);
        if (ara == null) {
            return l;
        }
        addrIPv4 ned1 = new addrIPv4();
        addrIPv4 ned2 = new addrIPv4();
        ned1.fromString(cmd.word());
        ned2.fromString(cmd.word());
        for (int i = 0; i < ara.lsas.size(); i++) {
            rtrOspf4lsa ntry = ara.lsas.get(i);
            if (ntry == null) {
                continue;
            }
            if (ned1.compare(ned1, ntry.rtrID) != 0) {
                continue;
            }
            if (ned2.compare(ned2, ntry.lsaID) != 0) {
                continue;
            }
            l.add("" + ntry);
            packHolder pck = new packHolder(true, true);
            pck.putSkip(ntry.writeData(pck, 0, true));
            pck.merge2beg();
            userFlash.buf2hex(l, pck.getCopy(), 0);
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
        rtrOspf4area ara = new rtrOspf4area(this, area);
        ara = areas.find(ara);
        if (ara == null) {
            return l;
        }
        for (int i = 0; i < ara.lsas.size(); i++) {
            rtrOspf4lsa ntry = ara.lsas.get(i);
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
        rtrOspf4area ara = new rtrOspf4area(this, area);
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
        rtrOspf4area ara = new rtrOspf4area(this, area);
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
        rtrOspf4area ara = new rtrOspf4area(this, area);
        ara = areas.find(ara);
        if (ara == null) {
            return null;
        }
        if (cmd.size() < 1) {
            return ara.lastSpf.listTopology();
        }
        addrIPv4 ned = new addrIPv4();
        ned.fromString(cmd.word());
        return ara.lastSpf.listTopology(ned);
    }

    /**
     * show log
     *
     * @param area area number
     * @return log of spf
     */
    public userFormat showSpfLog(int area) {
        rtrOspf4area ara = new rtrOspf4area(this, area);
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
     * @return tree of spf
     */
    public List<String> showSpfTree(int area) {
        rtrOspf4area ara = new rtrOspf4area(this, area);
        ara = areas.find(ara);
        if (ara == null) {
            return new ArrayList<String>();
        }
        return ara.lastSpf.listTree();
    }

    /**
     * show graph
     *
     * @param area area number
     * @return graph of spf
     */
    public List<String> showSpfGraph(int area) {
        rtrOspf4area ara = new rtrOspf4area(this, area);
        ara = areas.find(ara);
        if (ara == null) {
            return new ArrayList<String>();
        }
        return ara.lastSpf.listGraphviz();
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
            rtrOspf4iface ifc = ifaces.get(o);
            if (ifc == null) {
                continue;
            }
            for (int i = 0; i < ifc.neighs.size(); i++) {
                rtrOspf4neigh nei = ifc.neighs.get(i);
                if (nei == null) {
                    continue;
                }
                addrIP adr = new addrIP();
                adr.fromIPv4addr(nei.peer);
                tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
                ntry.prefix = new addrPrefix<addrIP>(adr, addrIP.size * 8);
                tabRoute.addUpdatedEntry(tabRoute.addType.better, tab, rtrBgpUtil.safiUnicast, ntry, null, null, routerAutoMesh);
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

}
