package rtr;

import addr.addrIP;
import addr.addrIPv4;
import addr.addrPrefix;
import cfg.cfgAll;
import cfg.cfgInit;
import cfg.cfgPrfxlst;
import cfg.cfgRoump;
import cfg.cfgRouplc;
import ip.ipCor4;
import ip.ipCor6;
import ip.ipFwd;
import ip.ipFwdIface;
import ip.ipMpls;
import ip.ipRtr;
import java.util.List;
import prt.prtTcp;
import prt.prtUdp;
import pack.packDnsRec;
import tab.tabGen;
import tab.tabLabel;
import tab.tabLabelBier;
import tab.tabLabelNtry;
import tab.tabListing;
import tab.tabPrfxlstN;
import tab.tabRoute;
import tab.tabRouteEntry;
import tab.tabRtrmapN;
import tab.tabRtrplcN;
import user.userFormat;
import user.userHelping;
import util.bits;
import util.cmds;
import util.debugger;
import util.logger;
import util.notifier;
import util.shrtPthFrst;
import util.state;
import util.syncInt;
import util.version;

/**
 * link state routing protocol
 *
 * @author matecsaba
 */
public class rtrLsrp extends ipRtr implements Runnable {

    /**
     * port number
     */
    public static final int port = 1678;

    /**
     * forwarding core
     */
    public final ipFwd fwdCore;

    /**
     * udp core
     */
    protected final prtUdp udpCore;

    /**
     * tcp core
     */
    protected final prtTcp tcpCore;

    /**
     * router id
     */
    public addrIPv4 routerID;

    /**
     * stub flag
     */
    public boolean stub;

    /**
     * suppress interface addresses
     */
    public boolean suppressAddr = false;

    /**
     * default distance
     */
    public int distance = 70;

    /**
     * data lifetime
     */
    public int lifetime = 3600000;

    /**
     * data refresh
     */
    public int refresh = 2400000;

    /**
     * advertise default route
     */
    public boolean defOrigin = false;

    /**
     * segment routing index
     */
    public int segrouIdx = 0;

    /**
     * segment routing maximum
     */
    public int segrouMax = 0;

    /**
     * segment routing base
     */
    public int segrouBase = 0;

    /**
     * segment routing pop
     */
    public boolean segrouPop = false;

    /**
     * bier index
     */
    public int bierIdx = 0;

    /**
     * bier length
     */
    public int bierLen = 0;

    /**
     * bier maximum
     */
    public int bierMax = 0;

    /**
     * ingress prefix list
     */
    public tabListing<tabPrfxlstN, addrIP> prflstIn;

    /**
     * ingress route map
     */
    public tabListing<tabRtrmapN, addrIP> roumapIn;

    /**
     * ingress route policy
     */
    public tabListing<tabRtrplcN, addrIP> roupolIn;

    /**
     * list of interfaces
     */
    protected tabGen<rtrLsrpIface> ifaces;

    /**
     * link state database
     */
    protected tabGen<rtrLsrpData> database;

    /**
     * segment routing labels
     */
    protected tabLabelNtry[] segrouLab;

    /**
     * bier labels
     */
    protected tabLabelNtry[] bierLab;

    /**
     * notified on route change
     */
    protected notifier notif = new notifier();

    /**
     * need to work
     */
    protected syncInt todo = new syncInt(0);

    /**
     * data changed
     */
    protected int changeNum = 0;

    /**
     * data changed
     */
    protected long changeTim = 0;

    /**
     * last spf
     */
    protected shrtPthFrst<addrIPv4> lastSpf;

    private boolean need2run = true;

    /**
     * create one lsrp process
     *
     * @param forwarder the ip protocol
     * @param udp the udp protocol
     * @param tcp the tcp protocol
     * @param id process id
     */
    public rtrLsrp(ipFwd forwarder, prtUdp udp, prtTcp tcp, int id) {
        fwdCore = forwarder;
        udpCore = udp;
        tcpCore = tcp;
        routerID = new addrIPv4();
        ifaces = new tabGen<rtrLsrpIface>();
        tabRouteEntry.routeType rouTyp = null;
        switch (fwdCore.ipVersion) {
            case ipCor4.protocolVersion:
                rouTyp = tabRouteEntry.routeType.lsrp4;
                break;
            case ipCor6.protocolVersion:
                rouTyp = tabRouteEntry.routeType.lsrp6;
                break;
            default:
                break;
        }
        database = new tabGen<rtrLsrpData>();
        lastSpf = new shrtPthFrst<addrIPv4>(null);
        routerCreateComputed();
        fwdCore.routerAdd(this, rouTyp, id);
        new Thread(this).start();
    }

    /**
     * convert to string
     *
     * @return string
     */
    public String toString() {
        return "lsrp on " + fwdCore;
    }

    /**
     * add one interface to work on
     *
     * @param ifc ip forwarder interface
     * @return false if successful, true if error happened
     */
    public rtrLsrpIface addInterface(ipFwdIface ifc) {
        if (debugger.rtrLsrpEvnt) {
            logger.debug("add iface " + ifc);
        }
        if (ifc == null) {
            return null;
        }
        rtrLsrpIface ntry = new rtrLsrpIface(this, ifc);
        rtrLsrpIface old = ifaces.add(ntry);
        if (old != null) {
            ntry = old;
        }
        ntry.register2udp();
        routerCreateComputed();
        return ntry;
    }

    /**
     * delete one interface
     *
     * @param ifc interface to delete
     */
    public void delInterface(ipFwdIface ifc) {
        if (debugger.rtrLsrpEvnt) {
            logger.debug("del iface " + ifc);
        }
        if (ifc == null) {
            return;
        }
        rtrLsrpIface ntry = new rtrLsrpIface(this, ifc);
        ntry = ifaces.del(ntry);
        if (ntry == null) {
            return;
        }
        ntry.routerCloseNow();
        routerCreateComputed();
    }

    /**
     * list of neighbors
     *
     * @return list
     */
    public userFormat showNeighs() {
        userFormat res = new userFormat("|", "iface|router|name|peerif|peer|ready|uptime");
        for (int i = 0; i < ifaces.size(); i++) {
            rtrLsrpIface ifc = ifaces.get(i);
            ifc.showNeighs(res);
        }
        return res;
    }

    /**
     * find neighbor
     *
     * @param adr address
     * @return neighbor, null if not found
     */
    public rtrLsrpNeigh findNeigh(addrIP adr) {
        for (int i = 0; i < ifaces.size(); i++) {
            rtrLsrpIface ifc = ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            rtrLsrpNeigh r = ifc.findNeigh(adr);
            if (r != null) {
                return r;
            }
        }
        return null;
    }

    /**
     * list interfaces
     *
     * @return list of interfaces
     */
    public userFormat showIfaces() {
        userFormat l = new userFormat("|", "interface|neighbors");
        for (int i = 0; i < ifaces.size(); i++) {
            rtrLsrpIface ifc = ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            l.add(ifc.iface + "|" + ifc.neighs.size());
        }
        return l;
    }

    /**
     * list database
     *
     * @param mod mode: 1=summary, 2=uptime, 3=software
     * @return list of database
     */
    public userFormat showDatabase(int mod) {
        userFormat l;
        switch (mod) {
            case 1:
                l = new userFormat("|", "id|name|nei|net|seq|topo|left");
                break;
            case 2:
                l = new userFormat("|", "id|name|uptime|changes|changed");
                break;
            case 3:
                l = new userFormat("|", "id|name|hw|sw|middle|kernel");
                break;
            default:
                return null;
        }
        for (int i = 0; i < database.size(); i++) {
            rtrLsrpData ntry = database.get(i);
            if (ntry == null) {
                continue;
            }
            switch (mod) {
                case 1:
                    l.add(ntry.rtrId + "|" + ntry.hostname + "|" + ntry.neighbor.size() + "|" + ntry.network.size() + "|" + ntry.sequence + "|" + bits.toHexD(ntry.topoSum) + "|" + bits.timeLeft(ntry.time));
                    break;
                case 2:
                    l.add(ntry.rtrId + "|" + ntry.hostname + "|" + bits.timeDump(ntry.uptime / 1000) + "|" + ntry.changesNum + "|" + bits.timeDump(ntry.changesTim / 1000));
                    break;
                case 3:
                    l.add(ntry.rtrId + "|" + ntry.hostname + "|" + ntry.hardware + "|" + ntry.software + "|" + ntry.middleware + "|" + ntry.kernel);
                    break;
            }
        }
        return l;
    }

    /**
     * list database
     *
     * @param cmd entry to find
     * @return list of entry
     */
    public userFormat showDatabase(cmds cmd) {
        addrIPv4 ned = new addrIPv4();
        ned.fromString(cmd.word());
        rtrLsrpData ntry = new rtrLsrpData();
        ntry.rtrId = ned;
        ntry = database.find(ntry);
        if (ntry == null) {
            return null;
        }
        cmd = new cmds("", ntry.dump(rtrLsrpData.dmpFull));
        userFormat l = new userFormat("|", "type|value");
        for (;;) {
            String a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            int i = a.indexOf("=");
            String s = "";
            if (i >= 0) {
                s = a.substring(i + 1, a.length());
                a = a.substring(0, i);
            }
            l.add(a + "|" + s);
        }
        return l;
    }

    /**
     * show zonefile
     *
     * @param s domain
     * @return zonefile
     */
    public userFormat showZoneFile(String s) {
        userFormat l = new userFormat("|", "cmd|value|cmd|value");
        for (int i = 0; i < database.size(); i++) {
            rtrLsrpData ntry = database.get(i);
            for (int o = 0; o < ntry.address.size(); o++) {
                l.add("rr|" + packDnsRec.generateReverse(ntry.address.get(o)) + "|ptr|" + ntry.hostname + "." + s);
            }
            for (int o = 0; o < ntry.network.size(); o++) {
                l.add("rr|" + packDnsRec.generateReverse(ntry.network.get(o).prefix.network) + "|ptr|" + ntry.hostname + "." + s);
            }
        }
        return l;
    }

    /**
     * show spf
     *
     * @return log of spf
     */
    public userFormat showSpfStat() {
        return lastSpf.listStatistics();
    }

    /**
     * show topology
     *
     * @param cmd entry to find
     * @return log of spf
     */
    public userFormat showSpfTopo(cmds cmd) {
        if (cmd.size() < 1) {
            return lastSpf.listTopology();
        }
        addrIPv4 ned = new addrIPv4();
        ned.fromString(cmd.word());
        return lastSpf.listTopology(ned);
    }

    /**
     * show log
     *
     * @return log of spf
     */
    public userFormat showSpfLog() {
        return lastSpf.listUsages();
    }

    /**
     * show tree
     *
     * @return tree of spf
     */
    public List<String> showSpfTree() {
        return lastSpf.listTree();
    }

    /**
     * show graph
     *
     * @return graph of spf
     */
    public List<String> showSpfGraph() {
        return lastSpf.listGraphviz();
    }

    /**
     * get ip protocol version
     *
     * @return protocol version
     */
    public int getProtoVer() {
        return fwdCore.ipVersion;
    }

    /**
     * create computed
     */
    public synchronized void routerCreateComputed() {
        if (debugger.rtrLsrpEvnt) {
            logger.debug("create table");
        }
        rtrLsrpData dat = new rtrLsrpData();
        dat.fromString(new cmds("", ""));
        if (defOrigin) {
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = addrPrefix.defaultRoute(getProtoVer());
            ntry.segrouIdx = segrouIdx;
            ntry.rouSrc = segrouPop ? 16 : 0;
            ntry.bierIdx = bierIdx;
            dat.network.add(tabRoute.addType.always, ntry, true, true);
        }
        for (int o = 0; o < ifaces.size(); o++) {
            rtrLsrpIface ifc = ifaces.get(o);
            if (ifc == null) {
                continue;
            }
            if (ifc.iface.lower.getState() != state.states.up) {
                continue;
            }
            for (int i = 0; i < ifc.neighs.size(); i++) {
                rtrLsrpNeigh nei = ifc.neighs.get(i);
                if (nei == null) {
                    continue;
                }
                if (!nei.isReady()) {
                    continue;
                }
                int adj = 0;
                if (nei.segrouLab != null) {
                    adj = nei.segrouLab.getValue();
                }
                int met = ifc.metric;
                if (ifc.acceptMetric) {
                    met = nei.gotMet;
                }
                dat.addNeigh(nei.rtrId, met, (stub || ifc.stub) && (!ifc.unstub), ifc.iface.bandwidth / 1000, ifc.affinity, ifc.srlg, adj, nei.peer);
            }
            dat.address.add(ifc.iface.addr.copyBytes());
            if ((suppressAddr || ifc.suppressAddr) && (!ifc.unsuppressAddr)) {
                continue;
            }
            tabRouteEntry<addrIP> ntry = dat.network.add(tabRoute.addType.better, ifc.iface.network, null);
            ntry.rouTyp = tabRouteEntry.routeType.conn;
            ntry.iface = ifc.iface;
            ntry.distance = tabRouteEntry.distanIfc;
            if (ifc.segrouIdx >= 0) {
                ntry.segrouIdx = ifc.segrouIdx;
                ntry.rouSrc = ifc.segrouPop ? 16 : 0;
            } else {
                ntry.segrouIdx = segrouIdx;
                ntry.rouSrc = segrouPop ? 16 : 0;
            }
            if (ifc.bierIdx >= 0) {
                ntry.bierIdx = ifc.bierIdx;
            } else {
                ntry.bierIdx = bierIdx;
            }
        }
        for (int i = 0; i < routerRedistedU.size(); i++) {
            tabRouteEntry<addrIP> ntry = routerRedistedU.get(i);
            if (ntry == null) {
                continue;
            }
            ntry = ntry.copyBytes();
            ntry.distance = tabRouteEntry.distanIfc + 1;
            ntry.segrouIdx = segrouIdx;
            ntry.rouSrc = segrouPop ? 17 : 1;
            ntry.bierIdx = bierIdx;
            dat.network.add(tabRoute.addType.better, ntry, false, false);
        }
        dat.rtrId = routerID.copyBytes();
        dat.topoSum = lastSpf.listTopoSum().hashCode();
        dat.hostname = cfgAll.hostName.replaceAll(" ", "_");
        dat.software = version.usrAgnt.replaceAll(" ", "_");
        dat.hardware = (cfgInit.hwIdNum + " " + version.getCPUname()).replaceAll(" ", "_");
        dat.middleware = version.getVMname().replaceAll(" ", "_");
        dat.kernel = version.getKernelName().replaceAll(" ", "_");
        if (segrouLab != null) {
            dat.segrouMax = segrouMax;
            dat.segrouBeg = segrouLab[0].getValue();
        }
        if (bierLab != null) {
            dat.bierMax = bierMax;
            dat.bierLen = bierLen;
            dat.bierBeg = bierLab[0].getValue();
        }
        long tim = bits.getTime();
        dat.time = tim + lifetime;
        rtrLsrpData old = database.find(dat);
        if (old == null) {
            old = new rtrLsrpData();
            old.fromString(new cmds("", ""));
        }
        dat.sequence = old.sequence + 1;
        dat.uptime = tim - cfgInit.started;
        dat.changesNum = changeNum;
        dat.changesTim = tim - changeTim;
        boolean ned = !dat.dump(rtrLsrpData.dmpComp).equals(old.dump(rtrLsrpData.dmpComp));
        if (ned) {
            changeNum++;
            changeTim = tim;
        }
        ned |= (old.time - tim) < (lifetime - refresh);
        ned &= !routerID.isEmpty();
        if (ned) {
            if (debugger.rtrLsrpEvnt) {
                logger.debug("originate");
            }
            database.put(dat);
        }
        for (int i = database.size() - 1; i >= 0; i--) {
            rtrLsrpData ntry = database.get(i);
            if (tim < ntry.time) {
                continue;
            }
            database.del(ntry);
        }
        shrtPthFrst<addrIPv4> spf = new shrtPthFrst<addrIPv4>(lastSpf);
        for (int i = 0; i < database.size(); i++) {
            rtrLsrpData ntry = database.get(i);
            if (ntry == null) {
                continue;
            }
            ntry.put2spf(spf);
        }
        spf.doCalc(routerID, null);
        boolean[] segrouUsd = null;
        if (segrouLab != null) {
            segrouUsd = new boolean[segrouMax];
        }
        for (int o = 0; o < ifaces.size(); o++) {
            rtrLsrpIface ifc = ifaces.get(o);
            if (ifc == null) {
                continue;
            }
            if (ifc.iface.lower.getState() != state.states.up) {
                continue;
            }
            if ((segrouUsd != null) && (ifc.segrouIdx > 0)) {
                segrouLab[ifc.segrouIdx].setFwdCommon(6, fwdCore);
                segrouUsd[ifc.segrouIdx] = true;
            }
            for (int i = 0; i < ifc.neighs.size(); i++) {
                rtrLsrpNeigh nei = ifc.neighs.get(i);
                if (nei == null) {
                    continue;
                }
                if (!nei.isReady()) {
                    continue;
                }
                int met = ifc.metric;
                if (ifc.acceptMetric) {
                    met = nei.gotMet;
                }
                spf.addNextHop(met, nei.rtrId, nei.peer.copyBytes(), ifc.iface);
            }
        }
        tabRoute<addrIP> tab1 = new tabRoute<addrIP>("routes");
        for (int o = 0; o < database.size(); o++) {
            rtrLsrpData ntry = database.get(o);
            if (ntry == null) {
                continue;
            }
            addrIP hop = spf.getNextHop(ntry.rtrId);
            if (hop == null) {
                continue;
            }
            int hops = spf.getHops(ntry.rtrId);
            ipFwdIface iface = (ipFwdIface) spf.getNextIfc(ntry.rtrId);
            int met = spf.getMetric(ntry.rtrId);
            int srb = spf.getSegRouB(ntry.rtrId, false);
            int sro = spf.getSegRouB(ntry.rtrId, true);
            int brb = spf.getBierB(ntry.rtrId, false);
            int bro = spf.getBierB(ntry.rtrId, true);
            for (int i = 0; i < ntry.network.size(); i++) {
                tabRouteEntry<addrIP> rou = ntry.network.get(i).copyBytes();
                rou.srcRtr = ntry.rtrId.copyBytes();
                rou.nextHop = hop.copyBytes();
                rou.metric += met;
                rou.distance = distance;
                rou.iface = iface;
                spf.addSegRouI(ntry.rtrId, rou.segrouIdx);
                spf.addBierI(ntry.rtrId, rou.bierIdx, true);
                if ((segrouUsd != null) && (rou.segrouIdx > 0) && (rou.segrouIdx < segrouMax) && (srb > 0)) {
                    List<Integer> lab = tabLabel.int2labels(srb + rou.segrouIdx);
                    if (((rou.rouSrc & 16) != 0) && (hops <= 1)) {
                        lab = tabLabel.int2labels(ipMpls.labelImp);
                    }
                    segrouLab[rou.segrouIdx].setFwdMpls(6, fwdCore, iface, hop, lab);
                    segrouUsd[rou.segrouIdx] = true;
                    rou.labelRem = lab;
                }
                rou.segrouBeg = srb;
                rou.segrouOld = sro;
                rou.bierBeg = brb;
                rou.bierOld = bro;
                rou.bierHdr = tabLabelBier.num2bsl(ntry.bierLen);
                tab1.add(tabRoute.addType.better, rou, false, true);
            }
        }
        if (segrouUsd != null) {
            if (segrouIdx > 0) {
                segrouLab[segrouIdx].setFwdCommon(6, fwdCore);
                segrouUsd[segrouIdx] = true;
            }
            for (int i = 0; i < segrouUsd.length; i++) {
                if (segrouUsd[i]) {
                    continue;
                }
                segrouLab[i].setFwdDrop(6);
            }
        }
        tabRoute<addrIP> tab2 = new tabRoute<addrIP>("routes");
        tabRoute.addUpdatedTable(tabRoute.addType.better, rtrBgpUtil.safiUnicast, tab2, tab1, true, roumapIn, roupolIn, prflstIn);
        routerDoAggregates(rtrBgpUtil.safiUnicast, tab2, null, fwdCore.commonLabel, 0, null, 0);
        if (bierLab != null) {
            tabLabelBier res = spf.getBierI();
            res.base = bierLab[0].getValue();
            res.fwdr = fwdCore;
            res.bsl = tabLabelBier.num2bsl(bierLen);
            res.idx = bierIdx;
            if (bierIdx < 1) {
                for (int i = 0; i < ifaces.size(); i++) {
                    rtrLsrpIface ifc = ifaces.get(i);
                    if (ifc == null) {
                        continue;
                    }
                    if (ifc.bierIdx < 1) {
                        continue;
                    }
                    res.idx = ifc.bierIdx;
                    break;
                }
            }
            for (int i = 0; i < bierLab.length; i++) {
                bierLab[i].setBierMpls(18, fwdCore, res);
            }
        }
        for (int o = 0; o < ifaces.size(); o++) {
            rtrLsrpIface ifc = ifaces.get(o);
            if (ifc == null) {
                continue;
            }
            for (int i = 0; i < ifc.neighs.size(); i++) {
                rtrLsrpNeigh nei = ifc.neighs.get(i);
                if (nei == null) {
                    continue;
                }
                nei.notif.wakeup();
            }
        }
        if (debugger.rtrLsrpEvnt) {
            logger.debug("unreachable:" + spf.listUnreachables());
            logger.debug("reachable:" + spf.listReachables());
        }
        lastSpf = spf;
        tab2.setProto(routerProtoTyp, routerProcNum);
        tab2.preserveTime(routerComputedU);
        routerComputedU = tab2;
        fwdCore.routerChg(this);
    }

    /**
     * redistribution changed
     */
    public void routerRedistChanged() {
        routerCreateComputed();
    }

    /**
     * others changed
     */
    public void routerOthersChanged() {
    }

    /**
     * stop work
     */
    public void routerCloseNow() {
        if (debugger.rtrLsrpEvnt) {
            logger.debug("shutdown");
        }
        need2run = false;
        for (int i = 0; i < ifaces.size(); i++) {
            rtrLsrpIface ifc = ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            ifc.routerCloseNow();
        }
        tabLabel.release(segrouLab, 6);
        tabLabel.release(bierLab, 18);
    }

    /**
     * get help
     *
     * @param l list
     */
    public void routerGetHelp(userHelping l) {
        l.add("1 2   router-id                   specify router id");
        l.add("2 .     <addr>                    router id");
        l.add("1 2   distance                    specify default distance");
        l.add("2 .     <num>                     distance");
        l.add("1 .   default-originate           advertise default route");
        l.add("1 2   route-map                   process prefixes");
        l.add("2 .     <name>                    name of route map");
        l.add("1 2   route-policy                process prefixes");
        l.add("2 .     <name>                    name of route policy");
        l.add("1 2   prefix-list                 filter prefixes");
        l.add("2 .     <name>                    name of prefix list");
        l.add("1 2   refresh                     data refresh time");
        l.add("2 .     <num>                     age in ms");
        l.add("1 2   lifetime                    data life time");
        l.add("2 .     <num>                     age in ms");
        l.add("1 .   spf-bidir                   spf bidir check");
        l.add("1 2   spf-log                     spf log size");
        l.add("2 .     <num>                     number of entries");
        l.add("1 .   stub                        stub router");
        l.add("1 .   suppress-prefix             do not advertise interfaces");
        l.add("1 2   segrout                     segment routing parameters");
        l.add("2 3     <num>                     maximum index");
        l.add("3 4,.     <num>                   this node index");
        l.add("4 4,.       pop                   advertise php");
        l.add("4 5         base                  specify base");
        l.add("5 4,.         <num>               label base");
        l.add("1 2   bier                        bier parameters");
        l.add("2 3     <num>                     bitstring length");
        l.add("3 4       <num>                   maximum index");
        l.add("4 .         <num>                 this node index");
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
        l.add(beg + "distance " + distance);
        l.add(beg + "refresh " + refresh);
        l.add(beg + "lifetime " + lifetime);
        l.add(beg + "spf-log " + lastSpf.logSize);
        cmds.cfgLine(l, lastSpf.bidir.get() == 0, beg, "spf-bidir", "");
        cmds.cfgLine(l, !stub, beg, "stub", "");
        cmds.cfgLine(l, !suppressAddr, beg, "suppress-prefix", "");
        cmds.cfgLine(l, !defOrigin, beg, "default-originate", "");
        cmds.cfgLine(l, prflstIn == null, beg, "prefix-list", "" + prflstIn);
        cmds.cfgLine(l, roumapIn == null, beg, "route-map", "" + roumapIn);
        cmds.cfgLine(l, roupolIn == null, beg, "route-policy", "" + roupolIn);
        String a = "";
        if (segrouPop) {
            a += " pop";
        }
        if (segrouBase != 0) {
            a += " base " + segrouBase;
        }
        cmds.cfgLine(l, segrouMax < 1, beg, "segrout", segrouMax + " " + segrouIdx + a);
        cmds.cfgLine(l, bierMax < 1, beg, "bier", bierLen + " " + bierMax + " " + bierIdx);
    }

    /**
     * configure
     *
     * @param cmd command
     * @return false if success, true if error
     */
    public boolean routerConfigure(cmds cmd) {
        String s = cmd.word();
        boolean negated = false;
        if (s.equals("no")) {
            s = cmd.word();
            negated = true;
        }
        if (s.equals("router-id")) {
            routerID.fromString(cmd.word());
            if (negated) {
                routerID = new addrIPv4();
            }
            todo.set(0);
            notif.wakeup();
            return false;
        }
        if (s.equals("spf-log")) {
            lastSpf.logSize.set(bits.str2num(cmd.word()));
            if (negated) {
                lastSpf.logSize.set(0);
            }
            return false;
        }
        if (s.equals("spf-bidir")) {
            if (negated) {
                lastSpf.bidir.set(0);
            } else {
                lastSpf.bidir.set(1);
            }
            todo.set(0);
            notif.wakeup();
            return false;
        }
        if (s.equals("default-originate")) {
            defOrigin = !negated;
            todo.set(0);
            notif.wakeup();
            return false;
        }
        if (s.equals("stub")) {
            stub = !negated;
            todo.set(0);
            notif.wakeup();
            return false;
        }
        if (s.equals("suppress-prefix")) {
            suppressAddr = !negated;
            todo.set(0);
            notif.wakeup();
            return false;
        }
        if (s.equals("distance")) {
            distance = bits.str2num(cmd.word());
            todo.set(0);
            notif.wakeup();
            return false;
        }
        if (s.equals("refresh")) {
            refresh = bits.str2num(cmd.word());
            todo.set(0);
            notif.wakeup();
            return false;
        }
        if (s.equals("lifetime")) {
            lifetime = bits.str2num(cmd.word());
            todo.set(0);
            notif.wakeup();
            return false;
        }
        if (s.equals("prefix-list")) {
            if (negated) {
                prflstIn = null;
                todo.set(0);
                notif.wakeup();
                return false;
            }
            cfgPrfxlst ntry = cfgAll.prfxFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such prefix list");
                return false;
            }
            prflstIn = ntry.prflst;
            todo.set(0);
            notif.wakeup();
            return false;
        }
        if (s.equals("route-map")) {
            if (negated) {
                roumapIn = null;
                todo.set(0);
                notif.wakeup();
                return false;
            }
            cfgRoump ntry = cfgAll.rtmpFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such route map");
                return false;
            }
            roumapIn = ntry.roumap;
            todo.set(0);
            notif.wakeup();
            return false;
        }
        if (s.equals("route-policy")) {
            if (negated) {
                roupolIn = null;
                todo.set(0);
                notif.wakeup();
                return false;
            }
            cfgRouplc ntry = cfgAll.rtplFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such route policy");
                return false;
            }
            roupolIn = ntry.rouplc;
            todo.set(0);
            notif.wakeup();
            return false;
        }
        if (s.equals("segrout")) {
            tabLabel.release(segrouLab, 6);
            segrouLab = null;
            if (negated) {
                segrouIdx = 0;
                segrouMax = 0;
                segrouBase = 0;
                segrouPop = false;
                todo.set(0);
                notif.wakeup();
                return false;
            }
            segrouMax = bits.str2num(cmd.word());
            segrouIdx = bits.str2num(cmd.word());
            segrouPop = false;
            segrouBase = 0;
            for (;;) {
                s = cmd.word();
                if (s.length() < 1) {
                    break;
                }
                if (s.equals("pop")) {
                    segrouPop = true;
                    continue;
                }
                if (s.equals("base")) {
                    segrouBase = bits.str2num(cmd.word());
                    continue;
                }
            }
            segrouLab = tabLabel.allocate(6, segrouBase, segrouMax);
            todo.set(0);
            notif.wakeup();
            return false;
        }
        if (s.equals("bier")) {
            tabLabel.release(bierLab, 18);
            bierLab = null;
            if (negated) {
                bierIdx = 0;
                bierMax = 0;
                bierLen = 0;
                todo.set(0);
                notif.wakeup();
                return false;
            }
            bierLen = tabLabelBier.normalizeBsl(bits.str2num(cmd.word()));
            bierMax = bits.str2num(cmd.word());
            bierIdx = bits.str2num(cmd.word());
            bierLab = tabLabel.allocate(18, (bierMax + bierLen - 1) / bierLen);
            todo.set(0);
            notif.wakeup();
            return false;
        }
        return true;
    }

    public void run() {
        for (;;) {
            notif.misleep(10000);
            if (!need2run) {
                return;
            }
            if (todo.sub(1) > 0) {
                continue;
            }
            todo.set(6);
            try {
                routerCreateComputed();
            } catch (Exception e) {
                logger.traceback(e);
            }
        }
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
            rtrLsrpIface ifc = ifaces.get(o);
            if (ifc == null) {
                continue;
            }
            if (ifc.iface.lower.getState() != state.states.up) {
                continue;
            }
            for (int i = 0; i < ifc.neighs.size(); i++) {
                rtrLsrpNeigh nei = ifc.neighs.get(i);
                if (nei == null) {
                    continue;
                }
                tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
                ntry.prefix = new addrPrefix<addrIP>(nei.peer, addrIP.size * 8);
                tabRoute.addUpdatedEntry(tabRoute.addType.better, tab, rtrBgpUtil.safiUnicast, ntry, true, null, null, routerAutoMesh);
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
