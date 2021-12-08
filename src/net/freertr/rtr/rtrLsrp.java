package net.freertr.rtr;

import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrIPv4;
import net.freertr.addr.addrPrefix;
import net.freertr.auth.authLocal;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgInit;
import net.freertr.cfg.cfgPrfxlst;
import net.freertr.cfg.cfgRoump;
import net.freertr.cfg.cfgRouplc;
import net.freertr.ip.ipCor4;
import net.freertr.ip.ipCor6;
import net.freertr.ip.ipFwd;
import net.freertr.ip.ipFwdIface;
import net.freertr.ip.ipFwdTab;
import net.freertr.ip.ipRtr;
import net.freertr.pack.packDnsRec;
import net.freertr.prt.prtTcp;
import net.freertr.prt.prtUdp;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabIndex;
import net.freertr.tab.tabIntMatcher;
import net.freertr.tab.tabLabel;
import net.freertr.tab.tabLabelBier;
import net.freertr.tab.tabLabelEntry;
import net.freertr.tab.tabListing;
import net.freertr.tab.tabPrfxlstN;
import net.freertr.tab.tabRoute;
import net.freertr.tab.tabRouteAttr;
import net.freertr.tab.tabRouteEntry;
import net.freertr.tab.tabRtrmapN;
import net.freertr.tab.tabRtrplcN;
import net.freertr.user.userFormat;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.debugger;
import net.freertr.util.logger;
import net.freertr.util.notifier;
import net.freertr.util.shrtPthFrst;
import net.freertr.util.state;
import net.freertr.util.syncInt;
import net.freertr.util.version;

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
     * database password
     */
    public String authentication = null;

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
    protected tabLabelEntry[] segrouLab;

    /**
     * bier labels
     */
    protected tabLabelEntry[] bierLab;

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
        tabRouteAttr.routeType rouTyp = null;
        switch (fwdCore.ipVersion) {
            case ipCor4.protocolVersion:
                rouTyp = tabRouteAttr.routeType.lsrp4;
                break;
            case ipCor6.protocolVersion:
                rouTyp = tabRouteAttr.routeType.lsrp6;
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
        ntry.unregister2udp();
        ntry.closeNeighbors();
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
     * list of neighbors
     *
     * @return list
     */
    public userFormat showMetrics() {
        userFormat res = new userFormat("|", "iface|router|name|peer|metric|delay|gotmet");
        for (int i = 0; i < ifaces.size(); i++) {
            rtrLsrpIface ifc = ifaces.get(i);
            ifc.showMetrics(res);
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
     * @param mod mode: 1=summary, 2=uptime, 3=software, 4=middleware, 5=kernel
     * @return list of database
     */
    public userFormat showDatabase(int mod) {
        userFormat l;
        switch (mod) {
            case 1:
                l = new userFormat("|", "id|name|nei|net|seq|topo|left");
                break;
            case 2:
                l = new userFormat("|", "id|name|since|uptime|changes|changed");
                break;
            case 3:
                l = new userFormat("|", "id|name|hw|software");
                break;
            case 4:
                l = new userFormat("|", "id|name|middle");
                break;
            case 5:
                l = new userFormat("|", "id|name|kernel");
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
                    l.add(ntry.rtrId + "|" + ntry.hostname + "|" + bits.time2str(cfgAll.timeZoneName, ntry.since, 3) + "|" + bits.timeDump(ntry.uptime / 1000) + "|" + ntry.changesNum + "|" + bits.timeDump(ntry.changesTim / 1000));
                    break;
                case 3:
                    l.add(ntry.rtrId + "|" + ntry.hostname + "|" + ntry.hardware + "|" + ntry.software);
                    break;
                case 4:
                    l.add(ntry.rtrId + "|" + ntry.hostname + "|" + ntry.middleware);
                    break;
                case 5:
                    l.add(ntry.rtrId + "|" + ntry.hostname + "|" + ntry.kernel);
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
     * @param d domain
     * @param s separator
     * @param r replacers
     * @return zonefile
     */
    public userFormat showZoneRev(String d, String s, List<String> r) {
        userFormat l = new userFormat("|", "cmd|value|cmd|value");
        for (int i = 0; i < database.size(); i++) {
            rtrLsrpData ntry = database.get(i);
            for (int o = 0; o < ntry.address.size(); o++) {
                String a = ntry.address.get(o).iface;
                for (int p = 0; p < r.size(); p += 2) {
                    a = a.replaceAll(r.get(p), r.get(p + 1));
                }
                l.add("rr|" + packDnsRec.generateReverse(ntry.address.get(o).addr) + "|ptr|" + ntry.hostname + s + a + "." + d);
            }
            for (int o = 0; o < ntry.network.size(); o++) {
                l.add("rr|" + packDnsRec.generateReverse(ntry.network.get(o).prefix.network) + "|ptr|" + ntry.hostname + "." + d);
            }
            l.add("rr|" + packDnsRec.generateReverse(ntry.mgmtIp) + "|ptr|" + ntry.hostname + "." + d);
        }
        return l;
    }

    /**
     * show zonefile
     *
     * @param d domain
     * @param s separator
     * @param r replacers
     * @return zonefile
     */
    public userFormat showZoneFwd(String d, String s, List<String> r) {
        userFormat l = new userFormat("|", "cmd|value|cmd|value");
        for (int i = 0; i < database.size(); i++) {
            rtrLsrpData ntry = database.get(i);
            String t;
            if (ntry.mgmtIp.isIPv4()) {
                t = packDnsRec.type2str(packDnsRec.typeA);
            } else {
                t = packDnsRec.type2str(packDnsRec.typeAAAA);
            }
            for (int o = 0; o < ntry.address.size(); o++) {
                String a = ntry.address.get(o).iface;
                for (int p = 0; p < r.size(); p += 2) {
                    a = a.replaceAll(r.get(p), r.get(p + 1));
                }
                l.add("rr|" + ntry.hostname + s + a + "." + d + "|" + t + "|" + ntry.address.get(o).addr);
            }
            l.add("rr|" + ntry.hostname + "." + d + "|" + t + "|" + ntry.mgmtIp);
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
     * show tree
     *
     * @param cmd entry to find
     * @return tree of spf
     */
    public List<String> showSpfOtherTree(cmds cmd) {
        shrtPthFrst<addrIPv4> spf = lastSpf.copyBytes();
        addrIPv4 ned = new addrIPv4();
        ned.fromString(cmd.word());
        spf.doCalc(ned, null);
        return spf.listTree();
    }

    /**
     * show topology
     *
     * @param cmd entry to find
     * @return log of spf
     */
    public userFormat showSpfOtherTopo(cmds cmd) {
        shrtPthFrst<addrIPv4> spf = lastSpf.copyBytes();
        addrIPv4 ned = new addrIPv4();
        ned.fromString(cmd.word());
        spf.doCalc(ned, null);
        if (cmd.size() < 1) {
            return spf.listTopology();
        }
        ned = new addrIPv4();
        ned.fromString(cmd.word());
        return spf.listTopology(ned);
    }

    /**
     * show graph
     *
     * @param nocli no cli
     * @param nonets no nets
     * @param noints no ints
     * @return graph of spf
     */
    public List<String> showSpfGraph(boolean nocli, boolean nonets, boolean noints) {
        return lastSpf.listGraphviz(nocli, nonets, noints);
    }

    /**
     * show nh inconsistency
     *
     * @param mtch matcher
     * @return inconsistency list
     */
    public userFormat showNhIncons(tabIntMatcher mtch) {
        return lastSpf.listNhIncons(mtch);
    }

    /**
     * show met inconsistency
     *
     * @param mtch matcher
     * @return inconsistency list
     */
    public userFormat showMetIncons(tabIntMatcher mtch) {
        return lastSpf.listMetIncons(mtch);
    }

    /**
     * show hostnames
     *
     * @return names list
     */
    public userFormat showHostnames() {
        return lastSpf.listHostnames();
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
            ntry.best.segrouIdx = segrouIdx;
            ntry.best.rouSrc = segrouPop ? 16 : 0;
            ntry.best.bierIdx = bierIdx;
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
                    adj = nei.segrouLab.label;
                }
                dat.addNeigh(nei.rtrId, "" + ifc.iface, nei.getMetric(), (stub || ifc.stub) && (!ifc.unstub), ifc.iface.bandwidth / 1000, ifc.affinity, ifc.srlg, ifc.iface.mtu, adj, nei.peer, nei.inam);
            }
            dat.addAddr("" + ifc.iface, ifc.iface.addr);
            if ((suppressAddr || ifc.suppressAddr) && (!ifc.unsuppressAddr)) {
                continue;
            }
            tabRouteEntry<addrIP> ntry = dat.network.add(tabRoute.addType.better, ifc.iface.network, null);
            ntry.best.rouTyp = tabRouteAttr.routeType.conn;
            ntry.best.iface = ifc.iface;
            ntry.best.distance = tabRouteAttr.distanIfc;
            if (ifc.segrouIdx >= 0) {
                ntry.best.segrouIdx = ifc.segrouIdx;
                ntry.best.rouSrc = ifc.segrouPop ? 16 : 0;
            } else {
                ntry.best.segrouIdx = segrouIdx;
                ntry.best.rouSrc = segrouPop ? 16 : 0;
            }
            if (ifc.bierIdx >= 0) {
                ntry.best.bierIdx = ifc.bierIdx;
            } else {
                ntry.best.bierIdx = bierIdx;
            }
        }
        for (int i = 0; i < routerRedistedU.size(); i++) {
            tabRouteEntry<addrIP> ntry = routerRedistedU.get(i);
            if (ntry == null) {
                continue;
            }
            ntry = ntry.copyBytes(tabRoute.addType.notyet);
            ntry.best.distance = tabRouteAttr.distanIfc + 1;
            ntry.best.segrouIdx = segrouIdx;
            ntry.best.rouSrc = segrouPop ? 17 : 1;
            ntry.best.bierIdx = bierIdx;
            dat.network.add(tabRoute.addType.better, ntry, false, false);
        }
        dat.rtrId = routerID.copyBytes();
        ipFwdIface mgmtIf = ipFwdTab.findStableIface(fwdCore);
        if (mgmtIf != null) {
            if (mgmtIf.addr != null) {
                dat.mgmtIp = mgmtIf.addr.copyBytes();
            }
        }
        dat.topoSum = lastSpf.listTopoHsh();
        dat.hostname = cfgAll.hostName.replaceAll(" ", "_");
        dat.software = version.usrAgnt.replaceAll(" ", "_");
        dat.hardware = (cfgInit.hwIdNum + " " + version.getCPUname() + " " + version.getMemoryInfo()).replaceAll(" ", "_");
        dat.middleware = version.getVMname().replaceAll(" ", "_");
        dat.kernel = version.getKernelName().replaceAll(" ", "_");
        if (segrouLab != null) {
            dat.segrouMax = segrouMax;
            dat.segrouBeg = segrouLab[0].label;
        }
        if (bierLab != null) {
            dat.bierMax = bierMax;
            dat.bierLen = bierLen;
            dat.bierBeg = bierLab[0].label;
        }
        long tim = bits.getTime();
        dat.time = tim + lifetime;
        dat.uptime = tim - cfgInit.started;
        dat.changesNum = changeNum;
        dat.changesTim = tim - changeTim;
        rtrLsrpData old = database.find(dat);
        if (old == null) {
            old = new rtrLsrpData();
            old.fromString(new cmds("", ""));
        }
        dat.since = old.since;
        dat.sequence = old.sequence + 1;
        if (authentication != null) {
            dat.password = dat.calcPass(authentication);
        }
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
            ntry.put2spf(spf, distance);
        }
        spf.doCalc(routerID, null);
        tabGen<tabIndex<addrIP>> segrouUsd = null;
        if (segrouLab != null) {
            segrouUsd = new tabGen<tabIndex<addrIP>>();
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
                tabIndex.add2table(segrouUsd, new tabIndex<addrIP>(ifc.segrouIdx, new addrPrefix<addrIP>(new addrIP(), 0)));
            }
            for (int i = 0; i < ifc.neighs.size(); i++) {
                rtrLsrpNeigh nei = ifc.neighs.get(i);
                if (nei == null) {
                    continue;
                }
                if (!nei.isReady()) {
                    continue;
                }
                spf.addNextHop(nei.getMetric(), nei.rtrId, nei.peer, ifc.iface, null, null);
            }
        }
        tabRoute<addrIP> tab1 = spf.getRoutes(fwdCore, 6, segrouLab, segrouUsd);
        if (segrouUsd != null) {
            if (segrouIdx > 0) {
                segrouLab[segrouIdx].setFwdCommon(6, fwdCore);
                tabIndex.add2table(segrouUsd, new tabIndex<addrIP>(segrouIdx, new addrPrefix<addrIP>(new addrIP(), 0)));
            }
            for (int i = 0; i < segrouLab.length; i++) {
                if (segrouUsd.find(new tabIndex<addrIP>(i, null)) != null) {
                    continue;
                }
                segrouLab[i].setFwdDrop(6);
            }
        }
        tabRoute<addrIP> tab2 = new tabRoute<addrIP>("routes");
        tabRoute.addUpdatedTable(tabRoute.addType.ecmp, rtrBgpUtil.sfiUnicast, 0, tab2, tab1, true, roumapIn, roupolIn, prflstIn);
        routerDoAggregates(rtrBgpUtil.sfiUnicast, tab2, tab2, fwdCore.commonLabel, null, 0);
        if (bierLab != null) {
            tabLabelBier res = spf.getBierI(bierLab[0].label, tabLabelBier.num2bsl(bierLen));
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
            logger.debug("unreachable:" + spf.listReachablility(false));
            logger.debug("reachable:" + spf.listReachablility(true));
        }
        lastSpf = spf;
        tab2.setProto(routerProtoTyp, routerProcNum);
        tab2.preserveTime(routerComputedU);
        routerComputedU = tab2;
        routerComputedM = tab2;
        routerComputedF = new tabRoute<addrIP>("rx");
        routerComputedI = segrouUsd;
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
            ifc.unregister2udp();
            ifc.closeNeighbors();
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
        l.add(null, "1 2   router-id                   specify router id");
        l.add(null, "2 .     <addr>                    router id");
        l.add(null, "1 2   distance                    specify default distance");
        l.add(null, "2 .     <num>                     distance");
        l.add(null, "1 .   default-originate           advertise default route");
        l.add(null, "1 2   route-map                   process prefixes");
        l.add(null, "2 .     <name:rm>                 name of route map");
        l.add(null, "1 2   route-policy                process prefixes");
        l.add(null, "2 .     <name:rpl>                name of route policy");
        l.add(null, "1 2   prefix-list                 filter prefixes");
        l.add(null, "2 .     <name:pl>                 name of prefix list");
        l.add(null, "1 2   database-password           database password");
        l.add(null, "2 .     <str>                     password");
        l.add(null, "1 2   refresh                     data refresh time");
        l.add(null, "2 .     <num>                     age in ms");
        l.add(null, "1 2   lifetime                    data life time");
        l.add(null, "2 .     <num>                     age in ms");
        l.add(null, "1 .   spf-bidir                   spf bidir check");
        l.add(null, "1 2,. spf-topolog                 spf topology logging");
        l.add(null, "2 2,.   noappear                  exclude node (dis)appearance");
        l.add(null, "2 2,.   noconnect                 exclude link (dis)connection");
        l.add(null, "2 2,.   noforward                 exclude forward (un)willingness");
        l.add(null, "2 2,.   noreachable               exclude node (un)reachable");
        l.add(null, "2 2,.   nometric                  exclude link metric change");
        l.add(null, "2 2,.   noprefix                  exclude prefix change");
        l.add(null, "1 .   spf-hops                    spf hops disallow");
        l.add(null, "1 .   spf-ecmp                    spf ecmp allow");
        l.add(null, "1 2   spf-log                     spf log size");
        l.add(null, "2 .     <num>                     number of entries");
        l.add(null, "1 .   stub                        stub router");
        l.add(null, "1 .   suppress-prefix             do not advertise interfaces");
        l.add(null, "1 2   segrout                     segment routing parameters");
        l.add(null, "2 3     <num>                     maximum index");
        l.add(null, "3 4,.     <num>                   this node index");
        l.add(null, "4 4,.       pop                   advertise php");
        l.add(null, "4 5         base                  specify base");
        l.add(null, "5 4,.         <num>               label base");
        l.add(null, "1 2   bier                        bier parameters");
        l.add(null, "2 3     <num>                     bitstring length");
        l.add(null, "3 4       <num>                   maximum index");
        l.add(null, "4 .         <num>                 this node index");
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
        l.add(beg + "distance " + distance);
        l.add(beg + "refresh " + refresh);
        l.add(beg + "lifetime " + lifetime);
        l.add(beg + "spf-log " + lastSpf.logSize);
        cmds.cfgLine(l, lastSpf.topoLog.get() == 0, beg, "spf-topolog", lastSpf.getTopoLogMode());
        cmds.cfgLine(l, lastSpf.bidir.get() == 0, beg, "spf-bidir", "");
        cmds.cfgLine(l, lastSpf.hops.get() == 0, beg, "spf-hops", "");
        cmds.cfgLine(l, lastSpf.ecmp.get() == 0, beg, "spf-ecmp", "");
        cmds.cfgLine(l, !stub, beg, "stub", "");
        cmds.cfgLine(l, !suppressAddr, beg, "suppress-prefix", "");
        cmds.cfgLine(l, !defOrigin, beg, "default-originate", "");
        cmds.cfgLine(l, prflstIn == null, beg, "prefix-list", "" + prflstIn);
        cmds.cfgLine(l, roumapIn == null, beg, "route-map", "" + roumapIn);
        cmds.cfgLine(l, roupolIn == null, beg, "route-policy", "" + roupolIn);
        cmds.cfgLine(l, authentication == null, beg, "database-password", authLocal.passwdEncode(authentication, (filter & 2) != 0));
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
        if (s.equals("database-password")) {
            authentication = authLocal.passwdDecode(cmd.word());
            if (negated) {
                authentication = null;
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
        if (s.equals("spf-topolog")) {
            if (negated) {
                lastSpf.topoLog.set(0);
                return false;
            }
            lastSpf.setTopoLogMode(cmd);
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
        if (s.equals("spf-hops")) {
            if (negated) {
                lastSpf.hops.set(0);
            } else {
                lastSpf.hops.set(1);
            }
            todo.set(0);
            notif.wakeup();
            return false;
        }
        if (s.equals("spf-ecmp")) {
            if (negated) {
                lastSpf.ecmp.set(0);
            } else {
                lastSpf.ecmp.set(1);
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
     * @param par parameter
     * @param asn asn
     * @param adv advertiser
     */
    public void routerLinkStates(tabRoute<addrIP> tab, int par, int asn, addrIPv4 adv) {
        lastSpf.listLinkStates(tab, 227, -1, asn, adv, addrIPv4.size);
    }

}
