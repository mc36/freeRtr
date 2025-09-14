package org.freertr.rtr;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrPrefix;
import org.freertr.auth.authLocal;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgInit;
import org.freertr.cfg.cfgPrfxlst;
import org.freertr.cfg.cfgRoump;
import org.freertr.cfg.cfgRouplc;
import org.freertr.cfg.cfgVrf;
import org.freertr.ip.ipCor4;
import org.freertr.ip.ipCor6;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipFwdTab;
import org.freertr.ip.ipRtr;
import org.freertr.pack.packDnsRec;
import org.freertr.pipe.pipeShell;
import org.freertr.prt.prtTcp;
import org.freertr.prt.prtUdp;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabIndex;
import org.freertr.tab.tabIntMatcher;
import org.freertr.tab.tabLabel;
import org.freertr.tab.tabLabelBier;
import org.freertr.tab.tabLabelEntry;
import org.freertr.tab.tabListing;
import org.freertr.tab.tabPrfxlstN;
import org.freertr.tab.tabRoute;
import org.freertr.tab.tabRouteAttr;
import org.freertr.tab.tabRouteEntry;
import org.freertr.tab.tabRtrmapN;
import org.freertr.tab.tabRtrplcN;
import org.freertr.user.userFormat;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.notifier;
import org.freertr.spf.spfCalc;
import org.freertr.spf.spfLnkst;
import org.freertr.util.keyword;
import org.freertr.util.state;
import org.freertr.util.syncInt;

/**
 * link state routing protocol
 *
 * @author matecsaba
 */
public class rtrLsrp extends ipRtr implements Runnable {

    /**
     * port number
     */
    public final static int port = 1678;

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
     * ha mode
     */
    public boolean haMode;

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
     * bier subdomain
     */
    public int bierSub = 0;

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
     * list of flexalgos
     */
    protected tabGen<rtrAlgo> algos;

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
    protected spfCalc<addrIPv4> lastSpf;

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
        algos = new tabGen<rtrAlgo>();
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
        lastSpf = new spfCalc<addrIPv4>(null);
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
     * @param brief only briefly
     * @return list
     */
    public userFormat showNeighs(boolean brief) {
        userFormat res;
        if (brief) {
            res = new userFormat("|", "router|name|ready|uptime");
        } else {
            res = new userFormat("|", "iface|router|name|peerif|peer|ready|uptime");
        }
        for (int i = 0; i < ifaces.size(); i++) {
            rtrLsrpIface ifc = ifaces.get(i);
            ifc.showNeighs(res, brief);
        }
        return res;
    }

    /**
     * list of algorithms
     *
     * @return list
     */
    public userFormat showAlgorithms() {
        return lastSpf.listAlgorithm();
    }

    /**
     * list of neighbors
     *
     * @return list
     */
    public userFormat showMetrics() {
        userFormat res = new userFormat("|", "iface|router|name|peer|metric|gotmet|delay");
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
     * list statistics
     *
     * @param iface forwarding interface
     * @return list of interfaces
     */
    public userFormat showStats(ipFwdIface iface) {
        if (iface == null) {
            return null;
        }
        rtrLsrpIface ifc = new rtrLsrpIface(this, iface);
        ifc = ifaces.find(ifc);
        if (ifc == null) {
            return null;
        }
        return keyword.dump(ifc.msgStatRx, ifc.msgStatTx);
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
     * @param mod mode: 1=summary, 2=uptime, 3=software, 4=middleware, 5=kernel,
     * 6=hardware, 7=forwarder
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
                l = new userFormat("|", "id|name|software");
                break;
            case 4:
                l = new userFormat("|", "id|name|middle");
                break;
            case 5:
                l = new userFormat("|", "id|name|kernel|boot");
                break;
            case 6:
                l = new userFormat("|", "id|name|hardware");
                break;
            case 7:
                l = new userFormat("|", "id|name|forwarder");
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
                    l.add(ntry.rtrId + "|" + ntry.hostname + "|" + ntry.software);
                    break;
                case 4:
                    l.add(ntry.rtrId + "|" + ntry.hostname + "|" + ntry.middleware);
                    break;
                case 5:
                    l.add(ntry.rtrId + "|" + ntry.hostname + "|" + ntry.kernel + "|" + ntry.kernup);
                    break;
                case 6:
                    l.add(ntry.rtrId + "|" + ntry.hostname + "|" + ntry.hardware);
                    break;
                case 7:
                    l.add(ntry.rtrId + "|" + ntry.hostname + "|" + ntry.forwarder);
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
        return lastSpf.listTopology(new addrIPv4(), cmd);
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
     * @param cmd entry to find
     * @return tree of spf
     */
    public List<String> showSpfTree(cmds cmd) {
        return lastSpf.listTree(cmd);
    }

    /**
     * show tree
     *
     * @param cmd entry to find
     * @return tree of spf
     */
    public List<String> showSpfOtherTree(cmds cmd) {
        spfCalc<addrIPv4> spf = lastSpf.copyBytes();
        addrIPv4 ned = new addrIPv4();
        ned.fromString(cmd.word());
        spf.doWork(ned);
        return spf.listTree(cmd);
    }

    /**
     * show topology
     *
     * @param cmd entry to find
     * @return log of spf
     */
    public userFormat showSpfOtherTopo(cmds cmd) {
        spfCalc<addrIPv4> spf = lastSpf.copyBytes();
        addrIPv4 ned = new addrIPv4();
        ned.fromString(cmd.word());
        spf.doWork(ned);
        return spf.listTopology(new addrIPv4(), cmd);
    }

    /**
     * show graph
     *
     * @param cmds masks
     * @return graph of spf
     */
    public List<String> showSpfGraph(cmds cmds) {
        return lastSpf.listGraphviz(cmds);
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
     * show non redundant nodes
     *
     * @return necessity list
     */
    public userFormat showNonRedundant() {
        return lastSpf.listNonRedundant();
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
            ntry.prefix = addrPrefix.defaultRoute(fwdCore.ipVersion);
            ntry.best.segrouIdx = segrouIdx;
            ntry.best.rouSrc = segrouPop ? 16 : 0;
            ntry.best.bierIdx = bierIdx;
            ntry.best.bierSub = bierSub;
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
                ntry.best.bierSub = ifc.bierSub;
            } else {
                ntry.best.bierIdx = bierIdx;
                ntry.best.bierSub = bierSub;
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
            ntry.best.bierSub = bierSub;
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
        dat.domain = (cfgAll.domainName + "").replaceAll(" ", "_");
        dat.software = cfgInit.versionAgent.replaceAll(" ", "_");
        dat.hardware = (cfgInit.hwIdNum + " " + cfgInit.getCPUname() + " " + cfgInit.getMemoryInfo()).replaceAll(" ", "_");
        dat.forwarder = cfgInit.getHWfwd1liner().replaceAll(" ", "_");
        dat.middleware = cfgInit.getVMname().replaceAll(" ", "_");
        dat.kernel = cfgInit.getKernelName().replaceAll(" ", "_");
        dat.kernup = bits.time2str(cfgAll.timeZoneName, pipeShell.getKernelUptime() + cfgAll.timeServerOffset, 3).replaceAll(" ", "_");
        if (segrouLab != null) {
            dat.segrouMax = segrouMax;
            dat.segrouBeg = segrouLab[0].label;
        }
        if (bierLab != null) {
            dat.bierMax = bierMax;
            dat.bierLen = bierLen;
            dat.bierBeg = bierLab[0].label;
        }
        for (int i = 0; i < algos.size(); i++) {
            rtrAlgo alg = algos.get(i);
            if (alg == null) {
                continue;
            }
            dat.flexalgo.add(alg.num);
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
        ned &= !dat.rtrId.isEmpty();
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
        spfCalc<addrIPv4> spf = new spfCalc<addrIPv4>(lastSpf);
        for (int i = 0; i < database.size(); i++) {
            rtrLsrpData ntry = database.get(i);
            if (ntry == null) {
                continue;
            }
            ntry.put2spf(spf, distance);
        }
        spf.doWork(routerID);
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
                segrouLab[ifc.segrouIdx].setFwdCommon(tabLabelEntry.owner.lsrpSrgb, fwdCore);
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
        tabRoute<addrIP> tab1 = spf.getRoutes(fwdCore, tabLabelEntry.owner.lsrpSrgb, segrouLab, segrouUsd);
        if (segrouUsd != null) {
            if (segrouIdx > 0) {
                segrouLab[segrouIdx].setFwdCommon(tabLabelEntry.owner.lsrpSrgb, fwdCore);
                tabIndex.add2table(segrouUsd, new tabIndex<addrIP>(segrouIdx, new addrPrefix<addrIP>(new addrIP(), 0)));
            }
            for (int i = 0; i < segrouLab.length; i++) {
                if (segrouUsd.find(new tabIndex<addrIP>(i, null)) != null) {
                    continue;
                }
                segrouLab[i].setFwdDrop(tabLabelEntry.owner.lsrpSrgb);
            }
        }
        tabRoute<addrIP> tab2 = new tabRoute<addrIP>("routes");
        tabRoute.addUpdatedTable(tabRoute.addType.ecmp, rtrBgpUtil.sfiUnicast, 0, tab2, tab1, true, roumapIn, roupolIn, prflstIn);
        routerDoAggregates(rtrBgpUtil.sfiUnicast, tab2, tab2, fwdCore.commonLabel, null, 0);
        if (bierLab != null) {
            tabLabelBier res = spf.getBierI(fwdCore, bierLab[0].label, tabLabelBier.num2bsl(bierLen));
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
                bierLab[i].setBierMpls(tabLabelEntry.owner.lsrpBier, fwdCore, res);
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
        boolean same = tab2.preserveTime(routerComputedU);
        same &= !tabIndex.compareTables(routerComputedI, segrouUsd);
        if (same) {
            return;
        }
        routerComputedU = tab2;
        routerComputedM = tab2;
        routerComputedF = new tabRoute<addrIP>("rx");
        routerComputedI = segrouUsd;
        fwdCore.routerChg(this, false);
        for (int p = 0; p < algos.size(); p++) {
            rtrAlgo alg = algos.get(p);
            if (alg == null) {
                continue;
            }
            spf = lastSpf.copyBytes();
            spf.justFlexAlgo(alg.num);
            spf.doWork(routerID);
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
                    spf.addNextHop(nei.getMetric(), nei.rtrId, nei.peer, ifc.iface, null, null);
                }
            }
            tab1 = spf.getRoutes(fwdCore, null, null, null);
            if (debugger.rtrLsrpEvnt) {
                logger.debug("algo" + alg.num + " unreachable:" + spf.listReachablility(false));
                logger.debug("algo" + alg.num + " reachable:" + spf.listReachablility(true));
            }
            tab2 = new tabRoute<addrIP>("routes");
            tabRoute.addUpdatedTable(tabRoute.addType.ecmp, rtrBgpUtil.sfiUnicast, 0, tab2, tab1, true, roumapIn, roupolIn, prflstIn);
            routerDoAggregates(rtrBgpUtil.sfiUnicast, tab2, tab2, fwdCore.commonLabel, null, 0);
            alg.vrf.update2ip(tab2);
        }
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
        tabLabel.release(segrouLab, tabLabelEntry.owner.lsrpSrgb);
        tabLabel.release(bierLab, tabLabelEntry.owner.lsrpBier);
    }

    /**
     * get help
     *
     * @param l list
     */
    public void routerGetHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "router-id", "specify router id");
        l.add(null, false, 2, new int[]{-1}, "<addr>", "router id");
        l.add(null, false, 1, new int[]{2}, "distance", "specify default distance");
        l.add(null, false, 2, new int[]{-1}, "<num>", "distance");
        l.add(null, false, 1, new int[]{-1}, "default-originate", "advertise default route");
        l.add(null, false, 1, new int[]{2}, "route-map", "process prefixes");
        l.add(null, false, 2, new int[]{-1}, "<name:rm>", "name of route map");
        l.add(null, false, 1, new int[]{2}, "route-policy", "process prefixes");
        l.add(null, false, 2, new int[]{-1}, "<name:rpl>", "name of route policy");
        l.add(null, false, 1, new int[]{2}, "prefix-list", "filter prefixes");
        l.add(null, false, 2, new int[]{-1}, "<name:pl>", "name of prefix list");
        l.add(null, false, 1, new int[]{2}, "database-password", "database password");
        l.add(null, false, 2, new int[]{-1}, "<str>", "password");
        l.add(null, false, 1, new int[]{2}, "refresh", "data refresh time");
        l.add(null, false, 2, new int[]{-1}, "<num>", "age in ms");
        l.add(null, false, 1, new int[]{2}, "lifetime", "data life time");
        l.add(null, false, 2, new int[]{-1}, "<num>", "age in ms");
        l.add(null, false, 1, new int[]{-1}, "spf-bidir", "spf bidir check");
        l.add(null, false, 1, new int[]{2, -1}, "spf-topolog", "spf topology logging");
        l.add(null, false, 2, new int[]{2, -1}, "noappear", "exclude node (dis)appearance");
        l.add(null, false, 2, new int[]{2, -1}, "noconnect", "exclude link (dis)connection");
        l.add(null, false, 2, new int[]{2, -1}, "noforward", "exclude forward (un)willingness");
        l.add(null, false, 2, new int[]{2, -1}, "noreachable", "exclude node (un)reachable");
        l.add(null, false, 2, new int[]{2, -1}, "nometric", "exclude link metric change");
        l.add(null, false, 2, new int[]{2, -1}, "noprefix", "exclude prefix change");
        l.add(null, false, 1, new int[]{-1}, "spf-hops", "spf hops disallow");
        l.add(null, false, 1, new int[]{-1}, "spf-ecmp", "spf ecmp allow");
        l.add(null, false, 1, new int[]{2}, "spf-log", "spf log size");
        l.add(null, false, 2, new int[]{-1}, "<num>", "number of entries");
        l.add(null, false, 1, new int[]{-1}, "ha-mode", "save state");
        l.add(null, false, 1, new int[]{-1}, "stub", "stub router");
        l.add(null, false, 1, new int[]{-1}, "suppress-prefix", "do not advertise interfaces");
        l.add(null, false, 1, new int[]{2}, "segrout", "segment routing parameters");
        l.add(null, false, 2, new int[]{3}, "<num>", "maximum index");
        l.add(null, false, 3, new int[]{4, -1}, "<num>", "this node index");
        l.add(null, false, 4, new int[]{4, -1}, "pop", "advertise php");
        l.add(null, false, 4, new int[]{5}, "base", "specify base");
        l.add(null, false, 5, new int[]{4, -1}, "<num>", "label base");
        l.add(null, false, 1, new int[]{2}, "bier", "bier parameters");
        l.add(null, false, 2, new int[]{3}, "<num>", "bitstring length");
        l.add(null, false, 3, new int[]{4}, "<num>", "maximum index");
        l.add(null, false, 4, new int[]{5, -1}, "<num>", "node index");
        l.add(null, false, 5, new int[]{-1}, "<num>", "node subdomain");
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
        l.add(beg + "distance " + distance);
        l.add(beg + "refresh " + refresh);
        l.add(beg + "lifetime " + lifetime);
        cmds.cfgLine(l, !haMode, beg, "ha-mode", "");
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
        cmds.cfgLine(l, bierMax < 1, beg, "bier", bierLen + " " + bierMax + " " + bierIdx + " " + bierSub);
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
        boolean negated = false;
        if (s.equals(cmds.negated)) {
            s = cmd.word();
            negated = true;
        }
        if (s.equals("router-id")) {
            s = cmd.word();
            routerID.fromString(s);
            cfgIfc ifc = cfgAll.ifcFind(s, 0);
            if (ifc != null) {
                if (ifc.addr4 != null) {
                    routerID.setAddr(ifc.addr4);
                }
            }
            if (negated) {
                routerID = new addrIPv4();
            }
            todo.set(0);
            notif.wakeup();
            return false;
        }
        if (s.equals("flexalgo")) {
            int i = bits.str2num(cmd.word());
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                cmd.error("no such vrf");
                return false;
            }
            rtrAlgo alg = new rtrAlgo(i, fwdCore.ipVersion == 4 ? vrf.fwd4 : vrf.fwd6, routerProtoTyp, routerProcNum);
            if (!negated) {
                algos.add(alg);
                alg.vrf.register2ip();
                todo.set(0);
                notif.wakeup();
                return false;
            }
            alg = algos.del(alg);
            if (alg == null) {
                return false;
            }
            alg.vrf.unregister2ip();
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
        if (s.equals("ha-mode")) {
            haMode = !negated;
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
            tabLabel.release(segrouLab, tabLabelEntry.owner.lsrpSrgb);
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
            segrouLab = tabLabel.allocate(tabLabelEntry.owner.lsrpSrgb, segrouBase, segrouMax);
            todo.set(0);
            notif.wakeup();
            return false;
        }
        if (s.equals("bier")) {
            tabLabel.release(bierLab, tabLabelEntry.owner.lsrpBier);
            bierLab = null;
            if (negated) {
                bierIdx = 0;
                bierSub = 0;
                bierMax = 0;
                bierLen = 0;
                todo.set(0);
                notif.wakeup();
                return false;
            }
            bierLen = tabLabelBier.normalizeBsl(bits.str2num(cmd.word()));
            bierMax = bits.str2num(cmd.word());
            bierIdx = bits.str2num(cmd.word());
            bierSub = bits.str2num(cmd.word());
            bierLab = tabLabel.allocate(tabLabelEntry.owner.lsrpBier, (bierMax + bierLen - 1) / bierLen);
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
        lastSpf.listLinkStates(tab, spfLnkst.protoLsrp, -1, asn, adv, addrIPv4.size, 4);
    }

    /**
     * get state information
     *
     * @param lst list to append
     */
    public void routerStateGet(List<String> lst) {
        if (!haMode) {
            return;
        }
        String a = routerGetName() + " time=1800000 ";
        for (int i = 0; i < database.size(); i++) {
            rtrLsrpData ntry = database.get(i);
            if (ntry == null) {
                continue;
            }
            lst.add(a + ntry.dump(rtrLsrpData.dmpSave));
        }
    }

    /**
     * set state information
     *
     * @param cmd string to append
     * @return true on error, false on success
     */
    public boolean routerStateSet(cmds cmd) {
        rtrLsrpData ntry = new rtrLsrpData();
        if (ntry.fromString(cmd)) {
            return true;
        }
        database.put(ntry);
        return false;
    }

}
