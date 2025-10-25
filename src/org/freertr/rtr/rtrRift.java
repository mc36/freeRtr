package org.freertr.rtr;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrIPv6;
import org.freertr.addr.addrPrefix;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgPrfxlst;
import org.freertr.cfg.cfgRoump;
import org.freertr.cfg.cfgRouplc;
import org.freertr.enc.encThrift;
import org.freertr.enc.encThriftEntry;
import org.freertr.ip.ipCor4;
import org.freertr.ip.ipCor6;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipRtr;
import org.freertr.pack.packHolder;
import org.freertr.prt.prtUdp;
import org.freertr.spf.spfCalc;
import org.freertr.spf.spfLnkst;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabIndex;
import org.freertr.tab.tabIntMatcher;
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
import org.freertr.util.state;

/**
 * routing in fat trees (rfcXXX)
 *
 * @author matecsaba
 */
public class rtrRift extends ipRtr implements Runnable {

    /**
     * lie port number
     */
    public final static int portL = 914;

    /**
     * tie port number
     */
    public final static int portT = 915;

    /**
     * packet magic
     */
    public final static int magic = 0xa1f7;

    /**
     * major version
     */
    public final static int version = 6;

    /**
     * forwarding core
     */
    public final ipFwd fwdCore;

    /**
     * udp core
     */
    protected final prtUdp udpCore;

    /**
     * ip size
     */
    protected int ipSize;

    /**
     * router id
     */
    public long nodeID;

    /**
     * node level
     */
    public int level;

    /**
     * default distance
     */
    public int distance;

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
    protected tabGen<rtrRiftIface> ifaces;

    /**
     * tie database
     */
    protected tabGen<rtrRiftTie> ties;

    /**
     * last north spf
     */
    protected spfCalc<rtrRiftTieSpf> lastSpfN;

    /**
     * last south spf
     */
    protected spfCalc<rtrRiftTieSpf> lastSpfS;

    /**
     * suppress interface addresses
     */
    public boolean suppressAddr = false;

    /**
     * tie lifetime
     */
    public int lifeTime;

    /**
     * advertise default route
     */
    public boolean defOrigin = false;

    /**
     * notified on route change
     */
    protected notifier notif = new notifier();

    private boolean need2run = true;

    /**
     * create one rift process
     *
     * @param forwarder the ip protocol
     * @param udp the udp protocol
     * @param id process id
     */
    public rtrRift(ipFwd forwarder, prtUdp udp, int id) {
        fwdCore = forwarder;
        udpCore = udp;
        nodeID = 0;
        level = 24;
        distance = 100;
        lifeTime = 604800000;
        ifaces = new tabGen<rtrRiftIface>();
        ties = new tabGen<rtrRiftTie>();
        tabRouteAttr.routeType rouTyp = null;
        switch (fwdCore.ipVersion) {
            case ipCor4.protocolVersion:
                rouTyp = tabRouteAttr.routeType.rift4;
                ipSize = ipCor4.size;
                break;
            case ipCor6.protocolVersion:
                rouTyp = tabRouteAttr.routeType.rift6;
                ipSize = ipCor6.size;
                break;
            default:
                break;
        }
        lastSpfN = new spfCalc<rtrRiftTieSpf>(null);
        lastSpfS = new spfCalc<rtrRiftTieSpf>(null);
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
        return "rift on " + fwdCore;
    }

    /**
     * add one interface to work on
     *
     * @param ifc ip forwarder interface
     * @return false if successful, true if error happened
     */
    public rtrRiftIface addInterface(ipFwdIface ifc) {
        if (debugger.rtrRiftEvnt) {
            logger.debug("add iface " + ifc);
        }
        if (ifc == null) {
            return null;
        }
        rtrRiftIface ntry = new rtrRiftIface(this, ifc);
        rtrRiftIface old = ifaces.add(ntry);
        if (old != null) {
            ntry = old;
        }
        ntry.register2udp();
        notif.wakeup();
        return ntry;
    }

    /**
     * list of neighbors
     *
     * @return list
     */
    public userFormat showMetrics() {
        userFormat l = new userFormat("|", "interface|address|nodeid|metric|delay");
        for (int o = 0; o < ifaces.size(); o++) {
            rtrRiftIface ifc = ifaces.get(o);
            if (ifc == null) {
                continue;
            }
            if (!ifc.ready) {
                continue;
            }
            l.add(ifc + "|" + ifc.peer + "|" + ifc.rtrId + "|" + ifc.getMetric() + "|" + ifc.echoParam);
        }
        return l;
    }

    /**
     * delete one interface
     *
     * @param ifc interface to delete
     */
    public void delInterface(ipFwdIface ifc) {
        if (debugger.rtrRiftEvnt) {
            logger.debug("del iface " + ifc);
        }
        if (ifc == null) {
            return;
        }
        rtrRiftIface ntry = new rtrRiftIface(this, ifc);
        ntry = ifaces.del(ntry);
        if (ntry == null) {
            return;
        }
        ntry.unregister2udp();
        notif.wakeup();
    }

    /**
     * list of database
     *
     * @return list
     */
    public userFormat showDatabase() {
        userFormat res = new userFormat("|", "dir|origin|num|typ|seq|left");
        for (int i = 0; i < ties.size(); i++) {
            res.add("" + ties.get(i));
        }
        return res;
    }

    /**
     * list of database
     *
     * @param cmd entry
     * @return list
     */
    public List<String> showDatabase(cmds cmd) {
        rtrRiftTie tie = new rtrRiftTie();
        String a = cmd.word();
        tie.direct = bits.str2num(a);
        if (a.equals("s")) {
            tie.direct = 1;
        }
        if (a.equals("n")) {
            tie.direct = 2;
        }
        tie.origin = bits.str2long(cmd.word());
        tie.number = bits.str2num(cmd.word());
        tie = ties.find(tie);
        if (tie == null) {
            return null;
        }
        List<String> res = new ArrayList<String>();
        encThrift t = new encThrift();
        t.data = tie.elements.elm;
        res.add("direction=" + tie.direct);
        res.add("originator=" + tie.origin);
        res.add("number=" + tie.number);
        res.add("type=" + tie.type);
        res.add("sequence=" + tie.sequence);
        res.add("lifetime=" + bits.timeLeft(tie.expire));
        res.add("header=" + bits.byteDump(dumpElement(tie.putHeader3()), 0, -1));
        res.add("body=" + bits.byteDump(dumpElement(tie.elements), 0, -1));
        res.addAll(t.show());
        return res;
    }

    private byte[] dumpElement(encThriftEntry ntry) {
        encThrift th1 = new encThrift();
        th1.putField(0, encThriftEntry.tpStr, ntry.elm);
        packHolder pck = new packHolder(true, true);
        th1.toPacket(pck);
        return pck.getCopy();
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
            res = new userFormat("|", "nodeid|name|uptime");
        } else {
            res = new userFormat("|", "iface|nodeid|name|peer|uptime");
        }
        for (int i = 0; i < ifaces.size(); i++) {
            rtrRiftIface ifc = ifaces.get(i);
            ifc.showNeighs(res, brief);
        }
        return res;
    }

    /**
     * find one neighbor
     *
     * @param adr address of peer
     * @return neighbor, null if not found
     */
    public rtrRiftIface findNeigh(addrIP adr) {
        for (int i = 0; i < ifaces.size(); i++) {
            rtrRiftIface ifc = ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            if (ifc.peer == null) {
                continue;
            }
            if (adr.compareTo(ifc.peer) == 0) {
                return ifc;
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
            rtrRiftIface ifc = ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            l.add(ifc.iface + "|" + (ifc.ready ? "1" : "0"));
        }
        return l;
    }

    private spfCalc<rtrRiftTieSpf> getSpf(String dir) {
        if (dir.toLowerCase().startsWith("n")) {
            return lastSpfN;
        } else {
            return lastSpfS;
        }
    }

    /**
     * show spf
     *
     * @param dir direction
     * @return log of spf
     */
    public userFormat showSpfStat(String dir) {
        return getSpf(dir).listStatistics();
    }

    /**
     * show topology
     *
     * @param cmd entry to find
     * @return log of spf
     */
    public userFormat showSpfTopo(cmds cmd) {
        spfCalc<rtrRiftTieSpf> doer = getSpf(cmd.word());
        return doer.listTopology(new rtrRiftTieSpf(0), cmd);
    }

    /**
     * show log
     *
     * @param dir direction
     * @return log of spf
     */
    public userFormat showSpfLog(String dir) {
        return getSpf(dir).listUsages();
    }

    /**
     * show tree
     *
     * @param dir direction
     * @param cmd entry to find
     * @return tree of spf
     */
    public List<String> showSpfTree(String dir, cmds cmd) {
        return getSpf(dir).listTree(cmd);
    }

    /**
     * show tree
     *
     * @param cmd entry to find
     * @return tree of spf
     */
    public List<String> showSpfOtherTree(cmds cmd) {
        spfCalc<rtrRiftTieSpf> spf = getSpf(cmd.word()).copyBytes();
        rtrRiftTieSpf ned = new rtrRiftTieSpf(bits.str2long(cmd.word()));
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
        spfCalc<rtrRiftTieSpf> spf = getSpf(cmd.word()).copyBytes();
        rtrRiftTieSpf ned = new rtrRiftTieSpf(bits.str2long(cmd.word()));
        spf.doWork(ned);
        return spf.listTopology(new rtrRiftTieSpf(0), cmd);
    }

    /**
     * show graph
     *
     * @param dir direction
     * @param cmd masks
     * @return graph of spf
     */
    public List<String> showSpfGraph(String dir, cmds cmd) {
        return getSpf(dir).listGraphviz(cmd);
    }

    /**
     * show nh inconsistency
     *
     * @param dir direction
     * @param mtch matcher
     * @return inconsistency list
     */
    public userFormat showNhIncons(String dir, tabIntMatcher mtch) {
        return getSpf(dir).listNhIncons(mtch);
    }

    /**
     * show met inconsistency
     *
     * @param dir direction
     * @param mtch matcher
     * @return inconsistency list
     */
    public userFormat showMetIncons(String dir, tabIntMatcher mtch) {
        return getSpf(dir).listMetIncons(mtch);
    }

    /**
     * show non redundant nodes
     *
     * @param dir direction
     * @return necessity list
     */
    public userFormat showNonRedundant(String dir) {
        return getSpf(dir).listNonRedundant();
    }

    /**
     * show hostnames
     *
     * @param dir direction
     * @return names list
     */
    public userFormat showHostnames(String dir) {
        return getSpf(dir).listHostnames();
    }

    /**
     * create computed
     */
    public synchronized void routerCreateComputed() {
        if (debugger.rtrRiftEvnt) {
            logger.debug("create table");
        }
        if (nodeID == 0) {
            return;
        }
        tabRoute<addrIP> rou = new tabRoute<addrIP>("adv");
        if (defOrigin) {
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = addrPrefix.defaultRoute(fwdCore.ipVersion);
            rou.add(tabRoute.addType.always, ntry, true, true);
        }
        for (int o = 0; o < ifaces.size(); o++) {
            rtrRiftIface ifc = ifaces.get(o);
            if (ifc == null) {
                continue;
            }
            if (ifc.iface.lower.getState() != state.states.up) {
                continue;
            }
            if ((suppressAddr || ifc.suppressAddr) && (!ifc.unsuppressAddr)) {
                continue;
            }
            tabRouteEntry<addrIP> ntry = rou.add(tabRoute.addType.better, ifc.iface.network, null);
            ntry.best.rouTyp = tabRouteAttr.routeType.conn;
            ntry.best.iface = ifc.iface;
            ntry.best.distance = tabRouteAttr.distanIfc;
        }
        for (int i = 0; i < routerRedistedU.size(); i++) {
            tabRouteEntry<addrIP> ntry = routerRedistedU.get(i);
            if (ntry == null) {
                continue;
            }
            ntry = ntry.copyBytes(tabRoute.addType.notyet);
            ntry.best.distance = tabRouteAttr.distanIfc + 1;
            ntry.best.rouSrc = 1;
            rou.add(tabRoute.addType.better, ntry, false, false);
        }
        tabGen<rtrRiftTie> ned = new tabGen<rtrRiftTie>();
        ned.put(createNode(1));
        ned.put(createNode(2));
        for (int i = 0; i < rou.size(); i++) {
            tabRouteEntry<addrIP> ntry = rou.get(i);
            ned.put(createPrefix(1, 2 + i, ntry));
            ned.put(createPrefix(2, 2 + i, ntry));
        }
        if (haveNorthPeer()) {
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = addrPrefix.defaultRoute(fwdCore.ipVersion);
            ned.put(createPrefix(1, Integer.MAX_VALUE, ntry));
        }
        long tim = bits.getTime();
        for (int i = 0; i < ned.size(); i++) {
            rtrRiftTie tie = ned.get(i);
            tie.expire = tim + lifeTime;
            advertTie(tie, false);
        }
        for (int i = ties.size() - 1; i >= 0; i--) {
            rtrRiftTie tie = ties.get(i);
            if (tie == null) {
                continue;
            }
            if (tie.expire < tim) {
                if (debugger.rtrRiftEvnt) {
                    logger.debug("purging " + tie);
                }
                ties.del(tie);
                continue;
            }
            if (tie.origin != nodeID) {
                continue;
            }
            if (ned.find(tie) != null) {
                continue;
            }
            if (tie.isExpired()) {
                continue;
            }
            tie.expire = tim + 30000;
            advertTie(tie, true);
        }
        for (int o = 0; o < ifaces.size(); o++) {
            rtrRiftIface ifc = ifaces.get(o);
            if (ifc == null) {
                continue;
            }
            ifc.notif.wakeup();
        }
        rou = calcSpf(1);
        rou.mergeFrom(tabRoute.addType.ecmp, calcSpf(2), tabRouteAttr.distanLim);
        rou.setProto(routerProtoTyp, routerProcNum);
        if (rou.preserveTime(routerComputedU)) {
            return;
        }
        routerComputedU = rou;
        routerComputedM = new tabRoute<addrIP>("rx");
        routerComputedF = new tabRoute<addrIP>("rx");
        routerComputedI = new tabGen<tabIndex<addrIP>>();
        fwdCore.routerChg(this, false);
    }

    private boolean haveNorthPeer() {
        for (int i = 0; i < ifaces.size(); i++) {
            rtrRiftIface ntry = ifaces.get(i);
            if (ntry == null) {
                continue;
            }
            if (!ntry.ready) {
                continue;
            }
            if (level < ntry.level) {
                return true;
            }
        }
        return false;
    }

    private tabRoute<addrIP> calcSpf(int dir) {
        spfCalc<rtrRiftTieSpf> spf;
        if (dir == 1) {
            spf = new spfCalc<rtrRiftTieSpf>(lastSpfS);
        } else {
            spf = new spfCalc<rtrRiftTieSpf>(lastSpfN);
        }
        for (int o = 0; o < ties.size(); o++) {
            rtrRiftTie tie = ties.get(o);
            if (tie == null) {
                continue;
            }
            if (tie.isExpired()) {
                continue;
            }
            if (tie.direct != dir) {
                continue;
            }
            rtrRiftTieSpf orig = new rtrRiftTieSpf(tie.origin);
            switch (tie.type) {
                case 2:
                    encThriftEntry th1 = tie.elements.getField(1, 0);
                    if (th1 == null) {
                        continue;
                    }
                    encThriftEntry th2 = th1.getField(5, 0); // hostname
                    if (th2 != null) {
                        if (th2.dat != null) {
                            spf.addIdent(orig, new String(th2.dat));
                        }
                    }
                    th2 = th1.getField(2, 0); // neighbors
                    if (th2 == null) {
                        continue;
                    }
                    if (th2.elm == null) {
                        continue;
                    }
                    for (int i = (th2.elm.size() / 2) - 1; i >= 0; i--) {
                        encThriftEntry th3 = th2.elm.get(i * 2);
                        rtrRiftTieSpf peer = new rtrRiftTieSpf(th3.val);
                        th3 = th2.elm.get((i * 2) + 1);
                        encThriftEntry th4 = th3.getField(3, 0);
                        if (th4 == null) {
                            continue;
                        }
                        int met = (int) th4.val;
                        spf.addConn(orig, peer, met, true, false, null);
                    }
                    break;
                case 3:
                    th1 = tie.elements.getField(2, 0);
                    if (th1 == null) {
                        continue;
                    }
                    th2 = th1.getField(1, 0); // prefixes
                    if (th2 == null) {
                        continue;
                    }
                    if (th2.elm == null) {
                        continue;
                    }
                    for (int i = (th2.elm.size() / 2) - 1; i >= 0; i--) {
                        encThriftEntry th3 = th2.elm.get(i * 2);
                        addrPrefix<addrIP> pfx = null;
                        encThriftEntry th4 = th3.getField(1, 0); // ipv4
                        if (th4 != null) {
                            encThriftEntry th5 = th4.getField(1, 0); // address
                            if (th5 == null) {
                                continue;
                            }
                            addrIPv4 adr4 = new addrIPv4();
                            bits.msbPutD(adr4.getBytes(), 0, (int) th5.val);
                            th5 = th4.getField(2, 0); // mask
                            if (th5 == null) {
                                continue;
                            }
                            pfx = addrPrefix.ip4toIP(new addrPrefix<addrIPv4>(adr4, (int) th5.val));
                        }
                        th4 = th3.getField(2, 0); // ipv6
                        if (th4 != null) {
                            encThriftEntry th5 = th4.getField(1, 0); // address
                            if (th5 == null) {
                                continue;
                            }
                            if (th5.dat == null) {
                                continue;
                            }
                            addrIPv6 adr6 = new addrIPv6();
                            adr6.fromBuf(th5.dat, 0);
                            th5 = th4.getField(2, 0); // mask
                            if (th5 == null) {
                                continue;
                            }
                            pfx = addrPrefix.ip6toIP(new addrPrefix<addrIPv6>(adr6, (int) th5.val));
                        }
                        if (pfx == null) {
                            continue;
                        }
                        th3 = th2.elm.get((i * 2) + 1);
                        th4 = th3.getField(2, 0);
                        if (th4 == null) {
                            continue;
                        }
                        int met = (int) th4.val;
                        int tag = 0;
                        th4 = th3.getField(3, 0);
                        if (th4 != null) {
                            tag = (int) th4.val;
                        }
                        tabRouteEntry<addrIP> rou = new tabRouteEntry<addrIP>();
                        rou.prefix = pfx;
                        rou.best.distance = distance;
                        rou.best.metric = met;
                        rou.best.tag = tag;
                        spf.addPref(orig, rou, false);
                    }
                    break;
            }
        }
        rtrRiftTieSpf adr = new rtrRiftTieSpf(nodeID);
        spf.doWork(adr);
        for (int o = 0; o < ifaces.size(); o++) {
            rtrRiftIface ifc = ifaces.get(o);
            if (ifc == null) {
                continue;
            }
            if (ifc.iface.lower.getState() != state.states.up) {
                continue;
            }
            if (!ifc.ready) {
                continue;
            }
            bits.msbPutQ(adr.getBytes(), 0, ifc.rtrId);
            spf.addNextHop(ifc.getMetric(), adr, ifc.peer, ifc.iface, null, null);
        }
        tabRoute<addrIP> rou1 = spf.getRoutes(fwdCore, null, null, null);
        tabRoute<addrIP> rou2 = new tabRoute<addrIP>("rou");
        tabRoute.addUpdatedTable(tabRoute.addType.ecmp, rtrBgpUtil.sfiUnicast, 0, rou2, rou1, true, roumapIn, roupolIn, prflstIn);
        routerDoAggregates(rtrBgpUtil.sfiUnicast, rou2, rou2, fwdCore.commonLabel, null, 0);
        if (dir == 1) {
            lastSpfS = spf;
        } else {
            lastSpfN = spf;
        }
        return rou2;
    }

    private void advertTie(rtrRiftTie tie, boolean forced) {
        rtrRiftTie old = ties.find(tie);
        if (old == null) {
            tie.sequence = 1;
            ties.put(tie);
            return;
        }
        tie.sequence = old.sequence + 1;
        forced |= old.isExpired() != tie.isExpired();
        if (forced) {
            ties.put(tie);
            return;
        }
        byte[] buf1 = dumpElement(tie.elements);
        byte[] buf2 = dumpElement(old.elements);
        if (buf1.length == buf2.length) {
            if (bits.byteComp(buf1, 0, buf2, 0, buf1.length) == 0) {
                return;
            }
        }
        ties.put(tie);
    }

    private rtrRiftTie createPrefix(int dir, int num, tabRouteEntry<addrIP> ntry) {
        rtrRiftTie tie = new rtrRiftTie();
        tie.direct = dir;
        tie.number = num;
        tie.origin = nodeID;
        tie.type = 3;
        encThriftEntry th2 = new encThriftEntry();
        if (ntry.prefix.network.isIPv4()) {
            encThriftEntry th3 = new encThriftEntry();
            encThriftEntry th4 = new encThriftEntry();
            addrPrefix<addrIPv4> pfx = addrPrefix.ip2ip4(ntry.prefix);
            th4.putField(1, encThriftEntry.tpI32, bits.msbGetD(pfx.network.getBytes(), 0));
            th4.putField(2, encThriftEntry.tpI8, pfx.maskLen);
            th3.putField(1, encThriftEntry.tpStr, th4.elm);
            th2.putField(0, encThriftEntry.tpStr, th3.elm);
        } else {
            encThriftEntry th3 = new encThriftEntry();
            encThriftEntry th4 = new encThriftEntry();
            addrPrefix<addrIPv6> pfx = addrPrefix.ip2ip6(ntry.prefix);
            th4.putField(1, encThriftEntry.tpBin, pfx.network.getBytes());
            th4.putField(2, encThriftEntry.tpI8, pfx.maskLen);
            th3.putField(2, encThriftEntry.tpStr, th4.elm);
            th2.putField(0, encThriftEntry.tpStr, th3.elm);
        }
        encThriftEntry th3 = new encThriftEntry();
        th3.putField(2, encThriftEntry.tpI32, ntry.best.metric);
        th3.putField(3, encThriftEntry.tpI32, ntry.best.tag);
        th2.putField(0, encThriftEntry.tpStr, th3.elm);
        encThriftEntry th1 = new encThriftEntry();
        th1.putField(1, encThriftEntry.tpMap, th2.elm);
        th1.putTypKV(encThriftEntry.tpStr, encThriftEntry.tpStr);
        tie.elements = new encThriftEntry();
        tie.elements.putField(2, encThriftEntry.tpStr, th1.elm); // prefix
        return tie;
    }

    private rtrRiftTie createNode(int dir) {
        rtrRiftTie tie = new rtrRiftTie();
        tie.direct = dir;
        tie.number = 1;
        tie.origin = nodeID;
        tie.type = 2;
        encThriftEntry th1 = new encThriftEntry();
        th1.putField(1, encThriftEntry.tpI8, level);
        encThriftEntry th2 = new encThriftEntry();
        th2.elm = new ArrayList<encThriftEntry>();
        for (int i = 0; i < ifaces.size(); i++) {
            rtrRiftIface ifc = ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            if (!ifc.ready) {
                continue;
            }
            th2.putField(i, encThriftEntry.tpI64, ifc.rtrId);
            encThriftEntry th3 = new encThriftEntry();
            th3.putField(1, encThriftEntry.tpI8, ifc.level);
            th3.putField(3, encThriftEntry.tpI32, ifc.getMetric());
            encThriftEntry th4 = new encThriftEntry();
            th4.putField(1, encThriftEntry.tpI32, ifc.iface.ifwNum);
            th4.putField(2, encThriftEntry.tpI32, ifc.lnkId);
            encThriftEntry th5 = new encThriftEntry();
            th5.putField(0, encThriftEntry.tpStr, th4.elm);
            th3.putField(4, encThriftEntry.tpSet, th5.elm); // links
            th3.putTypKV(encThriftEntry.tpStr, encThriftEntry.tpStr);
            th3.putField(5, encThriftEntry.tpI32, -1); // bandwidth
            th2.putField(i, encThriftEntry.tpStr, th3.elm);
        }
        th1.putField(2, encThriftEntry.tpMap, th2.elm); // neighbors
        th1.putTypKV(encThriftEntry.tpI64, encThriftEntry.tpStr);
        th2 = new encThriftEntry();
        th2.putField(1, encThriftEntry.tpI16, 1); // minor
        th1.putField(3, encThriftEntry.tpStr, th2.elm);
        th1.putField(5, encThriftEntry.tpBin, cfgAll.hostName.getBytes());
        tie.elements = new encThriftEntry();
        tie.elements.putField(1, encThriftEntry.tpStr, th1.elm); // node
        return tie;
    }

    /**
     * redistribution changed
     */
    public void routerRedistChanged() {
        notif.wakeup();
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
        if (debugger.rtrRiftEvnt) {
            logger.debug("shutdown");
        }
        need2run = false;
        for (int i = 0; i < ifaces.size(); i++) {
            rtrRiftIface ifc = ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            ifc.unregister2udp();
        }
    }

    /**
     * get help
     *
     * @param l list
     */
    public void routerGetHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "router-id", "specify node id");
        l.add(null, false, 2, new int[]{-1}, "<addr>", "router id");
        l.add(null, false, 1, new int[]{2}, "distance", "specify distance");
        l.add(null, false, 2, new int[]{-1}, "<num>", "distance");
        l.add(null, false, 1, new int[]{2}, "level", "specify level");
        l.add(null, false, 2, new int[]{-1}, "<num>", "level");
        l.add(null, false, 1, new int[]{2}, "lifetime", "specify tie lifetime");
        l.add(null, false, 2, new int[]{-1}, "<num>", "ms");
        l.add(null, false, 1, new int[]{-1}, "suppress-prefix", "do not advertise interfaces");
        l.add(null, false, 1, new int[]{-1}, "default-originate", "advertise default route");
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
        l.add(null, false, 1, new int[]{2}, "route-map", "process prefixes");
        l.add(null, false, 2, new int[]{-1}, "<name:rm>", "name of route map");
        l.add(null, false, 1, new int[]{2}, "route-policy", "process prefixes");
        l.add(null, false, 2, new int[]{-1}, "<name:rpl>", "name of route policy");
        l.add(null, false, 1, new int[]{2}, "prefix-list", "filter prefixes");
        l.add(null, false, 2, new int[]{-1}, "<name:pl>", "name of prefix list");
    }

    /**
     * get config
     *
     * @param l list
     * @param beg beginning
     * @param filter filter
     */
    public void routerGetConfig(List<String> l, String beg, int filter) {
        l.add(beg + "router-id " + nodeID);
        l.add(beg + "level " + level);
        l.add(beg + "distance " + distance);
        l.add(beg + "lifetime " + lifeTime);
        cmds.cfgLine(l, !suppressAddr, beg, "suppress-prefix", "");
        cmds.cfgLine(l, !defOrigin, beg, "default-originate", "");
        l.add(beg + "spf-log " + lastSpfN.logSize);
        cmds.cfgLine(l, lastSpfN.topoLog.get() == 0, beg, "spf-topolog", lastSpfN.getTopoLogMode());
        cmds.cfgLine(l, lastSpfN.bidir.get() == 0, beg, "spf-bidir", "");
        cmds.cfgLine(l, lastSpfN.hops.get() == 0, beg, "spf-hops", "");
        cmds.cfgLine(l, lastSpfN.ecmp.get() == 0, beg, "spf-ecmp", "");
        cmds.cfgLine(l, prflstIn == null, beg, "prefix-list", "" + prflstIn);
        cmds.cfgLine(l, roumapIn == null, beg, "route-map", "" + roumapIn);
        cmds.cfgLine(l, roupolIn == null, beg, "route-policy", "" + roupolIn);
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
            nodeID = bits.str2long(cmd.word());
            if (negated) {
                nodeID = 0;
            }
            return false;
        }
        if (s.equals("level")) {
            level = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("distance")) {
            distance = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("lifetime")) {
            lifeTime = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("suppress-prefix")) {
            suppressAddr = !negated;
            notif.wakeup();
            return false;
        }
        if (s.equals("default-originate")) {
            defOrigin = !negated;
            notif.wakeup();
            return false;
        }
        if (s.equals("spf-log")) {
            s = cmd.word();
            lastSpfN.logSize.set(bits.str2num(s));
            lastSpfS.logSize.set(bits.str2num(s));
            if (negated) {
                lastSpfN.logSize.set(0);
                lastSpfS.logSize.set(0);
            }
            return false;
        }
        if (s.equals("spf-topolog")) {
            if (negated) {
                lastSpfN.topoLog.set(0);
                lastSpfS.topoLog.set(0);
                return false;
            }
            lastSpfN.setTopoLogMode(cmd.copyBytes(false));
            lastSpfS.setTopoLogMode(cmd.copyBytes(false));
            return false;
        }
        if (s.equals("spf-bidir")) {
            if (negated) {
                lastSpfN.bidir.set(0);
                lastSpfS.bidir.set(0);
            } else {
                lastSpfN.bidir.set(1);
                lastSpfS.bidir.set(1);
            }
            notif.wakeup();
            return false;
        }
        if (s.equals("spf-hops")) {
            if (negated) {
                lastSpfN.hops.set(0);
                lastSpfS.hops.set(0);
            } else {
                lastSpfN.hops.set(1);
                lastSpfS.hops.set(1);
            }
            notif.wakeup();
            return false;
        }
        if (s.equals("spf-ecmp")) {
            if (negated) {
                lastSpfN.ecmp.set(0);
                lastSpfS.ecmp.set(0);
            } else {
                lastSpfN.ecmp.set(1);
                lastSpfS.ecmp.set(1);
            }
            notif.wakeup();
            return false;
        }
        if (s.equals("prefix-list")) {
            if (negated) {
                prflstIn = null;
                notif.wakeup();
                return false;
            }
            cfgPrfxlst ntry = cfgAll.prfxFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such prefix list");
                return false;
            }
            prflstIn = ntry.prflst;
            notif.wakeup();
            return false;
        }
        if (s.equals("route-map")) {
            if (negated) {
                roumapIn = null;
                notif.wakeup();
                return false;
            }
            cfgRoump ntry = cfgAll.rtmpFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such route map");
                return false;
            }
            roumapIn = ntry.roumap;
            notif.wakeup();
            return false;
        }
        if (s.equals("route-policy")) {
            if (negated) {
                roupolIn = null;
                notif.wakeup();
                return false;
            }
            cfgRouplc ntry = cfgAll.rtplFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such route policy");
                return false;
            }
            roupolIn = ntry.rouplc;
            notif.wakeup();
            return false;
        }
        return true;
    }

    public void run() {
        for (;;) {
            notif.misleep(30000);
            if (!need2run) {
                return;
            }
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
            o += ifaces.get(i).ready ? 1 : 0;
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
            rtrRiftIface ifc = ifaces.get(o);
            if (ifc == null) {
                continue;
            }
            if (ifc.iface.lower.getState() != state.states.up) {
                continue;
            }
            if (!ifc.ready) {
                continue;
            }
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = new addrPrefix<addrIP>(ifc.peer, addrIP.size * 8);
            tabRoute.addUpdatedEntry(tabRoute.addType.better, tab, rtrBgpUtil.sfiUnicast, 0, ntry, true, null, null, routerAutoMesh);
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
        lastSpfN.listLinkStates(tab, spfLnkst.protoRift, -1, asn, adv, addrIPv4.size, 4);
    }

    /**
     * get state information
     *
     * @param lst list to append
     */
    public void routerStateGet(List<String> lst) {
    }

    /**
     * set state information
     *
     * @param cmd string to append
     * @return true on error, false on success
     */
    public boolean routerStateSet(cmds cmd) {
        return true;
    }

}
