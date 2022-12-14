package net.freertr.rtr;

import java.util.ArrayList;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrIPv4;
import net.freertr.addr.addrIPv6;
import net.freertr.addr.addrPrefix;
import net.freertr.cfg.cfgAll;
import net.freertr.enc.encThrift;
import net.freertr.enc.encThriftEntry;
import net.freertr.ip.ipCor4;
import net.freertr.ip.ipCor6;
import net.freertr.ip.ipFwd;
import net.freertr.ip.ipFwdIface;
import net.freertr.ip.ipRtr;
import net.freertr.pack.packHolder;
import net.freertr.prt.prtUdp;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabRoute;
import net.freertr.tab.tabRouteAttr;
import net.freertr.tab.tabRouteEntry;
import net.freertr.user.userFormat;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.debugger;
import net.freertr.util.logger;
import net.freertr.util.notifier;
import net.freertr.util.state;

/**
 * routing in fat trees (rfcXXX)
 *
 * @author matecsaba
 */
public class rtrRift extends ipRtr implements Runnable {

    /**
     * lie port number
     */
    public static final int portL = 914;

    /**
     * tie port number
     */
    public static final int portT = 915;

    /**
     * packet magic
     */
    public static final int magic = 0xa1f7;

    /**
     * major version
     */
    public static final int version = 6;

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
     * list of interfaces
     */
    protected tabGen<rtrRiftIface> ifaces;

    /**
     * tie database
     */
    protected tabGen<rtrRiftTie> ties;

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
        routerCreateComputed();
        return ntry;
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
        routerCreateComputed();
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
            res = new userFormat("|", "router|name|uptime");
        } else {
            res = new userFormat("|", "iface|router|name|peer|uptime");
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
            if (adr.compare(adr, ifc.peer) == 0) {
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
        if (debugger.rtrRiftEvnt) {
            logger.debug("create table");
        }
        if (nodeID == 0) {
            return;
        }
        tabRoute<addrIP> rou = new tabRoute<addrIP>("adv");
        if (defOrigin) {
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = addrPrefix.defaultRoute(getProtoVer());
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
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        ntry.prefix = addrPrefix.defaultRoute(getProtoVer());
        ned.put(createPrefix(1, Integer.MAX_VALUE, ntry));
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
    }

    private void advertTie(rtrRiftTie tie, boolean forced) {
        rtrRiftTie old = ties.find(tie);
        if (old == null) {
            tie.sequence = 1;
            ties.put(tie);
            return;
        }
        tie.sequence = old.sequence + 1;
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
            th3.putField(3, encThriftEntry.tpI32, ifc.metric);
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
    public void routerGetHelp(userHelping l) {
        l.add(null, "1 2   router-id                   specify node id");
        l.add(null, "2 .     <addr>                    router id");
        l.add(null, "1 2   distance                    specify distance");
        l.add(null, "2 .     <num>                     distance");
        l.add(null, "1 2   level                       specify level");
        l.add(null, "2 .     <num>                     level");
        l.add(null, "1 2   lifetime                    specify tie lifetime");
        l.add(null, "2 .     <num>                     ms");
        l.add(null, "1 .   suppress-prefix             do not advertise interfaces");
        l.add(null, "1 .   default-originate           advertise default route");
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

    public void routerLinkStates(tabRoute<addrIP> tab, int par, int asn, addrIPv4 adv) {
    }

}
