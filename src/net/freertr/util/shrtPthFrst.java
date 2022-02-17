package net.freertr.util;

import java.util.ArrayList;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrIPv4;
import net.freertr.addr.addrPrefix;
import net.freertr.addr.addrType;
import net.freertr.cfg.cfgAll;
import net.freertr.cry.cryHashCrc32;
import net.freertr.cry.cryHashMd5;
import net.freertr.ip.ipFwd;
import net.freertr.ip.ipFwdIface;
import net.freertr.ip.ipMpls;
import net.freertr.pack.packHolder;
import net.freertr.rtr.rtrBgpUtil;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabIndex;
import net.freertr.tab.tabIntMatcher;
import net.freertr.tab.tabLabel;
import net.freertr.tab.tabLabelBier;
import net.freertr.tab.tabLabelBierN;
import net.freertr.tab.tabLabelEntry;
import net.freertr.tab.tabRoute;
import net.freertr.tab.tabRouteAttr;
import net.freertr.tab.tabRouteEntry;
import net.freertr.tab.tabRouteIface;
import net.freertr.user.userFormat;

/**
 * dijkstra's shortest path first
 *
 * @param <Ta> type of nodes
 * @author matecsaba
 */
public class shrtPthFrst<Ta extends addrType> {

    /**
     * beginning of graph
     */
    public final static String graphBeg1 = "sfdp -Tpng > net.png << EOF";

    /**
     * beginning of graph
     */
    public final static String graphBeg2 = "graph net {";

    /**
     * beginning of graph
     */
    public final static String graphBeg3 = "node [fontname=ubuntu,shape=none,labelloc=b,image=\"../misc/router.svg\"] edge [fontname=ubuntu,shape=none]";

    /**
     * ending of graph
     */
    public final static String graphEnd1 = "}";

    /**
     * ending of graph
     */
    public final static String graphEnd2 = "EOF";

    private final tabGen<shrtPthFrstNode<Ta>> nodes;

    private final List<shrtPthFrstLog> log = new ArrayList<shrtPthFrstLog>();

    private final int count;

    private final long tim1;

    private long tim2;

    private long tim3;

    private long tim4;

    private shrtPthFrstNode<Ta> spfRoot;

    private shrtPthFrst<Ta> prev;

    /**
     * log size
     */
    public final syncInt logSize;

    /**
     * log topology changes: 0x1=(dis)appear, 0x2=connect, 0x4=forward,
     * 0x8=(un)reachable, 0x10=metric, 0x20=prefix
     */
    public final syncInt topoLog;

    /**
     * bidir check
     */
    public final syncInt bidir;

    /**
     * consider ecmp
     */
    public final syncInt ecmp;

    /**
     * consider hops in ecmp
     */
    public final syncInt hops;

    /**
     * construct spf
     *
     * @param old old spf
     */
    public shrtPthFrst(shrtPthFrst<Ta> old) {
        tim1 = bits.getTime();
        nodes = new tabGen<shrtPthFrstNode<Ta>>();
        if (old == null) {
            count = 1;
            logSize = new syncInt(0);
            topoLog = new syncInt(0);
            bidir = new syncInt(0);
            hops = new syncInt(0);
            ecmp = new syncInt(0);
            return;
        }
        log.addAll(old.log);
        logSize = old.logSize;
        topoLog = old.topoLog;
        bidir = old.bidir;
        hops = old.hops;
        ecmp = old.ecmp;
        count = old.count + 1;
        if (topoLog.get() > 0) {
            prev = old;
        }
        shrtPthFrstLog ntry = new shrtPthFrstLog();
        ntry.when = old.tim1;
        ntry.tim = (int) (old.tim4 - old.tim1);
        ntry.unreach = old.listReachablility(false);
        ntry.topo = old.listTopoHsh();
        log.add(ntry);
        int max = logSize.get();
        for (; log.size() > max;) {
            log.remove(0);
        }
    }

    /**
     * get log mode
     *
     * @return mode
     */
    public String getTopoLogMode() {
        String a = "";
        int mod = topoLog.get();
        if ((mod & 0x1) == 0) {
            a += "noappear";
        }
        if ((mod & 0x2) == 0) {
            a += "noconnect";
        }
        if ((mod & 0x4) == 0) {
            a += "noforward";
        }
        if ((mod & 0x8) == 0) {
            a += "noreachable";
        }
        if ((mod & 0x10) == 0) {
            a += "nometric";
        }
        if ((mod & 0x20) == 0) {
            a += "noprefix";
        }
        return a;
    }

    /**
     * get log mode
     *
     * @param cmd commands
     */
    public void setTopoLogMode(cmds cmd) {
        int mod = 0xffff;
        for (;;) {
            String a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            if (a.equals("noappear")) {
                mod &= ~0x1;
                continue;
            }
            if (a.equals("noconnect")) {
                mod &= ~0x2;
                continue;
            }
            if (a.equals("noforward")) {
                mod &= ~0x4;
                continue;
            }
            if (a.equals("noreachable")) {
                mod &= ~0x8;
                continue;
            }
            if (a.equals("nometric")) {
                mod &= ~0x10;
                continue;
            }
            if (a.equals("noprefix")) {
                mod &= ~0x20;
                continue;
            }
        }
        topoLog.set(mod);
    }

    /**
     * copy topology
     *
     * @return copy
     */
    public shrtPthFrst<Ta> copyBytes() {
        shrtPthFrst<Ta> res = new shrtPthFrst<Ta>(this);
        for (int o = 0; o < nodes.size(); o++) {
            shrtPthFrstNode<Ta> nod = nodes.get(o);
            for (int i = 0; i < nod.conn.size(); i++) {
                shrtPthFrstConn<Ta> con = nod.conn.get(i);
                res.addConn(nod.name, con.target.name, con.metric, con.realHop, con.stub, con.ident);
            }
            for (int i = 0; i < nod.prfAdd.size(); i++) {
                res.addPref(nod.name, nod.prfAdd.get(i), false);
            }
            for (int i = 0; i < nod.prfFix.size(); i++) {
                res.addPref(nod.name, nod.prfFix.get(i), true);
            }
            for (int i = 0; i < nod.othAdd.size(); i++) {
                res.addOpref(nod.name, nod.othAdd.get(i), false);
            }
            for (int i = 0; i < nod.othFix.size(); i++) {
                res.addOpref(nod.name, nod.othFix.get(i), true);
            }
            res.addIdent(nod.name, nod.ident);
            res.addSegRouB(nod.name, nod.srBeg);
            res.addSegRouI(nod.name, nod.srIdx);
            res.addBierB(nod.name, nod.brBeg);
            res.addBierI(nod.name, nod.brIdx);
        }
        return res;
    }

    /**
     * add one connection
     *
     * @param from source node
     * @param to target node
     * @param metric metric
     * @param realHop true if hop, false if network
     * @param stub stub adjacency
     * @param ident link id
     */
    public void addConn(Ta from, Ta to, int metric, boolean realHop, boolean stub, String ident) {
        if (metric < 0) {
            metric = 0;
        }
        shrtPthFrstNode<Ta> ntry = new shrtPthFrstNode<Ta>(to);
        shrtPthFrstNode<Ta> old = nodes.add(ntry);
        if (old != null) {
            ntry = old;
        }
        shrtPthFrstConn<Ta> c = new shrtPthFrstConn<Ta>();
        c.metric = metric;
        c.target = ntry;
        c.realHop = realHop;
        c.stub = stub;
        c.ident = ident;
        ntry = new shrtPthFrstNode<Ta>(from);
        old = nodes.add(ntry);
        if (old != null) {
            ntry = old;
        }
        ntry.conn.add(c);
    }

    /**
     * add next hop
     *
     * @param met metric of interface
     * @param nod node to add
     * @param hop hop to add
     * @param ifc interface number
     * @param ohop other hop to add
     * @param oifc other interface number
     */
    public void addNextHop(int met, Ta nod, addrIP hop, tabRouteIface ifc, addrIP ohop, tabRouteIface oifc) {
        shrtPthFrstNode<Ta> ntry = new shrtPthFrstNode<Ta>(nod);
        ntry = nodes.find(ntry);
        if (ntry == null) {
            return;
        }
        if (ntry.uplinks == null) {
            return;
        }
        if (hop != null) {
            hop = hop.copyBytes();
        }
        if (ohop != null) {
            ohop = ohop.copyBytes();
        }
        if (met > ntry.nxtMet) {
            return;
        }
        if (met < ntry.nxtMet) {
            for (int i = 0; i < ntry.uplinks.size(); i++) {
                shrtPthFrstRes<Ta> upl = ntry.uplinks.get(i);
                upl.nxtHop = null;
                upl.iface = null;
                upl.othHop = null;
                upl.oface = null;
            }
            ntry.nxtMet = met;
        }
        for (int i = 0; i < ntry.uplinks.size(); i++) {
            shrtPthFrstRes<Ta> upl = ntry.uplinks.get(i);
            if (upl.hops > 1) {
                continue;
            }
            if (upl.iface != null) {
                continue;
            }
            upl.nxtHop = hop;
            upl.iface = ifc;
            upl.othHop = ohop;
            upl.oface = oifc;
            return;
        }
    }

    /**
     * add prefix
     *
     * @param nod node to add
     * @param rou route
     * @param fix fixed metric
     */
    public void addPref(Ta nod, tabRouteEntry<addrIP> rou, boolean fix) {
        shrtPthFrstNode<Ta> ntry = new shrtPthFrstNode<Ta>(nod);
        shrtPthFrstNode<Ta> old = nodes.add(ntry);
        if (old != null) {
            ntry = old;
        }
        if (fix) {
            ntry.prfFix.add(tabRoute.addType.ecmp, rou, false, false);
        } else {
            ntry.prfAdd.add(tabRoute.addType.ecmp, rou, false, false);
        }
    }

    /**
     * add other prefix
     *
     * @param nod node to add
     * @param rou route
     * @param fix fixed metric
     */
    public void addOpref(Ta nod, tabRouteEntry<addrIP> rou, boolean fix) {
        shrtPthFrstNode<Ta> ntry = new shrtPthFrstNode<Ta>(nod);
        shrtPthFrstNode<Ta> old = nodes.add(ntry);
        if (old != null) {
            ntry = old;
        }
        if (fix) {
            ntry.othFix.add(tabRoute.addType.ecmp, rou, false, false);
        } else {
            ntry.othAdd.add(tabRoute.addType.ecmp, rou, false, false);
        }
    }

    /**
     * add ident
     *
     * @param nod node to add
     * @param ident link id
     */
    public void addIdent(Ta nod, String ident) {
        if (ident == null) {
            return;
        }
        shrtPthFrstNode<Ta> ntry = new shrtPthFrstNode<Ta>(nod);
        shrtPthFrstNode<Ta> old = nodes.add(ntry);
        if (old != null) {
            ntry = old;
        }
        ntry.ident = ident;
    }

    /**
     * add segment routing base
     *
     * @param nod node to add
     * @param beg base label
     */
    public void addSegRouB(Ta nod, int beg) {
        if (beg < 1) {
            return;
        }
        shrtPthFrstNode<Ta> ntry = new shrtPthFrstNode<Ta>(nod);
        shrtPthFrstNode<Ta> old = nodes.add(ntry);
        if (old != null) {
            ntry = old;
        }
        if (ntry.srBeg != 0) {
            return;
        }
        ntry.srBeg = beg;
    }

    /**
     * add segment routing index
     *
     * @param nod node to add
     * @param idx node index
     */
    public void addSegRouI(Ta nod, int idx) {
        if (idx < 1) {
            return;
        }
        shrtPthFrstNode<Ta> ntry = new shrtPthFrstNode<Ta>(nod);
        shrtPthFrstNode<Ta> old = nodes.add(ntry);
        if (old != null) {
            ntry = old;
        }
        ntry.srIdx = idx;
    }

    /**
     * add segment routing index
     *
     * @param nod node to add
     * @param pref prefix to update
     * @param idx node index
     * @param src rouSrc
     */
    public void addSegRouI(Ta nod, addrPrefix<addrIP> pref, int idx, int src) {
        if (idx < 1) {
            return;
        }
        shrtPthFrstNode<Ta> ntry = new shrtPthFrstNode<Ta>(nod);
        shrtPthFrstNode<Ta> old = nodes.add(ntry);
        if (old != null) {
            ntry = old;
        }
        ntry.srIdx = idx;
        tabRouteEntry<addrIP> rou;
        rou = ntry.prfFix.find(pref);
        if (rou != null) {
            rou.best.segrouIdx = idx;
            rou.best.rouSrc |= src;
        }
        rou = ntry.prfAdd.find(pref);
        if (rou != null) {
            rou.best.segrouIdx = idx;
            rou.best.rouSrc |= src;
        }
        rou = ntry.othFix.find(pref);
        if (rou != null) {
            rou.best.segrouIdx = idx;
            rou.best.rouSrc |= src;
        }
        rou = ntry.othAdd.find(pref);
        if (rou != null) {
            rou.best.segrouIdx = idx;
            rou.best.rouSrc |= src;
        }
    }

    /**
     * add bier base
     *
     * @param nod node to add
     * @param beg base label
     */
    public void addBierB(Ta nod, int beg) {
        if (beg < 1) {
            return;
        }
        shrtPthFrstNode<Ta> ntry = new shrtPthFrstNode<Ta>(nod);
        shrtPthFrstNode<Ta> old = nodes.add(ntry);
        if (old != null) {
            ntry = old;
        }
        if (ntry.brBeg != 0) {
            return;
        }
        ntry.brBeg = beg;
    }

    /**
     * add bier index
     *
     * @param nod node to add
     * @param idx node index
     */
    public void addBierI(Ta nod, int idx) {
        if (idx < 1) {
            return;
        }
        shrtPthFrstNode<Ta> ntry = new shrtPthFrstNode<Ta>(nod);
        shrtPthFrstNode<Ta> old = nodes.add(ntry);
        if (old != null) {
            ntry = old;
        }
        ntry.brIdx = idx;
        ntry.brLst.add(new shrtPthFrstIdx(idx));
    }

    /**
     * add bier index
     *
     * @param nod node to add
     * @param pref prefix
     * @param idx node index
     * @param hdr header
     */
    public void addBierI(Ta nod, addrPrefix<addrIP> pref, int idx, int hdr) {
        if (idx < 1) {
            return;
        }
        shrtPthFrstNode<Ta> ntry = new shrtPthFrstNode<Ta>(nod);
        shrtPthFrstNode<Ta> old = nodes.add(ntry);
        if (old != null) {
            ntry = old;
        }
        ntry.brIdx = idx;
        ntry.brLst.add(new shrtPthFrstIdx(idx));
        tabRouteEntry<addrIP> rou;
        rou = ntry.prfFix.find(pref);
        if (rou != null) {
            rou.best.bierIdx = idx;
            rou.best.bierHdr = hdr;
        }
        rou = ntry.prfAdd.find(pref);
        if (rou != null) {
            rou.best.bierIdx = idx;
            rou.best.bierHdr = hdr;
        }
        rou = ntry.othFix.find(pref);
        if (rou != null) {
            rou.best.bierIdx = idx;
            rou.best.bierHdr = hdr;
        }
        rou = ntry.othAdd.find(pref);
        if (rou != null) {
            rou.best.bierIdx = idx;
            rou.best.bierHdr = hdr;
        }
    }

    private void diffPrefix(shrtPthFrstNode<Ta> nod, tabRoute<addrIP> cl, tabRoute<addrIP> ol) {
        for (int i = 0; i < cl.size(); i++) {
            tabRouteEntry<addrIP> cr = cl.get(i);
            tabRouteEntry<addrIP> or = ol.find(cr);
            if (or == null) {
                logger.info("prefix " + addrPrefix.ip2str(cr.prefix) + " appeared at " + nod);
                continue;
            }
            if (cr.best.metric != or.best.metric) {
                logger.info("prefix " + addrPrefix.ip2str(cr.prefix) + " metric changed at " + nod + " from " + or.best.metric + " to " + cr.best.metric);
                continue;
            }
            if (cr.best.tag != or.best.tag) {
                logger.info("prefix " + addrPrefix.ip2str(cr.prefix) + " tag changed at " + nod + " from " + or.best.tag + " to " + cr.best.tag);
                continue;
            }
        }
        for (int i = 0; i < ol.size(); i++) {
            tabRouteEntry<addrIP> or = ol.get(i);
            tabRouteEntry<addrIP> cr = cl.find(or);
            if (cr == null) {
                logger.info("prefix " + addrPrefix.ip2str(or.prefix) + " lost at " + nod);
                continue;
            }
        }
    }

    /**
     * find shortest path
     *
     * @param from starting node
     * @param to target node, null to every node
     * @return false on success, true on error
     */
    public boolean doCalc(Ta from, Ta to) {
        tim2 = bits.getTime();
        for (int i = 0; i < nodes.size(); i++) {
            shrtPthFrstNode<Ta> ntry = nodes.get(i);
            if (ntry == null) {
                continue;
            }
            ntry.uplink = null;
            ntry.uplinks = null;
            ntry.result = null;
            ntry.metric = Integer.MAX_VALUE;
            ntry.nxtMet = Integer.MAX_VALUE;
            ntry.visited = false;
        }
        shrtPthFrstNode<Ta> ntry = nodes.find(new shrtPthFrstNode<Ta>(from));
        if (ntry == null) {
            prev = null;
            return true;
        }
        spfRoot = ntry;
        tabGen<shrtPthFrstNode<Ta>> lst = new tabGen<shrtPthFrstNode<Ta>>();
        ntry.metric = 0;
        ntry.visited = true;
        lst.add(ntry);
        boolean frst = true;
        boolean bid = bidir.get() != 0;
        boolean ecm = ecmp.get() != 0;
        boolean hps = hops.get() != 0;
        boolean res = true;
        for (;;) {
            if (lst.size() < 1) {
                break;
            }
            ntry = lst.get(0);
            for (int i = 1; i < lst.size(); i++) {
                shrtPthFrstNode<Ta> cur = lst.get(i);
                if (cur.metric < ntry.metric) {
                    ntry = cur;
                }
            }
            if (to != null) {
                if (to.compare(to, ntry.name) == 0) {
                    res = false;
                    break;
                }
            }
            lst.del(ntry);
            ntry.visited = true;
            for (int i = 0; i < ntry.conn.size(); i++) {
                shrtPthFrstConn<Ta> c = ntry.conn.get(i);
                if (c == null) {
                    continue;
                }
                if ((!frst) && c.stub) {
                    continue;
                }
                if (bid) {
                    if (c.target.findConn(ntry, -1) == null) {
                        continue;
                    }
                }
                int o = ntry.metric + c.metric;
                if (c.target.metric < o) {
                    continue;
                }
                int p;
                if (frst) {
                    p = 0;
                } else {
                    p = ntry.uplink.hops;
                }
                if (c.realHop) {
                    p++;
                }
                shrtPthFrstRes<Ta> upl = new shrtPthFrstRes<Ta>(ntry, p);
                if (c.target.metric != o) {
                    c.target.uplinks = new ArrayList<shrtPthFrstRes<Ta>>();
                    c.target.uplinks.add(upl);
                    c.target.uplink = upl;
                    c.target.metric = o;
                    lst.add(c.target);
                    continue;
                }
                if (hps && (upl.hops > c.target.uplink.hops)) {
                    continue;
                }
                if (ecm) {
                    c.target.uplinks.add(upl);
                }
                if (upl.compare(c.target.uplink, upl) < 0) {
                    continue;
                }
                if (!ecm) {
                    c.target.uplinks.clear();
                    c.target.uplinks.add(upl);
                }
                if (hps && (upl.hops < c.target.uplink.hops)) {
                    c.target.uplinks.clear();
                    c.target.uplinks.add(upl);
                }
                c.target.uplink = upl;
            }
            frst = false;
        }
        tim3 = bits.getTime();
        if (prev == null) {
            return res;
        }
        int mode = topoLog.get();
        for (int o = 0; o < prev.nodes.size(); o++) {
            shrtPthFrstNode<Ta> cn = prev.nodes.get(o);
            if (cn == null) {
                continue;
            }
            shrtPthFrstNode<Ta> on = nodes.find(cn);
            if (on == null) {
                if ((mode & 0x1) != 0) {
                    logger.warn("old node " + cn + " disappeared");
                }
                continue;
            }
        }
        for (int o = 0; o < nodes.size(); o++) {
            shrtPthFrstNode<Ta> cn = nodes.get(o);
            if (cn == null) {
                continue;
            }
            shrtPthFrstNode<Ta> on = prev.nodes.find(cn);
            if (on == null) {
                if ((mode & 0x1) != 0) {
                    logger.warn("new node " + cn + " appeared");
                }
                continue;
            }
            for (int i = 0; i < cn.conn.size(); i++) {
                shrtPthFrstConn<Ta> cc = cn.conn.get(i);
                if (cc == null) {
                    continue;
                }
                shrtPthFrstConn<Ta> oc = on.findConn(cc.target, cc.metric);
                if (oc == null) {
                    if ((mode & 0x2) != 0) {
                        logger.warn("node " + cn + " established connection to " + cc.target);
                    }
                    continue;
                }
                if (cc.metric != oc.metric) {
                    if ((mode & 0x10) != 0) {
                        logger.warn("metric changed on node " + cn + " toward " + cc.target + " from " + oc.metric + " to " + cc.metric);
                    }
                }
                if (cc.stub && !oc.stub) {
                    if ((mode & 0x4) != 0) {
                        logger.warn("node " + cn + " unwilling to forward to " + cc.target);
                    }
                }
                if (!cc.stub && oc.stub) {
                    if ((mode & 0x4) != 0) {
                        logger.warn("node " + cn + " willing to forward to " + cc.target);
                    }
                }
            }
            for (int i = 0; i < on.conn.size(); i++) {
                shrtPthFrstConn<Ta> oc = on.conn.get(i);
                if (oc == null) {
                    continue;
                }
                shrtPthFrstConn<Ta> cc = cn.findConn(oc.target, oc.metric);
                if (cc == null) {
                    if ((mode & 0x2) != 0) {
                        logger.warn("node " + on + " lost connection to " + oc.target);
                    }
                    continue;
                }
            }
            if (on.visited && !cn.visited) {
                if ((mode & 0x8) != 0) {
                    logger.warn("node " + cn + " became unreachable");
                }
            }
            if (!on.visited && cn.visited) {
                if ((mode & 0x8) != 0) {
                    logger.warn("node " + cn + " became reachable");
                }
            }
            if ((mode & 0x20) != 0) {
                diffPrefix(cn, cn.prfAdd, on.prfAdd);
                diffPrefix(cn, cn.prfFix, on.prfFix);
                diffPrefix(cn, cn.othAdd, on.othAdd);
                diffPrefix(cn, cn.othFix, on.othFix);
            }
        }
        prev = null;
        return res;
    }

    /**
     * find next hops
     *
     * @param which node id
     * @return list of next hops
     */
    protected List<shrtPthFrstRes<Ta>> findNextHop(Ta which) {
        List<shrtPthFrstRes<Ta>> res = new ArrayList<shrtPthFrstRes<Ta>>();
        shrtPthFrstNode<Ta> old = nodes.find(new shrtPthFrstNode<Ta>(which));
        if (old == null) {
            return res;
        }
        if (old.result != null) {
            return old.result;
        }
        List<shrtPthFrstRes<Ta>> ned = new ArrayList<shrtPthFrstRes<Ta>>();
        ned.add(new shrtPthFrstRes<Ta>(old, -1));
        for (;;) {
            if (ned.size() < 1) {
                break;
            }
            shrtPthFrstRes<Ta> cur = ned.remove(0);
            if (cur.nodeH.uplinks == null) {
                continue;
            }
            for (int i = 0; i < cur.nodeH.uplinks.size(); i++) {
                shrtPthFrstRes<Ta> upl = cur.nodeH.uplinks.get(i);
                int hops = cur.hops;
                if (hops < 0) {
                    hops = upl.hops;
                }
                if (upl.iface == null) {
                    ned.add(new shrtPthFrstRes<Ta>(upl.nodeH, hops));
                    continue;
                }
                shrtPthFrstRes<Ta> out = new shrtPthFrstRes<Ta>(cur.nodeH, hops);
                out.iface = upl.iface;
                out.nxtHop = upl.nxtHop;
                out.oface = upl.oface;
                out.othHop = upl.othHop;
                out.srBeg = cur.nodeH.srBeg;
                out.brBeg = cur.nodeH.brBeg;
                res.add(out);
            }
        }
        old.result = res;
        return res;
    }

    /**
     * get metric to node
     *
     * @param ntry node to query
     * @return segment routing peers
     */
    protected tabGen<tabIndex<addrIP>> findSegrouPeers(shrtPthFrstNode<Ta> ntry) {
        tabGen<tabIndex<addrIP>> res = new tabGen<tabIndex<addrIP>>();
        for (int i = 0; i < ntry.conn.size(); i++) {
            shrtPthFrstConn<Ta> con = ntry.conn.get(i);
            if (con.target.srIdx < 1) {
                continue;
            }
            res.add(new tabIndex<addrIP>(con.target.srIdx, null));
        }
        return res;
    }

    /**
     * get metric to node
     *
     * @param which node to query
     * @return metric to node, negative on error
     */
    public int getMetric(Ta which) {
        shrtPthFrstNode<Ta> ntry = nodes.find(new shrtPthFrstNode<Ta>(which));
        if (ntry == null) {
            return -1;
        }
        return ntry.metric;
    }

    /**
     * get segment routing base
     *
     * @param which node to query
     * @return label, -1=not found
     */
    public int getSegRouB(Ta which) {
        shrtPthFrstNode<Ta> ntry = nodes.find(new shrtPthFrstNode<Ta>(which));
        if (ntry == null) {
            return -1;
        }
        return ntry.srBeg;
    }

    /**
     * get bier base
     *
     * @param which node to query
     * @return label, -1=not found
     */
    public int getBierB(Ta which) {
        shrtPthFrstNode<Ta> ntry = nodes.find(new shrtPthFrstNode<Ta>(which));
        if (ntry == null) {
            return -1;
        }
        return ntry.brBeg;
    }

    private void doBier(shrtPthFrstNode<Ta> ntry) {
        if (ntry.uplink == null) {
            return;
        }
        for (int o = 0; o < ntry.brLst.size(); o++) {
            ntry.uplink.nodeH.brLst.add(ntry.brLst.get(o));
        }
        doBier(ntry.uplink.nodeH);
    }

    /**
     * get bier info
     *
     * @param base base
     * @param bsl bsl
     * @return calculated bier info
     */
    public tabLabelBier getBierI(int base, int bsl) {
        tabLabelBier res = new tabLabelBier(base, bsl);
        for (int i = 0; i < nodes.size(); i++) {
            shrtPthFrstNode<Ta> ntry = nodes.get(i);
            if (ntry == null) {
                continue;
            }
            doBier(ntry);
        }
        for (int i = 0; i < nodes.size(); i++) {
            shrtPthFrstNode<Ta> ntry = nodes.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.uplink == null) {
                continue;
            }
            if (ntry.uplink.iface == null) {
                continue;
            }
            if (ntry.brBeg <= 0) {
                continue;
            }
            tabLabelBierN per = new tabLabelBierN(ntry.uplink.iface, ntry.uplink.nxtHop, ntry.brBeg);
            for (int o = 0; o < ntry.brLst.size(); o++) {
                per.setBit(ntry.brLst.get(o).get() - 1);
            }
            res.peers.add(per);
        }
        tim4 = bits.getTime();
        return res;
    }

    /**
     * list segment routing
     *
     * @return list of segment routing
     */
    public String listSegRou() {
        String s = "";
        for (int i = 0; i < nodes.size(); i++) {
            shrtPthFrstNode<Ta> ntry = nodes.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.srIdx <= 0) {
                continue;
            }
            s += " " + ntry + "=" + ntry.srIdx;
        }
        return s;
    }

    /**
     * list no segment routing
     *
     * @return list of no segment routing
     */
    public String listNoSegRou() {
        String s = "";
        for (int i = 0; i < nodes.size(); i++) {
            shrtPthFrstNode<Ta> ntry = nodes.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.srIdx > 0) {
                continue;
            }
            s += " " + ntry;
        }
        return s;
    }

    /**
     * list bier
     *
     * @return list of bier
     */
    public String listBier() {
        String s = "";
        for (int i = 0; i < nodes.size(); i++) {
            shrtPthFrstNode<Ta> ntry = nodes.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.brIdx <= 0) {
                continue;
            }
            s += " " + ntry + "=" + ntry.brIdx;
        }
        return s;
    }

    /**
     * list no bier
     *
     * @return list of no bier
     */
    public String listNoBier() {
        String s = "";
        for (int i = 0; i < nodes.size(); i++) {
            shrtPthFrstNode<Ta> ntry = nodes.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.brIdx > 0) {
                continue;
            }
            s += " " + ntry;
        }
        return s;
    }

    /**
     * count reachables
     *
     * @param state required state
     * @return number of reachable nodes
     */
    public int countReachablility(boolean state) {
        int o = 0;
        for (int i = 0; i < nodes.size(); i++) {
            shrtPthFrstNode<Ta> ntry = nodes.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.visited != state) {
                continue;
            }
            o++;
        }
        return o;
    }

    /**
     * list reachables
     *
     * @param state required state
     * @return list of reachable nodes
     */
    public String listReachablility(boolean state) {
        String s = "";
        for (int i = 0; i < nodes.size(); i++) {
            shrtPthFrstNode<Ta> ntry = nodes.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.visited != state) {
                continue;
            }
            s += " " + ntry;
        }
        return s;
    }

    /**
     * list stubs
     *
     * @return list of stub nodes
     */
    public String listStubs() {
        String s = "";
        for (int i = 0; i < nodes.size(); i++) {
            shrtPthFrstNode<Ta> ntry = nodes.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.conn.size() > 1) {
                continue;
            }
            s += " " + ntry;
        }
        return s;
    }

    /**
     * list topology
     *
     * @return list of topology
     */
    public int listTopoHsh() {
        cryHashCrc32 h = new cryHashCrc32(cryHashCrc32.polyCrc32i);
        h.init();
        h.update(listTopoSum().getBytes());
        return h.getCrc();
    }

    /**
     * list topology
     *
     * @return list of topology
     */
    public String listTopoSum() {
        String s = "";
        for (int i = 0; i < nodes.size(); i++) {
            shrtPthFrstNode<Ta> ntry = nodes.get(i);
            if (ntry == null) {
                continue;
            }
            s += " " + ntry + "," + ntry.visited + "," + ntry.conn.size() + "," + (ntry.prfFix.size() + ntry.prfAdd.size() + ntry.othFix.size() + ntry.othAdd.size());
        }
        return s;
    }

    /**
     * list topology
     *
     * @param adr address of node
     * @return list of topology
     */
    public userFormat listTopology(Ta adr) {
        userFormat res = new userFormat("|", "category|value");
        shrtPthFrstNode<Ta> ntry = new shrtPthFrstNode<Ta>(adr);
        ntry = nodes.find(ntry);
        if (ntry == null) {
            return null;
        }
        res.add("node|" + ntry.name);
        res.add("name|" + ntry.ident);
        res.add("reachable|" + ntry.visited);
        res.add("stub|" + (ntry.conn.size() <= 1));
        res.add("uplink|" + ntry.uplink);
        if (ntry.uplinks != null) {
            res.add("uplinks|" + ntry.uplinks.size());
            for (int i = 0; i < ntry.uplinks.size(); i++) {
                shrtPthFrstRes<Ta> upl = ntry.uplinks.get(i);
                res.add("uplinknod|" + upl.nodeH);
                res.add("uplinkhop|" + upl.hops);
            }
        }
        if (ntry.result != null) {
            res.add("reaches|" + ntry.result.size());
            for (int i = 0; i < ntry.result.size(); i++) {
                shrtPthFrstRes<Ta> upl = ntry.result.get(i);
                res.add("reachnod|" + upl.nodeH);
                res.add("reachhop|" + upl.hops);
                res.add("reachvia|" + upl.nxtHop);
                res.add("reachifc|" + upl.iface);
                res.add("reachothvia|" + upl.othHop);
                res.add("reachothifc|" + upl.oface);
            }
        }
        res.add("reachmet|" + ntry.metric);
        res.add("hopmet|" + ntry.nxtMet);
        res.add("connections|" + ntry.conn.size());
        res.add("prefixes|" + ntry.prfFix.size() + " " + ntry.prfAdd.size() + " " + ntry.othFix.size() + " " + ntry.othAdd.size());
        res.add("segrout|" + ntry.srIdx + " " + ntry.srBeg);
        res.add("bier|" + ntry.brIdx + " " + ntry.brBeg);
        String a = "";
        for (int i = 0; i < ntry.brLst.size(); i++) {
            shrtPthFrstIdx idx = ntry.brLst.get(i);
            if (idx == null) {
                continue;
            }
            a += idx + " ";
        }
        res.add("biers|" + a);
        for (int i = 0; i < ntry.conn.size(); i++) {
            shrtPthFrstConn<Ta> con = ntry.conn.get(i);
            if (con == null) {
                continue;
            }
            res.add("neighbor|" + con.target + "=" + con.metric + "=" + con.ident);
        }
        for (int i = 0; i < ntry.prfFix.size(); i++) {
            tabRouteEntry<addrIP> rou = ntry.prfFix.get(i);
            if (rou == null) {
                continue;
            }
            res.add("fixprefix|" + addrPrefix.ip2str(rou.prefix) + "=" + rou.best.metric);
        }
        for (int i = 0; i < ntry.prfAdd.size(); i++) {
            tabRouteEntry<addrIP> rou = ntry.prfAdd.get(i);
            if (rou == null) {
                continue;
            }
            res.add("addprefix|" + addrPrefix.ip2str(rou.prefix) + "=" + rou.best.metric);
        }
        for (int i = 0; i < ntry.othFix.size(); i++) {
            tabRouteEntry<addrIP> rou = ntry.othFix.get(i);
            if (rou == null) {
                continue;
            }
            res.add("fixprefix|" + addrPrefix.ip2str(rou.prefix) + "=" + rou.best.metric);
        }
        for (int i = 0; i < ntry.othAdd.size(); i++) {
            tabRouteEntry<addrIP> rou = ntry.othAdd.get(i);
            if (rou == null) {
                continue;
            }
            res.add("addprefix|" + addrPrefix.ip2str(rou.prefix) + "=" + rou.best.metric);
        }
        return res;
    }

    /**
     * list topology
     *
     * @return list of topology
     */
    public userFormat listTopology() {
        userFormat res = new userFormat("|", "node|reach|met|uplink|ups|res|conn|prfx|sr|br|neighbors|prefixes");
        for (int i = 0; i < nodes.size(); i++) {
            shrtPthFrstNode<Ta> ntry = nodes.get(i);
            if (ntry == null) {
                continue;
            }
            String a = "";
            if (ntry.uplinks == null) {
                a = "-1";
            } else {
                a = "" + ntry.uplinks.size();
            }
            String s = "";
            if (ntry.result == null) {
                s = "-1";
            } else {
                s = "" + ntry.result.size();
            }
            a = ntry + "|" + ntry.visited + "|" + ntry.metric + "|" + ntry.uplink + "|" + a + "|" + s + "|" + ntry.conn.size() + "|" + ntry.prfFix.size() + "+" + ntry.prfAdd.size() + "+" + ntry.othFix.size() + "+" + ntry.othAdd.size() + "|" + ntry.srIdx + "|" + ntry.brIdx + "|";
            for (int o = 0; o < ntry.conn.size(); o++) {
                shrtPthFrstConn<Ta> con = ntry.conn.get(o);
                if (con == null) {
                    continue;
                }
                a += con.target + "=" + con.metric + "=" + con.ident + " ";
            }
            a += "|";
            for (int o = 0; o < ntry.prfFix.size(); o++) {
                tabRouteEntry<addrIP> rou = ntry.prfFix.get(o);
                if (rou == null) {
                    continue;
                }
                a += addrPrefix.ip2str(rou.prefix) + "=" + rou.best.metric + " ";
            }
            for (int o = 0; o < ntry.prfAdd.size(); o++) {
                tabRouteEntry<addrIP> rou = ntry.prfAdd.get(o);
                if (rou == null) {
                    continue;
                }
                a += addrPrefix.ip2str(rou.prefix) + "=" + rou.best.metric + " ";
            }
            for (int o = 0; o < ntry.othFix.size(); o++) {
                tabRouteEntry<addrIP> rou = ntry.othFix.get(o);
                if (rou == null) {
                    continue;
                }
                a += addrPrefix.ip2str(rou.prefix) + "=" + rou.best.metric + " ";
            }
            for (int o = 0; o < ntry.othAdd.size(); o++) {
                tabRouteEntry<addrIP> rou = ntry.othAdd.get(o);
                if (rou == null) {
                    continue;
                }
                a += addrPrefix.ip2str(rou.prefix) + "=" + rou.best.metric + " ";
            }
            res.add(a);
        }
        return res;
    }

    /**
     * list statistics
     *
     * @return list
     */
    public userFormat listStatistics() {
        userFormat res = new userFormat("|", "category|value");
        res.add("reach|" + listReachablility(true));
        res.add("reachable|" + countReachablility(true));
        res.add("unreach|" + listReachablility(false));
        res.add("unreachable|" + countReachablility(false));
        res.add("stub|" + listStubs());
        res.add("segrou|" + listSegRou());
        res.add("nosegrou|" + listNoSegRou());
        res.add("bier|" + listBier());
        res.add("nobier|" + listNoBier());
        res.add("topostr|" + listTopoSum());
        res.add("topoid|" + bits.toHexD(listTopoHsh()));
        res.add("last|" + bits.time2str(cfgAll.timeZoneName, tim1 + cfgAll.timeServerOffset, 3) + " (" + bits.timePast(tim1) + " ago)");
        res.add("fill|" + (tim2 - tim1));
        res.add("calc|" + (tim3 - tim2));
        res.add("table|" + (tim4 - tim3));
        res.add("run|" + count);
        return res;
    }

    /**
     * list statistics
     *
     * @return list
     */
    public userFormat listUsages() {
        userFormat res = new userFormat("|", "when|ago|time|topoid|unreach");
        for (int i = log.size() - 1; i >= 0; i--) {
            res.add("" + log.get(i));
        }
        return res;
    }

    /**
     * list tree
     *
     * @return list
     */
    public List<String> listTree() {
        List<String> res = new ArrayList<String>();
        if (spfRoot == null) {
            return res;
        }
        listTree(res, spfRoot, "");
        return res;
    }

    private void listTree(List<String> res, shrtPthFrstNode<Ta> ntry, String pref) {
        List<shrtPthFrstConn<Ta>> down = new ArrayList<shrtPthFrstConn<Ta>>();
        for (int i = 0; i < ntry.conn.size(); i++) {
            shrtPthFrstConn<Ta> cur = ntry.conn.get(i);
            if (cur.target.uplink == null) {
                continue;
            }
            if (ntry.compare(ntry, cur.target.uplink.nodeH) != 0) {
                continue;
            }
            down.add(cur);
        }
        res.add(pref + "`--" + ntry);
        for (int i = 0; i < down.size(); i++) {
            shrtPthFrstConn<Ta> cur = down.get(i);
            String a = (i + 1) == down.size() ? "   " : "  |";
            listTree(res, cur.target, pref + a);
        }
    }

    /**
     * list graphviz
     *
     * @param nocli no cli
     * @param nonets no nets
     * @param noints no ints
     * @return list
     */
    public List<String> listGraphviz(boolean nocli, boolean nonets, boolean noints) {
        List<String> res = new ArrayList<String>();
        if (!nocli) {
            res.add(graphBeg1);
        }
        res.add(graphBeg2);
        for (int o = 0; o < nodes.size(); o++) {
            shrtPthFrstNode<Ta> ntry = nodes.get(o);
            res.add("//" + ntry);
            for (int i = 0; i < ntry.conn.size(); i++) {
                shrtPthFrstConn<Ta> cur = ntry.conn.get(i);
                String a;
                if (noints) {
                    a = "";
                } else {
                    a = " [taillabel=\"" + cur.ident + "\"]";
                }
                res.add("  \"" + ntry + "\" -- \"" + cur.target + "\" [weight=" + cur.metric + "]" + a);
            }
            if (nonets) {
                continue;
            }
            for (int i = 0; i < ntry.prfAdd.size(); i++) {
                tabRouteEntry<addrIP> cur = ntry.prfAdd.get(i);
                res.add("  \"" + ntry + "\" -- \"" + addrPrefix.ip2str(cur.prefix) + "\" [weight=" + cur.best.metric + "]");
            }
            for (int i = 0; i < ntry.prfFix.size(); i++) {
                tabRouteEntry<addrIP> cur = ntry.prfFix.get(i);
                res.add("  \"" + ntry + "\" -- \"" + addrPrefix.ip2str(cur.prefix) + "\" [weight=" + cur.best.metric + "]");
            }
            for (int i = 0; i < ntry.othAdd.size(); i++) {
                tabRouteEntry<addrIP> cur = ntry.othAdd.get(i);
                res.add("  \"" + ntry + "\" -- \"" + addrPrefix.ip2str(cur.prefix) + "\" [weight=" + cur.best.metric + "]");
            }
            for (int i = 0; i < ntry.othFix.size(); i++) {
                tabRouteEntry<addrIP> cur = ntry.othFix.get(i);
                res.add("  \"" + ntry + "\" -- \"" + addrPrefix.ip2str(cur.prefix) + "\" [weight=" + cur.best.metric + "]");
            }
        }
        res.add(graphEnd1);
        if (!nocli) {
            res.add(graphEnd2);
        }
        return res;
    }

    private void listNhIncons(tabGen<shrtPthFrstPfx<Ta>> lst, shrtPthFrstNode<Ta> nod, addrPrefix<addrIP> pfx) {
        shrtPthFrstPfx<Ta> ntry = new shrtPthFrstPfx<Ta>(pfx);
        shrtPthFrstPfx<Ta> old = lst.add(ntry);
        if (old != null) {
            ntry = old;
        }
        ntry.nodes.add(nod);
    }

    /**
     * hostnames
     *
     * @return text
     */
    public userFormat listHostnames() {
        userFormat res = new userFormat("|", "router|name");
        for (int o = 0; o < nodes.size(); o++) {
            shrtPthFrstNode<Ta> ntry = nodes.get(o);
            res.add(ntry.name + "|" + ntry.ident);
        }
        return res;
    }

    /**
     * inconsistent next hops
     *
     * @param mtch matcher
     * @return text
     */
    public userFormat listNhIncons(tabIntMatcher mtch) {
        tabGen<shrtPthFrstPfx<Ta>> lst = new tabGen<shrtPthFrstPfx<Ta>>();
        for (int o = 0; o < nodes.size(); o++) {
            shrtPthFrstNode<Ta> ntry = nodes.get(o);
            for (int i = 0; i < ntry.prfFix.size(); i++) {
                listNhIncons(lst, ntry, ntry.prfFix.get(i).prefix);
            }
            for (int i = 0; i < ntry.prfAdd.size(); i++) {
                listNhIncons(lst, ntry, ntry.prfAdd.get(i).prefix);
            }
            for (int i = 0; i < ntry.othFix.size(); i++) {
                listNhIncons(lst, ntry, ntry.othFix.get(i).prefix);
            }
            for (int i = 0; i < ntry.othAdd.size(); i++) {
                listNhIncons(lst, ntry, ntry.othAdd.get(i).prefix);
            }
        }
        userFormat res = new userFormat("|", "path|nexthops");
        for (int i = 0; i < lst.size(); i++) {
            shrtPthFrstPfx<Ta> ntry = lst.get(i);
            if (!mtch.matches(ntry.nodes.size())) {
                continue;
            }
            res.add("" + ntry);
        }
        return res;
    }

    /**
     * inconsistent metrics
     *
     * @param mtch matcher
     * @return text
     */
    public userFormat listMetIncons(tabIntMatcher mtch) {
        userFormat res = new userFormat("|", "source|target|diff");
        for (int o = 0; o < nodes.size(); o++) {
            shrtPthFrstNode<Ta> ntry = nodes.get(o);
            for (int i = 0; i < ntry.conn.size(); i++) {
                shrtPthFrstConn<Ta> cn = ntry.conn.get(i);
                shrtPthFrstConn<Ta> co = cn.target.findConn(ntry, cn.metric);
                if (co == null) {
                    res.add(ntry + "|" + cn.target + "|missing");
                    continue;
                }
                int p = cn.metric - co.metric;
                if (p < 0) {
                    p = -p;
                }
                if (p < 1) {
                    continue;
                }
                if (!mtch.matches(p)) {
                    continue;
                }
                res.add(ntry + "|" + cn.target + "|" + p);
            }
        }
        return res;
    }

    /**
     * get routes
     *
     * @param fwdCor forwarding core
     * @param fwdKey forwarder key
     * @param segrouLab segment routing labels
     * @param segrouUsd segment routing usage
     * @return routes
     */
    public tabRoute<addrIP> getRoutes(ipFwd fwdCor, int fwdKey, tabLabelEntry[] segrouLab, tabGen<tabIndex<addrIP>> segrouUsd) {
        tabRoute<addrIP> tab1 = new tabRoute<addrIP>("routes");
        for (int o = 0; o < nodes.size(); o++) {
            shrtPthFrstNode<Ta> ntry = nodes.get(o);
            List<shrtPthFrstRes<Ta>> hop = findNextHop(ntry.name);
            if (hop.size() < 1) {
                continue;
            }
            int met = getMetric(ntry.name);
            int sro = getSegRouB(ntry.name);
            int bro = getBierB(ntry.name);
            tabGen<tabIndex<addrIP>> srp = findSegrouPeers(ntry);
            for (int i = 0; i < ntry.prfAdd.size(); i++) {
                tabRouteEntry<addrIP> rou = ntry.prfAdd.get(i).copyBytes(tabRoute.addType.notyet);
                rou.best.srcRtr = ntry.name.copyBytes();
                rou.best.segrouOld = sro;
                rou.best.bierOld = bro;
                rou.best.metric += met;
                populateRoute(tab1, fwdCor, ntry, fwdKey, segrouLab, segrouUsd, srp, rou, hop, false);
            }
            for (int i = 0; i < ntry.prfFix.size(); i++) {
                tabRouteEntry<addrIP> rou = ntry.prfFix.get(i).copyBytes(tabRoute.addType.notyet);
                rou.best.srcRtr = ntry.name.copyBytes();
                rou.best.segrouOld = sro;
                rou.best.bierOld = bro;
                populateRoute(tab1, fwdCor, ntry, fwdKey, segrouLab, segrouUsd, srp, rou, hop, false);
            }
        }
        tim4 = bits.getTime();
        return tab1;
    }

    /**
     * get other routes
     *
     * @param fwdCor forwarding core
     * @param fwdKey forwarder key
     * @param segrouLab segment routing labels
     * @param segrouUsd segment routing usage
     * @return routes
     */
    public tabRoute<addrIP> getOroutes(ipFwd fwdCor, int fwdKey, tabLabelEntry[] segrouLab, tabGen<tabIndex<addrIP>> segrouUsd) {
        tabRoute<addrIP> tab1 = new tabRoute<addrIP>("routes");
        for (int o = 0; o < nodes.size(); o++) {
            shrtPthFrstNode<Ta> ntry = nodes.get(o);
            List<shrtPthFrstRes<Ta>> hop = findNextHop(ntry.name);
            if (hop.size() < 1) {
                continue;
            }
            int met = getMetric(ntry.name);
            int sro = getSegRouB(ntry.name);
            int bro = getBierB(ntry.name);
            tabGen<tabIndex<addrIP>> srp = findSegrouPeers(ntry);
            for (int i = 0; i < ntry.othAdd.size(); i++) {
                tabRouteEntry<addrIP> rou = ntry.othAdd.get(i).copyBytes(tabRoute.addType.notyet);
                rou.best.srcRtr = ntry.name.copyBytes();
                rou.best.segrouOld = sro;
                rou.best.bierOld = bro;
                rou.best.metric += met;
                populateRoute(tab1, fwdCor, ntry, fwdKey, segrouLab, segrouUsd, srp, rou, hop, true);
            }
            for (int i = 0; i < ntry.othFix.size(); i++) {
                tabRouteEntry<addrIP> rou = ntry.othFix.get(i).copyBytes(tabRoute.addType.notyet);
                rou.best.srcRtr = ntry.name.copyBytes();
                rou.best.segrouOld = sro;
                rou.best.bierOld = bro;
                populateRoute(tab1, fwdCor, ntry, fwdKey, segrouLab, segrouUsd, srp, rou, hop, true);
            }
        }
        tim4 = bits.getTime();
        return tab1;
    }

    private void populateRoute(tabRoute<addrIP> tab1, ipFwd fwdCor, shrtPthFrstNode<Ta> ntry, int fwdKey, tabLabelEntry[] segrouLab, tabGen<tabIndex<addrIP>> segrouUsd, tabGen<tabIndex<addrIP>> srp, tabRouteEntry<addrIP> rou, List<shrtPthFrstRes<Ta>> hop, boolean other) {
        rou.alts.clear();
        boolean srPop = (rou.best.rouSrc & 16) != 0;
        for (int i = 0; i < hop.size(); i++) {
            shrtPthFrstRes<Ta> upl = hop.get(i);
            tabRouteAttr<addrIP> res = new tabRouteAttr<addrIP>();
            rou.best.copyBytes(res, false);
            if (!other) {
                if (upl.nxtHop == null) {
                    continue;
                }
                res.nextHop = upl.nxtHop.copyBytes();
                res.iface = upl.iface;
            } else {
                if (upl.othHop == null) {
                    continue;
                }
                res.nextHop = upl.othHop.copyBytes();
                res.iface = upl.oface;
            }
            res.hops = upl.hops;
            res.segrouBeg = upl.srBeg;
            res.bierBeg = upl.brBeg;
            res.segrouIdx = rou.best.segrouIdx;
            res.segrouOld = rou.best.segrouOld;
            res.bierIdx = rou.best.bierIdx;
            res.bierHdr = rou.best.bierHdr;
            res.bierOld = rou.best.bierOld;
            if ((segrouUsd == null) || (res.segrouIdx < 1) || (res.segrouBeg < 1)) {
                res.labelRem = null;
                rou.addAlt(res);
                continue;
            }
            res.labelRem = tabLabel.int2labels(res.segrouBeg + res.segrouIdx);
            if (srPop && (res.hops <= 1)) {
                res.labelRem = tabLabel.int2labels(ipMpls.labelImp);
            }
            rou.addAlt(res);
        }
        rou.hashBest();
        long oldVer = tab1.version;
        tab1.add(tabRoute.addType.ecmp, rou, false, true);
        if (oldVer == tab1.version) {
            return;
        }
        if (segrouUsd == null) {
            return;
        }
        if (rou.best.labelRem == null) {
            return;
        }
        if (rou.best.segrouIdx >= segrouLab.length) {
            return;
        }
        segrouLab[rou.best.segrouIdx].setFwdMpls(fwdKey, fwdCor, (ipFwdIface) rou.best.iface, rou.best.nextHop, rou.best.labelRem);
        tabIndex<addrIP> sri = new tabIndex<addrIP>(rou.best.segrouIdx, rou.prefix);
        sri.neighs = srp;
        sri.conned = ntry.nxtMet < Integer.MAX_VALUE;
        tabIndex.add2table(segrouUsd, sri);
    }

    private void listLinStateHdr(typLenVal tlv, packHolder pck, int prt, int typ) {
        pck.clear();
        pck.msbPutW(0, typ); // type
        pck.putByte(2, prt); // protocol
        pck.msbPutQ(3, 0); // identifier
        pck.putSkip(11);
    }

    private void listLinStateNod(typLenVal tlv, packHolder pck, packHolder hlp, int siz, int asn, addrIPv4 adv, int par, shrtPthFrstNode<Ta> nod, int typ) {
        hlp.clear();
        tlv.valSiz = 4;
        tlv.valTyp = 512; // asn
        bits.msbPutD(tlv.valDat, 0, asn);
        tlv.putThis(hlp);
        tlv.putAddr(hlp, 513, adv); // ls id
        hlp.merge2end();
        if (par != -1) {
            byte[] buf = new byte[4];
            bits.msbPutD(buf, 0, par);
            tlv.putBytes(hlp, 514, buf); // area id
        }
        nod.name.toBuffer(tlv.valDat, 0);
        tlv.putBytes(hlp, 515, siz, tlv.valDat); // router id
        hlp.merge2end();
        byte[] buf = hlp.getCopy();
        bits.byteCopy(buf, 0, tlv.valDat, 0, buf.length);
        tlv.valSiz = buf.length;
        tlv.putBytes(pck, typ); // node type
        pck.merge2end();
    }

    private void listLinStatePrf(tabRoute<addrIP> tab, typLenVal tlv, packHolder pck, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        hlp.clear();
        if (ntry.prefix.network.isIPv4()) {
            rtrBgpUtil.writePrefix(rtrBgpUtil.safiIp4uni, hlp, ntry);
        } else {
            rtrBgpUtil.writePrefix(rtrBgpUtil.safiIp6uni, hlp, ntry);
        }
        hlp.merge2end();
        tlv.putBytes(pck, 265, hlp.getCopy());
        pck.merge2end();
        tabRouteEntry<addrIP> rou = new tabRouteEntry<addrIP>();
        rou.best.rouSrc = rtrBgpUtil.peerOriginate;
        addrIP adr = new addrIP();
        rou.nlri = pck.getCopy();
        adr.fromBuf(cryHashMd5.compute(new cryHashMd5(), rou.nlri), 0);
        rou.prefix = new addrPrefix<addrIP>(adr, addrIP.size * 8);
        pck.clear();
        if (ntry.best.metric > 0) {
            bits.msbPutD(tlv.valDat, 0, ntry.best.metric);
            tlv.putBytes(pck, 1155, 4, tlv.valDat); // metric
            pck.merge2end();
        }
        if (ntry.best.tag > 0) {
            bits.msbPutD(tlv.valDat, 0, ntry.best.tag);
            tlv.putBytes(pck, 1153, 4, tlv.valDat); // metric
            pck.merge2end();
        }
        if (pck.dataSize() > 0) {
            rou.best.linkStat = pck.getCopy();
        }
        tab.add(tabRoute.addType.better, rou, true, true);
    }

    private void listLinStateAdd(tabRoute<addrIP> tab, typLenVal tlv, packHolder pck, int met) {
        tabRouteEntry<addrIP> rou = new tabRouteEntry<addrIP>();
        rou.best.rouSrc = rtrBgpUtil.peerOriginate;
        addrIP adr = new addrIP();
        rou.nlri = pck.getCopy();
        adr.fromBuf(cryHashMd5.compute(new cryHashMd5(), rou.nlri), 0);
        rou.prefix = new addrPrefix<addrIP>(adr, addrIP.size * 8);
        if (met >= 0) {
            pck.clear();
            bits.msbPutD(tlv.valDat, 0, met << 8);
            tlv.putBytes(pck, 1095, 3, tlv.valDat); // metric
            pck.merge2end();
            rou.best.linkStat = pck.getCopy();
        }
        tab.add(tabRoute.addType.better, rou, true, true);
    }

    /**
     * list link states
     *
     * @param tab table to populate
     * @param prt protocol id
     * @param par parameter
     * @param asn asn
     * @param adv advertiser
     * @param siz size of node
     */
    public void listLinkStates(tabRoute<addrIP> tab, int prt, int par, int asn, addrIPv4 adv, int siz) {
        typLenVal tlv = new typLenVal(0, 16, 16, 16, 1, 0, 4, 1, 0, 1024, true);
        packHolder pck = new packHolder(true, true);
        packHolder hlp = new packHolder(true, true);
        for (int o = 0; o < nodes.size(); o++) {
            shrtPthFrstNode<Ta> nod = nodes.get(o);
            listLinStateHdr(tlv, pck, prt, 1);
            listLinStateNod(tlv, pck, hlp, siz, asn, adv, par, nod, 256); // local node
            listLinStateAdd(tab, tlv, pck, -1);
            for (int i = 0; i < nod.conn.size(); i++) {
                shrtPthFrstConn<Ta> con = nod.conn.get(i);
                listLinStateHdr(tlv, pck, prt, 2);
                listLinStateNod(tlv, pck, hlp, siz, asn, adv, par, nod, 256); // local node
                listLinStateNod(tlv, pck, hlp, siz, asn, adv, par, con.target, 257); // remote node
                listLinStateAdd(tab, tlv, pck, con.metric);
            }
            for (int i = 0; i < nod.prfFix.size(); i++) {
                tabRouteEntry<addrIP> rou = nod.prfFix.get(i);
                listLinStateHdr(tlv, pck, prt, 3);
                listLinStateNod(tlv, pck, hlp, siz, asn, adv, par, nod, 256); // local node
                listLinStatePrf(tab, tlv, pck, hlp, rou);
            }
            for (int i = 0; i < nod.prfAdd.size(); i++) {
                tabRouteEntry<addrIP> rou = nod.prfAdd.get(i);
                listLinStateHdr(tlv, pck, prt, 3);
                listLinStateNod(tlv, pck, hlp, siz, asn, adv, par, nod, 256); // local node
                listLinStatePrf(tab, tlv, pck, hlp, rou);
            }
            for (int i = 0; i < nod.othFix.size(); i++) {
                tabRouteEntry<addrIP> rou = nod.othFix.get(i);
                listLinStateHdr(tlv, pck, prt, 3);
                listLinStateNod(tlv, pck, hlp, siz, asn, adv, par, nod, 256); // local node
                listLinStatePrf(tab, tlv, pck, hlp, rou);
            }
            for (int i = 0; i < nod.othAdd.size(); i++) {
                tabRouteEntry<addrIP> rou = nod.othAdd.get(i);
                listLinStateHdr(tlv, pck, prt, 3);
                listLinStateNod(tlv, pck, hlp, siz, asn, adv, par, nod, 256); // local node
                listLinStatePrf(tab, tlv, pck, hlp, rou);
            }
        }
    }

}
