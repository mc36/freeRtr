package util;

import addr.addrIP;
import cfg.cfgAll;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import tab.tabGen;
import tab.tabLabelBier;
import tab.tabLabelBierN;
import tab.tabRouteIface;
import user.userFormat;

/**
 * dijkstra's shortest path first
 *
 * @param <Ta> type of nodes
 * @author matecsaba
 */
public class shrtPthFrst<Ta extends Comparator<? super Ta>> {

    private final tabGen<shrtPthFrstNode<Ta>> nodes;

    private final List<shrtPthFrstLog> log;

    private final int count;

    private final long tim1;

    private long tim2;

    private long tim3;

    /**
     * construct spf
     *
     * @param old old spf
     */
    public shrtPthFrst(shrtPthFrst<Ta> old) {
        nodes = new tabGen<shrtPthFrstNode<Ta>>();
        tim1 = bits.getTime();
        if (old == null) {
            log = new ArrayList<shrtPthFrstLog>();
            count = 1;
            return;
        }
        log = old.log;
        count = old.count + 1;
        shrtPthFrstLog ntry = new shrtPthFrstLog();
        ntry.when = old.tim1;
        ntry.tim = (int) (old.tim3 - old.tim1);
        ntry.unreach = old.listUnreachables();
        log.add(ntry);
        for (; log.size() > 250;) {
            log.remove(0);
        }
    }

    /**
     * add one connection
     *
     * @param from source node
     * @param to target node
     * @param metric metric
     * @param realHop true if hop, false if network
     */
    public void addConn(Ta from, Ta to, int metric, boolean realHop) {
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
     * @return true on error, false on success
     */
    public boolean addNextHop(int met, Ta nod, addrIP hop, tabRouteIface ifc) {
        shrtPthFrstNode<Ta> ntry = new shrtPthFrstNode<Ta>(nod);
        ntry = nodes.find(ntry);
        if (ntry == null) {
            return true;
        }
        if (ntry.nxtMet < met) {
            return false;
        }
        if (ntry.hops > 1) {
            return false;
        }
        ntry.nxtMet = met;
        ntry.nxtHop = hop;
        ntry.iface = ifc;
        ntry.nxtCon = true;
        return false;
    }

    /**
     * add segment routing base
     *
     * @param nod node to add
     * @param beg base label
     */
    public void addSegRouB(Ta nod, int beg) {
        shrtPthFrstNode<Ta> ntry = new shrtPthFrstNode<Ta>(nod);
        shrtPthFrstNode<Ta> old = nodes.add(ntry);
        if (old != null) {
            ntry = old;
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
        shrtPthFrstNode<Ta> ntry = new shrtPthFrstNode<Ta>(nod);
        shrtPthFrstNode<Ta> old = nodes.add(ntry);
        if (old != null) {
            ntry = old;
        }
        ntry.srIdx = idx;
    }

    /**
     * add bier base
     *
     * @param nod node to add
     * @param beg base label
     */
    public void addBierB(Ta nod, int beg) {
        shrtPthFrstNode<Ta> ntry = new shrtPthFrstNode<Ta>(nod);
        shrtPthFrstNode<Ta> old = nodes.add(ntry);
        if (old != null) {
            ntry = old;
        }
        ntry.brBeg = beg;
    }

    /**
     * add bier index
     *
     * @param nod node to add
     * @param idx node index
     * @param pri primary index
     */
    public void addBierI(Ta nod, int idx, boolean pri) {
        shrtPthFrstNode<Ta> ntry = new shrtPthFrstNode<Ta>(nod);
        shrtPthFrstNode<Ta> old = nodes.add(ntry);
        if (old != null) {
            ntry = old;
        }
        if (pri) {
            ntry.brIdx = idx;
        }
        ntry.brLst.add(new shrtPthFrstIdx(idx));
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
            ntry.nxtHop = null;
            ntry.metric = -1;
            ntry.iface = null;
            ntry.hops = -1;
        }
        shrtPthFrstNode<Ta> ntry = nodes.find(new shrtPthFrstNode<Ta>(from));
        if (ntry == null) {
            return true;
        }
        tabGen<shrtPthFrstNode<Ta>> lst = new tabGen<shrtPthFrstNode<Ta>>();
        ntry.metric = 0;
        ntry.hops = 0;
        lst.add(ntry);
        for (;;) {
            if (lst.size() < 1) {
                tim3 = bits.getTime();
                return true;
            }
            ntry = lst.get(0);
            for (int i = 0; i < lst.size(); i++) {
                shrtPthFrstNode<Ta> cur = lst.get(i);
                if (cur.metric < ntry.metric) {
                    ntry = cur;
                }
            }
            if (to != null) {
                if (to.compare(to, ntry.name) == 0) {
                    tim3 = bits.getTime();
                    return false;
                }
            }
            lst.del(ntry);
            for (int i = 0; i < ntry.conn.size(); i++) {
                shrtPthFrstConn<Ta> c = ntry.conn.get(i);
                if (c == null) {
                    continue;
                }
                int o = ntry.metric + c.metric;
                if ((c.target.metric < 0) || (c.target.metric > o)) {
                    c.target.uplink = ntry;
                    c.target.metric = o;
                    c.target.hops = ntry.hops;
                    if (c.realHop) {
                        c.target.hops++;
                    }
                    lst.add(c.target);
                }
            }
        }
    }

    /**
     * get uplink of node
     *
     * @param which node to query
     * @return uplink node, null if not found
     */
    public Ta getUplink(Ta which) {
        shrtPthFrstNode<Ta> ntry = nodes.find(new shrtPthFrstNode<Ta>(which));
        if (ntry == null) {
            return null;
        }
        if (ntry.uplink == null) {
            return null;
        }
        return ntry.uplink.name;
    }

    private shrtPthFrstNode<Ta> findNextHop(Ta which) {
        shrtPthFrstNode<Ta> ntry = nodes.find(new shrtPthFrstNode<Ta>(which));
        if (ntry == null) {
            return null;
        }
        shrtPthFrstNode<Ta> old = ntry;
        for (;;) {
            if (ntry.nxtHop != null) {
                old.nxtHop = ntry.nxtHop;
                old.iface = ntry.iface;
                old.srBeg = ntry.srBeg;
                old.brBeg = ntry.brBeg;
                return ntry;
            }
            if (ntry.uplink == null) {
                return null;
            }
            ntry = ntry.uplink;
        }
    }

    /**
     * get next hop address
     *
     * @param which node to query
     * @return next hop, null if not found
     */
    public addrIP getNextHop(Ta which) {
        shrtPthFrstNode<Ta> ntry = findNextHop(which);
        if (ntry == null) {
            return null;
        }
        return ntry.nxtHop;
    }

    /**
     * get next hop interface
     *
     * @param which node to query
     * @return interface, -1=not found
     */
    public tabRouteIface getNextIfc(Ta which) {
        shrtPthFrstNode<Ta> ntry = findNextHop(which);
        if (ntry == null) {
            return null;
        }
        return ntry.iface;
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
     * get hops to node
     *
     * @param which node to query
     * @return hops to node, negative on error
     */
    public int getHops(Ta which) {
        shrtPthFrstNode<Ta> ntry = nodes.find(new shrtPthFrstNode<Ta>(which));
        if (ntry == null) {
            return -1;
        }
        return ntry.hops;
    }

    /**
     * get segment routing label
     *
     * @param which node to query
     * @return label, -1=not found
     */
    public int getSegRouL(Ta which) {
        shrtPthFrstNode<Ta> nod = nodes.find(new shrtPthFrstNode<Ta>(which));
        if (nod == null) {
            return -1;
        }
        shrtPthFrstNode<Ta> hop = findNextHop(which);
        if (hop == null) {
            return -1;
        }
        if ((hop.srBeg < 1) || (nod.srIdx < 1)) {
            return -1;
        }
        return hop.srBeg + nod.srIdx;
    }

    /**
     * get segment routing base
     *
     * @param which node to query
     * @return label, -1=not found
     */
    public int getSegRouB(Ta which) {
        shrtPthFrstNode<Ta> hop = findNextHop(which);
        if (hop == null) {
            return -1;
        }
        return hop.srBeg;
    }

    /**
     * get bier base
     *
     * @param which node to query
     * @return label, -1=not found
     */
    public int getBierB(Ta which) {
        shrtPthFrstNode<Ta> hop = findNextHop(which);
        if (hop == null) {
            return -1;
        }
        return hop.brBeg;
    }

    private void doBier(shrtPthFrstNode<Ta> ntry) {
        if (ntry.uplink == null) {
            return;
        }
        for (int o = 0; o < ntry.brLst.size(); o++) {
            ntry.uplink.brLst.add(ntry.brLst.get(o));
        }
        doBier(ntry.uplink);
    }

    /**
     * get bier info
     *
     * @return calculated bier info
     */
    public tabLabelBier getBierI() {
        tabLabelBier res = new tabLabelBier();
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
            if (!ntry.nxtCon) {
                continue;
            }
            if (ntry.brBeg <= 0) {
                continue;
            }
            BigInteger msk = BigInteger.ZERO;
            for (int o = 0; o < ntry.brLst.size(); o++) {
                msk = msk.setBit(ntry.brLst.get(o).get());
            }
            tabLabelBierN per = new tabLabelBierN(ntry.iface, ntry.nxtHop, ntry.brBeg);
            per.ned = msk.shiftRight(1);
            res.peers.add(per);
        }
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
            s += " " + ntry.name + "=" + ntry.srIdx;
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
            s += " " + ntry.name;
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
            s += " " + ntry.name + "=" + ntry.brIdx;
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
            s += " " + ntry.name;
        }
        return s;
    }

    /**
     * list unreachables
     *
     * @return list of unreachable nodes
     */
    public String listUnreachables() {
        String s = "";
        for (int i = 0; i < nodes.size(); i++) {
            shrtPthFrstNode<Ta> ntry = nodes.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.metric >= 0) {
                continue;
            }
            s += " " + ntry.name;
        }
        return s;
    }

    /**
     * list reachables
     *
     * @return list of reachable nodes
     */
    public String listReachables() {
        String s = "";
        for (int i = 0; i < nodes.size(); i++) {
            shrtPthFrstNode<Ta> ntry = nodes.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.metric < 0) {
                continue;
            }
            s += " " + ntry.name;
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
            s += " " + ntry.name;
        }
        return s;
    }

    /**
     * list statistics
     *
     * @return list
     */
    public userFormat listStatistics() {
        userFormat res = new userFormat("|", "category|value");
        res.add("reach|" + listReachables());
        res.add("unreach|" + listUnreachables());
        res.add("stub|" + listStubs());
        res.add("segrou|" + listSegRou());
        res.add("nosegrou|" + listNoSegRou());
        res.add("bier|" + listBier());
        res.add("nobier|" + listNoBier());
        res.add("last|" + bits.time2str(cfgAll.timeZoneName, tim1 + cfgAll.timeServerOffset, 3) + " (" + bits.timePast(tim1) + " ago)");
        res.add("fill|" + (tim2 - tim1) + " ms");
        res.add("calc|" + (tim3 - tim2) + " ms");
        res.add("run|" + count + " times");
        return res;
    }

    /**
     * list statistics
     *
     * @return list
     */
    public userFormat listUsages() {
        userFormat res = new userFormat("|", "when|ago|time|unreach");
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
        shrtPthFrstNode<Ta> ntry = null;
        for (int i = 0; i < nodes.size(); i++) {
            shrtPthFrstNode<Ta> cur = nodes.get(i);
            if (cur.hops == 0) {
                ntry = cur;
            }
        }
        if (ntry == null) {
            return res;
        }
        listTree(res, ntry, "");
        return res;
    }

    private void listTree(List<String> res, shrtPthFrstNode<Ta> ntry, String pref) {
        List<shrtPthFrstConn<Ta>> down = new ArrayList<shrtPthFrstConn<Ta>>();
        for (int i = 0; i < ntry.conn.size(); i++) {
            shrtPthFrstConn<Ta> cur = ntry.conn.get(i);
            if (cur.target.uplink == null) {
                continue;
            }
            if (ntry.compare(ntry, cur.target.uplink) != 0) {
                continue;
            }
            down.add(cur);
        }
        res.add(pref + "`--" + ntry.name);
        for (int i = 0; i < down.size(); i++) {
            shrtPthFrstConn<Ta> cur = down.get(i);
            String a = (i + 1) == down.size() ? "   " : "  |";
            listTree(res, cur.target, pref + a);
        }
    }

    /**
     * list graphviz
     *
     * @return list
     */
    public List<String> listGraphviz() {
        List<String> res = new ArrayList<String>();
        res.add("echo \"graph net {");
        for (int o = 0; o < nodes.size(); o++) {
            shrtPthFrstNode<Ta> ntry = nodes.get(o);
            res.add("//" + ntry.name);
            for (int i = 0; i < ntry.conn.size(); i++) {
                shrtPthFrstConn<Ta> cur = ntry.conn.get(i);
                res.add("  \\\"" + ntry.name + "\\\" -- \\\"" + cur.target.name + "\\\" [weight=" + cur.metric + "]");
            }
        }
        res.add("}\" | dot -Tpng > net.png");
        return res;
    }

}

class shrtPthFrstLog {

    protected long when;

    protected int tim;

    protected String unreach;

    public String toString() {
        return bits.time2str(cfgAll.timeZoneName, when + cfgAll.timeServerOffset, 3) + "|" + bits.timePast(when) + "|" + tim + "|" + unreach;
    }

}

class shrtPthFrstConn<Ta extends Comparator<? super Ta>> {

    protected int metric;

    protected boolean realHop;

    protected shrtPthFrstNode<Ta> target;

}

class shrtPthFrstIdx implements Comparator<shrtPthFrstIdx> {

    protected final int val;

    public shrtPthFrstIdx(int i) {
        val = i;
    }

    public int get() {
        return val;
    }

    public int compare(shrtPthFrstIdx o1, shrtPthFrstIdx o2) {
        if (o1.val < o2.val) {
            return -1;
        }
        if (o1.val > o2.val) {
            return +1;
        }
        return 0;
    }

}

class shrtPthFrstNode<Ta extends Comparator<? super Ta>> implements Comparator<shrtPthFrstNode<Ta>> {

    protected Ta name;

    protected addrIP nxtHop;

    protected int nxtMet;

    protected boolean nxtCon;

    protected List<shrtPthFrstConn<Ta>> conn = new ArrayList<shrtPthFrstConn<Ta>>();

    protected shrtPthFrstNode<Ta> uplink;

    protected int metric;

    protected int hops;

    protected tabRouteIface iface;

    protected int srBeg;

    protected int srIdx;

    protected int brBeg;

    protected int brIdx;

    protected tabGen<shrtPthFrstIdx> brLst = new tabGen<shrtPthFrstIdx>();

    public shrtPthFrstNode(Ta nam) {
        nxtMet = Integer.MAX_VALUE;
        name = nam;
    }

    public int compare(shrtPthFrstNode<Ta> o1, shrtPthFrstNode<Ta> o2) {
        return o1.name.compare(o1.name, o2.name);
    }

}
