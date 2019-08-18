package rtr;

import auth.authLocal;
import addr.addrIP;
import addr.addrPrefix;
import cfg.cfgAll;
import ip.ipFwd;
import ip.ipFwdIface;
import ip.ipFwdMcast;
import ip.ipFwdTab;
import ip.ipRtr;

import java.util.List;

import prt.prtTcp;
import tab.tabGen;
import tab.tabRoute;
import tab.tabRouteEntry;
import user.userFormat;
import user.userHelping;
import util.bits;
import util.cmds;

/**
 * multicast source discovery (rfc3618) protocol
 *
 * @author matecsaba
 */
public class rtrMsdp extends ipRtr {

    /**
     * port to use
     */
    public static final int port = 639;

    /**
     * the forwarder protocol
     */
    protected ipFwd fwdCore;

    /**
     * the tcp protocol
     */
    protected prtTcp tcpCore;

    /**
     * list of neighbors
     */
    protected tabGen<rtrMsdpNeigh> neighs = new tabGen<rtrMsdpNeigh>();

    /**
     * router number
     */
    protected int rtrNum;

    /**
     * accepted sas
     */
    public tabGen<ipFwdMcast> cache = new tabGen<ipFwdMcast>();

    /**
     * create bgp process
     *
     * @param forwarder forwarder to update
     * @param protocol tcp protocol to use
     * @param id process id
     */
    public rtrMsdp(ipFwd forwarder, prtTcp protocol, int id) {
        fwdCore = forwarder;
        tcpCore = protocol;
        rtrNum = id;
    }

    /**
     * convert to string
     *
     * @return string
     */
    public String toString() {
        return "msdp on " + fwdCore;
    }

    /**
     * get neighbor count
     *
     * @return count
     */
    public int routerNeighCount() {
        return neighs.size();
    }

    /**
     * list neighbors
     *
     * @param tab list
     */
    public void routerNeighList(tabRoute<addrIP> tab) {
        for (int i = 0; i < neighs.size(); i++) {
            rtrMsdpNeigh nei = neighs.get(i);
            if (nei == null) {
                continue;
            }
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = new addrPrefix<addrIP>(nei.peer, addrIP.size * 8);
            tabRoute.addUpdatedEntry(tabRoute.addType.better, tab, rtrBgpUtil.safiUnicast, ntry, null, null, routerAutoMesh);
        }
    }

    /**
     * get interface count
     *
     * @return count
     */
    public int routerIfaceCount() {
        return 0;
    }

    private int getIface(addrIP adr) {
        ipFwdIface ifc = ipFwdTab.findSendingIface(fwdCore, adr);
        if (ifc == null) {
            return 0;
        }
        return ifc.ifwNum;
    }

    /**
     * create computed
     */
    public synchronized void routerCreateComputed() {
        tabGen<ipFwdMcast> lst = new tabGen<ipFwdMcast>();
        for (int o = 0; o < neighs.size(); o++) {
            rtrMsdpNeigh nei = neighs.get(o);
            if (nei == null) {
                continue;
            }
            int ifc = getIface(nei.peer);
            if (ifc == 0) {
                continue;
            }
            for (int i = 0; i < nei.learned.size(); i++) {
                ipFwdMcast ntry = nei.learned.get(i);
                if (getIface(ntry.upstream) != ifc) {
                    continue;
                }
                lst.add(ntry);
            }
        }
        cache = lst;
    }

    /**
     * redistribution changed
     */
    public void routerRedistChanged() {
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
        l.add("1 2   neighbor                    specify neighbor parameters");
        l.add("2 3     <addr>                    address of peer");
        l.add("3 .       enable                  enable this peer");
        l.add("3 4       description             describe this neighbor");
        l.add("4 .         <name>                description of neighbor");
        l.add("3 4       password                set session password");
        l.add("4 .         <text>                tcp password");
        l.add("3 4       update-source           connection source for this peer");
        l.add("4 .         <name>                name of interface");
        l.add("3 4       timer                   neighbor keepalive times");
        l.add("4 5         <num>                 keepalive in ms");
        l.add("5 6           <num>               hold time in ms");
        l.add("6 7             <num>             refresh time in ms");
        l.add("7 .               <num>           flush time in ms");
        l.add("3 .       shutdown                connection disabled for this peer");
        l.add("3 .       bfd                     enable bfd triggered down");
    }

    /**
     * get config
     *
     * @param l list
     * @param beg beginning
     * @param filter filter
     */
    public void routerGetConfig(List<String> l, String beg, boolean filter) {
        for (int i = 0; i < neighs.size(); i++) {
            rtrMsdpNeigh ntry = neighs.get(i);
            if (ntry == null) {
                continue;
            }
            ntry.getCfg(l, beg);
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
        if (s.equals("no")) {
            s = cmd.word();
            negated = true;
        }
        if (!s.equals("neighbor")) {
            return true;
        }
        rtrMsdpNeigh ntry = new rtrMsdpNeigh(this);
        if (ntry.peer.fromString(cmd.word())) {
            cmd.error("bad address");
            return false;
        }
        s = cmd.word();
        if (s.equals("enable")) {
            if (negated) {
                ntry = neighs.del(ntry);
                if (ntry == null) {
                    return false;
                }
                ntry.stopNow();
                return false;
            }
            if (neighs.add(ntry) != null) {
                return false;

            }
            ntry.startNow();
            return false;
        }
        ntry = neighs.find(ntry);
        if (ntry == null) {
            cmd.error("no such neighbor");
            return false;
        }
        if (s.equals("update-source")) {
            if (negated) {
                ntry.srcIface = null;
                return false;
            }
            ntry.srcIface = cfgAll.ifcFind(cmd.word(), false);
            if (ntry.srcIface == null) {
                cmd.error("no such interface");
            }
            return false;
        }
        if (s.equals("password")) {
            if (negated) {
                ntry.passwd = null;
                return false;
            }
            ntry.passwd = authLocal.passwdDecode(cmd.getRemaining());
            return false;
        }
        if (s.equals("description")) {
            if (negated) {
                ntry.description = null;
                return false;
            }
            ntry.description = cmd.getRemaining();
            return false;
        }
        if (s.equals("timer")) {
            ntry.keepAlive = bits.str2num(cmd.word());
            ntry.holdTimer = bits.str2num(cmd.word());
            ntry.freshTimer = bits.str2num(cmd.word());
            ntry.flushTimer = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("shutdown")) {
            ntry.shutdown = !negated;
            if (!negated) {
                ntry.closeNow();
            }
            return false;
        }
        if (s.equals("bfd")) {
            ntry.bfdTrigger = !negated;
            return false;
        }
        return true;
    }

    /**
     * stop work
     */
    public void routerCloseNow() {
        for (int i = 0; i < neighs.size(); i++) {
            rtrMsdpNeigh ntry = neighs.get(i);
            ntry.stopNow();
            ntry.closeNow();
        }
        fwdCore.routerDel(this);
    }

    /**
     * get neighbor show
     *
     * @return list of neighbors
     */
    public userFormat getNeighShow() {
        userFormat l = new userFormat("|", "learned|address|uptime");
        for (int i = 0; i < neighs.size(); i++) {
            rtrMsdpNeigh ntry = neighs.get(i);
            if (ntry == null) {
                continue;
            }
            l.add(ntry.learned.size() + "|" + ntry.peer + "|" + bits.timePast(ntry.upTime));
        }
        return l;
    }

    /**
     * get sources show
     *
     * @return list of sources
     */
    public userFormat getSourcesShow() {
        userFormat l = new userFormat("|", "source|group|upstream");
        for (int i = 0; i < cache.size(); i++) {
            ipFwdMcast ntry = cache.get(i);
            if (ntry == null) {
                continue;
            }
            l.add(ntry.source + "|" + ntry.group + "|" + ntry.upstream);
        }
        return l;
    }

}
