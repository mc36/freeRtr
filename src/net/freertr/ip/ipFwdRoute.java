package net.freertr.ip;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrIPv4;
import net.freertr.addr.addrIPv6;
import net.freertr.addr.addrPrefix;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgIfc;
import net.freertr.cfg.cfgRoump;
import net.freertr.cfg.cfgRouplc;
import net.freertr.cfg.cfgTrack;
import net.freertr.clnt.clntTrack;
import net.freertr.rtr.rtrBgpUtil;
import net.freertr.tab.tabListing;
import net.freertr.tab.tabRoute;
import net.freertr.tab.tabRouteAttr;
import net.freertr.tab.tabRouteEntry;
import net.freertr.tab.tabRtrmapN;
import net.freertr.tab.tabRtrplcN;
import net.freertr.util.bits;
import net.freertr.util.cmds;

/**
 * stores one unicast route
 *
 * @author matecsaba
 */
public class ipFwdRoute implements Comparator<ipFwdRoute> {

    /**
     * create instance
     */
    public ipFwdRoute() {
    }

    /**
     * prefix
     */
    public addrPrefix<addrIP> pref;

    /**
     * address
     */
    public addrIP addr;

    /**
     * mask
     */
    public addrIP mask;

    /**
     * next hop
     */
    public addrIP hop;

    /**
     * interface
     */
    public ipFwdIface iface;

    /**
     * id
     */
    public int id;

    /**
     * distance
     */
    public int dist;

    /**
     * metric
     */
    public int met;

    /**
     * tag
     */
    public int tag;

    /**
     * route map
     */
    public tabListing<tabRtrmapN, addrIP> roumap;

    /**
     * route policy
     */
    public tabListing<tabRtrplcN, addrIP> rouplc;

    /**
     * mpls mode: 0=untagged, 1=implicit, 2=explicit
     */
    public int mpls;

    /**
     * recursive mode: 0=connected, 1=igp, 2=bgp, 3=vpn
     */
    public int recur;

    /**
     * tracker
     */
    public clntTrack track;

    /**
     * forwarder core
     */
    protected ipFwd fwdCor;

    public int compare(ipFwdRoute o1, ipFwdRoute o2) {
        if (o1.id < o2.id) {
            return -1;
        }
        if (o1.id > o2.id) {
            return +1;
        }
        if (o1.dist < o2.dist) {
            return -1;
        }
        if (o1.dist > o2.dist) {
            return +1;
        }
        return o1.pref.compare(o1.pref, o2.pref);
    }

    private addrIP getAddr(int ver, String s) {
        addrIP a = new addrIP();
        if (ver == 4) {
            addrIPv4 a4 = new addrIPv4();
            if (a4.fromString(s)) {
                return null;
            }
            a.fromIPv4addr(a4);
        } else {
            addrIPv6 a6 = new addrIPv6();
            if (a6.fromString(s)) {
                return null;
            }
            a.fromIPv6addr(a6);
        }
        return a;
    }

    /**
     * convert from string
     *
     * @param ver protocol version
     * @param cmd command to read
     * @return false on success, true on error
     */
    public boolean fromString(int ver, cmds cmd) {
        dist = 255;
        addr = getAddr(ver, cmd.word());
        if (addr == null) {
            cmd.error("bad network");
            return true;
        }
        int msk;
        String a = cmd.word();
        if (a.startsWith("/")) {
            msk = bits.str2num(a.substring(1, a.length()));
            mask = new addrIP();
            if (ver == 4) {
                addrIPv4 m4 = new addrIPv4();
                m4.fromNetmask(msk);
                mask.fromIPv4addr(m4);
            } else {
                addrIPv6 m6 = new addrIPv6();
                m6.fromNetmask(msk);
                mask.fromIPv6addr(m6);
            }
        } else {
            mask = getAddr(ver, a);
            if (mask == null) {
                cmd.error("bad mask");
                return true;
            }
            if (ver == 4) {
                msk = mask.toIPv4().toNetmask();
            } else {
                msk = mask.toIPv6().toNetmask();
            }
        }
        hop = getAddr(ver, cmd.word());
        if (hop == null) {
            cmd.error("bad hop");
            return true;
        }
        if (ver == 4) {
            pref = addrPrefix.ip4toIP(new addrPrefix<addrIPv4>(addr.toIPv4(), msk));
        } else {
            pref = addrPrefix.ip6toIP(new addrPrefix<addrIPv6>(addr.toIPv6(), msk));
        }
        dist = 1;
        met = 0;
        tag = 0;
        mpls = 0;
        recur = 0;
        roumap = null;
        rouplc = null;
        for (;;) {
            a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            if (a.equals("id")) {
                id = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("distance")) {
                dist = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("metric")) {
                met = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("tag")) {
                tag = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("tracker")) {
                cfgTrack tr = cfgAll.trackFind(cmd.word(), false);
                if (tr == null) {
                    cmd.error("no such tracker");
                    return true;
                }
                track = tr.worker;
                continue;
            }
            if (a.equals("interface")) {
                cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
                if (ifc == null) {
                    cmd.error("no such interface");
                    return true;
                }
                if (ver == 4) {
                    iface = ifc.fwdIf4;
                } else {
                    iface = ifc.fwdIf6;
                }
                continue;
            }
            if (a.equals("route-map")) {
                cfgRoump rm = cfgAll.rtmpFind(cmd.word(), false);
                if (rm == null) {
                    cmd.error("no such route map");
                    return true;
                }
                roumap = rm.roumap;
                continue;
            }
            if (a.equals("route-policy")) {
                cfgRouplc rp = cfgAll.rtplFind(cmd.word(), false);
                if (rp == null) {
                    cmd.error("no such route map");
                    return true;
                }
                rouplc = rp.rouplc;
                continue;
            }
            if (a.equals("recurigp")) {
                recur = 1;
                continue;
            }
            if (a.equals("recurbgp")) {
                recur = 2;
                continue;
            }
            if (a.equals("recurvpn")) {
                recur = 3;
                continue;
            }
            if (a.equals("mplsimp")) {
                mpls = 1;
                continue;
            }
            if (a.equals("mplsexp")) {
                mpls = 2;
                continue;
            }
            cmd.badCmd();
            return true;
        }
        return false;
    }

    public String toString() {
        String s = addr + " " + mask + " " + hop;
        if (id != 0) {
            s += " id " + id;
        }
        if (iface != null) {
            s += " interface " + iface;
        }
        if (dist != 1) {
            s += " distance " + dist;
        }
        if (met != 0) {
            s += " metric " + met;
        }
        if (tag != 0) {
            s += " tag " + tag;
        }
        if (roumap != null) {
            s += " route-map " + roumap.listName;
        }
        if (rouplc != null) {
            s += " route-policy " + rouplc.listName;
        }
        if (track != null) {
            s += " tracker " + track.name;
        }
        switch (mpls) {
            case 1:
                s += " mplsimp";
                break;
            case 2:
                s += " mplsexp";
                break;
        }
        switch (recur) {
            case 1:
                s += " recurigp";
                break;
            case 2:
                s += " recurbgp";
                break;
            case 3:
                s += " recurvpn";
                break;
        }
        return s;
    }

    /**
     * get prefix, check ready status
     *
     * @return prefix, null if nothing
     */
    public tabRouteEntry<addrIP> getPrefix() {
        if (dist >= tabRouteAttr.distanMax) {
            return null;
        }
        if (track != null) {
            if (!track.getStatus()) {
                return null;
            }
        }
        tabRouteEntry<addrIP> prf = new tabRouteEntry<addrIP>();
        prf.prefix = pref.copyBytes();
        prf.best.nextHop = hop.copyBytes();
        prf.best.distance = dist;
        prf.best.metric = met;
        prf.best.tag = tag;
        prf.best.ident = id;
        switch (mpls) {
            case 1:
                List<Integer> lab = new ArrayList<Integer>();
                lab.add(ipMpls.labelImp);
                prf.best.labelRem = lab;
                break;
            case 2:
                lab = new ArrayList<Integer>();
                if (addr.isIPv4()) {
                    lab.add(ipMpls.labelExp4);
                } else {
                    lab.add(ipMpls.labelExp6);
                }
                prf.best.labelRem = lab;
                break;
        }
        prf.best.rouTyp = tabRouteAttr.routeType.staticRoute;
        tabRouteEntry<addrIP> res = tabRoute.doUpdateEntry(rtrBgpUtil.sfiUnicast, 0, prf, roumap, rouplc, null);
        if (res == null) {
            return prf;
        } else {
            return res;
        }
    }

}
