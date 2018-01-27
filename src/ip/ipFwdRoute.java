package ip;

import addr.addrIP;
import addr.addrIPv4;
import addr.addrIPv6;
import addr.addrPrefix;
import cfg.cfgAll;
import cfg.cfgIfc;
import cfg.cfgTrack;
import clnt.clntTrack;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import tab.tabRouteEntry;
import util.bits;
import util.cmds;

/**
 * stores one unicast route
 *
 * @author matecsaba
 */
public class ipFwdRoute implements Comparator<ipFwdRoute> {

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
     * distance
     */
    public int dist;

    /**
     * tag
     */
    public int tag;

    /**
     * mpls mode: 0=untagged, 1=implicit, 2=explicit
     */
    public int mpls;

    /**
     * tracker
     */
    public clntTrack track;

    /**
     * forwarder core
     */
    protected ipFwd fwdCor;

    public int compare(ipFwdRoute o1, ipFwdRoute o2) {
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
        mask = getAddr(ver, cmd.word());
        if (mask == null) {
            cmd.error("bad mask");
            return true;
        }
        hop = getAddr(ver, cmd.word());
        if (hop == null) {
            cmd.error("bad hop");
            return true;
        }
        if (ver == 4) {
            pref = addrPrefix.ip4toIP(new addrPrefix<addrIPv4>(addr.toIPv4(), mask.toIPv4().toNetmask()));
        } else {
            pref = addrPrefix.ip6toIP(new addrPrefix<addrIPv6>(addr.toIPv6(), mask.toIPv6().toNetmask()));
        }
        dist = 1;
        tag = 0;
        for (;;) {
            String a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            if (a.equals("distance")) {
                dist = bits.str2num(cmd.word());
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
                cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
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
        if (iface != null) {
            s += " interface " + iface;
        }
        if (dist != 1) {
            s += " distance " + dist;
        }
        if (tag != 0) {
            s += " tag " + tag;
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
        return s;
    }

    /**
     * get prefix, check ready status
     *
     * @return prefix, null if nothing
     */
    public tabRouteEntry<addrIP> getPrefix() {
        if (dist >= tabRouteEntry.distanMax) {
            return null;
        }
        if (track != null) {
            if (!track.getStatus()) {
                return null;
            }
        }
        tabRouteEntry<addrIP> prf = new tabRouteEntry<addrIP>();
        prf.prefix = pref.copyBytes();
        prf.nextHop = hop.copyBytes();
        prf.distance = dist;
        prf.tag = tag;
        switch (mpls) {
            case 1:
                List<Integer> lab = new ArrayList<Integer>();
                lab.add(ipMpls.labelImp);
                prf.labelRem = lab;
                break;
            case 2:
                lab = new ArrayList<Integer>();
                if (addr.isIPv4()) {
                    lab.add(ipMpls.labelExp4);
                } else {
                    lab.add(ipMpls.labelExp6);
                }
                prf.labelRem = lab;
                break;
        }
        prf.rouTyp = tabRouteEntry.routeType.staticRoute;
        return prf;
    }

}
