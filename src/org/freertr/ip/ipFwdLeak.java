package org.freertr.ip;

import org.freertr.addr.addrIP;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgPrfxlst;
import org.freertr.cfg.cfgRoump;
import org.freertr.cfg.cfgRouplc;
import org.freertr.cfg.cfgTrack;
import org.freertr.clnt.clntTrack;
import org.freertr.rtr.rtrBgpUtil;
import org.freertr.tab.tabListing;
import org.freertr.tab.tabPrfxlstN;
import org.freertr.tab.tabRoute;
import org.freertr.tab.tabRouteAttr;
import org.freertr.tab.tabRouteEntry;
import org.freertr.tab.tabRtrmapN;
import org.freertr.tab.tabRtrplcN;
import org.freertr.util.bits;
import org.freertr.util.cmds;

/**
 * stores one unicast leak
 *
 * @author matecsaba
 */
public class ipFwdLeak implements Comparable<ipFwdLeak> {

    /**
     * create instance
     *
     * @param s source
     */
    public ipFwdLeak(ipFwd s) {
        src = s;
    }

    public int compareTo(ipFwdLeak o) {
        return src.compareTo(o.src);
    }

    /**
     * source vrf
     */
    public final ipFwd src;

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
     * tracker
     */
    public clntTrack track;

    /**
     * prefix list
     */
    public tabListing<tabPrfxlstN, addrIP> prflst;

    /**
     * route map
     */
    public tabListing<tabRtrmapN, addrIP> roumap;

    /**
     * route policy
     */
    public tabListing<tabRtrplcN, addrIP> rouplc;

    /**
     * convert from string
     *
     * @param cmd command to read
     * @return false on success, true on error
     */
    public boolean fromString(cmds cmd) {
        dist = 0;
        met = 0;
        tag = 0;
        prflst = null;
        roumap = null;
        rouplc = null;
        for (;;) {
            String a = cmd.word();
            if (a.length() < 1) {
                break;
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
            if (a.equals("prefix-list")) {
                cfgPrfxlst pl = cfgAll.prfxFind(cmd.word(), false);
                if (pl == null) {
                    cmd.error("no such route map");
                    return true;
                }
                prflst = pl.prflst;
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
            cmd.badCmd();
            return true;
        }
        return false;
    }

    public String toString() {
        String s = src.cfgName;
        if (dist != 0) {
            s += " distance " + dist;
        }
        if (met != 0) {
            s += " metric " + met;
        }
        if (tag != 0) {
            s += " tag " + tag;
        }
        if (prflst != null) {
            s += " prefix-list " + prflst.listName;
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
        return s;
    }

    /**
     * convert prefix, check ready status
     *
     * @param rou route to convert
     * @return prefix, null if nothing
     */
    public tabRouteEntry<addrIP> convPrefix(tabRouteEntry<addrIP> rou) {
        if (rou == null) {
            return null;
        }
        if (dist >= tabRouteAttr.distanMax) {
            return null;
        }
        if (track != null) {
            if (!track.getStatus()) {
                return null;
            }
        }
        rou = tabRoute.doUpdateEntry(rtrBgpUtil.sfiUnicast, 0, rou, roumap, rouplc, prflst);
        if (rou == null) {
            return null;
        }
        if (dist > 0) {
            rou.best.distance = dist;
        }
        if (met > 0) {
            rou.best.metric = met;
        }
        if (tag > 0) {
            rou.best.tag = tag;
        }
        return rou;
    }

}
