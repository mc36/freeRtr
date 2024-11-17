package org.freertr.rtr;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.enc.encTlv;
import org.freertr.pack.packHolder;
import org.freertr.spf.spfLnkst;
import org.freertr.tab.tabRouteEntry;
import org.freertr.util.cmds;

/**
 * bgp4 shortest path first
 *
 * @author matecsaba
 */
public class rtrBgpSpf {

    /**
     * enabled
     */
    public boolean enabled;

    /**
     * import distance
     */
    public int distance;

    private final rtrBgp parent;

    /**
     * create new instance
     *
     * @param p parent to use
     */
    public rtrBgpSpf(rtrBgp p) {
        parent = p;
        distance = 10;
    }

    /**
     * convert to string
     *
     * @return string
     */
    public String toString() {
        return "bgp on " + parent.fwdCore;
    }

    /**
     * get config
     *
     * @param l list to append
     * @param beg beginning
     */
    public void getConfig(List<String> l, String beg) {
        if (enabled) {
            l.add(beg + "enable");
        } else {
            l.add(cmds.tabulator + cmds.negated + beg + "enable");
        }
        l.add(beg + "distance " + distance);
    }

    private void doAdvertNei(encTlv tlv, packHolder pck, packHolder hlp, rtrBgpNeigh nei) {
        if (nei == null) {
            return;
        }
        if (!nei.conn.ready2adv) {
            return;
        }
        spfLnkst.listLinkStateHdr(tlv, pck, 4, 2);
        spfLnkst.listSpfNod(tlv, pck, hlp, parent.localAs, parent.routerID, 256); // local node
        spfLnkst.listSpfNod(tlv, pck, hlp, nei.remoteAs, nei.conn.peerRouterID, 257); // remote node
        spfLnkst.listSpfLnk(tlv, pck, nei.localAddr, nei.peerAddr);
        spfLnkst.listLinkStateAdd(parent.newlySpf, tlv, pck, 4, nei.spfMetric, 0);
    }

    private void doAdvertPfx(encTlv tlv, packHolder pck, packHolder hlp, tabRouteEntry<addrIP> rou) {
        if (rou == null) {
            return;
        }
        spfLnkst.listLinkStateHdr(tlv, pck, 4, 3);
        spfLnkst.listSpfNod(tlv, pck, hlp, parent.localAs, parent.routerID, 256); // local node
        spfLnkst.listLinkStatePrf(parent.newlySpf, tlv, pck, hlp, rou);
    }

    /**
     * merge routes to table
     */
    public void doAdvertise() {
        if (!enabled) {
            return;
        }
        encTlv tlv = spfLnkst.listLinkStateTlv();
        packHolder pck = new packHolder(true, true);
        packHolder hlp = new packHolder(true, true);
        spfLnkst.listLinkStateHdr(tlv, pck, 4, 1);
        spfLnkst.listSpfNod(tlv, pck, hlp, parent.localAs, parent.routerID, 256); // local node
        spfLnkst.listLinkStateAdd(parent.newlySpf, tlv, pck, 0, 0, 0);
        for (int i = 0; i < parent.neighs.size(); i++) {
            doAdvertNei(tlv, pck, hlp, parent.neighs.get(i));
        }
        for (int i = 0; i < parent.lstnNei.size(); i++) {
            doAdvertNei(tlv, pck, hlp, parent.lstnNei.get(i));
        }
        for (int i = 0; i < parent.routerRedistedU.size(); i++) {
            doAdvertPfx(tlv, pck, hlp, parent.routerRedistedU.get(i));
        }
    }

    /**
     * import routes from table
     *
     * @return other changes trigger full recomputation
     */
    public boolean doPeers() {
        return false;
    }

}
