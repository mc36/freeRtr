package org.freertr.rtr;

import java.util.List;
import org.freertr.addr.addrIPv4;
import org.freertr.enc.encTlv;
import org.freertr.pack.packHolder;
import org.freertr.spf.spfLnkst;
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
        spfLnkst.listLinkStateAdd(parent.newlySpf, tlv, pck, -1, 0);
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
