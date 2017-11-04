package ip;

import addr.addrIP;
import tab.tabRouteEntry;

/**
 * multicast to ldp handler
 *
 * @author matecsaba
 */
public class ipMldpIface {

    private ipFwd fwdCore;

    /**
     * interface
     */
    protected ipFwdIface iface;

    /**
     * create new instance
     *
     * @param fwd forwarder
     * @param ifc interface
     */
    public ipMldpIface(ipFwd fwd, ipFwdIface ifc) {
        fwdCore = fwd;
        iface = ifc;
    }

    /**
     * send one join
     *
     * @param grp group to join
     * @param need 1=join, 0=prune
     */
    public void sendJoin(ipFwdMcast grp, boolean need) {
        addrIP rot = grp.source.copyBytes();
        tabRouteEntry<addrIP> rou = fwdCore.actualU.route(rot);
        if (rou != null) {
            if (rou.oldHop != null) {
                rot = rou.oldHop.copyBytes();
            }
        }
        ipFwdMpmp ntry = ipFwdMpmp.create4multicast(false, rot, grp);
        if (need) {
            fwdCore.mldpAdd(ntry);
        } else {
            fwdCore.mldpDel(ntry);
        }
    }

}
