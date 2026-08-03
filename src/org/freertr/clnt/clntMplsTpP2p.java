package org.freertr.clnt;

import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrType;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcUp;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipMpls;
import org.freertr.pack.packHolder;
import org.freertr.tab.tabLabel;
import org.freertr.tab.tabLabelEntry;
import org.freertr.tab.tabRouteEntry;
import org.freertr.util.counter;
import org.freertr.util.state;

/**
 * point to point tp tunnel client
 *
 * @author matecsaba
 */
public class clntMplsTpP2p implements ifcDn {

    /**
     * create instance
     */
    public clntMplsTpP2p() {
    }

    /**
     * upper layer
     */
    public ifcUp upper = new ifcNull();

    /**
     * forwarder to use
     */
    public ipFwd fwdCor;

    /**
     * source interface
     */
    public ipFwdIface srcIfc = null;

    /**
     * target of tunnel
     */
    public addrIP target = null;

    /**
     * tunnel tx label
     */
    public int trgLab;

    /**
     * tunnel rx label
     */
    public int srcLab;

    /**
     * experimental value, -1 means maps out
     */
    public int expr = -1;

    /**
     * entropy value, -1 means maps out
     */
    public int entr = -1;

    /**
     * marking value, -1 means maps out
     */
    public int mark = -1;

    /**
     * ttl value
     */
    public int ttl = 255;

    /**
     * counter
     */
    public counter cntr = new counter();

    private tabLabelEntry resLab;

    public String toString() {
        return "p2ptp to " + target;
    }

    /**
     * get hw address
     *
     * @return hw address
     */
    public addrType getHwAddr() {
        return new addrEmpty();
    }

    /**
     * set filter
     *
     * @param promisc promuscous mode
     */
    public void setFilter(boolean promisc) {
    }

    /**
     * get state
     *
     * @return state
     */
    public state.states getState() {
        return state.states.up;
    }

    /**
     * close interface
     */
    public void closeDn() {
    }

    /**
     * flap interface
     */
    public void flapped() {
    }

    /**
     * set upper layer
     *
     * @param server upper layer
     */
    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
    }

    /**
     * get counter
     *
     * @return counter
     */
    public counter getCounter() {
        return cntr;
    }

    /**
     * get mtu size
     *
     * @return mtu size
     */
    public int getMTUsize() {
        return 1500;
    }

    /**
     * get bandwidth
     *
     * @return bandwidth
     */
    public long getBandwidth() {
        return 8000000;
    }

    /**
     * send packet
     *
     * @param pck packet
     */
    public void sendPack(packHolder pck) {
        pck.getSkip(2);
        cntr.tx(pck);
        if (expr >= 0) {
            pck.MPLSexp = expr;
        }
        if (entr > 0) {
            pck.MPLSntr = entr;
        }
        if (mark > 0) {
            pck.MPLSmrkV = mark;
        }
        if (ttl >= 0) {
            pck.MPLSttl = ttl;
        }
        pck.MPLSlabel = trgLab;
        ipMpls.createMPLSheader(pck);
        fwdCor.mplsTxPack(target, pck, false);
    }

    /**
     * get resulting route
     *
     * @param src source to use
     * @return route, null if no suitable
     */
    public tabRouteEntry<addrIP> getResultRoute(tabRouteEntry<addrIP> src) {
        src.best.iface = srcIfc;
        src.best.nextHop = target.copyBytes();
        src.best.labelRem = tabLabel.prependLabel(src.best.labelRem, trgLab);
        return src;
    }

    /**
     * start connection
     */
    public void workStart() {
        if (srcLab == 0) {
            srcLab = trgLab;
        }
        resLab = tabLabel.allocateExact(tabLabelEntry.owner.trnprf, srcLab);
        if (resLab == null) {
            return;
        }
        resLab.setFwdCommon(tabLabelEntry.owner.trnprf, fwdCor);
    }

    /**
     * stop connection
     */
    public void workStop() {
        tabLabel.release(resLab, tabLabelEntry.owner.trnprf);
    }

}
