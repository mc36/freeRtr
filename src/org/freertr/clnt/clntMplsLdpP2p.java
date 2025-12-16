package org.freertr.clnt;

import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrType;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcUp;
import org.freertr.ip.ipFwd;
import org.freertr.pack.packHolder;
import org.freertr.tab.tabLabel;
import org.freertr.tab.tabRouteEntry;
import org.freertr.util.bits;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.state;

/**
 * point to point ldp tunnel client
 *
 * @author matecsaba
 */
public class clntMplsLdpP2p implements Runnable, ifcDn {

    /**
     * create instance
     */
    public clntMplsLdpP2p() {
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
     * target of tunnel
     */
    public addrIP target = null;

    /**
     * tunnel id
     */
    public int trgId;

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

    private boolean working = true;

    private state.states lastStat = state.states.down;

    public String toString() {
        return "p2pldp to " + target;
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
        return lastStat;
    }

    /**
     * close interface
     */
    public void closeDn() {
        clearState();
    }

    /**
     * flap interface
     */
    public void flapped() {
        clearState();
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
        fwdCor.mplsTxPack(target, pck, false);
    }

    /**
     * get resulting route
     *
     * @param src source to use
     * @return route, null if no suitable
     */
    public tabRouteEntry<addrIP> getResultRoute(tabRouteEntry<addrIP> src) {
        tabRouteEntry<addrIP> prf = fwdCor.actualU.route(target);
        if (prf == null) {
            return null;
        }
        if (prf.best.labelRem == null) {
            return null;
        }
        src.best.iface = prf.best.iface;
        if (prf.best.nextHop != null) {
            src.best.nextHop = prf.best.nextHop;
        }
        src.best.labelRem = tabLabel.prependLabels(src.best.labelRem, prf.best.labelRem);
        return src;
    }

    /**
     * start connection
     */
    public void workStart() {
        if (debugger.clntMplsLdpTraf) {
            logger.debug("starting work");
        }
        new Thread(this).start();
    }

    /**
     * stop connection
     */
    public void workStop() {
        if (debugger.clntMplsLdpTraf) {
            logger.debug("stopping work");
        }
        working = false;
        clearState();
    }

    public void run() {
        for (;;) {
            if (!working) {
                break;
            }
            try {
                clearState();
                workDoer();
            } catch (Exception e) {
                logger.traceback(e);
            }
            clearState();
            bits.sleep(1000);
        }
    }

    private void workDoer() {
        for (;;) {
            if (!working) {
                return;
            }
            bits.sleep(1000);
            tabRouteEntry<addrIP> ntry = fwdCor.actualU.route(target);
            if (ntry == null) {
                protStat(state.states.down);
                continue;
            }
            if (ntry.best.labelRem == null) {
                protStat(state.states.down);
                continue;
            }
            if (ntry.best.labelRem.size() < 1) {
                protStat(state.states.down);
                continue;
            }
            protStat(state.states.up);
        }
    }

    private void protStat(state.states st) {
        if (st == lastStat) {
            return;
        }
        if (debugger.clntMplsLdpTraf) {
            logger.debug("session " + st);
        }
        lastStat = st;
        upper.setState(st);
    }

    private void clearState() {
        protStat(state.states.down);
    }

}
