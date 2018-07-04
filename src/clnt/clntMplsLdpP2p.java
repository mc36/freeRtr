package clnt;

import addr.addrEmpty;
import addr.addrIP;
import addr.addrType;
import ifc.ifcDn;
import ifc.ifcNull;
import ifc.ifcUp;
import ip.ipFwd;
import pack.packHolder;
import tab.tabRouteEntry;
import util.bits;
import util.counter;
import util.debugger;
import util.logger;
import util.state;

/**
 * point to point ldp tunnel client
 *
 * @author matecsaba
 */
public class clntMplsLdpP2p implements Runnable, ifcDn {

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

    public addrType getHwAddr() {
        return new addrEmpty();
    }

    public void setFilter(boolean promisc) {
    }

    public state.states getState() {
        return lastStat;
    }

    public void closeDn() {
        clearState();
    }

    public void flapped() {
        clearState();
    }

    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
    }

    public counter getCounter() {
        return cntr;
    }

    public int getMTUsize() {
        return 1500;
    }

    public long getBandwidth() {
        return 8000000;
    }

    public void sendPack(packHolder pck) {
        pck.getSkip(2);
        cntr.tx(pck);
        if (expr >= 0) {
            pck.MPLSexp = expr;
        }
        if (ttl >= 0) {
            pck.MPLSttl = ttl;
        }
        fwdCor.mplsTxPack(target, pck, false);
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
            if (ntry.labelRem == null) {
                protStat(state.states.down);
                continue;
            }
            if (ntry.labelRem.size() < 1) {
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
