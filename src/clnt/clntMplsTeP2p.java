package clnt;

import addr.addrEmpty;
import addr.addrIP;
import addr.addrType;
import ifc.ifcDn;
import ifc.ifcNull;
import ifc.ifcUp;
import ip.ipFwd;
import ip.ipFwdIface;
import ip.ipFwdTab;
import ip.ipFwdTrfng;
import ip.ipMpls;
import java.util.Comparator;
import pack.packHolder;
import util.bits;
import util.counter;
import util.debugger;
import util.logger;
import util.state;

/**
 * point to point mpls te tunnel client
 *
 * @author matecsaba
 */
public class clntMplsTeP2p implements Comparator<clntMplsTeP2p>, Runnable, ifcDn {

    /**
     * upper layer
     */
    public ifcUp upper = new ifcNull();

    /**
     * target of tunnel
     */
    public addrIP target;

    /**
     * vrf of target
     */
    public ipFwd fwdCor;

    /**
     * source interface
     */
    public ipFwdIface fwdIfc = null;

    /**
     * description
     */
    public String descr;

    /**
     * setup priority
     */
    public int prioS = 7;

    /**
     * holding priority
     */
    public int prioH = 7;

    /**
     * experimental value, -1 means maps out
     */
    public int expr = -1;

    /**
     * ttl value
     */
    public int ttl = 255;

    /**
     * bandwidth
     */
    public long bndwdt;

    /**
     * record route
     */
    public boolean recRou;

    /**
     * counter
     */
    public counter cntr = new counter();

    private boolean working = true;

    private ipFwdTrfng trfEng;

    private state.states lastStat = state.states.down;

    public String toString() {
        return "p2pte to " + target;
    }

    public int compare(clntMplsTeP2p o1, clntMplsTeP2p o2) {
        return o1.target.compare(o1.target, o2.target);
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
     * @param promisc promiscous mode
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
        return bndwdt;
    }

    /**
     * get traffeng handler
     *
     * @return handler
     */
    public ipFwdTrfng getTraffEng() {
        if (trfEng == null) {
            return null;
        }
        if (trfEng.trgHop == null) {
            return null;
        }
        if (trfEng.srcLoc != 1) {
            return null;
        }
        return trfEng;
    }

    /**
     * send packet
     *
     * @param pck packet
     */
    public void sendPack(packHolder pck) {
        if (trfEng == null) {
            return;
        }
        if (trfEng.trgHop == null) {
            return;
        }
        if (trfEng.srcLoc != 1) {
            return;
        }
        pck.getSkip(2);
        cntr.tx(pck);
        pck.MPLSlabel = trfEng.trgLab;
        if (expr >= 0) {
            pck.MPLSexp = expr;
        }
        if (ttl >= 0) {
            pck.MPLSttl = ttl;
        }
        ipMpls.createMPLSheader(pck);
        fwdCor.mplsTxPack(trfEng.trgHop, pck, false);
    }

    /**
     * start connection
     */
    public void workStart() {
        if (debugger.clntMplsTeTraf) {
            logger.debug("starting work");
        }
        new Thread(this).start();
    }

    /**
     * stop connection
     */
    public void workStop() {
        if (debugger.clntMplsTeTraf) {
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
        ipFwdIface ifc = fwdIfc;
        if (ifc == null) {
            ifc = ipFwdTab.findSendingIface(fwdCor, target);
        }
        if (ifc == null) {
            return;
        }
        if (debugger.clntMplsTeTraf) {
            logger.debug("starting session");
        }
        trfEng = new ipFwdTrfng(new addrIP(), 0, new addrIP(), 0);
        trfEng.srcIfc = ifc;
        trfEng.srcAdr = ifc.addr.copyBytes();
        trfEng.trgAdr = target.copyBytes();
        trfEng.trgId = bits.randomD();
        trfEng.bwdt = ((float) bndwdt) / 8;
        trfEng.descr = descr;
        trfEng.recRou = recRou;
        fwdCor.tetunAdd(trfEng, false);
        for (int cnt = 0;; cnt++) {
            if (!working) {
                return;
            }
            trfEng = fwdCor.trafEngs.find(trfEng);
            if (trfEng == null) {
                return;
            }
            if (trfEng.trgHop != null) {
                break;
            }
            if (cnt > 5) {
                fwdCor.tetunSignal(trfEng);
                cnt = 0;
            }
            bits.sleep(1000);
        }
        fwdCor.routerStaticChg();
        protStat(state.states.up);
        for (int cnt = 0;; cnt++) {
            if (!working) {
                return;
            }
            trfEng = fwdCor.trafEngs.find(trfEng);
            if (trfEng == null) {
                return;
            }
            if (trfEng.srcLoc != 1) {
                break;
            }
            bits.sleep(1000);
            if (cnt < 60) {
                continue;
            }
            fwdCor.tetunSignal(trfEng);
            cnt = 0;
        }
    }

    private void protStat(state.states st) {
        if (st == lastStat) {
            return;
        }
        if (debugger.clntMplsTeTraf) {
            logger.debug("session " + st);
        }
        lastStat = st;
        upper.setState(st);
    }

    private void clearState() {
        if (trfEng != null) {
            fwdCor.tetunDel(trfEng);
        }
        trfEng = null;
        protStat(state.states.down);
    }

}
