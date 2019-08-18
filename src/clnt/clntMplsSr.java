package clnt;

import addr.addrEmpty;
import addr.addrIP;
import addr.addrType;
import ifc.ifcDn;
import ifc.ifcNull;
import ifc.ifcUp;
import ip.ipFwd;
import ip.ipMpls;
import java.util.ArrayList;
import java.util.List;
import pack.packHolder;
import tab.tabRouteEntry;
import util.cmds;
import util.counter;
import util.debugger;
import util.logger;
import util.notifier;
import util.state;

/**
 * sr over mpls tunnel client
 *
 * @author matecsaba
 */
public class clntMplsSr implements Runnable, ifcDn {

    /**
     * upper layer
     */
    public ifcUp upper = new ifcNull();

    /**
     * forwarder
     */
    public ipFwd fwdCor;

    /**
     * target
     */
    public addrIP target;

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

    private addrIP[] targets = new addrIP[1];

    private addrIP nextHop = new addrIP();

    private int[] labels = null;

    private boolean working = false;

    private notifier notif = new notifier();

    public String toString() {
        return "srmpls to " + target;
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
        if (labels == null) {
            return state.states.down;
        } else {
            return state.states.up;
        }
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
        int[] labs = labels;
        if (labs == null) {
            return;
        }
        pck.getSkip(2);
        cntr.tx(pck);
        if (expr >= 0) {
            pck.MPLSexp = expr;
        }
        if (ttl >= 0) {
            pck.MPLSttl = ttl;
        }
        for (int i = labs.length - 1; i >= 0; i--) {
            pck.MPLSlabel = labs[i];
            ipMpls.createMPLSheader(pck);
        }
        fwdCor.mplsTxPack(nextHop, pck, false);
    }

    /**
     * set targets
     *
     * @param s targets
     */
    public void setTargets(String s) {
        List<addrIP> trgs = new ArrayList<addrIP>();
        cmds c = new cmds("adrs", s);
        for (;;) {
            s = c.word();
            if (s.length() < 1) {
                break;
            }
            addrIP a = new addrIP();
            if (a.fromString(s)) {
                continue;
            }
            trgs.add(a);
        }
        setTargets(trgs);
    }

    /**
     * set targets
     *
     * @param trg targets
     */
    public void setTargets(List<addrIP> trg) {
        clearState();
        addrIP[] ts = new addrIP[trg.size()];
        for (int i = 0; i < ts.length; i++) {
            ts[i] = trg.get(i).copyBytes();
        }
        labels = null;
        targets = ts;
        notif.wakeup();
    }

    /**
     * get targets
     *
     * @return targets
     */
    public String getTargets() {
        String s = "";
        for (int i = 0; i < targets.length; i++) {
            s += " " + targets[i];
        }
        return s.trim();
    }

    /**
     * start connection
     */
    public void workStart() {
        if (debugger.clntMplsSrTraf) {
            logger.debug("starting work");
        }
        working = true;
        new Thread(this).start();
    }

    /**
     * stop connection
     */
    public void workStop() {
        if (debugger.clntMplsSrTraf) {
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
                workDoer();
            } catch (Exception e) {
                logger.traceback(e);
            }
            notif.sleep(5000);
        }
    }

    private void clearState() {
        labels = null;
        upper.setState(state.states.down);
    }

    private void workDoer() {
        int[] labs = new int[targets.length + 1];
        tabRouteEntry<addrIP> prev = fwdCor.actualU.route(target);
        if (prev == null) {
            if (debugger.clntMplsSrTraf) {
                logger.debug("no route for " + target);
            }
            clearState();
            return;
        }
        for (int i = targets.length - 1; i >= 0; i--) {
            tabRouteEntry<addrIP> ntry = fwdCor.actualU.route(targets[i]);
            if (ntry == null) {
                if (debugger.clntMplsSrTraf) {
                    logger.debug("no route for " + targets[i]);
                }
                clearState();
                return;
            }
            if (prev.segrouIdx < 1) {
                if (debugger.clntMplsSrTraf) {
                    logger.debug("no index for " + prev);
                }
                clearState();
                return;
            }
            if (ntry.segrouOld < 1) {
                if (debugger.clntMplsSrTraf) {
                    logger.debug("no base for " + ntry);
                }
                clearState();
                return;
            }
            labs[i + 1] = ntry.segrouOld + prev.segrouIdx;
            prev = ntry;
        }
        if (prev.segrouBeg < 1) {
            if (debugger.clntMplsSrTraf) {
                logger.debug("no base for " + prev);
            }
            clearState();
            return;
        }
        labs[0] = prev.segrouBeg + prev.segrouIdx;
        nextHop = prev.nextHop.copyBytes();
        labels = labs;
        upper.setState(state.states.up);
    }

}
