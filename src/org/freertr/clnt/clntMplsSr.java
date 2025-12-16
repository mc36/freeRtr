package org.freertr.clnt;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrType;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcUp;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipFwdTab;
import org.freertr.ip.ipMpls;
import org.freertr.pack.packHolder;
import org.freertr.tab.tabHop;
import org.freertr.tab.tabLabel;
import org.freertr.tab.tabRouteEntry;
import org.freertr.tab.tabRouteIface;
import org.freertr.util.cmds;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.notifier;
import org.freertr.util.state;

/**
 * sr over mpls tunnel client
 *
 * @author matecsaba
 */
public class clntMplsSr implements Runnable, ifcDn {

    /**
     * create instance
     */
    public clntMplsSr() {
    }

    /**
     * upper layer
     */
    public ifcUp upper = new ifcNull();

    /**
     * forwarder
     */
    public ipFwd fwdCor;

    /**
     * source interface
     */
    public ipFwdIface fwdIfc = null;

    /**
     * pcep config to use
     */
    public String pcep = null;

    /**
     * target
     */
    public addrIP target;

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
     * bandwidth
     */
    public long bndwdt;

    /**
     * counter
     */
    public counter cntr = new counter();

    private addrIP[] targets = new addrIP[1];

    private addrIP nextHop = new addrIP();

    private tabRouteIface nextIfc;

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
        if (entr > 0) {
            pck.MPLSntr = entr;
        }
        if (mark > 0) {
            pck.MPLSmrkV = mark;
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
     * get resulting route
     *
     * @param src source to use
     * @return route, null if no suitable
     */
    public tabRouteEntry<addrIP> getResultRoute(tabRouteEntry<addrIP> src) {
        int[] labs = labels;
        if (labs == null) {
            return null;
        }
        src.best.nextHop = nextHop.copyBytes();
        src.best.iface = nextIfc;
        src.best.labelRem = tabLabel.int2labels(labs[0]);
        return src;
    }

    /**
     * get resulting labels
     *
     * @return labels, null if no suitable
     */
    public int[] getLabels() {
        return labels;
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
        ipFwdIface ifc = fwdIfc;
        if (ifc == null) {
            ifc = ipFwdTab.findSendingIface(fwdCor, target);
        }
        if (ifc == null) {
            return;
        }
        if (pcep != null) {
            clntPcep pc = new clntPcep();
            pc.setTarget(pcep);
            if (pc.doConnect()) {
                if (debugger.clntMplsSrTraf) {
                    logger.debug("unable to connect pce for " + target);
                }
                clearState();
                return;
            }
            List<tabHop> res = pc.doCompute(1, ifc.addr.copyBytes(), target.copyBytes(), 0, 0, 0, prioS, prioH, ((float) bndwdt) / 8, 2, 0);
            pc.doClose();
            if (res == null) {
                if (debugger.clntMplsSrTraf) {
                    logger.debug("no info got for " + target);
                }
                clearState();
                return;
            }
            if (res.size() < 1) {
                if (debugger.clntMplsSrTraf) {
                    logger.debug("empty info got for " + target);
                }
                clearState();
                return;
            }
            tabHop hop = res.get(0);
            if (hop.label == 0) {
                if (debugger.clntMplsSrTraf) {
                    logger.debug("no label got for " + target);
                }
                clearState();
                return;
            }
            int[] labs = new int[1];
            tabRouteEntry<addrIP> ntry = fwdCor.actualU.route(target);
            if (ntry == null) {
                if (debugger.clntMplsSrTraf) {
                    logger.debug("no route for " + target);
                }
                clearState();
                return;
            }
            if (hop.index) {
                if (ntry.best.segrouBeg < 1) {
                    if (debugger.clntMplsSrTraf) {
                        logger.debug("no base for " + ntry);
                    }
                    clearState();
                    return;
                }
                labs[0] = ntry.best.segrouBeg + hop.label;
            } else {
                labs[0] = hop.label >>> 12;
            }
            nextHop = ntry.best.nextHop.copyBytes();
            nextIfc = ntry.best.iface;
            labels = labs;
            upper.setState(state.states.up);
            return;
        }
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
            if (prev.best.segrouIdx < 1) {
                if (debugger.clntMplsSrTraf) {
                    logger.debug("no index for " + prev);
                }
                clearState();
                return;
            }
            if (ntry.best.segrouOld < 1) {
                if (debugger.clntMplsSrTraf) {
                    logger.debug("no base for " + ntry);
                }
                clearState();
                return;
            }
            labs[i + 1] = ntry.best.segrouOld + prev.best.segrouIdx;
            prev = ntry;
        }
        if (prev.best.segrouBeg < 1) {
            if (debugger.clntMplsSrTraf) {
                logger.debug("no base for " + prev);
            }
            clearState();
            return;
        }
        labs[0] = prev.best.segrouBeg + prev.best.segrouIdx;
        nextHop = prev.best.nextHop.copyBytes();
        nextIfc = prev.best.iface;
        labels = labs;
        upper.setState(state.states.up);
    }

}
