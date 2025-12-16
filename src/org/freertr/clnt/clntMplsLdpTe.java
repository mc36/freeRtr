package org.freertr.clnt;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrType;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgVrf;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcUp;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipMpls;
import org.freertr.pack.packHolder;
import org.freertr.rtr.rtrLdpIface;
import org.freertr.rtr.rtrLdpNeigh;
import org.freertr.rtr.rtrLdpTrgtd;
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
 * ldp te tunnel client
 *
 * @author matecsaba
 */
public class clntMplsLdpTe implements Runnable, ifcDn {

    /**
     * create instance
     */
    public clntMplsLdpTe() {
    }

    /**
     * upper layer
     */
    public ifcUp upper = new ifcNull();

    /**
     * forwarder to use
     */
    public cfgVrf vrf;

    /**
     * source interface
     */
    public cfgIfc srcIfc = null;

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

    private addrIP[] targets = new addrIP[0];

    private ipFwd nextVrf;

    private addrIP nextHop = new addrIP();

    private tabRouteIface nextIfc;

    private int[] labels = null;

    private boolean working = true;

    private notifier notif = new notifier();

    public String toString() {
        return "teldp to " + target;
    }

    /**
     * set targets
     *
     * @param s targets
     */
    public void setTargets(String s) {
        if (s == null) {
            s = "";
        }
        List<addrIP> trgs = new ArrayList<addrIP>();
        cmds c = new cmds("adrs", s + " " + target);
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
     * get targets
     *
     * @return targets
     */
    public String getTargets() {
        if (targets.length < 1) {
            return null;
        }
        String s = "";
        for (int i = 0; i < (targets.length - 1); i++) {
            s += " " + targets[i];
        }
        return s.trim();
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
        nextVrf.mplsTxPack(nextHop, pck, false);
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
        if (debugger.clntMplsLdpTraf) {
            logger.debug("starting targeted sessions");
        }
        for (int i = 0; i < (targets.length - 1); i++) {
            ipFwd fwdCor = vrf.getFwd(targets[i]);
            ipFwdIface fwdIfc = srcIfc.getFwdIfc(targets[i]);
            rtrLdpIface ldpIfc = srcIfc.getLdpIface(targets[i]);
            rtrLdpTrgtd neighT = fwdCor.ldpTargetFind(fwdIfc, ldpIfc, targets[i], true);
            if (neighT == null) {
                return;
            }
            if (neighT.tcp == null) {
                neighT.tcp = vrf.getTcp(targets[i]);
                neighT.udp = vrf.getUdp(targets[i]);
                neighT.workStart();
            }
            neighT.keepWorking();
        }
        nextVrf = vrf.getFwd(targets[0]);
        tabRouteEntry<addrIP> rou = nextVrf.actualU.route(targets[0]);
        if (rou == null) {
            if (debugger.clntMplsLdpTraf) {
                logger.debug("no route for " + targets[0]);
            }
            clearState();
            return;
        }
        if (rou.best.labelRem == null) {
            if (debugger.clntMplsLdpTraf) {
                logger.debug("no label for " + targets[0]);
            }
            clearState();
            return;
        }
        nextHop = rou.best.nextHop.copyBytes();
        nextIfc = rou.best.iface;
        int labs[] = new int[targets.length];
        labs[0] = rou.best.labelRem.get(0);
        for (int i = 0; i < (targets.length - 1); i++) {
            ipFwd fwdCor = vrf.getFwd(targets[i]);
            rtrLdpNeigh neighL = fwdCor.ldpNeighFind(targets[i], false);
            if (neighL == null) {
                if (debugger.clntMplsLdpTraf) {
                    logger.debug("no neighbor for " + targets[i]);
                }
                clearState();
                return;
            }
            rou = neighL.prefLearn.route(targets[i + 1]);
            if (rou == null) {
                if (debugger.clntMplsLdpTraf) {
                    logger.debug("no prefix for " + targets[i + 1]);
                }
                clearState();
                return;
            }
            if (rou.best.labelRem == null) {
                if (debugger.clntMplsLdpTraf) {
                    logger.debug("no label for " + targets[i + 1]);
                }
                clearState();
                return;
            }
            labs[i + 1] = rou.best.labelRem.get(0);
        }
        labels = labs;
        upper.setState(state.states.up);
    }

}
