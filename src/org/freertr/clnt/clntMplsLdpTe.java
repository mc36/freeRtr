package org.freertr.clnt;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrPrefix;
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
import org.freertr.tab.tabRouteEntry;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;
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
     * entrp[y value, -1 means maps out
     */
    public int entr = -1;

    /**
     * ttl value
     */
    public int ttl = 255;

    /**
     * counter
     */
    public counter cntr = new counter();

    private boolean working = true;

    private addrIP[] targets = new addrIP[0];

    private int[] labels = null;

    private ipFwd firstCor;

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
        for (int i = 0; i < targets.length - 1; i++) {
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
        targets = ts;
    }

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
        pck.merge2beg();
        if (labels == null) {
            return;
        }
        pck.getSkip(2);
        cntr.tx(pck);
        if (expr >= 0) {
            pck.MPLSexp = expr;
        }
        if (entr > 0) {
            pck.MPLSrnd = entr;
        }
        if (ttl >= 0) {
            pck.MPLSttl = ttl;
        }
        for (int i = labels.length - 1; i >= 0; i--) {
            pck.MPLSlabel = labels[i];
            ipMpls.createMPLSheader(pck);
        }
        firstCor.mplsTxPack(targets[0], pck, false);
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

    private void clearState() {
        labels = null;
        upper.setState(state.states.down);
    }

    private void workDoer() {
        if (debugger.clntMplsLdpTraf) {
            logger.debug("starting targeted sessions");
        }
        for (int i = 0; i < targets.length - 1; i++) {
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
        List<Integer> labs;
        for (;;) {
            if (!working) {
                return;
            }
            labs = new ArrayList<Integer>();
            for (int i = 0; i < targets.length - 1; i++) {
                ipFwd fwdCor = vrf.getFwd(targets[i]);
                ipFwdIface fwdIfc = srcIfc.getFwdIfc(targets[i]);
                rtrLdpIface ldpIfc = srcIfc.getLdpIface(targets[i]);
                rtrLdpTrgtd neighT = fwdCor.ldpTargetFind(fwdIfc, ldpIfc, targets[i], false);
                if (neighT == null) {
                    if (debugger.clntMplsLdpTraf) {
                        logger.debug("no targeted for " + targets[i]);
                    }
                    clearState();
                    return;
                }
                neighT.keepWorking();
                rtrLdpNeigh neighL = fwdCor.ldpNeighFind(targets[i], false);
                if (neighL == null) {
                    if (debugger.clntMplsLdpTraf) {
                        logger.debug("no neighbor for " + targets[i]);
                    }
                    clearState();
                    return;
                }
                tabRouteEntry<addrIP> res = neighL.prefLearn.find(new addrPrefix<addrIP>(targets[i + 1], addrIP.size * 8));
                if (res == null) {
                    if (debugger.clntMplsLdpTraf) {
                        logger.debug("no prefix for " + targets[i + 1]);
                    }
                    clearState();
                    return;
                }
                if (res.best.labelRem == null) {
                    if (debugger.clntMplsLdpTraf) {
                        logger.debug("no label for " + targets[i + 1]);
                    }
                    clearState();
                    return;
                }
                labs.add(res.best.labelRem.get(0));
            }
            firstCor = vrf.getFwd(targets[0]);
            int[] res = new int[labs.size()];
            for (int i = 0; i < res.length; i++) {
                res[i] = labs.get(i);
            }
            labels = res;
            upper.setState(state.states.up);
            bits.sleep(1000);
        }
    }

}
