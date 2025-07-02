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

    public addrType getHwAddr() {
        return new addrEmpty();
    }

    public void setFilter(boolean promisc) {
    }

    public state.states getState() {
        return state.states.up;
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
        if (debugger.clntPweTraf) {
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
                    return;
                }
                neighT.keepWorking();
                rtrLdpNeigh neighL = fwdCor.ldpNeighFind(targets[i], false);
                if (neighL == null) {
                    continue;
                }
                tabRouteEntry<addrIP> res = neighL.prefLearn.find(new addrPrefix<addrIP>(targets[i + 1], addrIP.size * 8));
                if (res == null) {
                    continue;
                }
                if (res.best.labelRem == null) {
                    continue;
                }
                labs.add(res.best.labelRem.get(0));
            }
            if (labs.size() >= (targets.length - 1)) {
                int[] res = new int[labs.size()];
                for (int i = 0; i < res.length; i++) {
                    res[i] = labs.get(i);
                }
                labels = res;
            } else {
                labels = null;
            }
            bits.sleep(1000);
        }
    }

    private void clearState() {
        labels = null;
    }

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
        vrf.getFwd(targets[0]).mplsTxPack(targets[0], pck, false);
    }

}
