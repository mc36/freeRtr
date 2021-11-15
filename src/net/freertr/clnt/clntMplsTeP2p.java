package net.freertr.clnt;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import net.freertr.addr.addrEmpty;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrType;
import net.freertr.ifc.ifcDn;
import net.freertr.ifc.ifcNull;
import net.freertr.ifc.ifcUp;
import net.freertr.ip.ipFwd;
import net.freertr.ip.ipFwdIface;
import net.freertr.ip.ipFwdTab;
import net.freertr.ip.ipFwdTrfng;
import net.freertr.ip.ipMpls;
import net.freertr.pack.packHolder;
import net.freertr.tab.tabHop;
import net.freertr.tab.tabLabel;
import net.freertr.tab.tabRouteEntry;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.counter;
import net.freertr.util.debugger;
import net.freertr.util.logger;
import net.freertr.util.state;

/**
 * point to point mpls te tunnel client
 *
 * @author matecsaba
 */
public class clntMplsTeP2p implements Comparator<clntMplsTeP2p>, Runnable, ifcDn {

    /**
     * create instance
     */
    public clntMplsTeP2p() {
    }

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
     * middle targets
     */
    public List<addrIP> middles = new ArrayList<addrIP>();

    /**
     * pcep config to use
     */
    public String pcep = null;

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
     * association id
     */
    public int ascId = 0;

    /**
     * association global id
     */
    public int ascId2 = 0;

    /**
     * association address
     */
    public addrIP ascAdr = null;

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
     * set middle targets
     *
     * @param s targets
     */
    public void setMiddles(String s) {
        middles = new ArrayList<addrIP>();
        if (s == null) {
            return;
        }
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
            middles.add(a);
        }
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
        if (trfEng.trgLab < 1) {
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
        if (trfEng.trgLab < 1) {
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
        if (entr > 0) {
            pck.MPLSrnd = entr;
        }
        if (ttl >= 0) {
            pck.MPLSttl = ttl;
        }
        ipMpls.createMPLSheader(pck);
        fwdCor.mplsTxPack(trfEng.trgHop, pck, false);
    }

    /**
     * get resulting route
     *
     * @param src source to use
     * @return route, null if no suitable
     */
    public tabRouteEntry<addrIP> getResultRoute(tabRouteEntry<addrIP> src) {
        if (trfEng == null) {
            return null;
        }
        if (trfEng.trgLab < 1) {
            return null;
        }
        if (trfEng.srcLoc != 1) {
            return null;
        }
        src.best.nextHop = trfEng.trgHop.copyBytes();
        src.best.iface = trfEng.trgIfc;
        src.best.labelRem = tabLabel.int2labels(trfEng.trgLab);
        return src;
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
        if (pcep != null) {
            clntPcep pc = new clntPcep();
            pc.setTarget(pcep);
            if (pc.doConnect()) {
                return;
            }
            List<tabHop> res = pc.doCompute(0, ifc.addr.copyBytes(), target.copyBytes(), 0, 0, 0, prioS, prioH, ((float) bndwdt) / 8, 2, 0);
            pc.doClose();
            if (res == null) {
                return;
            }
            if (res.size() < 1) {
                return;
            }
        }
        if (debugger.clntMplsTeTraf) {
            logger.debug("starting session");
        }
        trfEng = new ipFwdTrfng(new addrIP(), 0, new addrIP(), 0);
        trfEng.srcIfc = ifc;
        trfEng.srcAdr = ifc.addr.copyBytes();
        trfEng.trgAdr = target.copyBytes();
        for (int i = 0; i < middles.size(); i++) {
            tabHop hop = new tabHop();
            hop.adr = middles.get(i).copyBytes();
            hop.strict = false;
            trfEng.midAdrs.add(hop);
        }
        trfEng.trgId = bits.randomD();
        trfEng.bwdt = ((float) bndwdt) / 8;
        trfEng.descr = descr;
        trfEng.recRou = recRou;
        if (ascAdr != null) {
            trfEng.asocAdr = ascAdr.copyBytes();
            trfEng.asocId = ascId;
            trfEng.asocGlb = ascId2;
            trfEng.asocTyp = 3;
        }
        fwdCor.tetunAdd(trfEng, false);
        for (int cnt = 0;; cnt++) {
            if (!working) {
                return;
            }
            trfEng = fwdCor.trafEngs.find(trfEng);
            if (trfEng == null) {
                return;
            }
            if (trfEng.srcLoc != 1) {
                return;
            }
            if (trfEng.trgLab > 1) {
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
