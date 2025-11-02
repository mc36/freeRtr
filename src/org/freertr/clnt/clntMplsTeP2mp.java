package org.freertr.clnt;

import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrType;
import org.freertr.cfg.cfgIfc;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcUp;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipFwdTab;
import org.freertr.ip.ipFwdTrfng;
import org.freertr.ip.ipMpls;
import org.freertr.pack.packHolder;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabLabel;
import org.freertr.tab.tabLabelDup;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.notifier;
import org.freertr.util.state;

/**
 * point to multipoint mpls te tunnel client
 *
 * @author matecsaba
 */
public class clntMplsTeP2mp implements Runnable, ifcDn {

    /**
     * create instance
     */
    public clntMplsTeP2mp() {
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
     * tunnel id
     */
    public addrIP trgId;

    /**
     * source interface
     */
    public cfgIfc srcIfc = null;

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
     * exclude affinity
     */
    public int affiE = 0;

    /**
     * include affinity
     */
    public int affiI = 0;

    /**
     * must affinity
     */
    public int affiM = 0;

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
     * record route
     */
    public boolean recRou;

    /**
     * counter
     */
    public counter cntr = new counter();

    private boolean working = false;

    private int srcId;

    private ipFwdIface fwdIfc;

    private tabGen<addrIP> targets = new tabGen<addrIP>();

    private ipFwdTrfng[] trfEngs = new ipFwdTrfng[0];

    private tabGen<tabLabelDup> fwdDups = new tabGen<tabLabelDup>();

    private notifier notif1 = new notifier();

    private notifier notif2 = new notifier();

    public String toString() {
        return "p2mpte to " + getTargets();
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
        return state.states.up;
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
     * send packet
     *
     * @param orig packet
     */
    public void sendPack(packHolder orig) {
        tabGen<tabLabelDup> trgs = fwdDups;
        orig.getSkip(2);
        cntr.tx(orig);
        for (int i = 0; i < trgs.size(); i++) {
            tabLabelDup ntry = trgs.get(i);
            packHolder pck = orig.copyBytes(true, true);
            pck.MPLSlabel = ntry.label.get(0);
            if (expr >= 0) {
                pck.MPLSexp = expr;
            }
            if (entr > 0) {
                pck.MPLSntr = entr;
            }
            if (mark > 0) {
                pck.MPLSmrk = mark;
            }
            if (ttl >= 0) {
                pck.MPLSttl = ttl;
            }
            ipMpls.createMPLSheader(pck);
            fwdCor.mplsTxPack(ntry.hop, pck, false);
        }
    }

    /**
     * set targets
     *
     * @param s targets
     */
    public void setTargets(String s) {
        targets = new tabGen<addrIP>();
        cmds c = new cmds("adrs", s);
        srcId = bits.randomW();
        for (;;) {
            s = c.word();
            if (s.length() < 1) {
                break;
            }
            addrIP a = new addrIP();
            if (a.fromString(s)) {
                continue;
            }
            targets.add(a);
        }
        setTargets(targets);
    }

    /**
     * set targets
     *
     * @param trg targets
     */
    public void setTargets(tabGen<addrIP> trg) {
        clearState();
        targets = trg;
        int i = targets.size();
        trfEngs = new ipFwdTrfng[i];
        if (i < 1) {
            notif1.wakeup();
            return;
        }
        addrIP fwdTrg = targets.get(0);
        fwdIfc = null;
        if (srcIfc != null) {
            fwdIfc = srcIfc.getFwdIfc(fwdTrg);
        }
        if (fwdIfc == null) {
            fwdIfc = ipFwdTab.findSendingIface(fwdCor, fwdTrg);
        }
        notif1.wakeup();
    }

    /**
     * add one target
     *
     * @param trg target
     */
    public void addTarget(addrIP trg) {
        targets.add(trg);
        setTargets(targets);
    }

    /**
     * delete one target
     *
     * @param trg target
     */
    public void delTarget(addrIP trg) {
        targets.del(trg);
        setTargets(targets);
    }

    /**
     * get targets
     *
     * @return targets
     */
    public String getTargets() {
        String s = "";
        for (int i = 0; i < targets.size(); i++) {
            s += " " + targets.get(i);
        }
        return s.trim();
    }

    /**
     * start connection
     */
    public void workStart() {
        if (debugger.clntMplsTeTraf) {
            logger.debug("starting work");
        }
        working = true;
        new Thread(this).start();
    }

    /**
     * wait until setup complete
     *
     * @param tim time to wait
     */
    public void wait4setup(int tim) {
        notif2.misleep(tim);
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
        notif1.wakeup();
    }

    public void run() {
        int rnd = 0;
        for (;;) {
            if (!working) {
                break;
            }
            try {
                workDoer(rnd);
                rnd = (rnd + 1) % 6;
            } catch (Exception e) {
                logger.traceback(e);
            }
            notif1.sleep(10000);
        }
    }

    private void clearState() {
        if (debugger.clntMplsTeTraf) {
            logger.debug("stopping sessions");
        }
        for (int i = 0; i < trfEngs.length; i++) {
            if (trfEngs[i] == null) {
                continue;
            }
            fwdCor.tetunDel(trfEngs[i]);
            trfEngs[i] = null;
        }
        trfEngs = new ipFwdTrfng[0];
        fwdDups = new tabGen<tabLabelDup>();
    }

    private void workDoer(int rnd) {
        tabGen<tabLabelDup> trgs = new tabGen<tabLabelDup>();
        for (int cur = 0; cur < trfEngs.length; cur++) {
            ipFwdTrfng trfEng = trfEngs[cur];
            if (trfEng != null) {
                trfEng = fwdCor.trafEngs.find(trfEng);
            }
            if (trfEng == null) {
                addrIP adr = targets.get(cur);
                trfEng = new ipFwdTrfng(fwdIfc.addr, srcId, adr, cur + 1);
                trfEng.srcIfc = fwdIfc;
                trfEng.trgAdr = adr.copyBytes();
                trfEng.trgId = (srcId << 16) | cur;
                trfEng.bwdt = ((float) bndwdt) / 8;
                trfEng.priS = prioS;
                trfEng.priH = prioH;
                trfEng.affE = affiE;
                trfEng.affI = affiI;
                trfEng.affM = affiM;
                trfEng.descr = descr;
                trfEng.recRou = recRou;
                if (debugger.clntMplsTeTraf) {
                    logger.debug("starting session " + trfEng);
                }
                fwdCor.tetunAdd(trfEng, true);
                trfEngs[cur] = trfEng;
            }
            if (trfEng.srcLoc != 1) {
                fwdCor.tetunDel(trfEng);
                trfEngs[cur] = null;
                continue;
            }
            if (trfEng.trgLab < 1) {
                fwdCor.tetunSignal(trfEng);
                continue;
            }
            if (rnd == 0) {
                fwdCor.tetunSignal(trfEng);
            }
            trgs.add(new tabLabelDup(null, trfEng.trgHop, tabLabel.int2labels(trfEng.trgLab)));
        }
        fwdDups = trgs;
        notif2.wakeup();
    }

}
