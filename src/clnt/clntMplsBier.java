package clnt;

import addr.addrEmpty;
import addr.addrIP;
import addr.addrType;
import ifc.ifcBridge;
import ifc.ifcDn;
import ifc.ifcNull;
import ifc.ifcUp;
import ip.ipFwd;
import ip.ipIfc4;
import ip.ipIfc6;
import ip.ipMpls;
import java.math.BigInteger;
import pack.packHolder;
import tab.tabGen;
import tab.tabLabelBier;
import tab.tabLabelBierN;
import tab.tabRouteEntry;
import util.cmds;
import util.counter;
import util.debugger;
import util.logger;
import util.notifier;
import util.state;

/**
 * mpls bier tunnel client
 *
 * @author matecsaba
 */
public class clntMplsBier implements Runnable, ifcDn {

    /**
     * upper layer
     */
    public ifcUp upper = new ifcNull();

    /**
     * forwarder
     */
    public ipFwd fwdCor;

    /**
     * source id
     */
    public int srcId = 0;

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

    private boolean working = false;

    private tabGen<addrIP> targets = new tabGen<addrIP>();

    private tabGen<tabLabelBierN> fwdDups = new tabGen<tabLabelBierN>();

    private notifier notif1 = new notifier();

    private notifier notif2 = new notifier();

    public String toString() {
        return "bier to " + getTargets();
    }

    /**
     * get hw address
     *
     * @return
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
    }

    /**
     * flap interface
     */
    public void flapped() {
    }

    /**
     * set upper
     *
     * @param server upper
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
     * @param orig packet
     */
    public void sendPack(packHolder orig) {
        int prt = orig.msbGetW(0);
        orig.getSkip(2);
        cntr.tx(orig);
        switch (prt) {
            case ipMpls.typeU:
            case ipMpls.typeM:
                prt = ipMpls.bierLabD;
                break;
            case ipIfc4.type:
                prt = ipMpls.bierIp4;
                break;
            case ipIfc6.type:
                prt = ipMpls.bierIp6;
                break;
            case ifcBridge.serialType:
                prt = ipMpls.bierEth;
                break;
            default:
                return;
        }
        orig.BIERid = srcId;
        orig.BIERoam = 0;
        orig.IPprt = prt;
        if (expr >= 0) {
            orig.MPLSexp = expr;
        }
        if (ttl >= 0) {
            orig.MPLSttl = ttl;
        }
        tabGen<tabLabelBierN> trgs = fwdDups;
        for (int i = 0; i < trgs.size(); i++) {
            tabLabelBierN trg = trgs.get(i);
            BigInteger ned = trg.ned;
            int sft = tabLabelBier.bsl2num(trg.len);
            for (int o = 0;; o++) {
                if (ned.bitCount() < 1) {
                    break;
                }
                BigInteger cur = ned.and(trg.msk);
                ned = ned.shiftRight(sft);
                if (cur.bitCount() < 1) {
                    continue;
                }
                packHolder pck = orig.copyBytes(true, true);
                pck.BIERsi = o;
                pck.BIERbsl = trg.len;
                pck.BIERbs = cur;
                ipMpls.createBIERheader(pck);
                pck.MPLSlabel = trg.lab + o;
                ipMpls.createMPLSheader(pck);
                fwdCor.mplsTxPack(trg.hop, pck, false);
            }
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
        targets = trg;
        if (targets.size() < 1) {
            notif1.wakeup();
            return;
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
        if (debugger.clntMplsBierTraf) {
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
        if (debugger.clntMplsBierTraf) {
            logger.debug("stop work");
        }
        working = false;
        notif1.wakeup();
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
            notif1.sleep(10000);
        }
    }

    private void workDoer() {
        tabGen<tabLabelBierN> trgs = new tabGen<tabLabelBierN>();
        for (int i = 0; i < targets.size(); i++) {
            addrIP trg = targets.get(i);
            if (trg == null) {
                continue;
            }
            tabRouteEntry<addrIP> rou = fwdCor.actualU.route(trg);
            if (rou == null) {
                if (debugger.clntMplsBierTraf) {
                    logger.debug("no route for " + trg);
                }
                continue;
            }
            if (rou.oldHop != null) {
                rou = fwdCor.actualU.route(rou.oldHop);
                if (rou == null) {
                    continue;
                }
            }
            if (rou.bierIdx < 1) {
                if (debugger.clntMplsBierTraf) {
                    logger.debug("no index for " + trg);
                }
                continue;
            }
            if (rou.bierBeg < 1) {
                if (debugger.clntMplsBierTraf) {
                    logger.debug("no base for " + trg);
                }
                continue;
            }
            tabLabelBierN ntry = new tabLabelBierN(rou.iface, rou.nextHop, rou.bierBeg);
            tabLabelBierN old = trgs.add(ntry);
            if (old != null) {
                ntry = old;
            } else {
                ntry.ned = BigInteger.ZERO;
            }
            ntry.len = rou.bierHdr;
            ntry.ned = ntry.ned.setBit(rou.bierIdx - 1);
        }
        for (int i = 0; i < trgs.size(); i++) {
            tabLabelBierN ntry = trgs.get(i);
            ntry.msk = tabLabelBier.bsl2msk(ntry.len);
        }
        fwdDups = trgs;
        notif2.wakeup();
    }

}
