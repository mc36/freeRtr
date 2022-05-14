package net.freertr.serv;

import net.freertr.addr.addrIP;
import net.freertr.addr.addrPrefix;
import net.freertr.tab.tabRouteEntry;
import net.freertr.tab.tabRouteIface;
import net.freertr.util.bits;
import net.freertr.util.logger;
import net.freertr.util.shrtPthFrst;
import net.freertr.util.state;

/**
 * one p4lang backplane discovery
 *
 * @author matecsaba
 */
public class servP4langDcvr implements Runnable {

    private final servP4lang parent;

    /**
     * need to run
     */
    protected boolean need2work = true;

    /**
     * random id
     */
    protected int randId;

    /**
     * create instance
     *
     * @param prnt parent
     */
    protected servP4langDcvr(servP4lang prnt) {
        randId = bits.randomD();
        parent = prnt;
        new Thread(this).start();
    }

    /**
     * send keepalives and check if spf needed
     *
     * @return true if changed, false if not
     */
    protected boolean doRound() {
        long tim = bits.getTime() - parent.discoTim;
        boolean chg = false;
        for (int o = 0; o < parent.fwds.size(); o++) {
            servP4langCfg cur = parent.fwds.get(o);
            for (int i = 0; i < cur.backPlanes.size(); i++) {
                servP4langBkpl ntry = cur.backPlanes.get(i);
                boolean res = ntry.ifc.getState() == state.states.up;
                if (res) {
                    ntry.sendHello();
                }
                res &= ntry.lastTime > tim;
                if (ntry.ready == res) {
                    continue;
                }
                ntry.ready = res;
                chg = true;
            }
        }
        return chg;
    }

    /**
     * calculate spf
     */
    protected void doCalc() {
        servP4langCfg cur = parent.fwds.get(0);
        shrtPthFrst<addrIP> spf = new shrtPthFrst<addrIP>(cur.bckplnSpf);
        for (int o = 0; o < parent.fwds.size(); o++) {
            cur = parent.fwds.get(o);
            addrIP adr = servP4langUtil.forwarder2addr(o);
            for (int i = 0; i < cur.backPlanes.size(); i++) {
                servP4langBkpl ntry = cur.backPlanes.get(i);
                if (!ntry.ready) {
                    continue;
                }
                addrIP nei = servP4langUtil.forwarder2addr(ntry.lastFwdr);
                spf.addConn(adr, nei, ntry.metric, true, false, "prt" + ntry.id);
            }
            tabRouteEntry<addrIP> rou = new tabRouteEntry<addrIP>();
            rou.prefix = new addrPrefix<addrIP>(adr, addrIP.size * 8);
            spf.addPref(adr, rou, false);
            spf.addIdent(adr, "fwd" + o);
        }
        spf.topoLog.set(1);
        cur = parent.fwds.get(0);
        cur.bckplnSpf = spf;
        for (int o = 1; o < parent.fwds.size(); o++) {
            parent.fwds.get(o).bckplnSpf = spf.copyBytes();
        }
        for (int o = 0; o < parent.fwds.size(); o++) {
            cur = parent.fwds.get(o);
            cur.bckplnSpf.bidir.set(1);
            cur.bckplnSpf.ecmp.set(1);
            addrIP adr = servP4langUtil.forwarder2addr(o);
            cur.bckplnSpf.doCalc(adr, null);
        }
        for (int o = 0; o < parent.fwds.size(); o++) {
            cur = parent.fwds.get(o);
            for (int i = 0; i < cur.backPlanes.size(); i++) {
                servP4langBkpl ntry = cur.backPlanes.get(i);
                if (!ntry.ready) {
                    continue;
                }
                addrIP nei = servP4langUtil.forwarder2addr(ntry.lastFwdr);
                tabRouteIface ifc = new tabRouteIface();
                ifc.ifwNum = ntry.id;
                cur.bckplnSpf.addNextHop(ntry.metric, nei, nei, ifc, null, null);
            }
            cur.bckplnRou = cur.bckplnSpf.getRoutes(null, 6, null, null);
        }
    }

    public void run() {
        for (;;) {
            if (!need2work) {
                break;
            }
            try {
                if (doRound()) {
                    doCalc();
                }
            } catch (Exception e) {
                logger.traceback(e);
            }
            bits.sleep(parent.discoInt);
        }
    }

}
