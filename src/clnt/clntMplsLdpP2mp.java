package clnt;

import addr.addrEmpty;
import addr.addrIP;
import addr.addrType;
import cfg.cfgVrf;
import ifc.ifcDn;
import ifc.ifcNull;
import ifc.ifcUp;
import ip.ipFwd;
import ip.ipFwdMpmp;
import pack.packHolder;
import user.userTerminal;
import util.bits;
import util.counter;
import util.debugger;
import util.logger;
import util.state;

/**
 * point to multipoint mldp tunnel client
 *
 * @author matecsaba
 */
public class clntMplsLdpP2mp implements Runnable, ifcDn {

    /**
     * upper layer
     */
    public ifcUp upper = new ifcNull();

    /**
     * multipoint to multipoint
     */
    public boolean mp2mp;

    /**
     * target of tunnel
     */
    public String target = null;

    /**
     * vrf of target
     */
    public cfgVrf vrf = null;

    /**
     * tunnel id
     */
    public int trgId;

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

    private boolean working = true;

    private ipFwd fwdCor;

    private addrIP fwdTrg;

    private ipFwdMpmp mpLsp;

    public String toString() {
        return "p2mpldp to " + fwdTrg;
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
        return 8000000;
    }

    /**
     * send packet
     *
     * @param pck packet
     */
    public void sendPack(packHolder pck) {
        if (mpLsp == null) {
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
        mpLsp.sendPack(fwdCor, pck);
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
        fwdTrg = userTerminal.justResolv(target, 0);
        if (fwdTrg == null) {
            return;
        }
        fwdCor = vrf.getFwd(fwdTrg);
        mpLsp = ipFwdMpmp.create4tunnel(mp2mp, fwdTrg, trgId);
        fwdCor.mldpAdd(mpLsp);
        if (debugger.clntMplsLdpTraf) {
            logger.debug("session up");
        }
        for (;;) {
            if (!working) {
                return;
            }
            mpLsp = fwdCor.mp2mpLsp.find(mpLsp);
            if (mpLsp == null) {
                return;
            }
            if (!mpLsp.local) {
                mpLsp.local = true;
                mpLsp.updateState(fwdCor);
            }
            bits.sleep(1000);
        }
    }

    private void clearState() {
        if (mpLsp != null) {
            fwdCor.mldpDel(mpLsp);
        }
        mpLsp = null;
        fwdCor = null;
        fwdTrg = null;
    }

}
