package clnt;

import ifc.ifcDn;
import ifc.ifcNull;
import ifc.ifcUp;
import ip.ipFwd;
import ip.ipFwdIface;
import ip.ipMpls;
import pack.packHolder;
import pack.packLdpPwe;
import rtr.rtrLdpNeigh;
import rtr.rtrLdpTrgtd;
import tab.tabLabel;
import tab.tabLabelNtry;
import user.userTerminal;
import util.bits;
import util.counter;
import util.debugger;
import util.logger;
import util.state;
import addr.addrEmpty;
import addr.addrIP;
import addr.addrType;
import cfg.cfgIfc;
import cfg.cfgVrf;
import ip.ipFwdTab;

/**
 * pseudo wire over mpls (rfc4447) client
 *
 * @author matecsaba
 */
public class clntMplsPwe implements Runnable, ifcDn {

    /**
     * upper layer
     */
    public ifcUp upper = new ifcNull();

    /**
     * target of tunnel
     */
    public String target = null;

    /**
     * vrf of target
     */
    public cfgVrf vrf = null;

    /**
     * source interface
     */
    public cfgIfc srcIfc = null;

    /**
     * vc id
     */
    public long vcid;

    /**
     * generalized
     */
    public boolean general;

    /**
     * pseudowire type
     */
    public int pwType;

    /**
     * control word
     */
    public boolean ctrlWrd;

    /**
     * pseudowire mtu
     */
    public int pwMtu;

    /**
     * description
     */
    public String descr;

    /**
     * counter
     */
    public counter cntr = new counter();

    private boolean working = true;

    private ipFwd fwdCor;

    private addrIP fwdTrg;

    private ipFwdIface fwdIfc;

    private rtrLdpTrgtd neighT;

    private rtrLdpNeigh neighL;

    private tabLabelNtry labelL;

    private packLdpPwe pweL;

    private packLdpPwe pweR;

    private state.states lastStat = state.states.down;

    public String toString() {
        return "pwe " + fwdTrg + " " + vcid;
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
        return pwMtu;
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
        if (pweR == null) {
            return;
        }
        if (pwType == packLdpPwe.pwtPpp) {
            pck.getSkip(2);
        }
        if (ctrlWrd) {
            pck.msbPutD(0, 0); // control word
            pck.putSkip(4);
            pck.merge2beg();
        }
        ipMpls.beginMPLSfields(pck, false);
        pck.MPLSlabel = pweR.label;
        ipMpls.createMPLSheader(pck);
        fwdCor.mplsTxPack(fwdTrg, pck, false);
    }

    /**
     * start connection
     */
    public void workStart() {
        if (debugger.clntPweTraf) {
            logger.debug("starting work");
        }
        new Thread(this).start();
    }

    /**
     * stop connection
     */
    public void workStop() {
        if (debugger.clntPweTraf) {
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

    private byte[] getL2append() {
        if (pwType == packLdpPwe.pwtPpp) {
            byte[] buf = new byte[2];
            bits.msbPutW(buf, 0, 0xff03);
            return buf;
        }
        return null;
    }

    private void workDoer() {
        fwdTrg = userTerminal.justResolv(target, 0);
        if (fwdTrg == null) {
            return;
        }
        fwdCor = vrf.getFwd(fwdTrg);
        fwdIfc = null;
        if (srcIfc != null) {
            fwdIfc = srcIfc.getFwdIfc(fwdTrg);
        }
        if (fwdIfc == null) {
            fwdIfc = ipFwdTab.findSendingIface(fwdCor, fwdTrg);
        }
        if (fwdIfc == null) {
            return;
        }
        if (debugger.clntPweTraf) {
            logger.debug("starting targeted session");
        }
        neighT = fwdCor.ldpTargetFind(fwdIfc, fwdTrg, true);
        if (neighT == null) {
            return;
        }
        if (neighT.tcp == null) {
            neighT.tcp = vrf.getTcp(fwdTrg);
            neighT.udp = vrf.getUdp(fwdTrg);
            neighT.workStart();
        }
        for (;;) {
            if (!working) {
                return;
            }
            neighT = fwdCor.ldpTargetFind(fwdIfc, fwdTrg, false);
            if (neighT == null) {
                return;
            }
            neighT.keepWorking();
            neighL = fwdCor.ldpNeighFind(null, fwdTrg, false);
            if (neighL != null) {
                break;
            }
            bits.sleep(1000);
        }
        if (debugger.clntPweTraf) {
            logger.debug("exchanging labels");
        }
        labelL = tabLabel.allocate(3);
        if (labelL == null) {
            return;
        }
        labelL.setFwdDrop(3);
        pweL = new packLdpPwe();
        pweL.srcA = fwdIfc.addr.copyBytes();
        pweL.trgA = fwdTrg.copyBytes();
        pweL.grp = 0;
        pweL.vcid = vcid;
        pweL.general = general;
        pweL.label = labelL.getValue();
        pweL.typ = pwType;
        pweL.ctrlWrd = ctrlWrd;
        pweL.mtu = pwMtu;
        if (ctrlWrd) {
            pweL.vccv = 0x100;
        } else {
            pweL.vccv = 0;
        }
        pweL.desc = descr;
        neighL.pweNeed2adv.put(pweL);
        for (;;) {
            if (!working) {
                return;
            }
            neighT = fwdCor.ldpTargetFind(fwdIfc, fwdTrg, false);
            if (neighT == null) {
                return;
            }
            neighT.keepWorking();
            neighL = fwdCor.ldpNeighFind(null, fwdTrg, false);
            if (neighL == null) {
                return;
            }
            if (neighL.pweNeed2adv.find(pweL) == null) {
                return;
            }
            pweR = neighL.pweLearn.find(pweL);
            if (pweR != null) {
                break;
            }
            bits.sleep(1000);
        }
        protStat(state.states.up);
        if (ctrlWrd) {
            labelL.setFwdPwe(3, fwdCor, upper, 4, getL2append());
        } else {
            labelL.setFwdPwe(3, fwdCor, upper, 0, getL2append());
        }
        bits.sleep(bits.random(1000, 9000));
        for (;;) {
            if (!working) {
                break;
            }
            neighT = fwdCor.ldpTargetFind(fwdIfc, fwdTrg, false);
            if (neighT == null) {
                break;
            }
            neighT.keepWorking();
            neighL = fwdCor.ldpNeighFind(null, fwdTrg, false);
            if (neighL == null) {
                break;
            }
            if (neighL.pweNeed2adv.find(pweL) == null) {
                break;
            }
            pweR = neighL.pweLearn.find(pweL);
            if (pweR == null) {
                break;
            }
            bits.sleep(1000);
        }
    }

    private void protStat(state.states st) {
        if (st == lastStat) {
            return;
        }
        if (debugger.clntPweTraf) {
            logger.debug("session " + st);
        }
        lastStat = st;
        upper.setState(st);
    }

    private void clearState() {
        if (neighT != null) {
            neighT.keepWorking();
        }
        if (labelL != null) {
            labelL.setFwdDrop(3);
            tabLabel.release(labelL, 3);
        }
        if ((pweL != null) && (neighL != null)) {
            neighL.pweNeed2adv.del(pweL);
        }
        neighL = null;
        neighT = null;
        labelL = null;
        pweL = null;
        pweR = null;
        protStat(state.states.down);
    }

}
