package org.freertr.clnt;

import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrType;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgVrf;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcPpp;
import org.freertr.ifc.ifcUp;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipFwdTab;
import org.freertr.ip.ipMpls;
import org.freertr.pack.packHolder;
import org.freertr.pack.packLdpPwe;
import org.freertr.rtr.rtrLdpIface;
import org.freertr.rtr.rtrLdpNeigh;
import org.freertr.rtr.rtrLdpTrgtd;
import org.freertr.tab.tabLabel;
import org.freertr.tab.tabLabelEntry;
import org.freertr.util.bits;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.state;

/**
 * pseudo wire over mpls (rfc4447) client
 *
 * @author matecsaba
 */
public class clntMplsPwe implements Runnable, ifcDn {

    /**
     * create instance
     */
    public clntMplsPwe() {
    }

    /**
     * upper layer
     */
    public ifcUp upper = new ifcNull();

    /**
     * preferred ip protocol version
     */
    public int prefer = 0;

    /**
     * target of tunnel
     */
    public String target = null;

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

    private rtrLdpIface ldpIfc;

    private rtrLdpTrgtd neighT;

    private rtrLdpNeigh neighL;

    private tabLabelEntry labelL;

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

    /**
     * get local label
     *
     * @return label
     */
    public int getLabelLoc() {
        if (labelL == null) {
            return -1;
        }
        return labelL.label;
    }

    /**
     * get remote label
     *
     * @return label
     */
    public int getLabelRem() {
        if (pweR == null) {
            return -1;
        }
        return pweR.label;
    }

    /**
     * get remote address
     *
     * @return address
     */
    public addrIP getRemote() {
        if (fwdTrg == null) {
            return null;
        }
        return fwdTrg.copyBytes();
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
            bits.msbPutW(buf, 0, ifcPpp.preamble);
            return buf;
        }
        return null;
    }

    private void workDoer() {
        fwdTrg = clntDns.justResolv(target, prefer);
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
        if (fwdTrg.isIPv4()) {
            ldpIfc = srcIfc.mplsLdp4;
        } else {
            ldpIfc = srcIfc.mplsLdp6;
        }
        if (debugger.clntPweTraf) {
            logger.debug("starting targeted session");
        }
        neighT = fwdCor.ldpTargetFind(fwdIfc, ldpIfc, fwdTrg, true);
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
            neighT = fwdCor.ldpTargetFind(fwdIfc, null, fwdTrg, false);
            if (neighT == null) {
                return;
            }
            neighT.keepWorking();
            neighL = fwdCor.ldpNeighFind(fwdTrg, false);
            if (neighL != null) {
                break;
            }
            bits.sleep(1000);
        }
        if (debugger.clntPweTraf) {
            logger.debug("exchanging labels");
        }
        labelL = tabLabel.allocate(tabLabelEntry.owner.pwe);
        if (labelL == null) {
            return;
        }
        labelL.setFwdDrop(tabLabelEntry.owner.pwe);
        pweL = new packLdpPwe();
        pweL.srcA = fwdIfc.addr.copyBytes();
        pweL.trgA = fwdTrg.copyBytes();
        pweL.grp = 0;
        pweL.vcid = vcid;
        pweL.general = general;
        pweL.label = labelL.label;
        pweL.typ = pwType;
        pweL.ctrlWrd = ctrlWrd;
        pweL.mtu = pwMtu;
        if (ctrlWrd) {
            pweL.vccv = 0x100;
        } else {
            pweL.vccv = 0;
        }
        pweL.desc = descr;
        if (neighL == null) {
            return;
        }
        neighL.pweNeed2adv.put(pweL);
        for (;;) {
            if (!working) {
                return;
            }
            neighT = fwdCor.ldpTargetFind(fwdIfc, null, fwdTrg, false);
            if (neighT == null) {
                return;
            }
            neighT.keepWorking();
            neighL = fwdCor.ldpNeighFind(fwdTrg, false);
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
            labelL.setFwdPwe(tabLabelEntry.owner.pwe, fwdCor, upper, 4, getL2append());
        } else {
            labelL.setFwdPwe(tabLabelEntry.owner.pwe, fwdCor, upper, 0, getL2append());
        }
        bits.sleep(bits.random(1000, 9000));
        for (;;) {
            if (!working) {
                break;
            }
            neighT = fwdCor.ldpTargetFind(fwdIfc, null, fwdTrg, false);
            if (neighT == null) {
                break;
            }
            neighT.keepWorking();
            neighL = fwdCor.ldpNeighFind(fwdTrg, false);
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
            labelL.setFwdDrop(tabLabelEntry.owner.pwe);
            tabLabel.release(labelL, tabLabelEntry.owner.pwe);
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
