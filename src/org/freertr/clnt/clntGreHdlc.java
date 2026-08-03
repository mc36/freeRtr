package org.freertr.clnt;

import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrType;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgVrf;
import org.freertr.cry.cryHashFcs16;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcHdlc;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcUp;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipFwdTab;
import org.freertr.pack.packHolder;
import org.freertr.pipe.pipeHdlc;
import org.freertr.prt.prtGre;
import org.freertr.util.bits;
import org.freertr.util.counter;
import org.freertr.util.logger;
import org.freertr.util.state;

/**
 * hdlc over gre
 *
 * @author matecsaba
 */
public class clntGreHdlc implements ifcDn, ifcUp, Runnable {

    /**
     * create instance
     */
    public clntGreHdlc() {
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
     * vrf of target
     */
    public cfgVrf vrf = null;

    /**
     * source interface
     */
    public cfgIfc srcIfc = null;

    /**
     * sending ttl value, -1 means maps out
     */
    public int sendingTTL = 255;

    /**
     * sending tos value, -1 means maps out
     */
    public int sendingTOS = -1;

    /**
     * sending df value, -1 means maps out
     */
    public int sendingDFN = -1;

    /**
     * sending flow value, -1 means maps out
     */
    public int sendingFLW = -1;

    /**
     * vc id
     */
    public int vcid;

    /**
     * counter
     */
    public counter cntr = new counter();

    private boolean working = true;

    private ipFwd fwdCor;

    private addrIP fwdTrg;

    private ipFwdIface fwdIfc;

    private prtGre gre;

    public String toString() {
        return "greppp " + fwdTrg;
    }

    /**
     * get hw address
     *
     * @return address
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
     * flapped interface
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
        cntr.tx(pck);
        if (gre == null) {
            return;
        }
        cryHashFcs16 sum = new cryHashFcs16();
        sum.init();
        pck.hashData(sum, 0, pck.dataSize());
        byte[] cb = sum.finish();
        pck.putCopy(cb, 0, 0, cb.length);
        pck.putSkip(cb.length);
        pck.putByte(0, pipeHdlc.charFlag);
        pck.putSkip(1);
        pck.merge2end();
        pck.msbPutW(0, ifcHdlc.ethtyp);
        pck.putByte(2, pipeHdlc.charFlag);
        pck.putSkip(3);
        pck.merge2beg();
        pck.putDefaults();
        gre.sendPack(pck);
    }

    /**
     * start connection
     */
    public void workStart() {
        logger.startThread(this);
    }

    /**
     * stop connection
     */
    public void workStop() {
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
        gre = new prtGre(fwdCor);
        gre.tunnelKyT = vcid;
        gre.tunnelKyR = vcid;
        gre.setEndpoints(fwdIfc, fwdTrg, true);
        gre.sendingTOS = sendingTOS;
        gre.sendingDFN = sendingDFN;
        gre.sendingFLW = sendingFLW;
        gre.sendingTTL = sendingTTL;
        gre.setUpper(this);
        for (;;) {
            bits.sleep(1000);
            if (!working) {
                break;
            }
        }
    }

    private void clearState() {
        if (gre == null) {
            return;
        }
        gre.closeDn();
        gre = null;
    }

    /**
     * receive packet
     *
     * @param pck packet
     */
    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        if (pck.msbGetW(0) != ifcHdlc.ethtyp) {
            return;
        }
        if (pck.getByte(2) != pipeHdlc.charFlag) {
            cntr.drop(pck, counter.reasons.badHdr);
            return;
        }
        pck.getSkip(3);
        int i = pck.dataSize() - 3;
        if (i < 0) {
            cntr.drop(pck, counter.reasons.tooSmall);
            return;
        }
        if (pck.getByte(i + 2) != pipeHdlc.charFlag) {
            cntr.drop(pck, counter.reasons.badFlag);
            return;
        }
        cryHashFcs16 sum = new cryHashFcs16();
        sum.init();
        pck.hashData(sum, 0, i);
        byte[] cb = sum.finish();
        byte[] cg = new byte[cb.length];
        pck.getCopy(cg, 0, i, cg.length);
        if (bits.byteComp(cg, 0, cb, 0, cg.length) != 0) {
            cntr.drop(pck, counter.reasons.badSum);
            return;
        }
        pck.setDataSize(i);
        upper.recvPack(pck);
    }

    /**
     * set upper layer
     *
     * @param parent upper layer
     */
    public void setParent(ifcDn parent) {
    }

    /**
     * set state
     *
     * @param stat state
     */
    public void setState(state.states stat) {
    }

    /**
     * close interface
     */
    public void closeUp() {
    }

}
