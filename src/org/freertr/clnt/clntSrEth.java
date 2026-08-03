package org.freertr.clnt;

import org.freertr.addr.addrIP;
import org.freertr.addr.addrMac;
import org.freertr.addr.addrType;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgVrf;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcUp;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipFwdTab;
import org.freertr.ip.ipPrt;
import org.freertr.pack.packHolder;
import org.freertr.util.bits;
import org.freertr.util.counter;
import org.freertr.util.logger;
import org.freertr.util.state;

/**
 * segment routing ethernet client
 *
 * @author matecsaba
 */
public class clntSrEth implements Runnable, ipPrt, ifcDn {

    /**
     * create instance
     */
    public clntSrEth() {
    }

    /**
     * protocol number
     */
    public final static int prot = 143;

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
     * counter
     */
    public counter cntr = new counter();

    private ipFwd fwdCor;

    private addrIP fwdTrg;

    private ipFwdIface fwdIfc;

    private boolean working = true;

    public String toString() {
        return "sreth to " + fwdTrg;
    }

    /**
     * sent endpoints
     *
     * @param fwd forwarder
     * @param ifc interface
     * @param adr address
     */
    public void setEndpoints(ipFwd fwd, ipFwdIface ifc, addrIP adr) {
        fwdCor = fwd;
        fwdIfc = ifc;
        fwdTrg = adr.copyBytes();
    }

    /**
     * get hw address
     *
     * @return address
     */
    public addrType getHwAddr() {
        return addrMac.getRandom();
    }

    /**
     * set filter mode
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
        return 1400;
    }

    /**
     * get bandwidth
     *
     * @return bandwidth
     */
    public long getBandwidth() {
        return 10000000;
    }

    /**
     * send packet
     *
     * @param pck packet
     */
    public void sendPack(packHolder pck) {
        pck.merge2beg();
        if (fwdIfc == null) {
            return;
        }
        cntr.tx(pck);
        pck.putDefaults();
        if (sendingTTL >= 0) {
            pck.IPttl = sendingTTL;
        }
        if (sendingTOS >= 0) {
            pck.IPtos = sendingTOS;
        }
        if (sendingDFN >= 0) {
            pck.IPdf = sendingDFN == 1;
        }
        if (sendingFLW >= 0) {
            pck.IPid = sendingFLW;
        }
        pck.IPprt = prot;
        pck.IPsrc.setAddr(fwdIfc.addr);
        pck.IPtrg.setAddr(fwdTrg);
        fwdCor.protoPack(fwdIfc, null, pck);
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
        fwdCor.protoAdd(this, fwdIfc, fwdTrg);
        for (;;) {
            if (!working) {
                break;
            }
            bits.sleep(1000);
        }
    }

    private void clearState() {
        if (fwdIfc != null) {
            fwdCor.protoDel(this, fwdIfc, fwdTrg);
            fwdIfc = null;
        }
    }

    /**
     * get protocol number
     *
     * @return number
     */
    public int getProtoNum() {
        return prot;
    }

    /**
     * close interface
     *
     * @param iface interface
     */
    public void closeUp(ipFwdIface iface) {
    }

    /**
     * set state
     *
     * @param iface interface
     * @param stat state
     */
    public void setState(ipFwdIface iface, state.states stat) {
    }

    /**
     * receive packet
     *
     * @param rxIfc interface
     * @param pck packet
     */
    public void recvPack(ipFwdIface rxIfc, packHolder pck) {
        cntr.rx(pck);
        upper.recvPack(pck);
    }

    /**
     * alert packet
     *
     * @param rxIfc interface
     * @param pck packet
     * @return false on success, true on error
     */
    public boolean alertPack(ipFwdIface rxIfc, packHolder pck) {
        return true;
    }

    /**
     * error packet
     *
     * @param err error
     * @param rtr address
     * @param rxIfc interface
     * @param pck packet
     */
    public void errorPack(counter.reasons err, addrIP rtr, ipFwdIface rxIfc, packHolder pck) {
    }

}
