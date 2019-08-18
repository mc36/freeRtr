package clnt;

import addr.addrIP;
import addr.addrMac;
import addr.addrType;
import cfg.cfgIfc;
import cfg.cfgVrf;
import ifc.ifcDn;
import ifc.ifcNull;
import ifc.ifcUp;
import ip.ipFwd;
import ip.ipFwdIface;
import ip.ipFwdTab;
import ip.ipPrt;
import pack.packErspan;
import pack.packHolder;
import user.userTerminal;
import util.bits;
import util.counter;
import util.logger;
import util.state;

/**
 * encapsulated remote switch port analizer (erspan) client
 *
 * @author matecsaba
 */
public class clntErspan implements Runnable, ipPrt, ifcDn {

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
     * span id
     */
    public int spnid;

    /**
     * vlan id
     */
    public int vlnid;

    /**
     * sending ttl value, -1 means maps out
     */
    public int sendingTTL = 255;

    /**
     * sending tos value, -1 means maps out
     */
    public int sendingTOS = -1;

    /**
     * counter
     */
    public counter cntr = new counter();

    private ipFwd fwdCor;

    private addrIP fwdTrg;

    private ipFwdIface fwdIfc;

    private int seqTx;

    private boolean working = true;

    public String toString() {
        return "erspan to " + fwdTrg;
    }

    /**
     * get hw address
     *
     * @return hw address
     */
    public addrType getHwAddr() {
        return addrMac.getRandom();
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
     * @param pckBin packet
     */
    public void sendPack(packHolder pckBin) {
        pckBin.merge2beg();
        if (fwdIfc == null) {
            return;
        }
        seqTx++;
        packErspan tx = new packErspan();
        tx.seq = seqTx;
        tx.vlan = vlnid;
        tx.span = spnid;
        tx.createHeader(pckBin);
        cntr.tx(pckBin);
        pckBin.putDefaults();
        if (sendingTTL >= 0) {
            pckBin.IPttl = sendingTTL;
        }
        if (sendingTOS >= 0) {
            pckBin.IPtos = sendingTOS;
        }
        pckBin.IPprt = packErspan.prot;
        pckBin.IPsrc.setAddr(fwdIfc.addr);
        pckBin.IPtrg.setAddr(fwdTrg);
        fwdCor.protoPack(fwdIfc, pckBin);
    }

    /**
     * start connection
     */
    public void workStart() {
        new Thread(this).start();
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
        seqTx = 0;
    }

    /**
     * get protocol number
     *
     * @return number
     */
    public int getProtoNum() {
        return packErspan.prot;
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
     * received packet
     *
     * @param rxIfc interface
     * @param pckBin packet
     */
    public void recvPack(ipFwdIface rxIfc, packHolder pckBin) {
        packErspan pckRx = new packErspan();
        if (pckRx.parseHeader(pckBin)) {
            cntr.drop(pckBin, counter.reasons.badHdr);
            return;
        }
        if (pckRx.span != spnid) {
            cntr.drop(pckBin, counter.reasons.badID);
            return;
        }
        cntr.rx(pckBin);
        upper.recvPack(pckBin);
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
     * @param err error code
     * @param rtr address
     * @param rxIfc interface
     * @param pck packet
     */
    public void errorPack(counter.reasons err, addrIP rtr, ipFwdIface rxIfc, packHolder pck) {
    }

}
