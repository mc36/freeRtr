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
import pack.packHolder;
import user.userTerminal;
import util.bits;
import util.counter;
import util.logger;
import util.state;

/**
 * data link switching (rfc1795) client
 *
 * @author matecsaba
 */
public class clntDlsw implements Runnable, ipPrt, ifcDn {

    /**
     * protocol number
     */
    public static final int prot = 91;

    /**
     * header size
     */
    public static final int size = 72;

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

    private boolean working = true;

    public String toString() {
        return "dlsw to " + fwdTrg;
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
     * @param pck packet
     */
    public void sendPack(packHolder pck) {
        pck.merge2beg();
        if (fwdIfc == null) {
            return;
        }
        pck.putByte(0, 0x31); // version
        pck.putByte(1, size); // header size
        pck.msbPutW(2, pck.dataSize()); // data size
        pck.putByte(14, 0x14); // type
        pck.putByte(16, 0x42); // protocol
        pck.putByte(23, 0x14); // type
        pck.putAddr(24, pck.ETHtrg);
        pck.putAddr(30, pck.ETHsrc);
        pck.putSkip(size);
        pck.merge2beg();
        cntr.tx(pck);
        pck.putDefaults();
        if (sendingTTL >= 0) {
            pck.IPttl = sendingTTL;
        }
        if (sendingTOS >= 0) {
            pck.IPtos = sendingTOS;
        }
        pck.IPprt = prot;
        pck.IPsrc.setAddr(fwdIfc.addr);
        pck.IPtrg.setAddr(fwdTrg);
        fwdCor.protoPack(fwdIfc, pck);
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
     * received packet
     *
     * @param rxIfc interface
     * @param pck packet
     */
    public void recvPack(ipFwdIface rxIfc, packHolder pck) {
        if (pck.getByte(0) != 0x31) {
            cntr.drop(pck, counter.reasons.badHdr);
            return;
        }
        if (pck.getByte(1) != size) {
            cntr.drop(pck, counter.reasons.badHdr);
            return;
        }
        int i = pck.msbGetW(2) + size;
        if (pck.dataSize() < i) {
            cntr.drop(pck, counter.reasons.badLen);
            return;
        }
        pck.setDataSize(i);
        if (pck.getByte(14) != 0x14) {
            cntr.drop(pck, counter.reasons.badTyp);
            return;
        }
        pck.getSkip(size);
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
     * @param err error code
     * @param rtr address
     * @param rxIfc interface
     * @param pck packet
     */
    public void errorPack(counter.reasons err, addrIP rtr, ipFwdIface rxIfc, packHolder pck) {
    }

}
