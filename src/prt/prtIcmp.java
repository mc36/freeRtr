package prt;

import addr.addrEmpty;
import addr.addrIP;
import addr.addrType;
import ifc.ifcDn;
import ifc.ifcNull;
import ifc.ifcUp;
import ip.ipFwd;
import ip.ipFwdIface;
import ip.ipIcmp;
import ip.ipIcmp4;
import ip.ipIcmp6;
import ip.ipPrt;
import pack.packHolder;
import util.counter;
import util.state;
import util.logger;

/**
 * icmp tunnel
 *
 * @author matecsaba
 */
public class prtIcmp implements ipPrt, ifcDn {

    /**
     * sending ttl value, -1 means maps out
     */
    public int sendingTTL = 255;

    /**
     * sending tos value, -1 means maps out
     */
    public int sendingTOS = -1;

    /**
     * tunnel key to use, 0 means disabled
     */
    public int tunnelKey = 0;

    private ipFwdIface sendingIfc;

    private ifcUp upper = new ifcNull();

    private ipFwd lower;

    private addrIP remote = new addrIP();

    private counter cntr = new counter();

    private ipIcmp icmpCr;

    private int protoNum;

    private int clntCod;

    private int servCod;

    private int icmpSiz;

    private int seqTx;

    /**
     * initialize context
     *
     * @param parent forwarder of encapsulated packets
     */
    public prtIcmp(ipFwd parent) {
        lower = parent;
        doInternals();
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
     * get protocol number
     *
     * @return number
     */
    public int getProtoNum() {
        return protoNum;
    }

    /**
     * close interface
     *
     * @param iface interface
     */
    public void closeUp(ipFwdIface iface) {
        upper.closeUp();
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
     * set state
     *
     * @param iface interface
     * @param stat state
     */
    public void setState(ipFwdIface iface, state.states stat) {
        if (iface.ifwNum != sendingIfc.ifwNum) {
            return;
        }
        upper.setState(stat);
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
     * close interface
     */
    public void closeDn() {
        lower.protoDel(this, sendingIfc, remote);
    }

    /**
     * flap interface
     */
    public void flapped() {
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
     * set target of tunnel
     *
     * @param ifc interface to source from
     * @param trg ip address of remote
     * @return false if successful, true if error happened
     */
    public boolean setEndpoints(ipFwdIface ifc, addrIP trg) {
        if (sendingIfc != null) {
            lower.protoDel(this, sendingIfc, remote);
        }
        remote = trg;
        sendingIfc = ifc;
        doInternals();
        return lower.protoAdd(this, sendingIfc, remote);
    }

    private void doInternals() {
        if (remote.isIPv4()) {
            icmpCr = new ipIcmp4();
            protoNum = ipIcmp4.protoNum;
            clntCod = ipIcmp4.icmpEchoReq;
            servCod = ipIcmp4.icmpEchoRep;
            icmpSiz = ipIcmp4.size;
        } else {
            icmpCr = new ipIcmp6();
            protoNum = ipIcmp6.protoNum;
            clntCod = ipIcmp6.icmpEchoReq;
            servCod = ipIcmp6.icmpEchoRep;
            icmpSiz = ipIcmp6.size;
        }
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

    public String toString() {
        return "icmp to " + remote;
    }

    /**
     * get mtu size
     *
     * @return mtu size
     */
    public int getMTUsize() {
        int i = sendingIfc.mtu - icmpSiz;
        return i;
    }

    /**
     * get bandwidth
     *
     * @return bandwidth
     */
    public long getBandwidth() {
        return sendingIfc.bandwidth;
    }

    /**
     * send packet
     *
     * @param pck packet
     */
    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        pck.merge2beg();
        if (tunnelKey < 0) {
            pck.msbPutW(4, -tunnelKey);
        } else {
            pck.msbPutW(4, tunnelKey);
        }
        pck.msbPutW(6, seqTx);
        seqTx++;
        pck.IPsrc.setAddr(sendingIfc.addr);
        pck.IPtrg.setAddr(remote);
        if (tunnelKey >= 0) {
            pck.ICMPtc = clntCod;
        } else {
            pck.ICMPtc = servCod;
        }
        icmpCr.createICMPheader(pck);
        pck.putDefaults();
        if (sendingTTL >= 0) {
            pck.IPttl = sendingTTL;
        }
        if (sendingTOS >= 0) {
            pck.IPtos = sendingTOS;
        }
        lower.protoPack(sendingIfc, pck);
    }

    /**
     * received packet
     *
     * @param rxIfc interface
     * @param pck packet
     */
    public void recvPack(ipFwdIface rxIfc, packHolder pck) {
        cntr.rx(pck);
        if (icmpCr.parseICMPheader(pck)) {
            cntr.drop(pck, counter.reasons.badHdr);
            return;
        }
        if ((pck.ICMPtc != servCod) && (pck.ICMPtc != clntCod)) {
            cntr.drop(pck, counter.reasons.badCod);
        }
        int key = pck.msbGetW(4);
//  int seq = pck.msbGetW(6);
        pck.getSkip(icmpSiz);
        if ((key != tunnelKey) && (key != (-tunnelKey))) {
            logger.info("got bad key from " + remote);
            cntr.drop(pck, counter.reasons.badID);
            return;
        }
        upper.recvPack(pck);
    }

}
