package org.freertr.prt;

import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrType;
import org.freertr.clnt.clntSrEth;
import org.freertr.ifc.ifcBridge;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcMacSec;
import org.freertr.ifc.ifcNshFwd;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcSgt;
import org.freertr.ifc.ifcUp;
import org.freertr.ip.ipCor4;
import org.freertr.ip.ipCor6;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipIfc4;
import org.freertr.ip.ipIfc6;
import org.freertr.ip.ipMpls;
import org.freertr.ip.ipPrt;
import org.freertr.pack.packHolder;
import org.freertr.rtr.rtrNshIface;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.state;

/**
 * handle ipip (rfc1853) packets
 *
 * @author matecsaba
 */
public class prtIpIp implements ifcDn {

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

    private counter cntr = new counter();

    private ifcUp upper = new ifcNull();

    private final prtIpIpHnd mpls;

    private final prtIpIpHnd ip4;

    private final prtIpIpHnd ip6;

    private final prtIpIpHnd mcs;

    private final prtIpIpHnd sgt;

    private final prtIpIpHnd nsh;

    private final prtIpIpHnd eth;

    /**
     * initialize context
     *
     * @param parent forwarder of encapsulated packets
     */
    public prtIpIp(ipFwd parent) {
        mpls = new prtIpIpHnd(parent, this, prtMplsIp.prot, ipMpls.typeU);
        ip4 = new prtIpIpHnd(parent, this, ipCor4.protocolNumber, ipIfc4.type);
        ip6 = new prtIpIpHnd(parent, this, ipCor6.protocolNumber, ipIfc6.type);
        mcs = new prtIpIpHnd(parent, this, prtSwipe.prot, ifcMacSec.ethtyp);
        sgt = new prtIpIpHnd(parent, this, prtSkip.prot, ifcSgt.type);
        nsh = new prtIpIpHnd(parent, this, rtrNshIface.protoNum, ifcNshFwd.type);
        eth = new prtIpIpHnd(parent, this, clntSrEth.prot, ifcBridge.serialType);
    }

    /**
     * get counter
     *
     * @return counter
     */
    public counter getCounter() {
        return cntr;
    }

    public String toString() {
        return "" + mpls;
    }

    /**
     * set target of tunnel
     *
     * @param ifc interface to source from
     * @param trg ip address of remote
     * @return false if successful, true if error happened
     */
    public boolean setEndpoints(ipFwdIface ifc, addrIP trg) {
        boolean b = false;
        b |= mpls.setEndpoints(ifc, trg);
        b |= ip4.setEndpoints(ifc, trg);
        b |= ip6.setEndpoints(ifc, trg);
        b |= mcs.setEndpoints(ifc, trg);
        b |= sgt.setEndpoints(ifc, trg);
        b |= nsh.setEndpoints(ifc, trg);
        b |= eth.setEndpoints(ifc, trg);
        return b;
    }

    /**
     * close interface
     */
    public void closeDn() {
        mpls.closeDn();
        ip4.closeDn();
        ip6.closeDn();
        mcs.closeDn();
        sgt.closeDn();
        nsh.closeDn();
        eth.closeDn();
    }

    /**
     * flap interface
     */
    public void flapped() {
    }

    /**
     * close interface
     */
    public void closeUp() {
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
     * get state
     *
     * @return state
     */
    public state.states getState() {
        return state.states.up;
    }

    /**
     * set filter
     *
     * @param promisc promiscous mode
     */
    public void setFilter(boolean promisc) {
    }

    /**
     * set state
     *
     * @param stat state
     */
    public void setState(state.states stat) {
        cntr.stateChange(stat);
        upper.setState(stat);
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
     * get mtu size
     *
     * @return mtu size
     */
    public int getMTUsize() {
        return mpls.sendingIfc.mtu;
    }

    /**
     * get bandwidth
     *
     * @return bandwidth
     */
    public long getBandwidth() {
        return mpls.sendingIfc.bandwidth;
    }

    /**
     * set sending tos value
     *
     * @param i tos value
     */
    public void setTxTOS(int i) {
        sendingTOS = i;
    }

    /**
     * set sending df value
     *
     * @param i tos value
     */
    public void setTxDFN(int i) {
        sendingDFN = i;
    }

    /**
     * set sending flow value
     *
     * @param i tos value
     */
    public void setTxFLW(int i) {
        sendingFLW = i;
    }

    /**
     * set sending ttl value
     *
     * @param i ttl value
     */
    public void setTxTTL(int i) {
        sendingTTL = i;
    }

    /**
     * send packet
     *
     * @param pck packet
     */
    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        int i = pck.msbGetW(0);
        pck.getSkip(2);
        switch (i) {
            case ipMpls.typeU:
            case ipMpls.typeM:
            case ipMpls.typeB:
                mpls.sendPack(pck);
                break;
            case ipIfc4.type:
                ip4.sendPack(pck);
                break;
            case ipIfc6.type:
                ip6.sendPack(pck);
                break;
            case ifcMacSec.ethtyp:
                mcs.sendPack(pck);
                break;
            case ifcSgt.type:
                sgt.sendPack(pck);
                break;
            case ifcNshFwd.type:
                nsh.sendPack(pck);
                break;
            case ifcBridge.serialType:
                eth.sendPack(pck);
                break;
            default:
                cntr.drop(pck, counter.reasons.badProto);
                return;
        }
    }

    /**
     * received packet
     *
     * @param pck packet
     */
    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        upper.recvPack(pck);
    }

    /**
     * get remote address
     *
     * @return peer address, null if no session
     */
    public addrIP getAddrRem() {
        return mpls.remote;
    }

    /**
     * get local address
     *
     * @return peer address, null if no session
     */
    public addrIP getAddrLoc() {
        return mpls.sendingIfc.addr;
    }

    /**
     * get local address
     *
     * @return peer address, null if no session
     */
    public ipFwd getFwd() {
        return mpls.lower;
    }

}

class prtIpIpHnd implements ipPrt {

    public final int protoNum;

    public final int etherTyp;

    public final prtIpIp upper;

    public final ipFwd lower;

    public ipFwdIface sendingIfc;

    public addrIP remote = new addrIP();

    public counter cntr = new counter();

    public prtIpIpHnd(ipFwd fwder, prtIpIp prnt, int proto, int ethtyp) {
        lower = fwder;
        upper = prnt;
        protoNum = proto;
        etherTyp = ethtyp;
    }

    public String toString() {
        return "ipip to " + remote;
    }

    public counter getCounter() {
        return cntr;
    }

    public boolean setEndpoints(ipFwdIface ifc, addrIP trg) {
        if (sendingIfc != null) {
            lower.protoDel(this, sendingIfc, remote);
        }
        remote = trg;
        sendingIfc = ifc;
        return lower.protoAdd(this, sendingIfc, remote);
    }

    public int getProtoNum() {
        return protoNum;
    }

    public void closeDn() {
        lower.protoDel(this, sendingIfc, remote);
    }

    public void closeUp(ipFwdIface iface) {
        upper.closeUp();
    }

    public void setState(ipFwdIface iface, state.states stat) {
        if (iface.ifwNum != sendingIfc.ifwNum) {
            return;
        }
        upper.setState(stat);
    }

    public void recvPack(ipFwdIface rxIfc, packHolder pck) {
        cntr.rx(pck);
        if (debugger.prtIpIpTraf) {
            logger.debug("rx " + pck.IPsrc + " -> " + pck.IPtrg + " pr=" + pck.IPprt);
        }
        if (pck.IPprt != protoNum) {
            cntr.drop(pck, counter.reasons.badProto);
            return;
        }
        if (pck.IPsrc.compareTo(remote) != 0) {
            cntr.drop(pck, counter.reasons.badSrcAddr);
            return;
        }
        pck.msbPutW(0, etherTyp);
        pck.putSkip(2);
        pck.merge2beg();
        upper.recvPack(pck);
    }

    public boolean alertPack(ipFwdIface rxIfc, packHolder pck) {
        return true;
    }

    public void errorPack(counter.reasons err, addrIP rtr, ipFwdIface rxIfc, packHolder pck) {
    }

    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        if (debugger.prtIpIpTraf) {
            logger.debug("tx " + pck.IPsrc + " -> " + pck.IPtrg + " pr=" + pck.IPprt);
        }
        pck.putDefaults();
        if (upper.sendingTTL >= 0) {
            pck.IPttl = upper.sendingTTL;
        }
        if (upper.sendingTOS >= 0) {
            pck.IPtos = upper.sendingTOS;
        }
        if (upper.sendingDFN >= 0) {
            pck.IPdf = upper.sendingDFN == 1;
        }
        if (upper.sendingFLW >= 0) {
            pck.IPid = upper.sendingFLW;
        }
        pck.IPprt = protoNum;
        pck.IPtrg.setAddr(remote);
        pck.IPsrc.setAddr(sendingIfc.addr);
        lower.protoPack(sendingIfc, null, pck);
    }

}
