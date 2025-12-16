package org.freertr.prt;

import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrType;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcUp;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipPrt;
import org.freertr.ipx.ipxIface;
import org.freertr.pack.packHolder;
import org.freertr.util.counter;
import org.freertr.util.state;

/**
 * ipxip encapsulation
 *
 * @author matecsaba
 */
public class prtIpxip implements ipPrt, ifcDn {

    /**
     * protocol number
     */
    public final static int proto = 111;

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
     * flow id
     */
    public int flowId;

    private ipFwdIface sendingIfc;

    private ifcUp upper = new ifcNull();

    private ipFwd lower;

    private addrIP remote;

    private counter cntr = new counter();

    /**
     * get counter
     *
     * @return counter
     */
    public counter getCounter() {
        return cntr;
    }

    /**
     * initialize context
     *
     * @param parent forwarder of encapsulated packets
     */
    public prtIpxip(ipFwd parent) {
        lower = parent;
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
        return lower.protoAdd(this, sendingIfc, remote);
    }

    /**
     * get protocol number
     *
     * @return number
     */
    public int getProtoNum() {
        return proto;
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
     * received packet
     *
     * @param rxIfc interface
     * @param pck packet
     */
    public void recvPack(ipFwdIface rxIfc, packHolder pck) {
        cntr.rx(pck);
        if (pck.IPprt != proto) {
            cntr.drop(pck, counter.reasons.badProto);
            return;
        }
        if (pck.IPsrc.compareTo(remote) != 0) {
            cntr.drop(pck, counter.reasons.badSrcAddr);
            return;
        }
        pck.msbPutW(0, ipxIface.type); // ethertype
        pck.putSkip(2);
        pck.merge2beg();
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
            case ipxIface.type:
                break;
            default:
                cntr.drop(pck, counter.reasons.badProto);
                return;
        }
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
        pck.IPprt = proto;
        pck.IPtrg.setAddr(remote);
        pck.IPsrc.setAddr(sendingIfc.addr);
        lower.protoPack(sendingIfc, null, pck);
    }

    public String toString() {
        return "ipxip to " + remote;
    }

    /**
     * get mtu size
     *
     * @return mtu size
     */
    public int getMTUsize() {
        return sendingIfc.mtu;
    }

    /**
     * get bandwidth
     *
     * @return bandwidth
     */
    public long getBandwidth() {
        return sendingIfc.bandwidth;
    }

}
