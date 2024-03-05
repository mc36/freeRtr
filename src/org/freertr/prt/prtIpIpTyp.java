package org.freertr.prt;

import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrType;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcEther;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcUp;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipIfc4;
import org.freertr.ip.ipIfc6;
import org.freertr.pack.packHolder;
import org.freertr.util.counter;
import org.freertr.util.state;

/**
 * handle ipip ethertyped packets
 *
 * @author matecsaba
 */
public class prtIpIpTyp implements ifcDn, ifcUp {

    private counter cntr = new counter();

    private prtIpIp ip4;

    private prtIpIp ip6;

    private ifcUp upper = new ifcNull();

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
    public prtIpIpTyp(ipFwd parent) {
        ip4 = new prtIpIp(parent, 4);
        ip6 = new prtIpIp(parent, 6);
        ip4.setUpper(this);
        ip6.setUpper(this);
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
        b |= ip4.setEndpoints(ifc, trg);
        b |= ip6.setEndpoints(ifc, trg);
        return b;
    }

    /**
     * close interface
     */
    public void closeDn() {
        ip4.closeDn();
        ip6.closeDn();
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
        state.states st = ip4.getState();
        if (st != state.states.up) {
            return st;
        }
        return ip6.getState();
    }

    /**
     * set filter
     *
     * @param promisc promiscous mode
     */
    public void setFilter(boolean promisc) {
    }

    /**
     * set parent
     *
     * @param parent parent
     */
    public void setParent(ifcDn parent) {
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
        int i = ip4.getMTUsize();
        int o = ip6.getMTUsize();
        if (o < i) {
            i = o;
        }
        return i;
    }

    /**
     * get bandwidth
     *
     * @return bandwidth
     */
    public long getBandwidth() {
        long i = ip4.getBandwidth();
        long o = ip4.getBandwidth();
        if (o < i) {
            i = o;
        }
        return i;
    }

    /**
     * set sending tos value
     *
     * @param i tos value
     */
    public void setTxTOS(int i) {
        ip4.sendingTOS = i;
        ip6.sendingTOS = i;
    }

    /**
     * set sending df value
     *
     * @param i tos value
     */
    public void setTxDFN(int i) {
        ip4.sendingDFN = i;
        ip6.sendingDFN = i;
    }

    /**
     * set sending flow value
     *
     * @param i tos value
     */
    public void setTxFLW(int i) {
        ip4.sendingFLW = i;
        ip6.sendingFLW = i;
    }

    /**
     * set sending ttl value
     *
     * @param i ttl value
     */
    public void setTxTTL(int i) {
        ip4.sendingTTL = i;
        ip6.sendingTTL = i;
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
            case ipIfc4.type:
                ip4.sendPack(pck);
                break;
            case ipIfc6.type:
                ip6.sendPack(pck);
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
        int i = ifcEther.guessEtherType(pck);
        if (i < 0) {
            cntr.drop(pck, counter.reasons.badVer);
            return;
        }
        pck.msbPutW(0, i);
        i = pck.headSize();
        pck.putSkip(2);
        pck.mergeHeader(-1, i);
        upper.recvPack(pck);
    }

}
