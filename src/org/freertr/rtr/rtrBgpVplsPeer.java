package org.freertr.rtr;

import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrType;
import org.freertr.clnt.clntMplsPwe;
import org.freertr.ifc.ifcBridgeIfc;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcUp;
import org.freertr.ip.ipMpls;
import org.freertr.pack.packHolder;
import org.freertr.tab.tabRouteUtil;
import org.freertr.util.counter;
import org.freertr.util.state;

/**
 * bgp4 vpls peer
 *
 * @author matecsaba
 */
public class rtrBgpVplsPeer implements ifcDn, Comparable<rtrBgpVplsPeer> {

    /**
     * peer address
     */
    protected addrIP peer;

    /**
     * needed
     */
    protected boolean needed = false;

    /**
     * pwe client
     */
    protected clntMplsPwe clnt;

    /**
     * bridge interface
     */
    protected ifcBridgeIfc brdg;

    /**
     * ve id
     */
    protected int veId;

    /**
     * ve label
     */
    protected int veLab;

    /**
     * counter
     */
    public counter cntr = new counter();

    private ifcUp upper = new ifcNull();

    private rtrBgp parentB;

    private rtrBgpVpls parentV;

    /**
     * create new instance
     *
     * @param pb parent to use
     * @param pv parent to use
     */
    protected rtrBgpVplsPeer(rtrBgp pb, rtrBgpVpls pv) {
        parentV = pv;
        parentB = pb;
    }

    public int compareTo(rtrBgpVplsPeer o) {
        return peer.compareTo(o.peer);
    }

    public String toString() {
        return "vpls " + peer + " " + tabRouteUtil.rd2string(parentV.id);
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
     * close interface
     */
    public void closeDn() {
    }

    /**
     * flap interface
     */
    public void flapped() {
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
        pck.merge2beg();
        ipMpls.beginMPLSfields(pck, false);
        pck.MPLSlabel = veLab + parentV.veId - 1;
        ipMpls.createMPLSheader(pck);
        parentB.fwdCore.mplsTxPack(peer, pck, false);
    }

}
