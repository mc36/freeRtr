package org.freertr.rtr;

import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrMac;
import org.freertr.addr.addrType;
import org.freertr.clnt.clntVxlan;
import org.freertr.ifc.ifcBridgeIfc;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcUp;
import org.freertr.ip.ipFwd;
import org.freertr.pack.packHolder;
import org.freertr.util.counter;
import org.freertr.util.state;

/**
 * bgp4 evpn peer
 *
 * @author matecsaba
 */
public class rtrBgpEvpnPeer implements ifcDn, Comparable<rtrBgpEvpnPeer> {

    /**
     * peer address
     */
    protected addrIP peer;

    /**
     * backbone mac
     */
    protected addrMac bbmac;

    /**
     * needed
     */
    protected int needed = 0;

    /**
     * unicast label
     */
    protected int labUni;

    /**
     * multicast label
     */
    protected int labMul;

    /**
     * unicast srv6
     */
    protected addrIP srv6uni;

    /**
     * multicast srv6
     */
    protected addrIP srv6mul;

    /**
     * bridge interface
     */
    protected ifcBridgeIfc brdg;

    /**
     * vxlan interface
     */
    protected clntVxlan vxlan;

    /**
     * counter
     */
    public counter cntr = new counter();

    /**
     * upper layer
     */
    public ifcUp upper = new ifcNull();

    /**
     * upper layer
     */
    protected final rtrBgpEvpn parent;

    /**
     * create new instance
     *
     * @param p parent to use
     */
    public rtrBgpEvpnPeer(rtrBgpEvpn p) {
        parent = p;
    }

    public int compareTo(rtrBgpEvpnPeer o) {
        return bbmac.compareTo(o.bbmac);
    }

    public String toString() {
        return "evpn " + peer + " " + parent.id;
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
        parent.sendPack(this, pck);
    }

    /**
     * stop work
     */
    public void doStop() {
        if (brdg != null) {
            brdg.closeUp();
        }
        if (vxlan != null) {
            vxlan.workStop();
        }
    }

    /**
     * get local label
     *
     * @return label
     */
    public int getLabelLoc() {
        return parent.label.label;
    }

    /**
     * get remote label
     *
     * @return label
     */
    public int getLabelRem() {
        return labUni;
    }

    /**
     * get remote address
     *
     * @return address
     */
    public addrIP getRemote() {
        return peer.copyBytes();
    }

    /**
     * get srv remote address
     *
     * @return address
     */
    public addrIP getSrvRem() {
        if (srv6uni == null) {
            return null;
        }
        return srv6uni.copyBytes();
    }

    /**
     * get forwarder
     *
     * @return forwarder
     */
    public ipFwd getForwarder() {
        if (srv6uni == null) {
            return parent.parent.fwdCore;
        } else {
            return parent.parent.vrfCore.fwd6;
        }
    }

    /**
     * received packet
     *
     * @param pck packet
     */
    public void recvPack(packHolder pck) {
        if (brdg == null) {
            cntr.drop(pck, counter.reasons.badAddr);
            return;
        }
        brdg.recvPack(pck);
    }

}
