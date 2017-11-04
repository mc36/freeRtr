package rtr;

import addr.addrEmpty;
import addr.addrIP;
import addr.addrMac;
import addr.addrType;
import clnt.clntVxlan;
import ifc.ifcBridgeIfc;
import ifc.ifcDn;
import ifc.ifcNull;
import ifc.ifcUp;
import java.util.Comparator;
import pack.packHolder;
import util.counter;
import util.state;

/**
 * bgp4 evpn peer
 *
 * @author matecsaba
 */
public class rtrBgpEvpnPeer implements ifcDn, Comparator<rtrBgpEvpnPeer> {

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

    private rtrBgpEvpn parent;

    /**
     * create new instance
     *
     * @param p parent to use
     */
    public rtrBgpEvpnPeer(rtrBgpEvpn p) {
        parent = p;
    }

    public int compare(rtrBgpEvpnPeer o1, rtrBgpEvpnPeer o2) {
        return o1.bbmac.compare(o1.bbmac, o2.bbmac);
    }

    public String toString() {
        return "evpn " + peer + " " + parent.id;
    }

    public addrType getHwAddr() {
        return new addrEmpty();
    }

    public void setFilter(boolean promisc) {
    }

    public state.states getState() {
        return state.states.up;
    }

    public void closeDn() {
    }

    public void flapped() {
    }

    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
    }

    public counter getCounter() {
        return cntr;
    }

    public int getMTUsize() {
        return 1500;
    }

    public long getBandwidth() {
        return 8000000;
    }

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

}
