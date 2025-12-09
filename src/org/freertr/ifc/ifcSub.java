package org.freertr.ifc;

import org.freertr.addr.addrType;
import org.freertr.pack.packHolder;
import org.freertr.util.counter;
import org.freertr.util.state;

/**
 * vlan subinterface handler
 *
 * @author matecsaba
 */
public class ifcSub implements ifcDn, Comparable<ifcSub> {

    /**
     * vlan id
     */
    protected int vLan;

    /**
     * upper layer
     */
    protected ifcUp upper = new ifcNull();

    /**
     * promisc mode
     */
    protected boolean promiscous;

    /**
     * lower layer
     */
    protected ifcVlan lower;

    /**
     * counter
     */
    public counter cntr = new counter();

    public counter getCounter() {
        return cntr;
    }

    public addrType getHwAddr() {
        return lower.vlnHwAddr(vLan);
    }

    public String toString() {
        return "vlan" + vLan + " on " + lower;
    }

    public state.states getState() {
        return lower.vlnState();
    }

    /**
     * create subinterface
     *
     * @param parent lower layer
     * @param server upper layer
     */
    public ifcSub(ifcVlan parent, ifcUp server) {
        lower = parent;
        upper = server;
    }

    public void closeDn() {
        lower.delVlan(vLan);
    }

    public void flapped() {
    }

    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
    }

    public void setFilter(boolean promisc) {
        promiscous = promisc;
        lower.setFilter(promisc);
    }

    public int compareTo(ifcSub o) {
        if (vLan < o.vLan) {
            return -1;
        }
        if (vLan > o.vLan) {
            return +1;
        }
        return 0;
    }

    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        pck.ETHvlan = vLan;
        lower.createHeader(pck);
        lower.vlnTxPack(pck);
    }

    public int getMTUsize() {
        return lower.remainingMtu();
    }

    public long getBandwidth() {
        return lower.vlnBandwidth();
    }

}
