package net.freertr.ifc;

import java.util.Comparator;
import net.freertr.addr.addrType;
import net.freertr.pack.packHolder;
import net.freertr.util.counter;
import net.freertr.util.state;

/**
 * vlan subinterface handler
 *
 * @author matecsaba
 */
public class ifcSub implements ifcDn, Comparator<ifcSub> {

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

    public int compare(ifcSub v1, ifcSub v2) {
        if (v1.vLan < v2.vLan) {
            return -1;
        }
        if (v1.vLan > v2.vLan) {
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
