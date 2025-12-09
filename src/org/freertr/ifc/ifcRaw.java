package org.freertr.ifc;

import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrType;
import org.freertr.pack.packHolder;
import org.freertr.util.counter;
import org.freertr.util.state;

/**
 * raw encapsulation handler
 *
 * @author matecsaba
 */
public class ifcRaw implements ifcUp, ifcDn {

    /**
     * counter of this interface
     */
    public counter cntr = new counter();

    /**
     * server that handler received packets
     */
    public ifcUp upper = new ifcNull();

    /**
     * server that sends our packets
     */
    public ifcDn lower = new ifcNull();

    public counter getCounter() {
        return cntr;
    }

    public void setParent(ifcDn parent) {
        lower = parent;
    }

    public void closeUp() {
        setState(state.states.close);
        upper.closeUp();
    }

    public void closeDn() {
        setState(state.states.close);
        lower.closeDn();
    }

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
        setState(lower.getState());
    }

    public state.states getState() {
        return lower.getState();
    }

    public void setFilter(boolean promisc) {
    }

    public addrType getHwAddr() {
        return new addrEmpty();
    }

    /**
     * create new instance
     */
    public ifcRaw() {
    }

    public void setState(state.states stat) {
        stat = state.toForceable(stat);
        upper.setState(stat);
    }

    public int getMTUsize() {
        return lower.getMTUsize();
    }

    public long getBandwidth() {
        return lower.getBandwidth();
    }

    public String toString() {
        return "raw on " + lower;
    }

    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        upper.recvPack(pck);
    }

    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        lower.sendPack(pck);
    }

}
