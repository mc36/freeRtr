package ifc;

import addr.addrEmpty;
import addr.addrType;
import pack.packHolder;
import util.counter;
import util.state;

/**
 * raw encapsulation handler
 *
 * @author matecsaba
 */
public class ifcRaw implements ifcUp, ifcDn {

    /**
     * last known state
     */
    public state.states lastState = state.states.up;

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
        setState(lastState);
    }

    public state.states getState() {
        return lastState;
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
        if (lastState != state.states.up) {
            cntr.drop(pck, counter.reasons.notUp);
            return;
        }
        upper.recvPack(pck);
    }

    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        if (lastState != state.states.up) {
            cntr.drop(pck, counter.reasons.notUp);
            return;
        }
        lower.sendPack(pck);
    }

}
