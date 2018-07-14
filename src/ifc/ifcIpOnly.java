package ifc;

import addr.addrEmpty;
import addr.addrType;
import pack.packHolder;
import util.counter;
import util.state;

/**
 * ip without any handler
 *
 * @author matecsaba
 */
public class ifcIpOnly implements ifcUp, ifcDn {

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
    public ifcIpOnly() {
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
        return "iponly on " + lower;
    }

    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        if (lastState != state.states.up) {
            cntr.drop(pck, counter.reasons.notUp);
            return;
        }
        int i = ifcEther.guessEtherType(pck);
        if (i < 0) {
            cntr.drop(pck, counter.reasons.badHdr);
            return;
        }
        pck.msbPutW(0, i); // ethertype                                                                                    
        pck.putSkip(2);
        pck.merge2beg();
        upper.recvPack(pck);
    }

    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        if (lastState != state.states.up) {
            cntr.drop(pck, counter.reasons.notUp);
            return;
        }
        if (ifcEther.stripEtherType(pck)) {
            cntr.drop(pck, counter.reasons.badHdr);
            return;
        }
        lower.sendPack(pck);
    }

}
