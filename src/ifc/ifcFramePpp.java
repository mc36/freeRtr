package ifc;

import addr.addrEmpty;
import addr.addrType;
import pack.packHolder;
import util.counter;
import util.state;

/**
 * ppp over frame relay encapsulation handler
 *
 * @author matecsaba
 */
public class ifcFramePpp implements ifcUp, ifcDn {

    /**
     * type of framerelay header
     */
    public final static int frmType = 0x3cf;

    /**
     * type of ppp header
     */
    public final static int pppType = 0xff03;

    /**
     * size of headers
     */
    public final static int size = 2;

    /**
     * last known state
     */
    public state.states lastState = state.states.down;

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
        lower.flapped();
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

    public void setState(state.states stat) {
        stat = state.toForceable(stat);
        lastState = stat;
        upper.setState(stat);
    }

    public int getMTUsize() {
        return lower.getMTUsize();
    }

    public long getBandwidth() {
        return lower.getBandwidth();
    }

    public String toString() {
        return "frppp on " + lower;
    }

    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        if (pck.dataSize() < size) {
            cntr.drop(pck, counter.reasons.tooSmall);
            return;
        }
        int i = pck.msbGetW(0);
        pck.getSkip(size);
        if (i != frmType) {
            cntr.drop(pck, counter.reasons.badHdr);
            return;
        }
        pck.msbPutW(0, pppType);
        pck.putSkip(size);
        pck.merge2beg();
        upper.recvPack(pck);
    }

    public void sendPack(packHolder pck) {
        pck.getSkip(size);
        pck.msbPutW(0, frmType);
        pck.putSkip(size);
        pck.merge2beg();
        lower.sendPack(pck);
    }

}
