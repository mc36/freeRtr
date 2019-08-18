package ifc;

import addr.addrArcnet;
import addr.addrType;
import pack.packHolder;
import util.counter;
import util.state;

/**
 * arcnet encapsulation handler
 *
 * @author matecsaba
 */
public class ifcArcnet implements ifcUp, ifcDn {

    /**
     * size of header
     */
    public final static int headSize = 3;

    /**
     * size of payload
     */
    public final static int paySize = 508;

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
    }

    public state.states getState() {
        return state.states.up;
    }

    public void setFilter(boolean promisc) {
    }

    public addrType getHwAddr() {
        return new addrArcnet();
    }

    public void setState(state.states stat) {
    }

    public int getMTUsize() {
        return lower.getMTUsize() - headSize;
    }

    public long getBandwidth() {
        return lower.getBandwidth();
    }

    public String toString() {
        return "arcnet on " + lower;
    }

    /**
     * create new instance
     */
    public ifcArcnet() {
    }

    public void recvPack(packHolder pck) {
        upper.recvPack(pck);
    }

    public void sendPack(packHolder pck) {
        for (;;) {
            int i = pck.dataSize();
            if (i < 253) {
                break;
            }
            if (i > 257) {
                break;
            }
            pck.putByte(0, 0);
            pck.merge2end();
        }
        lower.sendPack(pck);
    }

}
