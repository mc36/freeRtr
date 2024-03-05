package org.freertr.ifc;

import org.freertr.addr.addrInfiniband;
import org.freertr.addr.addrType;
import org.freertr.pack.packHolder;
import org.freertr.util.bits;
import org.freertr.util.counter;
import org.freertr.util.state;

/**
 * infiniband encapsulation handler
 *
 * @author matecsaba
 */
public class ifcInfiniband implements ifcUp, ifcDn {

    /**
     * size of header
     */
    public final static int headSize = 20;

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

    private int seq = bits.randomD();

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
        return new addrInfiniband();
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
        return "infiniband on " + lower;
    }

    /**
     * create new instance
     */
    public ifcInfiniband() {
    }

    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        if (pck.dataSize() < headSize) {
            cntr.drop(pck, counter.reasons.tooSmall);
            return;
        }
        pck.getSkip(headSize);
        upper.recvPack(pck);
    }

    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        pck.putByte(0, 0); // lane, version
        pck.putByte(1, 2); // service, nexthdr
        pck.msbPutW(2, 0xffff); // destination
        pck.msbPutW(4, (pck.dataSize() + headSize) >>> 2); // length
        pck.msbPutW(6, 0xffff); // source
        pck.putByte(8, 100); // opcode
        pck.putByte(9, 0); // solicit, migreq, pad, version
        pck.msbPutW(10, 0xffff); // partition
        pck.msbPutD(12, 1); // destination queue
        pck.msbPutD(16, seq & 0xffffff); // sequence
        seq++;
        pck.putSkip(headSize);
        pck.merge2beg();
        lower.sendPack(pck);
    }

}
