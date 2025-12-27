package org.freertr.ifc;

import org.freertr.addr.addrType;
import org.freertr.pack.packHolder;
import org.freertr.util.counter;
import org.freertr.util.state;

/**
 * iou protocol
 *
 * @author matecsaba
 */
public class ifcIou implements ifcUp, ifcDn {

    /**
     * size of header
     */
    public final static int size = 8;

    private ifcUp upper = new ifcNull();

    private ifcDn lower = new ifcNull();

    private counter cntr = new counter();

    private int nodeLoc;

    private int nodeRem;

    /**
     * create new encapsulater
     *
     * @param upp upper layer
     */
    public ifcIou(ifcUp upp) {
        upper = upp;
        upper.setParent(this);
    }

    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        if (pck.msbGetW(0) != nodeLoc) { // target node
            return;
        }
        if (pck.msbGetW(2) != nodeRem) { // source node
            return;
        }
        pck.NSHsi = pck.getByte(4); // target port
        pck.NSHsp = pck.getByte(5); // source port
        if (pck.getByte(6) != 1) { // type
            return;
        }
        if (pck.getByte(6) != 0) { // channel
            return;
        }
        pck.getSkip(size);
        upper.recvPack(pck);
    }

    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        pck.msbPutW(0, nodeRem); // target node
        pck.msbPutW(2, nodeLoc); // source node
        pck.putByte(4, pck.NSHsp); // target port
        pck.putByte(5, pck.NSHsi); // source port
        pck.putByte(6, 1); // type
        pck.putByte(7, 0); // channel
        pck.putSkip(size);
        pck.merge2beg();
        lower.sendPack(pck);
    }

    public void setParent(ifcDn parent) {
        lower = parent;
    }

    /**
     * set upper layer
     *
     * @param server upper layer
     */
    public void setUpper(ifcUp server) {
        upper = server;
    }

    public void setState(state.states stat) {
        upper.setState(stat);
    }

    public void closeUp() {
        upper.closeUp();
    }

    public counter getCounter() {
        return cntr;
    }

    public addrType getHwAddr() {
        return lower.getHwAddr();
    }

    public void setFilter(boolean promisc) {
        lower.setFilter(promisc);
    }

    public state.states getState() {
        return lower.getState();
    }

    public void closeDn() {
        lower.closeDn();
    }

    public void flapped() {
        lower.flapped();
    }

    public int getMTUsize() {
        return lower.getMTUsize();
    }

    public long getBandwidth() {
        return lower.getBandwidth();
    }

}
