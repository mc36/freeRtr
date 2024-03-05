package org.freertr.ifc;

import org.freertr.addr.addrType;
import org.freertr.pack.packHolder;
import org.freertr.util.counter;
import org.freertr.util.state;

/**
 * trill (rfc6325) base header
 *
 * @author matecsaba
 */
public class ifcTrillBas implements ifcUp, ifcDn {

    /**
     * ethertype
     */
    public final static int type = 0x22f3;

    /**
     * size
     */
    public final static int size = 8;

    private ifcUp upper = new ifcNull();

    private ifcDn lower = new ifcNull();

    private counter cntr = new counter();

    /**
     * create new encapsulater
     *
     * @param upp upper layer
     */
    public ifcTrillBas(ifcUp upp) {
        upper = upp;
        upper.setParent(this);
    }

    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        if (pck.msbGetW(0) != type) {
            return;
        }
        if ((pck.getByte(2) & 0xc0) != 0) {
            return;
        }
        pck.NSHttl = pck.getByte(3) & 0x1f;
        pck.NSHsi = pck.msbGetW(4);
        pck.NSHsp = pck.msbGetW(6);
        pck.getSkip(size);
        upper.recvPack(pck);
    }

    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        pck.merge2beg();
        pck.msbPutW(0, type);
        pck.msbPutW(2, pck.NSHttl & 0x1f);
        pck.msbPutW(4, pck.NSHsi);
        pck.msbPutW(6, pck.NSHsp);
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
