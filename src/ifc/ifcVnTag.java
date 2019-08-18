package ifc;

import addr.addrType;
import pack.packHolder;
import util.counter;
import util.state;

/**
 * vn tag protocol
 *
 * @author matecsaba
 */
public class ifcVnTag implements ifcUp, ifcDn {

    /**
     * ethertype of these packets
     */
    public final static int type = 0x8926;

    /**
     * size of header
     */
    public final static int size = 6;

    /**
     * source port
     */
    public int source;

    /**
     * target port
     */
    public int target;

    private ifcUp upper = new ifcNull();

    private ifcDn lower = new ifcNull();

    private counter cntr = new counter();

    /**
     * create new encapsulater
     *
     * @param upp upper layer
     */
    public ifcVnTag(ifcUp upp) {
        upper = upp;
        upper.setParent(this);
    }

    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        if (pck.msbGetW(0) != type) {
            return;
        }
        pck.getSkip(size);
        upper.recvPack(pck);
    }

    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        pck.msbPutW(0, type);
        pck.msbPutD(2, source | (target << 16));
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
