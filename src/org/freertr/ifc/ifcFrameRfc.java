package org.freertr.ifc;

import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrType;
import org.freertr.ip.ipCor4;
import org.freertr.ip.ipCor6;
import org.freertr.ip.ipIfc4;
import org.freertr.ip.ipIfc6;
import org.freertr.pack.packHolder;
import org.freertr.util.counter;
import org.freertr.util.state;

/**
 * ip over frame relay (rfc2427) encapsulation handler
 *
 * @author matecsaba
 */
public class ifcFrameRfc implements ifcUp, ifcDn {

    /**
     * create instance
     */
    public ifcFrameRfc() {
    }

    /**
     * type of framerelay header
     */
    public final static int frmType = 0x3;

    /**
     * size of headers
     */
    public final static int size = 2;

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

    /**
     * get counter
     *
     * @return counter
     */
    public counter getCounter() {
        return cntr;
    }

    /**
     * set parent
     *
     * @param parent parent
     */
    public void setParent(ifcDn parent) {
        lower = parent;
    }

    /**
     * close interface
     */
    public void closeUp() {
        setState(state.states.close);
        upper.closeUp();
    }

    /**
     * close interface
     */
    public void closeDn() {
        setState(state.states.close);
        lower.closeDn();
    }

    /**
     * flap interface
     */
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
        setState(lower.getState());
    }

    /**
     * get state
     *
     * @return state
     */
    public state.states getState() {
        return lower.getState();
    }

    /**
     * set filter
     *
     * @param promisc promiscous mode
     */
    public void setFilter(boolean promisc) {
    }

    /**
     * get hw address
     *
     * @return hw address
     */
    public addrType getHwAddr() {
        return new addrEmpty();
    }

    /**
     * set state
     *
     * @param stat state
     */
    public void setState(state.states stat) {
        stat = state.toForceable(stat);
        upper.setState(stat);
    }

    /**
     * get mtu size
     *
     * @return mtu size
     */
    public int getMTUsize() {
        return lower.getMTUsize();
    }

    /**
     * get bandwidth
     *
     * @return bandwidth
     */
    public long getBandwidth() {
        return lower.getBandwidth();
    }

    public String toString() {
        return "frrfc on " + lower;
    }

    /**
     * received packet
     *
     * @param pck packet
     */
    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        if (pck.dataSize() < size) {
            cntr.drop(pck, counter.reasons.tooSmall);
            return;
        }
        int i = pck.getByte(0);
        if (i != frmType) {
            cntr.drop(pck, counter.reasons.badHdr);
            return;
        }
        i = pck.getByte(1);
        pck.getSkip(size);
        switch (i) {
            case ipCor4.protocolNLPID:
                i = ipIfc4.type;
                break;
            case ipCor6.protocolNLPID:
                i = ipIfc6.type;
                break;
            default:
                cntr.drop(pck, counter.reasons.badTyp);
                return;
        }
        pck.msbPutW(0, i);
        pck.putSkip(size);
        pck.merge2beg();
        upper.recvPack(pck);
    }

    /**
     * send packet
     *
     * @param pck packet
     */
    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        int i = pck.msbGetW(0);
        switch (i) {
            case ipIfc4.type:
                i = ipCor4.protocolNLPID;
                break;
            case ipIfc6.type:
                i = ipCor6.protocolNLPID;
                break;
            default:
                cntr.drop(pck, counter.reasons.badTyp);
                return;
        }
        pck.getSkip(size);
        pck.putByte(0, frmType);
        pck.putByte(1, i);
        pck.putSkip(size);
        pck.merge2beg();
        lower.sendPack(pck);
    }

}
