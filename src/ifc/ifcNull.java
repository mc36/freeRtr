package ifc;

import addr.addrEmpty;
import addr.addrType;
import pack.packHolder;
import util.counter;
import util.state;

/**
 * drops all packets
 *
 * @author matecsaba
 */
public class ifcNull implements ifcUp, ifcDn {

    private counter cntr = new counter();

    private final boolean reg2par;
    private final boolean reg2upp;

    /**
     * create instance
     */
    public ifcNull() {
        reg2par = true;
        reg2upp = true;
    }

    /**
     * create instance
     *
     * @param r2p register to parent
     * @param r2u register to upper
     */
    public ifcNull(boolean r2p, boolean r2u) {
        reg2par = r2p;
        reg2upp = r2u;
    }

    /**
     * get counter
     *
     * @return counter
     */
    public counter getCounter() {
        return cntr;
    }

    /**
     * close interface
     */
    public void closeUp() {
    }

    /**
     * set state
     *
     * @param stat state
     */
    public void setState(state.states stat) {
        cntr.stateChange(stat);
    }

    /**
     * set parent
     *
     * @param parent parent
     */
    public void setParent(ifcDn parent) {
        if (reg2par) {
            parent.setUpper(this);
        }
    }

    /**
     * set upper layer
     *
     * @param server upper layer
     */
    public void setUpper(ifcUp server) {
        if (reg2upp) {
            server.setParent(this);
        }
    }

    /**
     * received packet
     *
     * @param pck packet
     */
    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        cntr.drop(pck, counter.reasons.noIface);
    }

    /**
     * send packet
     *
     * @param pck packet
     */
    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        cntr.drop(pck, counter.reasons.noIface);
    }

    public String toString() {
        return "dropper";
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
     * set filter
     *
     * @param promisc promiscous mode
     */
    public void setFilter(boolean promisc) {
    }

    /**
     * get state
     *
     * @return state
     */
    public state.states getState() {
        return state.states.up;
    }

    /**
     * close interface
     */
    public void closeDn() {
    }

    /**
     * flap interface
     */
    public void flapped() {
    }

    /**
     * get mtu size
     *
     * @return mtu size
     */
    public int getMTUsize() {
        return 65535;
    }

    /**
     * get bandwidth
     *
     * @return bandwidth
     */
    public long getBandwidth() {
        return 1000000000;
    }

}
