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

    public counter getCounter() {
        return cntr;
    }

    public void closeUp() {
    }

    public void setState(state.states stat) {
        cntr.stateChange(stat);
    }

    public void setParent(ifcDn parent) {
        if (reg2par) {
            parent.setUpper(this);
        }
    }

    public void setUpper(ifcUp server) {
        if (reg2upp) {
            server.setParent(this);
        }
    }

    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        cntr.drop(pck, counter.reasons.noIface);
    }

    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        cntr.drop(pck, counter.reasons.noIface);
    }

    public String toString() {
        return "dropper";
    }

    public addrType getHwAddr() {
        return new addrEmpty();
    }

    public void setFilter(boolean promisc) {
    }

    public state.states getState() {
        return state.states.up;
    }

    public void closeDn() {
    }

    public void flapped() {
    }

    public int getMTUsize() {
        return 65535;
    }

    public long getBandwidth() {
        return 1000000000;
    }

}
