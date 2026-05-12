package org.freertr.ifc;

import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrType;
import org.freertr.cfg.cfgIfc;
import org.freertr.pack.packHolder;
import org.freertr.util.counter;
import org.freertr.util.state;

/**
 * host cloner interface
 *
 * @author matecsaba
 */
public class ifcCloner implements ifcDn {

    /**
     * upper handler
     */
    public final cfgIfc upper;

    /**
     * encap handler
     */
    public ifcUp encap  = new ifcNull();

    /**
     * inside handler
     */
    public final ifcEthTyp inside;

    /**
     * outside handler
     */
    public final ifcEthTyp outside;

    /**
     * counter of this interface
     */
    public counter cntr = new counter();

    /**
     * create instance
     *
     * @param upp upper to use
     * @param in inside to use
     * @param out outside to use
     */
    public ifcCloner(cfgIfc upp, ifcEthTyp in, ifcEthTyp out) {
        upper = upp;
        inside = in;
        outside = out;
    }

    public String toString() {
        return "cloner on " + outside;
    }

    /**
     * set filter
     *
     * @param prom pormiscous
     */
    public void setPromiscous(boolean prom) {

    }

    /**
     * get outside handler
     *
     * @return iface to use
     */
    public ifcUp getSideO() {
        return new ifcNull();
    }

    /**
     * get inside handler
     *
     * @return iface to use
     */
    public ifcUp getSideI() {
        return new ifcNull();
    }

    public void sendPack(packHolder pck) {
        
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

    public counter getCounter() {
        return cntr;
    }

    public int getMTUsize() {
        return outside.getMTUsize();
    }

    public long getBandwidth() {
        return outside.getBandwidth();
    }

    public void setUpper(ifcUp server) {
        encap = server;
        encap.setParent(this);
    }

}
