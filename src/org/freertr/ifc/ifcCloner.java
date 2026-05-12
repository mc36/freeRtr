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
     * inside handler
     */
    public final ifcEthTyp inSide;

    /**
     * outside handler
     */
    public final ifcEthTyp outSide;

    /**
     * encap handler
     */
    protected ifcUp encap = new ifcNull();

    /**
     * inside handler
     */
    protected final ifcClonerIn inIfc;

    /**
     * outside handler
     */
    protected final ifcClonerOut outIfc;

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
        inIfc = new ifcClonerIn(this);
        outIfc = new ifcClonerOut(this);
        upper = upp;
        inSide = in;
        outSide = out;
    }

    public String toString() {
        return "cloner on " + outSide;
    }

    /**
     * set filter
     *
     * @param prom pormiscous
     */
    public void setPromiscous(boolean prom) {
        inSide.setFilter(prom);
        outSide.setFilter(prom);
    }

    /**
     * get outside handler
     *
     * @return iface to use
     */
    public ifcUp getSideO() {
        return outIfc;
    }

    /**
     * get inside handler
     *
     * @return iface to use
     */
    public ifcUp getSideI() {
        return inIfc;
    }

    public void sendPack(packHolder pck) {
        outIfc.lower.sendPack(pck);
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
        return outSide.getMTUsize();
    }

    public long getBandwidth() {
        return outSide.getBandwidth();
    }

    public void setUpper(ifcUp server) {
        encap = server;
        encap.setParent(this);
    }

}

class ifcClonerIn implements ifcUp {

    public final ifcCloner parent;

    public ifcDn lower = new ifcNull();

    public counter cntr = new counter();

    public ifcClonerIn(ifcCloner lower) {
        parent = lower;
    }

    public void recvPack(packHolder pck) {
        parent.outIfc.lower.sendPack(pck);
    }

    public void setParent(ifcDn prn) {
        lower = prn;
    }

    public void setState(state.states stat) {
    }

    public void closeUp() {
    }

    public counter getCounter() {
        return cntr;
    }

}

class ifcClonerOut implements ifcUp {

    public final ifcCloner parent;

    public ifcDn lower = new ifcNull();

    public counter cntr = new counter();

    public ifcClonerOut(ifcCloner lower) {
        parent = lower;
    }

    public void recvPack(packHolder pck) {
        parent.inIfc.lower.sendPack(pck);
    }

    public void setParent(ifcDn prn) {
        lower = prn;
    }

    public void setState(state.states stat) {
    }

    public void closeUp() {
    }

    public counter getCounter() {
        return cntr;
    }

}
