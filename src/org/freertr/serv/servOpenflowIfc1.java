package org.freertr.serv;

import org.freertr.addr.addrMac;
import org.freertr.addr.addrType;
import org.freertr.cfg.cfgIfc;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcEther;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcUp;
import org.freertr.pack.packHolder;
import org.freertr.pack.packOpenflow;
import org.freertr.util.counter;
import org.freertr.util.state;

/**
 * one openflow interface
 *
 * @author matecsaba
 */
public class servOpenflowIfc1 implements ifcDn, Comparable<servOpenflowIfc1> {

    /**
     * create instance
     */
    public servOpenflowIfc1() {
    }

    /**
     * interface id
     */
    protected int id;

    /**
     * group id
     */
    protected int grp;

    /**
     * cookie
     */
    protected int cook;

    /**
     * backing interface
     */
    protected cfgIfc ifc;

    /**
     * openflow served
     */
    protected servOpenflow lower;

    /**
     * ethertype layer
     */
    protected ifcUp upper = new ifcNull();

    /**
     * counter
     */
    protected counter cntr = new counter();

    /**
     * last state
     */
    protected state.states lastState = state.states.up;

    public int compareTo(servOpenflowIfc1 o) {
        if (id < o.id) {
            return -1;
        }
        if (id > o.id) {
            return +1;
        }
        if (id != servOpenflow.tabGrp) {
            return 0;
        }
        if (grp < o.grp) {
            return -1;
        }
        if (grp > o.grp) {
            return +1;
        }
        return 0;
    }

    public String toString() {
        return "openflow port " + id;
    }

    /**
     * get sent state
     *
     * @param cfg interface id
     */
    protected void sendState(int cfg) {
        if (id == servOpenflow.tabGrp) {
            return;
        }
        packHolder pckB = new packHolder(true, true);
        packOpenflow pckO = new packOpenflow();
        pckO.pipe = lower.conn;
        pckO.createPortMod(pckB, id, ifc.ethtyp.getHwAddr(), cfg, 1);
        lower.sendPack(pckB, pckO);
    }

    public addrType getHwAddr() {
        return addrMac.getRandom();
    }

    public void setFilter(boolean promisc) {
    }

    public state.states getState() {
        return lastState;
    }

    public void closeDn() {
    }

    public void flapped() {
    }

    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
    }

    public counter getCounter() {
        return cntr;
    }

    public int getMTUsize() {
        return 1500;
    }

    public long getBandwidth() {
        return 8000000;
    }

    public void sendPack(packHolder pck) {
        if (lower.conn == null) {
            return;
        }
        if (id == servOpenflow.tabGrp) {
            return;
        }
        ifcEther.createETHheader(pck, false);
        packOpenflow pckO = new packOpenflow();
        pckO.pipe = lower.conn;
        pckO.createPckOut(pck, id);
        lower.sendPack(pck, pckO);
    }

}
