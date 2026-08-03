package org.freertr.serv;

import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrType;
import org.freertr.cfg.cfgIfc;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcUp;
import org.freertr.pack.packHolder;
import org.freertr.util.counter;
import org.freertr.util.state;

/**
 * l2f session
 *
 * @author matecsaba
 */
public class servL2fSess implements ifcDn, Comparable<servL2fSess> {

    /**
     * multiplex id
     */
    public int multi;

    /**
     * lower layer
     */
    public servL2fConn lower;

    /**
     * upper interface
     */
    public ifcUp upper = new ifcNull();

    /**
     * access interface
     */
    public cfgIfc ifc;

    /**
     * counter
     */
    public counter cntr = new counter();

    /**
     * create instance
     *
     * @param parent connection
     */
    public servL2fSess(servL2fConn parent) {
        lower = parent;
    }

    public String toString() {
        return lower + "/" + multi;
    }

    public int compareTo(servL2fSess o) {
        if (multi < o.multi) {
            return -1;
        }
        if (multi > o.multi) {
            return +1;
        }
        return 0;
    }

    /**
     * start connection
     */
    public void doStartup() {
        upper = new ifcNull();
        ifc = lower.lower.clnIfc.cloneStart(this);
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
        lower.sesDel(multi, true);
        upper.closeUp();
        if (ifc != null) {
            ifc.cloneStop();
        }
    }

    public void flapped() {
        closeDn();
    }

    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
    }

    public counter getCounter() {
        return cntr;
    }

    public int getMTUsize() {
        return 1400;
    }

    public long getBandwidth() {
        return 8000000;
    }

    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        pck.putDefaults();
        lower.sesData(this, pck);
    }

}
