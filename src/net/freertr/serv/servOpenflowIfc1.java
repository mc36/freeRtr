package net.freertr.serv;

import java.util.Comparator;
import net.freertr.addr.addrMac;
import net.freertr.addr.addrType;
import net.freertr.cfg.cfgIfc;
import net.freertr.ifc.ifcDn;
import net.freertr.ifc.ifcEther;
import net.freertr.ifc.ifcNull;
import net.freertr.ifc.ifcUp;
import net.freertr.pack.packHolder;
import net.freertr.pack.packOpenflow;
import net.freertr.util.counter;
import net.freertr.util.state;

/**
 * one openflow interface
 *
 * @author matecsaba
 */
public class servOpenflowIfc1 implements ifcDn, Comparator<servOpenflowIfc1> {

    public int id;

    public int grp;

    public int cook;

    public cfgIfc ifc;

    public servOpenflow lower;

    public ifcUp upper = new ifcNull();

    public counter cntr = new counter();

    public state.states lastState = state.states.up;

    public int compare(servOpenflowIfc1 o1, servOpenflowIfc1 o2) {
        if (o1.id < o2.id) {
            return -1;
        }
        if (o1.id > o2.id) {
            return +1;
        }
        if (o1.id != servOpenflow.tabGrp) {
            return 0;
        }
        if (o1.grp < o2.grp) {
            return -1;
        }
        if (o1.grp > o2.grp) {
            return +1;
        }
        return 0;
    }

    public String toString() {
        return "openflow port " + id;
    }

    public void sendState(int cfg) {
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
