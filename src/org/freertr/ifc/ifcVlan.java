package org.freertr.ifc;

import java.util.List;
import org.freertr.addr.addrMac;
import org.freertr.addr.addrType;
import org.freertr.pack.packHolder;
import org.freertr.tab.tabGen;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.counter;
import org.freertr.util.state;

/**
 * vlan trunk handler
 *
 * @author matecsaba
 */
public abstract class ifcVlan implements ifcUp {

    /**
     * create instance
     */
    public ifcVlan() {
    }

    /**
     * subinterfaces use different macs
     */
    public boolean subMacs;

    /**
     * state of lower
     */
    protected state.states lastState = state.states.up;

    /**
     * lower layer
     */
    protected ifcDn lower = new ifcNull();

    /**
     * promiscous mode set
     */
    protected boolean promiscous = false;

    /**
     * packet counter
     */
    protected counter cntr = new counter();

    /**
     * vlan subinterfaces
     */
    protected tabGen<ifcSub> vLans = new tabGen<ifcSub>();

    /**
     * get help text
     *
     * @param l storage
     */
    public static void vlnGetHelp(userHelp l) {
        l.add(null, false, 2, new int[]{-1}, "subif-macs", "assign different macs to subinterfaces");
    }

    /**
     * get configuration
     *
     * @param l storage
     * @param beg beginning
     */
    public void vlnGetConfig(List<String> l, String beg) {
        cmds.cfgLine(l, !subMacs, beg, "vlan subif-macs", "");
    }

    /**
     * do configuration
     *
     * @param cmd command
     */
    public void vlnDoConfig(cmds cmd) {
        String a = cmd.word();
        if (a.equals("subif-macs")) {
            subMacs = true;
            return;
        }
        cmd.badCmd();
    }

    /**
     * do configuration
     *
     * @param cmd command
     */
    public void vlnUnConfig(cmds cmd) {
        String a = cmd.word();
        if (a.equals("subif-macs")) {
            subMacs = false;
            return;
        }
        cmd.badCmd();
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
     * set parent
     *
     * @param parent parent
     */
    public void setParent(ifcDn parent) {
        lower = parent;
    }

    /**
     * register to ethertype
     *
     * @param ethtyp where to register
     */
    public abstract void reg2ethTyp(ifcEthTyp ethtyp);

    /**
     * unregister from ethertype
     *
     * @param ethtyp where from register
     */
    public abstract void unreg2ethTyp(ifcEthTyp ethtyp);

    /**
     * get size of mtu
     *
     * @return mtu size
     */
    public abstract int remainingMtu();

    /**
     * parse header
     *
     * @param pck packet to parse
     * @return false on success, true on error
     */
    public abstract boolean parseHeader(packHolder pck);

    /**
     * create header
     *
     * @param pck packet to update
     */
    public abstract void createHeader(packHolder pck);

    /**
     * get hardware address
     *
     * @param vlan vlan id
     * @return hw address
     */
    protected addrType vlnHwAddr(int vlan) {
        addrType ifa = lower.getHwAddr();
        if (!subMacs) {
            return ifa;
        }
        addrMac ifm;
        try {
            ifm = (addrMac) ifa;
        } catch (Exception e) {
            return ifa;
        }
        addrMac adr = new addrMac();
        bits.msbPutD(adr.getBytes(), 2, vlan);
        adr.setAdd(adr, ifm);
        return adr;
    }

    /**
     * get state of interface
     *
     * @return state of line protocol
     */
    protected state.states vlnState() {
        return lastState;
    }

    /**
     * get bandwidth
     *
     * @return bandwidth
     */
    protected long vlnBandwidth() {
        return lower.getBandwidth();
    }

    /**
     * send one packet to the network
     *
     * @param pck packet to send
     */
    protected void vlnTxPack(packHolder pck) {
        cntr.tx(pck);
        if (lastState != state.states.up) {
            cntr.drop(pck, counter.reasons.notUp);
            return;
        }
        lower.sendPack(pck);
    }

    /**
     * set state
     *
     * @param stat state
     */
    public void setState(state.states stat) {
        if (lastState == stat) {
            return;
        }
        lastState = stat;
        for (int i = 0; i < vLans.size(); i++) {
            ifcSub ntry = vLans.get(i);
            ntry.upper.setState(stat);
        }
        cntr.stateChange(stat);
    }

    /**
     * close this interface
     */
    public void closeUp() {
        lastState = state.states.close;
        for (int i = 0; i < vLans.size(); i++) {
            ifcSub ntry = vLans.get(i);
            try {
                ntry.upper.closeUp();
            } catch (Exception e) {
            }
        }
    }

    /**
     * set filter criteria
     *
     * @param promisc need all packet (promiscous mode)
     */
    public void setFilter(boolean promisc) {
        for (int i = 0; i < vLans.size(); i++) {
            promisc |= vLans.get(i).promiscous;
        }
        if (promiscous == promisc) {
            return;
        }
        promiscous = promisc;
        lower.setFilter(promisc);
    }

    /**
     * this interface got a packet for processing
     *
     * @param pck packet needs to parsed
     */
    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        if (lastState != state.states.up) {
            cntr.drop(pck, counter.reasons.notUp);
            return;
        }
        if (parseHeader(pck)) {
            cntr.drop(pck, counter.reasons.badEthTyp);
            return;
        }
        ifcSub ntry = new ifcSub(null, null);
        ntry.vLan = pck.ETHvlan;
        ntry = vLans.find(ntry);
        if (ntry == null) {
            cntr.drop(pck, counter.reasons.badVlan);
            return;
        }
        ntry.cntr.rx(pck);
        ntry.upper.recvPack(pck);
    }

    /**
     * add vlan
     *
     * @param vl vlan id
     * @param ifc interface
     * @return handler
     */
    public ifcSub addVlan(int vl, ifcUp ifc) {
        ifcSub ntry = new ifcSub(this, ifc);
        ntry.vLan = vl;
        ifcSub old = vLans.add(ntry);
        if (old != null) {
            return old;
        }
        ifc.setParent(ntry);
        setFilter(false);
        setState(lower.getState());
        return ntry;
    }

    /**
     * update vlan
     *
     * @param vl vlan id
     * @param ifc interface
     * @return handler
     */
    public ifcSub updateVlan(int vl, ifcUp ifc) {
        ifcSub ntry = new ifcSub(this, ifc);
        ntry.vLan = vl;
        ntry = vLans.find(ntry);
        if (ntry == null) {
            return null;
        }
        ntry.upper = ifc;
        ifc.setParent(ntry);
        return ntry;
    }

    /**
     * delete vlan
     *
     * @param vl vlan id
     * @return interface
     */
    public ifcUp delVlan(int vl) {
        ifcSub ntry = new ifcSub(null, null);
        ntry.vLan = vl;
        ntry = vLans.del(ntry);
        if (ntry == null) {
            return null;
        }
        try {
            ntry.upper.closeUp();
        } catch (Exception e) {
        }
        setFilter(false);
        return ntry.upper;
    }

}
