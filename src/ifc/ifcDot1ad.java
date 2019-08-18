package ifc;

import addr.addrType;
import java.util.Comparator;
import pack.packHolder;
import tab.tabGen;
import util.counter;
import util.debugger;
import util.logger;
import util.state;

/**
 * ieee 802.1ad protocol
 *
 * @author matecsaba
 */
public class ifcDot1ad extends ifcVlan {

    /**
     * ethertype of these packets
     */
    public final static int type = 0x88a8; // old: 0x9100;

    /**
     * size of header
     */
    public final static int size = 4;

    private tabGen<ifcDot1adEntry> vLans;

    /**
     * parse header
     *
     * @param pck packet to parse
     * @return false on success, true on error
     */
    public static boolean parseHeader(packHolder pck) {
        if (pck.msbGetW(0) != type) {
            return true;
        }
        int i = pck.msbGetW(2); // vlan tag
        pck.ETHvlan = i & 0xfff; // id
        pck.ETHcos = (i >>> 13) & 7; // cos
        return false;
    }

    /**
     * create header
     *
     * @param pck packet to update
     */
    public static void createHeader(packHolder pck) {
        pck.msbPutW(0, type); // ether type
        pck.msbPutW(2, (pck.ETHvlan & 0xfff) | ((pck.ETHcos & 7) << 13)); // vlan tag
        pck.putSkip(size);
        pck.merge2beg();
    }

    /**
     * convert to string
     *
     * @return string
     */
    public String toString() {
        return "dot1ad on " + lower;
    }

    /**
     * register ethertype
     *
     * @param ethtyp handler
     */
    public void reg2ethTyp(ifcEthTyp ethtyp) {
        ethtyp.addET(type, "dot1ad", this);
        ethtyp.updateET(type, this);
    }

    /**
     * unregister ethertype
     *
     * @param ethtyp handler
     */
    public void unreg2ethTyp(ifcEthTyp ethtyp) {
        vLans.clear();
        ethtyp.delET(type);
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
            ifcDot1adEntry ntry = vLans.get(i);
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
            ifcDot1adEntry ntry = vLans.get(i);
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
        promisc = false;
        for (int i = 0; i < vLans.size(); i++) {
            promisc |= vLans.get(i).promiscous;
        }
        if (promiscous == promisc) {
            return;
        }
        promiscous = promisc;
        lower.setFilter(promisc);
        if (debugger.ifcDot1adTraf) {
            logger.debug("set filter to " + promisc);
        }
    }

    /**
     * create new multiplexer
     */
    public ifcDot1ad() {
        if (debugger.ifcDot1adTraf) {
            logger.debug("started");
        }
        vLans = new tabGen<ifcDot1adEntry>();
    }

    /**
     * get size of mtu
     *
     * @return mtu size
     */
    protected int doGetMtu() {
        return lower.getMTUsize() - size;
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
        pck.getSkip(size);
        if (debugger.ifcDot1adTraf) {
            logger.debug("rx vlan=" + pck.ETHvlan);
        }
        ifcDot1adEntry ntry = new ifcDot1adEntry(null, null);
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
    public ifcDot1adEntry addVlan(int vl, ifcUp ifc) {
        if (debugger.ifcDot1adTraf) {
            logger.debug("add vlan=" + vl);
        }
        ifcDot1adEntry ntry = new ifcDot1adEntry(this, ifc);
        ntry.vLan = vl;
        ifcDot1adEntry old = vLans.add(ntry);
        if (old != null) {
            return old;
        }
        ifc.setParent(ntry);
        setFilter(false);
        return ntry;
    }

    /**
     * update vlan
     *
     * @param vl vlan id
     * @param ifc interface
     * @return handler
     */
    public ifcDot1adEntry updateVlan(int vl, ifcUp ifc) {
        if (debugger.ifcDot1adTraf) {
            logger.debug("update vlan=" + vl);
        }
        ifcDot1adEntry ntry = new ifcDot1adEntry(this, ifc);
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
        if (debugger.ifcDot1adTraf) {
            logger.debug("del vlan=" + vl);
        }
        ifcDot1adEntry ntry = new ifcDot1adEntry(null, null);
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

class ifcDot1adEntry implements ifcDn, Comparator<ifcDot1adEntry> {

    public int vLan;

    public ifcUp upper = new ifcNull();

    public boolean promiscous;

    private ifcDot1ad lower;

    public counter cntr = new counter();

    public counter getCounter() {
        return cntr;
    }

    public addrType getHwAddr() {
        return lower.vlnHwAddr();
    }

    public String toString() {
        return "vlan" + vLan + " on " + lower;
    }

    public state.states getState() {
        return lower.vlnState();
    }

    public ifcDot1adEntry(ifcDot1ad parent, ifcUp server) {
        lower = parent;
        upper = server;
    }

    public void closeDn() {
        lower.delVlan(vLan);
    }

    public void flapped() {
    }

    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
    }

    public void setFilter(boolean promisc) {
        promiscous = promisc;
        lower.setFilter(promisc);
    }

    public int compare(ifcDot1adEntry v1, ifcDot1adEntry v2) {
        if (v1.vLan < v2.vLan) {
            return -1;
        }
        if (v1.vLan > v2.vLan) {
            return +1;
        }
        return 0;
    }

    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        pck.ETHvlan = vLan;
        ifcDot1ad.createHeader(pck);
        if (debugger.ifcDot1adTraf) {
            logger.debug("tx vlan=" + vLan);
        }
        lower.vlnTxPack(pck);
    }

    public int getMTUsize() {
        return lower.doGetMtu();
    }

    public long getBandwidth() {
        return lower.vlnBandwidth();
    }

}
