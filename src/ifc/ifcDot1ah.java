package ifc;

import addr.addrMac;
import addr.addrType;
import java.util.Comparator;
import pack.packHolder;
import tab.tabGen;
import util.bits;
import util.counter;
import util.debugger;
import util.logger;
import util.state;

/**
 * ieee 802.1ah protocol
 *
 * @author matecsaba
 */
public class ifcDot1ah extends ifcVlan {

    /**
     * ethertype of these packets
     */
    public final static int type = 0x88e7;

    /**
     * size of header
     */
    public final static int size = 18;

    private tabGen<ifcDot1ahEntry> vLans;

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
        int i = pck.msbGetD(2); // i tag
        pck.ETHvlan = i & 0xffffff; // id
        pck.ETHcos = (i >>> 29) & 7; // cos
        pck.getAddr(pck.ETHtrg, 6); // c-dst
        pck.getAddr(pck.ETHsrc, 12); // c-src
        return false;
    }

    /**
     * create header
     *
     * @param pck packet to update
     */
    public static void createHeader(packHolder pck) {
        pck.msbPutW(0, type); // ether type
        pck.msbPutD(2, (pck.ETHvlan & 0xffffff) | ((pck.ETHcos & 7) << 29)); // i tag
        pck.putAddr(6, pck.ETHtrg); // c dst
        pck.putAddr(12, pck.ETHsrc); // c src
        pck.putSkip(size);
        pck.merge2beg();
    }

    /**
     * create destination b-mac address for flooded traffic
     *
     * @param isid isid to use
     * @return dst bmac
     */
    public static addrMac dstBmac4flood(int isid) {
        byte[] buf = new byte[6];
        bits.msbPutD(buf, 2, isid);
        buf[0] = 0x01;
        buf[1] = 0x1e;
        buf[2] = (byte) 0x83;
        addrMac a = new addrMac();
        a.fromBuf(buf, 0);
        return a;
    }

    public String toString() {
        return "dot1ah on " + lower;
    }

    /**
     * register ethertype
     *
     * @param ethtyp handler
     */
    public void reg2ethTyp(ifcEthTyp ethtyp) {
        ethtyp.addET(type, "dot1ah", this);
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
            ifcDot1ahEntry ntry = vLans.get(i);
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
            ifcDot1ahEntry ntry = vLans.get(i);
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
        if (debugger.ifcDot1ahTraf) {
            logger.debug("set filter to " + promisc);
        }
    }

    /**
     * create new multiplexer
     */
    public ifcDot1ah() {
        if (debugger.ifcDot1ahTraf) {
            logger.debug("started");
        }
        vLans = new tabGen<ifcDot1ahEntry>();
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
        if (debugger.ifcDot1ahTraf) {
            logger.debug("rx vlan=" + pck.ETHvlan);
        }
        ifcDot1ahEntry ntry = new ifcDot1ahEntry(null, null);
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
    public ifcDot1ahEntry addVlan(int vl, ifcUp ifc) {
        if (debugger.ifcDot1ahTraf) {
            logger.debug("add vlan=" + vl);
        }
        ifcDot1ahEntry ntry = new ifcDot1ahEntry(this, ifc);
        ntry.vLan = vl;
        ifcDot1ahEntry old = vLans.add(ntry);
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
    public ifcDot1ahEntry updateVlan(int vl, ifcUp ifc) {
        if (debugger.ifcDot1ahTraf) {
            logger.debug("update vlan=" + vl);
        }
        ifcDot1ahEntry ntry = new ifcDot1ahEntry(this, ifc);
        ntry.vLan = vl;
        ntry = vLans.find(ntry);
        if (ntry == null) {
            return null;
        }
        ntry.upper = ifc;
        ifc.setParent(ntry);
        return ntry;
    }

    public ifcUp delVlan(int vl) {
        if (debugger.ifcDot1ahTraf) {
            logger.debug("del vlan=" + vl);
        }
        ifcDot1ahEntry ntry = new ifcDot1ahEntry(null, null);
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

class ifcDot1ahEntry implements ifcDn, Comparator<ifcDot1ahEntry> {

    public int vLan;

    public ifcUp upper = new ifcNull();

    public boolean promiscous;

    private ifcDot1ah lower;

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

    public ifcDot1ahEntry(ifcDot1ah parent, ifcUp server) {
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

    public int compare(ifcDot1ahEntry v1, ifcDot1ahEntry v2) {
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
        ifcDot1ah.createHeader(pck);
        if (debugger.ifcDot1ahTraf) {
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
