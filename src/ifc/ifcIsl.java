package ifc;

import addr.addrMac;
import addr.addrType;
import cry.cryHashCrc32;
import java.util.Comparator;
import pack.packHolder;
import tab.tabGen;
import util.bits;
import util.counter;
import util.debugger;
import util.logger;
import util.state;

/**
 * inter switch link protocol
 *
 * @author matecsaba
 */
public class ifcIsl extends ifcVlan {

    /**
     * ethertype of these packets
     */
    public final static int type = 0x00000c;

    /**
     * size of header
     */
    public final static int size = 18;

    /**
     * target address
     */
    protected addrMac targetAddr;

    private tabGen<ifcIslEntry> vLans;

    public String toString() {
        return "isl on " + lower;
    }

    /**
     * register ethertype
     *
     * @param ethtyp handler
     */
    public void reg2ethTyp(ifcEthTyp ethtyp) {
        ethtyp.addSNAP(type, "isl", this);
        ethtyp.updateSNAP(type, this);
    }

    /**
     * unregister ethertype
     *
     * @param ethtyp handler
     */
    public void unreg2ethTyp(ifcEthTyp ethtyp) {
        vLans.clear();
        ethtyp.delSNAP(type);
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
            ifcIslEntry ntry = vLans.get(i);
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
            ifcIslEntry ntry = vLans.get(i);
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
        if (debugger.ifcIslTraf) {
            logger.debug("set filter to " + promisc);
        }
    }

    /**
     * create new multiplexer
     */
    public ifcIsl() {
        if (debugger.ifcIslTraf) {
            logger.debug("started");
        }
        targetAddr = new addrMac();
        targetAddr.fromString("0100:0c00:0000");
        vLans = new tabGen<ifcIslEntry>();
    }

    /**
     * get size of mtu
     *
     * @return mtu size
     */
    protected int doGetMtu() {
        return lower.getMTUsize() - size - 4;
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
        pck.ETHcos = 0;
        pck.ETHvlan = pck.msbGetW(0) >>> 1; // vlan tag
        pck.getAddr(pck.ETHtrg, 6); // c dst
        pck.getAddr(pck.ETHsrc, 12); // c src
        pck.getSkip(size);
        pck.setDataSize(pck.dataSize() - 4);
        if (debugger.ifcIslTraf) {
            logger.debug("rx vlan=" + pck.ETHvlan);
        }
        ifcIslEntry ntry = new ifcIslEntry(null, null);
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
    public ifcIslEntry addVlan(int vl, ifcUp ifc) {
        if (debugger.ifcIslTraf) {
            logger.debug("add vlan=" + vl);
        }
        ifcIslEntry ntry = new ifcIslEntry(this, ifc);
        ntry.vLan = vl;
        ifcIslEntry old = vLans.add(ntry);
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
    public ifcIslEntry updateVlan(int vl, ifcUp ifc) {
        if (debugger.ifcIslTraf) {
            logger.debug("update vlan=" + vl);
        }
        ifcIslEntry ntry = new ifcIslEntry(this, ifc);
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
        if (debugger.ifcIslTraf) {
            logger.debug("del vlan=" + vl);
        }
        ifcIslEntry ntry = new ifcIslEntry(null, null);
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

class ifcIslEntry implements ifcDn, Comparator<ifcIslEntry> {

    public int vLan;

    public ifcUp upper = new ifcNull();

    public boolean promiscous;

    private ifcIsl lower;

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

    public ifcIslEntry(ifcIsl parent, ifcUp server) {
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

    public int compare(ifcIslEntry v1, ifcIslEntry v2) {
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
        pck.putAddr(0, pck.ETHtrg);
        pck.putAddr(addrMac.size, pck.ETHsrc);
        pck.putSkip(addrMac.sizeX2);
        pck.merge2beg();
        pck.ETHvlan = vLan;
        cryHashCrc32 sum = new cryHashCrc32();
        sum.init();
        pck.hashData(sum, 0, pck.dataSize());
        pck.lsbPutD(0, bits.msbGetD(sum.finish(), 0)); // crc
        pck.putSkip(4);
        pck.merge2end();
        pck.msbPutW(0, vLan << 1); // vlan tag
        pck.msbPutD(2, 0); // index
        pck.putSkip(ifcIsl.size - addrMac.sizeX2);
        pck.merge2beg();
        pck.ETHtrg.setAddr(lower.targetAddr);
        if (debugger.ifcIslTraf) {
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
