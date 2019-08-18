package ifc;

import addr.addrMac;
import addr.addrType;
import pack.packHolder;
import ip.ipCor4;
import ip.ipCor6;
import ip.ipIfc4;
import ip.ipIfc6;
import util.counter;
import util.state;

/**
 * ethernet encapsulater
 *
 * @author matecsaba
 */
public class ifcEther implements ifcUp, ifcDn {

    private ifcEtherWorker se;

    private ifcEtherWorker st;

    private boolean notEthernet;

    private ifcUp upper = new ifcNull();

    private ifcDn lower = new ifcNull();

    private counter cntr = new counter();

    /**
     * parse ethernet header
     *
     * @param pck packet to parse
     * @param typ set true to process ethertype
     * @return false on success, true on error
     */
    public static boolean parseETHheader(packHolder pck, boolean typ) {
        if (pck.dataSize() < addrMac.sizeX2) {
            return true;
        }
        pck.getAddr(pck.ETHtrg, 0);
        pck.getAddr(pck.ETHsrc, addrMac.size);
        pck.getSkip(addrMac.sizeX2);
        if (!typ) {
            return false;
        }
        if (pck.dataSize() < 2) {
            return true;
        }
        pck.ETHtype = pck.msbGetW(0);
        pck.getSkip(2);
        return false;
    }

    /**
     * create ethernet header
     *
     * @param pck packet to parse
     * @param typ set true to process ethertype
     */
    public static void createETHheader(packHolder pck, boolean typ) {
        pck.putAddr(0, pck.ETHtrg);
        pck.putAddr(addrMac.size, pck.ETHsrc);
        pck.putSkip(addrMac.sizeX2);
        if (typ) {
            pck.msbPutW(0, pck.ETHtype);
            pck.putSkip(2);
        }
        pck.merge2beg();
    }

    /**
     * update ethernet header
     *
     * @param pck packet to update
     */
    public static void updateETHheader(packHolder pck) {
        int size = addrMac.sizeX2;
        if (pck.ETHtype >= 0) {
            size += 2;
        }
        pck.unMergeBytes(size);
        pck.putSkip(-size);
        pck.putAddr(0, pck.ETHtrg);
        pck.putAddr(addrMac.size, pck.ETHsrc);
        pck.putSkip(addrMac.sizeX2);
        if (pck.ETHtype >= 0) {
            pck.msbPutW(0, pck.ETHtype);
            pck.putSkip(2);
        }
        pck.putSkip(size);
        pck.mergeHeader(-1, pck.headSize() - size);
    }

    /**
     * guess ethertype of ip packet
     *
     * @param pck packet to inspect
     * @return ethertype, -1 if not valid
     */
    public static int guessEtherType(packHolder pck) {
        switch (pck.getByte(0) >>> 4) {
            case ipCor4.protocolVersion:
                return ipIfc4.type;
            case ipCor6.protocolVersion:
                return ipIfc6.type;
            default:
                return -1;
        }
    }

    /**
     * strip ethertype of ip packet
     *
     * @param pck packet to inspect
     * @return false if ip, true if other packet
     */
    public static boolean stripEtherType(packHolder pck) {
        int i = pck.msbGetW(0);
        pck.getSkip(2);
        switch (i) {
            case ipIfc4.type:
                return false;
            case ipIfc6.type:
                return false;
            default:
                return true;
        }
    }

    /**
     * create new encapsulater
     *
     * @param notEther not ethernet encapsulation
     */
    public ifcEther(boolean notEther) {
        se = new ifcEtherWorker();
        st = new ifcEtherWorker();
        se.other = st;
        st.other = se;
        se.notEther = notEther;
        st.notEther = notEther;
        se.encDec = false;
        st.encDec = true;
    }

    /**
     * get encapsulated side
     *
     * @return interface handler
     */
    public ifcUp getSideEth() {
        return se;
    }

    /**
     * get ethertype side
     *
     * @return interface handler
     */
    public ifcUp getSideTyp() {
        return st;
    }

    /**
     * set filter
     *
     * @param prom pormiscous
     */
    public void setPromiscous(boolean prom) {
        se.lower.setFilter(prom);
        st.lower.setFilter(prom);
    }

    /**
     * create new encapsulater
     *
     * @param notEther not ethernet encapsulation
     * @param upp upper layer
     */
    public ifcEther(boolean notEther, ifcUp upp) {
        notEthernet = notEther;
        upper = upp;
        upper.setParent(this);
    }

    /**
     * received packet
     *
     * @param pck packet
     */
    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        parseETHheader(pck, notEthernet);
        upper.recvPack(pck);
    }

    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        createETHheader(pck, notEthernet);
        lower.sendPack(pck);
    }

    /**
     * set parent
     *
     * @param parent parent
     */
    public void setParent(ifcDn parent) {
        lower = parent;
    }

    public void setUpper(ifcUp server) {
        upper = server;
    }

    /**
     * set state
     *
     * @param stat state
     */
    public void setState(state.states stat) {
        upper.setState(stat);
    }

    /**
     * close interface
     */
    public void closeUp() {
        upper.closeUp();
    }

    public counter getCounter() {
        return cntr;
    }

    public addrType getHwAddr() {
        return lower.getHwAddr();
    }

    public void setFilter(boolean promisc) {
        lower.setFilter(promisc);
    }

    public state.states getState() {
        return lower.getState();
    }

    public void closeDn() {
        lower.closeDn();
    }

    public void flapped() {
        lower.flapped();
    }

    public int getMTUsize() {
        return lower.getMTUsize();
    }

    public long getBandwidth() {
        return lower.getBandwidth();
    }

}

class ifcEtherWorker implements ifcUp {

    public boolean encDec; // true=decapsulated, false=encapsulated

    public boolean notEther;

    public ifcEtherWorker other;

    public counter cntr = new counter();

    public ifcDn lower = new ifcNull();

    public void setParent(ifcDn parent) {
        lower = parent;
    }

    public void setState(state.states stat) {
    }

    public void closeUp() {
    }

    public counter getCounter() {
        return cntr;
    }

    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        if (encDec) {
            if (notEther) {
                pck.ETHsrc.setAddr(addrMac.getBroadcast());
                pck.ETHtrg.setAddr(addrMac.getBroadcast());
            }
            ifcEther.createETHheader(pck, false);
        } else {
            ifcEther.parseETHheader(pck, false);
        }
        other.lower.sendPack(pck);
    }

}
