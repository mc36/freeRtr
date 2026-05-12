package org.freertr.ifc;

import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrMac;
import org.freertr.addr.addrType;
import org.freertr.cfg.cfgIfc;
import org.freertr.ip.ipCor;
import org.freertr.ip.ipCor4;
import org.freertr.ip.ipCor6;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipIfc4;
import org.freertr.ip.ipIfc6;
import org.freertr.pack.packHolder;
import org.freertr.prt.prtDccp;
import org.freertr.prt.prtLudp;
import org.freertr.prt.prtSctp;
import org.freertr.prt.prtTcp;
import org.freertr.prt.prtUdp;
import org.freertr.tab.tabConnect;
import org.freertr.tab.tabConnectLower;
import org.freertr.tab.tabQos;
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
     * counter of this interface
     */
    public counter cntr = new counter();

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

    private addrMac inMac = new addrMac();

    private addrMac outMac = new addrMac();

    private addrIP inIp4 = new addrIP();

    private addrIP inIp6 = new addrIP();

    private ipCor core4 = new ipCor4();

    private ipCor core6 = new ipCor6();

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

    private void updateMac(addrMac trg, addrMac src) {
        if (src.isBroadcast()) {
            return;
        }
        if (src.isMulticast()) {
            return;
        }
        if (src.isFilled(0)) {
            return;
        }
        trg.setAddr(src);
    }

    /**
     * got one inside packet
     *
     * @param pck packet to process
     */
    protected void gotInnerPacket(packHolder pck) {
        updateMac(inMac, pck.ETHsrc);
        updateMac(outMac, pck.ETHtrg);
        pck.ETHtype = pck.msbGetW(0);
        pck.getSkip(2);
        boolean res;
        addrIP adr = null;
        switch (pck.ETHtype) {
            case ipIfc4.type:
                res = core4.parseIPheader(pck, false);
                adr = inIp4;
                break;
            case ipIfc6.type:
                res = core6.parseIPheader(pck, false);
                adr = inIp6;
                break;
            default:
                res = true;
                break;
        }
        pck.getSkip(-2);
        if (res) {
            outIfc.lower.sendPack(pck);
            return;
        }
        res = !pck.IPsrc.isUnicast();
        res |= pck.IPsrc.isEmpty();
        res |= pck.IPsrc.isLinkLocal();
        res |= pck.IPsrc.compareTo(adr) == 0;
        if (res) {
            outIfc.lower.sendPack(pck);
            return;
        }
        outIfc.lower.sendPack(pck);
        adr.setAddr(pck.IPsrc);
        if (adr.isIPv4()) {
            upper.addr4changed(adr.toIPv4(), upper.mask4, null);
        } else {
            upper.addr6changed(adr.toIPv6(), upper.mask6, null);
        }
    }

    /**
     * got one outside packet
     *
     * @param pck packet to process
     */
    protected void gotOuterPacket(packHolder pck) {
        pck.ETHtype = pck.msbGetW(0);
        pck.getSkip(2);
        boolean res;
        ipFwdIface ifc;
        switch (pck.ETHtype) {
            case ipIfc4.type:
                res = core4.parseIPheader(pck, false);
                ifc = upper.fwdIf4;
                break;
            case ipIfc6.type:
                res = core6.parseIPheader(pck, false);
                ifc = upper.fwdIf6;
                break;
            default:
                res = true;
                ifc = null;
                break;
        }
        if (res) {
            pck.getSkip(-2);
            inIfc.lower.sendPack(pck);
            return;
        }
        pck.getSkip(pck.IPsiz);
        tabQos.classifyLayer4(pck);
        pck.getSkip(-pck.IPsiz);
        pck.getSkip(-2);
        tabConnect<addrIP, ? extends tabConnectLower> clnt;
        switch (pck.IPprt) {
            case prtTcp.protoNum:
                clnt = upper.vrfFor.getTcp(pck.IPsrc).clnts;
                break;
            case prtUdp.protoNum:
                clnt = upper.vrfFor.getUdp(pck.IPsrc).clnts;
                break;
            case prtLudp.protoNum:
                clnt = upper.vrfFor.getLudp(pck.IPsrc).clnts;
                break;
            case prtDccp.protoNum:
                clnt = upper.vrfFor.getDccp(pck.IPsrc).clnts;
                break;
            case prtSctp.protoNum:
                clnt = upper.vrfFor.getSctp(pck.IPsrc).clnts;
                break;
            default:
                clnt = upper.vrfFor.getFwd(pck.IPsrc).protos;
                pck.UDPsrc = pck.IPprt;
                pck.UDPtrg = pck.IPprt;
                break;
        }
        if (clnt.get(ifc, pck.IPsrc, pck.UDPtrg, pck.UDPsrc) == null) {
            inIfc.lower.sendPack(pck);
        } else {
            encap.recvPack(pck);
        }
    }

    public void sendPack(packHolder pck) {
        pck.ETHsrc.setAddr(inMac);
        pck.ETHtrg.setAddr(outMac);
        pck.ETHtype = pck.msbGetW(0);
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
        cntr.rx(pck);
        parent.gotInnerPacket(pck);
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
        cntr.rx(pck);
        parent.gotOuterPacket(pck);
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
