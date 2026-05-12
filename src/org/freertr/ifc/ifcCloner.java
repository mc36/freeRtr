package org.freertr.ifc;

import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrMac;
import org.freertr.addr.addrType;
import org.freertr.cfg.cfgIfc;
import org.freertr.ip.ipCor;
import org.freertr.ip.ipCor4;
import org.freertr.ip.ipCor6;
import org.freertr.ip.ipFwd;
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
     * upper handler
     */
    public final ifcUp upcap;

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

    /**
     * inside mac
     */
    protected addrMac inMac = new addrMac();

    /**
     * outside mac
     */
    protected addrMac outMac = new addrMac();

    /**
     * inside ipv4
     */
    protected addrIP inIp4 = new addrIP();

    /**
     * inside ipv6
     */
    protected addrIP inIp6 = new addrIP();

    /**
     * core ipv4
     */
    protected final ipCor core4 = new ipCor4();

    /**
     * core ipv6
     */
    protected final ipCor core6 = new ipCor6();

    /**
     * ready to use
     */
    protected boolean ready2use = false;

    /**
     * create instance
     *
     * @param upp upper to use
     * @param upc upper to use
     * @param in inside to use
     * @param out outside to use
     */
    public ifcCloner(cfgIfc upp, ifcUp upc, ifcEthTyp in, ifcEthTyp out) {
        inIfc = new ifcClonerIn(this);
        outIfc = new ifcClonerOut(this);
        upcap = upc;
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
        if (!ready2use) {
            return;
        }
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
        encap.setState(state.states.up);
    }

}

class ifcClonerIn implements ifcUp {

    public final ifcCloner parent;

    public ifcDn lower = new ifcNull();

    public counter cntr = new counter();

    public ifcClonerIn(ifcCloner lower) {
        parent = lower;
    }

    private boolean updateMac(addrMac trg, addrMac src) {
        if (src.isBroadcast()) {
            return false;
        }
        if (src.isMulticast()) {
            return false;
        }
        if (src.isFilled(0)) {
            return false;
        }
        trg.setAddr(src);
        return true;
    }

    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        parent.ready2use |= updateMac(parent.inMac, pck.ETHsrc) & updateMac(parent.outMac, pck.ETHtrg);
        pck.ETHtype = pck.msbGetW(0);
        pck.getSkip(2);
        boolean res;
        addrIP adr = null;
        switch (pck.ETHtype) {
            case ipIfc4.type:
                res = parent.core4.parseIPheader(pck, false);
                adr = parent.inIp4;
                break;
            case ipIfc6.type:
                res = parent.core6.parseIPheader(pck, false);
                adr = parent.inIp6;
                break;
            default:
                res = true;
                break;
        }
        pck.getSkip(-2);
        if (res) {
            parent.outIfc.lower.sendPack(pck);
            return;
        }
        res = !pck.IPsrc.isUnicast();
        res |= pck.IPsrc.isEmpty();
        res |= pck.IPsrc.isLinkLocal();
        res |= pck.IPsrc.compareTo(adr) == 0;
        if (res) {
            parent.outIfc.lower.sendPack(pck);
            return;
        }
        parent.outIfc.lower.sendPack(pck);
        adr.setAddr(pck.IPsrc);
        if (adr.isIPv4()) {
            parent.upper.addr4changed(adr.toIPv4(), parent.upper.mask4, null);
        } else {
            parent.upper.addr6changed(adr.toIPv6(), parent.upper.mask6, null);
        }
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
        pck.ETHtype = pck.msbGetW(0);
        pck.getSkip(2);
        boolean res;
        ipFwdIface ifc = null;
        addrIP adr = null;
        ipFwd fwd = null;
        prtTcp tcp = null;
        prtUdp udp = null;
        prtLudp ludp = null;
        prtDccp dccp = null;
        prtSctp sctp = null;
        switch (pck.ETHtype) {
            case ipIfc4.type:
                res = parent.core4.parseIPheader(pck, false);
                fwd = parent.upper.vrfFor.fwd4;
                tcp = parent.upper.vrfFor.tcp4;
                udp = parent.upper.vrfFor.udp4;
                ludp = parent.upper.vrfFor.ludp4;
                dccp = parent.upper.vrfFor.dccp4;
                sctp = parent.upper.vrfFor.sctp4;
                ifc = parent.upper.fwdIf4;
                adr = parent.inIp4;
                break;
            case ipIfc6.type:
                res = parent.core6.parseIPheader(pck, false);
                fwd = parent.upper.vrfFor.fwd6;
                tcp = parent.upper.vrfFor.tcp6;
                udp = parent.upper.vrfFor.udp6;
                ludp = parent.upper.vrfFor.ludp6;
                dccp = parent.upper.vrfFor.dccp6;
                sctp = parent.upper.vrfFor.sctp6;
                ifc = parent.upper.fwdIf6;
                adr = parent.inIp6;
                break;
            default:
                res = true;
                break;
        }
        pck.getSkip(-2);
        if (res) {
            parent.inIfc.lower.sendPack(pck);
            return;
        }
        if (pck.IPtrg.compareTo(adr) != 0) {
            parent.inIfc.lower.sendPack(pck);
            return;
        }
        pck.getSkip(pck.IPsiz + 2);
        tabQos.classifyLayer4(pck);
        pck.getSkip(-pck.IPsiz - 2);
        tabConnect<addrIP, ? extends tabConnectLower> clnt;
        switch (pck.IPprt) {
            case prtTcp.protoNum:
                clnt = tcp.clnts;
                break;
            case prtUdp.protoNum:
                clnt = udp.clnts;
                break;
            case prtLudp.protoNum:
                clnt = ludp.clnts;
                break;
            case prtDccp.protoNum:
                clnt = dccp.clnts;
                break;
            case prtSctp.protoNum:
                clnt = sctp.clnts;
                break;
            default:
                clnt = fwd.protos;
                pck.UDPtrg = pck.IPprt;
                pck.UDPsrc = pck.IPprt;
                break;
        }
        if (clnt.get(ifc, pck.IPsrc, pck.UDPtrg, pck.UDPsrc) == null) {
            parent.inIfc.lower.sendPack(pck);
        } else {
            parent.encap.recvPack(pck);
        }
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
