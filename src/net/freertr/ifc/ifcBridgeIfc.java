package net.freertr.ifc;

import java.util.Comparator;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrMac;
import net.freertr.ip.ipCor;
import net.freertr.ip.ipIfc4;
import net.freertr.ip.ipIfc6;
import net.freertr.ip.ipMhostHndl;
import net.freertr.pack.packHolder;
import net.freertr.pack.packStp;
import net.freertr.prt.prtTcp;
import net.freertr.tab.tabAceslstN;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabListing;
import net.freertr.util.bits;
import net.freertr.util.counter;
import net.freertr.util.state;

/**
 * bridge interface handler
 *
 * @author matecsaba
 */
public class ifcBridgeIfc implements ifcUp, ipMhostHndl, Comparator<ifcBridgeIfc> {

    /**
     * bridging interface number
     */
    public int ifcNum;

    /**
     * last state of this interface
     */
    public state.states stated = state.states.up;

    /**
     * bridge handler
     */
    public ifcBridge lowerBr;

    /**
     * interface handler, null means bvi
     */
    public ifcDn lowerIf = new ifcNull();

    /**
     * interface is physical
     */
    public boolean physical;

    /**
     * interface is not ethernet
     */
    public boolean notEther;

    /**
     * interface needs ethertype fixup
     */
    public boolean needType;

    /**
     * blocking state
     */
    public boolean blocked;

    /**
     * time of last stp event
     */
    public long stpTime;

    /**
     * mac rewriter
     */
    public addrMac macRewrite;

    /**
     * ipv4 tcp mss in rewrite
     */
    public int tcp4mssIn;

    /**
     * ipv4 tcp mss out rewrite
     */
    public int tcp4mssOut;

    /**
     * ipv6 tcp mss in rewrite
     */
    public int tcp6mssIn;

    /**
     * ipv6 tcp mss out rewrite
     */
    public int tcp6mssOut;

    /**
     * port security
     */
    public tabGen<addrMac> portSec;

    /**
     * static addresses
     */
    public tabGen<addrMac> statAddr;

    /**
     * disable peer communication
     */
    public boolean privatePort;

    /**
     * enable peer communication
     */
    public boolean publicPort;

    /**
     * stp ingress filter
     */
    public boolean fltrStpIn;

    /**
     * stp egress filter
     */
    public boolean fltrStpOut;

    /**
     * stp root filter
     */
    public boolean fltrStpRoot;

    /**
     * ipv4 ingress acl
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> filter4in;

    /**
     * ipv4 egress acl
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> filter4out;

    /**
     * ipv6 ingress acl
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> filter6in;

    /**
     * ipv6 egress acl
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> filter6out;

    /**
     * joined groups
     */
    public tabGen<ifcBridgeGrp> groups;

    /**
     * ipv4 core
     */
    public ipCor ipCore4;

    /**
     * ipv6 core
     */
    public ipCor ipCore6;

    private counter cntr = new counter();

    /**
     * stp counters
     */
    protected counter stpCntr = new counter();

    public counter getCounter() {
        return cntr;
    }

    public void setState(state.states stat) {
        stated = state.toUsable(stat);
        cntr.stateChange(stated);
        if (stated == state.states.up) {
            lowerBr.addMacs(this, statAddr);
            return;
        }
        lowerBr.delMacs(this);
    }

    /**
     * creates new interface
     *
     * @param parent interface handler
     * @param phy physical interface
     * @param addrFix need address fixing
     * @param typeFix need type fixing
     */
    public ifcBridgeIfc(ifcBridge parent, boolean phy, boolean addrFix, boolean typeFix) {
        lowerBr = parent;
        physical = phy;
        notEther = addrFix;
        needType = typeFix;
    }

    public void closeUp() {
        lowerBr.delIface(ifcNum);
    }

    public void setParent(ifcDn parent) {
        lowerIf = parent;
        parent.setFilter(true);
    }

    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        if (stated != state.states.up) {
            cntr.drop(pck, counter.reasons.notUp);
            return;
        }
        pck.putStart();
        if (needType) {
            if (pck.msbGetW(0) != ifcBridge.serialType) {
                cntr.drop(pck, counter.reasons.badEthTyp);
                return;
            }
            pck.getSkip(ifcBridge.serialSize);
        }
        if (notEther) {
            ifcEther.parseETHheader(pck, false);
        }
        if ((pck.ETHtype == ipIfc4.type) && (filter4in != null)) {
            pck.getSkip(2);
            if (!filter4in.matches(true, true, pck)) {
                cntr.drop(pck, counter.reasons.denied);
                return;
            }
            pck.getSkip(-2);
        }
        if ((pck.ETHtype == ipIfc6.type) && (filter6in != null)) {
            pck.getSkip(2);
            if (!filter6in.matches(true, true, pck)) {
                cntr.drop(pck, counter.reasons.denied);
                return;
            }
            pck.getSkip(-2);
        }
        if (portSec != null) {
            if (portSec.find(pck.ETHsrc) == null) {
                cntr.drop(pck, counter.reasons.denied);
                return;
            }
        }
        ifaceAdjustMss(pck, tcp4mssIn, tcp6mssIn);
        lowerBr.doRxPack(this, pck);
    }

    /**
     * send one packet over this interface
     *
     * @param pck packet to send
     */
    protected void doTxPack(packHolder pck) {
        cntr.tx(pck);
        if (stated != state.states.up) {
            cntr.drop(pck, counter.reasons.notUp);
            return;
        }
        if (macRewrite != null) {
            pck.ETHsrc.setAddr(macRewrite);
        }
        if ((pck.ETHtype == ipIfc4.type) && (filter4out != null)) {
            pck.getSkip(2);
            if (!filter4out.matches(true, true, pck)) {
                cntr.drop(pck, counter.reasons.denied);
                return;
            }
            pck.getSkip(-2);
        }
        if ((pck.ETHtype == ipIfc6.type) && (filter6out != null)) {
            pck.getSkip(2);
            if (!filter6out.matches(true, true, pck)) {
                cntr.drop(pck, counter.reasons.denied);
                return;
            }
            pck.getSkip(-2);
        }
        ifaceAdjustMss(pck, tcp4mssOut, tcp6mssOut);
        if (notEther) {
            pck.merge2beg();
            ifcEther.createETHheader(pck, false);
            pck.merge2beg();
        }
        if (needType) {
            pck.msbPutW(0, ifcBridge.serialType);
            pck.putSkip(ifcBridge.serialSize);
            pck.merge2beg();
        }
        lowerIf.sendPack(pck);
    }

    private void ifaceAdjustMss(packHolder pck, int mss4, int mss6) {
        ipCor ipCore = null;
        int mss = 0;
        switch (pck.ETHtype) {
            case ipIfc4.type:
                ipCore = ipCore4;
                mss = mss4;
                break;
            case ipIfc6.type:
                ipCore = ipCore6;
                mss = mss6;
                break;
            default:
                return;
        }
        if (mss < 1) {
            return;
        }
        pck.getSkip(2);
        ipCore.parseIPheader(pck, false);
        if (pck.IPprt != prtTcp.protoNum) {
            pck.getSkip(-2);
            return;
        }
        pck.getSkip(pck.IPsiz);
        prtTcp.parseTCPports(pck);
        if ((pck.TCPflg & prtTcp.flagSYN) == 0) {
            pck.getSkip(-2);
            pck.getSkip(-pck.IPsiz);
            return;
        }
        prtTcp.updateTCPheader(pck, pck.UDPsrc, pck.UDPtrg, -1, -1, mss);
        pck.getSkip(-pck.IPsiz);
        ipCore.updateIPheader(pck, pck.IPsrc, pck.IPtrg, -1, -1, -1, -1, pck.UDPsiz);
        pck.getSkip(-2);
    }

    /**
     * get spantree packet
     *
     * @return spantree packet
     */
    public packStp getStpId() {
        packStp p = new packStp();
        p.id = 0;
        p.ver = 0;
        p.typ = 0;
        p.flag = 0;
        p.rootId = lowerBr.stpRoot.copyBytes();
        p.rootCost = lowerBr.stpCost;
        p.brdgId = lowerBr.getStpId();
        p.portId = ifcNum;
        p.msgAge = 0;
        p.maxAge = lowerBr.stpAge;
        p.hloTim = lowerBr.stpHlo;
        p.fwdTim = lowerBr.stpFwd;
        return p;
    }

    public int compare(ifcBridgeIfc v1, ifcBridgeIfc v2) {
        if (v1.ifcNum < v2.ifcNum) {
            return -1;
        }
        if (v1.ifcNum > v2.ifcNum) {
            return +1;
        }
        return 0;
    }

    /**
     * get interface name
     *
     * @return name
     */
    public String getIfcName() {
        if (lowerIf == null) {
            return "bvi";
        } else {
            return "" + lowerIf;
        }
    }

    public void mhostQuery(Object ifc, addrIP grp, addrIP src) {
    }

    public void mhostReport(Object ifc, addrIP grp, addrIP src, boolean need) {
        if (!need) {
            return;
        }
        if (groups == null) {
            groups = new tabGen<ifcBridgeGrp>();
        }
        ifcBridgeGrp group = new ifcBridgeGrp(grp);
        ifcBridgeGrp old = groups.add(group);
        if (old != null) {
            group = old;
        }
        group.time = bits.getTime();
    }

    public String toString() {
        return "brprt " + getIfcName();
    }

    /**
     * get interface show
     *
     * @return show
     */
    public String getShowIfc() {
        String a = "";
        if (groups != null) {
            for (int i = 0; i < groups.size(); i++) {
                a += " " + groups.get(i);
            }
        }
        return getIfcName() + "|" + (!blocked) + "|" + physical + "|" + cntr.getShPsum() + "|" + cntr.getShBsum() + "|" + a;
    }

    /**
     * get interface show
     *
     * @return show
     */
    public String getShowStp() {
        return getIfcName() + "|" + (!blocked) + "|" + physical + "|" + stpCntr.getShPsum();
    }

}
