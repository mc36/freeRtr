package ifc;

import addr.addrIP;
import addr.addrMac;
import ip.ipIfc4;
import ip.ipIfc6;
import java.util.Comparator;
import pack.packHolder;
import pack.packStp;
import tab.tabAceslstN;
import tab.tabListing;
import tab.tabGen;
import util.counter;
import util.state;

/**
 * bridge interface handler
 *
 * @author matecsaba
 */
public class ifcBridgeIfc implements ifcUp, Comparator<ifcBridgeIfc> {

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
    public ifcDn lowerIf;

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
     * mac security
     */
    public tabGen<addrMac> macSec;

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

    private counter cntr = new counter();

    public counter getCounter() {
        return cntr;
    }

    public void setState(state.states stat) {
        stated = state.toUsable(stat);
        cntr.stateChange(stated);
        if (stat != state.states.up) {
            lowerBr.portFlap();
        }
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
        if (macSec != null) {
            boolean needed = false;
            for (int i = 0; i < macSec.size(); i++) {
                needed |= (pck.ETHsrc.compare(pck.ETHsrc, macSec.get(i)) == 0);
                if (needed) {
                    break;
                }
            }
            if (!needed) {
                cntr.drop(pck, counter.reasons.denied);
                return;
            }
        }
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

    public String toString() {
        return getIfcName() + "|" + (!blocked) + "|" + physical + "|" + cntr.getShBsum();
    }

}
