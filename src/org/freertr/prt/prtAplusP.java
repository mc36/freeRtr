package org.freertr.prt;

import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrType;
import org.freertr.cfg.cfgVrf;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcUp;
import org.freertr.ip.ipIfc4;
import org.freertr.ip.ipIfc6;
import org.freertr.pack.packHolder;
import org.freertr.tab.tabIntMatcher;
import org.freertr.tab.tabQos;
import org.freertr.util.cmds;
import org.freertr.util.counter;
import org.freertr.util.state;

/**
 * address plus port (rfc6346) protocol translation
 *
 * @author matecsaba
 */
public class prtAplusP implements ifcDn {

    private final cfgVrf vrf;

    private final addrIP trg[];

    private counter cntr = new counter();

    private ifcUp upper = new ifcNull();

    /**
     * create instance
     *
     * @param v vrf to use
     * @param a config
     */
    public prtAplusP(cfgVrf v, String a) {
        vrf = v;
        trg = new addrIP[65536];
        cmds cmd = new cmds("lst", a);
        for (;;) {
            a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            addrIP adr = new addrIP();
            if (adr.fromString(a)) {
                continue;
            }
            tabIntMatcher m = new tabIntMatcher();
            a = cmd.word();
            if (m.fromString(a)) {
                continue;
            }
            for (int i = 0; i < trg.length; i++) {
                if (!m.matches(i)) {
                    continue;
                }
                trg[i] = adr;
            }
        }
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
     * close interface
     */
    public void closeDn() {
    }

    /**
     * flap interface
     */
    public void flapped() {
    }

    /**
     * set upper layer
     *
     * @param server upper layer
     */
    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
    }

    /**
     * get state
     *
     * @return state
     */
    public state.states getState() {
        return state.states.up;
    }

    /**
     * set filter
     *
     * @param promisc promiscous mode
     */
    public void setFilter(boolean promisc) {
    }

    /**
     * get hw address
     *
     * @return hw address
     */
    public addrType getHwAddr() {
        return new addrEmpty();
    }

    /**
     * get mtu size
     *
     * @return mtu size
     */
    public int getMTUsize() {
        return 1500;
    }

    /**
     * get bandwidth
     *
     * @return bandwidth
     */
    public long getBandwidth() {
        return 8000000;
    }

    /**
     * send packet
     *
     * @param pck packet
     */
    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        int i = pck.msbGetW(0);
        pck.getSkip(2);
        switch (i) {
            case ipIfc4.type:
                if (vrf.core4.parseIPheader(pck, true)) {
                    cntr.drop(pck, counter.reasons.badHdr);
                    return;
                }
                pck.getSkip(pck.IPsiz);
                break;
            case ipIfc6.type:
                if (vrf.core6.parseIPheader(pck, true)) {
                    cntr.drop(pck, counter.reasons.badHdr);
                    return;
                }
                pck.getSkip(pck.IPsiz);
                break;
            default:
                cntr.drop(pck, counter.reasons.badProto);
                return;
        }
        tabQos.classifyLayer4(pck);
        pck.getSkip(-pck.IPsiz);
        addrIP adr = trg[pck.UDPtrg % trg.length];
        if (adr == null) {
            cntr.drop(pck, counter.reasons.notInTab);
            return;
        }
        if (adr.isIPv4()) {
            vrf.fwd4.mplsTxPack(adr, pck, true);
        } else {
            vrf.fwd6.mplsTxPack(adr, pck, true);
        }
    }

}
