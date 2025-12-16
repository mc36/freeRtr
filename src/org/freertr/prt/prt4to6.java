package org.freertr.prt;

import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrType;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcUp;
import org.freertr.ip.ipCor4;
import org.freertr.ip.ipCor6;
import org.freertr.ip.ipIcmp4;
import org.freertr.ip.ipIcmp6;
import org.freertr.ip.ipIfc4;
import org.freertr.ip.ipIfc6;
import org.freertr.pack.packHolder;
import org.freertr.util.cmds;
import org.freertr.util.counter;
import org.freertr.util.state;

/**
 * v4 to v6 protocol translation
 *
 * @author matecsaba
 */
public class prt4to6 implements ifcDn {

    private counter cntr = new counter();

    private ifcUp upper = new ifcNull();

    private final ipCor4 ip4;

    private final ipCor6 ip6;

    private final ipIcmp4 icmp4;

    private final ipIcmp6 icmp6;

    private final addrIP loc4;

    private final addrIP loc6;

    private final addrIP rem4;

    private final addrIP rem6;

    /**
     * get counter
     *
     * @return counter
     */
    public counter getCounter() {
        return cntr;
    }

    /**
     * initialize context
     *
     * @param pr4 remote ipv4
     * @param cmd remote ipv6, loc4, loc6
     */
    public prt4to6(addrIP pr4, cmds cmd) {
        ip4 = new ipCor4();
        ip6 = new ipCor6();
        icmp4 = new ipIcmp4();
        icmp6 = new ipIcmp6();
        rem4 = pr4.copyBytes();
        rem6 = new addrIP();
        rem6.fromString(cmd.word());
        loc4 = new addrIP();
        loc4.fromString(cmd.word());
        loc6 = new addrIP();
        loc6.fromString(cmd.word());
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
                if (ip4.parseIPheader(pck, true)) {
                    cntr.drop(pck, counter.reasons.badHdr);
                    return;
                }
                pck.getSkip(pck.IPsiz);
                if (protoConv(false, pck)) {
                    break;
                }
                ip6.createIPheader(pck);
                pck.msbPutW(0, ipIfc6.type);
                pck.putSkip(2);
                pck.merge2beg();
                upper.recvPack(pck);
                break;
            case ipIfc6.type:
                if (ip6.parseIPheader(pck, true)) {
                    cntr.drop(pck, counter.reasons.badHdr);
                    return;
                }
                pck.getSkip(pck.IPsiz);
                if (protoConv(true, pck)) {
                    break;
                }
                ip4.createIPheader(pck);
                pck.msbPutW(0, ipIfc4.type);
                pck.putSkip(2);
                pck.merge2beg();
                upper.recvPack(pck);
                break;
            default:
                cntr.drop(pck, counter.reasons.badProto);
                return;
        }
    }

    private void addrConv(boolean mod, packHolder pck) {
        if (mod) {
            pck.IPsrc.setAddr(loc4);
            pck.IPtrg.setAddr(rem4);
        } else {
            pck.IPsrc.setAddr(loc6);
            pck.IPtrg.setAddr(rem6);
        }
    }

    private boolean protoConv(boolean mod, packHolder pck) {
        switch (pck.IPprt) {
            case ipIcmp4.protoNum:
                if (icmp4.parseICMPheader(pck)) {
                    return true;
                }
                switch (pck.ICMPtc) {
                    case ipIcmp4.icmpEchoRep:
                        pck.ICMPtc = ipIcmp6.icmpEchoRep;
                        break;
                    case ipIcmp4.icmpEchoReq:
                        pck.ICMPtc = ipIcmp6.icmpEchoReq;
                        break;
                    default:
                        return true;
                }
                pck.msbPutD(4, pck.msbGetD(4)); // id
                pck.getSkip(ipIcmp4.size);
                addrConv(mod, pck);
                icmp6.createICMPheader(pck);
                break;
            case ipIcmp6.protoNum:
                if (icmp6.parseICMPheader(pck)) {
                    return true;
                }
                switch (pck.ICMPtc) {
                    case ipIcmp6.icmpEchoRep:
                        pck.ICMPtc = ipIcmp4.icmpEchoRep;
                        break;
                    case ipIcmp6.icmpEchoReq:
                        pck.ICMPtc = ipIcmp4.icmpEchoReq;
                        break;
                    default:
                        return true;
                }
                pck.msbPutD(4, pck.msbGetD(4)); // id
                pck.getSkip(ipIcmp6.size);
                addrConv(mod, pck);
                icmp4.createICMPheader(pck);
                break;
            case prtTcp.protoNum:
                addrConv(mod, pck);
                prtTcp.updateTCPheader(pck, -1, -1, -1, -1, -1);
                break;
            case prtUdp.protoNum:
                addrConv(mod, pck);
                prtUdp.updateUDPheader(pck, -1, -1);
                break;
            case prtLudp.protoNum:
                addrConv(mod, pck);
                prtLudp.updateLUDPheader(pck, -1, -1);
                break;
            case prtDccp.protoNum:
                addrConv(mod, pck);
                prtDccp.updateDCCPheader(pck, -1, -1);
                break;
            case prtSctp.protoNum:
                if (prtSctp.parseSCTPheader(pck)) {
                    return true;
                }
                addrConv(mod, pck);
                prtSctp.updateSCTPheader(pck, -1, -1);
                break;
            default:
                addrConv(mod, pck);
                break;
        }
        return false;
    }

}
