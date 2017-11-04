package prt;

import addr.addrEmpty;
import addr.addrIP;
import addr.addrIPv4;
import addr.addrIPv6;
import addr.addrType;
import ifc.ifcDn;
import ifc.ifcNull;
import ifc.ifcUp;
import ip.ipCor4;
import ip.ipCor6;
import ip.ipFwd;
import ip.ipFwdIface;
import ip.ipIfc4;
import ip.ipIfc6;
import ip.ipPrt;
import pack.packHolder;
import util.counter;
import util.state;
import util.logger;

/**
 * minimal encapsulation (rfc2004) packets
 *
 * @author matecsaba
 */
public class prtMinenc implements ipPrt, ifcDn {

    /**
     * protocol number
     */
    public static final int proto = 55;

    /**
     * sending ttl value, -1 means maps out
     */
    public int sendingTTL = 255;

    /**
     * sending tos value, -1 means maps out
     */
    public int sendingTOS = -1;

    private ipFwdIface sendingIfc;

    private ifcUp upper = new ifcNull();

    private ipFwd lower;

    private addrIP remote;

    private counter cntr = new counter();

    private ipCor4 ip4;

    private ipCor6 ip6;

    public counter getCounter() {
        return cntr;
    }

    /**
     * initialize context
     *
     * @param parent forwarder of encapsulated packets
     */
    public prtMinenc(ipFwd parent) {
        lower = parent;
        ip4 = new ipCor4();
        ip6 = new ipCor6();
    }

    /**
     * set target of tunnel
     *
     * @param ifc interface to source from
     * @param trg ip address of remote
     * @return false if successful, true if error happened
     */
    public boolean setEndpoints(ipFwdIface ifc, addrIP trg) {
        if (sendingIfc != null) {
            lower.protoDel(this, sendingIfc, remote);
        }
        remote = trg;
        sendingIfc = ifc;
        return lower.protoAdd(this, sendingIfc, remote);
    }

    public int getProtoNum() {
        return proto;
    }

    public void closeDn() {
        lower.protoDel(this, sendingIfc, remote);
    }

    public void flapped() {
    }

    public void closeUp(ipFwdIface iface) {
        upper.closeUp();
    }

    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
    }

    public void setState(ipFwdIface iface, state.states stat) {
        if (iface.ifwNum != sendingIfc.ifwNum) {
            return;
        }
        upper.setState(stat);
    }

    public addrType getHwAddr() {
        return new addrEmpty();
    }

    public void setFilter(boolean promisc) {
    }

    public state.states getState() {
        return state.states.up;
    }

    public void recvPack(ipFwdIface rxIfc, packHolder pck) {
        cntr.rx(pck);
        if (pck.IPprt != proto) {
            cntr.drop(pck, counter.reasons.badProto);
            return;
        }
        if (pck.IPsrc.compare(pck.IPsrc, remote) != 0) {
            cntr.drop(pck, counter.reasons.badSrcAddr);
            return;
        }
        pck.IPprt = pck.getByte(0); // protocol
        int flg = pck.getByte(1); // flags
        int i;
        if ((flg & 0x40) != 0) {
            i = addrIPv6.size;
        } else {
            i = addrIPv4.size;
        }
        if ((flg & 0x80) != 0) {
            i += i;
        }
        if (pck.getIPsum(0, i + 4, 0) != 0xffff) { // sum
            logger.info("got bad checksum from " + remote);
            cntr.drop(pck, counter.reasons.badSum);
            return;
        }
        pck.getSkip(4);
        if ((flg & 0x40) != 0) {
            addrIPv6 adr = new addrIPv6();
            pck.getAddr(adr, 0);
            pck.IPtrg.fromIPv6addr(adr);
            pck.getSkip(addrIPv6.size);
            if ((flg & 0x80) != 0) {
                pck.getAddr(adr, 0);
                pck.IPsrc.fromIPv6addr(adr);
                pck.getSkip(addrIPv6.size);
            }
            ip6.createIPheader(pck);
            i = ipIfc6.type;
        } else {
            addrIPv4 adr = new addrIPv4();
            pck.getAddr(adr, 0);
            pck.IPtrg.fromIPv4addr(adr);
            pck.getSkip(addrIPv4.size);
            if ((flg & 0x80) != 0) {
                pck.getAddr(adr, 0);
                pck.IPsrc.fromIPv4addr(adr);
                pck.getSkip(addrIPv4.size);
            }
            ip4.createIPheader(pck);
            i = ipIfc4.type;
        }
        pck.msbPutW(0, i);
        i = pck.headSize();
        pck.putSkip(2);
        pck.mergeHeader(-1, i);
        upper.recvPack(pck);
    }

    public boolean alertPack(ipFwdIface rxIfc, packHolder pck) {
        return true;
    }

    public void errorPack(counter.reasons err, addrIP rtr, ipFwdIface rxIfc, packHolder pck) {
    }

    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        int i = pck.msbGetW(0);
        pck.getSkip(2);
        switch (i) {
            case ipIfc4.type:
                if (ip4.parseIPheader(pck, true)) {
                    return;
                }
                pck.putByte(1, 0x80); // flags
                pck.putAddr(4, pck.IPtrg.toIPv4());
                pck.putAddr(4 + addrIPv4.size, pck.IPsrc.toIPv4());
                i = 4 + addrIPv4.size + addrIPv4.size;
                break;
            case ipIfc6.type:
                if (ip6.parseIPheader(pck, true)) {
                    return;
                }
                pck.putByte(1, 0xc0); // flags
                pck.putAddr(4, pck.IPtrg.toIPv6());
                pck.putAddr(4 + addrIPv6.size, pck.IPsrc.toIPv6());
                i = 4 + addrIPv6.size + addrIPv6.size;
                break;
            default:
                cntr.drop(pck, counter.reasons.badProto);
                return;
        }
        pck.getSkip(pck.IPsiz);
        pck.putByte(0, pck.IPprt); // protocol
        pck.msbPutW(2, 0); // checksum
        pck.lsbPutW(2, 0xffff - pck.putIPsum(0, i, 0)); // checksum
        pck.putSkip(i);
        pck.merge2beg();
        pck.putDefaults();
        if (sendingTTL >= 0) {
            pck.IPttl = sendingTTL;
        }
        if (sendingTOS >= 0) {
            pck.IPtos = sendingTOS;
        }
        pck.IPprt = proto;
        pck.IPtrg.setAddr(remote);
        pck.IPsrc.setAddr(sendingIfc.addr);
        lower.protoPack(sendingIfc, pck);
    }

    public String toString() {
        return "minenc to " + remote;
    }

    public int getMTUsize() {
        return sendingIfc.mtu;
    }

    public long getBandwidth() {
        return sendingIfc.bandwidth;
    }

}
