package ip;

import addr.addrIP;
import addr.addrIPv6;
import addr.addrType;
import cfg.cfgAll;
import pack.packHolder;
import util.bits;
import util.counter;
import util.debugger;
import util.logger;
import util.state;
import util.typLenVal;

/**
 * ipv6 icmp (rfc2463) packet handler
 *
 * @author matecsaba
 */
public class ipIcmp6 implements ipIcmp, ipPrt {

    /**
     * size of icmp header
     */
    public final static int size = 8;

    /**
     * protocol number of icmp
     */
    public final static int protoNum = 58;

    private counter cntr = new counter();

    private ipFwd fwdCore;

    private ipCor6 ipCore = new ipCor6();

    private typLenVal tlv = getTLVreader();

    /**
     * get type length value handler
     *
     * @return tlv handler
     */
    public static typLenVal getTLVreader() {
        return new typLenVal(0, 8, 8, 8, 8, 2, 2, 1, 0, 512, true);
    }

    /**
     * set forwarder
     *
     * @param ifw forwarder
     */
    public void setForwarder(ipFwd ifw) {
        fwdCore = ifw;
        ifw.protoAdd(this, null, null);
    }

    /**
     * create echo
     *
     * @param pck packet
     * @param src source
     * @param trg target
     * @param id id
     * @return false on success, true on error
     */
    public boolean createEcho(packHolder pck, addrIP src, addrIP trg, int id) {
        pck.putDefaults();
        pck.IPsrc.setAddr(src);
        pck.IPtrg.setAddr(trg);
        pck.putStart();
        pck.ICMPtc = icmpEchoReq;
        pck.msbPutD(4, id); // id
        createICMPheader(pck);
        return false;
    }

    /**
     * create error
     *
     * @param pck packet
     * @param reason reason code
     * @param ifip address
     * @return false on success, true on error
     */
    public boolean createError(packHolder pck, counter.reasons reason, addrIP ifip) {
        final int maxErrorSize = 512;
        if (pck.IPprt == protoNum) {
            pck.getSkip(pck.IPsiz);
            boolean b = parseICMPheader(pck);
            pck.getSkip(-pck.IPsiz);
            if (b) {
                return true;
            }
            if (pck.ICMPtc != icmpEchoReq) {
                return true;
            }
        }
        int typ;
        switch (reason) {
            case noRoute:
            case noIface:
                typ = icmpUnreachRoute;
                break;
            case denied:
                typ = icmpUnreachFilter;
                break;
            case notInTab:
                typ = icmpUnreachAddress;
                break;
            case badProto:
            case badPort:
                typ = icmpUnreachPort;
                break;
            case fragment:
                typ = icmpFragNeed;
                break;
            case ttlExceed:
                typ = icmpTtlXced;
                break;
            default:
                return true;
        }
        pck.putDefaults();
        pck.IPtrg.setAddr(pck.IPsrc);
        pck.IPsrc.setAddr(ifip);
        pck.putStart();
        if (pck.dataSize() > maxErrorSize) {
            pck.setDataSize(maxErrorSize);
        }
        pck.ICMPtc = typ;
        pck.msbPutD(4, 0); // unused
        createICMPheader(pck);
        return false;
    }

    /**
     * parse header
     *
     * @param pck packet
     * @return false on success, true on error
     */
    public boolean parseICMPheader(packHolder pck) {
        if (pck.dataSize() < size) {
            logger.info("got too small from " + pck.IPsrc);
            cntr.drop(pck, counter.reasons.badLen);
            return true;
        }
        if (cfgAll.icmp6ChecksumRx) {
            int i = pck.pseudoIPsum(pck.dataSize());
            if (pck.getIPsum(0, pck.dataSize(), i) != 0xffff) { // sum
                logger.info("got bad checksum from " + pck.IPsrc);
                cntr.drop(pck, counter.reasons.badSum);
                return true;
            }
        }
        pck.ICMPtc = pck.msbGetW(0); // type:8 code:8
        pck.UDPsrc = pck.msbGetD(4); // id:16 seq:16
        if (debugger.ipIcmp6traf) {
            logger.debug("rx " + pck.IPsrc + " -> " + pck.IPtrg + " typ=" + icmp2string(pck.ICMPtc) + " id=" + pck.UDPsrc);
        }
        return false;
    }

    /**
     * create header
     *
     * @param pck packet
     */
    public void createICMPheader(packHolder pck) {
        cntr.tx(pck);
        pck.IPprt = protoNum;
        if (debugger.ipIcmp6traf) {
            logger.debug("tx " + pck.IPsrc + " -> " + pck.IPtrg + " typ=" + icmp2string(pck.ICMPtc));
        }
        pck.msbPutW(0, pck.ICMPtc); // type:8 code:8
        pck.msbPutW(2, 0); // checksum
        if (cfgAll.icmp6ChecksumTx) {
            int i = pck.pseudoIPsum(size + pck.dataSize());
            i = pck.putIPsum(0, size, i);
            i = pck.getIPsum(0, pck.dataSize(), i);
            pck.lsbPutW(2, 0xffff - i); // checksum
        }
        pck.putSkip(size);
        pck.merge2beg();
    }

    /**
     * update header
     *
     * @param pck packet
     */
    public void updateICMPheader(packHolder pck) {
        pck.unMergeBytes(size);
        pck.putSkip(-size);
        pck.lsbPutW(2, 0); // checksum
        if (cfgAll.icmp6ChecksumTx) {
            int i = pck.pseudoIPsum(size + pck.dataSize());
            i = pck.putIPsum(0, size, i);
            i = pck.getIPsum(0, pck.dataSize(), i);
            pck.lsbPutW(2, 0xffff - i); // checksum
        }
        pck.putSkip(size);
        pck.merge2beg();
    }

    /**
     * code to string
     *
     * @param i code
     * @return string
     */
    public String icmp2string(int i) {
        switch (i) {
            case icmpEchoReq:
                return "echoReq";
            case icmpEchoRep:
                return "echoRep";
            case icmpRtrSol:
                return "rtrSol";
            case icmpRtrAdv:
                return "rtrAdv";
            case icmpNeiSol:
                return "neiSol";
            case icmpNeiAdv:
                return "neiAdv";
            case icmpUnreachRoute:
                return "noRoute";
            case icmpUnreachFilter:
                return "prohibited";
            case icmpUnreachAddress:
                return "addrUnreach";
            case icmpUnreachPort:
                return "portUnreach";
            case icmpFragNeed:
                return "tooBig";
            case icmpTtlXced:
                return "ttlExcd";
            case icmpMcastQuery:
                return "mldQuery";
            case icmpMcastRprt1:
                return "mld1report";
            case icmpMcastDone:
                return "mldDone";
            case icmpMcastRprt2:
                return "mld2report";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * router solicitation
     */
    public final static int icmpRtrSol = 0x8500;

    /**
     * router advertisement
     */
    public final static int icmpRtrAdv = 0x8600;

    /**
     * neighbor solicitation
     */
    public final static int icmpNeiSol = 0x8700;

    /**
     * neighbor advertisement
     */
    public final static int icmpNeiAdv = 0x8800;

    /**
     * echo request
     */
    public final static int icmpEchoReq = 0x8000;

    /**
     * echo reply
     */
    public final static int icmpEchoRep = 0x8100;

    /**
     * no route to destination
     */
    public final static int icmpUnreachRoute = 0x0100;

    /**
     * administratively prohibited
     */
    public final static int icmpUnreachFilter = 0x0101;

    /**
     * address unreachable
     */
    public final static int icmpUnreachAddress = 0x0103;

    /**
     * port unreachable
     */
    public final static int icmpUnreachPort = 0x0104;

    /**
     * fragmentation needed
     */
    public final static int icmpFragNeed = 0x0200;

    /**
     * ttl exceeded
     */
    public final static int icmpTtlXced = 0x0300;

    /**
     * multicast listener query
     */
    public final static int icmpMcastQuery = 0x8200;

    /**
     * multicast listener report
     */
    public final static int icmpMcastRprt1 = 0x8300;

    /**
     * multicast listener done
     */
    public final static int icmpMcastDone = 0x8400;

    /**
     * multicast listener v2 report
     */
    public final static int icmpMcastRprt2 = 0x8f00;

    public int getProtoNum() {
        return protoNum;
    }

    /**
     * get header size
     *
     * @return size
     */
    public int getHeadSize() {
        return size;
    }

    /**
     * close interface
     *
     * @param iface interface
     */
    public void closeUp(ipFwdIface iface) {
    }

    /**
     * set state
     *
     * @param iface interface
     * @param stat state
     */
    public void setState(ipFwdIface iface, state.states stat) {
    }

    public counter getCounter() {
        return cntr;
    }

    /**
     * received packet
     *
     * @param rxIfc interface
     * @param pck packet
     */
    public void recvPack(ipFwdIface rxIfc, packHolder pck) {
        cntr.rx(pck);
        if (parseICMPheader(pck)) {
            return;
        }
        if (debugger.ipIcmp6traf) {
            logger.debug("rec " + pck.IPsrc + " -> " + pck.IPtrg + " typ=" + icmp2string(pck.ICMPtc));
        }
        ipIfc6 ifc = (ipIfc6) rxIfc.lower;
        addrIP adr;
        switch (pck.ICMPtc) {
            case icmpNeiSol:
                adr = new addrIP();
                addrIPv6 adr6 = new addrIPv6();
                pck.getAddr(adr6, size);
                adr.fromIPv6addr(adr6);
                ifc.gotIcmpPack(pck);
                addrType mac = rxIfc.lower.checkMyAlias(adr);
                if (mac != null) {
                    createNeighAdv(mac, pck, pck.IPsrc.toIPv6(), adr.toIPv6(), true);
                    ifc.sendProto(pck, pck.IPtrg);
                    break;
                }
                boolean rep = rxIfc.lower.checkMyAddress(adr);
                if (rxIfc.lower.checkConnected(adr)) {
                    rep |= ifc.ifcHdr.answerNetReqs;
                } else {
                    rep |= ifc.ifcHdr.answerDefReqs;
                }
                if (!rep) {
                    break;
                }
                mac = ifc.getHWaddr();
                createNeighAdv(mac, pck, pck.IPsrc.toIPv6(), adr.toIPv6(), true);
                ifc.sendProto(pck, pck.IPtrg);
                break;
            case icmpNeiAdv:
                ifc.gotIcmpPack(pck);
                break;
            case icmpRtrSol:
                ifc.gotIcmpPack(pck);
                if (ifc.rtrAdvInterval < 1) {
                    break;
                }
                if (ifc.rtrAdvSuppress) {
                    break;
                }
                adr6 = null;
                if (ifc.rtrAdvDns != null) {
                    adr6 = ifc.rtrAdvDns.toIPv6();
                }
                createRouterAdv(ifc.getHWaddr(), pck, pck.IPsrc.toIPv6(), ifc.getLLaddr().toIPv6(), rxIfc.addr.toIPv6(), rxIfc.mask, rxIfc.mtu, adr6);
                ifc.sendProto(pck, pck.IPtrg);
                break;
            case icmpRtrAdv:
                ifc.gotIcmpPack(pck);
                break;
            case icmpEchoReq:
                int i = pck.IPtos;
                pck.ICMPtc = icmpEchoRep;
                adr = pck.IPsrc.copyBytes();
                pck.IPsrc.setAddr(rxIfc.addr);
                pck.IPtrg.setAddr(adr);
                pck.putDefaults();
                pck.putStart();
                pck.IPtos = i;
                pck.msbPutD(4, pck.msbGetD(4)); // id
                pck.getSkip(size);
                createICMPheader(pck);
                fwdCore.protoPack(rxIfc, pck);
                break;
            case icmpEchoRep:
                int id = pck.msbGetD(4);
                pck.getSkip(size);
                fwdCore.echoRecvRep(pck, id);
                break;
            case icmpMcastQuery:
            case icmpMcastRprt1:
            case icmpMcastRprt2:
            case icmpMcastDone:
                pck.getSkip(size);
                fwdCore.mhostCore.recvPack(rxIfc, pck);
                break;
            case icmpUnreachRoute:
                pck.getSkip(size);
                fwdCore.errorReport(counter.reasons.noRoute, rxIfc, pck);
                break;
            case icmpUnreachFilter:
                pck.getSkip(size);
                fwdCore.errorReport(counter.reasons.denied, rxIfc, pck);
                break;
            case icmpUnreachAddress:
                pck.getSkip(size);
                fwdCore.errorReport(counter.reasons.notInTab, rxIfc, pck);
                break;
            case icmpUnreachPort:
                pck.getSkip(size);
                fwdCore.errorReport(counter.reasons.badPort, rxIfc, pck);
                break;
            case icmpFragNeed:
                pck.getSkip(size);
                fwdCore.errorReport(counter.reasons.fragment, rxIfc, pck);
                break;
            case icmpTtlXced:
                pck.getSkip(size);
                fwdCore.errorReport(counter.reasons.ttlExceed, rxIfc, pck);
                break;
            default:
                break;
        }
    }

    /**
     * create neighbor advertisement
     *
     * @param hwa hardware address
     * @param pck packet to create
     * @param trg neighbor address
     * @param src my address
     * @param soli solicited
     */
    public void createNeighAdv(addrType hwa, packHolder pck, addrIPv6 trg, addrIPv6 src, boolean soli) {
        src = src.copyBytes();
        trg = trg.copyBytes();
        if (debugger.ipIcmp6traf) {
            logger.debug("neiAdv " + src + " -> " + trg);
        }
        pck.clear();
        pck.ICMPtc = icmpNeiAdv;
        pck.IPsrc.fromIPv6addr(src);
        pck.IPtrg.fromIPv6addr(trg);
        pck.putDefaults();
        pck.putStart();
        pck.putAddr(0, src);
        pck.putSkip(addrIPv6.size);
        if (hwa.getSize() > 0) {
            tlv.putAddr(pck, 2, hwa);
        }
        pck.merge2beg();
        int i = 0x80000000; // router bit always set
        if (soli) {
            i |= 0x60000000; // solicited, override
        }
        pck.msbPutD(4, i); // flags
        createICMPheader(pck);
        ipCore.createIPheader(pck);
    }

    /**
     * create neighbor solicitation
     *
     * @param hwa hardware address
     * @param pck packet to create
     * @param trg neighbor address
     * @param src my address
     */
    public void createNeighSol(addrType hwa, packHolder pck, addrIPv6 trg, addrIPv6 src) {
        trg = trg.copyBytes();
        src = src.copyBytes();
        if (debugger.ipIcmp6traf) {
            logger.debug("neiSol " + src + " -> " + trg);
        }
        pck.clear();
        pck.ICMPtc = icmpNeiSol;
        pck.IPsrc.fromIPv6addr(src);
        src = trg.conv2solicited();
        pck.IPtrg.fromIPv6addr(src);
        pck.putDefaults();
        pck.putStart();
        pck.putAddr(0, trg);
        pck.putSkip(addrIPv6.size);
        if (hwa.getSize() > 0) {
            tlv.putAddr(pck, 1, hwa);
        }
        pck.merge2beg();
        pck.msbPutD(4, 0); // reserved
        createICMPheader(pck);
        ipCore.createIPheader(pck);
    }

    /**
     * create router solicitation
     *
     * @param hwa hardware address
     * @param pck packet to create
     * @param src my address
     */
    public void createRouterSol(addrType hwa, packHolder pck, addrIPv6 src) {
        src = src.copyBytes();
        if (debugger.ipIcmp6traf) {
            logger.debug("rtrSol " + src);
        }
        pck.clear();
        pck.ICMPtc = icmpRtrSol;
        pck.IPsrc.fromIPv6addr(src);
        pck.IPtrg.fromString("ff02::2");
        pck.putDefaults();
        pck.putStart();
        if (hwa.getSize() > 0) {
            tlv.putAddr(pck, 1, hwa);
        }
        pck.merge2beg();
        pck.msbPutD(4, 0); // reserved
        createICMPheader(pck);
        ipCore.createIPheader(pck);
    }

    /**
     * create router advertisement
     *
     * @param hwa hardware address
     * @param pck packet to create
     * @param trg neighbor address
     * @param src my address
     * @param net network
     * @param mask mask
     * @param mtu maximum transmission unit
     * @param dns name server address
     */
    public void createRouterAdv(addrType hwa, packHolder pck, addrIPv6 trg, addrIPv6 src, addrIPv6 net, int mask, int mtu, addrIPv6 dns) {
        if (trg == null) {
            trg = addrIPv6.getAllNodes();
        } else {
            trg = trg.copyBytes();
        }
        src = src.copyBytes();
        net = net.copyBytes();
        addrIPv6 msk = new addrIPv6();
        msk.fromNetmask(mask);
        net.setAnd(net, msk);
        if (debugger.ipIcmp6traf) {
            logger.debug("rtrAdv " + src + "/" + mask + " -> " + trg);
        }
        pck.clear();
        pck.ICMPtc = icmpRtrAdv;
        pck.IPsrc.fromIPv6addr(src);
        pck.IPtrg.fromIPv6addr(trg);
        pck.putDefaults();
        pck.putStart();
        pck.msbPutD(0, 0); // reachable time
        pck.msbPutD(4, 0); // retrans timer
        pck.putSkip(8);
        if (hwa.getSize() > 0) {
            tlv.putAddr(pck, 1, hwa); // hw address
        }
        byte buf[] = new byte[64];
        buf[0] = (byte) mask; // prefix length
        buf[1] = (byte) 0xc0; // onlink:1 auto:1 router:1 site:1 reserved:4
        bits.msbPutD(buf, 2, 2592000); // valid lifetime
        bits.msbPutD(buf, 6, 604800); // preferred lifetime
        bits.msbPutD(buf, 10, 0); // reserved
        net.toBuffer(buf, 14); // prefix
        tlv.putBytes(pck, 3, net.getSize() + 14, buf); // prefix info
        if (mtu > 0) {
            bits.msbPutW(buf, 0, 0); // reserved
            bits.msbPutD(buf, 2, mtu); // mtu
            tlv.putBytes(pck, 5, 6, buf); // mtu
        }
        if (dns != null) {
            bits.msbPutW(buf, 0, 0); // reserved
            bits.msbPutD(buf, 2, 604800); // preferred lifetime
            dns.toBuffer(buf, 6); // address
            tlv.putBytes(pck, 25, dns.getSize() + 6, buf); // dns info
        }
        pck.merge2beg();
        pck.putByte(4, 64); // hop limit
        pck.putByte(5, 0); // managed:1 other:1 homeAgent:1 prefer:2 reserved:3
        pck.msbPutW(6, 1800); // router lifetime
        createICMPheader(pck);
        ipCore.createIPheader(pck);
    }

    /**
     * alert packet
     *
     * @param rxIfc interface
     * @param pck packet
     * @return false on success, true on error
     */
    public boolean alertPack(ipFwdIface rxIfc, packHolder pck) {
        return true;
    }

    /**
     * error packet
     *
     * @param err error code
     * @param rtr address
     * @param rxIfc interface
     * @param pck packet
     */
    public void errorPack(counter.reasons err, addrIP rtr, ipFwdIface rxIfc, packHolder pck) {
    }

    public String toString() {
        return "icmp on " + fwdCore;
    }

}
