package net.freertr.ip;

import net.freertr.addr.addrIP;
import net.freertr.cfg.cfgAll;
import net.freertr.pack.packHolder;
import net.freertr.util.counter;
import net.freertr.util.debugger;
import net.freertr.util.logger;
import net.freertr.util.state;

/**
 * ipv4 icmp (rfc792) packet handler
 *
 * @author matecsaba
 */
public class ipIcmp4 implements ipIcmp, ipPrt {

    /**
     * create instance
     */
    public ipIcmp4() {
    }

    /**
     * size of icmp header
     */
    public final static int size = 8;

    /**
     * protocol number of icmp
     */
    public final static int protoNum = 1;

    private counter cntr = new counter();

    private ipFwd fwdCore;

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
     * @param reason reason
     * @param ifip interface
     * @param mplsExt add mpls extension
     * @return false on success, true on error
     */
    public boolean createError(packHolder pck, counter.reasons reason, ipFwdIface ifip, boolean mplsExt) {
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
        addrIP src = ifip.getUnreachAddr();
        if (src == null) {
            return true;
        }
        int typ;
        int dat = 0;
        switch (reason) {
            case noRoute:
            case noIface:
                typ = icmpUnreachNetw;
                break;
            case denied:
                typ = icmpUnreachFilter;
                break;
            case notInTab:
                typ = icmpUnreachHost;
                break;
            case badProto:
                typ = icmpUnreachProt;
                break;
            case badPort:
                typ = icmpUnreachPort;
                break;
            case fragment:
                typ = icmpUnreachFrag;
                dat = ifip.pmtuds;
                break;
            case ttlExceed:
                typ = icmpUnreachTtle;
                break;
            default:
                return true;
        }
        pck.putDefaults();
        pck.IPtrg.setAddr(pck.IPsrc);
        pck.IPsrc.setAddr(src);
        pck.putStart();
        if (pck.dataSize() > maxErrorSize) {
            pck.setDataSize(maxErrorSize);
        }
        pck.ICMPtc = typ;
        if (mplsExt) {
            ipFwdEcho.addMplsExt(pck);
        }
        pck.msbPutD(4, dat); // optional data
        createICMPheader(pck);
        return false;
    }

    /**
     * parse icmp ids
     *
     * @param pck packet to parse
     */
    public static void parseICMPports(packHolder pck) {
        pck.ICMPtc = pck.msbGetW(0); // type:8 code:8
        pck.UDPsrc = pck.msbGetD(4); // id:16 seq:16
        pck.UDPsiz = size;
    }

    /**
     * parse header
     *
     * @param pck packet
     * @return false on success, true on error
     */
    public boolean parseICMPheader(packHolder pck) {
        parseICMPports(pck);
        if (pck.dataSize() < size) {
            logger.info("got too small from " + pck.IPsrc);
            cntr.drop(pck, counter.reasons.badLen);
            return true;
        }
        if (cfgAll.icmp4ChecksumRx) {
            if (pck.getIPsum(0, pck.dataSize(), 0) != 0xffff) { // sum
                logger.info("got bad checksum from " + pck.IPsrc);
                cntr.drop(pck, counter.reasons.badSum);
                return true;
            }
        }
        if (debugger.ipIcmp4traf) {
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
        if (debugger.ipIcmp4traf) {
            logger.debug("tx " + pck.IPsrc + " -> " + pck.IPtrg + " typ=" + icmp2string(pck.ICMPtc));
        }
        pck.msbPutW(0, pck.ICMPtc); // type:8 code:8
        pck.msbPutW(2, 0); // checksum
        if (cfgAll.icmp4ChecksumTx) {
            int i = pck.putIPsum(0, size, 0);
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
        pck.msbPutW(2, 0); // checksum
        if (cfgAll.icmp4ChecksumTx) {
            int i = pck.putIPsum(0, size, 0);
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
            case icmpUnreachNetw:
                return "unreachNet";
            case icmpUnreachHost:
                return "unreachHost";
            case icmpUnreachProt:
                return "unreachProto";
            case icmpUnreachPort:
                return "unreachPort";
            case icmpUnreachFrag:
                return "fragNeeded";
            case icmpUnreachTtle:
                return "ttlExceed";
            case icmpUnreachFilter:
                return "prohibited";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * network unreachable
     */
    public final static int icmpUnreachNetw = 0x0300;

    /**
     * host unreachable
     */
    public final static int icmpUnreachHost = 0x0301;

    /**
     * protocol unreachable
     */
    public final static int icmpUnreachProt = 0x0302;

    /**
     * port unreachable
     */
    public final static int icmpUnreachPort = 0x0303;

    /**
     * fragmentation needed
     */
    public final static int icmpUnreachFrag = 0x0304;

    /**
     * administratively prohibited
     */
    public final static int icmpUnreachFilter = 0x030d;

    /**
     * ttl expired
     */
    public final static int icmpUnreachTtle = 0x0b00;

    /**
     * echo request
     */
    public final static int icmpEchoReq = 0x0800;

    /**
     * echo reply
     */
    public final static int icmpEchoRep = 0x0000;

    /**
     * get protocol number
     *
     * @return number
     */
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

    /**
     * get counter
     *
     * @return counter
     */
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
        if (debugger.ipIcmp4traf) {
            logger.debug("rec " + pck.IPsrc + " -> " + pck.IPtrg + " typ=" + icmp2string(pck.ICMPtc));
        }
        switch (pck.ICMPtc) {
            case icmpEchoReq:
                fwdCore.echoRcvd++;
                int i = pck.IPtos;
                int o = pck.IPid;
                pck.ICMPtc = icmpEchoRep;
                addrIP adr = pck.IPsrc.copyBytes();
                pck.IPsrc.setAddr(rxIfc.addr);
                pck.IPtrg.setAddr(adr);
                pck.putDefaults();
                pck.putStart();
                pck.IPtos = i;
                pck.IPid = o;
                pck.msbPutD(4, pck.msbGetD(4)); // id
                pck.getSkip(size);
                createICMPheader(pck);
                fwdCore.protoPack(rxIfc, null, pck);
                break;
            case icmpEchoRep:
                int id = pck.msbGetD(4);
                pck.getSkip(size);
                fwdCore.echoRecvRep(pck, id);
                break;
            case icmpUnreachNetw:
                pck.getSkip(size);
                fwdCore.errorReport(counter.reasons.noRoute, rxIfc, pck);
                break;
            case icmpUnreachHost:
                pck.getSkip(size);
                fwdCore.errorReport(counter.reasons.notInTab, rxIfc, pck);
                break;
            case icmpUnreachProt:
                pck.getSkip(size);
                fwdCore.errorReport(counter.reasons.badProto, rxIfc, pck);
                break;
            case icmpUnreachPort:
                pck.getSkip(size);
                fwdCore.errorReport(counter.reasons.badPort, rxIfc, pck);
                break;
            case icmpUnreachFrag:
                pck.getSkip(size);
                fwdCore.errorReport(counter.reasons.fragment, rxIfc, pck);
                break;
            case icmpUnreachTtle:
                pck.getSkip(size);
                fwdCore.errorReport(counter.reasons.ttlExceed, rxIfc, pck);
                break;
            case icmpUnreachFilter:
                pck.getSkip(size);
                fwdCore.errorReport(counter.reasons.denied, rxIfc, pck);
                break;
            default:
                break;
        }
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
        parseICMPports(pck);
        if (pck.ICMPtc != icmpEchoReq) {
            return;
        }
        int id = pck.msbGetD(4);
        pck.UDPsiz = size;
        fwdCore.echoRecvErr(pck, id, err, rtr);
    }

    public String toString() {
        return "icmp on " + fwdCore;
    }

}
