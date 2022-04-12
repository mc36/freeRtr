package net.freertr.ip;

import net.freertr.addr.addrIP;
import net.freertr.addr.addrIPv6;
import net.freertr.pack.packHolder;
import net.freertr.util.debugger;
import net.freertr.util.logger;

/**
 * does ipv6 (rfc2463) packet handling
 *
 * @author matecsaba
 */
public class ipCor6 implements ipCor {

    /**
     * create instance
     */
    public ipCor6() {
    }

    /**
     * default sending ttl value
     */
    public int sendingTTL = 255;

    /**
     * default sending tos value
     */
    public int sendingTOS = 0;

    /**
     * size of ipv6 header
     */
    public final static int size = 40;

    /**
     * protocol version
     */
    public final static int protocolVersion = 6;

    /**
     * protocol nlpid
     */
    public final static int protocolNLPID = 0x8e;

    /**
     * protocol number
     */
    public final static int protocolNumber = 41;

    /**
     * hop by hop
     */
    public final static int exthdrHopByHop = 0;

    /**
     * routing
     */
    public final static int exthdrRouting = 43;

    /**
     * fragment
     */
    public final static int exthdrFragment = 44;

    /**
     * destination options
     */
    public final static int exthdrDstOpt = 60;

    public int getVersion() {
        return protocolVersion;
    }

    public int getProtocol() {
        return protocolNumber;
    }

    public int getNlpid() {
        return protocolNLPID;
    }

    public int getHeaderSize() {
        return size;
    }

    /**
     * skip extension header
     *
     * @param pck packet to use
     */
    public static void skipExtHeader(packHolder pck) {
        pck.IPprt = pck.getByte(pck.IPsiz + 0); // next header
        pck.IPsiz += (pck.getByte(pck.IPsiz + 1) + 1) << 3; // header length
    }

    public boolean parseIPheader(packHolder pck, boolean chksiz) {
        int verTos = pck.msbGetW(0); // version:4 tos:8 reserved:4
        if ((verTos >>> 12) != protocolVersion) {
            logger.info("got bad version from " + pck.IPsrc);
            return true;
        }
        int totLen = pck.msbGetW(4) + size; // total length (hdr excl)
        if (chksiz) {
            if (totLen > pck.dataSize()) {
                logger.info("got truncated from " + pck.IPsrc);
                return true;
            }
            pck.setDataSize(totLen);
        }
        addrIPv6 adr = new addrIPv6();
        pck.IPtos = (verTos >>> 4) & 0xff;
        pck.IPid = pck.msbGetD(0) & 0xfffff; // flow label
        pck.IPprt = pck.getByte(6); // next header
        pck.IPttl = pck.getByte(7); // hop limit
        pck.getAddr(adr, 8); // source address
        pck.IPsrc.fromIPv6addr(adr);
        pck.IPlnk = adr.isLinkLocal();
        pck.getAddr(adr, 24); // destination address
        pck.IPtrg.fromIPv6addr(adr);
        pck.IPbrd = adr.isBroadcast();
        pck.IPmlt = adr.isMulticast();
        pck.IPmlr = adr.isRoutedMcast();
        pck.IPlnk |= adr.isLinkLocal();
        pck.IPsiz = size;
        pck.IPver = protocolVersion;
        pck.IPdf = false;
        pck.IPmf = false;
        pck.IPfrg = 0;
        switch (pck.IPprt) {
            case exthdrHopByHop:
                if (pck.msbGetW(pck.IPsiz + 2) == 0x0502) {
                    pck.IPalrt = pck.msbGetW(pck.IPsiz + 4);
                }
                skipExtHeader(pck);
                break;
            case exthdrFragment:
                pck.IPfrg = pck.msbGetW(pck.IPsiz + 2);
                pck.IPid = pck.msbGetD(pck.IPsiz + 4);
                pck.IPmf = (pck.IPfrg & 1) != 0;
                pck.IPfrg = (pck.IPfrg >>> 3) * 8;
                skipExtHeader(pck);
                break;
            case exthdrDstOpt:
                skipExtHeader(pck);
                break;
            default:
                break;
        }
        if (debugger.ipCor6traf) {
            logger.debug("rx " + pck.IPsrc + " -> " + pck.IPtrg + " pr=" + pck.IPprt + " tos=" + pck.IPtos);
        }
        return false;
    }

    public void createIPheader(packHolder pck) {
        if (pck.IPttl <= 0) {
            pck.IPttl = sendingTTL;
        }
        if (pck.IPtos < 0) {
            pck.IPtos = sendingTOS;
        }
        if (pck.IPid < 0) {
            pck.IPid = (pck.UDPsrc ^ pck.UDPtrg ^ pck.IPprt) & 0xffff;
        }
        if (debugger.ipCor6traf) {
            logger.debug("tx " + pck.IPsrc + " -> " + pck.IPtrg + " pr=" + pck.IPprt + " tos=" + pck.IPtos);
        }
        pck.IPsiz = size;
        int oldPrt = pck.IPprt;
        if (pck.IPalrt != -1) {
            pck.msbPutW(pck.IPsiz + 0, oldPrt << 8);
            pck.msbPutW(pck.IPsiz + 2, 0x0502);
            pck.msbPutW(pck.IPsiz + 4, pck.IPalrt);
            pck.msbPutW(pck.IPsiz + 6, 0x0100);
            pck.IPsiz += 8;
            oldPrt = exthdrHopByHop;
        }
        if (pck.IPmf || (pck.IPfrg > 0)) {
            pck.msbPutW(pck.IPsiz + 0, oldPrt << 8);
            int i = pck.IPfrg & 0xfff8;
            if (pck.IPmf) {
                i |= 1;
            }
            pck.msbPutW(pck.IPsiz + 2, i);
            pck.msbPutD(pck.IPsiz + 4, pck.IPid);
            pck.IPsiz += 8;
            oldPrt = exthdrFragment;
        }
        pck.msbPutW(0, 0x6000 | ((pck.IPtos & 0xff) << 4) | ((pck.IPid >>> 16) & 0xf)); // version:4 tos:8 reserved:4
        pck.msbPutW(2, pck.IPid); // flow label
        pck.msbPutW(4, pck.dataSize() + pck.IPsiz - size); // total length
        pck.putByte(6, oldPrt); // next header
        pck.putByte(7, pck.IPttl); // hop limit
        addrIPv6 adr = pck.IPsrc.toIPv6();
        pck.putAddr(8, adr); // source address
        checkAddrSrc(pck, adr);
        adr = pck.IPtrg.toIPv6();
        pck.putAddr(24, adr); // destination address
        checkAddrTrg(pck, adr);
        pck.IPver = protocolVersion;
        pck.IPmf = false;
        pck.IPfrg = 0;
        pck.putSkip(pck.IPsiz);
        pck.merge2beg();
    }

    private static void checkAddrSrc(packHolder pck, addrIPv6 adr) {
        pck.IPlnk = adr.isLinkLocal();
    }

    private static void checkAddrTrg(packHolder pck, addrIPv6 adr) {
        pck.IPbrd = adr.isBroadcast();
        pck.IPmlt = adr.isMulticast();
        pck.IPmlr = adr.isRoutedMcast();
        pck.IPlnk |= adr.isLinkLocal();
    }

    public void updateIPheader(packHolder pck, addrIP src, addrIP trg, int prt, int ttl, int tos, int id, int len) {
        if (debugger.ipCor6traf) {
            logger.debug("upd src=" + src + " trg=" + trg + " prt=" + prt + " ttl=" + ttl + " tos=" + tos + " len=" + len);
        }
        int verTos = pck.msbGetW(0); // version:4 tos:8 reserved:4
        pck.unMergeBytes(size);
        pck.putSkip(-size);
        if (prt != -1) {
            pck.putByte(6, prt); // next header
            pck.IPprt = prt;
        }
        if (ttl == -2) {
            int i = pck.IPttl - 1;
            if (i < 0) {
                i = 0;
            }
            ttl = i;
        }
        if (ttl != -1) {
            pck.putByte(7, ttl); // time to live
            pck.IPttl = ttl;
        }
        if (tos != -1) {
            verTos = (verTos & 0xf00f) | ((tos & 0xff) << 4);
            pck.msbPutW(0, verTos);
            pck.IPtos = tos;
        }
        if (id != -1) {
            verTos = (verTos & 0xfff0) | ((pck.IPid >>> 16) & 0xf);
            pck.msbPutW(0, verTos);
            pck.msbPutW(2, id); // flow label
            pck.IPid = id;
        }
        if (len != -1) {
            pck.msbPutW(4, len); // total length of this packet (hdr excl)
        }
        if (src != null) {
            addrIPv6 adr = src.toIPv6();
            pck.putAddr(8, adr); // source address
            pck.IPsrc.setAddr(src);
            checkAddrSrc(pck, adr);
        } else {
            checkAddrSrc(pck, pck.IPsrc.toIPv6());
        }
        if (trg != null) {
            addrIPv6 adr = trg.toIPv6();
            pck.putAddr(24, adr); // destination address
            pck.IPtrg.setAddr(trg);
            checkAddrTrg(pck, adr);
        } else {
            checkAddrTrg(pck, pck.IPtrg.toIPv6());
        }
        pck.putSkip(size);
        pck.mergeHeader(-1, pck.headSize() - size);
    }

}
