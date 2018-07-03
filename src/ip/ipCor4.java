package ip;

import pack.packHolder;
import util.debugger;
import util.logger;
import addr.addrIP;
import addr.addrIPv4;
import cfg.cfgAll;

/**
 * does ipv4 (rfc791) packet handling
 *
 * @author matecsaba
 */
public class ipCor4 implements ipCor {

    /**
     * default sending ttl value
     */
    public int sendingTTL = 255;

    /**
     * default sending tos value
     */
    public int sendingTOS = 0;

    /**
     * size of ipv4 header
     */
    public final static int size = 20;

    /**
     * protocol version
     */
    public final static int protocolVersion = 4;

    /**
     * protocol nlpid
     */
    public final static int protocolNLPID = 0xcc;

    /**
     * protocol number
     */
    public final static int protocolNumber = 4;

    private static int nextPackIDval = 0;

    public int getVersion() {
        return protocolVersion;
    }

    public int getNlpid() {
        return protocolNLPID;
    }

    public int getHeaderSize() {
        return size;
    }

    public boolean parseIPheader(packHolder pck, boolean chksiz) {
        int hdrLen = pck.getByte(0); // version:4 headerLen:4
        if ((hdrLen >>> 4) != protocolVersion) {
            logger.info("got bad version from " + pck.IPsrc);
            return true;
        }
        hdrLen = (hdrLen & 0xf) << 2;
        if (hdrLen < size) {
            logger.info("got too small from " + pck.IPsrc);
            return true;
        }
        if (cfgAll.ipv4ChecksumRx) {
            if (pck.getIPsum(0, hdrLen, 0) != 0xffff) {
                logger.info("got bad checksum from " + pck.IPsrc);
                return true;
            }
        }
        int totLen = pck.msbGetW(2); // total Length of this packet (hdr incl)
        if (hdrLen > totLen) {
            logger.info("got too small from " + pck.IPsrc);
            return true;
        }
        if (chksiz) {
            if (totLen > pck.dataSize()) {
                logger.info("got truncated from " + pck.IPsrc);
                return true;
            }
            pck.setDataSize(totLen);
        }
        addrIPv4 adr = new addrIPv4();
        pck.IPtos = pck.getByte(1); // type of service: dscp:6 unused:2
        pck.IPid = pck.msbGetW(4); // identification
        int flagFrag = pck.msbGetW(6); // res:1 DF:1 MF:1 fragOff:13
        pck.IPttl = pck.getByte(8); // time to live
        pck.IPprt = pck.getByte(9); // protocol
        // int sum = pck.msbGetW(10); // header checksum
        pck.getAddr(adr, 12); // source address
        pck.IPsrc.fromIPv4addr(adr);
        pck.getAddr(adr, 16); // destination address
        pck.IPtrg.fromIPv4addr(adr);
        pck.IPbrd = adr.isBroadcast();
        pck.IPmlt = adr.isMulticast();
        pck.IPdf = ((flagFrag & 0x4000) != 0);
        pck.IPmf = ((flagFrag & 0x2000) != 0);
        pck.IPfrg = (flagFrag & 0x1fff) << 3;
        pck.IPsiz = hdrLen;
        pck.IPver = protocolVersion;
        if (hdrLen > size) {
            if (pck.msbGetW(size) == 0x9404) {
                pck.IPalrt = -2;
            }
        }
        if (debugger.ipCor4traf) {
            logger.debug("rx " + pck.IPsrc + " -> " + pck.IPtrg + " pr=" + pck.IPprt + " tos=" + pck.IPtos);
        }
        return false;
    }

    public void testIPaddress(packHolder pck, addrIP test) {
        addrIPv4 adr = test.toIPv4();
        pck.IPbrd = adr.isBroadcast();
        pck.IPmlt = adr.isMulticast();
    }

    public void createIPheader(packHolder pck) {
        if (pck.IPttl <= 0) {
            pck.IPttl = sendingTTL;
        }
        if (pck.IPtos < 0) {
            pck.IPtos = sendingTOS;
        }
        if (debugger.ipCor4traf) {
            logger.debug("tx " + pck.IPsrc + " -> " + pck.IPtrg + " pr=" + pck.IPprt + " tos=" + pck.IPtos);
        }
        pck.IPsiz = size;
        if (pck.IPalrt != -1) {
            pck.msbPutD(size, 0x94040000);
            pck.IPsiz += 4;
        }
        pck.putByte(0, 0x40 | (pck.IPsiz >>> 2)); // version:4 ihl:4
        pck.putByte(1, pck.IPtos); // type of service
        pck.msbPutW(2, pck.dataSize() + pck.IPsiz); // total length
        nextPackIDval++;
        pck.msbPutW(4, nextPackIDval); // identification
        int i;
        if (pck.IPdf) {
            i = 0x4000;
        } else {
            i = 0;
        }
        pck.msbPutW(6, i); // res:1 DF:1 MF:1 fragOff:13
        pck.putByte(8, pck.IPttl); // time to live
        pck.putByte(9, pck.IPprt); // protocol
        pck.msbPutW(10, 0); // header checksum
        addrIPv4 adr = pck.IPsrc.toIPv4();
        pck.putAddr(12, adr); // source address
        adr = pck.IPtrg.toIPv4();
        pck.putAddr(16, adr); // destination address
        if (cfgAll.ipv4ChecksumTx) {
            pck.lsbPutW(10, 0xffff - pck.putIPsum(0, pck.IPsiz, 0)); // header checksum
        }
        pck.IPbrd = adr.isBroadcast();
        pck.IPmlt = adr.isMulticast();
        pck.IPver = protocolVersion;
        pck.IPid = nextPackIDval;
        pck.IPmf = false;
        pck.IPfrg = 0;
        pck.putSkip(pck.IPsiz);
        pck.merge2beg();
    }

    public void updateIPheader(packHolder pck, addrIP src, addrIP trg, int prt, int ttl, int tos, int len) {
        if (debugger.ipCor4traf) {
            logger.debug("upd src=" + src + " trg=" + trg + " prt=" + prt + " ttl=" + ttl + " tos=" + tos + " len=" + len);
        }
        pck.unMergeBytes(pck.IPsiz);
        pck.putSkip(-pck.IPsiz);
        if (prt != -1) {
            pck.putByte(9, prt); // protocol
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
            pck.putByte(8, ttl); // time to live
            pck.IPttl = ttl;
        }
        if (tos != -1) {
            pck.putByte(1, tos); // type of service: dscp:6 unused:2
            pck.IPtos = tos;
        }
        if (len != -1) {
            pck.msbPutW(2, len + pck.IPsiz); // total length
        }
        if (src != null) {
            addrIPv4 adr = src.toIPv4();
            pck.putAddr(12, adr); // source address
            pck.IPsrc.setAddr(src);
        }
        if (trg != null) {
            addrIPv4 adr = trg.toIPv4();
            pck.putAddr(16, adr); // destination address
            pck.IPtrg.setAddr(trg);
        }
        pck.msbPutW(10, 0); // header checksum
        if (cfgAll.ipv4ChecksumTx) {
            pck.lsbPutW(10, 0xffff - pck.putIPsum(0, pck.IPsiz, 0)); // header checksum
        }
        pck.putSkip(pck.IPsiz);
        pck.mergeHeader(-1, pck.headSize() - pck.IPsiz);
    }

}
