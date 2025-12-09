package org.freertr.prt;

import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrIPv6;
import org.freertr.cfg.cfgAll;
import org.freertr.cry.cryHashGeneric;
import org.freertr.cry.cryHashHmac;
import org.freertr.cry.cryHashMd5;
import org.freertr.cry.cryHashSha1;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.pack.packHolder;
import org.freertr.util.bits;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.state;
import org.freertr.enc.encTlv;

/**
 * handle tcp (rfc793) connections
 *
 * @author matecsaba
 */
public class prtTcp extends prtGen {

    /**
     * size of tcp header
     */
    public final static int size = 20;

    /**
     * protocol number of tcp
     */
    public final static int protoNum = 6;

    /**
     * no more data from sender
     */
    public final static int flagFIN = 0x01;

    /**
     * synchronize sequence numbers
     */
    public final static int flagSYN = 0x02;

    /**
     * reset the connection
     */
    public final static int flagRST = 0x04;

    /**
     * push function
     */
    public final static int flagPSH = 0x08;

    /**
     * acknowledgment field significant
     */
    public final static int flagACK = 0x10;

    /**
     * urgent pointer field significant
     */
    public final static int flagURG = 0x20;

    /**
     * explicit congestion experienced
     */
    public final static int flagECE = 0x40;

    /**
     * congestion window reduced
     */
    public final static int flagCWR = 0x80;

    /**
     * congestion nonce sum
     */
    public final static int flagNS = 0x100;

    /**
     * syn ack
     */
    public final static int flagSynAck = flagSYN | flagACK;

    /**
     * fin ack
     */
    public final static int flagFinAck = flagFIN | flagACK;

    /**
     * rst ack
     */
    public final static int flagRstAck = flagRST | flagACK;

    /**
     * psh ack
     */
    public final static int flagPshAck = flagPSH | flagACK;

    /**
     * syn fin rst this is a bad combination
     */
    public final static int flagSynFinRst = flagSYN | flagFIN | flagRST;

    /**
     * syn fin rst ack this is a bad combination
     */
    public final static int flagSynFinRstAck = flagSYN | flagFIN | flagRST | flagACK;

    /**
     * syn fin this is a bad combination
     */
    public final static int flagSynFin = flagSYN | flagFIN;

    /**
     * syn rst this is a bad combination
     */
    public final static int flagSynRst = flagSYN | flagRST;

    /**
     * fin rst this is a bad combination
     */
    public final static int flagFinRst = flagFIN | flagRST;

    private final static int winSizMin = 512;

    private final static int winSizMax = 49152;

    private final static int maxSegMax = 32768;

    private final static int pshNetOut = 16384;

    /**
     * create new instance
     *
     * @param ifw forwarder to use
     */
    public prtTcp(ipFwd ifw) {
        fwdCore = ifw;
        ifw.protoAdd(this, null, null);
    }

    /**
     * create new instance
     *
     */
    public prtTcp() {
    }

    /**
     * decode flags
     *
     * @param i flag value
     * @return flag in string
     */
    public static String decodeFlags(int i) {
        return bits.bit2str(i, flagFIN, "fin") + " " + bits.bit2str(i, flagSYN, "syn") + " "
                + bits.bit2str(i, flagRST, "rst") + " " + bits.bit2str(i, flagACK, "ack") + " " + bits.bit2str(i, flagPSH, "psh")
                + " " + bits.bit2str(i, flagURG, "urg") + " " + bits.bit2str(i, flagECE, "ece") + " "
                + bits.bit2str(i, flagCWR, "cwr") + " " + bits.bit2str(i, flagNS, "ns");
    }

    private static encTlv getTCPoption(packHolder pck) {
        encTlv tlv = new encTlv(0, 8, 8, 8, 1, 2, 2, 1, 0, 512, true);
        if (pck == null) {
            return tlv;
        }
        int i = pck.getByte(0);
        switch (i) {
            case 0x00:
            case 0x01:
                tlv.valTyp = i;
                tlv.valSiz = 0;
                pck.getSkip(1);
                break;
            default:
                if (tlv.getBytes(pck)) {
                    return null;
                }
                break;
        }
        return tlv;
    }

    private static boolean parseTCPoptions(packHolder pck, int max) {
        for (; pck.dataSize() > max;) {
            encTlv tlv = getTCPoption(pck);
            if (tlv == null) {
                break;
            }
            if (debugger.prtTcpTraf) {
                logger.debug("option " + tlv.valTyp);
            }
            switch (tlv.valTyp) {
                case 0x00: // end of option list
                    return false;
                case 0x01: // no operation
                    continue;
                case 0x02: // maximum segment size
                    pck.TCPmss = bits.msbGetW(tlv.valDat, 0);
                    continue;
                case 0x03: // window scale
                    pck.TCPwsc = tlv.valDat[0] & 0xff;
                    continue;
                case 0x04: // sack permitted
                    pck.TCPsak = -1;
                    continue;
                case 0x05: // sack option
                    pck.TCPsak = bits.msbGetD(tlv.valDat, 0) - pck.TCPack;
                    continue;
                case 0x08: // timestamps
                    pck.TCPtsV = bits.msbGetD(tlv.valDat, 0);
                    pck.TCPtsE = bits.msbGetD(tlv.valDat, 4);
                    continue;
                case 0x13: // md5 auth
                case 0x1d: // auth opt
                    pck.TCPaut = pck.dataSize() - max + tlv.valSiz;
                    continue;
                default:
                    continue;
            }
        }
        return false;
    }

    private static void replaceMSSoption(packHolder pck) {
        for (;;) {
            encTlv tlv = getTCPoption(pck);
            if (tlv == null) {
                break;
            }
            switch (tlv.valTyp) {
                case 0x00: // end of option list
                    pck.getSkip(-1);
                    return;
                case 0x01: // no operation
                    pck.putByte(0, 1);
                    pck.putSkip(1);
                    continue;
                case 0x02: // maximum segment size
                    if (pck.TCPmss < bits.msbGetW(tlv.valDat, 0)) {
                        bits.msbPutW(tlv.valDat, 0, pck.TCPmss);
                    }
                    tlv.putThis(pck);
                    continue;
                default:
                    tlv.putThis(pck);
                    continue;
            }
        }
    }

    private static int regulateMss(int i) {
        if (i < cfgAll.tcpSegmentMin) {
            i = cfgAll.tcpSegmentMin;
        }
        if (i > cfgAll.tcpSegmentMax) {
            i = cfgAll.tcpSegmentMax;
        }
        return i;
    }

    /**
     * parse tcp header
     *
     * @param pck packet to parse
     */
    public static void parseTCPports(packHolder pck) {
        pck.UDPsrc = pck.msbGetW(0); // source port
        pck.UDPtrg = pck.msbGetW(2); // target port
        pck.TCPflg = pck.msbGetW(12); // dataofs:4 flags:12
        pck.UDPsiz = (pck.TCPflg & 0xf000) >>> 10;
        pck.TCPflg &= 0xfff;
    }

    /**
     * parse tcp header
     *
     * @param pck packet to parse
     * @return false if successful, true if error happened
     */
    public static boolean parseTCPheader(packHolder pck) {
        parseTCPports(pck);
        if (pck.UDPsiz < size) {
            logger.info("got too small from " + pck.IPsrc);
            return true;
        }
        if (pck.UDPsiz > pck.dataSize()) {
            logger.info("got truncated from " + pck.IPsrc);
            return true;
        }
        if (cfgAll.tcpChecksumRx) {
            int i = pck.pseudoIPsum(pck.dataSize());
            if (pck.getIPsum(0, pck.dataSize(), i) != 0xffff) { // sum
                logger.info("got bad checksum from " + pck.IPsrc);
                return true;
            }
        }
        pck.TCPseq = pck.msbGetD(4); // sequence number
        pck.TCPack = pck.msbGetD(8); // acknowledgment number
        pck.TCPwin = pck.msbGetW(14); // window size
        // int sum = pck.msbGetW(16); // checksum
        pck.TCPurg = pck.msbGetW(18); // urgent pointer
        pck.getSkip(size);
        int hdrSiz = pck.UDPsiz - size;
        if (debugger.prtTcpTraf) {
            logger.debug("rx " + pck.UDPsrc + " -> " + pck.UDPtrg + " " + decodeFlags(pck.TCPflg) + " seq=" + pck.TCPseq
                    + " data=" + (pck.dataSize() - hdrSiz) + " ack=" + pck.TCPack + " sack=" + pck.TCPsak);
        }
        if (hdrSiz < 1) {
            return false;
        }
        int i = pck.dataSize();
        boolean b = parseTCPoptions(pck, i - hdrSiz);
        pck.setBytesLeft(i - hdrSiz);
        return b;
    }

    /**
     * update tcp header
     *
     * @param pck packet to work with
     * @param src source port
     * @param trg target port
     * @param flg flags
     * @param win window size
     * @param mss maximum segment size
     */
    public static void updateTCPheader(packHolder pck, int src, int trg, int flg, int win, int mss) {
        pck.unMergeBytes(size);
        pck.putSkip(-size);
        int optSiz = 0;
        if (mss >= 0) {
            pck.TCPmss = mss;
            pck.putSkip(size);
            replaceMSSoption(pck);
            optSiz = pck.dataSize();
            pck.getSkip(optSiz);
            pck.putFill(0, optSiz, 0);
            pck.putSkip(optSiz);
            optSiz = pck.headSize() - size;
            pck.putSkip(-optSiz - size);
        }
        if (src >= 0) {
            pck.msbPutW(0, src); // source port
            pck.UDPsrc = src;
        }
        if (trg >= 0) {
            pck.msbPutW(2, trg); // target port
            pck.UDPtrg = trg;
        }
        if (flg >= 0) {
            pck.msbPutW(12, flg); // dataofs:4 flags:12
            pck.TCPflg = flg;
        }
        if (win >= 0) {
            pck.msbPutW(14, win); // window size
            pck.TCPwin = win;
        }
        pck.msbPutW(16, 0); // checksum
        if (cfgAll.tcpChecksumTx) {
            int i = pck.pseudoIPsum(size + optSiz + pck.dataSize());
            i = pck.putIPsum(0, size + optSiz, i);
            i = pck.getIPsum(0, pck.dataSize(), i);
            pck.lsbPutW(16, 0xffff - i); // checksum
        }
        pck.UDPsiz = size + optSiz;
        pck.putSkip(size + optSiz);
        pck.merge2beg();
    }

    private static void hashOneAddress(cryHashGeneric md, addrIP adr) {
        byte[] buf;
        if (adr.isIPv4()) {
            buf = new byte[addrIPv4.size];
            addrIPv4 adr4 = adr.toIPv4();
            adr4.toBuffer(buf, 0);
        } else {
            buf = new byte[addrIPv6.size];
            addrIPv6 adr6 = adr.toIPv6();
            adr6.toBuffer(buf, 0);
        }
        md.update(buf);
    }

    private static void hashPseudoHdr(cryHashGeneric md, packHolder pck) {
        hashOneAddress(md, pck.IPsrc);
        hashOneAddress(md, pck.IPtrg);
        byte[] buf;
        if (pck.IPsrc.isIPv4()) {
            buf = new byte[4];
            bits.msbPutW(buf, 0, protoNum);
            bits.msbPutW(buf, 2, pck.dataSize() + pck.UDPsiz);
        } else {
            buf = new byte[8];
            bits.msbPutD(buf, 0, pck.dataSize() + pck.UDPsiz);
            bits.msbPutD(buf, 4, protoNum);
        }
        md.update(buf);
    }

    private static byte[] getTCPkdfRng(addrIP sa, addrIP ta, int sp, int tp, int kid, String pwd, int si, int ti) {
        if (pwd == null) {
            return null;
        }
        if (kid < 0) {
            return null;
        }
        cryHashHmac h = new cryHashHmac(new cryHashSha1(), pwd.getBytes());
        h.init();
        h.update(1); // i
        h.update("TCP-AO".getBytes()); // label
        hashOneAddress(h, sa);
        hashOneAddress(h, ta);
        byte[] buf = new byte[4];
        bits.msbPutW(buf, 0, sp);
        bits.msbPutW(buf, 2, tp);
        h.update(buf); // src-trg ports
        bits.msbPutD(buf, 0, si);
        h.update(buf); // src isn
        bits.msbPutD(buf, 0, ti);
        h.update(buf); // trg isn
        h.update(0); // length
        h.update(160); // length
        buf = h.finish();
        return buf;
    }

    private static byte[] getTCPpassword(packHolder pck, int kid, String pwd, byte[] tky) {
        if (kid < 0) {
            cryHashMd5 h = new cryHashMd5();
            h.init();
            hashPseudoHdr(h, pck);
            pck.hashHead(h, 0, size);
            pck.hashData(h, 0, pck.dataSize());
            h.update(pwd.getBytes());
            return h.finish();
        }
        if (tky == null) {
            return null;
        }
        cryHashHmac h = new cryHashHmac(new cryHashSha1(), tky);
        h.init();
        h.update(new byte[4]); // sne
        hashPseudoHdr(h, pck);
        pck.hashHead(h, 0, pck.UDPsiz - pck.TCPaut); // tcp header
        h.update(kid); // key id
        h.update(kid); // key id
        h.update(new byte[12]); // tcp-ao mac
        pck.hashHead(h, pck.UDPsiz - pck.TCPaut + 14, pck.TCPaut - 14); // tcp footer
        pck.hashData(h, 0, pck.dataSize()); // payload
        byte[] buf = h.finish();
        byte[] res = new byte[14];
        res[0] = (byte) kid;
        res[1] = (byte) kid;
        bits.byteCopy(buf, 0, res, 2, 12);
        return res;
    }

    /**
     * create tcp header
     *
     * @param pck packet to update
     * @param kid key id, if applicable
     * @param pwd password
     * @param tky traffic key
     */
    public static void createTCPheader(packHolder pck, int kid, String pwd, byte[] tky) {
        pck.IPprt = protoNum;
        if (debugger.prtTcpTraf) {
            logger.debug("tx " + pck.UDPsrc + " -> " + pck.UDPtrg + " " + decodeFlags(pck.TCPflg) + " seq=" + pck.TCPseq
                    + " data=" + pck.dataSize() + " ack=" + pck.TCPack + " sack=" + pck.TCPsak + " pwd=" + pwd);
        }
        if (pck.TCPmss > 0) {
            encTlv tlv = getTCPoption(null);
            bits.msbPutW(tlv.valDat, 0, pck.TCPmss);
            tlv.putBytes(pck, 2, 2, tlv.valDat); // mss
        }
        if (pck.TCPtsV != 0) {
            pck.putByte(0, 1); // nop
            pck.putByte(1, 1); // nop
            pck.putSkip(2);
            encTlv tlv = getTCPoption(null);
            bits.msbPutD(tlv.valDat, 0, pck.TCPtsV);
            bits.msbPutD(tlv.valDat, 4, pck.TCPtsE);
            tlv.putBytes(pck, 8, 8, tlv.valDat); // timestamp
        }
        if (pck.TCPsak != 0) {
            pck.putByte(0, 1); // nop
            pck.putByte(1, 1); // nop
            pck.putSkip(2);
            encTlv tlv = getTCPoption(null);
            if (pck.TCPsak == -1) {
                tlv.putBytes(pck, 4, 0, tlv.valDat);
            } else {
                bits.msbPutD(tlv.valDat, 0, pck.TCPsak + pck.TCPack);
                bits.msbPutD(tlv.valDat, 4, pck.TCPsak + pck.TCPack);
                tlv.putBytes(pck, 5, 8, tlv.valDat);
            }
        }
        if (pck.TCPwsc > 0) {
            encTlv tlv = getTCPoption(null);
            tlv.valDat[0] = (byte) pck.TCPwsc;
            tlv.putBytes(pck, 3, 1, tlv.valDat); // win scale
            pck.putByte(0, 1); // nop
            pck.putSkip(1);
        }
        if (pwd != null) {
            encTlv tlv = getTCPoption(null);
            if (kid < 0) {
                pck.putByte(0, 1); // nop
                pck.putByte(1, 1); // nop
                pck.putSkip(2);
                tlv.putBytes(pck, 19, 16, tlv.valDat); // md5 auth
                pck.TCPaut = 16;
            } else {
                tlv.putBytes(pck, 29, 14, tlv.valDat); // auth opt
                pck.TCPaut = 14;
            }
        }
        int hdrSiz = size + pck.headSize();
        pck.merge2beg();
        pck.msbPutW(0, pck.UDPsrc); // source port
        pck.msbPutW(2, pck.UDPtrg); // target port
        pck.msbPutD(4, pck.TCPseq); // sequence number
        pck.msbPutD(8, pck.TCPack); // acknowledgment number
        pck.msbPutW(12, (pck.TCPflg & 0xfff) | (hdrSiz << 10)); // flags
        pck.msbPutW(14, pck.TCPwin); // window size
        pck.msbPutW(16, 0); // checksum
        pck.msbPutW(18, pck.TCPurg); // urgent pointer
        if (cfgAll.tcpChecksumTx) {
            int i = pck.pseudoIPsum(size + pck.dataSize());
            i = pck.putIPsum(0, size, i);
            i = pck.getIPsum(0, pck.dataSize(), i);
            pck.lsbPutW(16, 0xffff - i); // checksum
        }
        pck.putSkip(size);
        pck.merge2beg();
        pck.UDPsiz = hdrSiz;
        if (pwd == null) {
            return;
        }
        pck.unMergeBytes(hdrSiz);
        pck.putSkip(-hdrSiz);
        pck.msbPutW(16, 0); // checksum
        byte[] buf = getTCPpassword(pck, kid, pwd, tky);
        if (buf != null) {
            pck.putCopy(buf, 0, hdrSiz - buf.length, buf.length);
        }
        if (cfgAll.tcpChecksumTx) {
            int i = pck.pseudoIPsum(hdrSiz + pck.dataSize());
            i = pck.putIPsum(0, hdrSiz, i);
            i = pck.getIPsum(0, pck.dataSize(), i);
            pck.lsbPutW(16, 0xffff - i); // checksum
        }
        pck.putSkip(hdrSiz);
        pck.merge2beg();
    }

    /**
     * send back a reset
     *
     * @param src source packet (won't be touched)
     * @param ifc interface to use
     */
    public void sendResetBack(packHolder src, ipFwdIface ifc) {
        if ((src.TCPflg & flagRST) != 0) {
            return;
        }
        packHolder pck = new packHolder(true, true);
        pck.clear();
        pck.TCPmss = 0;
        pck.TCPwsc = 0;
        pck.TCPtsE = 0;
        pck.TCPtsV = 0;
        pck.TCPsak = 0;
        pck.UDPsrc = src.UDPtrg;
        pck.UDPtrg = src.UDPsrc;
        pck.TCPseq = src.TCPack;
        pck.TCPack = src.TCPseq;
        pck.TCPflg = flagRstAck;
        pck.TCPwin = 1024;
        pck.TCPurg = 0;
        pck.IPsrc.setAddr(src.IPtrg);
        pck.IPtrg.setAddr(src.IPsrc);
        pck.IPdf = false;
        pck.IPfrg = 0;
        pck.IPalrt = -1;
        pck.IPttl = -1;
        pck.IPtos = -1;
        pck.IPid = -1;
        if ((src.TCPflg & flagSYN) != 0) {
            pck.TCPack++;
        }
        if ((src.TCPflg & flagFIN) != 0) {
            pck.TCPack++;
        }
        createTCPheader(pck, -1, null, null);
        fwdCore.protoPack(ifc, null, pck);
    }

    private int sendMyPacket(prtGenConn clnt, int flg, int datSiz, int sack) {
        prtTcpConn pr = (prtTcpConn) clnt.protoDat;
        packHolder pck = new packHolder(true, true);
        synchronized (pr.lck) {
            pck.clear();
            if (datSiz > 0) {
                byte[] buf = new byte[pr.netOut + datSiz];
                datSiz = clnt.pipeNetwork.nonDestructiveGet(buf, 0, pr.netOut + datSiz) - pr.netOut;
                if (datSiz < 1) {
                    return 0;
                }
                pck.putCopy(buf, pr.netOut, 0, datSiz);
                pck.putSkip(datSiz);
                pck.merge2beg();
            }
            pck.TCPmss = 0;
            pck.TCPwsc = 0;
            pck.UDPsrc = clnt.portLoc;
            pck.UDPtrg = clnt.portRem;
            pck.TCPseq = pr.seqLoc + pr.netOut;
            pck.TCPack = pr.seqRem;
            pck.TCPflg = flg;
            pck.TCPsak = sack;
            if (datSiz < 0) {
                pck.TCPseq--;
                datSiz = 0;
            }
            int i = clnt.freeAtServer() - 32;
            if (i < winSizMin) {
                i = winSizMin;
            }
            if (i > winSizMax) {
                i = winSizMax;
            }
            pck.TCPwin = i >>> cfgAll.tcpWinScale;
            pck.TCPurg = 0;
            if (pr.tmstmpTx != 0) {
                pck.TCPtsE = pr.tmstmpRx;
                pck.TCPtsV = pr.tmstmpTx + (int) bits.getTime();
            }
            pck.IPsrc.setAddr(clnt.iface.addr);
            pck.IPtrg.setAddr(clnt.peerAddr);
            pck.IPdf = false;
            pck.IPfrg = 0;
            pck.IPalrt = -1;
            pck.IPttl = clnt.sendTTL;
            pck.IPtos = clnt.sendTOS;
            if (pr.ecnTx) {
                if (pck.IPtos < 0) {
                    pck.IPtos = 0;
                }
                pck.IPtos = (pck.IPtos & 0xfc) | 0x02;
                if (pr.ecnRx) {
                    pck.TCPflg |= flagECE;
                }
                if (pr.ecnRe) {
                    pck.TCPflg |= flagCWR;
                }
            }
            pck.IPdf = clnt.sendDFN == 1;
            pck.IPid = clnt.sendFLW;
            if ((flg & flagSYN) != 0) {
                pck.TCPseq--;
                pck.TCPmss = cfgAll.tcpSegmentMax;
                pck.TCPwsc = cfgAll.tcpWinScale;
            }
            pr.netOut += datSiz;
        }
        createTCPheader(pck, clnt.keyId, clnt.passwd, pr.trfKtx);
        fwdCore.protoPack(clnt.iface, null, pck);
        return datSiz;
    }

    public String toString() {
        return "tcp on " + fwdCore;
    }

    /**
     * get protocol number
     *
     * @return number
     */
    public int getProtoNum() {
        return protoNum;
    }

    protected boolean testPortNumber(int i) {
        if (i < 1) {
            return true;
        }
        if (i > 65535) {
            return true;
        }
        return false;
    }

    protected int getRandomPortNum() {
        return bits.random(cfgAll.tcpRangeMin, cfgAll.tcpRangeMax);
    }

    /**
     * start connection
     *
     * @param clnt client
     * @param pck packet
     * @param res resume
     * @return false if success, true if error
     */
    protected boolean connectionStart(prtGenConn clnt, packHolder pck, boolean res) {
        if (debugger.prtTcpTraf) {
            logger.debug("start");
        }
        clnt.sendPRT = protoNum;
        prtTcpConn pr = new prtTcpConn();
        pr.netMax = cfgAll.tcpSegmentMin;
        clnt.protoDat = pr;
        clnt.timeout = cfgAll.tcpTimeSyn;
        clnt.workInterval = cfgAll.tcpTimeWork;
        pr.activWait = cfgAll.tcpTimeNow;
        pr.activFrcd = true;
        pr.seqLoc = bits.randomD();
        if (cfgAll.tcpTimStmp) {
            pr.tmstmpTx = 1 + bits.randomD();
        }
        pr.staTim = bits.getTime();
        if (pck == null) {
            if (res) {
                pr.sackTx = cfgAll.tcpSack;
                pr.state = prtTcpConn.stResReq;
                clnt.timeout = cfgAll.tcpTimeOpen;
                return false;
            }
            pr.trfKtx = getTCPkdfRng(clnt.iface.addr, clnt.peerAddr, clnt.portLoc, clnt.portRem, clnt.keyId, clnt.passwd, pr.seqLoc - 1, 0);
            pr.state = prtTcpConn.stConReq;
            return false;
        }
        pr.trfKrx = getTCPkdfRng(clnt.peerAddr, clnt.iface.addr, clnt.portRem, clnt.portLoc, clnt.keyId, clnt.passwd, pck.TCPseq, 0);
        pr.trfKtx = getTCPkdfRng(clnt.iface.addr, clnt.peerAddr, clnt.portLoc, clnt.portRem, clnt.keyId, clnt.passwd, pr.seqLoc - 1, pck.TCPseq);
        pr.trfKfx = getTCPkdfRng(clnt.peerAddr, clnt.iface.addr, clnt.portRem, clnt.portLoc, clnt.keyId, clnt.passwd, pck.TCPseq, pr.seqLoc - 1);
        if (pck.TCPtsV == 0) {
            pr.tmstmpTx = 0;
        }
        pr.segSiz = regulateMss(pck.TCPmss);
        pr.netMax = pr.segSiz;
        pr.ecnTx = cfgAll.tcpEcn && ((pck.TCPflg & flagECE) != 0);
        pr.sackTx = cfgAll.tcpSack && (pck.TCPsak == -1);
        pr.state = prtTcpConn.stGotSyn;
        pr.seqRem = pck.TCPseq + 1;
        if ((pck.TCPflg & flagSynFinRst) == flagSYN) {
            return false;
        }
        sendResetBack(pck, clnt.iface);
        return true;
    }

    protected void connectionRefuse(ipFwdIface ifc, packHolder pck) {
        if (debugger.prtTcpTraf) {
            logger.debug("refuse");
        }
        sendResetBack(pck, ifc);
    }

    /**
     * close connection
     *
     * @param clnt client
     */
    protected void connectionClose(prtGenConn clnt) {
        prtTcpConn pr = (prtTcpConn) clnt.protoDat;
        if (debugger.prtTcpTraf) {
            logger.debug("close");
        }
        clnt.pipeClient.setClose();
        clnt.pipeNetwork.setClose();
        if ((clnt.pipeNetwork.ready2rx() < 1) && (pr.state == prtTcpConn.stOpened)) {
            pr.state = prtTcpConn.stClrReq;
            pr.staTim = bits.getTime();
        }
        clnt.timeout = cfgAll.tcpTimeClose;
        pr.activWait = cfgAll.tcpTimeNow;
        pr.activFrcd = true;
    }

    /**
     * bytes available
     *
     * @param ntry connection
     * @return bytes
     */
    protected int connectionBytes(prtGenConn ntry) {
        int i = ntry.pipeClient.ready2tx();
        if (i > 512) {
            i = 512;
        }
        return i;
    }

    /**
     * send packet
     *
     * @param clnt client
     * @param pck packet
     * @return false if success, true if error
     */
    protected boolean connectionSend(prtGenConn clnt, packHolder pck) {
        int len = pck.dataSize();
        return pck.pipeSend(clnt.pipeClient, 0, len, 1) != len;
    }

    private static boolean spoofCheck(int i) {
        return (i < -winSizMax) || (i > winSizMax);
    }

    /**
     * received packet
     *
     * @param clnt client
     * @param pck packet
     */
    protected void connectionRcvd(prtGenConn clnt, packHolder pck) {
        prtTcpConn pr = (prtTcpConn) clnt.protoDat;
        synchronized (pr.lck) {
            if (pr.state == prtTcpConn.stResReq) {
                if ((pck.TCPflg & flagSynAck) == 0) {
                    return;
                }
                if (debugger.prtTcpTraf) {
                    logger.debug("resumed");
                }
                pr.seqLoc = pck.TCPack;
                pr.seqRem = pck.TCPseq;
                pr.trfKrx = getTCPkdfRng(clnt.peerAddr, clnt.iface.addr, clnt.portRem, clnt.portLoc, clnt.keyId, clnt.passwd, pr.seqRem, pr.seqLoc);
                pr.trfKtx = getTCPkdfRng(clnt.iface.addr, clnt.peerAddr, clnt.portLoc, clnt.portRem, clnt.keyId, clnt.passwd, pr.seqLoc, pr.seqRem);
                pr.segSiz = regulateMss(pck.TCPmss);
                pr.netMax = pr.segSiz;
                pr.state = prtTcpConn.stOpened;
                pr.staTim = bits.getTime();
                pr.activWait = cfgAll.tcpTimeNow;
                pr.activFrcd = true;
                clnt.setReady();
                clnt.timeout = cfgAll.tcpTimeOpen;
            }
            if (clnt.passwd != null) {
                if (pck.TCPaut < 0) {
                    if ((pr.state != prtTcpConn.stConReq) && ((pck.TCPflg & flagRST) == 0)) {
                        logger.info("got missing authentication " + clnt);
                        return;
                    }
                } else {
                    if (pr.state == prtTcpConn.stConReq) {
                        pr.trfKrx = getTCPkdfRng(clnt.peerAddr, clnt.iface.addr, clnt.portRem, clnt.portLoc, clnt.keyId, clnt.passwd, pck.TCPseq, pr.seqLoc - 1);
                        pr.trfKfx = getTCPkdfRng(clnt.iface.addr, clnt.peerAddr, clnt.portLoc, clnt.portRem, clnt.keyId, clnt.passwd, pr.seqLoc - 1, pck.TCPseq);
                    }
                    pck.getSkip(-pck.UDPsiz);
                    pck.unMergeBytes(pck.UDPsiz);
                    pck.putSkip(-pck.UDPsiz);
                    pck.msbPutW(16, 0); // checksum
                    byte[] buf1 = getTCPpassword(pck, clnt.keyId, clnt.passwd, pr.trfKrx);
                    if (buf1 == null) {
                        logger.info("got invalid authentication state " + clnt);
                        return;
                    }
                    if (pck.getByte(-pck.TCPaut - 1) != (buf1.length + 2)) {
                        logger.info("got invalid authentication size " + clnt);
                        return;
                    }
                    byte[] buf2 = new byte[buf1.length];
                    pck.getCopy(buf2, 0, -pck.TCPaut, buf1.length);
                    if (bits.byteComp(buf1, 0, buf2, 0, buf1.length) != 0) {
                        logger.info("got invalid authentication hash " + clnt);
                        return;
                    }
                }
            } else {
                if (pck.TCPaut >= 0) {
                    logger.info("got unwanted authentication " + clnt);
                    return;
                }
            }
            int nowAcked = pck.TCPack - pr.seqLoc; // bytes acked from tx buffer
            int oldBytes = pr.seqRem - pck.TCPseq; // old bytes arrived from past
            int packSize = pck.dataSize(); // bytes in packet
            int newBytes = packSize - oldBytes; // new bytes in packet
            int flg = pck.TCPflg & flagSynFinRstAck;
            if (pr.tmstmpTx != 0) {
                pr.tmstmpRx = pck.TCPtsV;
            }
            if (pr.ecnTx) {
                pr.ecnRx &= (pck.TCPflg & flagCWR) == 0;
                pr.ecnRx |= (pck.IPtos & 3) == 3;
                pr.ecnRe = (pck.TCPflg & flagECE) != 0;
            }
            if ((flg & flagSynFin) == flagSynFin) {
                logger.info("got both syn fin " + clnt);
                return;
            }
            if (flg == flagSynAck) {
                if (pr.state != prtTcpConn.stConReq) {
                    pr.activWait = cfgAll.tcpTimeLater;
                    logger.info("got unwanted synack " + clnt);
                    return;
                }
                if (nowAcked != 0) {
                    pr.activWait = cfgAll.tcpTimeLater;
                    logger.info("bad acknowkedge number in synack " + clnt);
                    return;
                }
                pr.seqRem = pck.TCPseq + 1;
                if (debugger.prtTcpTraf) {
                    logger.debug("accepted");
                }
                if (pck.TCPtsE == 0) {
                    pr.tmstmpTx = 0;
                }
                pr.ecnTx = cfgAll.tcpEcn && ((pck.TCPflg & flagECE) != 0);
                pr.sackTx = cfgAll.tcpSack && (pck.TCPsak == -1);
                pr.segSiz = regulateMss(pck.TCPmss);
                pr.state = prtTcpConn.stOpened;
                pr.trfKtx = pr.trfKfx;
                pr.staTim = bits.getTime();
                pr.activWait = cfgAll.tcpTimeNow;
                pr.activFrcd = true;
                clnt.setReady();
                clnt.timeout = cfgAll.tcpTimeOpen;
                return;
            }
            if (spoofCheck(oldBytes)) {
                logger.info("got far sequence number " + clnt);
                return;
            }
            if (((flg & flagACK) != 0) && spoofCheck(nowAcked)) {
                logger.info("got far acknowledge number " + clnt);
                return;
            }
            if ((flg & flagRST) != 0) {
                if ((flg & flagSynFin) != 0) {
                    logger.info("got rst with other controls " + clnt);
                    return;
                }
                if (debugger.prtTcpTraf) {
                    logger.debug("got reset");
                }
                pr.state = prtTcpConn.stDelete;
                clnt.setClosing();
                clnt.deleteImmediately();
                return;
            }
            if ((flg & flagACK) != 0) {
                if (nowAcked > clnt.pipeNetwork.ready2rx()) {
                    if ((flg & flagFIN) != 0) {
                        pr.seqLoc++;
                    }
                    logger.info("got future acknowledge number " + clnt);
                    pr.activWait = cfgAll.tcpTimeLater;
                    nowAcked = 0;
                }
                if (nowAcked > 0) {
                    int i = clnt.pipeNetwork.nonBlockSkip(nowAcked);
                    if (i < 1) {
                        logger.info("net buffer underflow " + clnt);
                        return;
                    }
                    pr.seqLoc += i;
                    pr.netOut -= i;
                    if (pr.netOut < 0) {
                        pr.netOut = 0;
                    }
                    pr.netMax += pr.segSiz;
                    if (pr.netMax > maxSegMax) {
                        pr.netMax = maxSegMax;
                    }
                }
                if (pck.TCPsak > 0) {
                    pr.sackTx = true;
                    pr.netMax = pr.segSiz;
                    pr.netOut = 0;
                    pr.activFrcd = true;
                }
                if (pr.state == prtTcpConn.stGotSyn) {
                    if (debugger.prtTcpTraf) {
                        logger.debug("opened");
                    }
                    pr.state = prtTcpConn.stOpened;
                    pr.staTim = bits.getTime();
                    pr.activWait = cfgAll.tcpTimeNow;
                    pr.activFrcd = true;
                    clnt.setReady();
                    clnt.timeout = cfgAll.tcpTimeOpen;
                }
            }
            if (oldBytes < 0) {
                if (debugger.prtTcpTraf) {
                    logger.debug("got future sequence number");
                }
                pr.activWait = cfgAll.tcpTimeLater;
                if (!pr.sackTx) {
                    return;
                }
                pr.activFrcd = true;
                pr.sackRx = -oldBytes;
                return;
            }
            if (newBytes > 0) {
                if ((flg & flagFIN) != 0) {
                    if (debugger.prtTcpTraf) {
                        logger.debug("got data with fin");
                    }
                    flg = 0;
                }
                if ((flg & flagSynFin) != 0) {
                    logger.info("got data with control flags " + clnt);
                }
            }
            if ((flg & flagSynFin) == 0) {
                if (pr.state != prtTcpConn.stOpened) {
                    if (pr.state == prtTcpConn.stGotFin) {
                        clnt.setClosing();
                        clnt.deleteImmediately();
                        return;
                    }
                    logger.info("got data while not open " + clnt);
                    if (pr.state == prtTcpConn.stClrReq) {
                        if (newBytes > 0) {
                            pr.seqRem += newBytes;
                        }
                        if (!pr.seenFin) {
                            return;
                        }
                        clnt.setClosing();
                        clnt.deleteImmediately();
                        return;
                    }
                    return;
                }
                if (newBytes < 1) {
                    if (newBytes == 0) {
                        return;
                    }
                    if (debugger.prtTcpTraf) {
                        logger.debug("got old data again");
                    }
                    return;
                }
                pck.getSkip(oldBytes);
                if (clnt.send2server(pck)) {
                    if (debugger.prtTcpTraf) {
                        logger.debug("upper don't need data");
                    }
                    if (!pr.sackTx) {
                        return;
                    }
                    pr.activWait = cfgAll.tcpTimeNow;
                    pr.activFrcd = true;
                    pr.sackRx = newBytes;
                    return;
                }
                pr.seqRem += newBytes;
                pr.activFrcd = true;
                pr.sackRx = 0;
                return;
            }
            if (flg == flagSYN) {
                if (pr.state != prtTcpConn.stGotSyn) {
                    logger.info("got unwanted syn " + clnt);
                    return;
                }
                pr.trfKrx = pr.trfKfx;
                return;
            }
            if ((flg & flagFIN) != 0) {
                pr.seenFin = true;
                if (pr.state == prtTcpConn.stOpened) {
                    if (debugger.prtTcpTraf) {
                        logger.debug("closing");
                    }
                    clnt.setClosing();
                    pr.state = prtTcpConn.stGotFin;
                    pr.staTim = bits.getTime();
                    pr.activWait = cfgAll.tcpTimeNow;
                    pr.activFrcd = true;
                    pr.seqRem++;
                    clnt.timeout = cfgAll.tcpTimeFin;
                    return;
                }
                if (pr.state == prtTcpConn.stClrReq) {
                    pr.state = prtTcpConn.stDelete;
                    pr.seqRem++;
                    sendMyPacket(clnt, flagACK, 0, 0);
                    clnt.setClosing();
                    clnt.deleteImmediately();
                    return;
                }
                if (pr.state == prtTcpConn.stGotFin) {
                    return;
                }
                logger.info("got fin while not open " + clnt);
                return;
            }
            logger.info("got invalid flags " + clnt);
        }
    }

    /**
     * received error
     *
     * @param clnt client
     * @param pck packet
     * @param rtr reporting router
     * @param err error happened
     * @param lab error label
     */
    protected void connectionError(prtGenConn clnt, packHolder pck, addrIP rtr, counter.reasons err, int lab) {
        pck.getSkip(pck.UDPsiz);
        clnt.error2server(pck, rtr, err, lab);
    }

    private boolean flush2net(prtGenConn clnt) {
        prtTcpConn pr = (prtTcpConn) clnt.protoDat;
        int bufSiz = clnt.pipeNetwork.ready2rx();
        if ((!pr.activFrcd) && (bufSiz < 1)) {
            if (clnt.pipeNetwork.isClosed() == 0) {
                pr.activWait = cfgAll.tcpTimeAlive;
                return true;
            }
            if (debugger.prtTcpTraf) {
                logger.debug("closing");
            }
            clnt.timeout = cfgAll.tcpTimeFin;
            pr.activWait = cfgAll.tcpTimeNow;
            pr.activFrcd = true;
            pr.state = prtTcpConn.stClrReq;
            pr.staTim = bits.getTime();
            return false;
        }
        int sent = 0;
        if (pr.activFrcd) {
            sent = -1;
        }
        pr.activFrcd = false;
        pr.activWait = cfgAll.tcpTimeLater;
        for (;;) {
            int flg = flagPshAck;
            int snd = bufSiz - pr.netOut;
            int i = pr.netMax - pr.netOut;
            if (snd > i) {
                snd = i;
            }
            if (snd > pr.segSiz) {
                snd = pr.segSiz;
                flg = flagACK;
            }
            if (pr.netOut > pshNetOut) {
                flg = flagPshAck;
            }
            if (snd < 1) {
                break;
            }
            snd = sendMyPacket(clnt, flg, snd, pr.sackRx);
            if (snd < 1) {
                break;
            }
            sent++;
        }
        if (sent < 0) {
            sendMyPacket(clnt, flagACK, 0, pr.sackRx);
        }
        return true;
    }

    /**
     * work connection
     *
     * @param clnt client
     */
    protected void connectionWork(prtGenConn clnt) {
        prtTcpConn pr = (prtTcpConn) clnt.protoDat;
        long curTim = bits.getTime();
        if (pr.state == prtTcpConn.stOpened) {
            if (!flush2net(clnt)) {
                pr.activLast = curTim;
                return;
            }
        }
        if ((!pr.activFrcd) && ((curTim - pr.activLast) < pr.activWait)) {
            return;
        }
        pr.activWait *= 2;
        if (pr.activWait > cfgAll.tcpTimeMax) {
            pr.activWait = cfgAll.tcpTimeMax;
        }
        pr.activLast = curTim;
        pr.activFrcd = false;
        switch (pr.state) {
            case prtTcpConn.stGotSyn:
                sendMyPacket(clnt, flagSynAck | (pr.ecnTx ? flagECE : 0), 0, pr.sackTx ? -1 : 0);
                break;
            case prtTcpConn.stConReq:
                sendMyPacket(clnt, flagSYN | (cfgAll.tcpEcn ? flagECE | flagCWR : 0), 0, cfgAll.tcpSack ? -1 : 0);
                break;
            case prtTcpConn.stResReq:
                break;
            case prtTcpConn.stClrReq:
                sendMyPacket(clnt, flagFinAck, 0, 0);
                if ((curTim - pr.staTim) > cfgAll.tcpTimeAlive) {
                    pr.state = prtTcpConn.stDelete;
                }
                break;
            case prtTcpConn.stGotFin:
                sendMyPacket(clnt, flagFinAck, 0, 0);
                if ((curTim - pr.staTim) > cfgAll.tcpTimeAlive) {
                    pr.state = prtTcpConn.stDelete;
                }
                break;
            case prtTcpConn.stOpened:
                sendMyPacket(clnt, flagACK, cfgAll.tcpKeepalive ? -1 : 0, pr.sackRx);
                pr.netMax = pr.segSiz;
                pr.netOut = 0;
                break;
            case prtTcpConn.stDelete:
                clnt.deleteImmediately();
                break;
            default:
                logger.info("invalid state " + clnt);
                clnt.setClosing();
                clnt.deleteImmediately();
                break;
        }
    }

    /**
     * received packet
     *
     * @param rxIfc interface
     * @param pck packet
     */
    public void recvPack(ipFwdIface rxIfc, packHolder pck) {
        cntr.rx(pck);
        if (parseTCPheader(pck)) {
            cntr.drop(pck, counter.reasons.badHdr);
            return;
        }
        if (debugger.prtTcpTraf) {
            logger.debug("rec " + pck.UDPsrc + " -> " + pck.UDPtrg + " " + decodeFlags(pck.TCPflg));
        }
        connectionSimpleWork(rxIfc, pck);
    }

    /**
     * alert packet
     *
     * @param rxIfc interface
     * @param pck packet
     * @return false if success, true if error
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
        parseTCPports(pck);
        if (debugger.prtTcpTraf) {
            logger.debug(counter.reason2string(err) + " " + pck.UDPsrc + " -> " + pck.UDPtrg + " " + decodeFlags(pck.TCPflg));
        }
        connectionSimpleError(err, rtr, rxIfc, pck);
    }

    /**
     * set state
     *
     * @param iface interface
     * @param stat state
     */
    public void setState(ipFwdIface iface, state.states stat) {
        connectionSimpleState(iface, stat);
    }

}

class prtTcpConn {

    /**
     * deleted
     */
    public final static int stDelete = 0;

    /**
     * got syn, send syn+ack, wait ack
     */
    public final static int stGotSyn = 1;

    /**
     * connect request, send syn, wait syn+ack
     */
    public final static int stConReq = 2;

    /**
     * resume request, wait ack
     */
    public final static int stResReq = 3;

    /**
     * close request, send fin+ack, wait fin+ack
     */
    public final static int stClrReq = 4;

    /**
     * got fin, send fin+ack, wiat a bit
     */
    public final static int stGotFin = 5;

    /**
     * normal data flow, send ack
     */
    public final static int stOpened = 6;

    /**
     * locker
     */
    protected final Object lck = new Object();

    /**
     * state of connection
     */
    protected int state;

    /**
     * state time
     */
    protected long staTim;

    /**
     * last tx time
     */
    protected long activLast;

    /**
     * timeout to wait
     */
    protected long activWait;

    /**
     * must send in any way
     */
    protected boolean activFrcd;

    /**
     * local sequence number
     */
    protected int seqLoc;

    /**
     * remote sequence number
     */
    protected int seqRem;

    /**
     * maximum segment size
     */
    protected int segSiz;

    /**
     * bytes sent to remote
     */
    protected int netOut;

    /**
     * packets could send to network
     */
    protected int netMax;

    /**
     * seen fin fro peer
     */
    protected boolean seenFin;

    /**
     * timestamp base
     */
    protected int tmstmpTx;

    /**
     * timestamp received
     */
    protected int tmstmpRx;

    /**
     * ecn base
     */
    protected boolean ecnTx;

    /**
     * ecn received
     */
    protected boolean ecnRx;

    /**
     * ece received
     */
    protected boolean ecnRe;

    /**
     * sack base
     */
    protected boolean sackTx;

    /**
     * sack value
     */
    protected int sackRx;

    /**
     * receive traffic key
     */
    protected byte[] trfKrx;

    /**
     * transmit traffic key
     */
    protected byte[] trfKtx;

    /**
     * final traffic key
     */
    protected byte[] trfKfx;

    public String toString() {
        switch (state) {
            case stDelete:
                return "del";
            case stGotSyn:
                return "accpt";
            case stConReq:
                return "init";
            case stResReq:
                return "resm";
            case stClrReq:
                return "term";
            case stGotFin:
                return "stop";
            case stOpened:
                return "estab";
            default:
                return "unknown=" + state;
        }
    }

}
