package prt;

import addr.addrIP;
import addr.addrIPv4;
import addr.addrIPv6;
import cfg.cfgAll;
import cry.cryHashMd5;
import ip.ipFwd;
import ip.ipFwdIface;
import pack.packHolder;
import pipe.pipeLine;
import pipe.pipeSide;
import util.bits;
import util.counter;
import util.debugger;
import util.logger;
import util.state;
import util.typLenVal;

/**
 * handle tcp (rfc793) connections
 *
 * @author matecsaba
 */
public class prtTcp extends prtGen {

    /**
     * timeout while connecting
     */
    public int timeoutSyn = 30 * 1000;

    /**
     * timeout while closing
     */
    public int timeoutFin = 45 * 1000;

    /**
     * timeout while closing with data
     */
    public int timeoutClos = 120 * 1000;

    /**
     * timeout while open
     */
    public int timeoutOpen = 300 * 1000;

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

    private final static int maxSegSiz = 1024;

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
        String s = bits.bit2str(i, flagFIN, "fin") + " " + bits.bit2str(i, flagSYN, "syn") + " "
                + bits.bit2str(i, flagRST, "rst") + " " + bits.bit2str(i, flagACK, "ack") + " " + bits.bit2str(i, flagPSH, "psh")
                + " " + bits.bit2str(i, flagURG, "urg") + " " + bits.bit2str(i, flagECE, "ece") + " "
                + bits.bit2str(i, flagCWR, "cwr");
        return s;
    }

    private static typLenVal getTCPoption(packHolder pck) {
        typLenVal tlv = new typLenVal(0, 8, 8, 8, 1, 2, 2, 1, 0, 512, true);
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
            typLenVal tlv = getTCPoption(pck);
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
                default:
                    continue;
            }
        }
        return false;
    }

    private static void replaceMSSoption(packHolder pck) {
        for (;;) {
            typLenVal tlv = getTCPoption(pck);
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

    /**
     * parse tcp header
     *
     * @param pck packet to parse
     */
    public static void parseTCPports(packHolder pck) {
        pck.UDPsrc = pck.msbGetW(0); // source port
        pck.UDPtrg = pck.msbGetW(2); // target port
        pck.TCPflg = pck.msbGetW(12); // dataofs:4 flags:12
    }

    /**
     * parse tcp header
     *
     * @param pck packet to parse
     * @return false if successful, true if error happened
     */
    public static boolean parseTCPheader(packHolder pck) {
        parseTCPports(pck);
        int hdrSiz = (pck.TCPflg & 0xf000) >>> 10;
        if (hdrSiz < size) {
            logger.info("got too small from " + pck.IPsrc);
            return true;
        }
        if (hdrSiz > pck.dataSize()) {
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
        pck.UDPsiz = hdrSiz;
        hdrSiz -= size;
        if (debugger.prtTcpTraf) {
            logger.debug("rx " + pck.UDPsrc + " -> " + pck.UDPtrg + " " + decodeFlags(pck.TCPflg) + " seq=" + pck.TCPseq
                    + " data=" + (pck.dataSize() - hdrSiz) + " ack=" + pck.TCPack);
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

    private static void hashOneAddress(cryHashMd5 md, addrIP adr) {
        byte[] buf = new byte[128];
        if (adr.isIPv4()) {
            addrIPv4 adr4 = adr.toIPv4();
            adr4.toBuffer(buf, 0);
            md.update(buf, 0, addrIPv4.size);
        } else {
            addrIPv6 adr6 = adr.toIPv6();
            adr6.toBuffer(buf, 0);
            md.update(buf, 0, addrIPv6.size);
        }
    }

    private static byte[] getTCPpassword(packHolder pck, byte[] pwd) {
        int hdr = size + 20;
        if (pck.TCPmss > 0) {
            hdr += 4;
        }
        cryHashMd5 h = new cryHashMd5();
        h.init();
        hashOneAddress(h, pck.IPsrc);
        hashOneAddress(h, pck.IPtrg);
        byte[] buf;
        if (pck.IPsrc.isIPv4()) {
            buf = new byte[4];
            bits.msbPutW(buf, 0, protoNum);
            bits.msbPutW(buf, 2, pck.dataSize() + hdr);
        } else {
            buf = new byte[8];
            bits.msbPutD(buf, 0, pck.dataSize() + hdr);
            bits.msbPutD(buf, 4, protoNum);
        }
        h.update(buf);
        buf = new byte[size];
        bits.msbPutW(buf, 0, pck.UDPsrc); // source
        bits.msbPutW(buf, 2, pck.UDPtrg); // target
        bits.msbPutD(buf, 4, pck.TCPseq); // seq
        bits.msbPutD(buf, 8, pck.TCPack); // ack
        bits.msbPutW(buf, 12, (pck.TCPflg & 0xfff) | (hdr << 10)); // flags
        bits.msbPutW(buf, 14, pck.TCPwin); // window
        bits.msbPutW(buf, 16, 0); // sum
        bits.msbPutW(buf, 18, pck.TCPurg); // urgent
        h.update(buf);
        pck.hashData(h, 0, pck.dataSize());
        h.update(pwd);
        return h.finish();
    }

    /**
     * create tcp header
     *
     * @param pck packet to update
     * @param pwd password
     */
    public static void createTCPheader(packHolder pck, String pwd) {
        pck.IPprt = protoNum;
        if (debugger.prtTcpTraf) {
            logger.debug("tx " + pck.UDPsrc + " -> " + pck.UDPtrg + " " + decodeFlags(pck.TCPflg) + " seq=" + pck.TCPseq
                    + " data=" + pck.dataSize() + " ack=" + pck.TCPack + " pwd=" + pwd);
        }
        if (pwd != null) {
            typLenVal tlv = getTCPoption(null);
            byte[] buf = getTCPpassword(pck, pwd.getBytes());
            bits.byteCopy(buf, 0, tlv.valDat, 0, buf.length);
            tlv.putBytes(pck, 19, 16, tlv.valDat); // md5
        }
        if (pck.TCPmss > 0) {
            typLenVal tlv = getTCPoption(null);
            bits.msbPutW(tlv.valDat, 0, pck.TCPmss);
            tlv.putBytes(pck, 2, 2, tlv.valDat); // mss
        }
        for (; (pck.headSize() & 3) != 0;) {
            pck.putByte(0, 0);
            pck.putSkip(1);
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
        pck.UDPsiz = size;
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
        pck.IPttl = -1;
        pck.IPtos = -1;
        if ((src.TCPflg & flagSYN) != 0) {
            pck.TCPack++;
        }
        if ((src.TCPflg & flagFIN) != 0) {
            pck.TCPack++;
        }
        createTCPheader(pck, null);
        fwdCore.protoPack(ifc, pck);
    }

    private int sendMyPacket(prtGenConn clnt, int flg, int datSiz) {
        prtTcpConn pr = (prtTcpConn) clnt.proto;
        packHolder pck = new packHolder(true, true);
        synchronized (pr.lck) {
            pck.clear();
            if (datSiz > 0) {
                byte buf[] = new byte[pr.netOut + datSiz];
                datSiz = pr.netBufRx.nonDestructiveGet(buf, 0, pr.netOut + datSiz) - pr.netOut;
                if (datSiz < 1) {
                    return 0;
                }
                pck.putCopy(buf, pr.netOut, 0, datSiz);
                pck.putSkip(datSiz);
                pck.merge2beg();
            } else {
                datSiz = 0;
            }
            pck.TCPmss = 0;
            pck.UDPsrc = clnt.portLoc;
            pck.UDPtrg = clnt.portRem;
            pck.TCPseq = pr.seqLoc + pr.netOut;
            pck.TCPack = pr.seqRem;
            pck.TCPflg = flg;
            int i = clnt.freeAtServer() - 32;
            if (i < winSizMin) {
                i = winSizMin;
            }
            if (i > winSizMax) {
                i = winSizMax;
            }
            pck.TCPwin = i;
            pck.TCPurg = 0;
            pck.IPsrc.setAddr(clnt.iface.addr);
            pck.IPtrg.setAddr(clnt.peerAddr);
            pck.IPdf = false;
            pck.IPttl = clnt.sendTTL;
            pck.IPtos = clnt.sendTOS;
            if ((flg & flagSYN) != 0) {
                pck.TCPseq--;
                pck.TCPmss = maxSegSiz;
            }
            pr.netOut += datSiz;
        }
        createTCPheader(pck, clnt.passwd);
        fwdCore.protoPack(clnt.iface, pck);
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
        return bits.random(0x8000, 0xf000);
    }

    /**
     * start connection
     *
     * @param clnt client
     * @param pck packet
     * @return false if success, true if error
     */
    protected boolean connectionStart(prtGenConn clnt, packHolder pck) {
        if (debugger.prtTcpTraf) {
            logger.debug("start");
        }
        clnt.sendPRT = protoNum;
        prtTcpConn pr = new prtTcpConn();
        pipeLine pip = new pipeLine(65536, false);
        pr.netBufRx = pip.getSide();
        pr.netBufTx = pip.getSide();
        pr.netMax = maxSegSiz;
        clnt.proto = pr;
        clnt.timeout = timeoutSyn;
        clnt.workInterval = 1000;
        pr.activWait = prtTcpConn.tmNow;
        pr.activFrcd = true;
        pr.seqLoc = bits.randomD();
        pr.staTim = bits.getTime();
        if (pck == null) {
            pr.state = prtTcpConn.stConReq;
            return false;
        }
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
        prtTcpConn pr = (prtTcpConn) clnt.proto;
        if (debugger.prtTcpTraf) {
            logger.debug("close");
        }
        pr.netBufRx.setClose();
        pr.netBufTx.setClose();
        if ((pr.netBufRx.ready2rx() < 1) && (pr.state == prtTcpConn.stOpened)) {
            pr.state = prtTcpConn.stClrReq;
            pr.staTim = bits.getTime();
        }
        clnt.timeout = timeoutClos;
        pr.activWait = prtTcpConn.tmNow;
        pr.activFrcd = true;
    }

    /**
     * bytes available
     *
     * @param ntry connection
     * @return bytes
     */
    protected int connectionBytes(prtGenConn ntry) {
        prtTcpConn pr = (prtTcpConn) ntry.proto;
        return pr.netBufTx.ready2tx();
    }

    /**
     * send packet
     *
     * @param clnt client
     * @param pck packet
     * @return false if success, true if error
     */
    protected boolean connectionSend(prtGenConn clnt, packHolder pck) {
        prtTcpConn pr = (prtTcpConn) clnt.proto;
        return pck.pipeSend(pr.netBufTx, 0, pck.dataSize(), 1) != pck.dataSize();
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
        prtTcpConn pr = (prtTcpConn) clnt.proto;
        synchronized (pr.lck) {
            int nowAcked = pck.TCPack - pr.seqLoc; // bytes acked from tx buffer
            int oldBytes = pr.seqRem - pck.TCPseq; // old bytes arrived from past
            int packSize = pck.dataSize(); // bytes in packet
            int newBytes = packSize - oldBytes; // new bytes in packet
            int flg = pck.TCPflg & flagSynFinRstAck;
            if ((flg & flagSynFin) == flagSynFin) {
                logger.info("got both syn fin " + clnt);
                return;
            }
            if (flg == flagSynAck) {
                if (pr.state != prtTcpConn.stConReq) {
                    pr.activWait = prtTcpConn.tmLater;
                    logger.info("got unwanted synack " + clnt);
                    return;
                }
                if (nowAcked != 0) {
                    pr.activWait = prtTcpConn.tmLater;
                    logger.info("bad acknowkedge number in synack " + clnt);
                    return;
                }
                pr.seqRem = pck.TCPseq + 1;
                if (debugger.prtTcpTraf) {
                    logger.debug("accepted");
                }
                pr.state = prtTcpConn.stOpened;
                pr.staTim = bits.getTime();
                pr.activWait = prtTcpConn.tmNow;
                pr.activFrcd = true;
                clnt.setReady();
                clnt.timeout = timeoutOpen;
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
                if (nowAcked > pr.netBufRx.ready2rx()) {
                    if ((flg & flagFIN) != 0) {
                        pr.seqLoc++;
                    }
                    logger.info("got future acknowledge number " + clnt);
                    pr.activWait = prtTcpConn.tmLater;
                    nowAcked = 0;
                }
                if (nowAcked > 0) {
                    int i = pr.netBufRx.nonBlockSkip(nowAcked);
                    if (i < 1) {
                        logger.info("net buffer underflow " + clnt);
                        return;
                    }
                    pr.seqLoc += i;
                    pr.netOut -= i;
                    if (pr.netOut < 0) {
                        pr.netOut = 0;
                    }
                    pr.netMax += maxSegSiz;
                    if (pr.netMax > maxSegMax) {
                        pr.netMax = maxSegMax;
                    }
                }
                if (pr.state == prtTcpConn.stGotSyn) {
                    if (debugger.prtTcpTraf) {
                        logger.debug("opened");
                    }
                    pr.state = prtTcpConn.stOpened;
                    pr.staTim = bits.getTime();
                    pr.activWait = prtTcpConn.tmNow;
                    pr.activFrcd = true;
                    clnt.setReady();
                    clnt.timeout = timeoutOpen;
                }
            }
            if (oldBytes < 0) {
                if (debugger.prtTcpTraf) {
                    logger.debug("got future sequence number");
                }
                pr.activWait = prtTcpConn.tmLater;
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
                    return;
                }
                pr.seqRem += newBytes;
                pr.activFrcd = true;
                return;
            }
            if (flg == flagSYN) {
                if (pr.state != prtTcpConn.stGotSyn) {
                    logger.info("got unwanted syn " + clnt);
                    return;
                }
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
                    pr.activWait = prtTcpConn.tmNow;
                    pr.activFrcd = true;
                    pr.seqRem++;
                    clnt.timeout = timeoutFin;
                    return;
                }
                if (pr.state == prtTcpConn.stClrReq) {
                    pr.state = prtTcpConn.stDelete;
                    pr.seqRem++;
                    sendMyPacket(clnt, flagACK, 0);
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

    private boolean flush2net(prtGenConn clnt) {
        prtTcpConn pr = (prtTcpConn) clnt.proto;
        int bufSiz = pr.netBufRx.ready2rx();
        if ((!pr.activFrcd) && (bufSiz < 1)) {
            if (pr.netBufRx.isClosed() == 0) {
                pr.activWait = prtTcpConn.tmAlive;
                return true;
            }
            if (debugger.prtTcpTraf) {
                logger.debug("closing");
            }
            clnt.timeout = timeoutFin;
            pr.activWait = prtTcpConn.tmNow;
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
        pr.activWait = prtTcpConn.tmLater;
        for (;;) {
            int flg = flagPshAck;
            int snd = bufSiz - pr.netOut;
            int i = pr.netMax - pr.netOut;
            if (snd > i) {
                snd = i;
            }
            if (snd > maxSegSiz) {
                snd = maxSegSiz;
                flg = flagACK;
            }
            if (pr.netOut > pshNetOut) {
                flg = flagPshAck;
            }
            if (snd < 1) {
                break;
            }
            snd = sendMyPacket(clnt, flg, snd);
            if (snd < 1) {
                break;
            }
            sent++;
        }
        if (sent < 0) {
            sendMyPacket(clnt, flagACK, 0);
        }
        return true;
    }

    /**
     * work connection
     *
     * @param clnt client
     */
    protected void connectionWork(prtGenConn clnt) {
        prtTcpConn pr = (prtTcpConn) clnt.proto;
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
        if (pr.activWait > prtTcpConn.tmMax) {
            pr.activWait = prtTcpConn.tmMax;
        }
        pr.activLast = curTim;
        pr.activFrcd = false;
        switch (pr.state) {
            case prtTcpConn.stGotSyn:
                sendMyPacket(clnt, flagSynAck, 0);
                break;
            case prtTcpConn.stConReq:
                sendMyPacket(clnt, flagSYN, 0);
                break;
            case prtTcpConn.stClrReq:
                sendMyPacket(clnt, flagFinAck, 0);
                if ((curTim - pr.staTim) > prtTcpConn.tmAlive) {
                    pr.state = prtTcpConn.stDelete;
                }
                break;
            case prtTcpConn.stGotFin:
                sendMyPacket(clnt, flagFinAck, 0);
                if ((curTim - pr.staTim) > prtTcpConn.tmAlive) {
                    pr.state = prtTcpConn.stDelete;
                }
                break;
            case prtTcpConn.stOpened:
                sendMyPacket(clnt, flagACK, 0);
                pr.netMax = maxSegSiz;
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
     * close request, send fin+ack, wait fin+ack
     */
    public final static int stClrReq = 3;

    /**
     * got fin, send fin+ack, wiat a bit
     */
    public final static int stGotFin = 4;

    /**
     * normal data flow, send ack
     */
    public final static int stOpened = 5;

    /**
     * keepalive timer
     */
    public final static int tmAlive = 60 * 1000;

    /**
     * delayed timer
     */
    public final static int tmLater = 3 * 1000;

    /**
     * maximum timeout
     */
    public final static int tmMax = 8 * 1000;

    /**
     * now timeout
     */
    public final static int tmNow = 100;

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
     * rx buffer
     */
    protected pipeSide netBufRx;

    /**
     * tx buffer
     */
    protected pipeSide netBufTx;

    public String toString() {
        return "" + state;
    }

}
