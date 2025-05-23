package org.freertr.prt;

import org.freertr.addr.addrIP;
import org.freertr.cfg.cfgAll;
import org.freertr.cry.cryHashCrc32;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.pack.packHolder;
import org.freertr.util.bits;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.state.states;
import org.freertr.enc.encTlv;

/**
 * handle sctp (rfc4960) connections
 *
 * @author matecsaba
 */
public class prtSctp extends prtGen {

    /**
     * minimum size of sctp header
     */
    public final static int size = 12;

    /**
     * protocol number of sctp
     */
    public final static int protoNum = 132;

    /**
     * create new instance
     *
     * @param ifw forwarder to use
     */
    public prtSctp(ipFwd ifw) {
        fwdCore = ifw;
        ifw.protoAdd(this, null, null);
    }

    /**
     * create new instance
     */
    public prtSctp() {
    }

    private static encTlv getTlv() {
        return new encTlv(0, 16, 16, 16, 1, 4, 4, 4, 0, 4096, true);
    }

    private static encTlv findTlv(packHolder pck, int msk, int typ) {
        encTlv tlv = getTlv();
        int siz = pck.dataSize();
        for (;;) {
            if (tlv.getBytes(pck)) {
                break;
            }
            if ((tlv.valTyp & msk) == typ) {
                pck.setBytesLeft(siz);
                return tlv;
            }
        }
        pck.setBytesLeft(siz);
        return null;
    }

    /**
     * create sctp header
     *
     * @param pck packet to update
     */
    public static void createSCTPheader(packHolder pck) {
        pck.IPprt = protoNum;
        if (debugger.prtSctpTraf) {
            logger.debug("tx " + pck.UDPsrc + " -> " + pck.UDPtrg + " data=" + pck.dataSize());
        }
        pck.msbPutW(0, pck.UDPsrc); // source port
        pck.msbPutW(2, pck.UDPtrg); // target port
        pck.msbPutD(4, pck.TCPflg); // verification tag
        pck.msbPutD(8, 0); // checksum
        if (cfgAll.sctpChecksumTx) {
            cryHashCrc32 sum = new cryHashCrc32(cryHashCrc32.polyCrc32c);
            sum.init();
            pck.hashHead(sum, 0, size);
            pck.hashData(sum, 0, pck.dataSize());
            byte[] calc = sum.finish();
            pck.putCopy(calc, 0, 8, calc.length);
        }
        pck.putSkip(size);
        pck.merge2beg();
    }

    /**
     * parse sctp ports
     *
     * @param pck packet to parse
     */
    public static void parseSCTPports(packHolder pck) {
        pck.UDPsrc = pck.msbGetW(0); // source port
        pck.UDPtrg = pck.msbGetW(2); // target port
        pck.UDPsiz = size;
    }

    /**
     * parse sctp header
     *
     * @param pck packet to parse
     * @return false if successful, true if error happened
     */
    public static boolean parseSCTPheader(packHolder pck) {
        parseSCTPports(pck);
        if (pck.dataSize() < size) {
            logger.info("got too small from " + pck.IPsrc);
            return true;
        }
        pck.TCPflg = pck.msbGetD(4); // verification tag
        if (cfgAll.sctpChecksumRx) {
            cryHashCrc32 sum = new cryHashCrc32(cryHashCrc32.polyCrc32c);
            sum.init();
            pck.hashData(sum, 0, 8);
            sum.update(0);
            sum.update(0);
            sum.update(0);
            sum.update(0);
            pck.hashData(sum, size, pck.dataSize() - size);
            byte[] calc = sum.finish();
            byte[] got = new byte[calc.length];
            pck.getCopy(got, 0, 8, got.length);
            if (bits.byteComp(calc, 0, got, 0, got.length) != 0) {
                logger.info("got bad checksum from " + pck.IPsrc);
                return true;
            }
        }
        pck.getSkip(size);
        return false;
    }

    /**
     * update sctp header
     *
     * @param pck packet to work with
     * @param src source port
     * @param trg target port
     */
    public static void updateSCTPheader(packHolder pck, int src, int trg) {
        pck.unMergeBytes(size);
        pck.putSkip(-size);
        if (src >= 0) {
            pck.msbPutW(0, src); // source port
            pck.UDPsrc = src;
        }
        if (trg >= 0) {
            pck.msbPutW(2, trg); // target port
            pck.UDPtrg = trg;
        }
        pck.msbPutD(8, 0); // checksum
        if (cfgAll.sctpChecksumTx) {
            cryHashCrc32 sum = new cryHashCrc32(cryHashCrc32.polyCrc32c);
            sum.init();
            pck.hashHead(sum, 0, size);
            pck.hashData(sum, 0, pck.dataSize());
            byte[] calc = sum.finish();
            pck.putCopy(calc, 0, 8, calc.length);
        }
        pck.putSkip(size);
        pck.merge2beg();
    }

    public String toString() {
        return "sctp on " + fwdCore;
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
        return bits.random(cfgAll.sctpRangeMin, cfgAll.sctpRangeMax);
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
        if (debugger.prtSctpTraf) {
            logger.debug("start");
        }
        clnt.sendPRT = protoNum;
        prtSctpConn pr = new prtSctpConn();
        clnt.protoDat = pr;
        pr.seqLoc = bits.randomD();
        pr.verLoc = bits.randomD();
        if (pck == null) {
            pr.state = 4;
            pr.lastRx = bits.getTime();
            return false;
        }
        encTlv tlv = findTlv(pck, prtSctpConn.opcMask, prtSctpConn.opcInitReq);
        if (tlv == null) {
            return true;
        }
        pr.verRem = bits.msbGetD(tlv.valDat, 0); // initial tag
        pr.seqRem = bits.msbGetD(tlv.valDat, 12); // initial tsn
        pr.state = 1;
        pr.lastRx = bits.getTime();
        return false;
    }

    protected void connectionRefuse(ipFwdIface ifc, packHolder src) {
        if (debugger.prtSctpTraf) {
            logger.debug("refuse");
        }
        if (findTlv(src, prtSctpConn.opcMask, prtSctpConn.opcAbort) != null) {
            return;
        }
        packHolder pck = new packHolder(true, true);
        encTlv tlv = getTlv();
        tlv.putBytes(pck, prtSctpConn.opcAbort, 0, tlv.valDat);
        pck.merge2beg();
        pck.UDPsrc = src.UDPtrg;
        pck.UDPtrg = src.UDPsrc;
        pck.TCPflg = src.TCPflg;
        createSCTPheader(pck);
        pck.IPsrc.setAddr(src.IPtrg);
        pck.IPtrg.setAddr(src.IPsrc);
        pck.IPdf = false;
        pck.IPfrg = 0;
        pck.IPalrt = -1;
        pck.IPttl = -1;
        pck.IPtos = -1;
        pck.IPid = -1;
        fwdCore.protoPack(ifc, null, pck);
    }

    /**
     * close connection
     *
     * @param clnt client
     */
    protected void connectionClose(prtGenConn clnt) {
        if (debugger.prtSctpTraf) {
            logger.debug("close");
        }
        prtSctpConn pr = (prtSctpConn) clnt.protoDat;
        pr.state = 3;
    }

    /**
     * work connection
     *
     * @param clnt client
     */
    protected void connectionWork(prtGenConn clnt) {
        prtSctpConn pr = (prtSctpConn) clnt.protoDat;
        if (debugger.prtSctpTraf) {
            logger.debug("work");
        }
        packHolder pck = new packHolder(true, true);
        encTlv tlv = getTlv();
        switch (pr.state) {
            case 1:
                bits.msbPutD(tlv.valDat, 0, pr.verLoc + pr.verRem);
                tlv.putBytes(pck, 7, 4, tlv.valDat);
                pck.merge2beg();
                pck.msbPutD(0, pr.verLoc); // initial tag
                pck.msbPutD(4, 0x10000); // initial window
                pck.msbPutW(8, 8); // out streams
                pck.msbPutW(10, 8); // in streams
                pck.msbPutD(12, pr.seqLoc); // initial tsn
                pck.putSkip(16);
                pck.merge2beg();
                sendMyPack(clnt, pck.getCopy(), prtSctpConn.opcInitAck);
                break;
            case 2:
                byte[] buf = pr.txBuf;
                if (buf == null) {
                    break;
                }
                pck.msbPutD(0, pr.seqLoc); // tsn
                pck.msbPutW(4, 0); // stream id
                pck.msbPutW(6, pr.stream); // stream seq
                pck.msbPutD(8, 0); // app id
                pck.putSkip(12);
                pck.putCopy(buf, 0, 0, buf.length);
                pck.putSkip(buf.length);
                pck.merge2beg();
                sendMyPack(clnt, pck.getCopy(), prtSctpConn.opcData | 3);
                break;
            case 3:
                buf = new byte[4];
                bits.msbPutD(buf, 0, pr.seqRem);
                sendMyPack(clnt, buf, prtSctpConn.opcShutReq);
                break;
            case 4:
                pck.msbPutD(0, pr.verLoc); // initial tag
                pck.msbPutD(4, 0x10000); // initial window
                pck.msbPutW(8, 8); // out streams
                pck.msbPutW(10, 8); // in streams
                pck.msbPutD(12, pr.seqLoc); // initial tsn
                pck.putSkip(16);
                pck.merge2beg();
                sendMyPack(clnt, pck.getCopy(), prtSctpConn.opcInitReq);
                break;
        }
    }

    /**
     * bytes available
     *
     * @param ntry connection
     * @return bytes
     */
    protected int connectionBytes(prtGenConn ntry) {
        prtSctpConn pr = (prtSctpConn) ntry.protoDat;
        if (pr.state != 2) {
            return 0;
        }
        if (pr.txBuf == null) {
            return 0xffffff;
        } else {
            return 0;
        }
    }

    /**
     * send packet
     *
     * @param clnt client
     * @param pck packet
     * @return false if success, true if error
     */
    protected boolean connectionSend(prtGenConn clnt, packHolder pck) {
        prtSctpConn pr = (prtSctpConn) clnt.protoDat;
        if (pr.state != 2) {
            return true;
        }
        if (pr.txBuf != null) {
            return true;
        }
        pr.txBuf = pck.getCopy();
        return false;
    }

    private void gotTlv(prtGenConn clnt, prtSctpConn pr, encTlv tlv) {
        int i;
        packHolder pck2;
        switch (tlv.valTyp & prtSctpConn.opcMask) {
            case prtSctpConn.opcData:
                if (pr.state != 2) {
                    break;
                }
                byte[] buf = new byte[12];
                bits.msbPutD(buf, 0, pr.seqRem); // cumulative ack
                bits.msbPutD(buf, 4, 0x10000); // window size
                bits.msbPutD(buf, 8, 0); // 0 gaps, 0 duplicates
                i = bits.msbGetD(tlv.valDat, 0);
                if (i != pr.seqRem) {
                    sendMyPack(clnt, buf, prtSctpConn.opcSelAck);
                    break;
                }
                pck2 = new packHolder(true, true);
                i = tlv.valSiz - 12;
                pck2.putCopy(tlv.valDat, 12, 0, i);
                pck2.putSkip(i);
                pck2.merge2beg();
                if (clnt.send2server(pck2)) {
                    break;
                }
                sendMyPack(clnt, buf, prtSctpConn.opcSelAck);
                pr.seqRem++;
                break;
            case prtSctpConn.opcSelAck:
                if (pr.state != 2) {
                    break;
                }
                i = bits.msbGetD(tlv.valDat, 0);
                if (i != pr.seqLoc) {
                    break;
                }
                if (pr.txBuf == null) {
                    break;
                }
                pr.seqLoc++;
                pr.stream++;
                pr.txBuf = null;
                break;
            case prtSctpConn.opcInitReq:
                break;
            case prtSctpConn.opcInitAck:
                pck2 = new packHolder(true, true);
                i = tlv.valSiz - 16;
                pck2.putCopy(tlv.valDat, 16, 0, i);
                pck2.putSkip(i);
                pck2.merge2beg();
                encTlv res = findTlv(pck2, 0xffff, 7);
                if (res == null) {
                    break;
                }
                if (pr.state == 4) {
                    pr.verRem = bits.msbGetD(tlv.valDat, 0); // initial tag
                    pr.seqRem = bits.msbGetD(tlv.valDat, 12); // initial tsn
                    pr.state = 2;
                    clnt.setReady();
                }
                sendMyPack(clnt, res.copyBytes(), prtSctpConn.opcCokieDat);
                break;
            case prtSctpConn.opcCokieAck:
                break;
            case prtSctpConn.opcCokieDat:
                if (bits.msbGetD(tlv.valDat, 0) != (pr.verLoc + pr.verRem)) {
                    break;
                }
                sendMyPack(clnt, new byte[0], prtSctpConn.opcCokieAck);
                if (pr.state != 1) {
                    break;
                }
                pr.state = 2;
                clnt.setReady();
                break;
            case prtSctpConn.opcAbort:
            case prtSctpConn.opcError:
                clnt.setClosing();
                clnt.deleteImmediately();
                break;
            case prtSctpConn.opcShutReq:
                sendMyPack(clnt, new byte[0], prtSctpConn.opcShutAck);
                clnt.setClosing();
                clnt.deleteImmediately();
                break;
            case prtSctpConn.opcShutAck:
                sendMyPack(clnt, new byte[0], prtSctpConn.opcShutCmp);
                clnt.setClosing();
                clnt.deleteImmediately();
                break;
            case prtSctpConn.opcShutCmp:
                clnt.setClosing();
                clnt.deleteImmediately();
                break;
            case prtSctpConn.opcHeartReq:
                sendMyPack(clnt, tlv.copyBytes(), prtSctpConn.opcHeartAck);
                break;
        }
    }

    /**
     * received packet
     *
     * @param clnt client
     * @param pck packet
     */
    protected void connectionRcvd(prtGenConn clnt, packHolder pck) {
        prtSctpConn pr = (prtSctpConn) clnt.protoDat;
        pr.lastRx = bits.getTime();
        encTlv tlv = getTlv();
        for (;;) {
            if (tlv.getBytes(pck)) {
                break;
            }
            gotTlv(clnt, pr, tlv);
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

    /**
     * received packet
     *
     * @param rxIfc interface
     * @param pck packet
     */
    public void recvPack(ipFwdIface rxIfc, packHolder pck) {
        cntr.rx(pck);
        if (parseSCTPheader(pck)) {
            cntr.drop(pck, counter.reasons.badHdr);
            return;
        }
        if (debugger.prtSctpTraf) {
            logger.debug("rec " + pck.UDPsrc + " -> " + pck.UDPtrg);
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
        parseSCTPports(pck);
        if (debugger.prtSctpTraf) {
            logger.debug(counter.reason2string(err) + " " + pck.UDPsrc + " -> " + pck.UDPtrg);
        }
        connectionSimpleError(err, rtr, rxIfc, pck);
    }

    private void sendMyPack(prtGenConn clnt, byte[] opt, int typ) {
        prtSctpConn pr = (prtSctpConn) clnt.protoDat;
        packHolder pck = new packHolder(true, true);
        encTlv tlv = getTlv();
        tlv.putBytes(pck, typ, opt);
        pck.merge2beg();
        pck.UDPsrc = clnt.portLoc;
        pck.UDPtrg = clnt.portRem;
        pck.TCPflg = pr.verRem;
        createSCTPheader(pck);
        pck.IPtrg.setAddr(clnt.peerAddr);
        pck.IPsrc.setAddr(clnt.iface.addr);
        pck.IPdf = false;
        pck.IPfrg = 0;
        pck.IPalrt = -1;
        pck.IPttl = clnt.sendTTL;
        pck.IPtos = clnt.sendTOS;
        pck.IPdf = clnt.sendDFN == 1;
        pck.IPid = clnt.sendFLW;
        fwdCore.protoPack(clnt.iface, null, pck);
    }

    /**
     * set state
     *
     * @param iface interface
     * @param stat state
     */
    public void setState(ipFwdIface iface, states stat) {
        connectionSimpleState(iface, stat);
    }

}

class prtSctpConn {

    public final static int opcMask = 0xff00;

    public final static int opcData = 0x0000;

    public final static int opcInitReq = 0x0100;

    public final static int opcInitAck = 0x0200;

    public final static int opcSelAck = 0x0300;

    public final static int opcHeartReq = 0x0400;

    public final static int opcHeartAck = 0x0500;

    public final static int opcAbort = 0x0600;

    public final static int opcShutReq = 0x0700;

    public final static int opcShutAck = 0x0800;

    public final static int opcError = 0x0900;

    public final static int opcCokieDat = 0x0a00;

    public final static int opcCokieAck = 0x0b00;

    public final static int opcShutCmp = 0x0e00;

    public final static int opcAuth = 0x0f00;

    public int seqLoc;

    public int seqRem;

    public int stream;

    public int verLoc;

    public int verRem;

    public int state;

    public long lastRx;

    public byte[] txBuf;

    public String toString() {
        switch (state) {
            case 1:
                return "accpt";
            case 2:
                return "estab";
            case 3:
                return "term";
            case 4:
                return "init";
            default:
                return "unknown=" + state;
        }
    }

}
