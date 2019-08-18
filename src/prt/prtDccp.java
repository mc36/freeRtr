package prt;

import addr.addrIP;
import cfg.cfgAll;
import ip.ipFwd;
import ip.ipFwdIface;
import pack.packHolder;
import util.bits;
import util.counter;
import util.debugger;
import util.logger;
import util.state;

/**
 * handle dccp (rfc4340) connections
 *
 * @author matecsaba
 */
public class prtDccp extends prtGen {

    /**
     * minimum size of dccp header
     */
    public final static int size = 12;

    /**
     * protocol number of dccp
     */
    public final static int protoNum = 33;

    /**
     * request
     */
    public final static int typReq = 0;

    /**
     * response
     */
    public final static int typResp = 1;

    /**
     * data
     */
    public final static int typData = 2;

    /**
     * acknowledge
     */
    public final static int typAck = 3;

    /**
     * data + acknowledge
     */
    public final static int typDatAck = 4;

    /**
     * close request
     */
    public final static int typCloseReq = 5;

    /**
     * close response
     */
    public final static int typCloseRep = 6;

    /**
     * reset
     */
    public final static int typReset = 7;

    /**
     * sync request
     */
    public final static int typSync = 8;

    /**
     * sync acknowledge
     */
    public final static int typSyncAck = 9;

    /**
     * listen
     */
    public final static int typListen = 10;

    /**
     * create new instance
     *
     * @param ifw forwarder to use
     */
    public prtDccp(ipFwd ifw) {
        fwdCore = ifw;
        ifw.protoAdd(this, null, null);
    }

    /**
     * create new instance
     */
    public prtDccp() {
    }

    private static boolean haveAck(packHolder pck) {
        switch (pck.TCPflg >>> 1) {
            case typReq:
            case typData:
            case typListen:
                return false;
            default:
                return true;
        }
    }

    private static int getSumLen(packHolder pck) {
        int o = pck.getByte(5) & 0xf;
        if (o == 0) {
            return pck.dataSize();
        } else {
            return ((o - 1) * 4) + (pck.getByte(4) * 4);
        }
    }

    /**
     * create dccp header
     *
     * @param pck packet to update
     */
    public static void createDCCPheader(packHolder pck) {
        pck.IPprt = protoNum;
        if (debugger.prtDccpTraf) {
            logger.debug("tx " + pck.UDPsrc + " -> " + pck.UDPtrg + " seq=" + pck.TCPseq + " data=" + pck.dataSize() + " ack=" + pck.TCPack);
        }
        pck.TCPflg = (pck.TCPflg & 0x1f) | 1;
        pck.msbPutW(0, pck.UDPsrc); // source port
        pck.msbPutW(2, pck.UDPtrg); // target port
        pck.putByte(5, 0); // cscov
        pck.msbPutW(6, 0); // checksum
        pck.putByte(8, pck.TCPflg); // type
        pck.putByte(9, 0); // reserved
        pck.msbPutW(10, pck.TCPwin); // seq high
        pck.msbPutD(12, pck.TCPseq); // seq low
        int o = 16;
        if (haveAck(pck)) {
            pck.msbPutW(16, 0); // reserved
            pck.msbPutW(18, pck.TCPurg); // ack high
            pck.msbPutD(20, pck.TCPack); // ack low
            o += 8;
        }
        pck.putByte(4, (o + pck.TCPmss) / 4); // data offset
        if (cfgAll.dccpChecksumTx) {
            int i = pck.pseudoIPsum(o + pck.dataSize());
            i = pck.putIPsum(0, o, i);
            i = pck.getIPsum(0, pck.dataSize(), i);
            pck.lsbPutW(6, 0xffff - i); // checksum
        }
        pck.putSkip(o);
        pck.merge2beg();
    }

    /**
     * parse dccp ports
     *
     * @param pck packet to parse
     */
    public static void parseDCCPports(packHolder pck) {
        pck.UDPsrc = pck.msbGetW(0); // source port
        pck.UDPtrg = pck.msbGetW(2); // target port
    }

    /**
     * parse dccp header
     *
     * @param pck packet to parse
     * @return false if successful, true if error happened
     */
    public static boolean parseDCCPheader(packHolder pck) {
        parseDCCPports(pck);
        int len = pck.dataSize();
        pck.UDPsiz = pck.getByte(4) * 4;
        if (len < pck.UDPsiz) {
            logger.info("got too small from " + pck.IPsrc);
            return true;
        }
        if (cfgAll.dccpChecksumRx) {
            int i = pck.pseudoIPsum(len);
            i = pck.getIPsum(0, getSumLen(pck), i);
            if (i != 0xffff) {
                logger.info("got bad checksum from " + pck.IPsrc);
                return true;
            }
        }
        int i;
        pck.TCPflg = pck.getByte(8) & 0x1f;
        if ((pck.TCPflg & 1) == 0) {
            pck.TCPseq = pck.msbGetD(8) & 0xffffff;
            i = 12;
            if (haveAck(pck)) {
                pck.TCPack = pck.msbGetD(12) & 0xffffff;
                i += 4;
            }
        } else {
            pck.TCPwin = pck.msbGetW(10);
            pck.TCPseq = pck.msbGetD(12);
            i = 16;
            if (haveAck(pck)) {
                pck.TCPurg = pck.msbGetW(18);
                pck.TCPack = pck.msbGetD(20);
                i += 8;
            }
        }
        pck.TCPmss = pck.UDPsiz - i;
        pck.getSkip(pck.UDPsiz);
        return false;
    }

    /**
     * update dccp header
     *
     * @param pck packet to work with
     * @param src source port
     * @param trg target port
     */
    public static void updateDCCPheader(packHolder pck, int src, int trg) {
        int o = getSumLen(pck);
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
        pck.lsbPutW(6, 0); // checksum
        if (cfgAll.dccpChecksumTx) {
            int i = pck.pseudoIPsum(size + pck.dataSize());
            i = pck.putIPsum(0, size, i);
            i = pck.getIPsum(0, o - size, i);
            pck.lsbPutW(6, 0xffff - i); // checksum
        }
        pck.putSkip(size);
        pck.merge2beg();
    }

    public String toString() {
        return "dccp on " + fwdCore;
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

    protected boolean connectionStart(prtGenConn clnt, packHolder pck) {
        if (debugger.prtDccpTraf) {
            logger.debug("start");
        }
        clnt.sendPRT = protoNum;
        prtDccpConn pr = new prtDccpConn();
        clnt.proto = pr;
        pr.seqLocHi = bits.randomD();
        pr.seqLocLo = bits.randomD();
        if (pck == null) {
            return false;
        }
        pr.conned = true;
        clnt.setReady();
        pr.seqRemHi = pck.TCPwin;
        pr.seqRemLo = pck.TCPseq;
        if ((pck.TCPflg >>> 1) != typReq) {
            return true;
        }
        byte[] buf = pr.parseOptions(pck, 4);
        if (buf == null) {
            return true;
        }
        return false;
    }

    protected void connectionRefuse(ipFwdIface ifc, packHolder src) {
        if (debugger.prtDccpTraf) {
            logger.debug("refuse");
        }
        if ((src.TCPflg >>> 1) == typReset) {
            return;
        }
        packHolder pck = new packHolder(true, true);
        pck.msbPutD(0, 3 << 24); // code
        pck.putSkip(4);
        pck.merge2beg();
        pck.TCPmss = pck.dataSize();
        pck.UDPsrc = src.UDPtrg;
        pck.UDPtrg = src.UDPsrc;
        pck.TCPwin = src.TCPurg;
        pck.TCPseq = src.TCPack;
        pck.TCPurg = src.TCPwin;
        pck.TCPack = src.TCPseq;
        pck.TCPflg = typReset << 1;
        pck.IPsrc.setAddr(src.IPtrg);
        pck.IPtrg.setAddr(src.IPsrc);
        pck.IPdf = false;
        pck.IPttl = -1;
        pck.IPtos = -1;
        createDCCPheader(pck);
        fwdCore.protoPack(ifc, pck);
    }

    protected void connectionClose(prtGenConn clnt) {
        if (debugger.prtDccpTraf) {
            logger.debug("close");
        }
        byte[] buf = new byte[4];
        buf[0] = 2; // code
        sendMyPack(clnt, buf, typReset);
        clnt.deleteImmediately();
    }

    protected void connectionWork(prtGenConn clnt) {
        prtDccpConn pr = (prtDccpConn) clnt.proto;
        if (pr.conned) {
            return;
        }
        byte[] opt = new byte[4];
        bits.msbPutD(opt, 0, clnt.portRem);
        sendMyPack(clnt, opt, typReq);
    }

    protected int connectionBytes(prtGenConn ntry) {
        return 0xffffff;
    }

    protected void connectionRcvd(prtGenConn clnt, packHolder pck) {
        prtDccpConn pr = (prtDccpConn) clnt.proto;
        byte[] buf;
        pr.seqRemHi = pck.TCPwin;
        pr.seqRemLo = pck.TCPseq;
        if (!pr.conned) {
            pr.conned = true;
            clnt.setReady();
        }
        switch (pck.TCPflg >>> 1) {
            case typReq:
                buf = pr.parseOptions(pck, 4);
                sendMyPack(clnt, buf, typResp);
                return;
            case typResp:
                buf = pr.parseOptions(pck, 4);
                sendMyPack(clnt, buf, typAck);
                return;
            case typAck:
                buf = pr.parseOptions(pck, 0);
                break;
            case typData:
            case typDatAck:
                buf = pr.parseOptions(pck, 0);
                pr.rxPkt++;
                clnt.send2server(pck);
                break;
            case typCloseReq:
            case typCloseRep:
                buf = pr.parseOptions(pck, 0);
                buf = new byte[4];
                buf[0] = 1; // code
                sendMyPack(clnt, buf, typReset);
                clnt.setClosing();
                clnt.deleteImmediately();
                return;
            case typReset:
                clnt.setClosing();
                clnt.deleteImmediately();
                return;
            case typSync:
                buf = pr.parseOptions(pck, 0);
                sendMyPack(clnt, buf, typSyncAck);
                return;
            case typSyncAck:
                buf = pr.parseOptions(pck, 0);
                sendMyPack(clnt, buf, typAck);
                return;
            case typListen:
                return;
            default:
                return;
        }
        if ((buf.length > 0) || (pr.rxPkt > 32)) {
            sendMyPack(clnt, buf, typAck);
            pr.rxPkt = 0;
        }
    }

    protected boolean connectionSend(prtGenConn clnt, packHolder pck) {
        prtDccpConn pr = (prtDccpConn) clnt.proto;
        pr.updateSeq();
        pck.IPttl = clnt.sendTTL;
        pck.IPtos = clnt.sendTOS;
        pck.TCPmss = 0;
        pr.setSeq(pck);
        pck.TCPflg = typDatAck << 1;
        createDCCPheader(pck);
        fwdCore.protoPack(clnt.iface, pck);
        return false;
    }

    /**
     * received packet
     *
     * @param rxIfc interface
     * @param pck packet
     */
    public void recvPack(ipFwdIface rxIfc, packHolder pck) {
        cntr.rx(pck);
        if (parseDCCPheader(pck)) {
            cntr.drop(pck, counter.reasons.badHdr);
            return;
        }
        if (debugger.prtDccpTraf) {
            logger.debug("rec " + pck.UDPsrc + " -> " + pck.UDPtrg);
        }
        connectionSimpleWork(rxIfc, pck);
    }

    /**
     * received alert
     *
     * @param rxIfc interface
     * @param pck packet
     * @return false on success, true on error
     */
    public boolean alertPack(ipFwdIface rxIfc, packHolder pck) {
        return true;
    }

    /**
     * received error
     *
     * @param err error code
     * @param rtr address
     * @param rxIfc interface
     * @param pck packet
     */
    public void errorPack(counter.reasons err, addrIP rtr, ipFwdIface rxIfc, packHolder pck) {
        parseDCCPports(pck);
        if (debugger.prtDccpTraf) {
            logger.debug(counter.reason2string(err) + " " + pck.UDPsrc + " -> " + pck.UDPtrg);
        }
        connectionSimpleError(err, rtr, rxIfc, pck);
    }

    private void sendMyPack(prtGenConn clnt, byte[] opt, int typ) {
        prtDccpConn pr = (prtDccpConn) clnt.proto;
        packHolder pck = new packHolder(true, true);
        pck.putCopy(opt, 0, 0, opt.length);
        pck.putSkip(opt.length);
        pck.merge2beg();
        pck.TCPmss = opt.length;
        pck.IPttl = clnt.sendTTL;
        pck.IPtos = clnt.sendTOS;
        pr.setSeq(pck);
        pck.TCPflg = typ << 1;
        pck.UDPsrc = clnt.portLoc;
        pck.UDPtrg = clnt.portRem;
        pck.IPtrg.setAddr(clnt.peerAddr);
        pck.IPsrc.setAddr(clnt.iface.addr);
        createDCCPheader(pck);
        fwdCore.protoPack(clnt.iface, pck);
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

class prtDccpConn {

    /**
     * padding
     */
    public final static int optPad = 0;

    /**
     * mandatory
     */
    public final static int optMand = 1;

    /**
     * slow responder
     */
    public final static int optSlow = 2;

    /**
     * change local
     */
    public final static int optChgL = 32;

    /**
     * confirm local
     */
    public final static int optCnfL = 33;

    /**
     * change remote
     */
    public final static int optChgR = 34;

    /**
     * confirm remote
     */
    public final static int optCnfR = 35;

    /**
     * init cookie
     */
    public final static int optCook = 36;

    /**
     * ndp count
     */
    public final static int optNdpC = 37;

    /**
     * ack vector 0
     */
    public final static int optAck0 = 38;

    /**
     * ack vector 1
     */
    public final static int optAck1 = 39;

    /**
     * data dropped
     */
    public final static int optDrop = 40;

    /**
     * timestamp request
     */
    public final static int optTimReq = 41;

    /**
     * timestamp reply
     */
    public final static int optTimRep = 42;

    /**
     * time elapsed
     */
    public final static int optTimOvr = 43;

    /**
     * data checksum
     */
    public final static int optChkSum = 44;

    /**
     * quick start response
     */
    public final static int optQckStrt = 45;

    /**
     * congestion control
     */
    public final static int fetCngCtrl = 1;

    /**
     * allow short sequence numbers
     */
    public final static int fetShrtSeq = 2;

    /**
     * sequence window
     */
    public final static int fetSeqWin = 3;

    /**
     * ecn incapable
     */
    public final static int fetNoEcn = 4;

    /**
     * ack ratio
     */
    public final static int fetAckRat = 5;

    /**
     * send ack vector
     */
    public final static int fetAckVct = 6;

    /**
     * send ndp count
     */
    public final static int fetNdpCnt = 7;

    /**
     * minimum checksum coverage
     */
    public final static int fetSumCvr = 8;

    /**
     * check data checksum
     */
    public final static int fetChkSum = 9;

    public int seqLocHi;

    public int seqLocLo;

    public int seqRemHi;

    public int seqRemLo;

    public boolean conned;

    public int rxPkt;

    public void updateSeq() {
        seqLocLo++;
        if (seqLocLo == 0) {
            seqLocHi++;
        }
    }

    public void setSeq(packHolder pck) {
        pck.TCPwin = seqLocHi;
        pck.TCPseq = seqLocLo;
        pck.TCPurg = seqRemHi;
        pck.TCPack = seqRemLo;
    }

    public byte[] parseOptions(packHolder pck, int cop) {
        int siz = pck.dataSize();
        pck.getSkip(-pck.TCPmss);
        byte[] res = new byte[cop];
        for (int i = 0; i < cop; i++) {
            res[i] = (byte) pck.getByte(i);
        }
        pck.getSkip(cop);
        for (;;) {
            if (pck.dataSize() <= (siz - pck.TCPmss)) {
                break;
            }
            int typ = pck.getByte(0);
            pck.getSkip(1);
            int len = 0;
            if (typ > 31) {
                len = pck.getByte(0) - 2;
                pck.getSkip(1);
            }
            if (len < 0) {
                len = 0;
            }
            byte[] dat = new byte[len];
            pck.getCopy(dat, 0, 0, len);
            pck.getSkip(len);
            byte[] add = new byte[0];
            switch (typ) {
                case optTimReq:
                    if (dat.length < 4) {
                        continue;
                    }
                    add = new byte[6];
                    bits.msbPutD(add, 0, bits.msbGetD(dat, 0));
                    bits.msbPutW(add, 4, 3);
                    typ = optTimRep;
                    break;
                case optChgL:
                    if (dat.length < 2) {
                        continue;
                    }
                    add = new byte[3];
                    add[0] = dat[0];
                    add[1] = (byte) getFeatVal(dat[0], dat[1]);
                    add[2] = (byte) getFeatVal(dat[0], -1);
                    typ = optCnfR;
                    break;
                case optChgR:
                    if (dat.length < 2) {
                        continue;
                    }
                    add = new byte[3];
                    add[0] = dat[0];
                    add[1] = (byte) getFeatVal(dat[0], dat[1]);
                    add[2] = (byte) getFeatVal(dat[0], -1);
                    typ = optCnfL;
                    break;
                default:
                    break;
            }
            if (add.length < 1) {
                continue;
            }
            dat = new byte[2];
            dat[0] = (byte) typ;
            dat[1] = (byte) (add.length + 2);
            dat = bits.byteConcat(dat, add);
            res = bits.byteConcat(res, dat);
        }
        pck.setBytesLeft(siz);
        if ((res.length & 3) == 0) {
            return res;
        }
        byte[] dat = new byte[4 - (res.length & 3)];
        return bits.byteConcat(res, dat);
    }

    public int getFeatVal(int ft, int vl) {
        switch (ft) {
            case fetCngCtrl:
                return 2;
            case fetShrtSeq:
                return vl < 0 ? 0 : vl;
            case fetSeqWin:
                return vl < 0 ? 100 : vl;
            case fetNoEcn:
                return vl < 0 ? 1 : vl;
            case fetAckRat:
                return vl < 0 ? 16 : vl;
            case fetAckVct:
                return 0;
            case fetNdpCnt:
                return 0;
            case fetSumCvr:
                return vl < 0 ? 0 : vl;
            case fetChkSum:
                return 0;
            default:
                return -1;
        }
    }

    public String toString() {
        return "" + conned;
    }

}
