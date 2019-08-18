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
 * handle lightweight udp (rfc3828) connections
 *
 * @author matecsaba
 */
public class prtLudp extends prtGen {

    /**
     * size of ludp header
     */
    public final static int size = 8;

    /**
     * protocol number of ludp
     */
    public final static int protoNum = 136;

    /**
     * create new instance
     *
     * @param ifw forwarder to use
     */
    public prtLudp(ipFwd ifw) {
        fwdCore = ifw;
        ifw.protoAdd(this, null, null);
    }

    /**
     * create new instance
     */
    public prtLudp() {
    }

    /**
     * create ludp header
     *
     * @param pck packet to update
     */
    public static void createLUDPheader(packHolder pck) {
        pck.IPprt = protoNum;
        if (debugger.prtLudpTraf) {
            logger.debug("tx " + pck.UDPsrc + " -> " + pck.UDPtrg);
        }
        pck.msbPutW(0, pck.UDPsrc); // source port
        pck.msbPutW(2, pck.UDPtrg); // target port
        pck.msbPutW(4, size); // length (hdr incl)
        pck.lsbPutW(6, 0); // checksum
        if (cfgAll.ludpChecksumTx) {
            int i = pck.pseudoIPsum(size + pck.dataSize());
            i = pck.putIPsum(0, size, i);
            pck.lsbPutW(6, 0xffff - i); // checksum
        }
        pck.putSkip(size);
        pck.merge2beg();
    }

    /**
     * parse ludp ports
     *
     * @param pck packet to parse
     */
    public static void parseLUDPports(packHolder pck) {
        pck.UDPsrc = pck.msbGetW(0); // source port
        pck.UDPtrg = pck.msbGetW(2); // target port
    }

    /**
     * parse ludp header
     *
     * @param pck packet to parse
     * @return false if successful, true if error happened
     */
    public static boolean parseLUDPheader(packHolder pck) {
        parseLUDPports(pck);
        int sumLen = pck.msbGetW(4); // length (hdr incl)
        if (sumLen == 0) {
            sumLen = pck.dataSize();
        }
        if (sumLen < size) {
            logger.info("got too small from " + pck.IPsrc);
            return true;
        }
        if (sumLen > pck.dataSize()) {
            logger.info("got truncated from " + pck.IPsrc);
            return true;
        }
        if (cfgAll.ludpChecksumRx) {
            int sum = pck.msbGetW(6); // checksum
            if (sum != 0) {
                int i = pck.pseudoIPsum(pck.dataSize());
                i = pck.getIPsum(0, sumLen, i);
                if (i != 0xffff) {
                    logger.info("got bad checksum from " + pck.IPsrc);
                    return true;
                }
            }
        }
        if (debugger.prtLudpTraf) {
            logger.debug("rx " + pck.UDPsrc + " -> " + pck.UDPtrg);
        }
        pck.getSkip(size);
        pck.UDPsiz = size;
        return false;
    }

    /**
     * update ludp header
     *
     * @param pck packet to work with
     * @param src source port
     * @param trg target port
     */
    public static void updateLUDPheader(packHolder pck, int src, int trg) {
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
        pck.msbPutW(4, size); // length (hdr incl)
        pck.lsbPutW(6, 0); // checksum
        if (cfgAll.ludpChecksumTx) {
            int i = pck.pseudoIPsum(size + pck.dataSize());
            i = pck.putIPsum(0, size, i);
            pck.lsbPutW(6, 0xffff - i); // checksum
        }
        pck.putSkip(size);
        pck.merge2beg();
    }

    public String toString() {
        return "ludp on " + fwdCore;
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
        clnt.sendPRT = protoNum;
        clnt.setReady();
        return false;
    }

    protected void connectionRefuse(ipFwdIface ifc, packHolder pck) {
        pck.getSkip(-size - pck.IPsiz);
        fwdCore.doDrop(pck, ifc, counter.reasons.badPort);
    }

    /**
     * close connection
     *
     * @param clnt client
     */
    protected void connectionClose(prtGenConn clnt) {
        clnt.deleteImmediately();
    }

    /**
     * work connection
     *
     * @param clnt client
     */
    protected void connectionWork(prtGenConn clnt) {
    }

    /**
     * bytes available
     *
     * @param ntry client
     * @return bytes
     */
    protected int connectionBytes(prtGenConn ntry) {
        return 0xffffff;
    }

    /**
     * received packet
     *
     * @param clnt client
     * @param pck packet
     */
    protected void connectionRcvd(prtGenConn clnt, packHolder pck) {
        clnt.send2server(pck);
    }

    /**
     * send packet
     *
     * @param clnt client
     * @param pck packet
     * @return false if success, true if error
     */
    protected boolean connectionSend(prtGenConn clnt, packHolder pck) {
        pck.IPttl = clnt.sendTTL;
        pck.IPtos = clnt.sendTOS;
        createLUDPheader(pck);
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
        if (parseLUDPheader(pck)) {
            cntr.drop(pck, counter.reasons.badHdr);
            return;
        }
        if (debugger.prtLudpTraf) {
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
        parseLUDPports(pck);
        if (debugger.prtLudpTraf) {
            logger.debug(counter.reason2string(err) + " " + pck.UDPsrc + " -> " + pck.UDPtrg);
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
