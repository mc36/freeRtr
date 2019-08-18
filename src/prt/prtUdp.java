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
 * handle udp (rfc768) connections
 *
 * @author matecsaba
 */
public class prtUdp extends prtGen {

    /**
     * size of udp header
     */
    public final static int size = 8;

    /**
     * protocol number of udp
     */
    public final static int protoNum = 17;

    /**
     * create new instance
     *
     * @param ifw forwarder to use
     */
    public prtUdp(ipFwd ifw) {
        fwdCore = ifw;
        ifw.protoAdd(this, null, null);
    }

    /**
     * create new instance
     */
    public prtUdp() {
    }

    /**
     * create udp header
     *
     * @param pck packet to update
     */
    public static void createUDPheader(packHolder pck) {
        pck.IPprt = protoNum;
        if (debugger.prtUdpTraf) {
            logger.debug("tx " + pck.UDPsrc + " -> " + pck.UDPtrg);
        }
        pck.msbPutW(0, pck.UDPsrc); // source port
        pck.msbPutW(2, pck.UDPtrg); // target port
        pck.msbPutW(4, size + pck.dataSize()); // length (hdr incl)
        pck.lsbPutW(6, 0); // checksum
        if (cfgAll.udpChecksumTx) {
            int i = pck.pseudoIPsum(size + pck.dataSize());
            i = pck.putIPsum(0, size, i);
            i = pck.getIPsum(0, pck.dataSize(), i);
            pck.lsbPutW(6, 0xffff - i); // checksum
        }
        pck.putSkip(size);
        pck.merge2beg();
    }

    /**
     * parse udp ports
     *
     * @param pck packet to parse
     */
    public static void parseUDPports(packHolder pck) {
        pck.UDPsrc = pck.msbGetW(0); // source port
        pck.UDPtrg = pck.msbGetW(2); // target port
    }

    /**
     * parse udp header
     *
     * @param pck packet to parse
     * @return false if successful, true if error happened
     */
    public static boolean parseUDPheader(packHolder pck) {
        parseUDPports(pck);
        int totLen = pck.msbGetW(4); // length (hdr incl)
        if (totLen < size) {
            logger.info("got too small from " + pck.IPsrc);
            return true;
        }
        if (totLen > pck.dataSize()) {
            logger.info("got truncated from " + pck.IPsrc);
            return true;
        }
        if (cfgAll.udpChecksumRx) {
            int sum = pck.msbGetW(6); // checksum
            if (sum != 0) {
                int i = pck.pseudoIPsum(totLen);
                i = pck.getIPsum(0, totLen, i);
                if (i != 0xffff) {
                    logger.info("got bad checksum from " + pck.IPsrc);
                    return true;
                }
            }
        }
        pck.setDataSize(totLen);
        if (debugger.prtUdpTraf) {
            logger.debug("rx " + pck.UDPsrc + " -> " + pck.UDPtrg);
        }
        pck.getSkip(size);
        pck.UDPsiz = size;
        return false;
    }

    /**
     * update udp header
     *
     * @param pck packet to work with
     * @param src source port
     * @param trg target port
     */
    public static void updateUDPheader(packHolder pck, int src, int trg) {
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
        if (cfgAll.udpChecksumTx) {
            int i = pck.pseudoIPsum(size + pck.dataSize());
            i = pck.putIPsum(0, size, i);
            i = pck.getIPsum(0, pck.dataSize(), i);
            pck.lsbPutW(6, 0xffff - i); // checksum
        }
        pck.putSkip(size);
        pck.merge2beg();
    }

    /**
     * convert to string
     *
     * @return string
     */
    public String toString() {
        return "udp on " + fwdCore;
    }

    /**
     * get protocol number
     *
     * @return number
     */
    public int getProtoNum() {
        return protoNum;
    }

    /**
     * test port
     *
     * @param i ort
     * @return false if success, true if error
     */
    protected boolean testPortNumber(int i) {
        if (i < 1) {
            return true;
        }
        if (i > 65535) {
            return true;
        }
        return false;
    }

    /**
     * get random port
     *
     * @return port
     */
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

    /**
     * refuse connection
     *
     * @param ifc interface
     * @param pck packet
     */
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
        createUDPheader(pck);
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
        if (parseUDPheader(pck)) {
            cntr.drop(pck, counter.reasons.badHdr);
            return;
        }
        if (debugger.prtUdpTraf) {
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
        parseUDPports(pck);
        if (debugger.prtUdpTraf) {
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
