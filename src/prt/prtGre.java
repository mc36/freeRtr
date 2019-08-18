package prt;

import ifc.ifcDn;
import ifc.ifcNull;
import ifc.ifcUp;
import ip.ipFwd;
import ip.ipFwdIface;
import ip.ipPrt;
import pack.packHolder;
import util.counter;
import util.debugger;
import util.logger;
import util.state;
import addr.addrEmpty;
import addr.addrIP;
import addr.addrType;

/**
 * handle gre (rfc2784) packets
 *
 * @author matecsaba
 */
public class prtGre implements ipPrt, ifcDn {

    /**
     * protocol number of my
     */
    public final static int protoNum = 47;

    /**
     * size of my header
     */
    public final static int size = 4;

    /**
     * sending ttl value, -1 means maps out
     */
    public int sendingTTL = 255;

    /**
     * sending tos value, -1 means maps out
     */
    public int sendingTOS = -1;

    /**
     * tunnel key to use, 0 means disabled
     */
    public int tunnelKey = 0;

    /**
     * tunnel mask to use, -1 means disabled
     */
    public int tunnelMsk = -1;

    /**
     * send checksum in packets
     */
    public boolean tunnelSum = false;

    /**
     * send sequence number in packets
     */
    public boolean tunnelSeq = false;

    private ipFwdIface sendingIfc;

    private ifcUp upper = new ifcNull();

    private ipFwd lower;

    private addrIP remote = new addrIP();

    private counter cntr = new counter();

    private int seqTx;

    /**
     * initialize context
     *
     * @param parent forwarder of encapsulated packets
     */
    public prtGre(ipFwd parent) {
        lower = parent;
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
     * get protocol number
     *
     * @return number
     */
    public int getProtoNum() {
        return protoNum;
    }

    /**
     * close interface
     *
     * @param iface interface
     */
    public void closeUp(ipFwdIface iface) {
        upper.closeUp();
    }

    /**
     * set upper layer
     *
     * @param server upper layer
     */
    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
    }

    /**
     * set state
     *
     * @param iface interface
     * @param stat state
     */
    public void setState(ipFwdIface iface, state.states stat) {
        if (iface.ifwNum != sendingIfc.ifwNum) {
            return;
        }
        upper.setState(stat);
    }

    /**
     * get hw address
     *
     * @return hw address
     */
    public addrType getHwAddr() {
        return new addrEmpty();
    }

    /**
     * set filter
     *
     * @param promisc promiscous mode
     */
    public void setFilter(boolean promisc) {
    }

    /**
     * close interface
     */
    public void closeDn() {
        lower.protoDel(this, sendingIfc, remote);
    }

    /**
     * flap interface
     */
    public void flapped() {
    }

    /**
     * get state
     *
     * @return state
     */
    public state.states getState() {
        return state.states.up;
    }

    /**
     * set target of tunnel
     *
     * @param ifc interface to source from
     * @param trg ip address of remote
     * @param reg register to lower layer
     * @return false if successful, true if error happened
     */
    public boolean setEndpoints(ipFwdIface ifc, addrIP trg, boolean reg) {
        if (!reg) {
            remote = trg;
            sendingIfc = ifc;
            return false;
        }
        if (sendingIfc != null) {
            lower.protoDel(this, sendingIfc, remote);
        }
        remote = trg;
        sendingIfc = ifc;
        return lower.protoAdd(this, sendingIfc, remote);
    }

    /**
     * received packet
     *
     * @param rxIface interface
     * @param pck packet
     */
    public void recvPack(ipFwdIface rxIface, packHolder pck) {
        cntr.rx(pck);
        if (debugger.prtGreTraf) {
            logger.debug("rx " + pck.IPsrc + " -> " + pck.IPtrg + " pr=" + pck.IPprt);
        }
        if (pck.IPprt != protoNum) {
            cntr.drop(pck, counter.reasons.badProto);
            return;
        }
        if (pck.IPsrc.compare(pck.IPsrc, remote) != 0) {
            cntr.drop(pck, counter.reasons.badSrcAddr);
            return;
        }
        int hdr = pck.msbGetW(0); // header
        int typ = pck.msbGetW(2); // ethertype
        int vers = hdr & 7; // version
        boolean sump = (hdr & 0x8000) != 0; // sum present
        boolean keyp = (hdr & 0x2000) != 0; // key present
        boolean seqp = (hdr & 0x1000) != 0; // seq present
        pck.getSkip(size);
        if (vers != 0) {
            logger.info("got bad version from " + remote);
            cntr.drop(pck, counter.reasons.badVer);
            return;
        }
        if (sump != tunnelSum) {
            logger.info("got mismatching header from " + remote);
            cntr.drop(pck, counter.reasons.badHdr);
            return;
        }
        if (keyp != (tunnelKey != 0)) {
            logger.info("got mismatching header from " + remote);
            cntr.drop(pck, counter.reasons.badHdr);
            return;
        }
        if (seqp != tunnelSeq) {
            logger.info("got mismatching header from " + remote);
            cntr.drop(pck, counter.reasons.badHdr);
            return;
        }
        if (sump) {
            int sum = pck.getIPsum(-size, pck.dataSize() + size, 0);
            pck.getSkip(size);
            if (sum != 0xffff) {
                logger.info("got invalid checksum from " + remote);
                cntr.drop(pck, counter.reasons.badSum);
                return;
            }
        }
        if (keyp) {
            int key = pck.msbGetD(0); // key
            pck.getSkip(size);
            if ((key & tunnelMsk) != tunnelKey) {
                logger.info("got bad key from " + remote);
                cntr.drop(pck, counter.reasons.badKey);
                return;
            }
        }
        if (seqp) {
            // pck.msbGetD(0); // sequence
            pck.getSkip(size);
        }
        pck.putStart();
        pck.msbPutW(0, typ);
        pck.putSkip(2);
        pck.merge2beg();
        upper.recvPack(pck);
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

    /**
     * send packet
     *
     * @param pck packet
     */
    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        int typ = pck.msbGetW(0);
        pck.getSkip(2);
        if (debugger.prtGreTraf) {
            logger.debug("tx typ=" + typ);
        }
        int hdr = 0;
        if (tunnelSum) {
            hdr |= 0x8000;
        }
        if (tunnelKey != 0) {
            hdr |= 0x2000;
        }
        if (tunnelSeq) {
            hdr |= 0x1000;
        }
        pck.msbPutW(0, hdr); // header
        pck.msbPutW(2, typ); // ethertype
        pck.putSkip(size);
        if (tunnelSum) {
            pck.msbPutD(0, 0); // sum
            pck.putSkip(size);
        }
        if (tunnelKey != 0) {
            pck.msbPutD(0, tunnelKey); // key
            pck.putSkip(size);
        }
        if (tunnelSeq) {
            seqTx++;
            pck.msbPutD(0, seqTx); // sequence
            pck.putSkip(size);
        }
        if (tunnelSum) {
            pck.merge2beg();
            int sum = pck.getIPsum(0, pck.dataSize(), 0);
            pck.unMergeBytes(size * 2);
            pck.lsbPutW(-size, 0xffff - sum); // checksum
        }
        pck.merge2beg();
        pck.putDefaults();
        if (sendingTTL >= 0) {
            pck.IPttl = sendingTTL;
        }
        if (sendingTOS >= 0) {
            pck.IPtos = sendingTOS;
        }
        pck.IPprt = protoNum;
        pck.IPsrc.setAddr(sendingIfc.addr);
        pck.IPtrg.setAddr(remote);
        lower.protoPack(sendingIfc, pck);
    }

    public String toString() {
        return "gre to " + remote;
    }

    /**
     * get mtu size
     *
     * @return mtu size
     */
    public int getMTUsize() {
        int i = sendingIfc.mtu - size;
        if (tunnelKey != 0) {
            i -= size;
        }
        if (tunnelSum) {
            i -= size;
        }
        if (tunnelSeq) {
            i -= size;
        }
        return i;
    }

    /**
     * get bandwidth
     *
     * @return bandwidth
     */
    public long getBandwidth() {
        return sendingIfc.bandwidth;
    }

}
