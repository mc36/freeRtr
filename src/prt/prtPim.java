package prt;

import addr.addrEmpty;
import addr.addrIP;
import addr.addrType;
import ifc.ifcDn;
import ifc.ifcEther;
import ifc.ifcNull;
import ifc.ifcUp;
import ip.ipFwd;
import ip.ipFwdIface;
import ip.ipPrt;
import pack.packHolder;
import pack.packPim;
import util.counter;
import util.debugger;
import util.logger;
import util.state;

/**
 * handle pim register packets
 *
 * @author matecsaba
 */
public class prtPim implements ipPrt, ifcDn {

    /**
     * sending ttl value, -1 means maps out
     */
    public int sendingTTL = 255;

    /**
     * sending tos value, -1 means maps out
     */
    public int sendingTOS = -1;

    private ipFwdIface sendingIfc;

    private ifcUp upper = new ifcNull();

    private ipFwd lower;

    private addrIP remote;

    private counter cntr = new counter();

    /**
     * get counter
     *
     * @return counter
     */
    public counter getCounter() {
        return cntr;
    }

    /**
     * initialize context
     *
     * @param parent forwarder of encapsulated packets
     */
    public prtPim(ipFwd parent) {
        lower = parent;
    }

    /**
     * set target of tunnel
     *
     * @param ifc interface to source from
     * @param trg ip address of remote
     * @return false if successful, true if error happened
     */
    public boolean setEndpoints(ipFwdIface ifc, addrIP trg) {
        if (sendingIfc != null) {
            lower.protoDel(this, sendingIfc, remote);
        }
        remote = trg;
        sendingIfc = ifc;
        return lower.protoAdd(this, sendingIfc, remote);
    }

    /**
     * get protocol number
     *
     * @return number
     */
    public int getProtoNum() {
        return packPim.proto;
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
     * get state
     *
     * @return state
     */
    public state.states getState() {
        return state.states.up;
    }

    /**
     * received packet
     *
     * @param rxIfc interface
     * @param pck packet
     */
    public void recvPack(ipFwdIface rxIfc, packHolder pck) {
        cntr.rx(pck);
        if (debugger.prtPimTraf) {
            logger.debug("rx " + pck.IPsrc + " -> " + pck.IPtrg + " pr=" + pck.IPprt);
        }
        if (pck.IPprt != packPim.proto) {
            cntr.drop(pck, counter.reasons.badProto);
            return;
        }
        if (pck.IPsrc.compare(pck.IPsrc, remote) != 0) {
            cntr.drop(pck, counter.reasons.badSrcAddr);
            return;
        }
        if (pck.getByte(0) != 0x21) { // type + version
            logger.info("got bad version from " + remote);
            cntr.drop(pck, counter.reasons.badVer);
            return;
        }
        if (pck.IPtrg.isIPv4()) {
            if (pck.getIPsum(0, packPim.size + packPim.size, 0) != 0xffff) { // sum
                logger.info("got bad checksum from " + remote);
                cntr.drop(pck, counter.reasons.badSum);
                return;
            }
        } else {
            int i = pck.pseudoIPsum(packPim.size + packPim.size);
            if (pck.getIPsum(0, packPim.size + packPim.size, i) != 0xffff) { // sum
                logger.info("got bad checksum from " + remote);
                cntr.drop(pck, counter.reasons.badSum);
                return;
            }
        }
        pck.getSkip(packPim.size + packPim.size);
        int i = ifcEther.guessEtherType(pck);
        if (i < 0) {
            logger.info("got bad protocol from " + remote);
            cntr.drop(pck, counter.reasons.badVer);
            return;
        }
        pck.msbPutW(0, i);
        i = pck.headSize();
        pck.putSkip(2);
        pck.mergeHeader(-1, i);
        upper.recvPack(pck);
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
    }

    /**
     * send packet
     *
     * @param pck packet
     */
    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        if (ifcEther.stripEtherType(pck)) {
            cntr.drop(pck, counter.reasons.badProto);
            return;
        }
        if (debugger.prtPimTraf) {
            logger.debug("tx " + pck.IPsrc + " -> " + pck.IPtrg + " pr=" + pck.IPprt);
        }
        pck.IPprt = packPim.proto;
        pck.IPtrg.setAddr(remote);
        pck.IPsrc.setAddr(sendingIfc.addr);
        pck.putByte(0, 0x21); // type + version
        pck.putByte(1, 0); // reserved
        pck.msbPutW(2, 0); // checksum
        pck.msbPutD(4, 0); // flags
        if (pck.IPsrc.isIPv4()) {
            int i = pck.putIPsum(0, packPim.size + packPim.size, 0);
            pck.lsbPutW(2, 0xffff - i); // checksum
        } else {
            int i = pck.pseudoIPsum(packPim.size + packPim.size);
            i = pck.putIPsum(0, packPim.size + packPim.size, i);
            pck.lsbPutW(2, 0xffff - i); // checksum
        }
        pck.putSkip(packPim.size + packPim.size);
        pck.merge2beg();
        pck.putDefaults();
        if (sendingTTL >= 0) {
            pck.IPttl = sendingTTL;
        }
        if (sendingTOS >= 0) {
            pck.IPtos = sendingTOS;
        }
        lower.protoPack(sendingIfc, pck);
    }

    public String toString() {
        return "pim to " + remote;
    }

    /**
     * get mtu size
     *
     * @return mtu size
     */
    public int getMTUsize() {
        return sendingIfc.mtu;
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
