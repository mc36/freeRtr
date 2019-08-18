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
import util.counter;
import util.state;
import util.logger;

/**
 * private ip ip encapsulation packets
 *
 * @author matecsaba
 */
public class prtPipe implements ipPrt, ifcDn {

    /**
     * protocol number
     */
    public static final int proto = 131;

    /**
     * header size
     */
    public static final int size = 8;

    /**
     * magic number
     */
    public static final int magic = 0xe0000000;

    /**
     * sending ttl value, -1 means maps out
     */
    public int sendingTTL = 255;

    /**
     * sending tos value, -1 means maps out
     */
    public int sendingTOS = -1;

    /**
     * vpn id
     */
    public int vpnId;

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
    public prtPipe(ipFwd parent) {
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
        return proto;
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
     * @param iface
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
        if (pck.IPprt != proto) {
            cntr.drop(pck, counter.reasons.badProto);
            return;
        }
        if (pck.IPsrc.compare(pck.IPsrc, remote) != 0) {
            cntr.drop(pck, counter.reasons.badSrcAddr);
            return;
        }
        if (pck.msbGetD(0) != magic) { // sel + vpnoid
            logger.info("got bad magic from " + remote);
            cntr.drop(pck, counter.reasons.badHdr);
            return;
        }
        if (pck.msbGetD(4) != vpnId) { // vpnid
            logger.info("got bad vpnid from " + remote);
            cntr.drop(pck, counter.reasons.badID);
            return;
        }
        pck.getSkip(size);
        int i = ifcEther.guessEtherType(pck);
        if (i < 0) {
            logger.info("got bad protocol from " + remote);
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
        pck.msbPutD(0, magic); // sel + vpnoid
        pck.msbPutD(4, vpnId); // vpnid
        pck.putSkip(size);
        pck.merge2beg();
        pck.putDefaults();
        if (sendingTTL >= 0) {
            pck.IPttl = sendingTTL;
        }
        if (sendingTOS >= 0) {
            pck.IPtos = sendingTOS;
        }
        pck.IPprt = proto;
        pck.IPtrg.setAddr(remote);
        pck.IPsrc.setAddr(sendingIfc.addr);
        lower.protoPack(sendingIfc, pck);
    }

    public String toString() {
        return "pipe to " + remote;
    }

    /**
     * get mtu size
     *
     * @return mtu size
     */
    public int getMTUsize() {
        return sendingIfc.mtu - size;
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
