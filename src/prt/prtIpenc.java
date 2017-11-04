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
 * internet encapsulation protocol (rfc1241) packets
 *
 * @author matecsaba
 */
public class prtIpenc implements ipPrt, ifcDn {

    /**
     * protocol number
     */
    public static final int proto = 98;

    /**
     * header size
     */
    public static final int size = 8;

    /**
     * sending ttl value, -1 means maps out
     */
    public int sendingTTL = 255;

    /**
     * sending tos value, -1 means maps out
     */
    public int sendingTOS = -1;

    /**
     * flow id
     */
    public int flowId;

    private ipFwdIface sendingIfc;

    private ifcUp upper = new ifcNull();

    private ipFwd lower;

    private addrIP remote;

    private counter cntr = new counter();

    public counter getCounter() {
        return cntr;
    }

    /**
     * initialize context
     *
     * @param parent forwarder of encapsulated packets
     */
    public prtIpenc(ipFwd parent) {
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

    public int getProtoNum() {
        return proto;
    }

    public void closeDn() {
        lower.protoDel(this, sendingIfc, remote);
    }

    public void flapped() {
    }

    public void closeUp(ipFwdIface iface) {
        upper.closeUp();
    }

    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
    }

    public void setState(ipFwdIface iface, state.states stat) {
        if (iface.ifwNum != sendingIfc.ifwNum) {
            return;
        }
        upper.setState(stat);
    }

    public addrType getHwAddr() {
        return new addrEmpty();
    }

    public void setFilter(boolean promisc) {
    }

    public state.states getState() {
        return state.states.up;
    }

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
        if (pck.getByte(0) != 0x18) { // version + header length
            logger.info("got bad version from " + remote);
            return;
        }
        if (pck.getByte(1) != 0x10) { // type + reason
            logger.info("got bad type from " + remote);
            return;
        }
        if (pck.getIPsum(0, size, 0) != 0xffff) { // sum
            logger.info("got bad checksum from " + remote);
            cntr.drop(pck, counter.reasons.badSum);
            return;
        }
        if (pck.msbGetD(4) != flowId) { // flow id
            logger.info("got bad flowid from " + remote);
            return;
        }
        pck.getSkip(size);
        int i = ifcEther.guessEtherType(pck);
        if (i < 0) {
            logger.info("got bad protocol from " + remote);
            return;
        }
        pck.msbPutW(0, i); // ethertype
        pck.putSkip(2);
        pck.merge2beg();
        upper.recvPack(pck);
    }

    public boolean alertPack(ipFwdIface rxIfc, packHolder pck) {
        return true;
    }

    public void errorPack(counter.reasons err, addrIP rtr, ipFwdIface rxIfc, packHolder pck) {
    }

    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        if (ifcEther.stripEtherType(pck)) {
            cntr.drop(pck, counter.reasons.badProto);
            return;
        }
        pck.putByte(0, 0x18); // version + header length
        pck.putByte(1, 0x10); // type + reason
        pck.msbPutW(2, 0); // checksum
        pck.msbPutD(4, flowId); // flow id
        pck.lsbPutW(2, 0xffff - pck.putIPsum(0, size, 0)); // checksum
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
        return "ipenc to " + remote;
    }

    public int getMTUsize() {
        return sendingIfc.mtu - size;
    }

    public long getBandwidth() {
        return sendingIfc.bandwidth;
    }

}
