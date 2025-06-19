package org.freertr.prt;

import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrType;
import org.freertr.clnt.clntSrEth;
import org.freertr.ifc.ifcBridge;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcMacSec;
import org.freertr.ifc.ifcNshFwd;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcSgt;
import org.freertr.ifc.ifcUp;
import org.freertr.ip.ipCor4;
import org.freertr.ip.ipCor6;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipIfc4;
import org.freertr.ip.ipIfc6;
import org.freertr.ip.ipMpls;
import org.freertr.ip.ipPrt;
import org.freertr.ipx.ipxIface;
import org.freertr.pack.packHolder;
import org.freertr.rtr.rtrIsis;
import org.freertr.rtr.rtrNshIface;
import org.freertr.util.counter;
import org.freertr.util.logger;
import org.freertr.util.state;

/**
 * transport multiplexing protocol (rfc1692) packets
 *
 * @author matecsaba
 */
public class prtTmux implements ipPrt, ifcDn {

    /**
     * protocol number
     */
    public final static int proto = 18;

    /**
     * header size
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
     * sending df value, -1 means maps out
     */
    public int sendingDFN = -1;

    /**
     * sending flow value, -1 means maps out
     */
    public int sendingFLW = -1;

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
    public prtTmux(ipFwd parent) {
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

    private int chksum(int len, int prt) {
        return (len & 0xff) ^ (len >>> 8) ^ prt;
    }

    /**
     * convert ethertype to protocol number
     *
     * @param i ethertype
     * @return protocol number, -1 if nothing
     */
    public static int ethtyp2proto(int i) {
        switch (i) {
            case ipMpls.typeU:
            case ipMpls.typeM:
            case ipMpls.typeB:
                return prtMplsIp.prot;
            case ipIfc4.type:
                return ipCor4.protocolNumber;
            case ipIfc6.type:
                return ipCor6.protocolNumber;
            case ifcMacSec.ethtyp:
                return prtSwipe.prot;
            case ifcSgt.type:
                return prtSkip.prot;
            case ifcNshFwd.type:
                return rtrNshIface.protoNum;
            case ifcBridge.serialType:
                return clntSrEth.prot;
            case rtrIsis.ethTyp:
                return prtIsoip.proto;
            case ipxIface.type:
                return prtIpxip.proto;
            default:
                return -1;
        }
    }

    /**
     * convert protocol number to ethertype
     *
     * @param i protocol number
     * @return ethertype, -1 if nothing
     */
    public static int proto2ethtyp(int i) {
        switch (i) {
            case prtMplsIp.prot:
                return ipMpls.typeU;
            case ipCor4.protocolNumber:
                return ipIfc4.type;
            case ipCor6.protocolNumber:
                return ipIfc6.type;
            case prtSwipe.prot:
                return ifcMacSec.ethtyp;
            case prtSkip.prot:
                return ifcSgt.type;
            case rtrNshIface.protoNum:
                return ifcNshFwd.type;
            case clntSrEth.prot:
                return ifcBridge.serialType;
            case prtIsoip.proto:
                return rtrIsis.ethTyp;
            case prtIpxip.proto:
                return ipxIface.type;
            default:
                return -1;
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
        if (pck.IPprt != proto) {
            cntr.drop(pck, counter.reasons.badProto);
            return;
        }
        if (pck.IPsrc.compareTo(remote) != 0) {
            cntr.drop(pck, counter.reasons.badSrcAddr);
            return;
        }
        int i = pck.msbGetW(0); // length
        if (i < size) {
            logger.info("got bad size from " + remote);
            cntr.drop(pck, counter.reasons.badSiz);
            return;
        }
        int o = pck.getByte(2); // protocol
        if (pck.getByte(3) != chksum(i, o)) {
            logger.info("got bad checksum from " + remote);
            cntr.drop(pck, counter.reasons.badSum);
            return;
        }
        if (i > pck.dataSize()) {
            logger.info("got truncated from " + remote);
            cntr.drop(pck, counter.reasons.badLen);
            return;
        }
        pck.setDataSize(i);
        pck.getSkip(size);
        i = proto2ethtyp(o);
        if (i < 0) {
            logger.info("got bad protocol from " + remote);
            cntr.drop(pck, counter.reasons.badProto);
            return;
        }
        pck.msbPutW(0, i); // ethertype
        pck.putSkip(2);
        pck.merge2beg();
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
        int i = pck.msbGetW(0);
        pck.getSkip(2);
        int o = ethtyp2proto(i);
        if (o < 0) {
            cntr.drop(pck, counter.reasons.badProto);
            return;
        }
        i = pck.dataSize() + size;
        pck.msbPutW(0, i); // length
        pck.putByte(2, o); // protocol
        pck.putByte(3, chksum(i, o)); // checksum
        pck.putSkip(size);
        pck.merge2beg();
        pck.putDefaults();
        if (sendingTTL >= 0) {
            pck.IPttl = sendingTTL;
        }
        if (sendingTOS >= 0) {
            pck.IPtos = sendingTOS;
        }
        if (sendingDFN >= 0) {
            pck.IPdf = sendingDFN == 1;
        }
        if (sendingFLW >= 0) {
            pck.IPid = sendingFLW;
        }
        pck.IPprt = proto;
        pck.IPtrg.setAddr(remote);
        pck.IPsrc.setAddr(sendingIfc.addr);
        lower.protoPack(sendingIfc, null, pck);
    }

    public String toString() {
        return "tmux to " + remote;
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

    /**
     * get remote address
     *
     * @return peer address, null if no session
     */
    public addrIP getAddrRem() {
        return remote;
    }

    /**
     * get local address
     *
     * @return peer address, null if no session
     */
    public addrIP getAddrLoc() {
        return sendingIfc.addr;
    }

    /**
     * get local address
     *
     * @return peer address, null if no session
     */
    public ipFwd getFwd() {
        return lower;
    }

}
