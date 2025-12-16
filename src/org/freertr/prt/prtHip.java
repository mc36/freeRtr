package org.freertr.prt;

import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrType;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcUp;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipPrt;
import org.freertr.pack.packHolder;
import org.freertr.util.counter;
import org.freertr.util.logger;
import org.freertr.util.state;

/**
 * host identity protocol (rfc5201) packets
 *
 * @author matecsaba
 */
public class prtHip implements ipPrt, ifcDn {

    /**
     * protocol number
     */
    public final static int proto = 139;

    /**
     * header size
     */
    public final static int size = 40;

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
     * initialize context
     *
     * @param parent forwarder of encapsulated packets
     */
    public prtHip(ipFwd parent) {
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

    public counter getCounter() {
        return cntr;
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
        int o = (pck.getByte(1) + 1) << 3; // header length
        if (o < size) {
            logger.info("got too small from " + pck.IPsrc);
            cntr.drop(pck, counter.reasons.tooSmall);
            return;
        }
        if (o > pck.dataSize()) {
            logger.info("got truncated from " + pck.IPsrc);
            cntr.drop(pck, counter.reasons.badSiz);
            return;
        }
        if (pck.getByte(3) != 0x10) {
            logger.info("got bad version from " + pck.IPsrc);
            cntr.drop(pck, counter.reasons.badVer);
            return;
        }
        if (pck.getByte(2) != 32) {
            logger.info("got bad type from " + pck.IPsrc);
            cntr.drop(pck, counter.reasons.badTyp);
            return;
        }
        int i = pck.pseudoIPsum(pck.dataSize());
        i = pck.getIPsum(0, pck.dataSize(), i);
        if (i != 0xffff) {
            logger.info("got bad checksum from " + pck.IPsrc);
            cntr.drop(pck, counter.reasons.badSum);
            return;
        }
        i = pck.getByte(0); // next header
        pck.getSkip(o);
        o = prtTmux.proto2ethtyp(i);
        if (o < 0) {
            logger.info("got bad protocol from " + remote);
            cntr.drop(pck, counter.reasons.badProto);
            return;
        }
        pck.msbPutW(0, o); // ethertype
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
        int i = pck.msbGetW(0);
        pck.getSkip(2);
        int o = prtTmux.ethtyp2proto(i);
        if (o < 0) {
            cntr.drop(pck, counter.reasons.badProto);
            return;
        }
        pck.putByte(0, o); // next header
        pck.putByte(1, (size >>> 3) - 1); // header length
        pck.putByte(2, 32); // packet type
        pck.putByte(3, 0x10); // version
        pck.msbPutW(4, 0); // checksum
        pck.msbPutW(6, 0); // controls
        pck.putAddr(8, sendingIfc.addr);
        pck.putAddr(24, remote);
        pck.IPprt = proto;
        pck.IPtrg.setAddr(remote);
        pck.IPsrc.setAddr(sendingIfc.addr);
        i = pck.pseudoIPsum(size + pck.dataSize());
        i = pck.putIPsum(0, size, i);
        i = pck.getIPsum(0, pck.dataSize(), i);
        pck.lsbPutW(4, 0xffff - i); // checksum
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
        lower.protoPack(sendingIfc, null, pck);
    }

    public int getMTUsize() {
        return sendingIfc.mtu - size;
    }

    public long getBandwidth() {
        return sendingIfc.bandwidth;
    }

}
