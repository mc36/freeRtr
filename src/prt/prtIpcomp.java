package prt;

import addr.addrEmpty;
import addr.addrIP;
import addr.addrType;
import ifc.ifcDn;
import ifc.ifcNull;
import ifc.ifcUp;
import ip.ipFwd;
import ip.ipFwdIface;
import ip.ipPrt;
import java.util.zip.Deflater;
import java.util.zip.Inflater;
import pack.packHolder;
import util.counter;
import util.state;
import util.logger;

/**
 * ip compression (rfc2393) packets
 *
 * @author matecsaba
 */
public class prtIpcomp implements ipPrt, ifcDn {

    /**
     * protocol number
     */
    public static final int proto = 108;

    /**
     * header size
     */
    public static final int size = 4;

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
    public prtIpcomp(ipFwd parent) {
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
        if (pck.msbGetW(2) != 2) { // compression parameter index
            logger.info("got bad index from " + remote);
            cntr.drop(pck, counter.reasons.badHdr);
            return;
        }
        int o = prtTmux.proto2ethtyp(pck.getByte(0)); // protocol
        if (o < 0) {
            logger.info("got bad type from " + remote);
            cntr.drop(pck, counter.reasons.badProto);
            return;
        }
        pck.getSkip(size);
        byte[] buf1 = pck.getCopy();
        pck.getSkip(buf1.length);
        byte[] buf2 = new byte[packHolder.maxData];
        Inflater decomp = new Inflater();
        decomp.setInput(buf1);
        int i;
        try {
            i = decomp.inflate(buf2);
        } catch (Exception e) {
            logger.info("got bad compressed from " + remote);
            cntr.drop(pck, counter.reasons.badSum);
            return;
        }
        pck.putCopy(buf2, 0, 0, i);
        pck.putSkip(i);
        pck.merge2beg();
        pck.msbPutW(0, o);
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
     *
     * @param pck
     */
    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        int o = pck.msbGetW(0);
        pck.getSkip(2);
        o = prtTmux.ethtyp2proto(o);
        if (o < 0) {
            cntr.drop(pck, counter.reasons.badProto);
            return;
        }
        byte[] buf1 = pck.getCopy();
        pck.getSkip(buf1.length);
        byte[] buf2 = new byte[packHolder.maxData];
        Deflater comp = new Deflater();
        comp.setInput(buf1);
        comp.finish();
        int i = comp.deflate(buf2);
        pck.putCopy(buf2, 0, 0, i);
        pck.putSkip(i);
        pck.merge2beg();
        pck.msbPutW(0, o << 8);
        pck.msbPutW(2, 2); // compression parameter index
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
        return "ipcomp to " + remote;
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
