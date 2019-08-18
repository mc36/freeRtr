package prt;

import addr.addrIP;
import addr.addrMac;
import addr.addrType;
import cry.cryEncrGeneric;
import cry.cryHashGeneric;
import ifc.ifcDn;
import ifc.ifcNull;
import ifc.ifcUp;
import ip.ipFwd;
import ip.ipFwdIface;
import ip.ipPrt;
import pack.packHolder;
import sec.secTransform;
import util.bits;
import util.counter;
import util.state;
import util.logger;

/**
 * simple key management for internet protocols client
 *
 * @author matecsaba
 */
public class prtSkip implements ipPrt, ifcDn {

    /**
     * protocol number
     */
    public static final int prot = 57;

    /**
     * size of header
     */
    public static final int size = 12;

    /**
     * upper layer
     */
    public ifcUp upper = new ifcNull();

    /**
     * sending ttl value, -1 means maps out
     */
    public int sendingTTL = 255;

    /**
     * sending tos value, -1 means maps out
     */
    public int sendingTOS = -1;

    /**
     * preshared key
     */
    public String preshared;

    /**
     * transform set to use
     */
    public secTransform transform;

    /**
     * counter
     */
    public counter cntr = new counter();

    private ipFwd lower;

    private addrIP remote;

    private ipFwdIface sendingIfc;

    private int seqTx;

    private cryEncrGeneric cphrTx;

    private cryEncrGeneric cphrRx;

    private cryHashGeneric hashTx;

    private cryHashGeneric hashRx;

    private int cphrSiz;

    private int hashSiz;

    /**
     * initialize context
     *
     * @param parent forwarder of encapsulated packets
     */
    public prtSkip(ipFwd parent) {
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
        byte[] buf1 = preshared.getBytes();
        byte[] buf2 = new byte[0];
        for (; buf2.length < 1024;) {
            cryHashGeneric hsh = transform.getHash();
            hsh.init();
            hsh.update(buf1);
            buf1 = hsh.finish();
            buf2 = bits.byteConcat(buf2, buf1);
        }
        cphrTx = transform.getEncr();
        cphrRx = transform.getEncr();
        byte[] res = buf2;
        buf1 = new byte[cphrTx.getKeySize()];
        buf2 = new byte[cphrTx.getBlockSize()];
        int pos = buf1.length + buf2.length;
        bits.byteCopy(res, 0, buf1, 0, buf1.length);
        bits.byteCopy(res, buf1.length, buf2, 0, buf2.length);
        cphrTx.init(buf1, buf2, true);
        cphrRx.init(buf1, buf2, false);
        cphrSiz = buf2.length;
        hashSiz = transform.getHash().getHashSize();
        buf1 = new byte[hashSiz];
        buf2 = new byte[hashSiz];
        bits.byteCopy(res, pos, buf1, 0, buf1.length);
        bits.byteCopy(res, pos, buf2, 0, buf2.length);
        hashTx = transform.getHmac(buf1);
        hashRx = transform.getHmac(buf2);
        if (sendingIfc != null) {
            lower.protoDel(this, sendingIfc, remote);
        }
        remote = trg;
        sendingIfc = ifc;
        return lower.protoAdd(this, sendingIfc, remote);
    }

    public String toString() {
        return "skip to " + remote;
    }

    /**
     * get hw address
     *
     * @return hw address
     */
    public addrType getHwAddr() {
        return addrMac.getRandom();
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
     * set upper layer
     *
     * @param server upper layer
     */
    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
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

    private int getAlgs() {
        return (transform.encrAlg << 16) | (transform.hashAlg << 8);
    }

    /**
     * send packet
     *
     * @param pck packet
     */
    public synchronized void sendPack(packHolder pck) {
        pck.merge2beg();
        if (sendingIfc == null) {
            return;
        }
        int i = pck.msbGetW(0); // ethertype
        pck.getSkip(2);
        int o = prtTmux.ethtyp2proto(i);
        if (o < 0) {
            return;
        }
        i = pck.dataSize() % cphrSiz;
        if (i > 0) {
            i = cphrSiz - i;
            pck.putFill(0, i, 0); // padding
            pck.putSkip(i);
            pck.merge2end();
        }
        hashTx.init();
        pck.hashData(hashTx, 0, pck.dataSize());
        byte[] hsh = hashTx.finish();
        byte[] buf = new byte[cphrSiz];
        for (i = 0; i < buf.length; i++) {
            buf[i] = (byte) bits.randomB();
        }
        pck.putCopy(buf, 0, 0, buf.length);
        pck.putSkip(buf.length);
        pck.merge2beg();
        pck.encrData(cphrTx, 0, pck.dataSize());
        pck.putCopy(hsh, 0, 0, hsh.length);
        pck.putSkip(hsh.length);
        pck.merge2beg();
        seqTx++;
        pck.msbPutD(0, 0x10000000 | o); // header
        pck.msbPutD(4, seqTx); // sequence
        pck.msbPutD(8, getAlgs()); // algorithms
        pck.putSkip(size);
        pck.merge2beg();
        cntr.tx(pck);
        pck.putDefaults();
        if (sendingTTL >= 0) {
            pck.IPttl = sendingTTL;
        }
        if (sendingTOS >= 0) {
            pck.IPtos = sendingTOS;
        }
        pck.IPprt = prot;
        pck.IPsrc.setAddr(sendingIfc.addr);
        pck.IPtrg.setAddr(remote);
        lower.protoPack(sendingIfc, pck);
    }

    /**
     * get protocol number
     *
     * @return number
     */
    public int getProtoNum() {
        return prot;
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
     * received packet
     *
     * @param rxIfc interface
     * @param pck packet
     */
    public synchronized void recvPack(ipFwdIface rxIfc, packHolder pck) {
        int o = pck.msbGetD(0); // header
        if ((o >>> 24) != 0x10) {
            logger.info("got bad header from " + remote);
            cntr.drop(pck, counter.reasons.badHdr);
            return;
        }
        if (pck.msbGetD(8) != getAlgs()) {
            logger.info("got bad algorithm from " + remote);
            cntr.drop(pck, counter.reasons.badHdr);
            return;
        }
        pck.getSkip(size);
        o = prtTmux.proto2ethtyp(o & 0xff);
        if (o < 0) {
            logger.info("got bad protocol from " + remote);
            cntr.drop(pck, counter.reasons.badProto);
            return;
        }
        int siz = pck.dataSize();
        if (siz < (hashSiz + cphrSiz)) {
            logger.info("got too small from " + remote);
            cntr.drop(pck, counter.reasons.badLen);
            return;
        }
        if (((siz - hashSiz) % cphrSiz) != 0) {
            logger.info("got bad padding from " + remote);
            cntr.drop(pck, counter.reasons.badLen);
            return;
        }
        byte[] sum = new byte[hashSiz];
        pck.getCopy(sum, 0, 0, hashSiz);
        pck.getSkip(hashSiz);
        siz -= hashSiz;
        pck.encrData(cphrRx, 0, siz);
        pck.getSkip(cphrSiz);
        siz -= cphrSiz;
        hashRx.init();
        pck.hashData(hashRx, 0, siz);
        if (bits.byteComp(sum, 0, hashRx.finish(), 0, hashSiz) != 0) {
            logger.info("got bad hash from " + remote);
            cntr.drop(pck, counter.reasons.badSum);
            return;
        }
        pck.msbPutW(0, o); // ethertype
        pck.putSkip(2);
        pck.merge2beg();
        cntr.rx(pck);
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

}
