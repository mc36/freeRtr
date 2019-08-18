package pack;

import addr.addrIP;
import cry.cryEncrGeneric;
import cry.cryHashGeneric;
import ifc.ifcUp;
import ifc.ifcNull;
import ip.ipCor4;
import ip.ipCor6;
import ip.ipFwd;
import ip.ipFwdIface;
import ip.ipIfc4;
import ip.ipIfc6;
import ip.ipPrt;
import util.bits;
import util.counter;
import util.logger;
import util.state;
import tab.tabWindow;

/**
 * encapsulating security payload (rfc2407) packet
 *
 * @author matecsaba
 */
public class packEsp implements ipPrt {

    /**
     * protocol number
     */
    public final static int protoNum = 50;

    /**
     * ipv6 sa
     */
    public boolean ipv6 = false;

    /**
     * do replay checking
     */
    public int replayCheck = 1024;

    /**
     * last bad spi
     */
    public int badSpi;

    /**
     * spi value
     */
    protected int spi;

    /**
     * hasher function
     */
    protected cryHashGeneric hasher;

    /**
     * cipher function
     */
    protected cryEncrGeneric cipher;

    /**
     * size of hash
     */
    protected int hashSize;

    /**
     * size of cipher
     */
    protected int encrSize;

    private int seqTx;

    private tabWindow sequence;

    private ifcUp lower = new ifcNull();

    private addrIP peerAddr;

    private ipFwd forwarder;

    private ipFwdIface fwdIface;

    private counter cntr = new counter();

    private int sendingTTL = 255;

    private int sendingTOS = -1;

    public String toString() {
        return "esp to " + peerAddr;
    }

    /**
     * create esp handler
     *
     * @param parent parent
     */
    public packEsp(ifcUp parent) {
        lower = parent;
        doInit();
    }

    /**
     * setup endpoint details
     *
     * @param fwd forwarder
     * @param ifc interface
     * @param trg peer address
     * @param tos sending tos
     * @param ttl sending ttl
     */
    public void lowerSetup(ipFwd fwd, ipFwdIface ifc, addrIP trg, int tos, int ttl) {
        forwarder = fwd;
        fwdIface = ifc;
        peerAddr = trg.copyBytes();
        sendingTOS = tos;
        sendingTTL = ttl;
    }

    /**
     * register this protocol
     *
     * @return false on success, true on error
     */
    public boolean lowerRegister() {
        return forwarder.protoAdd(this, fwdIface, peerAddr);
    }

    /**
     * unregister this protocol
     */
    public void lowerUnregister() {
        forwarder.protoDel(this, fwdIface, peerAddr);
    }

    /**
     * get state
     *
     * @return value
     */
    public state.states lowerGetState() {
        if (cipher == null) {
            return state.states.down;
        }
        return state.states.up;
    }

    /**
     * get mtu size
     *
     * @return value
     */
    public int lowerGetMTU() {
        int i = 0;
        if (cipher != null) {
            i = encrSize;
        }
        return fwdIface.mtu - 16 - i - hashSize;
    }

    /**
     * get bandwidth
     *
     * @return value
     */
    public long lowerGetBW() {
        return fwdIface.bandwidth;
    }

    public int getProtoNum() {
        return protoNum;
    }

    /**
     * close interface
     *
     * @param iface interface
     */
    public void closeUp(ipFwdIface iface) {
    }

    public counter getCounter() {
        return cntr;
    }

    private int getType() {
        if (ipv6) {
            return ipIfc6.type;
        } else {
            return ipIfc4.type;
        }
    }

    private int getProto() {
        if (ipv6) {
            return ipCor6.protocolNumber;
        } else {
            return ipCor4.protocolNumber;
        }
    }

    /**
     * received packet
     *
     * @param rxIfc interface
     * @param pck packet
     */
    public synchronized void recvPack(ipFwdIface rxIfc, packHolder pck) {
        cntr.rx(pck);
        if (rxIfc.ifwNum != fwdIface.ifwNum) {
            cntr.drop(pck, counter.reasons.noIface);
            return;
        }
        if (cipher == null) {
            cntr.drop(pck, counter.reasons.notUp);
            return;
        }
        int i = pck.msbGetD(0);
        if (i != spi) {
            badSpi = i;
            logger.info("invalid spi; got=" + i + " need=" + spi + " from " + peerAddr);
            cntr.drop(pck, counter.reasons.badID);
            return;
        }
        badSpi = 0;
        int seqRx = pck.msbGetD(4);
        if (sequence != null) {
            if (sequence.gotDat(seqRx)) {
                logger.info("replay check failed from " + peerAddr);
                cntr.drop(pck, counter.reasons.badRxSeq);
                return;
            }
        }
        int siz = pck.dataSize() - hashSize;
        if (siz < 8) {
            logger.info("too small from " + peerAddr);
            cntr.drop(pck, counter.reasons.tooSmall);
            return;
        }
        hasher.init();
        pck.hashData(hasher, 0, siz);
        byte[] got = new byte[hashSize];
        pck.getCopy(got, 0, siz, hashSize);
        if (bits.byteComp(got, 0, hasher.finish(), 0, hashSize) != 0) {
            logger.info("bad hash from " + peerAddr);
            cntr.drop(pck, counter.reasons.badSum);
            return;
        }
        pck.getSkip(8);
        siz -= 8;
        pck.encrData(cipher, 0, siz);
        siz -= pck.getByte(siz - 2) + encrSize + 2;
        pck.getSkip(encrSize);
        pck.setDataSize(siz);
        pck.msbPutW(0, getType());
        pck.putSkip(2);
        pck.merge2beg();
        lower.recvPack(pck);
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
     * set state
     *
     * @param iface interface
     * @param stat state
     */
    public void setState(ipFwdIface iface, state.states stat) {
    }

    /**
     * send one packet
     *
     * @param pck packet to send
     */
    public synchronized void sendPacket(packHolder pck) {
        cntr.tx(pck);
        if (cipher == null) {
            cntr.drop(pck, counter.reasons.notUp);
            return;
        }
        pck.merge2beg();
        if (pck.msbGetW(0) != getType()) {
            cntr.drop(pck, counter.reasons.badProto);
            return;
        }
        pck.getSkip(2);
        seqTx++;
        int o = pck.dataSize() + 2;
        o = encrSize - (o % encrSize);
        for (int i = 0; i < o; i++) {
            pck.putByte(i, i + 1);
        }
        pck.putSkip(o);
        pck.putByte(0, o);
        pck.putByte(1, getProto());
        pck.putSkip(2);
        pck.merge2end();
        for (int i = 0; i < encrSize; i++) {
            pck.putByte(i, bits.randomB());
        }
        pck.putSkip(encrSize);
        pck.merge2beg();
        pck.encrData(cipher, 0, pck.dataSize());
        pck.msbPutD(0, spi);
        pck.msbPutD(4, seqTx);
        pck.putSkip(8);
        pck.merge2beg();
        hasher.init();
        pck.hashData(hasher, 0, pck.dataSize());
        byte[] buf = hasher.finish();
        pck.putCopy(buf, 0, 0, hashSize);
        pck.putSkip(hashSize);
        pck.merge2end();
        pck.IPsrc.setAddr(fwdIface.addr);
        pck.IPtrg.setAddr(peerAddr);
        pck.putDefaults();
        if (sendingTTL >= 0) {
            pck.IPttl = sendingTTL;
        }
        if (sendingTOS >= 0) {
            pck.IPtos = sendingTOS;
        }
        pck.IPprt = protoNum;
        forwarder.protoPack(fwdIface, pck);
    }

    /**
     * send one packet
     */
    public synchronized void doInit() {
        seqTx = 0;
        if (replayCheck > 0) {
            sequence = new tabWindow(replayCheck);
        } else {
            sequence = null;
        }
    }

}
