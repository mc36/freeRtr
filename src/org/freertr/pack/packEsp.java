package org.freertr.pack;

import org.freertr.addr.addrIP;
import org.freertr.cry.cryEncrGeneric;
import org.freertr.cry.cryHashGeneric;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcUp;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipPrt;
import org.freertr.prt.prtTmux;
import org.freertr.tab.tabWindow;
import org.freertr.util.bits;
import org.freertr.util.counter;
import org.freertr.util.logger;
import org.freertr.util.state;

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
     * header size
     */
    public final static int size = 8;

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
    public int spi;

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
    public int hashSize;

    /**
     * size of cipher
     */
    public int encrSize;

    /**
     * tag size
     */
    public int tagSize;

    /**
     * encryption keys
     */
    public byte[] keyEncr = null;

    /**
     * authentication keys
     */
    public byte[] keyHash = null;

    private int seqTx;

    private tabWindow<packHolder> sequence;

    private ifcUp lower = new ifcNull();

    private addrIP peerAddr;

    private ipFwd forwarder;

    private ipFwdIface fwdIface;

    private counter cntr = new counter();

    private int sendingTTL = 255;

    private int sendingTOS = -1;

    private int sendingDFN = -1;

    private int sendingFLW = -1;

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
     * @param flw sending flow
     * @param dfn sending df
     */
    public void lowerSetup(ipFwd fwd, ipFwdIface ifc, addrIP trg, int tos, int ttl, int flw, int dfn) {
        forwarder = fwd;
        fwdIface = ifc;
        peerAddr = trg.copyBytes();
        sendingTOS = tos;
        sendingDFN = dfn;
        sendingTTL = ttl;
        sendingFLW = flw;
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
        if (siz < size) {
            logger.info("too small from " + peerAddr);
            cntr.drop(pck, counter.reasons.tooSmall);
            return;
        }
        if (hasher == null) {
            byte[] buf = new byte[12];
            bits.byteCopy(keyHash, 0, buf, 0, 4);
            pck.getCopy(buf, 4, size, buf.length - 4);
            cipher.init(keyEncr, buf, false);
            pck.authData(cipher, 0, size);
            pck.getSkip(buf.length - 4);
            siz -= buf.length - 4;
        } else {
            hasher.init();
            pck.hashData(hasher, 0, siz);
            byte[] got = new byte[hashSize];
            pck.getCopy(got, 0, siz, hashSize);
            if (bits.byteComp(got, 0, hasher.finish(), 0, hashSize) != 0) {
                logger.info("bad hash from " + peerAddr);
                cntr.drop(pck, counter.reasons.badSum);
                return;
            }
            byte[] buf = new byte[encrSize];
            pck.getCopy(buf, 0, size, buf.length);
            pck.getSkip(buf.length);
            cipher.init(keyEncr, buf, false);
            siz -= encrSize;
        }
        pck.getSkip(size);
        siz -= size;
        siz = pck.encrData(cipher, 0, siz);
        if (siz < 0) {
            cntr.drop(pck, counter.reasons.badSum);
            logger.info("bad aead from " + peerAddr);
            return;
        }
        i = pck.getByte(siz - 1);
        i = prtTmux.proto2ethtyp(i);
        if (i < 0) {
            logger.info("bad protocol from " + peerAddr);
            cntr.drop(pck, counter.reasons.badProto);
            return;
        }
        siz -= pck.getByte(siz - 2) + 2;
        pck.setDataSize(siz);
        pck.msbPutW(0, i);
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
    public void sendPacket(packHolder pck) {
        cntr.tx(pck);
        if (cipher == null) {
            cntr.drop(pck, counter.reasons.notUp);
            return;
        }
        pck.merge2beg();
        synchronized (cipher) {
            int p = pck.msbGetW(0);
            p = prtTmux.ethtyp2proto(p);
            if (p < 0) {
                cntr.drop(pck, counter.reasons.badProto);
                return;
            }
            pck.getSkip(2);
            seqTx++;
            int o = pck.dataSize() + 2;
            if (hasher == null) {
                o = 4 - (o & 3);
            } else {
                o = encrSize - (o % encrSize);
            }
            for (int i = 0; i < o; i++) {
                pck.putByte(i, i + 1);
            }
            pck.putSkip(o);
            pck.putByte(0, o);
            pck.putByte(1, p);
            pck.putSkip(2);
            pck.merge2end();
            if (hasher == null) {
                byte[] buf = new byte[12];
                bits.byteCopy(keyHash, 0, buf, 0, 4);
                for (int i = 4; i < buf.length; i++) {
                    buf[i] = (byte) bits.randomB();
                }
                cipher.init(keyEncr, buf, true);
                pck.msbPutD(0, spi);
                pck.msbPutD(4, seqTx);
                pck.putSkip(size);
                pck.authHead(cipher, 0, size);
                pck.putCopy(buf, 4, 0, buf.length - 4);
                pck.putSkip(buf.length - 4);
                o = pck.encrData(cipher, 0, pck.dataSize());
                pck.setDataSize(o);
                pck.merge2beg();
            } else {
                byte[] buf = new byte[encrSize];
                for (int i = 0; i < buf.length; i++) {
                    buf[i] = (byte) bits.randomB();
                }
                cipher.init(keyEncr, buf, true);
                pck.encrData(cipher, 0, pck.dataSize());
                pck.putCopy(buf, 0, 0, buf.length);
                pck.putSkip(buf.length);
                pck.merge2beg();
                pck.msbPutD(0, spi);
                pck.msbPutD(4, seqTx);
                pck.putSkip(size);
                pck.merge2beg();
                hasher.init();
                pck.hashData(hasher, 0, pck.dataSize());
                buf = hasher.finish();
                pck.putCopy(buf, 0, 0, hashSize);
                pck.putSkip(hashSize);
                pck.merge2end();
            }
        }
        pck.IPsrc.setAddr(fwdIface.addr);
        pck.IPtrg.setAddr(peerAddr);
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
        pck.IPprt = protoNum;
        forwarder.protoPack(fwdIface, null, pck);
    }

    /**
     * send one packet
     */
    public synchronized void doInit() {
        seqTx = 0;
        if (replayCheck > 0) {
            sequence = new tabWindow<packHolder>(replayCheck);
        } else {
            sequence = null;
        }
    }

}
