package ifc;

import addr.addrMac;
import cfg.cfgIpsec;
import cry.cryEncrGeneric;
import cry.cryHashGeneric;
import cry.cryKeyDH;
import java.math.BigInteger;
import pack.packHolder;
import util.bits;
import util.counter;
import util.debugger;
import util.logger;
import tab.tabWindow;

/**
 * mac security (ieee 802.1ae) protocol
 *
 * @author matecsaba
 */
public class ifcMacSec {

    /**
     * ethertype of these packets
     */
    public final static int ethtyp = 0x88e5;

    /**
     * size of header
     */
    public final static int size = 8;

    /**
     * ipsec profile
     */
    public cfgIpsec profil;

    /**
     * replay check window
     */
    public int replayCheck = 1024;

    private tabWindow sequence;

    private addrMac myaddr;

    private cryEncrGeneric cphrTx;

    private cryEncrGeneric cphrRx;

    private cryHashGeneric hashTx;

    private cryHashGeneric hashRx;

    private cryKeyDH keygen;

    private int cphrSiz;

    private int hashSiz;

    private int seqTx;

    private boolean reply;

    private int myTyp;

    private long lastKex;

    private counter keyUsage = new counter();

    private ifcEthTyp etht;

    public String toString() {
        String a = "";
        if (myTyp != ethtyp) {
            a = " " + bits.toHexW(myTyp);
        }
        return profil.name + a;
    }

    /**
     * initialize the crypter
     *
     * @param ips ipsec profile
     * @param eth ethertype to use
     * @param typ value to use
     */
    public void doInit(cfgIpsec ips, ifcEthTyp eth, int typ) {
        if (typ < 1) {
            typ = ethtyp;
        }
        etht = eth;
        myTyp = typ;
        profil = ips;
        keygen = ips.trans.getGroup();
        keygen.servXchg();
        replayCheck = profil.replay;
        if (replayCheck > 0) {
            sequence = new tabWindow(replayCheck);
        }
        try {
            myaddr = (addrMac) eth.getHwAddr().copyBytes();
        } catch (Exception e) {
            myaddr = addrMac.getBroadcast();
        }
        if (debugger.ifcMacSecTraf) {
            logger.debug("initialized");
        }
    }

    /**
     * encrypt one packet
     *
     * @param pck packet to encrypt
     * @return false on success, true on error
     */
    public synchronized boolean doEncrypt(packHolder pck) {
        if (hashTx == null) {
            return true;
        }
        keyUsage.tx(pck);
        int pad = pck.dataSize() % cphrSiz;
        if (pad > 0) {
            pad = cphrSiz - pad;
            pck.putFill(0, pad, 0); // padding
            pck.putSkip(pad);
            pck.merge2end();
        }
        byte[] buf = new byte[cphrSiz];
        for (int i = 0; i < buf.length; i++) {
            buf[i] = (byte) bits.randomB();
        }
        pck.putCopy(buf, 0, 0, buf.length);
        pck.putSkip(buf.length);
        pck.merge2beg();
        pck.encrData(cphrTx, 0, pck.dataSize());
        hashTx.init();
        pck.hashData(hashTx, 0, pck.dataSize());
        buf = hashTx.finish();
        pck.putCopy(buf, 0, 0, buf.length);
        pck.putSkip(buf.length);
        pck.merge2end();
        pck.msbPutW(0, myTyp); // ethertype
        pck.putByte(2, 0x08); // tci=v,e
        pck.putByte(3, pad); // sl
        pck.msbPutD(4, seqTx); // seq
        pck.putSkip(size);
        pck.merge2beg();
        seqTx++;
        return false;
    }

    /**
     * decrypt one packet
     *
     * @param pck packet to decrypt
     * @return false on success, true on error
     */
    public synchronized boolean doDecrypt(packHolder pck) {
        if (pck.dataSize() < size) {
            logger.info("too short on " + etht);
            return true;
        }
        int typ = pck.msbGetW(0);
        if (typ != myTyp) { // ethertype
            logger.info("bad type (" + bits.toHexW(typ) + ") on " + etht);
            return true;
        }
        typ = pck.getByte(2); // tci
        switch (typ) {
            case 0x08: // data
                break;
            case 0x01: // request
            case 0x02: // reply
                reply = typ == 1;
                lastKex = bits.getTime();
                keyUsage = new counter();
                if (replayCheck > 0) {
                    sequence = new tabWindow(replayCheck);
                }
                seqTx = 0;
                pck.getSkip(size);
                keygen.clntPub = new BigInteger(pck.getCopy());
                if (debugger.ifcMacSecTraf) {
                    logger.debug("got kex, reply=" + reply + ", modulus=" + keygen.clntPub);
                }
                keygen.servKey();
                if (debugger.ifcMacSecTraf) {
                    logger.debug("common=" + keygen.common);
                }
                byte[] buf1 = new byte[0];
                for (int i = 0; buf1.length < 1024; i++) {
                    cryHashGeneric hsh = profil.trans.getHash();
                    hsh.init();
                    hsh.update(keygen.common.toByteArray());
                    hsh.update(profil.preshared.getBytes());
                    hsh.update(i);
                    buf1 = bits.byteConcat(buf1, hsh.finish());
                }
                if (debugger.ifcMacSecTraf) {
                    logger.debug("master=" + bits.byteDump(buf1, 0, buf1.length));
                }
                cphrTx = profil.trans.getEncr();
                cphrRx = profil.trans.getEncr();
                byte[] res = buf1;
                buf1 = new byte[cphrTx.getKeySize()];
                byte[] buf2 = new byte[cphrTx.getBlockSize()];
                int pos = buf1.length + buf2.length;
                bits.byteCopy(res, 0, buf1, 0, buf1.length);
                bits.byteCopy(res, buf1.length, buf2, 0, buf2.length);
                cphrTx.init(buf1, buf2, true);
                cphrRx.init(buf1, buf2, false);
                cphrSiz = buf2.length;
                hashSiz = profil.trans.getHash().getHashSize();
                buf1 = new byte[hashSiz];
                buf2 = new byte[hashSiz];
                bits.byteCopy(res, pos, buf1, 0, buf1.length);
                bits.byteCopy(res, pos, buf2, 0, buf2.length);
                hashTx = profil.trans.getHmac(buf1);
                hashRx = profil.trans.getHmac(buf2);
                return true;
            default:
                logger.info("bad type " + typ + " on " + etht);
                return true;
        }
        if (hashRx == null) {
            return true;
        }
        int pad = pck.getByte(3); // sl
        int seqRx = pck.msbGetD(4); // seq
        pck.getSkip(size);
        if (sequence != null) {
            if (sequence.gotDat(seqRx)) {
                logger.info("replay check failed on " + etht);
                return true;
            }
        }
        int siz = pck.dataSize();
        if (siz < (hashSiz + cphrSiz)) {
            logger.info("too small on " + etht);
            return true;
        }
        if (((siz - hashSiz) % cphrSiz) != 0) {
            logger.info("bad padding on " + etht);
            return true;
        }
        hashRx.init();
        siz -= hashSiz;
        pck.hashData(hashRx, 0, siz);
        byte[] sum = new byte[hashSiz];
        pck.getCopy(sum, 0, siz, hashSiz);
        if (bits.byteComp(sum, 0, hashRx.finish(), 0, hashSiz) != 0) {
            logger.info("bad hash on " + etht);
            return true;
        }
        pck.encrData(cphrRx, 0, siz);
        pck.setDataSize(siz - pad);
        pck.getSkip(cphrSiz);
        keyUsage.rx(pck);
        return false;
    }

    /**
     * generate sync packet
     *
     * @return packet to send, null if nothing
     */
    public synchronized packHolder doSync() {
        if ((hashRx != null) && (!reply)) {
            boolean fin = true;
            if (((bits.getTime() - lastKex) > 28800000) || ((keyUsage.byteRx + keyUsage.byteTx) > 0xfff0000)) {
                if (debugger.ifcMacSecTraf) {
                    logger.debug("restarting kex");
                }
                keygen = profil.trans.getGroup();
                keygen.servXchg();
                reply = false;
                hashRx = null;
                fin = false;
            }
            if (fin) {
                return null;
            }
        }
        if (debugger.ifcMacSecTraf) {
            logger.debug("sending kex, common=" + keygen.common);
        }
        packHolder pck = new packHolder(true, true);
        pck.msbPutW(0, myTyp); // ethertype
        pck.putByte(2, hashRx == null ? 0x01 : 0x02); // tci=v,e
        pck.putByte(3, 0); // sl
        pck.msbPutD(4, 0); // seq
        pck.putSkip(size);
        byte[] buf = keygen.servPub.toByteArray();
        pck.putCopy(buf, 0, 0, buf.length);
        pck.putSkip(buf.length);
        pck.merge2beg();
        pck.ETHsrc.setAddr(myaddr);
        pck.ETHtrg.setAddr(addrMac.getBroadcast());
        reply = false;
        return pck;
    }

}
