package org.freertr.ifc;

import org.freertr.addr.addrMac;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgIpsec;
import org.freertr.cry.cryEncrGeneric;
import org.freertr.cry.cryHashGeneric;
import org.freertr.cry.cryKeyGeneric;
import org.freertr.pack.packHolder;
import org.freertr.tab.tabWindow;
import org.freertr.user.userFormat;
import org.freertr.util.bits;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;

/**
 * mac security (ieee 802.1ae) protocol
 *
 * @author matecsaba
 */
public class ifcMacSec {

    /**
     * create instance
     */
    public ifcMacSec() {
    }

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
     * need to check layer2 info
     */
    public boolean needLayer2 = true;

    /**
     * need to do aead mode
     */
    public boolean aeadMode = false;

    /**
     * encryption keys
     */
    public byte[] keyEncrTx = null;

    /**
     * decryption keys
     */
    public byte[] keyEncrRx = null;

    /**
     * encryption iv
     */
    public byte[] keyIvTx = null;

    /**
     * decryption iv
     */
    public byte[] keyIvRx = null;

    /**
     * authentication keys
     */
    public byte[] keyHashTx = null;

    /**
     * authentication keys
     */
    public byte[] keyHashRx = null;

    /**
     * ethertype in effect
     */
    public int myTyp;

    /**
     * cipher size
     */
    public int cphrSiz;

    /**
     * hash size
     */
    public int hashSiz;

    /**
     * tag size
     */
    public int tagSiz;

    /**
     * hardware counter
     */
    public counter hwCntr;

    private counter cntr = new counter();

    private tabWindow<packHolder> sequence;

    private addrMac myaddr;

    private int myDisc;

    private int peerDisc;

    private cryEncrGeneric cphrTx;

    private cryEncrGeneric cphrRx;

    private cryHashGeneric hashTx;

    private cryHashGeneric hashRx;

    private cryKeyGeneric keygen;

    private int seqTx;

    private int kexNum;

    private int replyTyp;

    private byte[] replyVal;

    private long replyTim = 0;

    private int lastRnd = 0;

    private ifcEthTyp etht;

    public String toString() {
        String a = "";
        if (myTyp != ethtyp) {
            a = " " + bits.toHexW(myTyp);
        }
        return profil.name + a;
    }

    /**
     * get show output
     *
     * @return text
     */
    public userFormat getShow() {
        userFormat l = new userFormat("|", "category|value");
        l.add("kex|" + kexNum);
        l.add("for|" + bits.timePast(replyTim));
        l.add("since|" + bits.time2str(cfgAll.timeZoneName, replyTim + cfgAll.timeServerOffset, 3));
        l.add("seq|" + seqTx);
        l.add("win|" + sequence);
        l.add("pack|" + cntr.getShHwPsum(hwCntr));
        l.add("byte|" + cntr.getShHwBsum(hwCntr));
        return l;
    }

    /**
     * get mode flags
     *
     * @return flags
     */
    public int getModeFlags() {
        int i = 0;
        if (needLayer2) {
            i |= 1;
        }
        if (aeadMode) {
            i |= 2;
        }
        return i;
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
        if (profil.replay > 0) {
            sequence = new tabWindow<packHolder>(profil.replay);
        }
        try {
            myaddr = (addrMac) eth.getHwAddr().copyBytes();
        } catch (Exception e) {
            myaddr = addrMac.getBroadcast();
        }
        keygen = profil.trans.getGroup();
        myDisc = 1 + bits.randomD();
        peerDisc = 0;
        if (debugger.ifcMacSecTraf) {
            logger.debug("initialized");
        }
        if (profil.role != cfgIpsec.roleMode.staticKeys) {
            return;
        }
        byte[] buf1 = new byte[profil.preshared.length() / 2];
        for (int i = 0; i < buf1.length; i++) {
            buf1[i] = (byte) bits.fromHex(profil.preshared.substring(i * 2, i * 2 + 2));
        }
        setupKeys(buf1, false);
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
        cntr.tx(pck);
        int pad = pck.dataSize() % cphrSiz;
        byte[] buf;
        if (pad > 0) {
            pad = cphrSiz - pad;
            buf = new byte[pad];
            pck.putCopy(buf, 0, 0, buf.length);
            pck.putSkip(buf.length);
            pck.merge2end();
        }
        int siz = pck.dataSize();
        pad = 0;
        if (siz < 48) {
            pad = siz;
        }
        byte[] mac = new byte[keyIvTx.length + 4];
        bits.byteCopy(keyIvTx, 0, mac, 0, keyIvTx.length);
        bits.msbPutD(mac, keyIvTx.length, seqTx);
        cphrTx.init(keyEncrTx, mac, true);
        hashTx.init();
        if (needLayer2) {
            mac = pck.ETHtrg.getBytes();
            hashTx.update(mac);
            cphrTx.authAdd(mac);
            mac = pck.ETHsrc.getBytes();
            hashTx.update(mac);
            cphrTx.authAdd(mac);
        }
        pck.msbPutW(0, myTyp); // ethertype
        pck.putByte(2, 0x0c); // tci=c,e
        pck.putByte(3, pad); // sl
        pck.msbPutD(4, seqTx); // seq
        pck.hashHead(hashTx, 0, size);
        pck.authHead(cphrTx, 0, size);
        pck.putSkip(size);
        pck.merge2beg();
        siz = pck.encrData(cphrTx, size, siz);
        if (siz < 0) {
            cntr.drop(pck, counter.reasons.badSum);
            logger.info("bad aead on " + etht);
            return true;
        }
        pck.setDataSize(size + siz);
        pck.hashData(hashTx, size, siz);
        buf = hashTx.finish();
        pck.putCopy(buf, 0, 0, buf.length);
        pck.putSkip(buf.length);
        pck.merge2end();
        seqTx++;
        return false;
    }

    /**
     * decrypt one packet
     *
     * @param pck packet to decrypt
     * @param allowClear allot cleartext also
     * @return false on success, true on error
     */
    public synchronized boolean doDecrypt(packHolder pck, boolean allowClear) {
        if (pck.dataSize() < size) {
            cntr.drop(pck, counter.reasons.tooSmall);
            logger.info("too short on " + etht);
            return true;
        }
        int typ = pck.msbGetW(0);
        if (typ != myTyp) { // ethertype
            if (allowClear) {
                return false;
            }
            cntr.drop(pck, counter.reasons.badTyp);
            logger.info("bad type (" + bits.toHexW(typ) + ") on " + etht);
            return true;
        }
        typ = pck.getByte(2); // tci
        switch (typ) {
            case 0x0c: // data
                break;
            case 0x01: // init-req
            case 0x02: // init-rep
            case 0x03: // kex-req
            case 0x04: // kex-rep
                if (profil.role == cfgIpsec.roleMode.staticKeys) {
                    return true;
                }
                pck.getSkip(size);
                replyTyp = typ;
                replyVal = pck.getCopy();
                replyTim = bits.getTime();
                if (debugger.ifcMacSecTraf) {
                    logger.debug("got kex " + replyTyp + " " + bits.byteDump(replyVal, 0, -1));
                }
                etht.triggerSync();
                return true;
            default:
                cntr.drop(pck, counter.reasons.badTyp);
                logger.info("bad type " + typ + " on " + etht);
                return true;
        }
        if (hashRx == null) {
            cntr.drop(pck, counter.reasons.notUp);
            return true;
        }
        int pad = pck.getByte(3); // sl
        int seqRx = pck.msbGetD(4); // seq
        if (sequence != null) {
            if (sequence.gotDat(seqRx)) {
                cntr.drop(pck, counter.reasons.badRxSeq);
                logger.info("replay check failed on " + etht);
                return true;
            }
        }
        byte[] mac = new byte[keyIvRx.length + 4];
        bits.byteCopy(keyIvRx, 0, mac, 0, keyIvRx.length);
        bits.msbPutD(mac, keyIvRx.length, seqRx);
        cphrRx.init(keyEncrRx, mac, false);
        hashRx.init();
        if (needLayer2) {
            mac = pck.ETHtrg.getBytes();
            hashRx.update(mac);
            cphrRx.authAdd(mac);
            mac = pck.ETHsrc.getBytes();
            hashRx.update(mac);
            cphrRx.authAdd(mac);
        }
        pck.hashData(hashRx, 0, size);
        pck.authData(cphrRx, 0, size);
        pck.getSkip(size);
        int siz = pck.dataSize();
        if (pad > 0) {
            pad += tagSiz + hashSiz;
            if (pad > siz) {
                cntr.drop(pck, counter.reasons.badLen);
                logger.info("invalid padding on " + etht);
                return true;
            }
            siz = pad;
        }
        if (siz < (hashSiz + cphrSiz)) {
            cntr.drop(pck, counter.reasons.tooSmall);
            logger.info("too small on " + etht);
            return true;
        }
        siz -= hashSiz;
        if (!aeadMode && ((siz % cphrSiz) != 0)) {
            cntr.drop(pck, counter.reasons.badSiz);
            logger.info("bad padding on " + etht);
            return true;
        }
        pck.hashData(hashRx, 0, siz);
        byte[] sum = new byte[hashSiz];
        pck.getCopy(sum, 0, siz, hashSiz);
        if (bits.byteComp(sum, 0, hashRx.finish(), 0, hashSiz) != 0) {
            cntr.drop(pck, counter.reasons.badSum);
            logger.info("bad hash on " + etht);
            return true;
        }
        siz = pck.encrData(cphrRx, 0, siz);
        if (siz < 0) {
            cntr.drop(pck, counter.reasons.badSum);
            logger.info("bad aead on " + etht);
            return true;
        }
        pck.setDataSize(siz);
        cntr.rx(pck);
        return false;
    }

    /**
     * generate sync packet
     *
     * @return packet to send, null if nothing
     */
    public synchronized packHolder doSync() {
        if (profil.role == cfgIpsec.roleMode.staticKeys) {
            return null;
        }
        packHolder pck = new packHolder(true, true);
        pck.ETHsrc.setAddr(myaddr);
        pck.ETHtrg.setAddr(addrMac.getBroadcast());
        pck.msbPutW(0, myTyp); // ethertype
        pck.putByte(3, 0); // sl
        pck.msbPutD(4, 0); // seq
        int replyOld = replyTyp;
        replyTyp = 0;
        switch (replyOld) {
            case 1:
                peerDisc = bits.msbGetD(replyVal, 0);
                if (myDisc == peerDisc) {
                    myDisc = 1 + bits.randomD();
                    peerDisc = 0;
                    return null;
                }
                pck.putByte(2, 2); // init
                pck.msbPutD(size, myDisc);
                pck.putSkip(size + 4);
                pck.merge2beg();
                keygen = profil.trans.getGroup();
                return pck;
            case 2:
                peerDisc = bits.msbGetD(replyVal, 0);
                if (myDisc == peerDisc) {
                    myDisc = 1 + bits.randomD();
                    peerDisc = 0;
                    return null;
                }
                break;
            case 3:
            case 4:
                if (peerDisc == 0) {
                    break;
                }
                if (myDisc < peerDisc) {
                    keygen.keyClntIke(replyVal, 0);
                } else {
                    keygen.keyServIke(replyVal, 0);
                }
                break;
            default:
                break;
        }
        if (peerDisc == 0) {
            pck.putByte(2, 1); // init
            pck.msbPutD(size, myDisc);
            pck.putSkip(size + 4);
            pck.merge2beg();
            return pck;
        }
        if (myDisc < peerDisc) {
            if (keygen.keyServIke() == null) {
                keygen.keyServInit();
            }
            if ((keygen.keyCommonIke() == null) && (keygen.keyClntIke() != null)) {
                keygen.keyServCalc();
                setupKeys();
            }
        } else {
            if (keygen.keyClntIke() == null) {
                keygen.keyClntInit();
            }
            if ((keygen.keyCommonIke() == null) && (keygen.keyServIke() != null)) {
                keygen.keyClntCalc();
                setupKeys();
            }
        }
        if (replyOld == 3) {
            byte[] buf;
            if (myDisc < peerDisc) {
                buf = keygen.keyServIke();
            } else {
                buf = keygen.keyClntIke();
            }
            if (debugger.ifcMacSecTraf) {
                logger.debug("send kex " + bits.byteDump(buf, 0, -1));
            }
            if (buf == null) {
                return null;
            }
            pck.putByte(2, 4); // kex
            pck.putCopy(buf, 0, size, buf.length);
            pck.putSkip(size + buf.length);
            pck.merge2beg();
            return pck;
        }
        if (keygen.keyServIke() == null) {
            if (myDisc < peerDisc) {
                return null;
            }
            byte[] buf = keygen.keyClntIke();
            if (debugger.ifcMacSecTraf) {
                logger.debug("send kex " + bits.byteDump(buf, 0, -1));
            }
            if (buf == null) {
                return null;
            }
            pck.putByte(2, 3); // kex
            pck.putCopy(buf, 0, size, buf.length);
            pck.putSkip(size + buf.length);
            pck.merge2beg();
            return pck;
        }
        if (hashRx == null) {
            return null;
        }
        boolean ned = false;
        if (profil.trans.lifeSec > 0) {
            ned |= (bits.getTime() - replyTim - lastRnd) > (profil.trans.lifeSec * 1000);
        }
        if (profil.trans.lifeByt > 0) {
            long tx = cntr.byteTx;
            if (hwCntr != null) {
                tx += hwCntr.byteTx;
            }
            ned |= tx > profil.trans.lifeByt;
        }
        if (!ned) {
            return null;
        }
        if (debugger.ifcMacSecTraf) {
            logger.debug("restarting kex");
        }
        hashRx = null;
        hwCntr = null;
        keygen = profil.trans.getGroup();
        myDisc = 1 + bits.randomD();
        peerDisc = 0;
        return null;
    }

    private void setupKeys() {
        if (debugger.ifcMacSecTraf) {
            logger.debug("keys " + keygen.keyDump());
        }
        byte[] buf1 = new byte[0];
        cryHashGeneric hsh = profil.trans.getHash();
        for (int i = 0; buf1.length < 1024; i++) {
            hsh.init();
            hsh.update(keygen.keyCommonIke());
            hsh.update(profil.preshared.getBytes());
            hsh.update(i);
            byte[] buf2 = hsh.finish();
            buf1 = bits.byteConcat(buf1, buf2);
            if (buf2.length > 0) {
                continue;
            }
            buf2 = keygen.keyCommonIke();
            byte[] buf3 = profil.preshared.getBytes();
            for (int o = 0; o < buf2.length; o++) {
                buf2[o] ^= buf3[o % buf3.length];
            }
            buf1 = bits.byteConcat(buf1, buf2);
        }
        setupKeys(buf1, myDisc < peerDisc);
    }

    private void setupKeys(byte[] res, boolean swp) {
        if (debugger.ifcMacSecTraf) {
            logger.debug("master=" + bits.byteDump(res, 0, res.length));
        }
        cryEncrGeneric cphTx = profil.trans.getEncr();
        cryEncrGeneric cphRx = profil.trans.getEncr();
        keyEncrTx = new byte[profil.trans.getKeyS()];
        int i = cphTx.getIVsize() - 4;
        if (i < 0) {
            i = 0;
        }
        keyIvTx = new byte[i];
        bits.byteCopy(res, 0, keyEncrTx, 0, keyEncrTx.length);
        i = keyEncrTx.length;
        keyEncrRx = new byte[keyEncrTx.length];
        bits.byteCopy(res, i, keyEncrRx, 0, keyEncrRx.length);
        i += keyEncrTx.length;
        bits.byteCopy(res, i, keyIvTx, 0, keyIvTx.length);
        i += keyIvTx.length;
        keyIvRx = new byte[keyIvTx.length];
        bits.byteCopy(res, i, keyIvRx, 0, keyIvRx.length);
        i += keyIvRx.length;
        cphrSiz = cphTx.getBlockSize();
        hashSiz = profil.trans.getHash().getHashSize();
        keyHashTx = new byte[hashSiz];
        bits.byteCopy(res, i, keyHashTx, 0, keyHashTx.length);
        i += keyHashTx.length;
        keyHashRx = new byte[keyHashTx.length];
        bits.byteCopy(res, i, keyHashRx, 0, keyHashRx.length);
        cntr = new counter();
        hwCntr = null;
        if (swp) {
            byte[] buf = keyIvTx;
            keyIvTx = keyIvRx;
            keyIvRx = buf;
            buf = keyHashTx;
            keyHashTx = keyHashRx;
            keyHashRx = buf;
            buf = keyEncrTx;
            keyEncrTx = keyEncrRx;
            keyEncrRx = buf;
        }
        kexNum++;
        if (profil.replay > 0) {
            sequence = new tabWindow<packHolder>(profil.replay);
        }
        if (profil.trans.lifeRnd > 1) {
            lastRnd = bits.random(1, profil.trans.lifeRnd);
        }
        seqTx = 0;
        tagSiz = cphTx.getTagSize();
        aeadMode = tagSiz > 0;
        i = cphTx.getIVsize();
        cphTx.init(keyEncrTx, new byte[i], true);
        cphRx.init(keyEncrRx, new byte[i], false);
        cphrTx = cphTx;
        cphrRx = cphRx;
        hashTx = profil.trans.getHmac(keyHashTx);
        hashRx = profil.trans.getHmac(keyHashRx);
    }

}
