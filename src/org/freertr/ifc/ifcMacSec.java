package org.freertr.ifc;

import java.math.BigInteger;
import org.freertr.addr.addrMac;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgIpsec;
import org.freertr.cry.cryEncrGeneric;
import org.freertr.cry.cryHashGeneric;
import org.freertr.cry.cryKeyDH;
import org.freertr.pack.packHolder;
import org.freertr.tab.tabWindow;
import org.freertr.user.userFormat;
import org.freertr.util.bits;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.syncInt;

/**
 * mac security (ieee 802.1ae) protocol
 *
 * @author matecsaba
 */
public class ifcMacSec implements Runnable {

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

    private cryEncrGeneric cphrTx;

    private cryEncrGeneric cphrRx;

    private cryHashGeneric hashTx;

    private cryHashGeneric hashRx;

    private cryKeyDH keygen;

    private int seqTx;

    private int kexNum;

    private boolean reply;

    private long lastKex = 0;

    private int lastRnd = 0;

    private syncInt calcing = new syncInt(0);

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
        l.add("for|" + bits.timePast(lastKex));
        l.add("since|" + bits.time2str(cfgAll.timeZoneName, lastKex + cfgAll.timeServerOffset, 3));
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
        keygen = ips.trans.getGroup();
        keygen.servXchg();
        if (profil.replay > 0) {
            sequence = new tabWindow<packHolder>(profil.replay);
        }
        try {
            myaddr = (addrMac) eth.getHwAddr().copyBytes();
        } catch (Exception e) {
            myaddr = addrMac.getBroadcast();
        }
        if (debugger.ifcMacSecTraf) {
            logger.debug("initialized");
        }
        if (profil.role != cfgIpsec.roleMode.staticKeys) {
            return;
        }
        byte[] buf1 = new byte[profil.preshared.length() / 2];
        for (int i = 0; i < buf1.length; i++) {
            try {
                buf1[i] = (byte) Integer.parseInt(profil.preshared.substring(i * 2, i * 2 + 2), 16);
            } catch (Exception e) {
            }
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
            case 0x01: // request
            case 0x02: // reply
                if (profil.role == cfgIpsec.roleMode.staticKeys) {
                    return true;
                }
                if (calcing.set(1) != 0) {
                    return true;
                }
                reply = typ == 1;
                lastKex = bits.getTime();
                pck.getSkip(size);
                keygen.clntPub = new BigInteger(pck.getCopy());
                new Thread(this).start();
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
        if ((hashRx != null) && (!reply)) {
            boolean ned = false;
            if (profil.trans.lifeSec > 0) {
                ned |= (bits.getTime() - lastKex - lastRnd) > (profil.trans.lifeSec * 1000);
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
            if (calcing.set(1) != 0) {
                return null;
            }
            if (debugger.ifcMacSecTraf) {
                logger.debug("restarting kex");
            }
            keygen = profil.trans.getGroup();
            keygen.servXchg();
            calcing.set(0);
            reply = false;
            hashRx = null;
            hwCntr = null;
        }
        boolean rep = hashRx == null;
        if (debugger.ifcMacSecTraf) {
            logger.debug("sending kex, reply=" + (!rep) + " common=" + keygen.common);
        }
        packHolder pck = new packHolder(true, true);
        pck.msbPutW(0, myTyp); // ethertype
        pck.putByte(2, rep ? 0x01 : 0x02); // tci=v,e
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

    private void doCalc() {
        if (debugger.ifcMacSecTraf) {
            logger.debug("got kex, reply=" + (!reply) + ", modulus=" + keygen.clntPub);
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
            byte[] buf2 = hsh.finish();
            if (buf2.length < 1) {
                buf2 = keygen.common.toByteArray();
                byte[] buf3 = profil.preshared.getBytes();
                for (int o = 0; o < buf2.length; o++) {
                    buf2[o] ^= buf3[o % buf3.length];
                }
            }
            buf1 = bits.byteConcat(buf1, buf2);
        }
        setupKeys(buf1, keygen.clntPub.compareTo(keygen.servPub) > 0);
        calcing.set(0);
        etht.triggerSync();
    }

    private void setupKeys(byte[] res, boolean swp) {
        if (debugger.ifcMacSecTraf) {
            logger.debug("master=" + bits.byteDump(res, 0, res.length));
        }
        cryEncrGeneric cphTx = profil.trans.getEncr();
        cryEncrGeneric cphRx = profil.trans.getEncr();
        keyEncrTx = new byte[profil.trans.getKeyS()];
        int pos = cphTx.getIVsize() - 4;
        if (pos < 0) {
            pos = 0;
        }
        keyIvTx = new byte[pos];
        bits.byteCopy(res, 0, keyEncrTx, 0, keyEncrTx.length);
        pos = keyEncrTx.length;
        keyEncrRx = new byte[keyEncrTx.length];
        bits.byteCopy(res, pos, keyEncrRx, 0, keyEncrRx.length);
        pos += keyEncrTx.length;
        bits.byteCopy(res, pos, keyIvTx, 0, keyIvTx.length);
        pos += keyIvTx.length;
        keyIvRx = new byte[keyIvTx.length];
        bits.byteCopy(res, pos, keyIvRx, 0, keyIvRx.length);
        pos += keyIvRx.length;
        cphrSiz = cphTx.getBlockSize();
        hashSiz = profil.trans.getHash().getHashSize();
        keyHashTx = new byte[hashSiz];
        bits.byteCopy(res, pos, keyHashTx, 0, keyHashTx.length);
        pos += keyHashTx.length;
        keyHashRx = new byte[keyHashTx.length];
        bits.byteCopy(res, pos, keyHashRx, 0, keyHashRx.length);
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
        pos = cphTx.getIVsize();
        cphTx.init(keyEncrTx, new byte[pos], true);
        cphRx.init(keyEncrRx, new byte[pos], false);
        cphrTx = cphTx;
        cphrRx = cphRx;
        hashTx = profil.trans.getHmac(keyHashTx);
        hashRx = profil.trans.getHmac(keyHashRx);
    }

    public void run() {
        try {
            doCalc();
        } catch (Exception e) {
            logger.exception(e);
        }
    }

}
