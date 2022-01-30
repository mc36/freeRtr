package net.freertr.ifc;

import java.math.BigInteger;
import net.freertr.addr.addrMac;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgIpsec;
import net.freertr.cry.cryEncrGeneric;
import net.freertr.cry.cryHashGeneric;
import net.freertr.cry.cryKeyDH;
import net.freertr.pack.packHolder;
import net.freertr.tab.tabWindow;
import net.freertr.user.userFormat;
import net.freertr.util.bits;
import net.freertr.util.counter;
import net.freertr.util.debugger;
import net.freertr.util.logger;
import net.freertr.util.syncInt;

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
     * replay check window
     */
    public int replayCheck = 1024;

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
    public byte[] keyEncr = null;

    /**
     * authentication keys
     */
    public byte[] keyHash = null;

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

    private long lastKex;

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
        replayCheck = profil.replay;
        if (replayCheck > 0) {
            sequence = new tabWindow<packHolder>(replayCheck);
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
        cntr.tx(pck);
        int pad = pck.dataSize() % cphrSiz;
        byte[] buf;
        if (pad > 0) {
            pad = cphrSiz - pad;
            buf = new byte[pad];
            for (int i = 0; i < buf.length; i++) {
                buf[i] = (byte) bits.randomB();
            }
            pck.putCopy(buf, 0, 0, pad);
            pck.putSkip(pad);
            pck.merge2end();
        }
        buf = new byte[cphrSiz];
        for (int i = 0; i < buf.length; i++) {
            buf[i] = (byte) bits.randomB();
        }
        pck.putCopy(buf, 0, 0, buf.length);
        pck.putSkip(buf.length);
        pck.merge2beg();
        int siz = pck.dataSize();
        if (aeadMode) {
            byte[] mac = new byte[cphrTx.getIVsize()];
            bits.msbPutD(mac, 0, seqTx);
            cphrTx.init(keyEncr, mac, true);
        }
        hashTx.init();
        if (needLayer2) {
            byte[] mac = pck.ETHsrc.getBytes();
            hashTx.update(mac);
            cphrTx.authAdd(mac);
            mac = pck.ETHtrg.getBytes();
            hashTx.update(mac);
            cphrTx.authAdd(mac);
        }
        siz = pck.encrData(cphrTx, 0, siz);
        if (siz < 0) {
            cntr.drop(pck, counter.reasons.badSum);
            logger.info("bad aead on " + etht);
            return true;
        }
        pck.setDataSize(siz);
        pck.hashData(hashTx, 0, siz);
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
            case 0x08: // data
                break;
            case 0x01: // request
            case 0x02: // reply
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
            return true;
        }
        int pad = pck.getByte(3); // sl
        int seqRx = pck.msbGetD(4); // seq
        pck.getSkip(size);
        if (sequence != null) {
            if (sequence.gotDat(seqRx)) {
                cntr.drop(pck, counter.reasons.badRxSeq);
                logger.info("replay check failed on " + etht);
                return true;
            }
        }
        int siz = pck.dataSize();
        if (siz < (hashSiz + cphrSiz)) {
            cntr.drop(pck, counter.reasons.tooSmall);
            logger.info("too small on " + etht);
            return true;
        }
        if (((siz - hashSiz) % cphrSiz) != 0) {
            cntr.drop(pck, counter.reasons.badSiz);
            logger.info("bad padding on " + etht);
            return true;
        }
        if (aeadMode) {
            byte[] mac = new byte[cphrTx.getIVsize()];
            bits.msbPutD(mac, 0, seqRx);
            cphrRx.init(keyEncr, mac, false);
        }
        hashRx.init();
        if (needLayer2) {
            byte[] mac = pck.ETHsrc.getBytes();
            hashRx.update(mac);
            cphrRx.authAdd(mac);
            mac = pck.ETHtrg.getBytes();
            hashRx.update(mac);
            cphrRx.authAdd(mac);
        }
        siz -= hashSiz;
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
        pck.setDataSize(siz - pad);
        pck.getSkip(cphrSiz);
        cntr.rx(pck);
        return false;
    }

    /**
     * generate sync packet
     *
     * @return packet to send, null if nothing
     */
    public synchronized packHolder doSync() {
        if ((hashRx != null) && (!reply)) {
            boolean ned = false;
            if (profil.trans.lifeSec > 0) {
                ned |= (bits.getTime() - lastKex) > (profil.trans.lifeSec * 1000);
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

    private void doCalc() {
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
        if (debugger.ifcMacSecTraf) {
            logger.debug("master=" + bits.byteDump(buf1, 0, buf1.length));
        }
        cryEncrGeneric cphTx = profil.trans.getEncr();
        cryEncrGeneric cphRx = profil.trans.getEncr();
        byte[] res = buf1;
        buf1 = new byte[profil.trans.getKeyS()];
        byte[] buf2 = new byte[cphTx.getBlockSize()];
        int pos = buf1.length + buf2.length;
        bits.byteCopy(res, 0, buf1, 0, buf1.length);
        bits.byteCopy(res, buf1.length, buf2, 0, buf2.length);
        keyEncr = buf1;
        cphTx.init(buf1, buf2, true);
        cphRx.init(buf1, buf2, false);
        cphrSiz = buf2.length;
        hashSiz = profil.trans.getHash().getHashSize();
        buf1 = new byte[hashSiz];
        buf2 = new byte[hashSiz];
        bits.byteCopy(res, pos, buf1, 0, buf1.length);
        bits.byteCopy(res, pos, buf2, 0, buf2.length);
        cntr = new counter();
        hwCntr = null;
        kexNum++;
        if (replayCheck > 0) {
            sequence = new tabWindow<packHolder>(replayCheck);
        }
        seqTx = 0;
        aeadMode = cphTx.getTagSize() > 0;
        cphrTx = cphTx;
        cphrRx = cphRx;
        hashTx = profil.trans.getHmac(buf1);
        hashRx = profil.trans.getHmac(buf2);
        keyHash = buf1;
        calcing.set(0);
    }

    public void run() {
        try {
            doCalc();
        } catch (Exception e) {
            logger.exception(e);
        }
    }

}
