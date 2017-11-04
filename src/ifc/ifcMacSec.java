package ifc;

import addr.addrMac;
import cfg.cfgIpsec;
import cry.cryEncrGeneric;
import cry.cryHashGeneric;
import cry.cryKeyDH;
import java.math.BigInteger;
import pack.packHolder;
import util.bits;

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

    /**
     * initialize the crypter
     *
     * @param ips ipsec profile
     * @param eth ethertype to use
     */
    public void doInit(cfgIpsec ips, ifcEthTyp eth) {
        profil = ips;
        keygen = ips.trans.getGroup();
        keygen.servXchg();
        try {
            myaddr = (addrMac) eth.getHwAddr().copyBytes();
        } catch (Exception e) {
            myaddr = addrMac.getBroadcast();
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
        pck.msbPutW(0, ethtyp); // ethertype
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
            return true;
        }
        if (pck.msbGetW(0) != ethtyp) { // ethertype
            return true;
        }
        int typ = pck.getByte(2); // tci
        switch (typ) {
            case 0x08: // data
                break;
            case 0x01: // request
            case 0x02: // reply
                reply = typ == 1;
                pck.getSkip(size);
                keygen.clntPub = new BigInteger(pck.getCopy());
                keygen.servKey();
                byte[] buf1 = new byte[0];
                for (int i = 0; buf1.length < 1024; i++) {
                    cryHashGeneric hsh = profil.trans.getHash();
                    hsh.init();
                    hsh.update(keygen.common.toByteArray());
                    hsh.update(profil.preshared.getBytes());
                    hsh.update(i);
                    buf1 = bits.byteConcat(buf1, hsh.finish());
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
                return true;
        }
        if (hashRx == null) {
            return true;
        }
        int pad = pck.getByte(3); // sl
//  int seqRx = pck.msbGetD(4); // seq
        pck.getSkip(size);
        int siz = pck.dataSize();
        if (siz < (hashSiz + cphrSiz)) {
            return true;
        }
        if (((siz - hashSiz) % cphrSiz) != 0) {
            return true;
        }
        hashRx.init();
        siz -= hashSiz;
        pck.hashData(hashRx, 0, siz);
        byte[] sum = new byte[hashSiz];
        pck.getCopy(sum, 0, siz, hashSiz);
        if (bits.byteComp(sum, 0, hashRx.finish(), 0, hashSiz) != 0) {
            return true;
        }
        pck.encrData(cphrRx, 0, siz);
        pck.setDataSize(siz - pad);
        pck.getSkip(cphrSiz);
        return false;
    }

    /**
     * generate sync packet
     *
     * @return packet to send, null if nothing
     */
    public synchronized packHolder doSync() {
        if ((hashRx != null) && (!reply)) {
            return null;
        }
        packHolder pck = new packHolder(true, true);
        pck.msbPutW(0, ethtyp); // ethertype
        pck.putByte(2, hashRx == null ? 0x01 : 0x02); // tci=v,e
        pck.putByte(3, 0); // sl
        pck.msbPutD(4, 0); // seq
        pck.putSkip(size);
        byte[] buf = keygen.servPub.toByteArray();
        pck.putCopy(buf, 0, 0, buf.length);
        pck.putSkip(buf.length);
        pck.merge2beg();
        pck.ETHsrc = myaddr.copyBytes();
        pck.ETHtrg = addrMac.getBroadcast();
        reply = false;
        return pck;
    }

}
