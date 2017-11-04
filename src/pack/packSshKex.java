package pack;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

import util.bits;
import util.debugger;
import util.logger;
import cry.cryHashSha1;
import cry.cryKeyDH;
import cry.cryKeyGeneric;

/**
 * secure shell group exchange (rfc4419) protocol
 *
 * @author matecsaba
 */
public class packSshKex {

    /**
     * minimum modulus
     */
    public int modMin;

    /**
     * preferred modulus
     */
    public int modBest;

    /**
     * maximum modulus
     */
    public int modMax;

    /**
     * diffie hellman
     */
    public cryKeyDH difHel;

    /**
     * server key
     */
    public byte[] cert;

    /**
     * server signature
     */
    public byte[] sign;

    /**
     * hash of exchange
     */
    public List<Integer> hash1;

    /**
     * backup hash
     */
    public List<Integer> hash2;

    /**
     * exchange hash
     */
    public byte[] hashVal;

    /**
     * init vector client->server
     */
    public byte[] ivCS;

    /**
     * init vector server->client
     */
    public byte[] ivSC;

    /**
     * encryption client->server
     */
    public byte[] encCS;

    /**
     * encryption server->client
     */
    public byte[] encSC;

    /**
     * integrity client->server
     */
    public byte[] macCS;

    /**
     * integrity server->client
     */
    public byte[] macSC;

    private final packSsh lower;

    /**
     * create instance
     *
     * @param pack lower layer
     */
    public packSshKex(packSsh pack) {
        lower = pack;
        hash1 = new ArrayList<Integer>();
        hash2 = new ArrayList<Integer>();
    }

    private byte[] hashKey(int letter) {
        cryHashSha1 h = new cryHashSha1();
        h.init();
        for (int i = 0; i < hash1.size(); i++) {
            h.update(hash1.get(i));
        }
        h.update(hashVal);
        h.update(letter);
        h.update(hashVal);
        byte[] res = new byte[h.getHashSize() * 4];
        int p = 0;
        for (;;) {
            byte[] buf = h.finish();
            for (int i = 0; i < buf.length; i++) {
                res[p] = buf[i];
                p++;
            }
            if (p >= res.length) {
                break;
            }
            h.init();
            for (int i = 0; i < hash1.size(); i++) {
                h.update(hash1.get(i));
            }
            h.update(hashVal);
            for (int i = 0; i < buf.length; i++) {
                h.update(buf[i]);
            }
        }
        return res;
    }

    /**
     * calculate exchange hash
     */
    public void hashCalc() {
        hashBig(difHel.clntPub);
        hashBig(difHel.servPub);
        hashBig(difHel.common);
        cryHashSha1 h = new cryHashSha1();
        h.init();
        for (int i = 0; i < hash1.size(); i++) {
            h.update(hash1.get(i));
        }
        hashVal = h.finish();
        hash1.clear();
        hash2.clear();
        hashBig(difHel.common);
        ivCS = hashKey(0x41);
        ivSC = hashKey(0x42);
        encCS = hashKey(0x43);
        encSC = hashKey(0x44);
        macCS = hashKey(0x45);
        macSC = hashKey(0x46);
        if (debugger.secSshTraf) {
            logger.debug("hash=" + bits.byteDump(hashVal, 0, -1) + " k=" + difHel.common + " ivCS="
                    + bits.byteDump(ivCS, 0, -1) + " ivSC=" + bits.byteDump(ivSC, 0, -1) + " encCS=" + bits.byteDump(encCS, 0, -1)
                    + " encSC=" + bits.byteDump(encSC, 0, -1) + " macCS=" + bits.byteDump(macCS, 0, -1) + " macSC="
                    + bits.byteDump(macSC, 0, -1));
        }
    }

    /**
     * swap hash values
     */
    public void hashSwap() {
        List<Integer> h = hash2;
        hash2 = hash1;
        hash1 = h;
    }

    /**
     * merge hash2 to hash1
     */
    public void hashMerge() {
        hash1.addAll(hash2);
        hash2 = new ArrayList<Integer>();
    }

    /**
     * hash buffer
     *
     * @param buf buffer to use
     */
    public void hashBuf(byte[] buf) {
        for (int i = 0; i < buf.length; i++) {
            hash1.add(Integer.valueOf(buf[i]));
        }
    }

    /**
     * hash integer
     *
     * @param i int to add
     */
    public void hashInt(int i) {
        byte[] buf = new byte[4];
        bits.msbPutD(buf, 0, i);
        hashBuf(buf);
    }

    /**
     * add string to hash
     *
     * @param s string to add
     */
    public void hashStr(String s) {
        hashInt(s.length());
        hashBuf(s.getBytes());
    }

    /**
     * add current packet
     */
    public void hashPck() {
        lower.pckDat.merge2beg();
        hashInt(lower.pckDat.dataSize() + 1);
        hash1.add(lower.pckTyp);
        hashPay();
    }

    /**
     * add current packet
     */
    public void hashPay() {
        lower.pckDat.merge2beg();
        byte[] buf = lower.pckDat.getCopy();
        hashBuf(buf);
    }

    /**
     * add big integer
     *
     * @param b integer to add
     */
    public void hashBig(BigInteger b) {
        byte[] buf = b.toByteArray();
        hashInt(buf.length);
        hashBuf(buf);
    }

    /**
     * setup encryption
     *
     * @param pi init packet
     * @param client true for client, false for server
     */
    public void encSetup(packSshInit pi, boolean client) {
        if (client) {
            lower.encRx = pi.getCipher(pi.kexEncSC, encSC, ivSC, false);
            lower.encTx = pi.getCipher(pi.kexEncCS, encCS, ivCS, true);
            lower.macRx = pi.getHasher(pi.kexMacSC, macSC);
            lower.macTx = pi.getHasher(pi.kexMacCS, macCS);
        } else {
            lower.encRx = pi.getCipher(pi.kexEncCS, encCS, ivCS, false);
            lower.encTx = pi.getCipher(pi.kexEncSC, encSC, ivSC, true);
            lower.macRx = pi.getHasher(pi.kexMacCS, macCS);
            lower.macTx = pi.getHasher(pi.kexMacSC, macSC);
        }
        lower.padModulo = lower.encTx.getBlockSize();
    }

    /**
     * parse request message
     *
     * @return false on success, true on error
     */
    public boolean gexReqParse() {
        if (lower.pckTyp == packSsh.typeDHXold) {
            modBest = lower.pckDat.msbGetD(0);
            modMin = modBest;
            modMax = modBest;
            if (debugger.secSshTraf) {
                gexReqDump("rx");
            }
            return false;
        }
        if (lower.pckTyp != packSsh.typeDHXreq) {
            return true;
        }
        modMin = lower.pckDat.msbGetD(0);
        modBest = lower.pckDat.msbGetD(4);
        modMax = lower.pckDat.msbGetD(8);
        lower.pckDat.getSkip(12);
        if (debugger.secSshTraf) {
            gexReqDump("rx");
        }
        return false;
    }

    /**
     * create request message
     */
    public void gexReqCreate() {
        if (debugger.secSshTraf) {
            gexReqDump("tx");
        }
        lower.pckTyp = packSsh.typeDHXreq;
        lower.pckDat.clear();
        lower.pckDat.msbPutD(0, modMin);
        lower.pckDat.msbPutD(4, modBest);
        lower.pckDat.msbPutD(8, modMax);
        lower.pckDat.putSkip(12);
    }

    /**
     * fill request message
     */
    public void gexReqFill() {
        modMin = 1024;
        modBest = 2048;
        modMax = 4096;
    }

    private void gexReqDump(String dir) {
        logger.debug(dir + " min=" + modMin + " best=" + modBest + " max=" + modMax);
    }

    /**
     * parse group exchange message
     *
     * @return false on success, true on error
     */
    public boolean gexGroupParse() {
        if (lower.pckTyp != packSsh.typeDHXgrp) {
            return true;
        }
        difHel = new cryKeyDH();
        difHel.modulus = lower.bigIntRead();
        difHel.group = lower.bigIntRead();
        if (debugger.secSshTraf) {
            gexGroupDump("rx");
        }
        return false;
    }

    /**
     * fill group exchange message
     */
    public void gexGroupFill() {
        difHel = cryKeyDH.findGroup(modBest);
    }

    /**
     * create group exchange message
     */
    public void gexGroupCreate() {
        if (debugger.secSshTraf) {
            gexGroupDump("tx");
        }
        lower.pckTyp = packSsh.typeDHXgrp;
        lower.pckDat.clear();
        lower.bigIntWrite(difHel.modulus);
        lower.bigIntWrite(difHel.group);
    }

    private void gexGroupDump(String dir) {
        logger.debug(dir + " " + difHel);
    }

    /**
     * parse init message
     *
     * @return false on success, true on error
     */
    public boolean gexInitParse() {
        if (lower.pckTyp != packSsh.typeDHXinit) {
            return true;
        }
        difHel.clntPub = lower.bigIntRead();
        if (debugger.secSshTraf) {
            gexInitDump("rx");
        }
        return false;
    }

    /**
     * fill init message
     */
    public void gexInitFill() {
        difHel.clntXchg();
    }

    /**
     * create init message
     */
    public void gexInitCreate() {
        if (debugger.secSshTraf) {
            gexInitDump("tx");
        }
        lower.pckTyp = packSsh.typeDHXinit;
        lower.pckDat.clear();
        lower.bigIntWrite(difHel.clntPub);
    }

    private void gexInitDump(String dir) {
        logger.debug(dir + " e=" + difHel.clntPub);
    }

    /**
     * parse reply message
     *
     * @return false on success, true on error
     */
    public boolean gexReplyParse() {
        if (lower.pckTyp != packSsh.typeDHXrply) {
            return true;
        }
        cert = lower.bytesRead();
        difHel.servPub = lower.bigIntRead();
        sign = lower.bytesRead();
        if (debugger.secSshTraf) {
            gexReplyDump("rx");
        }
        return false;
    }

    /**
     * fill reply message
     *
     * @param signer signer alrorithm
     */
    public void gexReplyFill(cryKeyGeneric signer) {
        cert = signer.sshWriter();
        sign = signer.sshSigning(hashVal);
    }

    /**
     * create reply message
     */
    public void gexReplyCreate() {
        lower.pckTyp = packSsh.typeDHXrply;
        lower.pckDat.clear();
        lower.bytesWrite(cert);
        lower.bigIntWrite(difHel.servPub);
        lower.bytesWrite(sign);
        if (debugger.secSshTraf) {
            gexReplyDump("tx");
        }
    }

    private void gexReplyDump(String dir) {
        logger.debug(dir + " f=" + difHel.servPub + " sign=" + bits.byteDump(sign, 0, -1) + " cert="
                + bits.byteDump(cert, 0, -1));
    }

    /**
     * parse init message
     *
     * @return false on success, true on error
     */
    public boolean kexInitParse() {
        if (lower.pckTyp != packSsh.typeDHGinit) {
            return true;
        }
        difHel.clntPub = lower.bigIntRead();
        if (debugger.secSshTraf) {
            gexInitDump("rx");
        }
        return false;
    }

    /**
     * create init message
     */
    public void kexInitCreate() {
        if (debugger.secSshTraf) {
            gexInitDump("tx");
        }
        lower.pckTyp = packSsh.typeDHGinit;
        lower.pckDat.clear();
        lower.bigIntWrite(difHel.clntPub);
    }

    /**
     * parse reply message
     *
     * @return false on success, true on error
     */
    public boolean kexReplyParse() {
        if (lower.pckTyp != packSsh.typeDHGrply) {
            return true;
        }
        cert = lower.bytesRead();
        difHel.servPub = lower.bigIntRead();
        sign = lower.bytesRead();
        if (debugger.secSshTraf) {
            gexReplyDump("rx");
        }
        return false;
    }

    /**
     * create reply message
     */
    public void kexReplyCreate() {
        lower.pckTyp = packSsh.typeDHGrply;
        lower.pckDat.clear();
        lower.bytesWrite(cert);
        lower.bigIntWrite(difHel.servPub);
        lower.bytesWrite(sign);
        if (debugger.secSshTraf) {
            gexReplyDump("tx");
        }
    }

}
