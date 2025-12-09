package org.freertr.pack;

import java.util.AbstractList;
import java.util.ArrayList;
import java.util.List;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgInit;
import org.freertr.cry.cryEncrCBCaes;
import org.freertr.cry.cryEncrCBCblowfish;
import org.freertr.cry.cryEncrCBCdes;
import org.freertr.cry.cryEncrCBCdes3;
import org.freertr.cry.cryEncrCBCrc2;
import org.freertr.cry.cryEncrCTRaes;
import org.freertr.cry.cryEncrCTRblowfish;
import org.freertr.cry.cryEncrCTRdes;
import org.freertr.cry.cryEncrCTRdes3;
import org.freertr.cry.cryEncrCTRrc2;
import org.freertr.cry.cryEncrGeneric;
import org.freertr.cry.cryHashGeneric;
import org.freertr.cry.cryHashHmac;
import org.freertr.cry.cryHashMd5;
import org.freertr.cry.cryHashSha1;
import org.freertr.cry.cryHashSha2256;
import org.freertr.cry.cryHashSha2384;
import org.freertr.cry.cryHashSha2512;
import org.freertr.cry.cryKeyCurve25519;
import org.freertr.cry.cryKeyDH;
import org.freertr.cry.cryKeyDSA;
import org.freertr.cry.cryKeyECDH;
import org.freertr.cry.cryKeyECDSA;
import org.freertr.cry.cryKeyGeneric;
import org.freertr.cry.cryKeyMLDSA;
import org.freertr.cry.cryKeyMLKEM;
import org.freertr.cry.cryKeyPQhybrid;
import org.freertr.cry.cryKeyRSA;
import org.freertr.pipe.pipeSide;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.debugger;
import org.freertr.util.logger;

/**
 * secure shell kex init (rfc4253) protocol
 *
 * @author matecsaba
 */
public class packSshInit {

    /**
     * remote version number
     */
    public String remoteVersion;

    /**
     * key exchange cookie
     */
    public byte[] kexCookie;

    /**
     * key exchange extensions
     */
    public boolean kexExts;

    /**
     * key exchange algorithm
     */
    public int[] kexAlgo;

    /**
     * key storage algorithm
     */
    public int[] kexKeys;

    /**
     * encryption algorithm client2server
     */
    public int[] kexEncCS;

    /**
     * encryption algorithm server2client
     */
    public int[] kexEncSC;

    /**
     * hash algorithm client2server
     */
    public int[] kexMacCS;

    /**
     * hash algorithm server2client
     */
    public int[] kexMacSC;

    /**
     * compression algorithm client2server
     */
    public int[] kexCompCS;

    /**
     * compression algorithm server2client
     */
    public int[] kexCompSC;

    /**
     * first key exchange packet follows
     */
    public boolean kexFirst;

    /**
     * encryption algorithms
     */
    public final static String[] cipherAlgs = {"aes256-ctr", "aes256-cbc", "3des-ctr", "3des-cbc", "blowfish-ctr", "blowfish-cbc", "rc2-ctr", "rc2-cbc", "des-ctr", "des-cbc"};

    /**
     * hash algorithms
     */
    public final static String[] hasherAlgs = {"hmac-sha2-512", "hmac-sha2-256", "hmac-sha1", "hmac-md5"};

    /**
     * key algorithm names
     */
    public String[] keySignAlgs = null;

    /**
     * key algorithm hashers
     */
    public List<cryHashGeneric> keySignHash;

    /**
     * key algorithms workers
     */
    public List<cryKeyGeneric> keySignDoer;

    /**
     * compression algorithms
     */
    public final static String[] compressAlgs = {"none"};

    /**
     * kex algorithms
     */
    public final static String[] keyXchgAlgs = {"mlkem1024-sha384", "mlkem768-sha256", "mlkem512-sha256", "mlkem768x25519-sha256", "curve25519-sha256", "ecdh-sha2-nistp521", "ecdh-sha2-nistp384", "ecdh-sha2-nistp256", "ecdh-sha2-nistp224", "ecdh-sha2-nistp192", "diffie-hellman-group-exchange-sha256", "diffie-hellman-group16-sha512", "diffie-hellman-group18-sha512", "diffie-hellman-group14-sha256", "diffie-hellman-group-exchange-sha1", "diffie-hellman-group14-sha1", "diffie-hellman-group1-sha1"};

    private final packSsh lower;

    /**
     * create instance
     *
     * @param pack lower layer
     */
    public packSshInit(packSsh pack) {
        lower = pack;
    }

    /**
     * get chosen key verifier
     *
     * @return key verifier
     */
    public cryKeyGeneric getKeyVerifier() {
        if (kexKeys.length < 1) {
            return null;
        }
        return keySignDoer.get(kexKeys[0]);
    }

    /**
     * get key hash algorithm
     *
     * @return hash
     */
    public cryHashGeneric getKeyHashAlgo() {
        if (kexKeys.length < 1) {
            return null;
        }
        return keySignHash.get(kexKeys[0]);
    }

    /**
     * get key algorithm name
     *
     * @return name
     */
    public String getKeyHashAlgn() {
        if (kexKeys.length < 1) {
            return null;
        }
        return keySignAlgs[kexKeys[0]];
    }

    /**
     * populate from algorithm list
     *
     * @param s string to read
     */
    public void setupKeyVerifier(String s) {
        kexKeys = packSshInit.algoParseList(s, keySignAlgs);
    }

    /**
     * get hasher algorithm
     *
     * @param lst list of algorithms
     * @param keyO hash key
     * @return hasher
     */
    public cryHashHmac getHasher(int[] lst, byte[] keyO) {
        cryHashGeneric h;
        switch (lst[0]) {
            case 0:
                h = new cryHashSha2512();
                break;
            case 1:
                h = new cryHashSha2256();
                break;
            case 2:
                h = new cryHashSha1();
                break;
            case 3:
                h = new cryHashMd5();
                break;
            default:
                return null;
        }
        byte[] keyT = new byte[h.getHashSize()];
        bits.byteCopy(keyO, 0, keyT, 0, keyT.length);
        return new cryHashHmac(h, keyT);
    }

    /**
     * get cipher algorithm
     *
     * @param alg list of algorithms
     * @param keyO key value
     * @param ivO iv value
     * @param encr true for encryption, false for decryption
     * @return cipher
     */
    public cryEncrGeneric getCipher(int[] alg, byte[] keyO, byte[] ivO, boolean encr) {
        cryEncrGeneric e;
        switch (alg[0]) {
            case 0:
                e = new cryEncrCTRaes();
                break;
            case 1:
                e = new cryEncrCBCaes();
                break;
            case 2:
                e = new cryEncrCTRdes3();
                break;
            case 3:
                e = new cryEncrCBCdes3();
                break;
            case 4:
                e = new cryEncrCTRblowfish();
                break;
            case 5:
                e = new cryEncrCBCblowfish();
                break;
            case 6:
                e = new cryEncrCTRrc2();
                break;
            case 7:
                e = new cryEncrCBCrc2();
                break;
            case 8:
                e = new cryEncrCTRdes();
                break;
            case 9:
                e = new cryEncrCBCdes();
                break;
            default:
                return null;
        }
        byte[] ivT = new byte[e.getBlockSize()];
        bits.byteCopy(ivO, 0, ivT, 0, ivT.length);
        byte[] keyT = new byte[e.getKeySize()];
        bits.byteCopy(keyO, 0, keyT, 0, keyT.length);
        e.init(keyT, ivT, encr);
        return e;
    }

    /**
     * get ssh version string
     *
     * @return version string
     */
    public static String getLocalVersion() {
        if (cfgAll.sshAgent == null) {
            return "SSH-2.0-" + cfgInit.versionAgent;
        } else {
            return "SSH-2.0-" + cfgAll.sshAgent;
        }
    }

    /**
     * exchange version string
     */
    public void exchangeVersion() {
        lower.pipe.lineTx = pipeSide.modTyp.modeCRLF;
        lower.pipe.lineRx = pipeSide.modTyp.modeCRLF;
        lower.pipe.linePut(getLocalVersion());
        String s = "";
        for (;;) {
            if (lower.pipe.isClosed() != 0) {
                break;
            }
            s = lower.pipe.lineGet(1);
            if (s.length() > 0) {
                break;
            }
        }
        remoteVersion = s;
        if (debugger.secSshTraf) {
            logger.debug("remote: " + s);
        }
    }

    /**
     * key algorithms
     *
     * @param s stirng to read
     * @param alg algorithm to find
     * @return true if found, false if not
     */
    public static boolean algoFindList(String s, String alg) {
        cmds cmd = new cmds("", s);
        for (;;) {
            s = cmd.word(",");
            if (s.length() < 1) {
                break;
            }
            s = s.trim();
            if (s.equals(alg)) {
                return true;
            }
        }
        return false;
    }

    /**
     * key algorithms
     *
     * @param s stirng to read
     * @param alg list of available algorithms
     * @return list of supported algorithms
     */
    public static int[] algoParseList(String s, String[] alg) {
        List<Integer> l1 = new ArrayList<Integer>();
        cmds cmd = new cmds("", s);
        for (;;) {
            s = cmd.word(",");
            if (s.length() < 1) {
                break;
            }
            s = s.trim();
            for (int i = 0; i < alg.length; i++) {
                if (!s.equals(alg[i])) {
                    continue;
                }
                l1.add(i);
                break;
            }
        }
        int[] l2 = new int[l1.size()];
        for (int i = 0; i < l2.length; i++) {
            l2[i] = l1.get(i);
        }
        return l2;
    }

    private int[] algoFillFull(String[] alg) {
        int[] l = new int[alg.length];
        for (int i = 0; i < alg.length; i++) {
            l[i] = i;
        }
        return l;
    }

    private String altoCreateList(int[] lst, String[] alg) {
        String s = "";
        for (int i = 0; i < lst.length; i++) {
            s = s + "," + alg[lst[i]];
        }
        if (s.length() < 1) {
            return "";
        }
        return s.substring(1, s.length());
    }

    private int[] algoChoose(int[] c, int[] s) {
        for (int o = 0; o < c.length; o++) {
            for (int i = 0; i < s.length; i++) {
                if (c[o] != s[i]) {
                    continue;
                }
                int[] l = new int[1];
                l[0] = c[o];
                return l;
            }
        }
        return null;
    }

    /**
     * parse key exchange message
     *
     * @param client set true if client, false on server
     * @return false on success, true on error
     */
    public boolean kexInitParse(boolean client) {
        if (lower.pckTyp != packSsh.typeKexInit) {
            return true;
        }
        kexCookie = new byte[16];
        for (int i = 0; i < kexCookie.length; i++) {
            kexCookie[i] = (byte) lower.pckDat.getByte(i);
        }
        lower.pckDat.getSkip(kexCookie.length);
        String a = lower.stringRead();
        if (client) {
            kexExts = algoFindList(a, "ext-info-s");
        } else {
            kexExts = algoFindList(a, "ext-info-c");
        }
        kexAlgo = algoParseList(a, keyXchgAlgs);
        kexKeys = algoParseList(lower.stringRead(), keySignAlgs);
        kexEncCS = algoParseList(lower.stringRead(), cipherAlgs);
        kexEncSC = algoParseList(lower.stringRead(), cipherAlgs);
        kexMacCS = algoParseList(lower.stringRead(), hasherAlgs);
        kexMacSC = algoParseList(lower.stringRead(), hasherAlgs);
        kexCompCS = algoParseList(lower.stringRead(), compressAlgs);
        kexCompSC = algoParseList(lower.stringRead(), compressAlgs);
        lower.stringRead(); // kex lang cs
        lower.stringRead(); // kex lang sc
        kexFirst = lower.pckDat.getByte(0) != 0;
        // int reserved = lower.pckDat.getByte(1);
        lower.pckDat.getSkip(5);
        if (debugger.secSshTraf) {
            kexInitDump("rx");
        }
        return false;
    }

    /**
     * fill in values for key exchange message
     *
     * @param client set true if client, false on server
     * @param dsa dsa key to use
     * @param rsa rsa key to use
     * @param ecdsa ecdsa key to use
     * @param mldsa mldsa key to use
     */
    public void kexInitFill(boolean client, cryKeyDSA dsa, cryKeyRSA rsa, cryKeyECDSA ecdsa, cryKeyMLDSA mldsa) {
        kexCookie = new byte[16];
        for (int i = 0; i < kexCookie.length; i++) {
            kexCookie[i] = (byte) bits.randomB();
        }
        List<String> lst = new ArrayList<String>();
        keySignHash = new ArrayList<cryHashGeneric>();
        keySignDoer = new ArrayList<cryKeyGeneric>();
        if (client) {
            cryKeyMLDSA ml = new cryKeyMLDSA();
            ml.keyMakeSize(44);
            keySignDoer.add(ml);
            keySignHash.add(ml.sshHash());
            lst.add(ml.sshName());
            ml = new cryKeyMLDSA();
            ml.keyMakeSize(65);
            keySignDoer.add(ml);
            keySignHash.add(ml.sshHash());
            lst.add(ml.sshName());
            ml = new cryKeyMLDSA();
            ml.keyMakeSize(87);
            keySignDoer.add(ml);
            keySignHash.add(ml.sshHash());
            lst.add(ml.sshName());
            cryKeyECDSA ec = new cryKeyECDSA();
            ec.keyMakeTls(19);
            keySignDoer.add(ec);
            keySignHash.add(ec.sshHash());
            lst.add(ec.sshName());
            ec = new cryKeyECDSA();
            ec.keyMakeTls(21);
            keySignDoer.add(ec);
            keySignHash.add(ec.sshHash());
            lst.add(ec.sshName());
            ec = new cryKeyECDSA();
            ec.keyMakeTls(23);
            keySignDoer.add(ec);
            keySignHash.add(ec.sshHash());
            lst.add(ec.sshName());
            ec = new cryKeyECDSA();
            ec.keyMakeTls(24);
            keySignDoer.add(ec);
            keySignHash.add(ec.sshHash());
            lst.add(ec.sshName());
            keySignDoer.add(new cryKeyRSA());
            keySignHash.add(new cryHashSha2256());
            lst.add("rsa-sha2-256");
            keySignDoer.add(new cryKeyRSA());
            keySignHash.add(new cryHashSha2512());
            lst.add("rsa-sha2-512");
            keySignDoer.add(new cryKeyRSA());
            keySignHash.add(new cryHashSha1());
            lst.add(cryKeyRSA.sshName);
            keySignDoer.add(new cryKeyDSA());
            keySignHash.add(new cryHashSha1());
            lst.add(cryKeyDSA.sshName);
        } else {
            if (mldsa != null) {
                keySignDoer.add(mldsa);
                keySignHash.add(mldsa.sshHash());
                lst.add(mldsa.sshName());
            }
            if (ecdsa != null) {
                keySignDoer.add(ecdsa);
                keySignHash.add(ecdsa.sshHash());
                lst.add(ecdsa.sshName());
            }
            if (rsa != null) {
                keySignDoer.add(rsa);
                keySignHash.add(new cryHashSha2256());
                lst.add("rsa-sha2-256");
                keySignDoer.add(rsa);
                keySignHash.add(new cryHashSha2512());
                lst.add("rsa-sha2-512");
                keySignDoer.add(rsa);
                keySignHash.add(new cryHashSha1());
                lst.add(cryKeyRSA.sshName);
            }
            if (dsa != null) {
                keySignDoer.add(dsa);
                keySignHash.add(new cryHashSha1());
                lst.add(cryKeyDSA.sshName);
            }
        }
        keySignAlgs = new String[lst.size()];
        for (int i = 0; i < keySignAlgs.length; i++) {
            keySignAlgs[i] = lst.get(i);
        }
        kexAlgo = algoFillFull(keyXchgAlgs);
        kexExts = true;
        kexKeys = algoFillFull(keySignAlgs);
        kexEncCS = algoFillFull(cipherAlgs);
        kexEncSC = algoFillFull(cipherAlgs);
        kexMacCS = algoFillFull(hasherAlgs);
        kexMacSC = algoFillFull(hasherAlgs);
        kexCompCS = algoFillFull(compressAlgs);
        kexCompSC = algoFillFull(compressAlgs);
    }

    /**
     * choose best algorithms
     *
     * @param c client kex packet
     * @param s server kex packet
     * @return false on success, true on error
     */
    public boolean kexInitChoose(packSshInit c, packSshInit s) {
        if (c == null) {
            c = new packSshInit(lower);
            c.kexInitFill(true, null, null, null, null);
        }
        if (s == null) {
            s = new packSshInit(lower);
            s.kexInitFill(true, null, null, null, null);
        }
        kexAlgo = algoChoose(c.kexAlgo, s.kexAlgo);
        kexKeys = algoChoose(c.kexKeys, s.kexKeys);
        kexEncCS = algoChoose(c.kexEncCS, s.kexEncCS);
        kexEncSC = algoChoose(c.kexEncSC, s.kexEncSC);
        kexMacCS = algoChoose(c.kexMacCS, s.kexMacCS);
        kexMacSC = algoChoose(c.kexMacSC, s.kexMacSC);
        kexCompCS = algoChoose(c.kexCompCS, s.kexCompCS);
        kexCompSC = algoChoose(c.kexCompSC, s.kexCompSC);
        if ((kexAlgo == null) || (kexKeys == null)) {
            return true;
        }
        if ((kexEncCS == null) || (kexEncSC == null)) {
            return true;
        }
        if ((kexMacCS == null) || (kexMacSC == null)) {
            return true;
        }
        if ((kexCompCS == null) || (kexCompSC == null)) {
            return true;
        }
        if (debugger.secSshTraf) {
            kexInitDump("final");
        }
        return false;
    }

    /**
     * create key exchange message
     *
     * @param client set true if client, false on server
     */
    public void kexInitCreate(boolean client) {
        lower.pckTyp = packSsh.typeKexInit;
        lower.pckDat.clear();
        for (int i = 0; i < kexCookie.length; i++) {
            lower.pckDat.putByte(i, kexCookie[i]);
        }
        lower.pckDat.putSkip(kexCookie.length);
        String a = "";
        if (kexExts) {
            if (client) {
                a += "ext-info-c,";
            } else {
                a += "ext-info-s,";
            }
        }
        lower.stringWrite(a + altoCreateList(kexAlgo, keyXchgAlgs));
        lower.stringWrite(altoCreateList(kexKeys, keySignAlgs));
        lower.stringWrite(altoCreateList(kexEncCS, cipherAlgs));
        lower.stringWrite(altoCreateList(kexEncSC, cipherAlgs));
        lower.stringWrite(altoCreateList(kexMacCS, hasherAlgs));
        lower.stringWrite(altoCreateList(kexMacSC, hasherAlgs));
        lower.stringWrite(altoCreateList(kexCompCS, compressAlgs));
        lower.stringWrite(altoCreateList(kexCompSC, compressAlgs));
        lower.stringWrite("");
        lower.stringWrite("");
        if (kexFirst) {
            lower.pckDat.putByte(0, 1);
        } else {
            lower.pckDat.putByte(0, 0);
        }
        lower.pckDat.msbPutD(1, 0);
        lower.pckDat.putSkip(5);
        if (debugger.secSshTraf) {
            kexInitDump("tx");
        }
    }

    private void kexInitDump(String dir) {
        logger.debug(dir + " ext=" + kexExts + " kex=" + altoCreateList(kexAlgo, keyXchgAlgs) + " sng=" + altoCreateList(kexKeys, keySignAlgs)
                + " encCS=" + altoCreateList(kexEncCS, cipherAlgs) + " encSC=" + altoCreateList(kexEncSC, cipherAlgs) + " macCS="
                + altoCreateList(kexMacCS, hasherAlgs) + " macSC=" + altoCreateList(kexMacSC, hasherAlgs) + " cmpCS="
                + altoCreateList(kexCompCS, compressAlgs) + " cmpSC=" + altoCreateList(kexCompSC, compressAlgs) + " frst="
                + kexFirst);
    }

    /**
     * get chonsen dh group
     *
     * @return dh group, null for group exchange
     */
    public cryKeyGeneric getDHgroup() {
        if (kexAlgo.length < 1) {
            return null;
        }
        switch (kexAlgo[0]) {
            case 0:
                cryKeyMLKEM ml = new cryKeyMLKEM();
                ml.keyMakeSize(1024);
                return ml;
            case 1:
                ml = new cryKeyMLKEM();
                ml.keyMakeSize(768);
                return ml;
            case 2:
                ml = new cryKeyMLKEM();
                ml.keyMakeSize(512);
                return ml;
            case 3:
                return new cryKeyPQhybrid();
            case 4:
                return new cryKeyCurve25519();
            case 5:
                cryKeyECDH ec = new cryKeyECDH();
                ec.keyMakeTls(25);
                return ec;
            case 6:
                ec = new cryKeyECDH();
                ec.keyMakeTls(24);
                return ec;
            case 7:
                ec = new cryKeyECDH();
                ec.keyMakeTls(23);
                return ec;
            case 8:
                ec = new cryKeyECDH();
                ec.keyMakeTls(21);
                return ec;
            case 9:
                ec = new cryKeyECDH();
                ec.keyMakeTls(19);
                return ec;
            case 10:
                return new cryKeyDH();
            case 11:
                cryKeyDH mp = new cryKeyDH();
                mp.keyMakeIke(16);
                return mp;
            case 12:
                mp = new cryKeyDH();
                mp.keyMakeIke(18);
                return mp;
            case 13:
                mp = new cryKeyDH();
                mp.keyMakeIke(14);
                return mp;
            case 14:
                return new cryKeyDH();
            case 15:
                mp = new cryKeyDH();
                mp.keyMakeIke(14);
                return mp;
            case 16:
                mp = new cryKeyDH();
                mp.keyMakeIke(1);
                return mp;
            default:
                return null;
        }
    }

    /**
     * get chonsen dh hash
     *
     * @return dh hash
     */
    public cryHashGeneric getDHhash() {
        if (kexAlgo.length < 1) {
            return null;
        }
        switch (kexAlgo[0]) {
            case 0:
                return new cryHashSha2384();
            case 1:
                return new cryHashSha2256();
            case 2:
                return new cryHashSha2256();
            case 3:
                return new cryHashSha2256();
            case 4:
                return new cryHashSha2256();
            case 5:
                return new cryHashSha2512();
            case 6:
                return new cryHashSha2384();
            case 7:
                return new cryHashSha2256();
            case 8:
                return new cryHashSha2256();
            case 9:
                return new cryHashSha2256();
            case 10:
                return new cryHashSha2256();
            case 11:
                return new cryHashSha2512();
            case 12:
                return new cryHashSha2512();
            case 13:
                return new cryHashSha2256();
            case 14:
                return new cryHashSha1();
            case 15:
                return new cryHashSha1();
            case 16:
                return new cryHashSha1();
            default:
                return null;
        }
    }

    /**
     * parse new keys message
     *
     * @return false on success, true on error
     */
    public boolean newKeysParse() {
        if (lower.pckTyp != packSsh.typeNewKeys) {
            return true;
        }
        return false;
    }

    /**
     * create new keys message
     */
    public void newKeysCreate() {
        lower.pckTyp = packSsh.typeNewKeys;
        lower.pckDat.clear();
    }

    /**
     * exchange new keys message
     *
     * @return false on success, true on error
     */
    public boolean newKeysExchange() {
        newKeysCreate();
        lower.packSend();
        lower.packRecv();
        return newKeysParse();
    }

    /**
     * parse extension info message
     *
     * @return false on success, true on error
     */
    public boolean extensInfoParse() {
        if (lower.pckTyp != packSsh.typeExtInfo) {
            return true;
        }
        return false;
    }

    /**
     * create extension info message
     */
    public void extensInfoCreate() {
        lower.pckDat.clear();
        lower.pckTyp = packSsh.typeExtInfo;
        lower.pckDat.msbPutD(0, 1); // number of extensions
        lower.pckDat.putSkip(4);
        lower.stringWrite("server-sig-algs");
        lower.stringWrite(altoCreateList(algoFillFull(keySignAlgs), keySignAlgs));
    }

}
