package net.freertr.pack;

import java.util.ArrayList;
import java.util.List;
import net.freertr.cry.cryEncrCBCaes;
import net.freertr.cry.cryEncrCBCblowfish;
import net.freertr.cry.cryEncrCBCdes;
import net.freertr.cry.cryEncrCBCdes3;
import net.freertr.cry.cryEncrCBCrc2;
import net.freertr.cry.cryEncrCTRaes;
import net.freertr.cry.cryEncrCTRblowfish;
import net.freertr.cry.cryEncrCTRdes;
import net.freertr.cry.cryEncrCTRdes3;
import net.freertr.cry.cryEncrCTRrc2;
import net.freertr.cry.cryEncrGeneric;
import net.freertr.cry.cryHashGeneric;
import net.freertr.cry.cryHashHmac;
import net.freertr.cry.cryHashMd5;
import net.freertr.cry.cryHashSha1;
import net.freertr.cry.cryHashSha2256;
import net.freertr.cry.cryHashSha2512;
import net.freertr.cry.cryKeyDH;
import net.freertr.pipe.pipeSide;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.debugger;
import net.freertr.util.logger;
import net.freertr.util.version;

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
    public packSshSign kexKeys;

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
     * compression algorithms
     */
    public final static String[] compressAlgs = {"none"};

    /**
     * kex algorithms
     */
    public final static String[] keyXchgAlgs = {"diffie-hellman-group-exchange-sha256", "diffie-hellman-group16-sha512", "diffie-hellman-group18-sha512", "diffie-hellman-group14-sha256", "diffie-hellman-group-exchange-sha1", "diffie-hellman-group14-sha1", "diffie-hellman-group1-sha1"};

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
        return "SSH-2.0-" + version.usrAgnt;
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
        kexKeys = new packSshSign(lower.stringRead());
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
     */
    public void kexInitFill() {
        kexCookie = new byte[16];
        for (int i = 0; i < kexCookie.length; i++) {
            kexCookie[i] = (byte) bits.randomB();
        }
        kexAlgo = algoFillFull(keyXchgAlgs);
        kexExts = true;
        kexKeys = new packSshSign("");
        kexKeys.algo = algoFillFull(packSshSign.keySignAlgs);
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
            c.kexInitFill();
        }
        if (s == null) {
            s = new packSshInit(lower);
            s.kexInitFill();
        }
        kexAlgo = algoChoose(c.kexAlgo, s.kexAlgo);
        kexKeys.algo = algoChoose(c.kexKeys.algo, s.kexKeys.algo);
        kexEncCS = algoChoose(c.kexEncCS, s.kexEncCS);
        kexEncSC = algoChoose(c.kexEncSC, s.kexEncSC);
        kexMacCS = algoChoose(c.kexMacCS, s.kexMacCS);
        kexMacSC = algoChoose(c.kexMacSC, s.kexMacSC);
        kexCompCS = algoChoose(c.kexCompCS, s.kexCompCS);
        kexCompSC = algoChoose(c.kexCompSC, s.kexCompSC);
        if ((kexAlgo == null) || (kexKeys.algo == null)) {
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
        lower.stringWrite(altoCreateList(kexKeys.algo, packSshSign.keySignAlgs));
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
        logger.debug(dir + " ext=" + kexExts + " kex=" + altoCreateList(kexAlgo, keyXchgAlgs) + " sng=" + altoCreateList(kexKeys.algo, packSshSign.keySignAlgs)
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
    public cryKeyDH getDHgroup() {
        if (kexAlgo.length < 1) {
            return null;
        }
        switch (kexAlgo[0]) {
            case 0:
                return null;
            case 1:
                return cryKeyDH.getGroup(16);
            case 2:
                return cryKeyDH.getGroup(18);
            case 3:
                return cryKeyDH.getGroup(14);
            case 4:
                return null;
            case 5:
                return cryKeyDH.getGroup(14);
            case 6:
                return cryKeyDH.getGroup(1);
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
                return new cryHashSha2256();
            case 1:
                return new cryHashSha2512();
            case 2:
                return new cryHashSha2512();
            case 3:
                return new cryHashSha2256();
            case 4:
                return new cryHashSha1();
            case 5:
                return new cryHashSha1();
            case 6:
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
        packSshSign algs = new packSshSign("");
        algs.algo = algoFillFull(packSshSign.keySignAlgs);
        lower.stringWrite(altoCreateList(algs.algo, packSshSign.keySignAlgs));
    }

}
