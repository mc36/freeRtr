package pack;

import cry.cryEncrCBCaes;
import cry.cryEncrCBCblowfish;
import cry.cryEncrCBCdes;
import cry.cryEncrCBCdes3;
import cry.cryEncrCBCrc2;
import cry.cryEncrCTRaes;
import cry.cryEncrCTRblowfish;
import cry.cryEncrCTRdes;
import cry.cryEncrCTRdes3;
import cry.cryEncrCTRrc2;
import cry.cryEncrGeneric;
import cry.cryHashGeneric;
import cry.cryHashHmac;
import cry.cryHashMd5;
import cry.cryHashSha1;
import cry.cryHashSha2256;
import cry.cryKeyDH;
import cry.cryKeyDSA;
import cry.cryKeyGeneric;
import cry.cryKeyRSA;
import java.util.ArrayList;
import java.util.List;
import pipe.pipeSide;
import util.bits;
import util.cmds;
import util.debugger;
import util.logger;
import util.version;

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
     * key exchange algorithm
     */
    public int[] kexAlgo;

    /**
     * key storage algorithm
     */
    public int[] kexKeys;

    /**
     * encryption algorithm client->server
     */
    public int[] kexEncCS;

    /**
     * encryption algorithm server->client
     */
    public int[] kexEncSC;

    /**
     * hash algorithm client->server
     */
    public int[] kexMacCS;

    /**
     * hash algorithm server->client
     */
    public int[] kexMacSC;

    /**
     * compression algorithm client->server
     */
    public int[] kexCompCS;

    /**
     * compression algorithm server->client
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
    public final static String[] hasherAlgs = {"hmac-sha2-256", "hmac-sha1", "hmac-md5"};

    /**
     * compression algorithms
     */
    public final static String[] compressAlgs = {"none"};

    /**
     * kex algorithms
     */
    public final static String[] keyXchgAlgs = {"diffie-hellman-group-exchange-sha1", "diffie-hellman-group14-sha1", "diffie-hellman-group1-sha1"};

    /**
     * key algorithms
     */
    public final static String[] keySignAlgs = {cryKeyDSA.sshName, cryKeyRSA.sshName};

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
                h = new cryHashSha2256();
                break;
            case 1:
                h = new cryHashSha1();
                break;
            case 2:
                h = new cryHashMd5();
                break;
            default:
                return null;
        }
        byte[] keyT = new byte[h.getHashSize()];
        bits.byteCopy(keyO, 0, keyT, 0, keyT.length);
        cryHashHmac x = new cryHashHmac(h, keyT);
        return x;
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
        lower.pipe.lineTx = pipeSide.modTyp.modeLF;
        lower.pipe.lineRx = pipeSide.modTyp.modeLF;
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

    private int[] algoParseList(String s, String[] alg) {
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
     * @return false on success, true on error
     */
    public boolean kexInitParse() {
        if (lower.pckTyp != packSsh.typeKexInit) {
            return true;
        }
        kexCookie = new byte[16];
        for (int i = 0; i < kexCookie.length; i++) {
            kexCookie[i] = (byte) lower.pckDat.getByte(i);
        }
        lower.pckDat.getSkip(kexCookie.length);
        kexAlgo = algoParseList(lower.stringRead(), keyXchgAlgs);
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
     */
    public void kexInitFill() {
        kexCookie = new byte[16];
        for (int i = 0; i < kexCookie.length; i++) {
            kexCookie[i] = (byte) bits.randomB();
        }
        kexAlgo = algoFillFull(keyXchgAlgs);
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
            c.kexInitFill();
        }
        if (s == null) {
            s = new packSshInit(lower);
            s.kexInitFill();
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
     */
    public void kexInitCreate() {
        lower.pckTyp = packSsh.typeKexInit;
        lower.pckDat.clear();
        for (int i = 0; i < kexCookie.length; i++) {
            lower.pckDat.putByte(i, kexCookie[i]);
        }
        lower.pckDat.putSkip(kexCookie.length);
        lower.stringWrite(altoCreateList(kexAlgo, keyXchgAlgs));
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
        logger.debug(dir + " kex=" + altoCreateList(kexAlgo, keyXchgAlgs) + " sng=" + altoCreateList(kexKeys, keySignAlgs)
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
                return cryKeyDH.getGroup(14);
            case 2:
                return cryKeyDH.getGroup(1);
            default:
                return null;
        }
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
        switch (kexKeys[0]) {
            case 0:
                return new cryKeyDSA();
            case 1:
                return new cryKeyRSA();
            default:
                return null;
        }
    }

    /**
     * get chosen key signer
     *
     * @param dss dss key to use
     * @param rsa rsa key to use
     * @return key signer
     */
    public cryKeyGeneric getKeySigner(cryKeyGeneric dss, cryKeyGeneric rsa) {
        if (kexKeys.length < 1) {
            return null;
        }
        switch (kexKeys[0]) {
            case 0:
                return dss;
            case 1:
                return rsa;
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

}
