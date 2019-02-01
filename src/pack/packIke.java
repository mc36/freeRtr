package pack;

import addr.addrIP;
import addr.addrIPv4;
import addr.addrIPv6;
import cry.cryEncrGeneric;
import cry.cryHashGeneric;
import cry.cryKeyDH;
import cry.cryKeyGeneric;
import cry.cryUtils;
import java.util.ArrayList;
import java.util.List;
import sec.secTransform;
import util.bits;
import util.debugger;
import util.logger;

/**
 * internet key exchange protocol (rfc5996) packet
 *
 * @author matecsaba
 */
public class packIke {

    /**
     * packet holder
     */
    public packHolder pckDat;

    /**
     * binary packet data
     */
    public byte[] pckBin;

    /**
     * size of packet
     */
    public int pckSiz;

    /**
     * initiator cookie
     */
    public long cookieI;

    /**
     * responder cookie
     */
    public long cookieR;

    /**
     * next header
     */
    public int pckNxt;

    /**
     * exchange type
     */
    public int xchgTyp;

    /**
     * flags
     */
    public int flags;

    /**
     * message id
     */
    public int msgId;

    /**
     * proposal number
     */
    public int proposNum;

    /**
     * protocol number
     */
    public int protoId;

    /**
     * initiator spi value
     */
    public int spiValI;

    /**
     * responder spi value
     */
    public int spiValR;

    /**
     * initiator spi old
     */
    public int spiOldI;

    /**
     * responder spi old
     */
    public int spiOldR;

    /**
     * selected transform
     */
    public secTransform transform;

    /**
     * true if this side is initiator, false for responder
     */
    public boolean initiator;

    /**
     * preshared key
     */
    public String preshared;

    /**
     * diffie hellman keys
     */
    public cryKeyDH diffie;

    /**
     * initiator nonce
     */
    public byte[] nonceI;

    /**
     * responder nonce
     */
    public byte[] nonceR;

    /**
     * diffie hellman common secret
     */
    public byte[] dhcomm;

    /**
     * skeyid
     */
    public byte[] skeyidG;

    /**
     * skeyid-d
     */
    public byte[] skeyidD;

    /**
     * skeyid-a
     */
    public byte[] skeyidA;

    /**
     * skeyid-e
     */
    public byte[] skeyidE;

    /**
     * skeyid-p
     */
    public byte[] skeyidP;

    /**
     * initiator's message
     */
    public byte[] msgI;

    /**
     * responder's message
     */
    public byte[] msgR;

    /**
     * initiator's identity
     */
    public byte[] idnI;

    /**
     * responder's identity
     */
    public byte[] idnR;

    /**
     * identification data
     */
    public addrIP ident;

    /**
     * port number
     */
    public final static int port = 500;

    /**
     * size of header
     */
    public final static int headSize = 28;

    /**
     * size of payload
     */
    public final static int paySize = 4;

    /**
     * none
     */
    public final static int payNone = 0;

    /**
     * security association
     */
    public final static int paySecAss = 33;

    /**
     * key exchange
     */
    public final static int payKeyEx = 34;

    /**
     * identification initiator
     */
    public final static int payIdntInit = 35;

    /**
     * identification responder
     */
    public final static int payIdntRspn = 36;

    /**
     * certificate data
     */
    public final static int payCrtDat = 37;

    /**
     * certificate request
     */
    public final static int payCrtReq = 38;

    /**
     * authentication
     */
    public final static int payAuthen = 39;

    /**
     * nonce
     */
    public final static int payNonce = 40;

    /**
     * notify
     */
    public final static int payNotify = 41;

    /**
     * delete
     */
    public final static int payDelete = 42;

    /**
     * vendor is
     */
    public final static int payVendor = 43;

    /**
     * traffic selector initiator
     */
    public final static int payTraffInit = 44;

    /**
     * traffic selector responder
     */
    public final static int payTraffResp = 45;

    /**
     * encrypted and authenticated
     */
    public final static int payEncrypt = 46;

    /**
     * configuration
     */
    public final static int payConfig = 47;

    /**
     * extensible authentication protocol
     */
    public final static int payExtAuth = 48;

    /**
     * ike sa init
     */
    public final static int xchgIkeSa = 34;

    /**
     * ike auth
     */
    public final static int xchgAuth = 35;

    /**
     * create child sa
     */
    public final static int xchgChild = 36;

    /**
     * informational
     */
    public final static int xchgInfo = 37;

    /**
     * initiator
     */
    public final static int flagInit = 0x08;

    /**
     * higher version
     */
    public final static int flagVers = 0x10;

    /**
     * response packet
     */
    public final static int flagResp = 0x20;

    /**
     * create new packet holder
     */
    public packIke() {
        pckDat = new packHolder(true, true);
    }

    /**
     * copy bytes of packet
     *
     * @return new instance
     */
    public packIke copyBytes() {
        packIke n = new packIke();
        n.cookieI = cookieI;
        n.cookieR = cookieR;
        n.transform = transform;
        n.initiator = initiator;
        n.preshared = preshared;
        n.diffie = diffie;
        n.nonceI = nonceI;
        n.nonceR = nonceR;
        n.skeyidA = skeyidA;
        n.skeyidD = skeyidD;
        n.skeyidE = skeyidE;
        n.skeyidG = skeyidG;
        n.skeyidP = skeyidP;
        n.msgI = msgI;
        n.msgR = msgR;
        n.idnI = idnI;
        n.idnR = idnR;
        n.msgId = msgId;
        n.spiValI = spiValI;
        n.spiValR = spiValR;
        n.spiOldI = spiOldI;
        n.spiOldR = spiOldR;
        return n;
    }

    /**
     * check if this is my peer
     *
     * @param c to check
     * @return true if yes, false if no
     */
    public boolean checkPeer(packIke c) {
        if (c == null) {
            return false;
        }
        if (c.cookieI != cookieI) {
            return false;
        }
        if (c.cookieR != cookieR) {
            return false;
        }
        return true;
    }

    /**
     * convert exchange type to string
     *
     * @param i exchange type
     * @return string
     */
    public static String exchange2string(int i) {
        switch (i) {
            case xchgIkeSa:
                return "ikeSa";
            case xchgAuth:
                return "ikeAuth";
            case xchgChild:
                return "childSa";
            case xchgInfo:
                return "inform";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * convert payload to string
     *
     * @param i payload type
     * @return string
     */
    public static String payload2string(int i) {
        switch (i) {
            case payNone:
                return "none";
            case paySecAss:
                return "secAss";
            case payKeyEx:
                return "kex";
            case payIdntInit:
                return "idi";
            case payIdntRspn:
                return "idr";
            case payCrtDat:
                return "cert";
            case payCrtReq:
                return "certReq";
            case payAuthen:
                return "auth";
            case payNonce:
                return "nonce";
            case payNotify:
                return "notify";
            case payDelete:
                return "delete";
            case payVendor:
                return "vendor";
            case payTraffInit:
                return "tsi";
            case payTraffResp:
                return "tsr";
            case payEncrypt:
                return "crypt";
            case payConfig:
                return "config";
            case payExtAuth:
                return "eap";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * parse one packet
     *
     * @return false on success, true on error
     */
    public boolean headerParse() {
        pckBin = pckDat.getCopy();
        if (pckDat.dataSize() < headSize) {
            return true;
        }
        cookieI = pckDat.msbGetQ(0);
        cookieR = pckDat.msbGetQ(8);
        pckNxt = pckDat.getByte(16);
        if (pckDat.getByte(17) != 0x20) {
            return true;
        }
        xchgTyp = pckDat.getByte(18);
        flags = pckDat.getByte(19);
        msgId = pckDat.msbGetD(20);
        pckSiz = pckDat.msbGetD(24) - headSize;
        pckDat.getSkip(headSize);
        if (pckSiz < 0) {
            return true;
        }
        if (pckSiz > pckDat.dataSize()) {
            return true;
        }
        pckDat.setDataSize(pckSiz);
        headerDump("rx");
        return false;
    }

    /**
     * create header
     */
    public void headerCreate() {
        pckDat.msbPutQ(0, cookieI);
        pckDat.msbPutQ(8, cookieR);
        pckDat.putByte(16, pckNxt);
        pckDat.putByte(17, 0x20); // version
        pckDat.putByte(18, xchgTyp);
        pckDat.putByte(19, flags);
        pckDat.msbPutD(20, msgId);
        pckDat.msbPutD(24, pckDat.dataSize() + headSize);
        pckDat.putSkip(headSize);
        pckDat.merge2beg();
        pckBin = pckDat.getCopy();
        headerDump("tx");
    }

    private void headerDump(String dir) {
        if (!debugger.secIkeTraf) {
            return;
        }
        logger.debug(dir + " " + exchange2string(xchgTyp) + " cookie=" + cookieI + "/" + cookieR + " msgid=" + msgId);
    }

    /**
     * find one payload
     *
     * @param needed type of payload
     * @return size of payload, negative if not found
     */
    protected int headerFind(int needed) {
        pckDat.setBytesLeft(pckSiz);
        int ths = pckNxt;
        for (;;) {
            if (pckDat.dataSize() < paySize) {
                return -1;
            }
            int nxt = pckDat.getByte(0);
            int siz = pckDat.msbGetW(2) - paySize;
            pckDat.getSkip(paySize);
            if (siz < 0) {
                return -1;
            }
            if (siz > pckDat.dataSize()) {
                return -1;
            }
            if (ths == needed) {
                return siz;
            }
            pckDat.getSkip(siz);
            ths = nxt;
        }
    }

    /**
     * write one payload
     *
     * @param typ type of payload
     */
    protected void headerWrite(int typ) {
        int siz = pckDat.headSize();
        pckDat.merge2beg();
        pckDat.putByte(0, pckNxt);
        pckDat.putByte(1, 0);
        pckDat.msbPutW(2, siz + paySize);
        pckDat.putSkip(paySize);
        pckDat.merge2beg();
        pckNxt = typ;
    }

    /**
     * read up header
     *
     * @param needed type of payload
     * @return bytes in header, null if not found
     */
    protected byte[] headerRead(int needed) {
        int i = headerFind(needed);
        if (i < 0) {
            return null;
        }
        byte[] buf = new byte[i];
        pckDat.getCopy(buf, 0, 0, i);
        return buf;
    }

    /**
     * read current header
     *
     * @return bytes
     */
    public byte[] headerCurr() {
        int siz = pckDat.dataSize();
        byte[] buf = new byte[siz - paySize];
        pckDat.getCopy(buf, 0, paySize, buf.length);
        return buf;
    }

    /**
     * check if empty
     *
     * @return true if yes, false if no
     */
    public boolean isEmpty() {
        return pckNxt == 0;
    }

    /**
     * check if reply
     *
     * @return true if yes, false if no
     */
    public boolean isReply() {
        return (flags & flagResp) != 0;
    }

    /**
     * set flags
     *
     * @param reply reply
     */
    public void setFlags(boolean reply) {
        flags = 0;
        if (initiator) {
            flags = flagInit;
        } else {
            flags = 0;
        }
        if (reply) {
            flags |= flagResp;
        }
    }

    /**
     * fill up security association
     *
     * @param ike true for ike sa, false of ipsec sa
     */
    public void secAssFill(boolean ike) {
        transform = transform.copyBytes();
        proposNum = 1;
        if (ike) {
            transform.prfAlg = transform.hashAlg;
            protoId = 1;
        } else {
            transform.prfAlg = 0;
            transform.groupNum = 0;
            transform.encapMet = 1;
            protoId = 3;
        }
    }

    /**
     * parse security association header
     *
     * @param ike true for ike sa, false of ipsec sa
     * @return false on success, true on error
     */
    public boolean secAssParse(boolean ike) {
        if (headerFind(paySecAss) < 0) {
            return true;
        }
        pckDat.getSkip(paySize);
        proposNum = pckDat.getByte(0);
        protoId = pckDat.getByte(1);
        int spiSize = pckDat.getByte(2);
        int transNum = pckDat.getByte(3);
        pckDat.getSkip(4);
        if (spiSize == 4) {
            spiValI = pckDat.msbGetD(0);
        }
        pckDat.getSkip(spiSize);
        List<secTransform> trans = new ArrayList<secTransform>();
        for (int i = 0; i < transNum; i++) {
            secTransform t = new secTransform();
            if (t.parse2transform(pckDat)) {
                continue;
            }
            trans.add(t);
        }
        secTransform t;
        if (ike) {
            t = new secTransform();
            t.groupNum = transform.groupNum;
            if (secTransform.findMatching(trans, t) < 0) {
                return true;
            }
            t = new secTransform();
            t.prfAlg = transform.hashAlg;
            if (secTransform.findMatching(trans, t) < 0) {
                return true;
            }
        }
        t = new secTransform();
        t.encrAlg = transform.encrAlg;
        t.encrKey = transform.encrKey;
        if (secTransform.findMatching(trans, t) < 0) {
            return true;
        }
        t = new secTransform();
        t.hashAlg = transform.hashAlg;
        if (secTransform.findMatching(trans, t) < 0) {
            return true;
        }
        secAssDump("rx");
        return false;
    }

    /**
     * create security association header
     */
    public void secAssCreate() {
        int p = transform.create2transform(pckDat);
        int i = pckDat.headSize();
        int o = 0;
        if (spiValI != 0) {
            o = 4;
        }
        pckDat.merge2beg();
        pckDat.msbPutW(0, 0); // next payload
        pckDat.msbPutW(2, 8 + i + o); // size
        pckDat.putByte(4, proposNum);
        pckDat.putByte(5, protoId);
        pckDat.putByte(6, o); // spi size
        pckDat.putByte(7, p); // number of transforms
        pckDat.putSkip(8);
        if (spiValI != 0) {
            pckDat.msbPutD(0, spiValI);
            pckDat.putSkip(4);
        }
        pckDat.unMergeBytes(i);
        headerWrite(paySecAss);
        secAssDump("tx");
    }

    private void secAssDump(String dir) {
        if (!debugger.secIkeTraf) {
            return;
        }
        logger.debug(dir + " prop=" + proposNum + " prot=" + protoId + " spi=" + spiValI + " trans=" + transform);
    }

    /**
     * fill key exchange header
     */
    public void keyXchgFill() {
        if (diffie == null) {
            diffie = transform.getGroup();
        }
        diffie.clntXchg();
        xchgTyp = xchgIkeSa;
    }

    /**
     * parse key exchange header
     *
     * @return false on success, true on error
     */
    public boolean keyXchgParse() {
        byte[] buf = headerRead(payKeyEx);
        if (buf == null) {
            return true;
        }

        if (diffie == null) {
            diffie = transform.getGroup();
        }
        if (bits.msbGetW(buf, 0) != transform.groupNum) {
            return true;
        }
        diffie.servPub = cryKeyGeneric.buffer2bigInt(buf, 4, buf.length - 4);
        keyXchgDump("rx");
        return false;
    }

    /**
     * create key exchange header
     */
    public void keyXchgCreate() {
        pckDat.msbPutW(0, transform.groupNum);
        pckDat.msbPutW(2, 0);
        pckDat.putSkip(4);
        byte[] buf = cryUtils.bigUint2buf(diffie.clntPub);
        pckDat.putCopy(buf, 0, 0, buf.length);
        pckDat.putSkip(buf.length);
        headerWrite(payKeyEx);
        keyXchgDump("tx");
    }

    private void keyXchgDump(String dir) {
        if (!debugger.secIkeTraf) {
            return;
        }
        logger.debug(dir + " kex");
    }

    /**
     * generate one nonce
     *
     * @return nonce data
     */
    public static byte[] nonceFill() {
        byte[] buf = new byte[20];
        for (int i = 0; i < buf.length; i++) {
            buf[i] = (byte) bits.randomB();
        }
        return buf;
    }

    /**
     * parse nonce header
     *
     * @return false on success, true on error
     */
    public boolean nonceParse() {
        nonceI = headerRead(payNonce);
        if (nonceI == null) {
            return true;
        }
        nonceDump("rx");
        return false;
    }

    /**
     * create nonce header
     */
    public void nonceCreate() {
        pckDat.putCopy(nonceI, 0, 0, nonceI.length);
        pckDat.putSkip(nonceI.length);
        headerWrite(payNonce);
        nonceDump("tx");
    }

    private void nonceDump(String dir) {
        if (!debugger.secIkeTraf) {
            return;
        }
        logger.debug(dir + " nonce=" + bits.byteDump(nonceI, 0, -1));
    }

    private byte[] doPrng(byte[] k, byte[] s, int len) {
        byte[] res = new byte[0];
        int cnt = 0;
        byte[] prev = new byte[0];
        for (;;) {
            cnt++;
            if (res.length > len) {
                break;
            }
            cryHashGeneric h = transform.getHmac(k);
            h.update(prev);
            h.update(s);
            prev = new byte[1];
            prev[0] = (byte) (cnt & 0xff);
            h.update(prev);
            prev = h.finish();
            res = bits.byteConcat(res, prev);
        }
        return res;
    }

    /**
     * compute keys
     */
    public void computeKeys() {
        diffie.clntKey();
        dhcomm = cryUtils.bigUint2buf(diffie.common);
        cryHashGeneric h = transform.getHmac(bits.byteConcat(nonceI, nonceR));
        h.update(dhcomm);
        skeyidG = h.finish();
        byte[] buf = new byte[16];
        bits.msbPutQ(buf, 0, cookieI);
        bits.msbPutQ(buf, 8, cookieR);
        buf = bits.byteConcat(bits.byteConcat(nonceI, nonceR), buf);
        buf = doPrng(skeyidG, buf, 1024);
        int p = 0;
        skeyidD = new byte[skeyidG.length];
        bits.byteCopy(buf, p, skeyidD, 0, skeyidD.length);
        p += skeyidD.length;
        skeyidA = new byte[skeyidG.length * 2];
        bits.byteCopy(buf, p, skeyidA, 0, skeyidA.length);
        p += skeyidA.length;
        skeyidE = new byte[transform.getKeyS() * 2];
        bits.byteCopy(buf, p, skeyidE, 0, skeyidE.length);
        p += skeyidE.length;
        skeyidP = new byte[skeyidG.length * 2];
        bits.byteCopy(buf, p, skeyidP, 0, skeyidP.length);
        p += skeyidP.length;
        if (debugger.secIkeTraf) {
            logger.debug("dh=" + bits.byteDump(dhcomm, 0, -1) + " skeyG=" + bits.byteDump(skeyidG, 0, -1) + " skeyD=" + bits.byteDump(skeyidD, 0, -1) + " skeyA=" + bits.byteDump(skeyidA, 0, -1) + " skeyE=" + bits.byteDump(skeyidE, 0, -1) + " skeyP=" + bits.byteDump(skeyidP, 0, -1));
        }
    }

    private byte[] getPart(byte[] buf, boolean encrtypt, boolean forced) {
        byte[] res = new byte[buf.length / 2];
        if (forced) {
            forced = encrtypt;
        } else {
            forced = initiator ^ encrtypt;
        }
        if (forced) {
            bits.byteCopy(buf, res.length, res, 0, res.length);
        } else {
            bits.byteCopy(buf, 0, res, 0, res.length);
        }
        return res;
    }

    /**
     * parse encrypted header
     *
     * @return false on success, true on error
     */
    public boolean encryptParse() {
        byte[] b1 = headerRead(payEncrypt);
        if (b1 == null) {
            return true;
        }
        pckNxt = pckDat.getByte(-4);
        int o = transform.getHashS();
        if (b1.length < o) {
            return true;
        }
        pckDat.setBytesLeft(pckSiz + headSize);
        byte[] b2 = new byte[pckDat.dataSize() - o];
        pckDat.getCopy(b2, 0, 0, b2.length);
        cryHashGeneric h = transform.getHmac(getPart(skeyidA, false, false));
        h.update(b2);
        pckDat.setBytesLeft(o);
        b2 = h.finish();
        if (bits.byteComp(b2, 0, pckDat.getCopy(), 0, o) != 0) {
            return true;
        }
        pckSiz = b1.length - o;
        cryEncrGeneric e = transform.getEncr();
        e.init(getPart(skeyidE, false, false), new byte[e.getBlockSize()], false);
        e.update(b1, 0, pckSiz);
        pckSiz--;
        pckSiz -= b1[pckSiz];
        o = e.getBlockSize();
        pckSiz -= o;
        pckDat.setDataSize(0);
        pckDat.putCopy(b1, o, 0, pckSiz);
        pckDat.putSkip(pckSiz);
        pckDat.merge2end();
        encryptDump("rx");
        return false;
    }

    /**
     * create encrypted header
     */
    public void encryptCreate() {
        encryptDump("tx");
        cryEncrGeneric e = transform.getEncr();
        int o = e.getBlockSize();
        e.init(getPart(skeyidE, true, false), new byte[e.getBlockSize()], true);
        for (int i = 0; i < o; i++) {
            pckDat.putByte(i, bits.randomB());
        }
        pckDat.putSkip(o);
        pckDat.merge2beg();
        int i = o - ((pckDat.dataSize() + 1) % o);
        pckDat.putFill(0, i, 0);
        pckDat.putByte(i, i);
        pckDat.putSkip(i + 1);
        pckDat.merge2end();
        byte[] b1 = pckDat.getCopy();
        e.update(b1, 0, b1.length);
        pckDat.setDataSize(0);
        pckDat.putCopy(b1, 0, 0, b1.length);
        pckDat.putSkip(b1.length);
        o = transform.getHashS();
        pckDat.putSkip(o);
        headerWrite(payEncrypt);
        headerCreate();
        pckDat.setDataSize(pckDat.dataSize() - o);
        cryHashGeneric h = transform.getHmac(getPart(skeyidA, true, false));
        h.update(pckDat.getCopy());
        b1 = h.finish();
        pckDat.getSkip(headSize);
        pckDat.putCopy(b1, 0, 0, o);
        pckDat.putSkip(o);
        pckDat.merge2end();
    }

    private void encryptDump(String dir) {
        if (!debugger.secIkeTraf) {
            return;
        }
        logger.debug(dir + " encrypt=(" + pckNxt + ") " + pckDat.dump());
    }

    /**
     * fill id header
     *
     * @param adr address to add
     */
    public void identFill(addrIP adr) {
        ident = adr.copyBytes();
    }

    /**
     * create id header
     */
    public void identCreate() {
        pckDat.msbPutD(0, 0);
        if (ident.isIPv4()) {
            pckDat.putByte(0, 1);
            pckDat.putAddr(4, ident.toIPv4());
            pckDat.putSkip(addrIPv4.size);
        } else {
            pckDat.putByte(0, 5);
            pckDat.putAddr(4, ident.toIPv6());
            pckDat.putSkip(addrIPv6.size);
        }
        pckDat.putSkip(4);
        if (initiator) {
            headerWrite(payIdntInit);
        } else {
            headerWrite(payIdntRspn);
        }
        identDump("tx");
    }

    /**
     * parse id header
     *
     * @return false on success, true on error
     */
    public boolean identParse() {
        if (initiator) {
            int i = headerFind(payIdntRspn);
            if (i < 0) {
                return true;
            }
            idnR = new byte[i];
            pckDat.getCopy(idnR, 0, 0, idnR.length);
        } else {
            int i = headerFind(payIdntInit);
            if (i < 0) {
                return true;
            }
            idnI = new byte[i];
            pckDat.getCopy(idnI, 0, 0, idnI.length);
        }
        ident = new addrIP();
        switch (pckDat.getByte(0)) {
            case 1:
                addrIPv4 adr4 = new addrIPv4();
                pckDat.getAddr(adr4, 4);
                ident.fromIPv4addr(adr4);
                break;
            case 5:
                addrIPv6 adr6 = new addrIPv6();
                pckDat.getAddr(adr6, 4);
                ident.fromIPv6addr(adr6);
                break;
            default:
                return true;
        }
        identDump("rx");
        return false;
    }

    private void identDump(String dir) {
        if (!debugger.secIkeTraf) {
            return;
        }
        logger.debug(dir + " ident=" + ident);
    }

    private int proto2trafsel(boolean ipv6) {
        if (ipv6) {
            return 8;
        } else {
            return 7;
        }
    }

    /**
     * parse traffic selector header
     *
     * @param init initiator side
     * @param ipv6 protected protocol
     * @return false on success, true on error
     */
    public boolean trafselParse(boolean init, boolean ipv6) {
        if (init) {
            if (headerFind(payTraffInit) < 0) {
                return true;
            }
        } else {
            if (headerFind(payTraffResp) < 0) {
                return true;
            }
        }
        if (pckDat.getByte(0) < 1) {
            return true;
        }
        pckDat.getSkip(4);
        if (pckDat.getByte(0) != proto2trafsel(ipv6)) {
            return true;
        }
        if (pckDat.getByte(1) != 0) {
            return true;
        }
        pckDat.getSkip(4);
        if (pckDat.msbGetW(0) != 0) {
            return true;
        }
        if (pckDat.msbGetW(2) != 0xffff) {
            return true;
        }
        pckDat.getSkip(4);
        ident = new addrIP();
        if (ipv6) {
            addrIPv6 adr6 = new addrIPv6();
            pckDat.getAddr(adr6, 0);
            pckDat.getAddr(adr6, addrIPv6.size);
            pckDat.getSkip(addrIPv6.size * 2);
            ident.fromIPv6addr(adr6);
        } else {
            addrIPv4 adr4 = new addrIPv4();
            pckDat.getAddr(adr4, 0);
            pckDat.getAddr(adr4, addrIPv4.size);
            pckDat.getSkip(addrIPv4.size * 2);
            ident.fromIPv4addr(adr4);
        }
        trafselDump("rx");
        return false;
    }

    /**
     * create traffic selector header
     *
     * @param init initiator side
     * @param ipv6 protected protocol
     */
    public void trafselCreate(boolean init, boolean ipv6) {
        pckDat.msbPutD(0, 0);
        pckDat.putByte(0, 1);
        pckDat.putByte(4, proto2trafsel(ipv6));
        pckDat.putByte(5, 0);
        pckDat.msbPutW(6, 0);
        pckDat.msbPutW(8, 0);
        pckDat.msbPutW(10, 0xffff);
        int p = 12;
        ident = new addrIP();
        if (ipv6) {
            addrIPv6 adr6 = new addrIPv6();
            pckDat.putAddr(p, adr6);
            p += addrIPv6.size;
            adr6.setNot(adr6);
            pckDat.putAddr(p, adr6);
            p += addrIPv6.size;
            ident.fromIPv6addr(adr6);
        } else {
            addrIPv4 adr4 = new addrIPv4();
            pckDat.putAddr(p, adr4);
            p += addrIPv4.size;
            adr4.setNot(adr4);
            pckDat.putAddr(p, adr4);
            p += addrIPv4.size;
            ident.fromIPv4addr(adr4);
        }
        pckDat.msbPutW(6, p - 4);
        pckDat.putSkip(p);
        if (init) {
            headerWrite(payTraffInit);
        } else {
            headerWrite(payTraffResp);
        }
        xchgTyp = xchgChild;
        trafselDump("tx");
    }

    private void trafselDump(String dir) {
        if (!debugger.secIkeTraf) {
            return;
        }
        logger.debug(dir + " trafsel=" + ident);
    }

    /**
     * parse authentication header
     *
     * @return false on success, true on error
     */
    public boolean authenParse() {
        byte[] b1 = headerRead(payAuthen);
        if (b1 == null) {
            return true;
        }
        if (b1[0] != 2) {
            return true;
        }
        byte[] b2 = getAuthen(!initiator);
        if ((b1.length - 4) != b2.length) {
            return true;
        }
        return bits.byteComp(b1, 4, b2, 0, b2.length) != 0;
    }

    /**
     * create authentication header
     */
    public void authenCreate() {
        pckDat.msbPutD(0, 0);
        pckDat.putByte(0, 2);
        pckDat.putSkip(4);
        byte[] buf = getAuthen(initiator);
        pckDat.putCopy(buf, 0, 0, buf.length);
        pckDat.putSkip(buf.length);
        headerWrite(payAuthen);
        xchgTyp = xchgAuth;
    }

    private byte[] getAuthen(boolean init) {
        byte[] msg;
        byte[] non;
        byte[] idn;
        byte[] skp;
        if (init) {
            msg = msgI;
            non = nonceR;
            idn = idnI;
            skp = getPart(skeyidP, false, true);
        } else {
            msg = msgR;
            non = nonceI;
            idn = idnR;
            skp = getPart(skeyidP, true, true);
        }
        cryHashGeneric h = transform.getHmac(skp);
        h.update(idn);
        skp = h.finish();
        h = transform.getHmac(preshared.getBytes());
        h.update("Key Pad for IKEv2".getBytes());
        h = transform.getHmac(h.finish());
        h.update(msg);
        h.update(non);
        h.update(skp);
        return h.finish();
    }

    /**
     * parse delete header
     *
     * @return false on success, true on error
     */
    public boolean deleteParse() {
        if (headerFind(payDelete) < 0) {
            return true;
        }
        if (pckDat.getByte(0) != 3) {
            return true;
        }
        if (pckDat.getByte(1) != 4) {
            return true;
        }
        if (pckDat.msbGetW(2) < 1) {
            return true;
        }
        spiValI = pckDat.msbGetD(4);
        deleteDump("rx");
        return false;
    }

    /**
     * create delete header
     */
    public void deleteCreate() {
        pckDat.putByte(0, 3);
        pckDat.putByte(1, 4);
        pckDat.msbPutW(2, 1);
        pckDat.msbPutD(4, spiValI);
        pckDat.putSkip(8);
        headerWrite(payDelete);
        xchgTyp = xchgInfo;
        deleteDump("tx");
    }

    private void deleteDump(String dir) {
        if (!debugger.secIkeTraf) {
            return;
        }
        logger.debug(dir + " delete spi=" + spiValI);
    }

    /**
     * update esp handler
     *
     * @param esp esp handler to update
     * @param spi spi value
     * @param encr true for encryption, false for decryption
     * @param init initiator side
     */
    public void updateEsp(packEsp esp, int spi, boolean encr, boolean init) {
        byte[] buf = doPrng(skeyidD, bits.byteConcat(nonceI, nonceR), 256);
        if (debugger.secIkeTraf) {
            logger.debug("spi=" + spi + " key=" + bits.byteDump(buf, 0, -1));
        }
        esp.spi = spi;
        cryEncrGeneric ciph = transform.getEncr();
        esp.hasher = transform.getHash();
        esp.encrSize = ciph.getBlockSize();
        esp.hashSize = transform.getHashS();
        int i = transform.getKeyS();
        int p;
        if (init) {
            p = i + esp.hasher.getHashSize();
        } else {
            p = 0;
        }
        byte[] last = new byte[i];
        bits.byteCopy(buf, p, last, 0, last.length);
        ciph.init(last, new byte[ciph.getBlockSize()], encr);
        last = new byte[esp.hasher.getHashSize()];
        bits.byteCopy(buf, p + i, last, 0, last.length);
        esp.hasher = transform.getHmac(last);
        esp.cipher = ciph;
        esp.doInit();
    }

}
