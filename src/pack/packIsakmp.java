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
import prt.prtUdp;
import sec.secTransform;
import util.bits;
import util.debugger;
import util.logger;

/**
 * internet security association and key management protocol (rfc2408) packet
 *
 * @author matecsaba
 */
public class packIsakmp {

    /**
     * packet holder
     */
    public packHolder pckDat;

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
     * domain of interpretation
     */
    public int domOfInt;

    /**
     * situation
     */
    public int situation;

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
     * phase1 initial iv
     */
    public byte[] phase1iv1;

    /**
     * phase1 next iv
     */
    public byte[] phase1iv2;

    /**
     * phase2 next iv
     */
    public byte[] phase2iv;

    /**
     * phase2 message id
     */
    public int phase2id;

    /**
     * identification data
     */
    public addrIP ident;

    /**
     * secass for hash
     */
    public byte[] hash2sai;

    /**
     * id for hash
     */
    public byte[] hash2idi;

    /**
     * hash data
     */
    public byte[] hash2got;

    /**
     * notify type
     */
    public int notifyTyp;

    /**
     * notify data
     */
    public int notifyDat;

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
    public final static int paySecAss = 1;

    /**
     * proposal
     */
    public final static int payProp = 2;

    /**
     * transform
     */
    public final static int payTrns = 3;

    /**
     * key exchange
     */
    public final static int payKeyEx = 4;

    /**
     * identification
     */
    public final static int payIdnt = 5;

    /**
     * certificate data
     */
    public final static int payCrtDat = 6;

    /**
     * certificate request
     */
    public final static int payCrtReq = 7;

    /**
     * hash
     */
    public final static int payHash = 8;

    /**
     * signature
     */
    public final static int paySign = 9;

    /**
     * nonce
     */
    public final static int payNonce = 10;

    /**
     * notification
     */
    public final static int payNotif = 11;

    /**
     * delete
     */
    public final static int payDel = 12;

    /**
     * vendor id
     */
    public final static int payVend = 13;

    /**
     * none
     */
    public final static int xchgNone = 0;

    /**
     * base
     */
    public final static int xchgBase = 1;

    /**
     * identity protection
     */
    public final static int xchgIdPrt = 2;

    /**
     * authentication only
     */
    public final static int xchgAuth = 3;

    /**
     * aggressive
     */
    public final static int xchgAggr = 4;

    /**
     * informational
     */
    public final static int xchgInfo = 5;

    /**
     * quick mode
     */
    public final static int xchgQuick = 32;

    /**
     * encryption
     */
    public final static int flagEnc = 1;

    /**
     * commit
     */
    public final static int flagCom = 2;

    /**
     * authentication
     */
    public final static int flagAuth = 4;

    /**
     * are you there question
     */
    public final static int notfDedPerReq = 36136;

    /**
     * are you there answer
     */
    public final static int notfDedPerRep = 36137;

    /**
     * create new packet holder
     */
    public packIsakmp() {
        pckDat = new packHolder(true, true);
    }

    /**
     * copy bytes of packet
     *
     * @return new instance
     */
    public packIsakmp copyBytes() {
        packIsakmp n = new packIsakmp();
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
        n.msgId = msgId;
        n.phase1iv1 = phase1iv1;
        n.phase1iv2 = phase1iv2;
        n.phase2iv = phase2iv;
        n.phase2id = phase2id;
        n.hash2idi = hash2idi;
        n.hash2sai = hash2sai;
        n.hash2got = hash2got;
        n.spiValI = spiValI;
        n.spiValR = spiValR;
        return n;
    }

    /**
     * check if this is my peer
     *
     * @param c to check
     * @return true if yes, false if no
     */
    public boolean checkPeer(packIsakmp c) {
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
            case payProp:
                return "prop";
            case payTrns:
                return "trans";
            case payKeyEx:
                return "kex";
            case payIdnt:
                return "id";
            case payCrtDat:
                return "cert";
            case payCrtReq:
                return "certReq";
            case payHash:
                return "hash";
            case paySign:
                return "sign";
            case payNonce:
                return "nonce";
            case payNotif:
                return "notify";
            case payDel:
                return "delete";
            case payVend:
                return "vendor";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * convert exchange type to string
     *
     * @param i exchange type
     * @return string
     */
    public static String exchange2string(int i) {
        switch (i) {
            case xchgNone:
                return "none";
            case xchgBase:
                return "base";
            case xchgIdPrt:
                return "main";
            case xchgAuth:
                return "authen";
            case xchgAggr:
                return "aggressive";
            case xchgInfo:
                return "info";
            case xchgQuick:
                return "quick";
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
        if (pckDat.dataSize() < headSize) {
            return true;
        }
        cookieI = pckDat.msbGetQ(0);
        cookieR = pckDat.msbGetQ(8);
        pckNxt = pckDat.getByte(16);
        if (pckDat.getByte(17) != 0x10) {
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
        pckDat.putByte(17, 0x10); // version
        pckDat.putByte(18, xchgTyp);
        pckDat.putByte(19, flags);
        pckDat.msbPutD(20, msgId);
        pckDat.msbPutD(24, pckDat.dataSize() + headSize);
        pckDat.putSkip(headSize);
        pckDat.merge2beg();
        headerDump("tx");
    }

    private void headerDump(String dir) {
        if (!debugger.secIkeTraf) {
            return;
        }
        logger.debug(dir + " " + exchange2string(xchgTyp) + " cookie=" + cookieI + "/" + cookieR + " msgid=" + msgId);
    }

    /**
     * decrypt the current packet
     *
     * @return false on success, true on error
     */
    public boolean doDecrypt() {
        if ((flags & flagEnc) == 0) {
            return true;
        }
        if (skeyidE == null) {
            return true;
        }
        byte[] currIV = getCurrIV();
        cryEncrGeneric e = transform.getEncr();
        e.init(skeyidE, currIV, false);
        pckDat.setBytesLeft(pckSiz);
        byte[] res = e.compute(pckDat.getCopy());
        pckDat.getCopy(currIV, 0, pckSiz - currIV.length, currIV.length);
        pckDat.getSkip(pckDat.dataSize());
        pckDat.putCopy(res, 0, 0, res.length);
        pckDat.putSkip(res.length);
        pckDat.merge2end();
        putCurrIV(currIV);
        pckDat.setBytesLeft(pckSiz);
        int nxt = pckNxt;
        int ofs = 0;
        for (;;) {
            if (nxt == 0) {
                break;
            }
            nxt = pckDat.getByte(ofs + 0);
            ofs += pckDat.msbGetW(ofs + 2);
            if (ofs > pckSiz) {
                return true;
            }
        }
        pckSiz = ofs;
        pckDat.setDataSize(ofs);
        return false;
    }

    /**
     * encrypt the current packet
     */
    public void doEncrypt() {
        byte[] currIV = getCurrIV();
        cryEncrGeneric e = transform.getEncr();
        e.init(skeyidE, currIV, true);
        int i = e.getBlockSize() - (pckDat.dataSize() % e.getBlockSize());
        pckDat.putFill(0, i, 0);
        pckDat.putSkip(i);
        pckDat.merge2end();
        byte[] res = e.compute(pckDat.getCopy());
        pckDat.getSkip(pckDat.dataSize());
        pckDat.putCopy(res, 0, 0, res.length);
        pckDat.putSkip(res.length);
        pckDat.merge2end();
        pckDat.getCopy(currIV, 0, pckDat.dataSize() - currIV.length, currIV.length);
        flags |= flagEnc;
        putCurrIV(currIV);
    }

    private byte[] getCurrIV() {
        if (msgId == 0) {
            return phase1iv2;
        }
        if (msgId == phase2id) {
            return phase2iv;
        }
        cryHashGeneric h = transform.getHash();
        h.update(phase1iv2);
        byte[] buf = new byte[4];
        bits.msbPutD(buf, 0, msgId);
        h.update(buf);
        return padSkey(h.finish(), transform.getEncr().getBlockSize());
    }

    private void putCurrIV(byte[] buf) {
        if (msgId == 0) {
            phase1iv2 = buf;
            return;
        }
        phase2id = msgId;
        phase2iv = buf;
    }

    /**
     * find one payload
     *
     * @param needed type of payload
     * @param occur number of occurence to get
     * @return size of payload, negative if not found
     */
    protected int headerFind(int needed, int occur) {
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
                occur--;
                if (occur < 1) {
                    return siz;
                }
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
     * @param occur number of occurence to get
     * @return bytes in header, null if not found
     */
    protected byte[] headerRead(int needed, int occur) {
        int i = headerFind(needed, occur);
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
    protected byte[] headerCurr() {
        int siz = pckDat.dataSize();
        byte[] buf = new byte[siz - paySize];
        pckDat.getCopy(buf, 0, paySize, buf.length);
        return buf;
    }

    /**
     * fill up security association
     */
    public void QMsecAssFill() {
        transform.transId = 1;
        transform.encapMet = 1;
        domOfInt = 1;
        situation = 1;
        protoId = 3;
        xchgTyp = xchgQuick;
    }

    /**
     * fill up security association
     */
    public void MMsecAssFill() {
        transform.transId = 1;
        transform = transform.copyBytes();
        transform.lifeByt = 0;
        domOfInt = 1;
        situation = 1;
        protoId = 1;
        spiValI = 0;
        spiValR = 0;
        xchgTyp = xchgIdPrt;
    }

    /**
     * read security association
     *
     * @param current set true to read current one
     * @return bytes read
     */
    public byte[] secAssRead(boolean current) {
        if (current) {
            return headerCurr();
        }
        return headerRead(paySecAss, 1);
    }

    /**
     * parse security association header
     *
     * @param mm main mode transform
     * @return false on success, true on error
     */
    public boolean secAssParse(boolean mm) {
        if (headerFind(paySecAss, 1) < 0) {
            return true;
        }
        domOfInt = pckDat.msbGetD(0);
        situation = pckDat.msbGetD(4);
        pckDat.getSkip(8);
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
            boolean b;
            if (mm) {
                b = t.parseMMtransform(pckDat);
            } else {
                b = t.parseQMtransform(pckDat);
                t.groupNum = transform.groupNum;
                t.authAlg = transform.authAlg;
            }
            if (b) {
                continue;
            }
            trans.add(t);
        }
        int i = secTransform.findMatching(trans, transform);
        if (i < 0) {
            return true;
        }
        secAssDump("rx");
        return false;
    }

    /**
     * create security association header
     *
     * @param mm main mode transform
     */
    public void secAssCreate(boolean mm) {
        if (mm) {
            transform.createMMtransform(pckDat, false);
        } else {
            transform.createQMtransform(pckDat, false);
        }
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
        pckDat.putByte(7, 1); // number of transforms
        pckDat.putSkip(8);
        if (spiValI != 0) {
            pckDat.msbPutD(0, spiValI);
            pckDat.putSkip(4);
        }
        pckDat.unMergeBytes(i);
        i = pckDat.headSize();
        pckDat.merge2beg();
        pckDat.msbPutD(0, domOfInt);
        pckDat.msbPutD(4, situation);
        pckDat.putSkip(8);
        pckDat.unMergeBytes(i);
        headerWrite(paySecAss);
        secAssDump("tx");
    }

    private void secAssDump(String dir) {
        if (!debugger.secIkeTraf) {
            return;
        }
        logger.debug(dir + " doi=" + domOfInt + " situ=" + situation + " prop=" + proposNum + " prot=" + protoId + " spi=" + spiValI + " trans=" + transform);
    }

    /**
     * fill key exchange header
     */
    public void keyXchgFill() {
        if (diffie == null) {
            diffie = transform.getGroup();
        }
        diffie.clntXchg();
        xchgTyp = xchgIdPrt;
    }

    /**
     * parse key exchange header
     *
     * @return false on success, true on error
     */
    public boolean keyXchgParse() {
        byte[] buf = headerRead(payKeyEx, 1);
        if (buf == null) {
            return true;
        }
        if (diffie == null) {
            diffie = transform.getGroup();
        }
        diffie.servPub = cryKeyGeneric.buffer2bigInt(buf, 0, buf.length);
        keyXchgDump("rx");
        return false;
    }

    /**
     * create key exchange header
     */
    public void keyXchgCreate() {
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
        nonceI = headerRead(payNonce, 1);
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

    private byte[] makeSkeyX(byte[] buf, int num) {
        cryHashGeneric h = transform.getHmac(skeyidG);
        h.update(buf);
        h.update(dhcomm);
        buf = new byte[8];
        bits.msbPutQ(buf, 0, cookieI);
        h.update(buf);
        bits.msbPutQ(buf, 0, cookieR);
        h.update(buf);
        h.update(num);
        return h.finish();
    }

    private byte[] padSkey(byte[] orig, int min) {
        if (orig.length < min) {
            byte[] buf = new byte[0];
            byte[] last = new byte[1];
            last[0] = 0;
            for (; buf.length < min;) {
                cryHashGeneric h = transform.getHmac(orig);
                h.update(last);
                last = h.finish();
                buf = bits.byteConcat(buf, last);
            }
            orig = buf;
        }
        byte[] buf = new byte[min];
        bits.byteCopy(orig, 0, buf, 0, buf.length);
        return buf;
    }

    /**
     * compute keys
     */
    public void computeKeys() {
        diffie.clntKey();
        dhcomm = cryUtils.bigUint2buf(diffie.common);
        cryHashGeneric h = transform.getHmac(preshared.getBytes());
        h.update(nonceI);
        h.update(nonceR);
        skeyidG = h.finish();
        h = transform.getHash();
        if (initiator) {
            h.update(cryUtils.bigUint2buf(diffie.clntPub));
            h.update(cryUtils.bigUint2buf(diffie.servPub));
        } else {
            h.update(cryUtils.bigUint2buf(diffie.servPub));
            h.update(cryUtils.bigUint2buf(diffie.clntPub));
        }
        phase1iv1 = h.finish();
        skeyidD = makeSkeyX(new byte[0], 0);
        skeyidA = makeSkeyX(skeyidD, 1);
        skeyidE = makeSkeyX(skeyidA, 2);
        int i = transform.getKeyS();
        skeyidE = padSkey(skeyidE, i);
        phase1iv1 = padSkey(phase1iv1, transform.getEncr().getBlockSize());
        phase1iv2 = phase1iv1;
        if (debugger.secIkeTraf) {
            logger.debug("dh=" + bits.byteDump(dhcomm, 0, -1) + " skeyG=" + bits.byteDump(skeyidG, 0, -1) + " skeyD="
                    + bits.byteDump(skeyidD, 0, -1) + " skeyA=" + bits.byteDump(skeyidA, 0, -1) + " skeyE="
                    + bits.byteDump(skeyidE, 0, -1) + " iv=" + bits.byteDump(phase1iv1, 0, -1));
        }
    }

    /**
     * fill id header
     *
     * @param adr address to add
     */
    public void MMidentFill(addrIP adr) {
        ident = adr.copyBytes();
    }

    /**
     * fill id header
     *
     * @param ipv6 ipv6 mode
     */
    public void QMidentFill(boolean ipv6) {
        ident = new addrIP();
        if (ipv6) {
            ident.fromIPv6addr(new addrIPv6());
        } else {
            ident.fromIPv4addr(new addrIPv4());
        }
    }

    /**
     * create id header
     */
    public void MMidentCreate() {
        pckDat.putByte(1, prtUdp.protoNum);
        pckDat.msbPutW(2, port);
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
        headerWrite(payIdnt);
        identDump("tx");
        xchgTyp = xchgIdPrt;
    }

    /**
     * read id header
     *
     * @param current set true to read current one
     * @return bytes read
     */
    public byte[] identRead(int current) {
        if (current < 1) {
            return headerCurr();
        }
        return headerRead(payIdnt, current);
    }

    /**
     * parse id header
     *
     * @return false on success, true on error
     */
    public boolean MMidentParse() {
        if (headerFind(payIdnt, 1) < 0) {
            return true;
        }
        if (pckDat.getByte(1) != prtUdp.protoNum) {
            return true;
        }
        if (pckDat.msbGetW(2) != port) {
            return true;
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

    /**
     * generate hash
     *
     * @param initer true for hashi, fasle for hashr
     * @return bytes generated
     */
    public byte[] hashGenMM(boolean initer) {
        cryHashGeneric h = transform.getHmac(skeyidG);
        if (initiator ^ initer) {
            h.update(cryUtils.bigUint2buf(diffie.servPub));
            h.update(cryUtils.bigUint2buf(diffie.clntPub));
        } else {
            h.update(cryUtils.bigUint2buf(diffie.clntPub));
            h.update(cryUtils.bigUint2buf(diffie.servPub));
        }
        if (initer) {
            byte[] buf = new byte[8];
            bits.msbPutQ(buf, 0, cookieI);
            h.update(buf);
            bits.msbPutQ(buf, 0, cookieR);
            h.update(buf);
        } else {
            byte[] buf = new byte[8];
            bits.msbPutQ(buf, 0, cookieR);
            h.update(buf);
            bits.msbPutQ(buf, 0, cookieI);
            h.update(buf);
        }
        h.update(hash2sai);
        h.update(hash2idi);
        return h.finish();
    }

    /**
     * parse hash header
     *
     * @return false on success, true on error
     */
    public boolean hashParse() {
        hash2got = headerRead(payHash, 1);
        if (hash2got == null) {
            return true;
        }
        pckDat.getSkip(hash2got.length);
        hashDump("rx");
        return false;
    }

    /**
     * create hash header
     */
    public void hashCreate() {
        pckDat.putCopy(hash2got, 0, 0, hash2got.length);
        pckDat.putSkip(hash2got.length);
        headerWrite(payHash);
        hashDump("tx");
    }

    private void hashDump(String dir) {
        if (!debugger.secIkeTraf) {
            return;
        }
        logger.debug(dir + " hash=" + bits.byteDump(hash2got, 0, -1));
    }

    /**
     * verify hash
     *
     * @param calc calculated value
     * @return false on success, true on error
     */
    public boolean hashVerify(byte[] calc) {
        if (calc.length != hash2got.length) {
            return true;
        }
        return bits.byteComp(calc, 0, hash2got, 0, hash2got.length) != 0;
    }

    /**
     * parse id header
     *
     * @param num number of record
     * @return false on success, true on error
     */
    public boolean QMidentParse(int num) {
        if (headerFind(payIdnt, num) < 0) {
            return true;
        }
        if (pckDat.getByte(1) != 0) {
            return true;
        }
        if (pckDat.msbGetW(2) != 0) {
            return true;
        }
        ident = new addrIP();
        switch (pckDat.getByte(0)) {
            case 4:
                addrIPv4 adr4 = new addrIPv4();
                pckDat.getAddr(adr4, 4);
                if (adr4.compare(adr4, new addrIPv4()) != 0) {
                    return true;
                }
                pckDat.getAddr(adr4, 4 + addrIPv4.size);
                if (adr4.compare(adr4, new addrIPv4()) != 0) {
                    return true;
                }
                ident.fromIPv4addr(adr4);
                break;
            case 6:
                addrIPv6 adr6 = new addrIPv6();
                pckDat.getAddr(adr6, 4);
                if (adr6.compare(adr6, new addrIPv6()) != 0) {
                    return true;
                }
                pckDat.getAddr(adr6, 4 + addrIPv6.size);
                if (adr6.compare(adr6, new addrIPv6()) != 0) {
                    return true;
                }
                ident.fromIPv6addr(adr6);
                break;
            default:
                return true;
        }
        identDump("rx");
        return false;
    }

    /**
     * create id header
     */
    public void QMidentCreate() {
        pckDat.putByte(1, 0);
        pckDat.msbPutW(2, 0);
        if (ident.isIPv4()) {
            pckDat.putByte(0, 4);
            pckDat.putAddr(4, new addrIPv4());
            pckDat.putAddr(4 + addrIPv4.size, new addrIPv4());
            pckDat.putSkip(2 * addrIPv4.size);
        } else {
            pckDat.putByte(0, 6);
            pckDat.putAddr(4, new addrIPv6());
            pckDat.putAddr(4 + addrIPv6.size, new addrIPv6());
            pckDat.putSkip(2 * addrIPv6.size);
        }
        pckDat.putSkip(4);
        headerWrite(payIdnt);
        identDump("tx");
        xchgTyp = xchgQuick;
    }

    /**
     * generate quick mode hash 1
     *
     * @return generated hash
     */
    public byte[] hash1genQM() {
        xchgTyp = xchgQuick;
        cryHashGeneric h = transform.getHmac(skeyidA);
        byte[] buf = new byte[4];
        bits.msbPutD(buf, 0, msgId);
        h.update(buf);
        h.update(pckDat.getCopy());
        return h.finish();
    }

    /**
     * generate quick mode hash 2
     *
     * @return generated hash
     */
    public byte[] hash2genQM() {
        xchgTyp = xchgQuick;
        cryHashGeneric h = transform.getHmac(skeyidA);
        byte[] buf = new byte[4];
        bits.msbPutD(buf, 0, msgId);
        h.update(buf);
        h.update(nonceR);
        h.update(pckDat.getCopy());
        return h.finish();
    }

    /**
     * generate quick mode hash 3
     *
     * @return generated hash
     */
    public byte[] hash3genQM() {
        xchgTyp = xchgQuick;
        cryHashGeneric h = transform.getHmac(skeyidA);
        byte[] buf = new byte[4];
        bits.msbPutD(buf, 0, phase2id);
        h.update(0);
        h.update(buf);
        h.update(nonceI);
        h.update(nonceR);
        return h.finish();
    }

    /**
     * update esp handler
     *
     * @param esp esp handler to update
     * @param spi spi value
     * @param encr true for encryption, false for decryption
     */
    public void updateEsp(packEsp esp, int spi, boolean encr) {
        byte[] buf = new byte[0];
        byte[] last = new byte[0];
        for (; buf.length < 256;) {
            cryHashGeneric h = transform.getHmac(skeyidD);
            h.update(last);
            h.update(3);
            last = new byte[4];
            bits.msbPutD(last, 0, spi);
            h.update(last);
            h.update(nonceI);
            h.update(nonceR);
            last = h.finish();
            buf = bits.byteConcat(buf, last);
        }
        if (debugger.secIkeTraf) {
            logger.debug("spi=" + spi + " key=" + bits.byteDump(buf, 0, -1));
        }
        esp.spi = spi;
        cryEncrGeneric ciph = transform.getEncr();
        esp.hasher = transform.getHash();
        esp.encrSize = ciph.getBlockSize();
        esp.hashSize = transform.getHashS();
        int i = transform.getKeyS();
        last = new byte[i];
        bits.byteCopy(buf, 0, last, 0, last.length);
        ciph.init(last, new byte[ciph.getBlockSize()], encr);
        last = new byte[esp.hasher.getHashSize()];
        bits.byteCopy(buf, i, last, 0, last.length);
        esp.hasher = transform.getHmac(last);
        esp.cipher = ciph;
        esp.doInit();
    }

    /**
     * create dead peer detection capability header
     */
    public void dedPerCapCreate() {
        byte[] buf = new byte[16];
        bits.msbPutD(buf, 0, 0xafcad713);
        bits.msbPutD(buf, 4, 0x68a1f1c9);
        bits.msbPutD(buf, 8, 0x6b8696fc);
        bits.msbPutD(buf, 12, 0x77570100);
        pckDat.putCopy(buf, 0, 0, buf.length);
        pckDat.putSkip(buf.length);
        headerWrite(payVend);
    }

    /**
     * generate hash
     *
     * @return bytes generated
     */
    public byte[] hashGenIM() {
        cryHashGeneric h = transform.getHmac(skeyidA);
        byte[] buf = new byte[4];
        bits.msbPutD(buf, 0, msgId);
        h.update(buf);
        h.update(pckDat.getCopy());
        return h.finish();
    }

    /**
     * parse notification header
     *
     * @return false on success, true on error
     */
    public boolean notifyParse() {
        if (headerFind(payNotif, 1) < 0) {
            return true;
        }
        domOfInt = pckDat.msbGetD(0);
        protoId = pckDat.getByte(4);
        int spiSiz = pckDat.getByte(5);
        notifyTyp = pckDat.msbGetW(6);
        notifyDat = pckDat.msbGetD(8 + spiSiz);
        notifyDump("rx");
        return false;
    }

    /**
     * fill id header
     *
     * @param reply response
     */
    public void notifyFill(boolean reply) {
        xchgTyp = xchgInfo;
        if (reply) {
            notifyTyp = notfDedPerRep;
        } else {
            notifyTyp = notfDedPerReq;
        }
        domOfInt = 1;
        protoId = 1;
    }

    /**
     * create hash header
     */
    public void notifyCreate() {
        pckDat.msbPutD(0, domOfInt);
        pckDat.putByte(4, protoId);
        pckDat.putByte(5, 16); // spi size
        pckDat.msbPutW(6, notifyTyp);
        pckDat.msbPutQ(8, cookieI);
        pckDat.msbPutQ(16, cookieR);
        pckDat.msbPutD(24, notifyDat);
        pckDat.putSkip(28);
        headerWrite(payNotif);
        notifyDump("tx");
    }

    private void notifyDump(String dir) {
        if (!debugger.secIkeTraf) {
            return;
        }
        logger.debug(dir + " doi=" + domOfInt + " prot=" + protoId + " notify=" + notifyTyp + " data=" + notifyDat);
    }

    /**
     * parse delete header
     *
     * @return false on success, true on error
     */
    public boolean deleteParse() {
        if (headerFind(payDel, 1) < 0) {
            return true;
        }
        if (pckDat.msbGetD(0) != 1) {
            return true;
        }
        if (pckDat.getByte(4) != 3) {
            return true;
        }
        if (pckDat.getByte(5) != 4) {
            return true;
        }
        if (pckDat.msbGetW(6) < 1) {
            return true;
        }
        spiValI = pckDat.msbGetD(8);
        deleteDump("rx");
        return false;
    }

    /**
     * create delete header
     */
    public void deleteCreate() {
        pckDat.msbPutD(0, 1);
        pckDat.putByte(4, 3);
        pckDat.putByte(5, 4);
        pckDat.msbPutW(6, 1);
        pckDat.msbPutD(8, spiValI);
        pckDat.putSkip(12);
        headerWrite(payDel);
        xchgTyp = xchgInfo;
        deleteDump("tx");
    }

    private void deleteDump(String dir) {
        if (!debugger.secIkeTraf) {
            return;
        }
        logger.debug(dir + " delete spi=" + spiValI);
    }

}
