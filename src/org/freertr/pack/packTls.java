package org.freertr.pack;

import org.freertr.cry.cryEncrGeneric;
import org.freertr.cry.cryHashGeneric;
import org.freertr.pipe.pipeSide;
import org.freertr.util.bits;
import org.freertr.util.debugger;
import org.freertr.util.logger;

/**
 * transport layer security (rfc5246) packet
 *
 * @author matecsaba
 */
public class packTls {

    /**
     * current packet
     */
    public packHolder pckDat;

    /**
     * type of packet
     */
    public int pckTyp;

    /**
     * lower pipe
     */
    public pipeSide pipe;

    /**
     * padding modulo number
     */
    public int padModulo = -1;

    /**
     * sendingsequence number
     */
    public long seqTx = 0;

    /**
     * receiving sequence number
     */
    public long seqRx = 0;

    /**
     * sendingsequence number
     */
    public long aeadTx = 0;

    /**
     * receiving sequence number
     */
    public long aeadRx = 0;

    /**
     * datagram mode
     */
    public boolean datagram;

    /**
     * aead mode
     */
    public boolean aeadMode;

    /**
     * connection version
     */
    public int verCurr = -1;

    /**
     * minimum version
     */
    public int verMin = 0x300;

    /**
     * maximum version
     */
    public int verMax = 0x304;

    /**
     * sending encryption
     */
    public cryEncrGeneric encTx;

    /**
     * receiving encryption
     */
    public cryEncrGeneric encRx;

    /**
     * sending hash
     */
    public cryHashGeneric macTx;

    /**
     * receiving hash
     */
    public cryHashGeneric macRx;

    /**
     * sending key
     */
    public byte[] keyTx;

    /**
     * receiving key
     */
    public byte[] keyRx;

    /**
     * sending iv
     */
    public byte[] ivTx;

    /**
     * receiving iv
     */
    public byte[] ivRx;

    /**
     * change cipher spec
     */
    public final static int typeChgCipher = 20;

    /**
     * alert
     */
    public final static int typeAlert = 21;

    /**
     * handshake
     */
    public final static int typeHandshk = 22;

    /**
     * application data
     */
    public final static int typeAppDat = 23;

    /**
     * convert version to dtls
     *
     * @param i tls version
     * @return dtls version
     */
    public static int version2dtls(int i) {
        return (0x10100 - (i & 0xff00)) | (0x101 - (i & 0xff));
    }

    /**
     * convert version to wire
     *
     * @param i tls version
     * @return wire version
     */
    public static int version2wire(int i) {
        if (i > 0x303) {
            return 0x303;
        } else {
            return i;
        }
    }

    /**
     * convert type to string
     *
     * @param i type of message
     * @return string
     */
    public static String type2string(int i) {
        switch (i) {
            case typeChgCipher:
                return "changeCipherSpec";
            case typeAlert:
                return "alert";
            case typeHandshk:
                return "handshake";
            case typeAppDat:
                return "appData";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * convert version to string
     *
     * @param dtls datagram
     * @param ver version
     * @return string
     */
    public static String version2string(boolean dtls, int ver) {
        if (dtls) {
            switch (ver) {
                case 0x302:
                    return "dtls10";
                case 0x303:
                    return "dtls12";
                case 0x304:
                    return "dtls13";
                default:
                    return "unknown=" + ver;
            }
        }
        switch (ver) {
            case 0x300:
                return "ssl30";
            case 0x301:
                return "tls10";
            case 0x302:
                return "tls11";
            case 0x303:
                return "tls12";
            case 0x304:
                return "tls13";
            default:
                return "unknown=" + ver;
        }
    }

    /**
     * create packet holder
     *
     * @param dtls datagram mode
     */
    public packTls(boolean dtls) {
        pckDat = new packHolder(true, true);
        datagram = dtls;
        if (dtls) {
            verMin += 2;
        }
    }

    /**
     * clone packet holder
     *
     * @return new packet holder
     */
    public packTls copyBytes() {
        packTls p = new packTls(datagram);
        p.pipe = pipe;
        p.padModulo = padModulo;
        p.aeadMode = aeadMode;
        p.aeadTx = aeadTx;
        p.aeadRx = aeadRx;
        p.seqTx = seqTx;
        p.seqRx = seqRx;
        p.encTx = encTx;
        p.encRx = encRx;
        p.macTx = macTx;
        p.macRx = macRx;
        p.keyTx = keyTx;
        p.keyRx = keyRx;
        p.ivTx = ivTx;
        p.ivRx = ivRx;
        p.verCurr = verCurr;
        p.verMax = verMax;
        p.verMin = verMin;
        return p;
    }

    /**
     * get header size
     *
     * @return bytes
     */
    public int getHeadSize() {
        if (datagram) {
            return 13;
        } else {
            return 5;
        }
    }

    /**
     * put bytes to packet
     *
     * @param buf buffer to use
     * @param hed size of prepended length
     */
    public void putBytes(byte[] buf, int hed) {
        int siz;
        if (buf == null) {
            siz = 0;
        } else {
            siz = buf.length;
        }
        switch (hed) {
            case 1:
                pckDat.putByte(0, siz);
                break;
            case 2:
                pckDat.msbPutW(0, siz);
                break;
            case 3:
                pckDat.msbPutD(0, siz << 8);
                break;
            case 4:
                pckDat.msbPutD(0, siz);
                break;
        }
        pckDat.putSkip(hed);
        pckDat.putCopy(buf, 0, 0, siz);
        pckDat.putSkip(siz);
    }

    /**
     * get bytes from packet
     *
     * @param hed size of header, -x=forced size
     * @return bytes readed
     */
    public byte[] getBytes(int hed) {
        if (pckDat.dataSize() < hed) {
            return null;
        }
        int siz = 0;
        switch (hed) {
            case 1:
                siz = pckDat.getByte(0);
                break;
            case 2:
                siz = pckDat.msbGetW(0);
                break;
            case 3:
                siz = pckDat.msbGetD(0) >>> 8;
                break;
            case 4:
                siz = pckDat.msbGetD(0);
                break;
        }
        if (hed < 0) {
            siz = -hed;
        } else {
            pckDat.getSkip(hed);
        }
        if (pckDat.dataSize() < siz) {
            return null;
        }
        byte[] buf = new byte[siz];
        pckDat.getCopy(buf, 0, 0, siz);
        pckDat.getSkip(siz);
        return buf;
    }

    /**
     * enable encryption
     */
    public void encryptEna() {
        padModulo = encTx.getBlockSize();
    }

    /**
     * disable encryption
     */
    public void encryptDis() {
        padModulo = -1;
    }

    private byte[] calcSum(cryHashGeneric hash, long seq) {
        hash.init();
        byte[] buf = new byte[8];
        bits.msbPutQ(buf, 0, seq);
        hash.update(buf);
        hash.update(pckTyp);
        if (verCurr == 0x300) {
            buf = new byte[2];
            bits.msbPutW(buf, 0, pckDat.dataSize());
        } else {
            buf = new byte[4];
            int i = verCurr;
            if (datagram) {
                i = version2dtls(i);
            }
            bits.msbPutW(buf, 0, i);
            bits.msbPutW(buf, 2, pckDat.dataSize());
        }
        hash.update(buf);
        pckDat.hashData(hash, 0, pckDat.dataSize());
        return hash.finish();
    }

    /**
     * send current packet
     */
    public void packSend() {
        pckDat.merge2beg();
        if (debugger.secTlsTraf) {
            logger.debug("tx type=" + type2string(pckTyp) + " size=" + pckDat.dataSize());
        }
        if (padModulo > 0) {
            byte[] buf = calcSum(macTx, seqTx);
            pckDat.putCopy(buf, 0, 0, buf.length);
            pckDat.putSkip(buf.length);
            pckDat.merge2end();
            buf = new byte[padModulo - (pckDat.dataSize() % padModulo)];
            bits.byteFill(buf, 0, buf.length, buf.length - 1);
            pckDat.putCopy(buf, 0, 0, buf.length);
            pckDat.putSkip(buf.length);
            pckDat.merge2end();
            if (verCurr >= 0x302) {
                for (int i = 0; i < padModulo; i++) {
                    pckDat.putByte(i, bits.randomB());
                }
                pckDat.putSkip(padModulo);
                pckDat.merge2beg();
            }
            pckDat.encrData(encTx, 0, pckDat.dataSize());
            seqTx++;
        }
        lineSend();
    }

    /**
     * receive one packet
     */
    public void packRecv() {
        if (lineRecv()) {
            return;
        }
        if (padModulo > 0) {
            pckDat.encrData(encRx, 0, pckDat.dataSize());
            if (verCurr >= 0x302) {
                pckDat.getSkip(padModulo);
            }
            int padS = pckDat.getByte(pckDat.dataSize() - 1);
            pckDat.setDataSize(pckDat.dataSize() - padS - 1);
            byte[] hshR = new byte[macRx.getHashSize()];
            if (pckDat.dataSize() < hshR.length) {
                logger.info("bad size");
                pipe.setClose();
                return;
            }
            pckDat.getCopy(hshR, 0, pckDat.dataSize() - hshR.length, hshR.length);
            pckDat.setDataSize(pckDat.dataSize() - hshR.length);
            byte[] hshC = calcSum(macRx, seqRx);
            for (int i = 0; i < hshC.length; i++) {
                if (hshC[i] == hshR[i]) {
                    continue;
                }
                logger.info("mac error");
                pipe.setClose();
                return;
            }
            seqRx++;
        }
        if (debugger.secTlsTraf) {
            logger.debug("rx type=" + type2string(pckTyp) + " size=" + pckDat.dataSize());
        }
    }

    private boolean lineRecv() {
        pckTyp = -1;
        pckDat.clear();
        int hed = getHeadSize();
        if (datagram) {
            if (pckDat.pipeRecv(pipe, 0, pckDat.dataSize(), 143) < hed) {
                pipe.setClose();
                return true;
            }
        } else {
            if (pckDat.pipeRecv(pipe, 0, hed, 144) != hed) {
                pipe.setClose();
                return true;
            }
        }
        pckTyp = pckDat.getByte(0);
        int ver = pckDat.msbGetW(1);
        int len;
        if (datagram) {
            ver = version2dtls(ver);
            len = pckDat.msbGetW(11);
            seqRx = pckDat.msbGetQ(3);
        } else {
            len = pckDat.msbGetW(3);
        }
        if (!datagram) {
            if (pckDat.pipeRecv(pipe, hed, len, 144) != len) {
                pipe.setClose();
                return true;
            }
        }
        pckDat.getSkip(hed);
        if (verCurr < 0) {
            verCurr = ver;
        } else {
            if (version2wire(verCurr) != ver) {
                pipe.setClose();
                return true;
            }
        }
        return false;
    }

    private void lineSend() {
        if (datagram) {
            pckDat.putByte(0, pckTyp);
            pckDat.msbPutW(1, version2dtls(version2wire(verCurr)));
            pckDat.msbPutQ(3, seqTx - 1);
            pckDat.msbPutW(11, pckDat.dataSize());
        } else {
            pckDat.putByte(0, pckTyp);
            pckDat.msbPutW(1, version2wire(verCurr));
            pckDat.msbPutW(3, pckDat.dataSize());
        }
        pckDat.putSkip(getHeadSize());
        pckDat.merge2beg();
        if (datagram) {
            pckDat.pipeSend(pipe, 0, pckDat.dataSize(), 2);
        } else {
            pckDat.pipeSend(pipe, 0, pckDat.dataSize(), 3);
        }
    }

    /**
     * receive one aead packet
     *
     * @return false on success, true on error
     */
    public boolean apackRecv() {
        if (lineRecv()) {
            return true;
        }
        if (pckTyp != typeAppDat) {
            pipe.setClose();
            return true;
        }
        byte[] buf = new byte[ivRx.length];
        bits.msbPutQ(buf, buf.length - 8, aeadRx);
        for (int i = 0; i < buf.length; i++) {
            buf[i] ^= ivRx[i];
        }
        encRx.init(keyRx, buf, false);
        int len = pckDat.dataSize();
        buf = new byte[5];
        buf[0] = typeAppDat;
        buf[1] = 3;
        buf[2] = 3;
        bits.msbPutW(buf, 3, len);
        encRx.authAdd(buf);
        len = pckDat.encrData(encRx, 0, len);
        if (len < 0) {
            logger.info("aead error");
            pipe.setClose();
            return true;
        }
        for (; len > 0; len--) {
            if (pckDat.getByte(len - 1) != 0) {
                break;
            }
        }
        len--;
        pckTyp = pckDat.getByte(len);
        pckDat.setDataSize(len);
        aeadRx++;
        if (debugger.secTlsTraf) {
            logger.debug("rx type=" + type2string(pckTyp) + " size=" + pckDat.dataSize());
        }
        return false;
    }

    /**
     * send current aead packet
     *
     * @return false on success, true on error
     */
    public boolean apackSend() {
        pckDat.merge2beg();
        if (debugger.secTlsTraf) {
            logger.debug("tx type=" + type2string(pckTyp) + " size=" + pckDat.dataSize());
        }
        pckDat.putByte(0, pckTyp);
        pckDat.putSkip(1);
        pckDat.merge2end();
        byte[] buf = new byte[ivTx.length];
        bits.msbPutQ(buf, buf.length - 8, aeadTx);
        for (int i = 0; i < buf.length; i++) {
            buf[i] ^= ivTx[i];
        }
        encTx.init(keyTx, buf, true);
        int len = pckDat.dataSize();
        buf = new byte[5];
        buf[0] = typeAppDat;
        buf[1] = 3;
        buf[2] = 3;
        bits.msbPutW(buf, 3, len + padModulo);
        encTx.authAdd(buf);
        len = pckDat.encrData(encTx, 0, len);
        if (len < 0) {
            logger.info("aead error");
            pipe.setClose();
            return true;
        }
        pckDat.setDataSize(len);
        pckTyp = typeAppDat;
        lineSend();
        aeadTx++;
        return false;
    }

}
