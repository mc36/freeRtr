package pack;

import cry.cryEncrGeneric;
import cry.cryHashHmac;
import cry.cryUtils;
import java.math.BigInteger;
import pipe.pipeSide;
import util.bits;
import util.debugger;
import util.logger;

/**
 * secure shell (rfc4251) packet
 *
 * @author matecsaba
 */
public class packSsh {

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
    public int padModulo = 8;

    /**
     * sendingsequence number
     */
    public int seqTx = 0;

    /**
     * receiving sequence number
     */
    public int seqRx = 0;

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
    public cryHashHmac macTx;

    /**
     * receiving hash
     */
    public cryHashHmac macRx;

    /**
     * disconnect
     */
    public final static int typeDisconn = 1;

    /**
     * ignore
     */
    public final static int typeIgnore = 2;

    /**
     * unimplemented
     */
    public final static int typeUnimp = 3;

    /**
     * debug
     */
    public final static int typeDebug = 4;

    /**
     * service request
     */
    public final static int typeSrvReq = 5;

    /**
     * service accept
     */
    public final static int typeSrvAcc = 6;

    /**
     * key exchange init
     */
    public final static int typeKexInit = 20;

    /**
     * new keys
     */
    public final static int typeNewKeys = 21;

    /**
     * diffie hellman fix group init
     */
    public final static int typeDHGinit = 30;

    /**
     * diffie hellman fix group reply
     */
    public final static int typeDHGrply = 31;

    /**
     * diffie hellman group exchange old request
     */
    public final static int typeDHXold = 30;

    /**
     * diffie hellman group exchange group
     */
    public final static int typeDHXgrp = 31;

    /**
     * diffie hellman group exchange init
     */
    public final static int typeDHXinit = 32;

    /**
     * diffie hellman group exchange reply
     */
    public final static int typeDHXrply = 33;

    /**
     * diffie hellman group exchange request
     */
    public final static int typeDHXreq = 34;

    /**
     * auth request
     */
    public final static int typeAuthReq = 50;

    /**
     * auth failure
     */
    public final static int typeAuthFail = 51;

    /**
     * auth success
     */
    public final static int typeAuthSucc = 52;

    /**
     * auth banner
     */
    public final static int typeAuthBann = 53;

    /**
     * global request
     */
    public final static int typeGlobReq = 80;

    /**
     * global success
     */
    public final static int typeGlobSucc = 81;

    /**
     * global failure
     */
    public final static int typeGlobFail = 82;

    /**
     * channel open
     */
    public final static int typeChanOpen = 90;

    /**
     * channel confirmation
     */
    public final static int typeOpenConf = 91;

    /**
     * channel failure
     */
    public final static int typeOpenFail = 92;

    /**
     * channel window adjust
     */
    public final static int typeChanWin = 93;

    /**
     * channel data
     */
    public final static int typeChanData = 94;

    /**
     * channel extended data
     */
    public final static int typeChanExtDat = 95;

    /**
     * channel eof
     */
    public final static int typeChanEof = 96;

    /**
     * channel close
     */
    public final static int typeChanClose = 97;

    /**
     * channel request
     */
    public final static int typeChanReq = 98;

    /**
     * channel success
     */
    public final static int typeChanSucc = 99;

    /**
     * channel failure
     */
    public final static int typeChanErr = 100;

    /**
     * authentication service
     */
    public final static String serviceAuth = "ssh-userauth";

    /**
     * connection service
     */
    public final static String serviceConn = "ssh-connection";

    /**
     * null authentication mode
     */
    public final static String authenNone = "none";

    /**
     * password authentication mode
     */
    public final static String authenPass = "password";

    /**
     * type of session
     */
    public final static String sessType = "session";

    private final static int headSize = 5;

    /**
     * create packet holder
     */
    public packSsh() {
        pckDat = new packHolder(true, true);
    }

    /**
     * clone packet holder
     *
     * @return new packet holder
     */
    public packSsh copyBytes() {
        packSsh p = new packSsh();
        p.pipe = pipe;
        p.padModulo = padModulo;
        p.seqTx = seqTx;
        p.seqRx = seqRx;
        p.encTx = encTx;
        p.encRx = encRx;
        p.macTx = macTx;
        p.macRx = macRx;
        return p;
    }

    /**
     * convert message type to string
     *
     * @param i message type
     * @return string
     */
    public static String type2string(int i) {
        switch (i) {
            case typeDisconn:
                return "disconnect";
            case typeIgnore:
                return "ignore";
            case typeUnimp:
                return "unimplemented";
            case typeDebug:
                return "debug";
            case typeSrvReq:
                return "service request";
            case typeSrvAcc:
                return "service accept";
            case typeKexInit:
                return "key exchange";
            case typeNewKeys:
                return "new keys";
            case typeDHXold:
                return "kex request";
            case typeDHXgrp:
                return "kex group";
            case typeDHXinit:
                return "kex init";
            case typeDHXrply:
                return "kex reply";
            case typeDHXreq:
                return "kex request";
            case typeAuthReq:
                return "auth request";
            case typeAuthFail:
                return "auth failure";
            case typeAuthSucc:
                return "auth success";
            case typeAuthBann:
                return "auth banner";
            case typeGlobReq:
                return "global request";
            case typeGlobSucc:
                return "global success";
            case typeGlobFail:
                return "global failure";
            case typeChanOpen:
                return "channel open";
            case typeOpenConf:
                return "channel confirmation";
            case typeOpenFail:
                return "channel rejected";
            case typeChanWin:
                return "window adjust";
            case typeChanData:
                return "channel data";
            case typeChanExtDat:
                return "channel ext data";
            case typeChanEof:
                return "channel eof";
            case typeChanClose:
                return "channel close";
            case typeChanReq:
                return "channel request";
            case typeChanSucc:
                return "channel success";
            case typeChanErr:
                return "channel failure";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * read buffer from packet
     *
     * @param pck packet to work on
     * @return bytes readed
     */
    public static byte[] bytesRead(packHolder pck) {
        int o = pck.msbGetD(0);
        if (o > 4096) {
            return null;
        }
        if (o < 0) {
            return null;
        }
        pck.getSkip(4);
        byte[] buf = new byte[o];
        for (int i = 0; i < o; i++) {
            buf[i] = (byte) pck.getByte(i);
        }
        pck.getSkip(o);
        return buf;
    }

    /**
     * write buffer to packet
     *
     * @param pck packet to work on
     * @param buf bytes to write
     */
    public static void bytesWrite(packHolder pck, byte[] buf) {
        pck.msbPutD(0, buf.length);
        pck.putSkip(4);
        for (int i = 0; i < buf.length; i++) {
            pck.putByte(i, buf[i]);
        }
        pck.putSkip(buf.length);
    }

    /**
     * read string from packet
     *
     * @param pck packet to work on
     * @return string readed
     */
    public static String stringRead(packHolder pck) {
        byte[] buf = bytesRead(pck);
        if (buf == null) {
            return "";
        }
        return new String(buf);
    }

    /**
     * write string to packet
     *
     * @param pck packet to work on
     * @param s string to write
     */
    public static void stringWrite(packHolder pck, String s) {
        bytesWrite(pck, s.getBytes());
    }

    /**
     * read bigint from packet
     *
     * @param pck packet to work on
     * @return bigint readed
     */
    public static BigInteger bigIntRead(packHolder pck) {
        byte[] buf = bytesRead(pck);
        if (buf == null) {
            return BigInteger.ZERO;
        }
        return new BigInteger(buf);
    }

    /**
     * write bigint to packet
     *
     * @param pck packet to work on
     * @param b bigint to write
     */
    public static void bigIntWrite(packHolder pck, BigInteger b) {
        bytesWrite(pck, b.toByteArray());
    }

    /**
     * read unsigned bigint from packet
     *
     * @param pck packet to work on
     * @return bigint readed
     */
    public static BigInteger bigUIntRead(packHolder pck) {
        byte[] dat = bytesRead(pck);
        if (dat == null) {
            return BigInteger.ZERO;
        }
        return cryUtils.buf2bigUint(dat);
    }

    /**
     * write unsigned bigint to packet
     *
     * @param pck packet to work on
     * @param b bigint to write
     */
    public static void bigUIntWrite(packHolder pck, BigInteger b) {
        bytesWrite(pck, cryUtils.bigUint2buf(b));
    }

    /**
     * read buffer from packet
     *
     * @return bytes readed
     */
    public byte[] bytesRead() {
        return bytesRead(pckDat);
    }

    /**
     * write buffer to packet
     *
     * @param buf bytes to write
     */
    public void bytesWrite(byte[] buf) {
        bytesWrite(pckDat, buf);
    }

    /**
     * read string from packet
     *
     * @return string readed
     */
    public String stringRead() {
        return stringRead(pckDat);
    }

    /**
     * write string to packet
     *
     * @param s string to write
     */
    public void stringWrite(String s) {
        stringWrite(pckDat, s);
    }

    /**
     * read bigint from packet
     *
     * @return bigint readed
     */
    public BigInteger bigIntRead() {
        return bigIntRead(pckDat);
    }

    /**
     * write bigint to packet
     *
     * @param b bigint to write
     */
    public void bigIntWrite(BigInteger b) {
        bigIntWrite(pckDat, b);
    }

    /**
     * send current packet
     */
    public synchronized void packSend() {
        pckDat.merge2beg();
        if (debugger.secSshTraf) {
            logger.debug("tx type=" + type2string(pckTyp) + " size=" + pckDat.dataSize());
        }
        pckDat.putByte(0, pckTyp);
        pckDat.putSkip(1);
        pckDat.merge2beg();
        int siz = pckDat.dataSize();
        int pad = siz + headSize;
        pad = padModulo - (pad % padModulo);
        if (pad < 4) {
            pad += padModulo;
        }
        pckDat.msbPutD(0, siz + 1 + pad);
        pckDat.putByte(4, pad);
        pckDat.putSkip(headSize);
        pckDat.merge2beg();
        for (int i = 0; i < pad; i++) {
            pckDat.putByte(i, bits.randomB());
        }
        pckDat.putSkip(pad);
        pckDat.merge2end();
        siz = pckDat.dataSize();
        byte[] hsh = null;
        if (macTx != null) {
            hsh = new byte[4];
            bits.msbPutD(hsh, 0, seqTx);
            macTx.init();
            macTx.update(hsh, 0, hsh.length);
            pckDat.hashData(macTx, 0, siz);
            hsh = macTx.finish();
        }
        if (encTx != null) {
            pckDat.encrData(encTx, 0, siz);
        }
        pckDat.pipeSend(pipe, 0, siz, 3);
        if (hsh != null) {
            pipe.morePut(hsh, 0, hsh.length);
        }
        seqTx++;
    }

    /**
     * receive one packet
     */
    public void packRecv() {
        pckTyp = -1;
        pckDat.clear();
        int pre;
        if (encRx == null) {
            pre = headSize;
        } else {
            pre = encRx.getBlockSize();
        }
        if (pckDat.pipeRecv(pipe, 0, pre, 144) != pre) {
            pipe.setClose();
            return;
        }
        if (encRx != null) {
            pckDat.encrData(encRx, 0, pre);
        }
        int siz = pckDat.msbGetD(0) + 4;
        int pad = pckDat.getByte(4);
        int pay = siz - pre;
        if ((pay < 0) || (pay > packHolder.maxData)) {
            logger.info("bad size");
            pipe.setClose();
            return;
        }
        if (pay > 0) {
            if (pckDat.pipeRecv(pipe, pre, pay, 144) != pay) {
                pipe.setClose();
                return;
            }
        }
        if (encRx != null) {
            pckDat.encrData(encRx, pre, pay);
        }
        if (macRx != null) {
            byte[] hshC = new byte[4];
            bits.msbPutD(hshC, 0, seqRx);
            macRx.init();
            macRx.update(hshC, 0, hshC.length);
            pckDat.hashData(macRx, 0, siz);
            hshC = macRx.finish();
            byte[] hshR = new byte[hshC.length];
            if (pipe.moreGet(hshR, 0, hshR.length) != hshR.length) {
                pipe.setClose();
                return;
            }
            for (int i = 0; i < hshC.length; i++) {
                if (hshC[i] == hshR[i]) {
                    continue;
                }
                logger.info("mac error");
                pipe.setClose();
                return;
            }
        }
        pckDat.getSkip(headSize);
        siz = siz - pad - headSize;
        pckDat.setDataSize(siz);
        pckTyp = pckDat.getByte(0);
        pckDat.getSkip(1);
        if (debugger.secSshTraf) {
            logger.debug("rx type=" + type2string(pckTyp) + " size=" + pckDat.dataSize());
        }
        seqRx++;
        if (pckTyp == packSsh.typeIgnore) {
            packRecv();
        }
    }

}
