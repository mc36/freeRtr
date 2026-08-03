package org.freertr.auth;

import org.freertr.cry.cryHashMd5;
import org.freertr.pack.packHolder;
import org.freertr.util.bits;

/**
 * challenge handshake authentication protocol (rfc1994)
 *
 * @author matecsaba
 */
public class autherChap extends autherDoer {

    /**
     * ppp name
     */
    public final static String pppName = "chap";

    /**
     * ppp control type
     */
    public final static int pppCtrl = 0xc223;

    /**
     * challenge
     */
    public final static int codeChal = 1;

    /**
     * response
     */
    public final static int codeResp = 2;

    /**
     * success
     */
    public final static int codeSucc = 3;

    /**
     * failure
     */
    public final static int codeFail = 4;

    private int sentId;

    private byte[] sentCh;

    /**
     * compute auth response
     *
     * @param id id received
     * @param pass password to send
     * @param chal challenge reveived
     * @return sum calculated
     */
    public static byte[] calcAuthHash(int id, String pass, byte[] chal) {
        cryHashMd5 md = new cryHashMd5();
        md.init();
        md.update(id);
        md.update(pass.getBytes());
        md.update(chal);
        return md.finish();
    }

    /**
     * the constructor
     *
     * @param prnt parent
     */
    public autherChap(authenDown prnt) {
        parent = prnt;
    }

    /**
     * convert code to string
     *
     * @param i code
     * @return string
     */
    public static String code2str(int i) {
        switch (i) {
            case codeChal:
                return "challenge";
            case codeResp:
                return "response";
            case codeSucc:
                return "success";
            case codeFail:
                return "failure";
            default:
                return "unknown=" + i;
        }
    }

    protected void authenRecv(packHolder pck, int code, int id) {
        autherChapMsg msg = new autherChapMsg();
        msg.code = code;
        msg.id = id;
        if (msg.parsePack(pck)) {
            return;
        }
        if (!working) {
            return;
        }
        working = false;
        parent.recvAuthPack("" + msg);
        switch (code) {
            case codeChal:
                if (!isClient()) {
                    return;
                }
                working = true;
                msg.code = codeResp;
                msg.message = sentUser;
                msg.data = calcAuthHash(id, sentPass, msg.data);
                msg.createPack(pck);
                parent.sendAuthPack(pck, pppCtrl, msg.code, msg.id, "" + msg);
                break;
            case codeResp:
                if (isClient()) {
                    return;
                }
                if (sentId != msg.id) {
                    parent.recvAuthPack("got bad id");
                    working = true;
                    return;
                }
                result = authenRem.authUserChap(msg.message, sentId, sentCh, msg.data);
                if (result.result == authResult.authSuccessful) {
                    msg.code = codeSucc;
                } else {
                    msg.code = codeFail;
                }
                msg.message = code2str(msg.code);
                msg.id = sentId;
                msg.createPack(pck);
                parent.sendAuthPack(pck, pppCtrl, msg.code, msg.id, "" + msg);
                break;
            case codeSucc:
                if (!isClient()) {
                    return;
                }
                result = new authResult(authenRem, authResult.authSuccessful, sentUser, sentPass);
                break;
            case codeFail:
                if (!isClient()) {
                    return;
                }
                result = new authResult(authenRem, authResult.authBadUserPass, sentUser, sentPass);
                break;
            default:
                working = true;
                break;
        }
    }

    protected void authenSend(packHolder pck) {
        if (!working) {
            return;
        }
        if (isClient()) {
            return;
        }
        sentId = bits.randomB();
        sentCh = new byte[16];
        for (int i = 0; i < sentCh.length; i++) {
            sentCh[i] = (byte) bits.randomB();
        }
        autherChapMsg msg = new autherChapMsg();
        msg.code = codeChal;
        msg.id = sentId;
        msg.message = "";
        msg.data = sentCh;
        msg.createPack(pck);
        parent.sendAuthPack(pck, pppCtrl, msg.code, msg.id, "" + msg);
        return;
    }

}

class autherChapMsg {

    public int code;

    public int id;

    public byte[] data;

    public String message;

    public boolean needValue() {
        switch (code) {
            case autherChap.codeChal:
            case autherChap.codeResp:
                return true;
            default:
                return false;
        }
    }

    public boolean parsePack(packHolder pck) {
        if (needValue()) {
            int i = pck.getByte(0);
            pck.getSkip(1);
            data = new byte[i];
            pck.getCopy(data, 0, 0, i);
            pck.getSkip(i);
        } else {
            data = new byte[0];
        }
        message = pck.getAsciiZ(0, pck.dataSize(), 0);
        return false;
    }

    public void createPack(packHolder pck) {
        pck.clear();
        if (needValue()) {
            pck.putByte(0, data.length);
            pck.putSkip(1);
            pck.putCopy(data, 0, 0, data.length);
            pck.putSkip(data.length);
        }
        pck.putAsciiZ(0, message.length(), message, 0);
        pck.putSkip(message.length());
        pck.merge2beg();
    }

    public String toString() {
        return "cod=" + autherChap.code2str(code) + " id=" + id + " msg=" + message + " val=" + bits.byteDump(data, 0, -1);
    }

}
