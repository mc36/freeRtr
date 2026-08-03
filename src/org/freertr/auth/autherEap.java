package org.freertr.auth;

import org.freertr.pack.packHolder;
import org.freertr.util.bits;

/**
 * extensible authentication protocol (rfc3748)
 *
 * @author matecsaba
 */
public class autherEap extends autherDoer {

    /**
     * ppp name
     */
    public final static String pppName = "eap";

    /**
     * ppp control type
     */
    public final static int pppCtrl = 0xc227;

    /**
     * request
     */
    public final static int codeReq = 1;

    /**
     * response
     */
    public final static int codeRep = 2;

    /**
     * success
     */
    public final static int codeSucc = 3;

    /**
     * failure
     */
    public final static int codeFail = 4;

    /**
     * identity
     */
    public final static int typeId = 1;

    /**
     * notification
     */
    public final static int typeNtfy = 2;

    /**
     * negative acknowledge
     */
    public final static int typeNak = 3;

    /**
     * md5 Challenge
     */
    public final static int typeChal = 4;

    /**
     * one time password
     */
    public final static int typeOtp = 5;

    /**
     * generic token card
     */
    public final static int typeGtc = 6;

    private String gotId = null;

    private int sentId;

    private byte[] sentCh;

    /**
     * the constructor
     *
     * @param prnt parent
     */
    public autherEap(authenDown prnt) {
        parent = prnt;
    }

    /**
     * convert code to string
     *
     * @param i code value
     * @return string value
     */
    public static String code2str(int i) {
        switch (i) {
            case codeReq:
                return "request";
            case codeRep:
                return "response";
            case codeSucc:
                return "success";
            case codeFail:
                return "failure";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * convert type to string
     *
     * @param i type value
     * @return string value
     */
    public static String type2str(int i) {
        switch (i) {
            case typeId:
                return "identity";
            case typeNtfy:
                return "notification";
            case typeNak:
                return "negativeAcknowledge";
            case typeChal:
                return "md5challenge";
            case typeOtp:
                return "oneTimePassword";
            case typeGtc:
                return "genericTokenCard";
            default:
                return "unknown=" + i;
        }
    }

    protected void authenRecv(packHolder pck, int code, int id) {
        autherEapMsg msg = new autherEapMsg();
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
            case codeReq:
                if (!isClient()) {
                    return;
                }
                working = true;
                switch (msg.type) {
                    case typeId:
                        msg.code = codeRep;
                        msg.message = sentUser;
                        break;
                    case typeChal:
                        msg.code = codeRep;
                        msg.message = sentUser;
                        msg.data = autherChap.calcAuthHash(id, sentPass, msg.data);
                        break;
                    default:
                        return;
                }
                msg.createPack(pck);
                parent.sendAuthPack(pck, pppCtrl, msg.code, msg.id, "" + msg);
                break;
            case codeRep:
                if (isClient()) {
                    return;
                }
                switch (msg.type) {
                    case typeId:
                        gotId = "" + msg.message;
                        working = true;
                        break;
                    case typeChal:
                        if (sentId != msg.id) {
                            parent.recvAuthPack("got bad id");
                            working = true;
                            return;
                        }
                        result = authenRem.authUserChap(gotId, sentId, sentCh, msg.data);
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
                }
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
        autherEapMsg msg = new autherEapMsg();
        msg.code = codeReq;
        msg.id = sentId;
        if (gotId == null) {
            msg.type = typeId;
            msg.data = new byte[0];
            msg.message = "";
        } else {
            sentCh = new byte[16];
            for (int i = 0; i < sentCh.length; i++) {
                sentCh[i] = (byte) bits.randomB();
            }
            msg.type = typeChal;
            msg.data = sentCh;
            msg.message = "";
        }
        msg.createPack(pck);
        parent.sendAuthPack(pck, pppCtrl, msg.code, msg.id, "" + msg);
        return;
    }

}

class autherEapMsg {

    public int code;

    public int id;

    public int type;

    public String message;

    public byte[] data;

    private boolean needData() {
        switch (code) {
            case autherEap.codeReq:
            case autherEap.codeRep:
                return true;
            default:
                return false;
        }
    }

    public boolean parsePack(packHolder pck) {
        if (!needData()) {
            return false;
        }
        type = pck.getByte(0);
        pck.getSkip(1);
        if (type == autherEap.typeChal) {
            int i = pck.getByte(0);
            pck.getSkip(1);
            data = new byte[i];
            pck.getCopy(data, 0, 0, data.length);
            pck.getSkip(data.length);
        }
        message = pck.getAsciiZ(0, pck.dataSize(), 0);
        return false;
    }

    public void createPack(packHolder pck) {
        pck.clear();
        if (!needData()) {
            return;
        }
        pck.putByte(0, type);
        pck.putSkip(1);
        if (type == autherEap.typeChal) {
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
        return "cod=" + autherEap.code2str(code) + " id=" + id + " typ=" + autherEap.type2str(type) + " message=" + message + " val=" + bits.byteDump(data, 0, -1);
    }

}
