package org.freertr.pack;

import org.freertr.util.bits;
import org.freertr.util.debugger;
import org.freertr.util.logger;

/**
 * secure shell authentication (rfc4252) protocol
 *
 * @author matecsaba
 */
public class packSshAuth {

    /**
     * service requested
     */
    public String service;

    /**
     * name of user
     */
    public String username;

    /**
     * password of user
     */
    public String password;

    /**
     * public key of user
     */
    public byte[] pkeyBlob;

    /**
     * signature of user
     */
    public byte[] pkeySign;

    /**
     * authentication method
     */
    public String method;

    private final packSsh lower;

    private final packSshKex keyXchg;

    /**
     * create instance
     *
     * @param ssh lower layer
     * @param kex lower layer
     */
    public packSshAuth(packSsh ssh, packSshKex kex) {
        lower = ssh;
        keyXchg = kex;
    }

    /**
     * get data to be signed
     *
     * @return bytes
     */
    public byte[] getAuthen2signed() {
        packHolder p = new packHolder(true, true);
        packSsh.bytesWrite(p, keyXchg.hashVal);
        p.putByte(0, packSsh.typeAuthReq);
        p.putSkip(1);
        packSsh.stringWrite(p, username);
        packSsh.stringWrite(p, service);
        packSsh.stringWrite(p, method);
        p.putByte(0, 1);
        p.putSkip(1);
        packSsh.stringWrite(p, password);
        packSsh.bytesWrite(p, pkeyBlob);
        p.merge2beg();
        return p.getCopy();
    }

    /**
     * parse service request message
     *
     * @return false on success, true on error
     */
    public boolean servReqParse() {
        if (lower.pckTyp != packSsh.typeSrvReq) {
            return true;
        }
        service = lower.stringRead();
        if (debugger.secSshTraf) {
            servReqDump("rx");
        }
        return false;
    }

    /**
     * create service request packet
     *
     * @param s service to request
     */
    public void servReqCreate(String s) {
        service = s;
        if (debugger.secSshTraf) {
            servReqDump("tx");
        }
        lower.pckTyp = packSsh.typeSrvReq;
        lower.pckDat.clear();
        lower.stringWrite(service);
    }

    private void servReqDump(String dir) {
        logger.debug(dir + " service=" + service);
    }

    /**
     * parse service accept message
     *
     * @return false on success, true on error
     */
    public boolean servAcptParse() {
        if (lower.pckTyp != packSsh.typeSrvAcc) {
            return true;
        }
        service = lower.stringRead();
        if (debugger.secSshTraf) {
            servReqDump("rx");
        }
        return false;
    }

    /**
     * create service accept packet
     *
     * @param srv service to request
     */
    public void servActpCreate(String srv) {
        service = srv;
        if (debugger.secSshTraf) {
            servReqDump("tx");
        }
        lower.pckTyp = packSsh.typeSrvAcc;
        lower.pckDat.clear();
        lower.stringWrite(service);
    }

    /**
     * parse authentication request
     *
     * @return false on success, true on error
     */
    public boolean authReqParse() {
        if (lower.pckTyp != packSsh.typeAuthReq) {
            return true;
        }
        username = lower.stringRead();
        service = lower.stringRead();
        method = lower.stringRead();
        int bool = lower.pckDat.getByte(0);
        lower.pckDat.getSkip(1);
        password = lower.stringRead();
        if (method.equals(packSsh.authenPkey)) {
            pkeyBlob = lower.bytesRead();
            if (bool != 0) {
                pkeySign = lower.bytesRead();
            }
        }
        if (debugger.secSshTraf) {
            authReqDump("rx");
        }
        return false;
    }

    /**
     * create authentication request
     *
     * @param srv service to request
     */
    public void authReqCreate(String srv) {
        service = srv;
        method = packSsh.authenPass;
        if (password == null) {
            method = packSsh.authenNone;
        }
        if (pkeyBlob != null) {
            method = packSsh.authenPkey;
        }
        if (debugger.secSshTraf) {
            authReqDump("tx");
        }
        lower.pckTyp = packSsh.typeAuthReq;
        lower.pckDat.clear();
        lower.stringWrite(username);
        lower.stringWrite(service);
        lower.stringWrite(method);
        if (pkeySign != null) {
            lower.pckDat.putByte(0, 1);
        } else {
            lower.pckDat.putByte(0, 0);
        }
        lower.pckDat.putSkip(1);
        if (password != null) {
            lower.stringWrite(password);
        }
        if (pkeyBlob != null) {
            lower.bytesWrite(pkeyBlob);
        }
        if (pkeySign != null) {
            lower.bytesWrite(pkeySign);
        }
    }

    private void authReqDump(String dir) {
        logger.debug(dir + " service=" + service + " user=" + username + " method=" + method + " password=" + password + " blob=" + bits.byteDump(pkeyBlob, 0, -1) + " sign=" + bits.byteDump(pkeySign, 0, -1));
    }

    /**
     * parse authentication request
     *
     * @return false on success, true on error
     */
    public boolean authPkeyParse() {
        if (lower.pckTyp != packSsh.typeAuthPkey) {
            return true;
        }
        password = lower.stringRead();
        pkeyBlob = lower.bytesRead();
        if (debugger.secSshTraf) {
            authPkeyDump("rx");
        }
        return false;
    }

    /**
     * create authentication request
     */
    public void authPkeyCreate() {
        if (debugger.secSshTraf) {
            authPkeyDump("tx");
        }
        lower.pckTyp = packSsh.typeAuthPkey;
        lower.stringWrite(password);
        lower.bytesWrite(pkeyBlob);
    }

    private void authPkeyDump(String dir) {
        logger.debug(dir + " method=" + password + " blob=" + bits.byteDump(pkeyBlob, 0, -1));
    }

    /**
     * parse authentication success
     *
     * @return false on success, true on error
     */
    public boolean authSuccParse() {
        if (lower.pckTyp != packSsh.typeAuthSucc) {
            return true;
        }
        return false;
    }

    /**
     * create authentication success
     */
    public void authSuccCreate() {
        lower.pckTyp = packSsh.typeAuthSucc;
        lower.pckDat.clear();
    }

    /**
     * parse authentication failure
     *
     * @return false on success, true on error
     */
    public boolean authFailParse() {
        if (lower.pckTyp != packSsh.typeAuthFail) {
            return true;
        }
        method = lower.stringRead();
        return false;
    }

    /**
     * create authentication success
     */
    public void authFailCreate() {
        method = packSsh.authenPkey + "," + packSsh.authenPass;
        lower.pckTyp = packSsh.typeAuthFail;
        lower.pckDat.clear();
        lower.stringWrite(method);
        lower.pckDat.putByte(0, 0);
        lower.pckDat.putSkip(1);
    }

}
