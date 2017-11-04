package pack;

import util.debugger;
import util.logger;

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
     * authentication method
     */
    public String method;

    private final packSsh lower;

    /**
     * create instance
     *
     * @param pack lower layer
     */
    public packSshAuth(packSsh pack) {
        lower = pack;
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
        lower.pckDat.getSkip(1);
        password = lower.stringRead();
        if (debugger.secSshTraf) {
            authReqDump("rx");
        }
        return false;
    }

    /**
     * create authentication request
     *
     * @param srv service to request
     * @param usr username
     * @param pwd password
     */
    public void authReqCreate(String srv, String usr, String pwd) {
        service = srv;
        username = usr;
        password = pwd;
        method = packSsh.authenPass;
        if (pwd == null) {
            method = packSsh.authenNone;
        }
        if (debugger.secSshTraf) {
            authReqDump("tx");
        }
        lower.pckTyp = packSsh.typeAuthReq;
        lower.pckDat.clear();
        lower.stringWrite(username);
        lower.stringWrite(service);
        lower.stringWrite(method);
        lower.pckDat.putByte(0, 0);
        lower.pckDat.putSkip(1);
        if (pwd != null) {
            lower.stringWrite(password);
        }
    }

    private void authReqDump(String dir) {
        logger.debug(dir + " service=" + service + " user=" + username + " method=" + method);
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
        method = packSsh.authenPass;
        lower.pckTyp = packSsh.typeAuthFail;
        lower.pckDat.clear();
        lower.stringWrite(method);
        lower.pckDat.putByte(0, 0);
        lower.pckDat.putSkip(1);
    }

}
