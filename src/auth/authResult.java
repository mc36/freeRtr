package auth;

import addr.addrEui;
import addr.addrIPv4;
import addr.addrIPv6;
import util.logger;

/**
 * result of authentication
 *
 * @author matecsaba
 */
public class authResult {

    /**
     * successfully authenticated
     */
    public final static int authSuccessful = 1;

    /**
     * bad authentication data
     */
    public final static int authBadUserPass = 2;

    /**
     * server error
     */
    public final static int authServerError = 3;

    /**
     * result of authentication
     */
    public final int result;

    /**
     * username authenticated
     */
    public final String user;

    /**
     * autocommand to execute
     */
    public String autoCommand = "";

    /**
     * hangup after command
     */
    public boolean autoHangup = false;

    /**
     * privilege level
     */
    public int privilege = 0;

    /**
     * ipv4 address
     */
    public addrIPv4 ipv4addr;

    /**
     * ipv4 routes
     */
    public String ipv4route;

    /**
     * ipv6 address
     */
    public addrIPv6 ipv6addr;

    /**
     * ipv6 interface id
     */
    public addrEui ipv6ifid;

    /**
     * ipv6 routes
     */
    public String ipv6route;

    private final authGeneric lower;

    /**
     * create new result
     */
    public authResult() {
        result = authServerError;
        user = "<nobody>";
        lower = null;
    }

    /**
     * create new result
     *
     * @param par authenticator
     * @param res result
     * @param nam username
     * @param pwd password
     */
    public authResult(authGeneric par, int res, String nam, String pwd) {
        result = res;
        user = nam;
        lower = par;
        if (lower == null) {
            return;
        }
        if (lower.logPass) {
            nam = nam + "/" + pwd;
        }
        switch (res) {
            case authServerError:
                if (lower.logErr) {
                    logger.info("error while authenticating " + nam);
                }
                par.sawErr++;
                break;
            case authBadUserPass:
                if (lower.logFail) {
                    logger.info("bad user/pass for " + nam);
                }
                par.sawFail++;
                break;
            case authSuccessful:
                if (lower.logOk) {
                    logger.info("successful for " + nam);
                }
                par.sawOk++;
                break;
        }
    }

    public String toString() {
        String s;
        switch (result) {
            case authSuccessful:
                s = "success";
                break;
            case authBadUserPass:
                s = "badCredentinals";
                break;
            case authServerError:
                s = "serverError";
                break;
            default:
                s = "unknown=" + result;
                break;
        }
        return s + " privi=" + privilege + " user=" + user;
    }

}
