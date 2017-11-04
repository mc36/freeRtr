package auth;

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
     * @param par parent
     * @param res result
     * @param nam username
     */
    public authResult(authGeneric par, int res, String nam) {
        result = res;
        user = nam;
        lower = par;
        if ((lower.logErr) && (res == authServerError)) {
            logger.info("error while authenticating " + nam);
        }
        if ((lower.logFail) && (res == authBadUserPass)) {
            logger.info("bad user/pass for " + nam);
        }
        if ((lower.logOk) && (res == authSuccessful)) {
            logger.info("successful for " + nam);
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
