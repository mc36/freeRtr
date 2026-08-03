package org.freertr.auth;

import org.freertr.addr.addrEui;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrIPv6;
import org.freertr.user.userFormat;
import org.freertr.util.bits;
import org.freertr.util.logger;

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
     * privilege level
     */
    public String filter;

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

    /**
     * convert result to string
     *
     * @param i result
     * @return string
     */
    public static String result2string(int i) {
        switch (i) {
            case authSuccessful:
                return "success";
            case authBadUserPass:
                return "badCredentinals";
            case authServerError:
                return "serverError";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * create new result
     */
    public authResult() {
        result = authServerError;
        user = "<nobody>";
    }

    /**
     * create new result
     *
     * @param lower authenticator
     * @param res result
     * @param nam username
     * @param pwd password
     */
    public authResult(authGeneric lower, int res, String nam, String pwd) {
        result = res;
        user = nam;
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
                lower.lastErr = bits.getTime();
                lower.sawErr++;
                break;
            case authBadUserPass:
                if (lower.logFail) {
                    logger.info("bad user/pass for " + nam);
                }
                lower.lastFail = bits.getTime();
                lower.sawFail++;
                break;
            case authSuccessful:
                if (lower.logOk) {
                    logger.info("successful for " + nam);
                }
                lower.lastOk = bits.getTime();
                lower.sawOk++;
                break;
        }
    }

    public String toString() {
        return result2string(result) + " privi=" + privilege + " user=" + user;
    }

    /**
     * dump result
     *
     * @return result
     */
    public userFormat dump() {
        userFormat lst = new userFormat("|", "category|value");
        lst.add("result|" + result2string(result));
        lst.add("username|" + user);
        lst.add("command|" + autoCommand);
        lst.add("hangup|" + autoHangup);
        lst.add("privilege|" + privilege);
        lst.add("filter|" + filter);
        lst.add("ipv4 addr|" + ipv4addr);
        lst.add("ipv4 route|" + ipv4route);
        lst.add("ipv6 addr|" + ipv6addr);
        lst.add("ipv6 route|" + ipv6route);
        lst.add("ipv6 ifid|" + ipv6ifid);
        return lst;
    }

}
