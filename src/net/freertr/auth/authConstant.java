package net.freertr.auth;

import java.util.ArrayList;
import java.util.List;
import net.freertr.cry.cryHashGeneric;
import net.freertr.cry.cryKeyGeneric;
import net.freertr.user.userFormat;
import net.freertr.user.userHelping;
import net.freertr.util.cmds;

/**
 * authentication that always succeeds
 *
 * @author matecsaba
 */
public class authConstant extends authGeneric {

    private final boolean res;

    /**
     * create authenticator
     *
     * @param result result of operation, true=success, false=failure
     */
    public authConstant(boolean result) {
        res = result;
    }

    public authResult authUserPass(String user, String pass) {
        if (res) {
            return new authResult(this, authResult.authSuccessful, user, pass);
        } else {
            return new authResult(this, authResult.authBadUserPass, user, pass);
        }
    }

    public authResult authUserCommand(String user, String cmd) {
        return authUserPass(user, "");
    }

    public authResult authUserChap(String user, int id, byte[] chal, byte[] resp) {
        return authUserPass(user, "");
    }

    public authResult authUserApop(String cookie, String user, String resp) {
        return authUserPass(user, "");
    }

    public authResult authUserPkey(cryKeyGeneric key, String user) {
        return authUserPass(user, "");
    }

    public authResult authUserPkey(cryKeyGeneric key, cryHashGeneric algo, String algn, byte[] chal, String user, byte[] resp) {
        return authUserPass(user, "");
    }

    public authResult authUserNone(String user) {
        return authUserPass(user, "");
    }
    
    public List<String> getShRun(String beg, int filter) {
        return new ArrayList<String>();
    }

    public void getHelp(userHelping l) {
    }

    public String getCfgName() {
        return "always";
    }

    public boolean fromString(cmds cmd) {
        return true;
    }

    public userFormat getShowSpec() {
        return null;
    }

}
