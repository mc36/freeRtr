package auth;

import java.util.ArrayList;
import java.util.List;
import user.userHelping;
import util.cmds;

/**
 * authentication that always succeeds
 *
 * @author matecsaba
 */
public class authConstant extends authGeneric {

    private boolean res;

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
            return new authResult(this, authResult.authSuccessful, user);
        } else {
            return new authResult(this, authResult.authBadUserPass, user);
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

    public List<String> getShRun(String beg) {
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

}
