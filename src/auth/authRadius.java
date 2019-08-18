package auth;

import clnt.clntRadius;
import java.util.ArrayList;
import java.util.List;
import user.userHelping;
import util.bits;
import util.cmds;

/**
 * radius authentication
 *
 * @author matecsaba
 */
public class authRadius extends authGeneric {

    /**
     * target server
     */
    public String server = null;

    /**
     * shared secret
     */
    public String secret;

    /**
     * default privilege
     */
    public int privilege = 15;

    public String getCfgName() {
        return "radius";
    }

    public List<String> getShRun(String beg) {
        List<String> l = new ArrayList<String>();
        cmds.cfgLine(l, secret == null, beg, "secret", authLocal.passwdEncode(secret));
        cmds.cfgLine(l, server == null, beg, "server", server);
        l.add(beg + "privilege " + privilege);
        return l;
    }

    public void getHelp(userHelping l) {
        l.add("1 2  server              specify server");
        l.add("2 .    <name>            name of server");
        l.add("1 2  secret              specify secret");
        l.add("2 .    <text>            shared secret");
        l.add("1 2  privilege           set default privilege");
        l.add("2 .    <num>             privilege of terminal");
    }

    public boolean fromString(cmds cmd) {
        String s = cmd.word();
        if (s.equals("server")) {
            server = cmd.word();
            return false;
        }
        if (s.equals("secret")) {
            secret = authLocal.passwdDecode(cmd.word());
            return false;
        }
        if (s.equals("privilege")) {
            privilege = bits.str2num(cmd.word());
            return false;
        }
        if (!s.equals("no")) {
            return true;
        }
        s = cmd.word();
        if (s.equals("server")) {
            server = null;
            return false;
        }
        if (s.equals("secret")) {
            secret = null;
            return false;
        }
        return true;
    }

    public authResult authUserChap(String user, int id, byte[] chal, byte[] resp) {
        clntRadius rad = new clntRadius();
        rad.secret = secret;
        rad.server = server;
        if (rad.doChap(user, id, chal, resp)) {
            return new authResult(this, authResult.authServerError, user);
        }
        return rad.checkResult(this, privilege);
    }

    public authResult authUserApop(String cookie, String user, String resp) {
        return new authResult(this, authResult.authServerError, user);
    }

    public authResult authUserCommand(String user, String cmd) {
        return new authResult(this, authResult.authServerError, user);
    }

    public authResult authUserPass(String user, String pass) {
        clntRadius rad = new clntRadius();
        rad.secret = secret;
        rad.server = server;
        if (rad.doPap(user, pass)) {
            return new authResult(this, authResult.authServerError, user);
        }
        return rad.checkResult(this, privilege);
    }

}
