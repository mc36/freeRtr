package net.freertr.auth;

import java.util.ArrayList;
import java.util.List;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgProxy;
import net.freertr.clnt.clntProxy;
import net.freertr.clnt.clntTacacs;
import net.freertr.cry.cryHashGeneric;
import net.freertr.cry.cryKeyGeneric;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;

/**
 * tacacs authentication
 *
 * @author matecsaba
 */
public class authTacacs extends authGeneric {

    /**
     * create instance
     */
    public authTacacs() {
    }

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

    /**
     * proxy to use
     */
    public clntProxy proxy;

    public String getCfgName() {
        return "tacacs";
    }

    public List<String> getShRun(String beg, int filter) {
        List<String> l = new ArrayList<String>();
        cmds.cfgLine(l, secret == null, beg, "secret", authLocal.passwdEncode(secret, (filter & 2) != 0));
        cmds.cfgLine(l, server == null, beg, "server", server);
        cmds.cfgLine(l, proxy == null, beg, "proxy", "" + proxy);
        l.add(beg + "privilege " + privilege);
        return l;
    }

    public void getHelp(userHelping l) {
        l.add(null, "1 2  server              specify server");
        l.add(null, "2 .    <str>             name of server");
        l.add(null, "1 2  secret              specify secret");
        l.add(null, "2 .    <text>            shared secret");
        l.add(null, "1 2  privilege           set default privilege");
        l.add(null, "2 .    <num>             privilege of terminal");
        l.add(null, "1 2  proxy               set proxy to use");
        l.add(null, "2 .    <name:prx>        proxy profile");
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
        if (s.equals("proxy")) {
            cfgProxy prx = cfgAll.proxyFind(cmd.word(), false);
            if (prx == null) {
                cmd.error("no such proxy");
                return false;
            }
            proxy = prx.proxy;
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
        if (s.equals("proxy")) {
            proxy = null;
            return false;
        }
        return true;
    }

    public authResult authUserChap(String user, int id, byte[] chal, byte[] resp) {
        clntTacacs tac = new clntTacacs(proxy);
        tac.secret = secret;
        tac.server = server;
        if (tac.doChap(user, id, chal, resp)) {
            return new authResult(this, authResult.authServerError, user, "");
        }
        return tac.checkAuthenResult(this, privilege);
    }

    public authResult authUserApop(String cookie, String user, String resp) {
        return new authResult(this, authResult.authServerError, user, "");
    }

    public authResult authUserPkey(cryKeyGeneric key, cryHashGeneric algo, byte[] chal, String user, byte[] resp) {
        return new authResult(this, authResult.authServerError, user, "");
    }

    public authResult authUserCommand(String user, String cmd) {
        clntTacacs tac = new clntTacacs(proxy);
        tac.secret = secret;
        tac.server = server;
        return tac.doCmd(this, user, cmd);
    }

    public authResult authUserPass(String user, String pass) {
        clntTacacs tac = new clntTacacs(proxy);
        tac.secret = secret;
        tac.server = server;
        if (tac.doPap(user, pass)) {
            return new authResult(this, authResult.authServerError, user, pass);
        }
        return tac.checkAuthenResult(this, privilege);
    }

}
