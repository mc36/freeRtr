package org.freertr.auth;

import java.util.ArrayList;
import java.util.List;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgProxy;
import org.freertr.clnt.clntProxy;
import org.freertr.clnt.clntTacacs;
import org.freertr.cry.cryHashGeneric;
import org.freertr.cry.cryKeyGeneric;
import org.freertr.user.userFormat;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.counter;

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
     * target port
     */
    public int port;

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
        cmds.cfgLine(l, port < 1, beg, "port", "" + port);
        cmds.cfgLine(l, secret == null, beg, "secret", authLocal.passwdEncode(secret, (filter & 2) != 0));
        cmds.cfgLine(l, server == null, beg, "server", server);
        cmds.cfgLine(l, proxy == null, beg, "proxy", "" + proxy);
        l.add(beg + "privilege " + privilege);
        return l;
    }

    public void getHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "server", "specify server");
        l.add(null, false, 2, new int[]{-1}, "<str>", "name of server");
        l.add(null, false, 1, new int[]{2}, "secret", "specify secret");
        l.add(null, false, 2, new int[]{-1}, "<text>", "shared secret");
        l.add(null, false, 1, new int[]{2}, "port", "set port number");
        l.add(null, false, 2, new int[]{-1}, "<num>", "port number");
        l.add(null, false, 1, new int[]{2}, "privilege", "set default privilege");
        l.add(null, false, 2, new int[]{-1}, "<num>", "privilege of terminal");
        l.add(null, false, 1, new int[]{2}, "proxy", "set proxy to use");
        l.add(null, false, 2, new int[]{-1}, "<name:prx>", "proxy profile");
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
        if (s.equals("port")) {
            port = bits.str2num(cmd.word());
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
        if (!s.equals(cmds.negated)) {
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
        if (s.equals("port")) {
            port = 0;
            return false;
        }
        return true;
    }

    public userFormat getShowSpec() {
        return null;
    }

    public authResult authUserChap(String user, int id, byte[] chal, byte[] resp) {
        clntTacacs tac = new clntTacacs(proxy);
        tac.port = port;
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

    public authResult authUserPkey(cryKeyGeneric key, String user) {
        return new authResult(this, authResult.authServerError, user, "");
    }

    public authResult authUserPkey(cryKeyGeneric key, cryHashGeneric algo, String algn, byte[] chal, String user, byte[] resp) {
        return new authResult(this, authResult.authServerError, user, "");
    }

    public authResult authUserNone(String user) {
        return new authResult(this, authResult.authServerError, user, "");
    }

    public authResult authUserCommand(String user, String cmd) {
        clntTacacs tac = new clntTacacs(proxy);
        tac.port = port;
        tac.secret = secret;
        tac.server = server;
        return tac.doCmd(this, user, cmd);
    }

    public authResult acntUserSession(String user, String addr, int sess, counter cntr, int stat) {
        return new authResult(this, authResult.authSuccessful, user, "");
    }

    public authResult authUserPass(String user, String pass) {
        clntTacacs tac = new clntTacacs(proxy);
        tac.port = port;
        tac.secret = secret;
        tac.server = server;
        if (tac.doPap(user, pass)) {
            return new authResult(this, authResult.authServerError, user, pass);
        }
        return tac.checkAuthenResult(this, privilege);
    }

}
