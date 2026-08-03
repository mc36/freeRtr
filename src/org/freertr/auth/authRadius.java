package org.freertr.auth;

import java.util.ArrayList;
import java.util.List;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgProxy;
import org.freertr.clnt.clntProxy;
import org.freertr.clnt.clntRadius;
import org.freertr.cry.cryHashGeneric;
import org.freertr.cry.cryKeyGeneric;
import org.freertr.user.userFormat;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.counter;

/**
 * radius authentication
 *
 * @author matecsaba
 */
public class authRadius extends authGeneric {

    /**
     * create instance
     */
    public authRadius() {
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
        return "radius";
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
        clntRadius rad = new clntRadius(proxy);
        rad.port = port;
        rad.secret = secret;
        rad.server = server;
        if (rad.doChap(user, id, chal, resp)) {
            return new authResult(this, authResult.authServerError, user, "");
        }
        return rad.checkResult(this, privilege);
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
        return new authResult(this, authResult.authServerError, user, cmd);
    }

    public authResult acntUserSession(String user, String addr, int sess, counter cntr, int stat) {
        clntRadius rad = new clntRadius(proxy);
        rad.port = port;
        rad.secret = secret;
        rad.server = server;
        rad.doAcnt(user, addr, sess, cntr, stat);
        return new authResult(this, authResult.authSuccessful, user, "");
    }

    public authResult authUserPass(String user, String pass) {
        clntRadius rad = new clntRadius(proxy);
        rad.port = port;
        rad.secret = secret;
        rad.server = server;
        if (rad.doPap(user, pass)) {
            return new authResult(this, authResult.authServerError, user, pass);
        }
        return rad.checkResult(this, privilege);
    }

}
