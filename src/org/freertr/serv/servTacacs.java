package org.freertr.serv;

import java.util.List;
import org.freertr.auth.authGeneric;
import org.freertr.auth.authLocal;
import org.freertr.auth.authResult;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgAuther;
import org.freertr.pack.packTacacs;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServS;
import org.freertr.tab.tabGen;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.debugger;
import org.freertr.util.logger;

/**
 * terminal access controller access control system (rfc1492) server
 *
 * @author matecsaba
 */
public class servTacacs extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servTacacs() {
    }

    /**
     * authenticator list
     */
    public authGeneric authentic;

    /**
     * authorization list
     */
    public authGeneric authorize;

    /**
     * shared secret
     */
    public String secret;

    /**
     * username message
     */
    public String msgUser = "user:";

    /**
     * password message
     */
    public String msgPass = "pass:";

    /**
     * failure message
     */
    public String msgFail = "failed";

    /**
     * success message
     */
    public String msgSucc = "successful";

    /**
     * log results
     */
    public boolean logRes = false;

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("server tacacs .*", cmds.tabulator + "port " + packTacacs.port, null),
        new userFilter("server tacacs .*", cmds.tabulator + "protocol " + proto2string(protoAllStrm), null),
        new userFilter("server tacacs .*", cmds.tabulator + cmds.negated + cmds.tabulator + "secret", null),
        new userFilter("server tacacs .*", cmds.tabulator + cmds.negated + cmds.tabulator + "logging", null),
        new userFilter("server tacacs .*", cmds.tabulator + "username user:", null),
        new userFilter("server tacacs .*", cmds.tabulator + "password pass:", null),
        new userFilter("server tacacs .*", cmds.tabulator + "success successful", null),
        new userFilter("server tacacs .*", cmds.tabulator + "failure failed", null)
    };

    public userFilter[] srvDefFlt() {
        return defaultF;
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.setTime(120000);
        new servTacacsConn(pipe, this);
        return false;
    }

    public void srvShRun(String beg, List<String> l, int filter) {
        cmds.cfgLine(l, authentic == null, beg, "authentication", "" + authentic);
        cmds.cfgLine(l, authorize == null, beg, "authorization", "" + authorize);
        cmds.cfgLine(l, secret == null, beg, "secret", "" + authLocal.passwdEncode(secret, (filter & 2) != 0));
        cmds.cfgLine(l, !logRes, beg, "logging", "");
        l.add(beg + "username " + msgUser);
        l.add(beg + "password " + msgPass);
        l.add(beg + "success " + msgSucc);
        l.add(beg + "failure " + msgFail);
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("authentication")) {
            cfgAuther usr = cfgAll.autherFind(cmd.word(), null);
            if (usr == null) {
                cmd.error("no such user list");
                return false;
            }
            authentic = usr.getAuther();
            return false;
        }
        if (s.equals("authorization")) {
            cfgAuther usr = cfgAll.autherFind(cmd.word(), null);
            if (usr == null) {
                cmd.error("no such user list");
                return false;
            }
            authorize = usr.getAuther();
            return false;
        }
        if (s.equals("secret")) {
            secret = authLocal.passwdDecode(cmd.word());
            return false;
        }
        if (s.equals("username")) {
            msgUser = cmd.word();
            return false;
        }
        if (s.equals("password")) {
            msgPass = cmd.word();
            return false;
        }
        if (s.equals("success")) {
            msgSucc = cmd.word();
            return false;
        }
        if (s.equals("failure")) {
            msgFail = cmd.word();
            return false;
        }
        if (s.equals("logging")) {
            logRes = true;
            return false;
        }
        if (!s.equals(cmds.negated)) {
            return true;
        }
        s = cmd.word();
        if (s.equals("authentication")) {
            authentic = null;
            return false;
        }
        if (s.equals("authorization")) {
            authorize = null;
            return false;
        }
        if (s.equals("secret")) {
            secret = null;
            return false;
        }
        if (s.equals("logging")) {
            logRes = false;
            return false;
        }
        return true;
    }

    public void srvHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "authentication", "set user list to use");
        l.add(null, false, 2, new int[]{-1}, "<name:aaa>", "name of list");
        l.add(null, false, 1, new int[]{2}, "authorization", "set user list to use");
        l.add(null, false, 2, new int[]{-1}, "<name:aaa>", "name of list");
        l.add(null, false, 1, new int[]{2}, "secret", "set shared secret");
        l.add(null, false, 2, new int[]{-1}, "<str>", "secret");
        l.add(null, false, 1, new int[]{2}, "username", "set username message");
        l.add(null, false, 2, new int[]{-1}, "<text>", "message");
        l.add(null, false, 1, new int[]{2}, "password", "set password message");
        l.add(null, false, 2, new int[]{-1}, "<text>", "message");
        l.add(null, false, 1, new int[]{2}, "success", "set success message");
        l.add(null, false, 2, new int[]{-1}, "<text>", "message");
        l.add(null, false, 1, new int[]{2}, "failure", "set failure message");
        l.add(null, false, 2, new int[]{-1}, "<text>", "message");
        l.add(null, false, 1, new int[]{-1}, "logging", "set logging of actions");
    }

    public String srvName() {
        return "tacacs";
    }

    public int srvPort() {
        return packTacacs.port;
    }

    public int srvProto() {
        return protoAllStrm;
    }

    public boolean srvInit() {
        return genStrmStart(this, new pipeLine(32768, false), 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

}

class servTacacsConn implements Runnable {

    public pipeSide pipe;

    public servTacacs lower;

    public servTacacsConn(pipeSide conn, servTacacs parent) {
        pipe = conn;
        lower = parent;
        new Thread(this).start();
    }

    public void run() {
        try {
            doer();
        } catch (Exception e) {
            logger.traceback(e);
        }
        pipe.setClose();
    }

    private void doer() {
        packTacacs pck = new packTacacs();
        pck.pipe = pipe;
        pck.secret = lower.secret;
        if (pck.packRecv()) {
            return;
        }
        if (pck.parseAuthenStrt()) {
            if (pck.parseAuthorReq()) {
                return;
            }
            if (debugger.servTacacsTraf) {
                logger.debug("rx " + pck.dump());
            }
            String a = "";
            for (int i = 1; i < (pck.arg.length - 1); i++) {
                String b = pck.arg[i];
                int o = b.indexOf("=");
                if (o < 0) {
                    continue;
                }
                a += b.substring(o + 1, b.length()) + " ";
            }
            a = a.trim();
            if (lower.logRes) {
                logger.info("usr=" + pck.usr + " cmd=" + a);
            }
            authResult res = lower.authorize.authUserCommand(pck.usr, a);
            if (res.result == authResult.authSuccessful) {
                pck.srv = packTacacs.staPassAdd;
            } else {
                pck.srv = packTacacs.staFail;
            }
            pck.usr = "";
            pck.adr = "";
            pck.arg = new String[0];
            pck.createAuthorRep();
            pck.packSend();
            if (debugger.servTacacsTraf) {
                logger.debug("tx " + pck.dump());
            }
            return;
        }
        if (debugger.servTacacsTraf) {
            logger.debug("rx " + pck.dump());
        }
        if (pck.act != packTacacs.actLogin) {
            return;
        }
        authResult res;
        String s;
        switch (pck.auty) {
            case packTacacs.autyAscii:
                if (pck.usr.length() < 1) {
                    pck.act = packTacacs.sttGetUsr;
                    pck.priv = 0;
                    pck.usr = lower.msgUser;
                    pck.createAuthenRply();
                    pck.packSend();
                    if (debugger.servTacacsTraf) {
                        logger.debug("tx " + pck.dump());
                    }
                    if (pck.packRecv()) {
                        return;
                    }
                    if (pck.parseAuthenCont()) {
                        return;
                    }
                    if (debugger.servTacacsTraf) {
                        logger.debug("rx " + pck.dump());
                    }
                }
                s = "" + pck.usr;
                pck.act = packTacacs.sttGetPwd;
                pck.priv = packTacacs.flgNech;
                pck.usr = lower.msgPass;
                pck.createAuthenRply();
                pck.packSend();
                if (debugger.servTacacsTraf) {
                    logger.debug("tx " + pck.dump());
                }
                if (pck.packRecv()) {
                    return;
                }
                if (pck.parseAuthenCont()) {
                    return;
                }
                if (debugger.servTacacsTraf) {
                    logger.debug("rx " + pck.dump());
                }
                res = lower.authentic.authUserPass(s, pck.usr);
                break;
            case packTacacs.autyPap:
                s = "" + pck.usr;
                res = lower.authentic.authUserPass(s, pck.dat);
                break;
            case packTacacs.autyChap:
                s = "" + pck.usr;
                byte[] buf = pck.dat.getBytes();
                int id = buf[0] & 0xff;
                byte[] rep = new byte[16];
                byte[] chl = new byte[buf.length - rep.length - 1];
                bits.byteCopy(buf, buf.length - rep.length, rep, 0, rep.length);
                bits.byteCopy(buf, 1, chl, 0, chl.length);
                res = lower.authentic.authUserChap(s, id, chl, rep);
                break;
            default:
                return;
        }
        boolean b = res.result == authResult.authSuccessful;
        if (lower.logRes) {
            logger.info("stat=" + b + " user=" + s);
        }
        if (b) {
            pck.act = packTacacs.sttPass;
            pck.usr = lower.msgSucc;
        } else {
            pck.act = packTacacs.sttFail;
            pck.usr = lower.msgFail;
        }
        pck.priv = 0;
        pck.dat = "";
        pck.createAuthenRply();
        pck.packSend();
        if (debugger.servTacacsTraf) {
            logger.debug("tx " + pck.dump());
        }
    }

}
