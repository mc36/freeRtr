package serv;

import auth.authGeneric;
import auth.authLocal;
import auth.authResult;
import cfg.cfgAll;
import cfg.cfgAuther;
import java.util.List;
import pack.packTacacs;
import pipe.pipeLine;
import pipe.pipeSide;
import prt.prtGenConn;
import prt.prtServS;
import tab.tabGen;
import user.userFilter;
import user.userHelping;
import util.bits;
import util.cmds;
import util.debugger;
import util.logger;

/**
 * terminal access controller access control system (rfc1492) server
 *
 * @author matecsaba
 */
public class servTacacs extends servGeneric implements prtServS {

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
    public final static String defaultL[] = {
        "server tacacs .*! port " + packTacacs.port,
        "server tacacs .*! protocol " + proto2string(protoAllStrm),
        "server tacacs .*! no secret",
        "server tacacs .*! no logging",
        "server tacacs .*! username user:",
        "server tacacs .*! password pass:",
        "server tacacs .*! success successful",
        "server tacacs .*! failure failed"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.timeout = 120000;
        new servTacacsConn(pipe, this);
        return false;
    }

    public void srvShRun(String beg, List<String> l) {
        cmds.cfgLine(l, authentic == null, beg, "authentication", "" + authentic);
        cmds.cfgLine(l, authorize == null, beg, "authorization", "" + authorize);
        cmds.cfgLine(l, secret == null, beg, "secret", "" + authLocal.passwdEncode(secret));
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
        if (!s.equals("no")) {
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

    public void srvHelp(userHelping l) {
        l.add("1 2  authentication               set user list to use");
        l.add("2 .    <name>                     name of list");
        l.add("1 2  authorization                set user list to use");
        l.add("2 .    <name>                     name of list");
        l.add("1 2  secret                       set shared secret");
        l.add("2 .    <name>                     secret");
        l.add("1 2  username                     set username message");
        l.add("2 .    <text>                     message");
        l.add("1 2  password                     set password message");
        l.add("2 .    <text>                     message");
        l.add("1 2  success                      set success message");
        l.add("2 .    <text>                     message");
        l.add("1 2  failure                      set failure message");
        l.add("2 .    <text>                     message");
        l.add("1 .  logging                      set logging of actions");
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
