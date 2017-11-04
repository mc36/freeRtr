package serv;

import auth.authGeneric;
import auth.authLocal;
import auth.authResult;
import cfg.cfgAll;
import cfg.cfgAuther;
import java.util.List;
import pack.packHolder;
import pack.packRadius;
import pack.packRadiusOption;
import pipe.pipeLine;
import pipe.pipeSide;
import prt.prtGenConn;
import prt.prtServS;
import tab.tabGen;
import user.userFilter;
import user.userHelping;
import util.cmds;
import util.debugger;
import util.logger;

/**
 * remote authentication dialin user (rfc2865) server
 *
 * @author matecsaba
 */
public class servRadius extends servGeneric implements prtServS {

    /**
     * authenticator list
     */
    public authGeneric authentic;

    /**
     * shared secret
     */
    public String secret;

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
     * vendors to add
     */
    public tabGen<packRadiusOption> vendors = new tabGen<packRadiusOption>();

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "server radius .*! port " + packRadius.port,
        "server radius .*! protocol " + proto2string(protoAllDgrm),
        "server radius .*! no secret",
        "server radius .*! no logging",
        "server radius .*! success successful",
        "server radius .*! failure failed"
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
        new servRadiusConn(pipe, this);
        return false;
    }

    public void srvShRun(String beg, List<String> l) {
        cmds.cfgLine(l, authentic == null, beg, "authentication", "" + authentic);
        cmds.cfgLine(l, secret == null, beg, "secret", "" + authLocal.passwdEncode(secret));
        cmds.cfgLine(l, !logRes, beg, "logging", "");
        l.add(beg + "success " + msgSucc);
        l.add(beg + "failure " + msgFail);
        for (int o = 0; o < vendors.size(); o++) {
            l.add(beg + "vendor " + vendors.get(o));
        }
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
        if (s.equals("secret")) {
            secret = authLocal.passwdDecode(cmd.word());
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
        if (s.equals("vendor")) {
            packRadiusOption opt = new packRadiusOption();
            opt.fromString(cmd);
            vendors.add(opt);
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
        if (s.equals("secret")) {
            secret = null;
            return false;
        }
        if (s.equals("logging")) {
            logRes = false;
            return false;
        }
        if (s.equals("vendor")) {
            packRadiusOption opt = new packRadiusOption();
            opt.fromString(cmd);
            vendors.del(opt);
            return false;
        }
        return true;
    }

    public void srvHelp(userHelping l) {
        l.add("1 2  authentication               set user list to use");
        l.add("2 .    <name>                     name of list");
        l.add("1 2  secret                       set shared secret");
        l.add("2 .    <name>                     secret");
        l.add("1 2  success                      set success message");
        l.add("2 .    <text>                     message");
        l.add("1 2  failure                      set failure message");
        l.add("2 .    <text>                     message");
        l.add("1 2  vendor                       set failure message");
        l.add("2 3    <num>                      vendor id");
        l.add("3 4      <num>                    vendor type");
        l.add("4 4,.      <num>                  data byte");
        l.add("1 .  logging                      set logging of actions");
    }

    public String srvName() {
        return "radius";
    }

    public int srvPort() {
        return packRadius.port;
    }

    public int srvProto() {
        return protoAllDgrm;
    }

    public boolean srvInit() {
        if (genStrmStart(this, new pipeLine(32768, true), srvPort + 1)) {
            return true;
        }
        return genStrmStart(this, new pipeLine(32768, true), 0);
    }

    public boolean srvDeinit() {
        if (genericStop(srvPort + 1)) {
            return true;
        }
        return genericStop(0);
    }

}

class servRadiusConn implements Runnable {

    public pipeSide pipe;

    public servRadius lower;

    public servRadiusConn(pipeSide conn, servRadius parent) {
        pipe = conn;
        lower = parent;
        new Thread(this).start();
    }

    private boolean doer() {
        packHolder pckBin = pipe.readPacket(true);
        if (pckBin == null) {
            return true;
        }
        packRadius radRx = new packRadius();
        radRx.secret = lower.secret;
        if (radRx.parsePacket(pckBin)) {
            return false;
        }
        if (debugger.servRadiusTraf) {
            logger.debug("rx " + radRx.dump());
        }
        boolean b = lower.authentic != null;
        if (radRx.valUsrNam == null) {
            b = false;
        }
        if (b && (radRx.valUsrPwd != null)) {
            authResult res = lower.authentic.authUserPass(radRx.valUsrNam, radRx.valUsrPwd);
            b = res.result == authResult.authSuccessful;
        }
        if (b && (radRx.valChpPwd != null)) {
            authResult res = lower.authentic.authUserChap(radRx.valUsrNam, radRx.valChpIdn, radRx.valChpChl, radRx.valChpPwd);
            b = res.result == authResult.authSuccessful;
        }
        packRadius radTx = new packRadius();
        radTx.secret = lower.secret;
        radTx.idnt = radRx.idnt;
        radTx.auther = radRx.auther;
        if (b) {
            radTx.code = packRadius.typeAccAcc;
            radTx.valReply = lower.msgSucc;
        } else {
            radTx.code = packRadius.typeAccRej;
            radTx.valReply = lower.msgFail;
        }
        if (lower.logRes) {
            logger.info("stat=" + b + " user=" + radRx.valUsrNam);
        }
        if (debugger.servRadiusTraf) {
            logger.debug("tx " + radTx.dump());
        }
        pckBin.clear();
        radTx.createPacket(pckBin, true, lower.vendors);
        pckBin.pipeSend(pipe, 0, pckBin.dataSize(), 2);
        return false;
    }

    public void run() {
        try {
            for (;;) {
                if (doer()) {
                    break;
                }
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        pipe.setClose();
    }

}
