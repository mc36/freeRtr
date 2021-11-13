package net.freertr.serv;

import java.util.List;
import net.freertr.auth.authGeneric;
import net.freertr.auth.authLocal;
import net.freertr.auth.authResult;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgAuther;
import net.freertr.pack.packHolder;
import net.freertr.pack.packRadius;
import net.freertr.pack.packRadiusOption;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServS;
import net.freertr.tab.tabGen;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
import net.freertr.util.cmds;
import net.freertr.util.debugger;
import net.freertr.util.logger;

/**
 * remote authentication dialin user (rfc2865) server
 *
 * @author matecsaba
 */
public class servRadius extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servRadius() {
    }

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
    public final static String[] defaultL = {
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
        pipe.setTime(120000);
        new servRadiusConn(pipe, this);
        return false;
    }

    public void srvShRun(String beg, List<String> l, int filter) {
        cmds.cfgLine(l, authentic == null, beg, "authentication", "" + authentic);
        cmds.cfgLine(l, secret == null, beg, "secret", "" + authLocal.passwdEncode(secret, (filter & 2) != 0));
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
        l.add(null, "1 2  authentication               set user list to use");
        l.add(null, "2 .    <name>                     name of list");
        l.add(null, "1 2  secret                       set shared secret");
        l.add(null, "2 .    <name>                     secret");
        l.add(null, "1 2  success                      set success message");
        l.add(null, "2 .    <text>                     message");
        l.add(null, "1 2  failure                      set failure message");
        l.add(null, "2 .    <text>                     message");
        l.add(null, "1 2  vendor                       set failure message");
        l.add(null, "2 3    <num>                      vendor id");
        l.add(null, "3 4      <num>                    vendor type");
        l.add(null, "4 4,.      <num>                  data byte");
        l.add(null, "1 .  logging                      set logging of actions");
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
        authResult res = null;
        if (radRx.valUsrNam == null) {
            b = false;
        }
        if (b && (radRx.valUsrPwd != null)) {
            res = lower.authentic.authUserPass(radRx.valUsrNam, radRx.valUsrPwd);
            b = res.result == authResult.authSuccessful;
        }
        if (b && (radRx.valChpPwd != null)) {
            res = lower.authentic.authUserChap(radRx.valUsrNam, radRx.valChpIdn, radRx.valChpChl, radRx.valChpPwd);
            b = res.result == authResult.authSuccessful;
        }
        packRadius radTx = new packRadius();
        radTx.secret = lower.secret;
        radTx.idnt = radRx.idnt;
        radTx.auther = radRx.auther;
        if (b) {
            radTx.code = packRadius.typeAccAcc;
            radTx.valReply = lower.msgSucc;
            if (res.privilege > 0) {
                radTx.valMgtPrv = res.privilege;
            }
            if (res.ipv4addr != null) {
                radTx.valFrmAdr4 = res.ipv4addr.copyBytes();
            }
            if (res.ipv4route != null) {
                radTx.valFrmRou4 = res.ipv4route;
            }
            if (res.ipv6addr != null) {
                radTx.valFrmAdr6 = res.ipv6addr.copyBytes();
            }
            if (res.ipv6ifid != null) {
                radTx.valFrmIfi = res.ipv6ifid.copyBytes();
            }
            if (res.ipv6route != null) {
                radTx.valFrmRou6 = res.ipv6route;
            }
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
