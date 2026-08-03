package org.freertr.serv;

import java.util.List;
import org.freertr.auth.authGeneric;
import org.freertr.auth.authLocal;
import org.freertr.auth.authResult;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgAuther;
import org.freertr.pack.packHolder;
import org.freertr.pack.packRadius;
import org.freertr.pack.packRadiusOption;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServS;
import org.freertr.tab.tabGen;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.util.cmds;
import org.freertr.util.debugger;
import org.freertr.util.logger;

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
    public final static userFilter[] defaultF = {
        new userFilter("server radius .*", cmds.tabulator + "port " + packRadius.port, null),
        new userFilter("server radius .*", cmds.tabulator + "protocol " + proto2string(protoAllDgrm), null),
        new userFilter("server radius .*", cmds.tabulator + cmds.negated + cmds.tabulator + "secret", null),
        new userFilter("server radius .*", cmds.tabulator + cmds.negated + cmds.tabulator + "logging", null),
        new userFilter("server radius .*", cmds.tabulator + "success successful", null),
        new userFilter("server radius .*", cmds.tabulator + "failure failed", null)
    };

    public userFilter[] srvDefFlt() {
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
        if (!s.equals(cmds.negated)) {
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

    public void srvHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "authentication", "set user list to use");
        l.add(null, false, 2, new int[]{-1}, "<name:aaa>", "name of list");
        l.add(null, false, 1, new int[]{2}, "secret", "set shared secret");
        l.add(null, false, 2, new int[]{-1}, "<str>", "secret");
        l.add(null, false, 1, new int[]{2}, "success", "set success message");
        l.add(null, false, 2, new int[]{-1}, "<text>", "message");
        l.add(null, false, 1, new int[]{2}, "failure", "set failure message");
        l.add(null, false, 2, new int[]{-1}, "<text>", "message");
        l.add(null, false, 1, new int[]{2}, "vendor", "set failure message");
        l.add(null, false, 2, new int[]{3}, "<num>", "vendor id");
        l.add(null, false, 3, new int[]{4}, "<num>", "vendor type");
        l.add(null, false, 4, new int[]{4, -1}, "<num>", "data byte");
        l.add(null, false, 1, new int[]{-1}, "logging", "set logging of actions");
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
        logger.startThread(this);
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
        if (radRx.code == packRadius.typeAcoReq) {
            return false;
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
            if (res.filter != null) {
                radTx.valFilter = res.filter;
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
