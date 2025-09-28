package org.freertr.serv;

import java.util.List;
import org.freertr.auth.authGeneric;
import org.freertr.auth.authResult;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgAuther;
import org.freertr.pack.packLdap;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServS;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.util.cmds;
import org.freertr.util.debugger;
import org.freertr.util.logger;

/**
 * lightweight directory access protocol (rfc4511) server
 *
 * @author matecsaba
 */
public class servLdap extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servLdap() {
    }

    /**
     * authenticator list
     */
    public authGeneric authentic;

    /**
     * prefix
     */
    public String prefix = null;

    /**
     * suffix
     */
    public String suffix = null;

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
        new userFilter("server ldap .*", cmds.tabulator + "port " + packLdap.port, null),
        new userFilter("server ldap .*", cmds.tabulator + "protocol " + proto2string(protoAllStrm), null),
        new userFilter("server ldap .*", cmds.tabulator + cmds.negated + cmds.tabulator + "prefix", null),
        new userFilter("server ldap .*", cmds.tabulator + cmds.negated + cmds.tabulator + "suffix", null),
        new userFilter("server ldap .*", cmds.tabulator + cmds.negated + cmds.tabulator + "logging", null),
        new userFilter("server ldap .*", cmds.tabulator + "success successful", null),
        new userFilter("server ldap .*", cmds.tabulator + "failure failed", null)
    };

    public userFilter[] srvDefFlt() {
        return defaultF;
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.setTime(120000);
        new servLdapConn(pipe, this);
        return false;
    }

    public void srvShRun(String beg, List<String> l, int filter) {
        cmds.cfgLine(l, authentic == null, beg, "authentication", "" + authentic);
        cmds.cfgLine(l, prefix == null, beg, "prefix", "" + prefix);
        cmds.cfgLine(l, suffix == null, beg, "suffix", "" + suffix);
        cmds.cfgLine(l, !logRes, beg, "logging", "");
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
        if (s.equals("prefix")) {
            prefix = cmd.word();
            return false;
        }
        if (s.equals("suffix")) {
            suffix = cmd.word();
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
        if (s.equals("prefix")) {
            prefix = null;
            return false;
        }
        if (s.equals("suffix")) {
            suffix = null;
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
        l.add(null, false, 1, new int[]{2}, "prefix", "specify prefix");
        l.add(null, false, 2, new int[]{-1}, "<str>", "bind prefix");
        l.add(null, false, 1, new int[]{2}, "suffix", "specify suffix");
        l.add(null, false, 2, new int[]{-1}, "<str>", "bind suffix");
        l.add(null, false, 1, new int[]{2}, "success", "set success message");
        l.add(null, false, 2, new int[]{-1}, "<text>", "message");
        l.add(null, false, 1, new int[]{2}, "failure", "set failure message");
        l.add(null, false, 2, new int[]{-1}, "<text>", "message");
        l.add(null, false, 1, new int[]{-1}, "logging", "set logging of actions");
    }

    public String srvName() {
        return "ldap";
    }

    public int srvPort() {
        return packLdap.port;
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

class servLdapConn implements Runnable {

    public pipeSide pipe;

    public servLdap lower;

    public servLdapConn(pipeSide conn, servLdap parent) {
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
        packLdap pck = new packLdap();
        pck.pipe = pipe;
        if (pck.packRecv()) {
            return;
        }
        if (pck.parseBindReq()) {
            return;
        }
        if (debugger.servLdapTraf) {
            logger.debug("rx " + pck.dump());
        }
        authResult res = lower.authentic.authUserPass(pck.usr, pck.pwd);
        boolean b = res.result == authResult.authSuccessful;
        if (lower.logRes) {
            logger.info("stat=" + b + " user=" + pck.usr);
        }
        if (b) {
            pck.cod = packLdap.cdSucc;
            pck.usr = lower.msgSucc;
        } else {
            pck.cod = packLdap.cdCred;
            pck.usr = lower.msgFail;
        }
        pck.createBindRep();
        pck.packSend();
        if (debugger.servLdapTraf) {
            logger.debug("tx " + pck.dump());
        }
    }

}
