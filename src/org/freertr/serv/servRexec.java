package org.freertr.serv;

import java.util.List;
import org.freertr.auth.authGeneric;
import org.freertr.auth.authResult;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgAuther;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSetting;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServS;
import org.freertr.sec.secRlogin;
import org.freertr.user.userExec;
import org.freertr.user.userFilter;
import org.freertr.user.userFormat;
import org.freertr.user.userHelp;
import org.freertr.user.userRead;
import org.freertr.util.cmds;
import org.freertr.util.logger;

/**
 * rexec server
 *
 * @author matecsaba
 */
public class servRexec extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servRexec() {
    }

    /**
     * authentication list
     */
    public authGeneric authenticList;

    /**
     * port number
     */
    public final static int portNum = 513;

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("server rexec .*", cmds.tabulator + "port " + portNum, null),
        new userFilter("server rexec .*", cmds.tabulator + "protocol " + proto2string(protoAllStrm), null)
    };

    public userFilter[] srvDefFlt() {
        return defaultF;
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.setTime(120000);
        new servRexecConn(this, pipe);
        return false;
    }

    public void srvHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "authentication", "set authentication");
        l.add(null, false, 2, new int[]{-1}, "<name:aaa>", "name of authentication list");
    }

    public String srvName() {
        return "rexec";
    }

    public int srvPort() {
        return portNum;
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

    public void srvShRun(String beg, List<String> l, int filter) {
        if (authenticList == null) {
            l.add(beg + "no authentication");
        } else {
            l.add(beg + "authentication " + authenticList.autName);
        }
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("authentication")) {
            cfgAuther lst = cfgAll.autherFind(cmd.word(), null);
            if (lst == null) {
                cmd.error("no such auth list");
                return false;
            }
            authenticList = lst.getAuther();
            return false;
        }
        if (!s.equals(cmds.negated)) {
            return true;
        }
        s = cmd.word();
        if (s.equals("authentication")) {
            authenticList = null;
            return false;
        }
        return true;
    }

}

class servRexecConn implements Runnable {

    private final servRexec lower;

    private final pipeSide conn;

    public servRexecConn(servRexec parent, pipeSide pipe) {
        lower = parent;
        conn = pipe;
        new Thread(this).start();
    }

    public void run() {
        try {
            secRlogin.readStr(conn);
            String usr = secRlogin.readStr(conn);
            String pwd = secRlogin.readStr(conn);
            String cmd = secRlogin.readStr(conn);
            secRlogin.sendStr(conn, "");
            authResult res = lower.authenticList.authUserPass(usr, pwd);
            if (res == null) {
                conn.setClose();
                return;
            }
            if (res.result != authResult.authSuccessful) {
                conn.setClose();
                return;
            }
            conn.settingsPut(pipeSetting.authed, res);
            conn.lineTx = pipeSide.modTyp.modeCRLF;
            conn.lineRx = pipeSide.modTyp.modeCRorLF;
            userRead rdr = new userRead(conn, null);
            conn.settingsPut(pipeSetting.tabMod, userFormat.tableMode.raw);
            conn.settingsPut(pipeSetting.height, 0);
            userExec exe = new userExec(conn, rdr);
            exe.privileged = res.privilege >= 15;
            cmd = exe.repairCommand(cmd);
            exe.executeCommand(cmd);
        } catch (Exception e) {
            logger.traceback(e);
        }
        conn.setClose();
    }

}
