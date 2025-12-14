package org.freertr.serv;

import java.util.List;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServS;
import org.freertr.sec.secTelnet;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.user.userLine;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.logger;

/**
 * telnet (rfc854) server
 *
 * @author matecsaba
 */
public class servTelnet extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servTelnet() {
    }

    /**
     * port number
     */
    public final static int port = 23;

    /**
     * second port to use
     */
    protected int secondPort = -1;

    /**
     * location
     */
    public boolean location = false;

    /**
     * line handler
     */
    protected userLine lin = new userLine();

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("server telnet .*", cmds.tabulator + "port " + port, null),
        new userFilter("server telnet .*", cmds.tabulator + cmds.negated + cmds.tabulator + "second-port", null),
        new userFilter("server telnet .*", cmds.tabulator + cmds.negated + cmds.tabulator + "location", null),
        new userFilter("server telnet .*", cmds.tabulator + "protocol " + proto2string(protoAllStrm), null)
    };

    public userFilter[] srvDefFlt() {
        return defaultF;
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        if (secondPort > 0) {
            new servTelnetConn(this, pipe, id);
            return false;
        }
        if (location) {
            new servTelnetConn(this, pipe, id);
            return false;
        }
        lin.createHandler(pipe, "" + id, 0);
        return false;
    }

    public void srvShRun(String beg, List<String> l, int filter) {
        lin.getShRun(beg, l, filter);
        cmds.cfgLine(l, !location, beg, "location", "");
        cmds.cfgLine(l, secondPort < 0, beg, "second-port", "" + secondPort);
    }

    public boolean srvCfgStr(cmds cmd) {
        cmds old = cmd.copyBytes(false);
        String a = cmd.word();
        if (a.equals("location")) {
            location = true;
            return false;
        }
        if (a.equals("second-port")) {
            srvDeinit();
            secondPort = bits.str2num(cmd.word());
            srvInit();
            return false;
        }
        if (a.equals(cmds.negated)) {
            a = cmd.word();
            if (a.equals("location")) {
                location = false;
                return false;
            }
            if (a.equals("second-port")) {
                srvDeinit();
                secondPort = -1;
                srvInit();
                return false;
            }
        }
        return lin.doCfgStr(old);
    }

    public void srvHelp(userHelp l) {
        l.add(null, false, 1, new int[]{-1}, "location", "receive source in telnet location");
        l.add(null, false, 1, new int[]{2}, "second-port", "enable dual binding");
        l.add(null, false, 2, new int[]{-1}, "<num>", "secure port");
        lin.getHelp(l);
    }

    public String srvName() {
        return "telnet";
    }

    public int srvPort() {
        return port;
    }

    public int srvProto() {
        return protoAllStrm;
    }

    public boolean srvInit() {
        if (secondPort > 0) {
            if (genStrmStart(this, new pipeLine(32768, false), secondPort)) {
                return true;
            }
        }
        return genStrmStart(this, new pipeLine(32768, false), 0);
    }

    public boolean srvDeinit() {
        if (secondPort > 0) {
            if (genericStop(secondPort)) {
                return true;
            }
        }
        return genericStop(0);
    }

}

class servTelnetConn implements Runnable {

    private servTelnet lower;

    private pipeSide pipe;

    private prtGenConn id;

    public servTelnetConn(servTelnet prnt, pipeSide pip, prtGenConn sck) {
        lower = prnt;
        pipe = pip;
        id = sck;
        new Thread(this).start();
    }

    public void run() {
        try {
            String a = "" + id;
            if (lower.location) {
                String b = secTelnet.recvLocation(pipe);
                if (b != null) {
                    a = b + " via " + a;
                }
            }
            pipeSide res = null;
            if (id.portLoc == lower.secondPort) {
                res = lower.negoSecSess(pipe, servGeneric.protoSsh, new pipeLine(32768, false), lower.srvAuther);
            } else {
                res = lower.negoSecSess(pipe, servGeneric.protoTelnet, new pipeLine(32768, false), null);
            }
            if (res == null) {
                pipe.setClose();
                return;
            }
            lower.lin.createHandler(res, a, 0);
            return;
        } catch (Exception e) {
            logger.traceback(e);
            pipe.setClose();
        }
    }

}
