package org.freertr.serv;

import java.util.List;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServS;
import org.freertr.tab.tabGen;
import org.freertr.user.userFilter;
import org.freertr.user.userHelping;
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
     * line handler
     */
    protected userLine lin = new userLine();

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        "server telnet .*!" + cmds.tabulator + "port " + port,
        "server telnet .*!" + cmds.tabulator + cmds.negated + cmds.tabulator + "second-port",
        "server telnet .*!" + cmds.tabulator + "protocol " + proto2string(protoAllStrm)
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        if (secondPort > 0) {
            new servTelnetConn(this, pipe, id);
        } else {
            lin.createHandler(pipe, "" + id, 0);
        }
        return false;
    }

    public void srvShRun(String beg, List<String> l, int filter) {
        cmds.cfgLine(l, secondPort < 0, beg, "second-port", "" + secondPort);
        lin.getShRun(beg, l);
    }

    public boolean srvCfgStr(cmds cmd) {
        cmds old = cmd.copyBytes(false);
        String a = cmd.word();
        if (a.equals("second-port")) {
            secondPort = bits.str2num(cmd.word());
            return false;
        }
        if (a.equals(cmds.negated)) {
            a = cmd.word();
            if (a.equals("second-port")) {
                secondPort = -1;
                return false;
            }
        }
        return lin.doCfgStr(old);
    }

    public void srvHelp(userHelping l) {
        l.add(null, "1 2  second-port                    enable dual binding");
        l.add(null, "2 .    <num>                        secure port");
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
            lower.lin.createHandler(res, "" + id, 0);
            return;
        } catch (Exception e) {
            logger.traceback(e);
            pipe.setClose();
        }
    }

}
