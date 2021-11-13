package net.freertr.serv;

import java.util.List;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServS;
import net.freertr.tab.tabGen;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
import net.freertr.user.userLine;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.logger;

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
    public static final int port = 23;

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
        "server telnet .*! port " + port,
        "server telnet .*! no second-port",
        "server telnet .*! protocol " + proto2string(protoAllStrm)
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
        if (a.equals("no")) {
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
