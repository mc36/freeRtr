package serv;

import java.util.List;
import pipe.pipeLine;
import pipe.pipeSide;
import prt.prtGenConn;
import prt.prtServS;
import tab.tabGen;
import user.userFilter;
import user.userHelping;
import user.userLine;
import util.cmds;

/**
 * telnet (rfc854) server
 *
 * @author matecsaba
 */
public class servTelnet extends servGeneric implements prtServS {

    /**
     * port number
     */
    public static final int port = 23;

    private userLine lin = new userLine();

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "server telnet .*! port " + port,
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
        lin.createHandler(pipe, "" + id, false);
        return false;
    }

    public void srvShRun(String beg, List<String> l) {
        lin.getShRun(beg, l);
    }

    public boolean srvCfgStr(cmds cmd) {
        return lin.doCfgStr(cmd);
    }

    public void srvHelp(userHelping l) {
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
        return genStrmStart(this, new pipeLine(32768, false), 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

}
