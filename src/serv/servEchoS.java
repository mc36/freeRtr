package serv;

import java.util.List;

import pipe.pipeConnect;
import pipe.pipeLine;
import pipe.pipeSide;
import prt.prtGenConn;
import prt.prtServS;
import tab.tabGen;
import user.userFilter;
import user.userHelping;
import util.cmds;

/**
 * echo (rfc862) server - stream mode
 *
 * @author matecsaba
 */
public class servEchoS extends servGeneric implements prtServS {

    /**
     * port number
     */
    public static final int port = 7;

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "server echo .*! port " + port,
        "server echo .*! protocol " + proto2string(protoAll)
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.timeout = 10000;
        pipeConnect.loopback(pipe);
        return false;
    }

    public void srvShRun(String beg, List<String> l) {
    }

    public boolean srvCfgStr(cmds cmd) {
        return true;
    }

    public void srvHelp(userHelping l) {
    }

    public String srvName() {
        return "echo";
    }

    public int srvPort() {
        return port;
    }

    public int srvProto() {
        return protoAll;
    }

    public boolean srvInit() {
        return genStrmStart(this, new pipeLine(32768, false), 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

}
