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
import user.userLine;
import util.cmds;

/**
 * udp terminal
 *
 * @author matecsaba
 */
public class servUdptn extends servGeneric implements prtServS {

    /**
     * port number
     */
    public static final int port = 53;

    private userLine lin = new userLine();

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "server udptn .*! port " + port,
        "server udptn .*! protocol " + proto2string(protoAllDgrm)
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipeLine pip = new pipeLine(32768, false);
        pipeConnect.connect(pip.getSide(), pipe, true);
        lin.createHandler(pip.getSide(), "" + id, false);
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
        return "udptn";
    }

    public int srvPort() {
        return port;
    }

    public int srvProto() {
        return protoAllDgrm;
    }

    public boolean srvInit() {
        return genStrmStart(this, new pipeLine(32768, true), 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

}
