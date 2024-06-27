package org.freertr.serv;

import java.util.List;
import org.freertr.pipe.pipeConnect;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServS;
import org.freertr.tab.tabGen;
import org.freertr.user.userFilter;
import org.freertr.user.userHelping;
import org.freertr.user.userLine;
import org.freertr.util.cmds;

/**
 * udp terminal
 *
 * @author matecsaba
 */
public class servUdptn extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servUdptn() {
    }

    /**
     * port number
     */
    public final static int port = 23;

    private userLine lin = new userLine();

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        "server udptn .*!" + cmds.tabulator + "port " + port,
        "server udptn .*!" + cmds.tabulator + "protocol " + proto2string(protoAllDgrm)
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
        lin.createHandler(pip.getSide(), "" + id, 0);
        return false;
    }

    public void srvShRun(String beg, List<String> l, int filter) {
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
