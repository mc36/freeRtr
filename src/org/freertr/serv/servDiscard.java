package org.freertr.serv;

import java.util.List;
import org.freertr.pipe.pipeDiscard;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServS;
import org.freertr.tab.tabGen;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.util.cmds;

/**
 * discard (rfc863) server
 *
 * @author matecsaba
 */
public class servDiscard extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servDiscard() {
    }

    /**
     * port number
     */
    public final static int port = 9;

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("server discard .*", cmds.tabulator + "port " + port, null),
        new userFilter("server discard .*", cmds.tabulator + "protocol " + proto2string(protoAll), null)
    };

    public userFilter[] srvDefFlt() {
        return defaultF;
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.setTime(10000);
        pipeDiscard.discard(pipe);
        return false;
    }

    public void srvShRun(String beg, List<String> l, int filter) {
    }

    public boolean srvCfgStr(cmds cmd) {
        return true;
    }

    public void srvHelp(userHelp l) {
    }

    public String srvName() {
        return "discard";
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
