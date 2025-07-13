package org.freertr.serv;

import java.util.List;
import org.freertr.pipe.pipeConnect;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServS;
import org.freertr.tab.tabGen;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;

/**
 * echo (rfc862) server - stream mode
 *
 * @author matecsaba
 */
public class servEchoS extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servEchoS() {
    }

    /**
     * port number
     */
    public final static int port = 7;

    /**
     * delay
     */
    public int delay = 0;

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("server echo .*", cmds.tabulator + "port " + port, null),
        new userFilter("server echo .*", cmds.tabulator + "protocol " + proto2string(protoAll), null),
        new userFilter("server echo .*", cmds.tabulator + "delay 0", null)
    };

    public userFilter[] srvDefFlt() {
        return defaultF;
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.setTime(10000);
        pipeConnect.loopback(pipe, delay);
        return false;
    }

    public void srvShRun(String beg, List<String> l, int filter) {
        l.add(beg + "delay " + delay);
    }

    public boolean srvCfgStr(cmds cmd) {
        String a = cmd.word();
        if (a.equals("delay")) {
            delay = bits.str2num(cmd.word());
            return false;
        }
        if (!a.equals(cmds.negated)) {
            return true;
        }
        a = cmd.word();
        if (a.equals("delay")) {
            delay = 0;
            return false;
        }
        return true;
    }

    public void srvHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "delay", "reply delay");
        l.add(null, false, 2, new int[]{-1}, "<num>", "time in ms");
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
