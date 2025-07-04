package org.freertr.serv;

import java.util.List;
import org.freertr.cfg.cfgAll;
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
 * daytime (rfc867) server
 *
 * @author matecsaba
 */
public class servDaytime extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servDaytime() {
    }

    /**
     * port number
     */
    public final static int port = 13;

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("server daytime .*", cmds.tabulator + "port " + port, null),
        new userFilter("server daytime .*", cmds.tabulator + "protocol " + proto2string(protoAll), null)
    };

    public userFilter[] srvDefFlt() {
        return defaultF;
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.setTime(5000);
        new servDaytimeConn(pipe);
        return false;
    }

    public void srvShRun(String beg, List<String> lst, int filter) {
    }

    public boolean srvCfgStr(cmds cmd) {
        return true;
    }

    public void srvHelp(userHelp l) {
    }

    public String srvName() {
        return "daytime";
    }

    public int srvPort() {
        return port;
    }

    public int srvProto() {
        return protoAll;
    }

    public boolean srvInit() {
        return genStrmStart(this, new pipeLine(32768, true), 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

}

class servDaytimeConn implements Runnable {

    private pipeSide pipe;

    public servDaytimeConn(pipeSide conn) {
        pipe = conn;
        new Thread(this).start();
    }

    public void run() {
        pipe.strPut(bits.time2str(cfgAll.timeZoneName, bits.getTime() + cfgAll.timeServerOffset, 4));
        pipe.setClose();
    }

}
