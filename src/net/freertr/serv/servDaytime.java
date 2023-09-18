package net.freertr.serv;

import java.util.List;
import net.freertr.cfg.cfgAll;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServS;
import net.freertr.tab.tabGen;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;

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
    public final static String[] defaultL = {
        "server daytime .*! port " + port,
        "server daytime .*! protocol " + proto2string(protoAll)
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
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

    public void srvHelp(userHelping l) {
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
