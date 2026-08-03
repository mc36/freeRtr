package org.freertr.serv;

import java.util.List;
import org.freertr.cfg.cfgAll;
import org.freertr.pack.packNtp;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServS;
import org.freertr.tab.tabGen;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.logger;

/**
 * time protocol (rfc868) server
 *
 * @author matecsaba
 */
public class servTime extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servTime() {
    }

    /**
     * port number
     */
    public final static int port = 37;

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("server time .*", cmds.tabulator + "port " + port, null),
        new userFilter("server time .*", cmds.tabulator + "protocol " + proto2string(protoAll), null)
    };

    public userFilter[] srvDefFlt() {
        return defaultF;
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.setTime(5000);
        new servTimeConn(pipe);
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
        return "time";
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

class servTimeConn implements Runnable {

    private pipeSide pipe;

    public servTimeConn(pipeSide conn) {
        pipe = conn;
        logger.startThread(this);
    }

    public void run() {
        byte[] buf = new byte[4];
        bits.msbPutD(buf, 0, (int) ((bits.getTime() + cfgAll.timeServerOffset + packNtp.timDif) / 1000));
        pipe.blockingPut(buf, 0, buf.length);
        pipe.setClose();
    }

}
