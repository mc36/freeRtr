package serv;

import cfg.cfgAll;
import java.util.List;
import pack.packNtp;
import pipe.pipeLine;
import pipe.pipeSide;
import prt.prtGenConn;
import prt.prtServS;
import tab.tabGen;
import user.userFilter;
import user.userHelping;
import util.bits;
import util.cmds;

/**
 * time protocol (rfc868) server
 *
 * @author matecsaba
 */
public class servTime extends servGeneric implements prtServS {

    /**
     * port number
     */
    public static final int port = 37;

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "server time .*! port " + port,
        "server time .*! protocol " + proto2string(protoAll)
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.timeout = 5000;
        new servTimeConn(pipe);
        return false;
    }

    public void srvShRun(String beg, List<String> lst) {
    }

    public boolean srvCfgStr(cmds cmd) {
        return true;
    }

    public void srvHelp(userHelping l) {
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
        new Thread(this).start();
    }

    public void run() {
        byte[] buf = new byte[4];
        bits.msbPutD(buf, 0, (int) ((bits.getTime() + cfgAll.timeServerOffset + packNtp.timDif) / 1000));
        pipe.blockingPut(buf, 0, buf.length);
        pipe.setClose();
    }

}
