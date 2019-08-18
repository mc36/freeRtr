package serv;

import java.util.List;
import pack.packHolder;
import pipe.pipeSide;
import prt.prtGenConn;
import prt.prtServP;
import tab.tabGen;
import user.userFilter;
import user.userHelping;
import util.cmds;

/**
 * echo (rfc862) server - packet mode
 *
 * @author matecsaba
 */
public class servEchoP extends servGeneric implements prtServP {

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

    /**
     * close connection
     *
     * @param id connection
     */
    public void datagramClosed(prtGenConn id) {
    }

    /**
     * connection ready
     *
     * @param id connection
     */
    public void datagramReady(prtGenConn id) {
    }

    /**
     * work connection
     *
     * @param id connection
     */
    public void datagramWork(prtGenConn id) {
    }

    /**
     * received packet
     *
     * @param id connection
     * @param pck packet
     * @return false on success, true on error
     */
    public boolean datagramRecv(prtGenConn id, packHolder pck) {
        id.send2net(pck);
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
        return "echo";
    }

    public int srvPort() {
        return port;
    }

    public int srvProto() {
        return protoAll;
    }

    public boolean srvInit() {
        return genDgrmStart(this, 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        id.timeout = 10000;
        return false;
    }

}
