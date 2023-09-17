package net.freertr.serv;

import java.util.List;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServS;
import net.freertr.tab.tabGen;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
import net.freertr.util.cmds;

/**
 * honeypot server
 *
 * @author matecsaba
 */
public class servHoneyPot extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servHoneyPot() {
    }

    /**
     * port number
     */
    public static final int port = 22;

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        "server honeypot .*! port " + port,
        "server honeypot .*! protocol " + proto2string(protoAllStrm),
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    /**
     * ip information configuration
     */
    public servHoneyPotCfg ipInfo = new servHoneyPotCfg();

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public String srvName() {
        return "honeypot";
    }

    public int srvPort() {
        return port;
    }

    public int srvProto() {
        return protoAllStrm;
    }

    public boolean srvInit() {
        return genStrmStart(this, new pipeLine(128 * 1024, false), 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

    public void srvShRun(String beg, List<String> lst, int filter) {
        ipInfo.doGetCfg(beg, lst, true);
    }

    public boolean srvCfgStr(cmds cmd) {
        boolean neg = cmd.word().equals("no");
        if (!neg) {
            cmd = cmd.copyBytes(true);
        }
        return ipInfo.doCfgStr(cmd, neg);
    }

    public void srvHelp(userHelping l) {
        servHoneyPotCfg.getHelp(l, 0, true);
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.setTime(60000);
        new servHoneyPotCon(this, pipe, id.peerAddr.copyBytes(), id.portRem);
        return false;
    }

}
