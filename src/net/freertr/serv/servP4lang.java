package net.freertr.serv;

import java.util.List;
import net.freertr.cfg.cfgIfc;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServS;
import net.freertr.tab.tabGen;
import net.freertr.user.userFilter;
import net.freertr.user.userFormat;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;

/**
 * p4lang server
 *
 * @author matecsaba
 */
public class servP4lang extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servP4lang() {
    }

    private final servP4langCfg dp = new servP4langCfg();

    /**
     * port
     */
    public final static int port = 9080;

    /**
     * buffer size
     */
    protected int bufSiz = 65536;

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        "server p4lang .*! port " + port,
        "server p4lang .*! protocol " + proto2string(protoAllStrm),
        "server p4lang .*! buffer 65536",
        "server p4lang .*! no api-stat",
        "server p4lang .*! no export-srv6",
        "server p4lang .*! no export-copp4",
        "server p4lang .*! no export-copp6",
        "server p4lang .*! no export-socket",
        "server p4lang .*! no interconnect",
        "server p4lang .*! export-interval 1000"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public void srvShRun(String beg, List<String> l, int filter) {
        l.add(beg + "buffer " + bufSiz);
        dp.getShowRun(beg, l);
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("buffer")) {
            bufSiz = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("no")) {
            s = cmd.word();
            return dp.doUnConfig(s, cmd);
        }
        return dp.doConfig(s, cmd);
    }

    public void srvHelp(userHelping l) {
        l.add(null, "1 2  buffer                    set buffer size on connection");
        l.add(null, "2 .    <num>                   buffer in bytes");
        dp.getHelpText(l, 1);
    }

    public String srvName() {
        return "p4lang";
    }

    public int srvPort() {
        return port;
    }

    public int srvProto() {
        return protoAllStrm;
    }

    public boolean srvInit() {
        return genStrmStart(this, new pipeLine(bufSiz, false), 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        dp.conn.pipe.setClose();
        dp.notif.wakeup();
        id.timeout = 120000;
        pipe.lineRx = pipeSide.modTyp.modeCRorLF;
        pipe.lineTx = pipeSide.modTyp.modeLF;
        dp.remote = id.peerAddr.copyBytes();
        dp.minBuf = bufSiz / 2;
        dp.conn = new servP4langConn(pipe, dp);
        dp.conn.startWork();
        return false;
    }

    /**
     * get generic show
     *
     * @param mod mode: 1=generic, 2=apiTx, 3=apiRx, 4=front, 5=ifaces, 6=neighs
     * @return show
     */
    public userFormat getShow(int mod) {
        switch (mod) {
            case 1:
                return dp.getShowGen();
            case 2:
                return servP4langUtil.dumpApiStats(dp.apiStatTx);
            case 3:
                return servP4langUtil.dumpApiStats(dp.apiStatRx);
            case 4:
                return dp.getShowFront();
            case 5:
                return dp.getShowIfaces();
            case 6:
                return dp.getShowNeighs();
            default:
                return null;
        }
    }

    /**
     * get interfaces show
     *
     * @param ifc interface
     * @return show
     */
    public userFormat getShowIface(cfgIfc ifc) {
        return dp.getShowIface(ifc);
    }

    /**
     * do clear
     */
    public void doClear() {
        dp.doClear();
    }

    /**
     * send line
     *
     * @param a line
     */
    public void sendLine(String a) {
        dp.sendLine(a);;
    }

}
