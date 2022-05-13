package net.freertr.serv;

import java.util.ArrayList;
import java.util.List;
import net.freertr.cfg.cfgIfc;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServS;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabIntMatcher;
import net.freertr.user.userFilter;
import net.freertr.user.userFormat;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.logger;

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
        fwds = new ArrayList<servP4langCfg>();
        fwds.add(new servP4langCfg(0));
    }

    private final List<servP4langCfg> fwds;

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
        "server p4lang .*! dataplanes 1",
        "server p4lang .*! no api-stat",
        "server p4lang .*! no export-srv6",
        "server p4lang .*! no export-copp4",
        "server p4lang .*! no export-copp6",
        "server p4lang .*! no export-socket",
        "server p4lang .*! no interconnect",
        "server p4lang .*! export-interval 1000",
        "server p4lang .*! no forwarder .* api-stat",
        "server p4lang .*! no forwarder .* export-srv6",
        "server p4lang .*! no forwarder .* export-copp4",
        "server p4lang .*! no forwarder .* export-copp6",
        "server p4lang .*! no forwarder .* export-socket",
        "server p4lang .*! no forwarder .* interconnect",
        "server p4lang .*! forwarder .* export-interval 1000"
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
        l.add(beg + "dataplanes " + fwds.size());
        if (fwds.size() <= 1) {
            fwds.get(0).getShowRun(beg, "", l);
            return;
        }
        for (int i = 0; i < fwds.size(); i++) {
            servP4langCfg cur = fwds.get(i);
            String a = "forwarder " + i + " ";
            cur.getShowRun(beg, a, l);
            cur.getShowRun2(beg, a, l);
        }
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        boolean neg = s.equals("no");
        if (neg) {
            s = cmd.word();
        }
        if (s.equals("buffer")) {
            bufSiz = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("dataplanes")) {
            int ned = bits.str2num(cmd.word());
            if (neg) {
                ned = 1;
            }
            if (ned < 1) {
                ned = 1;
            }
            for (int i = fwds.size(); i < ned; i++) {
                fwds.add(new servP4langCfg(i));
            }
            for (int i = fwds.size() - 1; i >= ned; i--) {
                servP4langCfg cur = fwds.remove(i);
                cur.doClear();
            }
            return false;
        }
        tabIntMatcher mat = new tabIntMatcher();
        mat.setExact(0);
        if (s.equals("forwarder")) {
            if (mat.fromString(cmd.word())) {
                return true;
            }
            s = cmd.word();
        }
        boolean res = false;
        for (int i = 0; i < fwds.size(); i++) {
            if (!mat.matches(i)) {
                continue;
            }
            servP4langCfg cur = fwds.get(i);
            cmds c = cmd.copyBytes(false);
            if (neg) {
                res |= cur.doUnConfig(s, c);
            } else {
                res |= cur.doConfig(s, c);
            }
        }
        return res;
    }

    public void srvHelp(userHelping l) {
        l.add(null, "1 2  buffer                    set buffer size on connection");
        l.add(null, "2 .    <num>                   buffer in bytes");
        l.add(null, "1 2  dataplanes                set number of forwarders");
        l.add(null, "2 .    <num>                   limit");
        if (fwds.size() <= 1) {
            fwds.get(0).getHelpText(l, 1);
            return;
        }
        l.add(null, "1 2  forwarder                 specify one forwarder parameters");
        l.add(null, "2 3    <num>                   forwarder number");
        l.add(null, "2 3    all                     every forwarder");
        fwds.get(0).getHelpText(l, 3);
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
        servP4langCfg cur = null;
        for (int i = 0; i < fwds.size(); i++) {
            servP4langCfg ntry = fwds.get(i);
            if (ntry.remote.compare(ntry.remote, id.peerAddr) != 0) {
                continue;
            }
            cur = ntry;
            break;
        }
        if (fwds.size() <= 1) {
            cur = fwds.get(0);
        }
        if (cur == null) {
            logger.error("dropping unknown forwarder " + id);
            return true;
        }
        cur.conn.pipe.setClose();
        cur.notif.wakeup();
        id.timeout = 120000;
        pipe.lineRx = pipeSide.modTyp.modeCRorLF;
        pipe.lineTx = pipeSide.modTyp.modeLF;
        cur.remote = id.peerAddr.copyBytes();
        cur.minBuf = bufSiz / 2;
        cur.conn = new servP4langConn(pipe, cur);
        cur.conn.startWork();
        return false;
    }

    /**
     * get generic show
     *
     * @param fwd forwarder
     * @param mod mode: 1=generic, 2=apiTx, 3=apiRx, 4=front, 5=ifaces, 6=neighs
     * @return show
     */
    public userFormat getShow(int fwd, int mod) {
        if ((fwd < 0) || (fwd >= fwds.size())) {
            return null;
        }
        servP4langCfg cur = fwds.get(fwd);
        switch (mod) {
            case 1:
                return cur.getShowGen();
            case 2:
                return servP4langUtil.dumpApiStats(cur.apiStatTx);
            case 3:
                return servP4langUtil.dumpApiStats(cur.apiStatRx);
            case 4:
                return cur.getShowFront();
            case 5:
                return cur.getShowIfaces();
            case 6:
                return cur.getShowNeighs();
            default:
                return null;
        }
    }

    /**
     * get interfaces show
     *
     * @param fwd forwarder
     * @param ifc interface
     * @return show
     */
    public userFormat getShowIface(int fwd, cfgIfc ifc) {
        if ((fwd < 0) || (fwd >= fwds.size())) {
            return null;
        }
        servP4langCfg cur = fwds.get(fwd);
        return cur.getShowIface(ifc);
    }

    /**
     * do clear
     *
     * @param fwd forwarder
     */
    public void doClear(int fwd) {
        if ((fwd < 0) || (fwd >= fwds.size())) {
            return;
        }
        servP4langCfg cur = fwds.get(fwd);
        cur.doClear();
    }

    /**
     * send line
     *
     * @param fwd forwarder
     * @param str line
     */
    public void sendLine(int fwd, String str) {
        if ((fwd < 0) || (fwd >= fwds.size())) {
            return;
        }
        servP4langCfg cur = fwds.get(fwd);
        cur.sendLine(str);
    }

}
