package serv;

import addr.addrIP;
import cfg.cfgAll;
import cfg.cfgIfc;
import cfg.cfgVrf;
import ip.ipFwdIface;
import java.util.List;
import pack.packHolder;
import pipe.pipeSide;
import prt.prtGenConn;
import prt.prtServP;
import prt.prtUdp;
import tab.tabGen;
import user.userFilter;
import user.userHelping;
import util.bits;
import util.cmds;
import util.counter;
import util.logger;
import util.state;

/**
 * udp forwarder server
 *
 * @author matecsaba
 */
public class servUdpFwd extends servGeneric implements prtServP {

    /**
     * port number
     */
    public static final int port = 1;

    /**
     * target vrf
     */
    public cfgVrf trgVrf;

    /**
     * target interface
     */
    public cfgIfc trgIface;

    /**
     * target address
     */
    public addrIP trgAddr = null;

    /**
     * target port
     */
    public int trgPort = port;

    /**
     * logging
     */
    public boolean logging = false;

    private prtGenConn clnt;

    private prtGenConn serv;

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        "server udpfwd .*! port " + port,
        "server udpfwd .*! protocol " + proto2string(protoAllDgrm),
        "server udpfwd .*! no target interface",
        "server udpfwd .*! no logging",};

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public void srvShRun(String beg, List<String> l, int filter) {
        cmds.cfgLine(l, !logging, beg, "logging", "");
        if (trgVrf == null) {
            l.add(beg + "no target vrf");
        } else {
            l.add(beg + "target vrf " + trgVrf.name);
        }
        if (trgIface == null) {
            l.add(beg + "no target interface");
        } else {
            l.add(beg + "target interface " + trgIface.name);
        }
        cmds.cfgLine(l, trgAddr == null, beg, "target address", "" + trgAddr);
        l.add(beg + "target port " + trgPort);
    }

    public boolean srvCfgStr(cmds cmd) {
        String a = cmd.word();
        if (a.equals("logging")) {
            logging = true;
            return false;
        }
        if (a.equals("target")) {
            a = cmd.word();
            if (a.equals("vrf")) {
                cfgVrf v = cfgAll.vrfFind(cmd.word(), false);
                if (v == null) {
                    cmd.error("no such vrf");
                    return false;
                }
                trgVrf = v;
                return false;
            }
            if (a.equals("interface")) {
                cfgIfc i = cfgAll.ifcFind(cmd.word(), false);
                if (i == null) {
                    cmd.error("no such interface");
                    return false;
                }
                trgIface = i;
                return false;
            }
            if (a.equals("address")) {
                addrIP adr = new addrIP();
                if (adr.fromString(cmd.word())) {
                    cmd.error("bad address");
                    return false;
                }
                trgAddr = adr;
                return false;
            }
            if (a.equals("port")) {
                trgPort = bits.str2num(cmd.word());
                return false;
            }
            return true;
        }
        if (!a.equals("no")) {
            return true;
        }
        a = cmd.word();
        if (a.equals("logging")) {
            logging = false;
            return false;
        }
        if (a.equals("target")) {
            a = cmd.word();
            if (a.equals("address")) {
                trgAddr = null;
                return false;
            }
            if (a.equals("vrf")) {
                trgVrf = null;
                return false;
            }
            if (a.equals("interface")) {
                trgIface = null;
                return false;
            }
            if (a.equals("port")) {
                trgPort = 0;
                return false;
            }
            return true;
        }
        return true;
    }

    public void srvHelp(userHelping l) {
        l.add("1 .  logging                      set logging");
        l.add("1 2  target                       set session target");
        l.add("2 3    vrf                        set source vrf");
        l.add("3 .      <name>                   name of vrf");
        l.add("2 3    interface                  set source interface");
        l.add("3 .      <name>                   name of interface");
        l.add("2 3    address                    set target address");
        l.add("3 .      <addr>                   remote address");
        l.add("2 3    port                       set target port");
        l.add("3 .      <num>                    remote port");
    }

    public String srvName() {
        return "udpfwd";
    }

    public int srvPort() {
        return port;
    }

    public int srvProto() {
        return protoAllDgrm;
    }

    public boolean srvInit() {
        return genDgrmStart(this, 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        if (logging) {
            logger.info("accepted " + id);
        }
        if (clnt != null) {
            clnt.setClosing();
        }
        id.timeout = 120000;
        clnt = id;
        return false;
    }

    public void datagramReady(prtGenConn id) {
    }

    public void datagramClosed(prtGenConn id) {
    }

    public void datagramWork(prtGenConn id) {
    }

    public boolean datagramError(prtGenConn id, packHolder pck, addrIP rtr, counter.reasons err, int lab) {
        return false;
    }

    /**
     * notified that state changed
     *
     * @param id id number to reference connection
     * @param stat state
     * @return return false if successful, true if error happened
     */
    public boolean datagramState(prtGenConn id, state.states stat) {
        return false;
    }

    public boolean datagramRecv(prtGenConn id, packHolder pck) {
        if (id == clnt) {
            if (serv == null) {
                if (doConn()) {
                    return false;
                }
            }
            if (serv.txBytesFree() < 0) {
                serv.setClosing();
                if (doConn()) {
                    return false;
                }
            }
            serv.send2net(pck);
            return false;
        }
        if (id == serv) {
            if (clnt == null) {
                return false;
            }
            clnt.send2net(pck);
            return false;
        }
        return true;
    }

    private boolean doConn() {
        prtUdp udp = trgVrf.getUdp(trgAddr);
        ipFwdIface fwdIfc = null;
        if (trgIface != null) {
            fwdIfc = trgIface.getFwdIfc(trgAddr);
        }
        serv = udp.packetConnect(this, fwdIfc, trgPort, trgAddr, trgPort, "udpfwd", null, -1);
        if (serv == null) {
            return true;
        }
        serv.timeout = 120000;
        return false;
    }

}
