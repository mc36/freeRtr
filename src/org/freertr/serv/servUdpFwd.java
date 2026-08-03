package org.freertr.serv;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgVrf;
import org.freertr.ip.ipFwdIface;
import org.freertr.pack.packHolder;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServP;
import org.freertr.prt.prtUdp;
import org.freertr.tab.tabGen;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.counter;
import org.freertr.util.logger;
import org.freertr.util.state;

/**
 * udp forwarder server
 *
 * @author matecsaba
 */
public class servUdpFwd extends servGeneric implements prtServP {

    /**
     * create instance
     */
    public servUdpFwd() {
    }

    /**
     * port number
     */
    public final static int port = 2263;

    /**
     * source port
     */
    public int sourceP = 0;

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
    public final static userFilter[] defaultF = {
        new userFilter("server udpfwd .*", cmds.tabulator + "port " + port, null),
        new userFilter("server udpfwd .*", cmds.tabulator + "protocol " + proto2string(protoAllDgrm), null),
        new userFilter("server udpfwd .*", cmds.tabulator + "source port 0", null),
        new userFilter("server udpfwd .*", cmds.tabulator + cmds.negated + cmds.tabulator + "source interface", null),
        new userFilter("server udpfwd .*", cmds.tabulator + cmds.negated + cmds.tabulator + "logging", null)
    };

    public userFilter[] srvDefFlt() {
        return defaultF;
    }

    public void srvShRun(String beg, List<String> l, int filter) {
        cmds.cfgLine(l, !logging, beg, "logging", "");
        if (trgIface == null) {
            l.add(beg + "no source interface");
        } else {
            l.add(beg + "source interface " + trgIface.name);
        }
        l.add(beg + "source port " + sourceP);
        if (trgVrf == null) {
            l.add(beg + "no target vrf");
        } else {
            l.add(beg + "target vrf " + trgVrf.name);
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
        if (a.equals("source")) {
            a = cmd.word();
            if (a.equals("interface")) {
                cfgIfc i = cfgAll.ifcFind(cmd.word(), 0);
                if (i == null) {
                    cmd.error("no such interface");
                    return false;
                }
                trgIface = i;
                return false;
            }
            if (a.equals("port")) {
                sourceP = bits.str2num(cmd.word());
                return false;
            }
            return true;
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
        if (!a.equals(cmds.negated)) {
            return true;
        }
        a = cmd.word();
        if (a.equals("logging")) {
            logging = false;
            return false;
        }
        if (a.equals("source")) {
            a = cmd.word();
            if (a.equals("interface")) {
                trgIface = null;
                return false;
            }
            if (a.equals("port")) {
                sourceP = 0;
                return false;
            }
            return true;
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
            if (a.equals("port")) {
                trgPort = 0;
                return false;
            }
            return true;
        }
        return true;
    }

    public void srvHelp(userHelp l) {
        l.add(null, false, 1, new int[]{-1}, "logging", "set logging");
        l.add(null, false, 1, new int[]{2}, "source", "set session source");
        l.add(null, false, 2, new int[]{3}, "interface", "set source interface");
        l.add(null, false, 3, new int[]{-1}, "<name:ifc>", "name of interface");
        l.add(null, false, 2, new int[]{3}, "port", "set target port");
        l.add(null, false, 3, new int[]{-1}, "<num>", "remote port");
        l.add(null, false, 1, new int[]{2}, "target", "set session target");
        l.add(null, false, 2, new int[]{3}, "vrf", "set source vrf");
        l.add(null, false, 3, new int[]{-1}, "<name:vrf>", "name of vrf");
        l.add(null, false, 2, new int[]{3}, "address", "set target address");
        l.add(null, false, 3, new int[]{-1}, "<addr>", "remote address");
        l.add(null, false, 2, new int[]{3}, "port", "set target port");
        l.add(null, false, 3, new int[]{-1}, "<num>", "remote port");
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
        serv = udp.packetConnect(this, fwdIfc, sourceP, trgAddr, trgPort, "udpfwd", -1, null, -1, -1);
        if (serv == null) {
            return true;
        }
        serv.timeout = 120000;
        return false;
    }

}
