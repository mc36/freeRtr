package org.freertr.serv;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.auth.authLocal;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgProxy;
import org.freertr.cfg.cfgVrf;
import org.freertr.clnt.clntProxy;
import org.freertr.enc.encBase64;
import org.freertr.ip.ipFwdIface;
import org.freertr.pipe.pipeConnect;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGen;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServS;
import org.freertr.sec.secClient;
import org.freertr.sec.secTelnet;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.logger;

/**
 * port forwarder
 *
 * @author matecsaba
 */
public class servForwarder extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servForwarder() {
    }

    /**
     * port number
     */
    public final static int port = 1;

    /**
     * target proxy
     */
    public clntProxy trgPrx;

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
    public int trgPort = 0;

    /**
     * target protocol
     */
    public int trgProto = protoTcp;

    /**
     * target security
     */
    public int trgSecur = 0;

    /**
     * target username
     */
    public String trgUser = null;

    /**
     * target password
     */
    public String trgPass = null;

    /**
     * timeout on connection
     */
    public int timeOut = 5 * 60 * 1000;

    /**
     * buffer size
     */
    public int bufSiz = 65536;

    /**
     * logging
     */
    public boolean logging = false;

    /**
     * location
     */
    public boolean location = false;

    /**
     * public key
     */
    public byte[] trgKey = null;

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("server forwarder .*", cmds.tabulator + "port " + port, null),
        new userFilter("server forwarder .*", cmds.tabulator + "protocol " + proto2string(protoAllStrm), null),
        new userFilter("server forwarder .*", cmds.tabulator + cmds.negated + cmds.tabulator + "target proxy", null),
        new userFilter("server forwarder .*", cmds.tabulator + cmds.negated + cmds.tabulator + "target vrf", null),
        new userFilter("server forwarder .*", cmds.tabulator + cmds.negated + cmds.tabulator + "target interface", null),
        new userFilter("server forwarder .*", cmds.tabulator + cmds.negated + cmds.tabulator + "target security", null),
        new userFilter("server forwarder .*", cmds.tabulator + cmds.negated + cmds.tabulator + "target username", null),
        new userFilter("server forwarder .*", cmds.tabulator + cmds.negated + cmds.tabulator + "target password", null),
        new userFilter("server forwarder .*", cmds.tabulator + cmds.negated + cmds.tabulator + "target pubkey", null),
        new userFilter("server forwarder .*", cmds.tabulator + "target protocol tcp", null),
        new userFilter("server forwarder .*", cmds.tabulator + "timeout 300000", null),
        new userFilter("server forwarder .*", cmds.tabulator + "buffer 65536", null),
        new userFilter("server forwarder .*", cmds.tabulator + cmds.negated + cmds.tabulator + "location", null),
        new userFilter("server forwarder .*", cmds.tabulator + cmds.negated + cmds.tabulator + "logging", null)
    };

    public userFilter[] srvDefFlt() {
        return defaultF;
    }

    public void srvShRun(String beg, List<String> l, int filter) {
        cmds.cfgLine(l, !location, beg, "location", "");
        cmds.cfgLine(l, !logging, beg, "logging", "");
        if (trgPrx == null) {
            l.add(beg + "no target proxy");
        } else {
            l.add(beg + "target proxy " + trgPrx.name);
        }
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
        if (trgKey == null) {
            l.add(beg + "no target pubkey");
        } else {
            l.add(beg + "target pubkey " + encBase64.encodeBytes(trgKey));
        }
        cmds.cfgLine(l, trgAddr == null, beg, "target address", "" + trgAddr);
        l.add(beg + "target port " + trgPort);
        l.add(beg + "target protocol " + proto2string(trgProto));
        cmds.cfgLine(l, trgSecur == 0, beg, "target security", proto2string(trgSecur));
        cmds.cfgLine(l, trgUser == null, beg, "target username", trgUser);
        cmds.cfgLine(l, trgPass == null, beg, "target password", authLocal.passwdEncode(trgPass, (filter & 2) != 0));
        l.add(beg + "timeout " + timeOut);
        l.add(beg + "buffer " + bufSiz);
    }

    public boolean srvCfgStr(cmds cmd) {
        String a = cmd.word();
        if (a.equals("location")) {
            location = true;
            return false;
        }
        if (a.equals("logging")) {
            logging = true;
            return false;
        }
        if (a.equals("timeout")) {
            timeOut = bits.str2num(cmd.word());
            return false;
        }
        if (a.equals("buffer")) {
            bufSiz = bits.str2num(cmd.word());
            return false;
        }
        if (a.equals("target")) {
            a = cmd.word();
            if (a.equals("pubkey")) {
                trgKey = encBase64.decodeBytes(cmd.getRemaining());
                return false;
            }
            if (a.equals("proxy")) {
                cfgProxy p = cfgAll.proxyFind(cmd.word(), false);
                if (p == null) {
                    cmd.error("no such proxy");
                    return false;
                }
                trgPrx = p.proxy;
                return false;
            }
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
                cfgIfc i = cfgAll.ifcFind(cmd.word(), 0);
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
            if (a.equals("protocol")) {
                trgProto = string2proto(cmd.word());
                return false;
            }
            if (a.equals("port")) {
                trgPort = bits.str2num(cmd.word());
                return false;
            }
            if (a.equals("security")) {
                trgSecur = string2proto(cmd.word());
                return false;
            }
            if (a.equals("username")) {
                trgUser = cmd.word();
                return false;
            }
            if (a.equals("password")) {
                trgPass = authLocal.passwdDecode(cmd.word());
                return false;
            }
            return true;
        }
        if (!a.equals(cmds.negated)) {
            return true;
        }
        a = cmd.word();
        if (a.equals("location")) {
            location = false;
            return false;
        }
        if (a.equals("logging")) {
            logging = false;
            return false;
        }
        if (a.equals("target")) {
            a = cmd.word();
            if (a.equals("pubkey")) {
                trgKey = null;
                return false;
            }
            if (a.equals("proxy")) {
                trgPrx = null;
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
            if (a.equals("address")) {
                trgAddr = null;
                return false;
            }
            if (a.equals("protocol")) {
                trgProto = protoTcp;
                return false;
            }
            if (a.equals("port")) {
                trgPort = 0;
                return false;
            }
            if (a.equals("security")) {
                trgSecur = 0;
                return false;
            }
            if (a.equals("username")) {
                trgUser = null;
                return false;
            }
            if (a.equals("password")) {
                trgPass = null;
                return false;
            }
            return true;
        }
        return true;
    }

    public void srvHelp(userHelp l) {
        l.add(null, false, 1, new int[]{-1}, "location", "send source in telnet location");
        l.add(null, false, 1, new int[]{-1}, "logging", "set logging");
        l.add(null, false, 1, new int[]{2}, "timeout", "set timeout on connection");
        l.add(null, false, 2, new int[]{-1}, "<num>", "timeout in ms");
        l.add(null, false, 1, new int[]{2}, "buffer", "set buffer size on connection");
        l.add(null, false, 2, new int[]{-1}, "<num>", "buffer in bytes");
        l.add(null, false, 1, new int[]{2}, "target", "set session target");
        l.add(null, false, 2, new int[]{3}, "proxy", "set proxy to use");
        l.add(null, false, 3, new int[]{-1}, "<name:prx>", "name of proxy");
        l.add(null, false, 2, new int[]{3}, "vrf", "set source vrf");
        l.add(null, false, 3, new int[]{-1}, "<name:vrf>", "name of vrf");
        l.add(null, false, 2, new int[]{3}, "interface", "set source interface");
        l.add(null, false, 3, new int[]{-1}, "<name:ifc>", "name of interface");
        l.add(null, false, 2, new int[]{3}, "pubkey", "set target public key");
        l.add(null, false, 3, new int[]{3, -1}, "<str>", "public key");
        l.add(null, false, 2, new int[]{3}, "address", "set target address");
        l.add(null, false, 3, new int[]{-1}, "<addr>", "remote address");
        l.add(null, false, 2, new int[]{3}, "port", "set target port");
        l.add(null, false, 3, new int[]{-1}, "<num>", "remote port");
        l.add(null, false, 2, new int[]{3}, "protocol", "set lower protocol to use");
        l.add(null, false, 3, new int[]{-1}, "tcp", "use tcp transport");
        l.add(null, false, 3, new int[]{-1}, "udp", "use udp transport");
        l.add(null, false, 3, new int[]{-1}, "ludp", "use ludp transport");
        l.add(null, false, 3, new int[]{-1}, "dccp", "use dccp transport");
        l.add(null, false, 3, new int[]{-1}, "sctp", "use sctp transport");
        l.add(null, false, 2, new int[]{3}, "security", "set security protocol");
        servGeneric.getSecProts(l, 3, new int[]{-1});
        l.add(null, false, 2, new int[]{3}, "username", "set username");
        l.add(null, false, 3, new int[]{-1}, "<text>", "username");
        l.add(null, false, 2, new int[]{3}, "password", "set password");
        l.add(null, false, 3, new int[]{-1}, "<text>", "username");
    }

    public String srvName() {
        return "forwarder";
    }

    public int srvPort() {
        return port;
    }

    public int srvProto() {
        return protoAllStrm;
    }

    public boolean srvInit() {
        dynBlckMod = true;
        return genStrmStart(this, new pipeLine(bufSiz, false), 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        if (logging) {
            logger.info("connection from " + id.peerAddr);
        }
        pipe.setTime(timeOut);
        new servForwarderDoer(this, pipe, id);
        return false;
    }

    /**
     * start connection
     *
     * @param con1 pipeline side
     * @param id connection
     * @return false on success, true on error
     */
    public boolean doConnStart(pipeSide con1, prtGenConn id) {
        con1.setTime(timeOut);
        if (con1.wait4ready(timeOut)) {
            return true;
        }
        pipeSide con2 = null;
        if (trgPrx != null) {
            con2 = trgPrx.doConnect(trgProto, trgAddr, trgPort, srvName());
        } else {
            if (trgVrf == null) {
                return true;
            }
            if (trgAddr == null) {
                return true;
            }
            prtGen prt = getProtocol(trgVrf, trgProto, trgAddr);
            if (prt == null) {
                return true;
            }
            ipFwdIface ifc = null;
            if (trgIface != null) {
                ifc = trgIface.getFwdIfc(trgAddr);
            }
            con2 = prt.streamConnect(new pipeLine(bufSiz, con1.isBlockMode()), ifc, 0, trgAddr, trgPort, srvName(), -1, null, -1, -1);
        }
        if (con2 == null) {
            return true;
        }
        con2.setTime(timeOut);
        if (con2.wait4ready(timeOut)) {
            return true;
        }
        pipeSide con3 = secClient.openSec(con2, trgSecur, trgKey, trgUser, trgPass);
        if (con3 == null) {
            con2.setClose();
            return true;
        }
        con3.setTime(timeOut);
        if (con3.wait4ready(timeOut)) {
            con2.setClose();
            con3.setClose();
            return true;
        }
        if (location) {
            secTelnet.sendLocation(con3, "" + id);
        }
        pipeConnect.connect(con1, con3, true);
        return false;
    }

}

class servForwarderDoer implements Runnable {

    private pipeSide pipe;

    private servForwarder parent;

    private prtGenConn conn;

    public servForwarderDoer(servForwarder prnt, pipeSide stream, prtGenConn id) {
        parent = prnt;
        pipe = stream;
        conn = id;
        logger.startThread(this);
    }

    public void run() {
        try {
            if (parent.doConnStart(pipe, conn)) {
                pipe.setClose();
            }
        } catch (Exception e) {
            pipe.setClose();
            logger.traceback(e);
        }
    }

}
