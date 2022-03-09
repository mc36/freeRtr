package net.freertr.serv;

import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.auth.authLocal;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgIfc;
import net.freertr.cfg.cfgVrf;
import net.freertr.cry.cryBase64;
import net.freertr.ip.ipFwdIface;
import net.freertr.pipe.pipeConnect;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGen;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServS;
import net.freertr.sec.secClient;
import net.freertr.tab.tabGen;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.logger;

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
     * public key
     */
    public String trgKey = null;

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        "server forwarder .*! port " + port,
        "server forwarder .*! protocol " + proto2string(protoAllStrm),
        "server forwarder .*! no target interface",
        "server forwarder .*! no target security",
        "server forwarder .*! no target username",
        "server forwarder .*! no target password",
        "server forwarder .*! no target pubkey",
        "server forwarder .*! target protocol tcp",
        "server forwarder .*! timeout 300000",
        "server forwarder .*! buffer 65536",
        "server forwarder .*! no logging",};

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
        cmds.cfgLine(l, trgKey == null, beg, "target pubkey", "" + trgKey);
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
                trgKey = cmd.getRemaining();
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
            if (a.equals("pubkey")) {
                trgKey = null;
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

    public void srvHelp(userHelping l) {
        l.add(null, "1 .  logging                      set logging");
        l.add(null, "1 2  timeout                      set timeout on connection");
        l.add(null, "2 .    <num>                      timeout in ms");
        l.add(null, "1 2  buffer                       set buffer size on connection");
        l.add(null, "2 .    <num>                      buffer in bytes");
        l.add(null, "1 2  target                       set session target");
        l.add(null, "2 3    vrf                        set source vrf");
        l.add(null, "3 .      <name:vrf>               name of vrf");
        l.add(null, "2 3    interface                  set source interface");
        l.add(null, "3 .      <name:ifc>               name of interface");
        l.add(null, "2 3    pubkey                     set target public key");
        l.add(null, "3 3,.    <str>                    public key");
        l.add(null, "2 3    address                    set target address");
        l.add(null, "3 .      <addr>                   remote address");
        l.add(null, "2 3    port                       set target port");
        l.add(null, "3 .      <num>                    remote port");
        l.add(null, "2 3    protocol                   set lower protocol to use");
        l.add(null, "3 .      tcp                      use tcp transport");
        l.add(null, "3 .      udp                      use udp transport");
        l.add(null, "3 .      ludp                     use ludp transport");
        l.add(null, "3 .      dccp                     use dccp transport");
        l.add(null, "3 .      sctp                     use sctp transport");
        l.add(null, "2 3    security                   set security protocol");
        l.add(null, "3 .      ssh                      use secure shell");
        l.add(null, "3 .      tls                      use transport layer security");
        l.add(null, "3 .      dtls                     use datagram transport layer security");
        l.add(null, "3 .      telnet                   use telnet protocol");
        l.add(null, "2 3    username                   set username");
        l.add(null, "3 .      <text>                   username");
        l.add(null, "2 3    password                   set password");
        l.add(null, "3 .      <text>                   username");
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
        new servForwarderDoer(this, pipe);
        return false;
    }

    /**
     * start connection
     *
     * @param con1 pipeline side
     * @return false on success, true on error
     */
    public boolean doConnStart(pipeSide con1) {
        con1.setTime(timeOut);
        if (con1.wait4ready(timeOut)) {
            return true;
        }
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
        pipeSide con2 = prt.streamConnect(new pipeLine(bufSiz, con1.isBlockMode()), ifc, 0, trgAddr, trgPort, srvName(), null, -1);
        if (con2 == null) {
            return true;
        }
        con2.setTime(timeOut);
        if (con2.wait4ready(timeOut)) {
            return true;
        }
        byte[] pkey = null;
        if (trgKey != null) {
            pkey = cryBase64.decodeBytes(trgKey);
        }
        pipeSide con3 = secClient.openSec(con2, trgSecur, null, trgUser, trgPass);
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
        pipeConnect.connect(con1, con3, true);
        return false;

    }

}

class servForwarderDoer implements Runnable {

    private pipeSide pipe;

    private servForwarder parent;

    public servForwarderDoer(servForwarder prnt, pipeSide stream) {
        parent = prnt;
        pipe = stream;
        new Thread(this).start();
    }

    public void run() {
        try {
            if (parent.doConnStart(pipe)) {
                pipe.setClose();
            }
        } catch (Exception e) {
            pipe.setClose();
            logger.traceback(e);
        }
    }

}
