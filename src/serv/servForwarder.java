package serv;

import addr.addrIP;
import auth.authLocal;
import cfg.cfgAll;
import cfg.cfgIfc;
import cfg.cfgVrf;
import ip.ipFwdIface;
import java.util.List;
import pipe.pipeConnect;
import pipe.pipeLine;
import pipe.pipeSide;
import prt.prtGen;
import prt.prtGenConn;
import prt.prtServS;
import sec.secClient;
import tab.tabGen;
import user.userFilter;
import user.userHelping;
import util.bits;
import util.cmds;
import util.logger;

/**
 * port forwarder
 *
 * @author matecsaba
 */
public class servForwarder extends servGeneric implements prtServS {

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
    public int timeOut = 60 * 1000;

    /**
     * buffer size
     */
    public int bufSiz = 65536;

    /**
     * logging
     */
    public boolean logging = false;

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "server forwarder .*! port " + port,
        "server forwarder .*! protocol " + proto2string(protoAllStrm),
        "server forwarder .*! no target interface",
        "server forwarder .*! no target security",
        "server forwarder .*! no target username",
        "server forwarder .*! no target password",
        "server forwarder .*! target protocol tcp",
        "server forwarder .*! timeout 60000",
        "server forwarder .*! buffer 65536",
        "server forwarder .*! no logging",};

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public void srvShRun(String beg, List<String> l) {
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
        l.add(beg + "target protocol " + proto2string(trgProto));
        cmds.cfgLine(l, trgSecur == 0, beg, "target security", proto2string(trgSecur));
        cmds.cfgLine(l, trgUser == null, beg, "target username", trgUser);
        cmds.cfgLine(l, trgPass == null, beg, "target password", authLocal.passwdEncode(trgPass));
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
        l.add("1 .  logging                      set logging");
        l.add("1 2  timeout                      set timeout on connection");
        l.add("2 .    <num>                      timeout in ms");
        l.add("1 2  buffer                       set buffer size on connection");
        l.add("2 .    <num>                      buffer in bytes");
        l.add("1 2  target                       set session target");
        l.add("2 3    vrf                        set source vrf");
        l.add("3 .      <name>                   name of vrf");
        l.add("2 3    interface                  set source interface");
        l.add("3 .      <name>                   name of interface");
        l.add("2 3    address                    set target address");
        l.add("3 .      <addr>                   remote address");
        l.add("2 3    port                       set target port");
        l.add("3 .      <num>                    remote port");
        l.add("2 3    protocol                   set lower protocol to use");
        l.add("3 .      tcp                      use tcp transport");
        l.add("3 .      udp                      use udp transport");
        l.add("3 .      ludp                     use ludp transport");
        l.add("3 .      dccp                     use dccp transport");
        l.add("3 .      sctp                     use sctp transport");
        l.add("2 3    security                   set security protocol");
        l.add("3 .      ssh                      use secure shell");
        l.add("3 .      tls                      use transport layer security");
        l.add("3 .      dtls                     use datagram transport layer security");
        l.add("3 .      telnet                   use telnet protocol");
        l.add("2 3    username                   set username");
        l.add("3 .      <text>                   username");
        l.add("2 3    password                   set password");
        l.add("3 .      <text>                   username");
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
        pipe.timeout = timeOut;
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
        con1.timeout = timeOut;
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
        con2.timeout = timeOut;
        if (con2.wait4ready(timeOut)) {
            return true;
        }
        pipeSide con3 = secClient.openSec(con2, trgSecur, trgUser, trgPass);
        if (con3 == null) {
            con2.setClose();
            return true;
        }
        con3.timeout = timeOut;
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
