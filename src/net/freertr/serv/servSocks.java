package net.freertr.serv;

import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrIPv4;
import net.freertr.addr.addrIPv6;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgIfc;
import net.freertr.cfg.cfgVrf;
import net.freertr.ip.ipFwdIface;
import net.freertr.pipe.pipeConnect;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGen;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServS;
import net.freertr.tab.tabGen;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.logger;

/**
 * socks (rfc1928) server
 *
 * @author matecsaba
 */
public class servSocks extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servSocks() {
    }

    /**
     * port number
     */
    public static final int port = 1080;

    /**
     * target vrf
     */
    public cfgVrf trgVrf;

    /**
     * target interface
     */
    public cfgIfc trgIface;

    /**
     * timeout on connection
     */
    public int trgTimeout = 60 * 1000;

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        "server socks .*! port " + port,
        "server socks .*! protocol " + proto2string(protoAllStrm),
        "server socks .*! no target interface",
        "server socks .*! timeout 60000"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public void srvShRun(String beg, List<String> l, int filter) {
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
        l.add(beg + "timeout " + trgTimeout);
    }

    public boolean srvCfgStr(cmds cmd) {
        String a = cmd.word();
        if (a.equals("timeout")) {
            trgTimeout = bits.str2num(cmd.word());
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
            return true;
        }
        if (!a.equals("no")) {
            return true;
        }
        a = cmd.word();
        if (a.equals("target")) {
            a = cmd.word();
            if (a.equals("vrf")) {
                trgVrf = null;
                return false;
            }
            if (a.equals("interface")) {
                trgIface = null;
                return false;
            }
            return true;
        }
        return true;
    }

    public void srvHelp(userHelping l) {
        l.add(null, "1 2  timeout                      set timeout on connection");
        l.add(null, "2 .    <num>                      timeout in ms");
        l.add(null, "1 2  target                       set session target");
        l.add(null, "2 3    vrf                        set source vrf");
        l.add(null, "3 .      <name:vrf>               name of vrf");
        l.add(null, "2 3    interface                  set source interface");
        l.add(null, "3 .      <name:ifc>               name of interface");
    }

    public String srvName() {
        return "socks";
    }

    public int srvPort() {
        return 1080;
    }

    public int srvProto() {
        return protoAllStrm;
    }

    public boolean srvInit() {
        return genStrmStart(this, new pipeLine(65536, false), 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.setTime(10000);
        new servSocksDoer(this, pipe);
        return false;
    }

    /**
     * start connection
     *
     * @param con1 pipeline side
     * @return false on success, true on error
     */
    public boolean doConnStart(pipeSide con1) {
        con1.setTime(trgTimeout);
        con1.wait4ready(trgTimeout);
        if (trgVrf == null) {
            return true;
        }
        addrIP trgAddr = new addrIP();
        int trgPort = 0;
        addrIPv4 adr4 = new addrIPv4();
        addrIPv6 adr6 = new addrIPv6();
        byte[] buf = new byte[1];
        if (con1.moreGet(buf, 0, buf.length) != buf.length) {
            return true;
        }
        int ver = buf[0];
        switch (ver) {
            case 4:
                buf = new byte[7];
                if (con1.moreGet(buf, 0, buf.length) != buf.length) {
                    return true;
                }
                if (buf[0] != 1) {
                    return true;
                }
                trgPort = bits.msbGetW(buf, 1);
                adr4.fromBuf(buf, 3);
                for (;;) {
                    buf = new byte[1];
                    if (con1.moreGet(buf, 0, buf.length) != buf.length) {
                        return true;
                    }
                    if (buf[0] == 0) {
                        break;
                    }
                }
                trgAddr.fromIPv4addr(adr4);
                buf = new byte[8];
                buf[0] = 4; // version
                buf[1] = 90; // successful
                bits.msbPutW(buf, 2, trgPort); // port
                adr4.toBuffer(buf, 4); // address
                con1.morePut(buf, 0, buf.length);
                break;
            case 5:
                buf = new byte[1];
                if (con1.moreGet(buf, 0, buf.length) != buf.length) {
                    return true;
                }
                buf = new byte[buf[0]];
                if (con1.moreGet(buf, 0, buf.length) != buf.length) {
                    return true;
                }
                buf = new byte[2];
                buf[0] = 5; // version
                buf[1] = 0; // no authentication
                con1.morePut(buf, 0, buf.length);
                buf = new byte[4];
                if (con1.moreGet(buf, 0, buf.length) != buf.length) {
                    return true;
                }
                if (buf[0] != 5) {
                    return true;
                }
                if (buf[1] != 1) {
                    return true;
                }
                ver = buf[3];
                switch (ver) {
                    case 1:
                        buf = new byte[addrIPv4.size + 2];
                        break;
                    case 4:
                        buf = new byte[addrIPv6.size + 2];
                        break;
                }
                if (con1.moreGet(buf, 0, buf.length) != buf.length) {
                    return true;
                }
                trgPort = bits.msbGetW(buf, buf.length - 2);
                if (ver == 1) {
                    adr4.fromBuf(buf, 0);
                    trgAddr.fromIPv4addr(adr4);
                    buf = new byte[addrIPv4.size + 6];
                    adr4.toBuffer(buf, 4); // address
                    buf[3] = 1; // type
                } else {
                    adr6.fromBuf(buf, 0);
                    trgAddr.fromIPv6addr(adr6);
                    buf = new byte[addrIPv6.size + 6];
                    adr6.toBuffer(buf, 4); // address
                    buf[3] = 4; // type
                }
                bits.msbPutW(buf, buf.length - 2, trgPort); // port
                buf[0] = 5; // version
                buf[1] = 0; // result
                buf[2] = 0; // reserved
                con1.morePut(buf, 0, buf.length);
                break;
            default:
                return true;
        }
        prtGen prt = getProtocol(trgVrf, protoTcp, trgAddr);
        if (prt == null) {
            return true;
        }
        ipFwdIface ifc = null;
        if (trgIface != null) {
            ifc = trgIface.getFwdIfc(trgAddr);
        }
        pipeSide con2 = prt.streamConnect(new pipeLine(65536, true), ifc, 0, trgAddr, trgPort, srvName(), null, -1);
        con2.setTime(trgTimeout);
        con2.wait4ready(trgTimeout);
        pipeConnect.connect(con1, con2, true);
        return false;
    }

}

class servSocksDoer implements Runnable {

    private pipeSide pipe;

    private servSocks parent;

    public servSocksDoer(servSocks prnt, pipeSide stream) {
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
