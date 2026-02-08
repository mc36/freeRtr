package org.freertr.serv;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrIPv6;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgProxy;
import org.freertr.cfg.cfgVrf;
import org.freertr.clnt.clntProxy;
import org.freertr.ip.ipFwdIface;
import org.freertr.pipe.pipeConnect;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGen;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServS;
import org.freertr.tab.tabGen;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.logger;

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
    public final static int port = 1080;

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
     * timeout on connection
     */
    public int trgTimeout = 60 * 1000;

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
    public final static userFilter[] defaultF = {
        new userFilter("server socks .*", cmds.tabulator + "port " + port, null),
        new userFilter("server socks .*", cmds.tabulator + "protocol " + proto2string(protoAllStrm), null),
        new userFilter("server socks .*", cmds.tabulator + cmds.negated + cmds.tabulator + "target proxy", null),
        new userFilter("server socks .*", cmds.tabulator + cmds.negated + cmds.tabulator + "target vrf", null),
        new userFilter("server socks .*", cmds.tabulator + cmds.negated + cmds.tabulator + "target interface", null),
        new userFilter("server socks .*", cmds.tabulator + "timeout 60000", null),
        new userFilter("server socks .*", cmds.tabulator + "buffer 65536", null),
        new userFilter("server socks .*", cmds.tabulator + cmds.negated + cmds.tabulator + "logging", null)
    };

    public userFilter[] srvDefFlt() {
        return defaultF;
    }

    public void srvShRun(String beg, List<String> l, int filter) {
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
        l.add(beg + "timeout " + trgTimeout);
        l.add(beg + "buffer " + bufSiz);
    }

    public boolean srvCfgStr(cmds cmd) {
        String a = cmd.word();
        if (a.equals("logging")) {
            logging = true;
            return false;
        }
        if (a.equals("timeout")) {
            trgTimeout = bits.str2num(cmd.word());
            return false;
        }
        if (a.equals("buffer")) {
            bufSiz = bits.str2num(cmd.word());
            return false;
        }
        if (a.equals("target")) {
            a = cmd.word();
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
        if (a.equals("target")) {
            a = cmd.word();
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
            return true;
        }
        return true;
    }

    public void srvHelp(userHelp l) {
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
        return genStrmStart(this, new pipeLine(bufSiz, false), 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        if (logging) {
            logger.info("connection from " + id.peerAddr);
        }
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
                    default:
                        return true;
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
        if (logging) {
            logger.info("connecting to " + trgAddr + " " + trgPort);
        }
        pipeSide con2 = null;
        if (trgPrx != null) {
            con2 = trgPrx.doConnect(srvProto, trgAddr, trgPort, srvName());
        } else {
            if (trgVrf == null) {
                return true;
            }
            prtGen prt = getProtocol(trgVrf, srvProto, trgAddr);
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
        logger.startThread(this);
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
