package clnt;

import addr.addrIP;
import addr.addrIPv4;
import addr.addrIPv6;
import cfg.cfgIfc;
import cfg.cfgVrf;
import ip.ipFwdIface;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.Socket;
import pipe.pipeLine;
import pipe.pipeSide;
import prt.prtGen;
import prt.prtLocTcp;
import prt.prtLocUdp;
import sec.secClient;
import serv.servGeneric;
import user.userTerminal;
import util.bits;
import util.debugger;
import util.logger;
import util.version;

/**
 * proxy connection client
 *
 * @author matecsaba
 */
public class clntProxy {

    /**
     * name of this proxy
     */
    public final String name;

    /**
     * prefer ip protocol 0=default, 4=ip4, 6=ip6
     */
    public int prefer = 0;

    /**
     * target of tunnel
     */
    public String target = null;

    /**
     * port number
     */
    public int port;

    /**
     * username to use
     */
    public String username = null;

    /**
     * password to use
     */
    public String password = null;

    /**
     * vrf of target
     */
    public cfgVrf vrf = null;

    /**
     * source interface
     */
    public cfgIfc srcIfc = null;

    /**
     * lower proxy to use
     */
    public clntProxy lowProxy = null;

    /**
     * security protocol to use
     */
    public int secProto = 0;

    /**
     * proxy protocol to use
     */
    public proxyType prxProto = proxyType.local;

    /**
     * type of proxy
     */
    public enum proxyType {

        /**
         * use local stack
         */
        local,
        /**
         * use socks4 proxy
         */
        sock4,
        /**
         * use socks5 proxy
         */
        sock5,
        /**
         * use http proxy
         */
        http,
        /**
         * use host os stack
         */
        host

    }

    /**
     * convert proxy type to string
     *
     * @param i proxy type
     * @return string
     */
    public static String type2string(proxyType i) {
        switch (i) {
            case local:
                return "local";
            case sock4:
                return "socks4";
            case sock5:
                return "socks5";
            case http:
                return "http";
            case host:
                return "hostos";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * convert string to proxy type
     *
     * @param s string
     * @return proxy type
     */
    public static proxyType string2type(String s) {
        if (s.equals("local")) {
            return proxyType.local;
        }
        if (s.equals("socks4")) {
            return proxyType.sock4;
        }
        if (s.equals("socks5")) {
            return proxyType.sock5;
        }
        if (s.equals("http")) {
            return proxyType.http;
        }
        if (s.equals("hostos")) {
            return proxyType.host;
        }
        return proxyType.local;
    }

    /**
     * create client
     *
     * @param nam name of proxy config
     */
    public clntProxy(String nam) {
        name = nam;
    }

    public String toString() {
        return name;
    }

    /**
     * create new proxy config
     *
     * @param vrf vrf to use
     * @param ifc interface to use
     * @return proxy config
     */
    public static clntProxy makeTemp(cfgVrf vrf, cfgIfc ifc) {
        clntProxy res = new clntProxy("temporary");
        res.vrf = vrf;
        res.srcIfc = ifc;
        return res;
    }

    /**
     * open one connection
     *
     * @param cProto protocol to use from servGeneric
     * @param cAddr target address
     * @param cPort target port
     * @param cName client name
     * @return pipeline, null on error
     */
    public pipeSide doConnect(int cProto, addrIP cAddr, int cPort, String cName) {
        if (debugger.clntProxyTraf) {
            logger.debug("connecting to " + servGeneric.proto2string(cProto) + " " + cAddr + " " + cPort);
        }
        if (cAddr == null) {
            return null;
        }
        pipeSide pip = null;
        cProto &= servGeneric.protoTrns;
        if (prxProto == proxyType.host) {
            if (debugger.clntProxyTraf) {
                logger.debug("using host stack");
            }
            switch (cProto) {
                case servGeneric.protoTcp:
                    try {
                        Socket sck = new Socket("" + cAddr, cPort);
                        pipeLine pl = new pipeLine(65536, false);
                        prtLocTcp.doSession(pl.getSide(), sck);
                        pip = pl.getSide();
                        pip.setReady();
                    } catch (Exception e) {
                        return null;
                    }
                    break;
                case servGeneric.protoUdp:
                    try {
                        DatagramSocket sck = new DatagramSocket();
                        sck.connect(InetAddress.getByName("" + cAddr), cPort);
                        pipeLine pl = new pipeLine(65536, true);
                        prtLocUdp.doSession(pl.getSide(), sck);
                        pip = pl.getSide();
                        pip.setReady();
                    } catch (Exception e) {
                        return null;
                    }
                    break;
            }
            if (pip == null) {
                return null;
            }
            pip.timeout = 120000;
            pip = secClient.openSec(pip, secProto, username, password);
            if (pip == null) {
                return null;
            }
            pip.timeout = 180000;
            return pip;
        }
        if (vrf == null) {
            return null;
        }
        if (prxProto == proxyType.local) {
            if (debugger.clntProxyTraf) {
                logger.debug("using local stack on vrf " + vrf.name);
            }
            prtGen prt;
            ipFwdIface ipif = null;
            if (srcIfc != null) {
                ipif = srcIfc.getFwdIfc(cAddr);
            }
            pipeLine pil;
            switch (cProto) {
                case servGeneric.protoTcp:
                    prt = vrf.getTcp(cAddr);
                    pil = new pipeLine(65536, false);
                    break;
                case servGeneric.protoUdp:
                    prt = vrf.getUdp(cAddr);
                    pil = new pipeLine(65536, true);
                    break;
                case servGeneric.protoLudp:
                    prt = vrf.getLudp(cAddr);
                    pil = new pipeLine(65536, true);
                    break;
                case servGeneric.protoDccp:
                    prt = vrf.getDccp(cAddr);
                    pil = new pipeLine(65536, true);
                    break;
                case servGeneric.protoSctp:
                    prt = vrf.getSctp(cAddr);
                    pil = new pipeLine(65536, true);
                    break;
                default:
                    return null;
            }
            pip = prt.streamConnect(pil, ipif, 0, cAddr, cPort, cName, null, -1);
            if (pip == null) {
                return null;
            }
            pip.timeout = 120000;
            if (pip.wait4ready(0)) {
                pip.setClose();
                return null;
            }
            pip.timeout = 120000;
            pip = secClient.openSec(pip, secProto, username, password);
            if (pip == null) {
                return null;
            }
            pip.timeout = 180000;
            return pip;
        }
        if (cProto != servGeneric.protoTcp) {
            return null;
        }
        if (debugger.clntProxyTraf) {
            logger.debug("using " + type2string(prxProto) + " at " + target + " " + port + " on vrf " + vrf.name);
        }
        addrIP adr = userTerminal.justResolv(target, prefer);
        if (adr == null) {
            return null;
        }
        if (lowProxy != null) {
            pip = lowProxy.doConnect(servGeneric.protoTcp, adr, port, cName);
        } else {
            pip = clntProxy.makeTemp(vrf, srcIfc).doConnect(servGeneric.protoTcp, adr, port, cName);
        }
        if (pip == null) {
            return null;
        }
        pip = secClient.openSec(pip, secProto, username, password);
        if (pip == null) {
            return null;
        }
        pipeSide.modTyp rm = pip.lineRx;
        pipeSide.modTyp tm = pip.lineTx;
        pip.lineTx = pipeSide.modTyp.modeCRLF;
        pip.lineRx = pipeSide.modTyp.modeCRtryLF;
        boolean good;
        switch (prxProto) {
            case sock4:
                good = !doConSock4(pip, cAddr, cPort);
                break;
            case sock5:
                good = !doConSock5(pip, cAddr, cPort);
                break;
            case http:
                good = !doConHttp(pip, cAddr, cPort);
                break;
            default:
                good = false;
                break;
        }
        pip.lineRx = rm;
        pip.lineTx = tm;
        if (debugger.clntProxyTraf) {
            logger.debug("result=" + good);
        }
        if (!good) {
            pip.setClose();
            return null;
        }
        return pip;
    }

    private boolean doConSock4(pipeSide pip, addrIP cAddr, int cPort) {
        if (!cAddr.isIPv4()) {
            return true;
        }
        byte[] buf = new byte[8];
        buf[0] = 4; // version
        buf[1] = 1; // connect
        bits.msbPutW(buf, 2, cPort);
        cAddr.toIPv4().toBuffer(buf, 4);
        pip.blockingPut(buf, 0, buf.length);
        if (username != null) {
            pip.strPut(username);
        }
        buf = new byte[1];
        buf[0] = 0;
        pip.blockingPut(buf, 0, buf.length);
        buf = new byte[8];
        if (pip.blockingGet(buf, 0, buf.length) != buf.length) {
            return true;
        }
        if (buf[0] != 4) {
            return true;
        }
        if (buf[1] != 90) {
            return true;
        }
        return false;
    }

    private boolean doConSock5(pipeSide pip, addrIP cAddr, int cPort) {
        int i;
        if (username == null) {
            i = 0;
        } else {
            i = 2;
        }
        byte[] buf = new byte[3];
        buf[0] = 5;
        buf[1] = 1;
        buf[2] = (byte) (i & 0xff);
        pip.blockingPut(buf, 0, buf.length);
        buf = new byte[2];
        if (pip.blockingGet(buf, 0, buf.length) != buf.length) {
            return true;
        }
        switch (buf[1] & 0xff) {
            case 0:
                break;
            case 2:
                buf = new byte[2];
                buf[0] = 1;
                buf[1] = (byte) (username.length() & 0xff);
                pip.blockingPut(buf, 0, buf.length);
                pip.strPut(username);
                buf = new byte[1];
                buf[0] = (byte) (password.length() & 0xff);
                pip.blockingPut(buf, 0, buf.length);
                pip.strPut(password);
                buf = new byte[2];
                if (pip.blockingGet(buf, 0, buf.length) != buf.length) {
                    return true;
                }
                if (buf[1] != 0) {
                    return true;
                }
                break;
            default:
                return true;
        }
        buf = new byte[3];
        buf[0] = 5;
        buf[1] = 1;
        buf[2] = 0;
        pip.blockingPut(buf, 0, buf.length);
        if (cAddr.isIPv4()) {
            buf = new byte[addrIPv4.size + 1];
            buf[0] = 1;
            cAddr.toIPv4().toBuffer(buf, 1);
        } else {
            buf = new byte[addrIPv6.size + 1];
            buf[0] = 4;
            cAddr.toIPv6().toBuffer(buf, 1);
        }
        pip.blockingPut(buf, 0, buf.length);
        buf = new byte[2];
        bits.msbPutW(buf, 0, cPort);
        pip.blockingPut(buf, 0, buf.length);
        buf = new byte[4];
        if (pip.blockingGet(buf, 0, buf.length) != buf.length) {
            return true;
        }
        if (buf[1] != 0) {
            return true;
        }
        switch (buf[3]) {
            case 1:
                i = addrIPv4.size;
                break;
            case 4:
                i = addrIPv6.size;
                break;
            default:
                return true;
        }
        buf = new byte[i + 2];
        if (pip.blockingGet(buf, 0, buf.length) != buf.length) {
            return true;
        }
        return false;
    }

    private boolean doConHttp(pipeSide pip, addrIP cAddr, int cPort) {
        pip.strPut("CONNECT ");
        boolean enc = !cAddr.isIPv4();
        if (enc) {
            pip.strPut("[");
        }
        pip.strPut("" + cAddr);
        if (enc) {
            pip.strPut("]");
        }
        pip.strPut(":" + cPort);
        pip.linePut(" HTTP/1.1");
        pip.linePut("user-agent: " + version.usrAgnt);
        String s = clntHttp.getAuthor(username, password);
        if (s != null) {
            pip.linePut(s);
        }
        pip.linePut("");
        for (;;) {
            s = pip.lineGet(1);
            if (s.length() < 1) {
                break;
            }
        }
        return pip.isClosed() != 0;
    }

}
