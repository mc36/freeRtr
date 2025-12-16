package org.freertr.clnt;

import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.Socket;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrIPv6;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgInit;
import org.freertr.cfg.cfgVrf;
import org.freertr.enc.encUrl;
import org.freertr.ip.ipFwdIface;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGen;
import org.freertr.prt.prtLocTcp;
import org.freertr.prt.prtLocUdp;
import org.freertr.sec.secClient;
import org.freertr.sec.secWebsock;
import org.freertr.serv.servGeneric;
import org.freertr.util.bits;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.syncInt;

/**
 * proxy connection client
 *
 * @author matecsaba
 */
public class clntProxy {

    /**
     * startup counter
     */
    public final static syncInt cntrStart = new syncInt(0);

    /**
     * error counter
     */
    public final static syncInt cntrError = new syncInt(0);

    /**
     * stop counter
     */
    public final static syncInt cntrStop = new syncInt(0);

    /**
     * name of this proxy
     */
    public String name;

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
     * public key
     */
    public byte[] pubkey;

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
     * ttl
     */
    public int tim2liv = -1;

    /**
     * tos
     */
    public int typOsrv = -1;

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
         * use websock proxy
         */
        websock,
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
            case websock:
                return "websock";
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
        if (s.equals("websock")) {
            return proxyType.websock;
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
        cntrStart.add(1);
        if (debugger.clntProxyTraf) {
            logger.debug("connecting to " + servGeneric.proto2string(cProto) + " " + cAddr + " " + cPort);
        }
        if (cAddr == null) {
            cntrError.add(1);
            return null;
        }
        pipeSide pip = null;
        cProto &= servGeneric.protoTrns;
        clntProxy curPrx = lowProxy;
        if (curPrx == null) {
            curPrx = clntProxy.makeTemp(vrf, srcIfc);
        }
        if (prxProto == proxyType.websock) {
            encUrl url = new encUrl();
            url.fromString("http://" + target + "/");
            url.port = port;
            url.server = target;
            if (debugger.clntProxyTraf) {
                logger.debug("using websock " + url.dump());
            }
            pip = secWebsock.doConnect(curPrx, pubkey, url, "binary");
            if (pip == null) {
                cntrError.add(1);
                return null;
            }
            secWebsock ws = new secWebsock(pip, new pipeLine(65536, false));
            ws.startClient();
            pip = ws.getPipe();
            cntrStop.add(1);
            return pip;
        }
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
                    cntrError.add(1);
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
                    cntrError.add(1);
                    return null;
                }
                break;
            }
            if (pip == null) {
                cntrError.add(1);
                return null;
            }
            pip.setTime(120000);
            pip = secClient.openSec(pip, secProto, pubkey, username, password);
            if (pip == null) {
                cntrError.add(1);
                return null;
            }
            pip.setTime(180000);
            cntrStop.add(1);
            return pip;
        }
        if (vrf == null) {
            cntrError.add(1);
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
                    cntrError.add(1);
                    return null;
            }
            pip = prt.streamConnect(pil, ipif, 0, cAddr, cPort, cName, -1, null, tim2liv, typOsrv);
            if (pip == null) {
                cntrError.add(1);
                return null;
            }
            pip.setTime(120000);
            if (pip.wait4ready(120000)) {
                pip.setClose();
                cntrError.add(1);
                return null;
            }
            pip = secClient.openSec(pip, secProto, pubkey, username, password);
            if (pip == null) {
                cntrError.add(1);
                return null;
            }
            pip.setTime(180000);
            cntrStop.add(1);
            return pip;
        }
        if (cProto != servGeneric.protoTcp) {
            cntrError.add(1);
            return null;
        }
        if (debugger.clntProxyTraf) {
            logger.debug("using " + type2string(prxProto) + " at " + target + " " + port + " on vrf " + vrf.name);
        }
        addrIP adr = clntDns.justResolv(target, prefer);
        if (adr == null) {
            cntrError.add(1);
            return null;
        }
        pip = curPrx.doConnect(servGeneric.protoTcp, adr, port, cName);
        if (pip == null) {
            cntrError.add(1);
            return null;
        }
        pip = secClient.openSec(pip, secProto, pubkey, username, password);
        if (pip == null) {
            cntrError.add(1);
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
        if (good) {
            cntrStop.add(1);
            return pip;
        }
        cntrError.add(1);
        pip.setClose();
        return null;
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
        pip.linePut("User-Agent: " + cfgInit.versionAgent);
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
