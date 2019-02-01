package serv;

import addr.addrIP;
import auth.authGeneric;
import cfg.cfgAceslst;
import cfg.cfgAll;
import cfg.cfgAuther;
import cfg.cfgCert;
import cfg.cfgIfc;
import cfg.cfgKey;
import cfg.cfgPrfxlst;
import cfg.cfgRoump;
import cfg.cfgRouplc;
import cfg.cfgVrf;
import cry.cryCertificate;
import cry.cryKeyDSA;
import cry.cryKeyECDSA;
import cry.cryKeyRSA;
import ip.ipFwd;
import ip.ipFwdIface;
import ip.ipPrt;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import pack.packHolder;
import pipe.pipeLine;
import pipe.pipeSide;
import prt.prtGen;
import prt.prtGenConn;
import prt.prtServP;
import prt.prtServS;
import sec.secServer;
import tab.tabAceslstN;
import tab.tabGen;
import tab.tabListing;
import tab.tabPlcmapN;
import tab.tabPrfxlstN;
import tab.tabRouteEntry;
import tab.tabRtrmapN;
import tab.tabRtrplc;
import tab.tabRtrplcN;
import user.userFilter;
import user.userHelping;
import util.bits;
import util.cmds;
import util.logger;

/**
 * generic server implementation
 *
 * @author matecsaba
 */
public abstract class servGeneric implements Comparator<servGeneric> {

    /**
     * name of server
     */
    public String srvName;

    /**
     * port number
     */
    protected int srvPort;

    /**
     * protocol list
     */
    protected int srvProto;

    /**
     * vrf to use
     */
    protected cfgVrf srvVrf;

    /**
     * interface to use
     */
    protected cfgIfc srvIface;

    /**
     * access list to use
     */
    protected tabListing<tabAceslstN<addrIP>, addrIP> srvAccess;

    /**
     * access prefix list
     */
    public tabListing<tabPrfxlstN, addrIP> srvPrfLst;

    /**
     * access route map
     */
    public tabListing<tabRtrmapN, addrIP> srvRouMap;

    /**
     * access route policy
     */
    public tabListing<tabRtrplcN, addrIP> srvRouPol;

    /**
     * accesses per interval
     */
    public int srvAccRat;

    /**
     * the interval
     */
    public int srvAccInt;

    /**
     * last access
     */
    protected long srvAccLst;

    /**
     * accesses per interval
     */
    public int srvAccCnt;

    /**
     * limit of all clients
     */
    protected int srvTotLim;

    /**
     * limit of one client
     */
    protected int srvPerLim;

    /**
     * log access drops
     */
    protected boolean srvLogDrop;

    /**
     * sample pipeline to use
     */
    protected pipeLine pipeSample;

    /**
     * authenticator to use
     */
    protected authGeneric srvAuther;

    /**
     * rsa key to use
     */
    protected cryKeyRSA keyrsa;

    /**
     * dsa key to use
     */
    protected cryKeyDSA keydsa;

    /**
     * ecdsa key to use
     */
    protected cryKeyECDSA keyecdsa;

    /**
     * rsa certificate to use
     */
    protected cryCertificate certrsa;

    /**
     * dsa certificate to use
     */
    protected cryCertificate certdsa;

    /**
     * ecdsa certificate to use
     */
    protected cryCertificate certecdsa;

    /**
     * security protocol to use
     */
    protected int secProto = 0;

    /**
     * dynamic block mode on accept
     */
    protected boolean dynBlckMod = false;

    /**
     * use ip4
     */
    public final static int protoIp4 = 0x01;

    /**
     * use ip6
     */
    public final static int protoIp6 = 0x02;

    /**
     * use ludp
     */
    public final static int protoLudp = 0x08;

    /**
     * use tcp
     */
    public final static int protoTcp = 0x10;

    /**
     * use udp
     */
    public final static int protoUdp = 0x20;

    /**
     * use dccp
     */
    public final static int protoDccp = 0x40;

    /**
     * use sctp
     */
    public final static int protoSctp = 0x80;

    /**
     * use ssh
     */
    public final static int protoSsh = 0x100;

    /**
     * use tls
     */
    public final static int protoTls = 0x200;

    /**
     * use dtls
     */
    public final static int protoDtls = 0x400;

    /**
     * use telnet
     */
    public final static int protoTelnet = 0x800;

    /**
     * use all networks
     */
    public final static int protoNets = protoIp4 | protoIp6;

    /**
     * use all transport
     */
    public final static int protoTrns = protoTcp | protoUdp | protoLudp | protoDccp | protoSctp;

    /**
     * use all security
     */
    public final static int protoSec = protoSsh | protoTls | protoDtls | protoTelnet;

    /**
     * use all stream
     */
    public final static int protoAllStrm = protoNets | protoTcp;

    /**
     * use all datagram
     */
    public final static int protoAllDgrm = protoNets | protoUdp | protoLudp | protoDccp | protoSctp;

    /**
     * use everything
     */
    public final static int protoAll = protoNets | protoTrns;

    /**
     * defaults text
     */
    public final static String[] srvdefsL = {
        // generic server
        "server .*! no security protocol",
        "server .*! no security authentication",
        "server .*! no security rsakey",
        "server .*! no security dsakey",
        "server .*! no security ecdsakey",
        "server .*! no security rsacert",
        "server .*! no security dsacert",
        "server .*! no security ecdsacert",
        "server .*! no access-class",
        "server .*! no access-prefix",
        "server .*! no access-map",
        "server .*! no access-policy",
        "server .*! no access-rate",
        "server .*! access-total 0",
        "server .*! access-peer 0",
        "server .*! no access-log",
        "server .*! no interface",
        "server .*! no vrf"
    };

    /**
     * set name of instance
     *
     * @param s name of server
     */
    public void rename(String s) {
        srvName = s;
    }

    /**
     * get running configuration
     *
     * @param beg beginning string
     * @param lst list to append
     */
    public abstract void srvShRun(String beg, List<String> lst);

    /**
     * get default filter
     *
     * @return default filter
     */
    public abstract tabGen<userFilter> srvDefFlt();

    /**
     * parse commands
     *
     * @param cmd commands
     * @return true if error happened
     */
    public abstract boolean srvCfgStr(cmds cmd);

    /**
     * get help text
     *
     * @param l list of commands
     */
    public abstract void srvHelp(userHelping l);

    /**
     * get name of server
     *
     * @return name of server
     */
    public abstract String srvName();

    /**
     * get port of server
     *
     * @return port of server
     */
    public abstract int srvPort();

    /**
     * get protocols of server
     *
     * @return protocols of server
     */
    public abstract int srvProto();

    /**
     * bind this server instance
     *
     * @return false if successful, true if error happened
     */
    public abstract boolean srvInit();

    /**
     * unbind this server instance
     *
     * @return false if successful, true if error happened
     */
    public abstract boolean srvDeinit();

    /**
     * accept connection to this server instance
     *
     * @param pipe pipeline just accepted
     * @param id connection just accepted
     * @return false if successful, true if error happened
     */
    public abstract boolean srvAccept(pipeSide pipe, prtGenConn id);

    /**
     * convert protocol number to string
     *
     * @param i number to convert
     * @return readable string
     */
    public static String proto2string(int i) {
        String a = "";
        if ((i & protoIp4) != 0) {
            a += " ipv4";
        }
        if ((i & protoIp6) != 0) {
            a += " ipv6";
        }
        if ((i & protoTcp) != 0) {
            a += " tcp";
        }
        if ((i & protoUdp) != 0) {
            a += " udp";
        }
        if ((i & protoLudp) != 0) {
            a += " ludp";
        }
        if ((i & protoDccp) != 0) {
            a += " dccp";
        }
        if ((i & protoSctp) != 0) {
            a += " sctp";
        }
        if ((i & protoSsh) != 0) {
            a += " ssh";
        }
        if ((i & protoTls) != 0) {
            a += " tls";
        }
        if ((i & protoDtls) != 0) {
            a += " dtls";
        }
        if ((i & protoTelnet) != 0) {
            a += " telnet";
        }
        return a.trim();
    }

    /**
     * convert string to protocol number
     *
     * @param a string to convert
     * @return protocol number
     */
    public static int string2proto(String a) {
        if (a.equals("ipv4")) {
            return protoIp4;
        }
        if (a.equals("ipv6")) {
            return protoIp6;
        }
        if (a.equals("tcp")) {
            return protoTcp;
        }
        if (a.equals("udp")) {
            return protoUdp;
        }
        if (a.equals("ludp")) {
            return protoLudp;
        }
        if (a.equals("dccp")) {
            return protoDccp;
        }
        if (a.equals("sctp")) {
            return protoSctp;
        }
        if (a.equals("ssh")) {
            return protoSsh;
        }
        if (a.equals("tls")) {
            return protoTls;
        }
        if (a.equals("dtls")) {
            return protoDtls;
        }
        if (a.equals("telnet")) {
            return protoTelnet;
        }
        return 0;
    }

    /**
     * initialize variables
     */
    public void srvInitialize() {
        srvPort = srvPort();
        srvProto = srvProto();
    }

    /**
     * get forwarder protocol
     *
     * @param vrf vrf to use
     * @param prt protocol to use
     * @param adr address to test
     * @return forwarder, null if error happened
     */
    public static prtGen getProtocol(cfgVrf vrf, int prt, addrIP adr) {
        switch (prt & protoTrns) {
            case protoTcp:
                return vrf.getTcp(adr);
            case protoUdp:
                return vrf.getUdp(adr);
            case protoLudp:
                return vrf.getLudp(adr);
            case protoDccp:
                return vrf.getDccp(adr);
            case protoSctp:
                return vrf.getSctp(adr);
            default:
                return null;
        }
    }

    /**
     * check keys
     *
     * @return true if no, false if have all
     */
    public boolean noneSecKeys() {
        return (keydsa == null) || (keyecdsa == null) || (keyrsa == null) || (certdsa == null) || (certecdsa == null) || (certrsa == null);
    }

    /**
     * negotiate secure session
     *
     * @param pipe cleartext pipeline
     * @param proto protocol to use
     * @param sample pipeline to clone
     * @param auther authenticator
     * @return encrypted pipeline, null if error
     */
    public pipeSide negoSecSess(pipeSide pipe, int proto, pipeLine sample, authGeneric auther) {
        return secServer.openSec(pipe, proto, sample, auther, keyrsa, keydsa, keyecdsa, certrsa, certdsa, certecdsa);
    }

    /**
     * start listening on protocols
     *
     * @param srv server to start
     * @param pip pipeline to use
     * @param prt port to access, 0=default
     * @return false if successful, true if error happened
     */
    public boolean genStrmStart(prtServS srv, pipeLine pip, int prt) {
        if (prt == 0) {
            prt = srvPort;
        }
        if (srvVrf == null) {
            return true;
        }
        pipeSample = pip;
        ipFwdIface ifc4 = null;
        ipFwdIface ifc6 = null;
        if (srvIface != null) {
            if (srvIface.vrfFor == null) {
                return true;
            }
            if (srvVrf.compare(srvIface.vrfFor, srvVrf) != 0) {
                return true;
            }
            ifc4 = srvIface.fwdIf4;
            ifc6 = srvIface.fwdIf6;
        }
        if ((srvProto & protoTcp) != 0) {
            if ((srvProto & protoIp4) != 0) {
                if (srvVrf.tcp4.streamListen(srv, pip, ifc4, prt, null, 0, 0, srvName(), null, -1)) {
                    return true;
                }
            }
            if ((srvProto & protoIp6) != 0) {
                if (srvVrf.tcp6.streamListen(srv, pip, ifc6, prt, null, 0, 0, srvName(), null, -1)) {
                    return true;
                }
            }
        }
        if ((srvProto & protoUdp) != 0) {
            if ((srvProto & protoIp4) != 0) {
                if (srvVrf.udp4.streamListen(srv, pip, ifc4, prt, null, 0, 0, srvName(), null, -1)) {
                    return true;
                }
            }
            if ((srvProto & protoIp6) != 0) {
                if (srvVrf.udp6.streamListen(srv, pip, ifc6, prt, null, 0, 0, srvName(), null, -1)) {
                    return true;
                }
            }
        }
        if ((srvProto & protoLudp) != 0) {
            if ((srvProto & protoIp4) != 0) {
                if (srvVrf.ludp4.streamListen(srv, pip, ifc4, prt, null, 0, 0, srvName(), null, -1)) {
                    return true;
                }
            }
            if ((srvProto & protoIp6) != 0) {
                if (srvVrf.ludp6.streamListen(srv, pip, ifc6, prt, null, 0, 0, srvName(), null, -1)) {
                    return true;
                }
            }
        }
        if ((srvProto & protoDccp) != 0) {
            if ((srvProto & protoIp4) != 0) {
                if (srvVrf.dccp4.streamListen(srv, pip, ifc4, prt, null, 0, 0, srvName(), null, -1)) {
                    return true;
                }
            }
            if ((srvProto & protoIp6) != 0) {
                if (srvVrf.dccp6.streamListen(srv, pip, ifc6, prt, null, 0, 0, srvName(), null, -1)) {
                    return true;
                }
            }
        }
        if ((srvProto & protoSctp) != 0) {
            if ((srvProto & protoIp4) != 0) {
                if (srvVrf.sctp4.streamListen(srv, pip, ifc4, prt, null, 0, 0, srvName(), null, -1)) {
                    return true;
                }
            }
            if ((srvProto & protoIp6) != 0) {
                if (srvVrf.sctp6.streamListen(srv, pip, ifc6, prt, null, 0, 0, srvName(), null, -1)) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * start listening on protocols
     *
     * @param srv server to start
     * @param prt port to access, 0=default
     * @return false if successful, true if error happened
     */
    public boolean genDgrmStart(prtServP srv, int prt) {
        if (prt == 0) {
            prt = srvPort;
        }
        if (srvVrf == null) {
            return true;
        }
        ipFwdIface ifc4 = null;
        ipFwdIface ifc6 = null;
        if (srvIface != null) {
            if (srvIface.vrfFor == null) {
                return true;
            }
            if (srvVrf.compare(srvIface.vrfFor, srvVrf) != 0) {
                return true;
            }
            ifc4 = srvIface.fwdIf4;
            ifc6 = srvIface.fwdIf6;
        }
        if ((srvProto & protoTcp) != 0) {
            if ((srvProto & protoIp4) != 0) {
                if (srvVrf.tcp4.packetListen(srv, ifc4, prt, null, 0, 0, srvName(), null, -1)) {
                    return true;
                }
            }
            if ((srvProto & protoIp6) != 0) {
                if (srvVrf.tcp6.packetListen(srv, ifc6, prt, null, 0, 0, srvName(), null, -1)) {
                    return true;
                }
            }
        }
        if ((srvProto & protoUdp) != 0) {
            if ((srvProto & protoIp4) != 0) {
                if (srvVrf.udp4.packetListen(srv, ifc4, prt, null, 0, 0, srvName(), null, -1)) {
                    return true;
                }
            }
            if ((srvProto & protoIp6) != 0) {
                if (srvVrf.udp6.packetListen(srv, ifc6, prt, null, 0, 0, srvName(), null, -1)) {
                    return true;
                }
            }
        }
        if ((srvProto & protoLudp) != 0) {
            if ((srvProto & protoIp4) != 0) {
                if (srvVrf.ludp4.packetListen(srv, ifc4, prt, null, 0, 0, srvName(), null, -1)) {
                    return true;
                }
            }
            if ((srvProto & protoIp6) != 0) {
                if (srvVrf.ludp6.packetListen(srv, ifc6, prt, null, 0, 0, srvName(), null, -1)) {
                    return true;
                }
            }
        }
        if ((srvProto & protoDccp) != 0) {
            if ((srvProto & protoIp4) != 0) {
                if (srvVrf.dccp4.packetListen(srv, ifc4, prt, null, 0, 0, srvName(), null, -1)) {
                    return true;
                }
            }
            if ((srvProto & protoIp6) != 0) {
                if (srvVrf.dccp6.packetListen(srv, ifc6, prt, null, 0, 0, srvName(), null, -1)) {
                    return true;
                }
            }
        }
        if ((srvProto & protoSctp) != 0) {
            if ((srvProto & protoIp4) != 0) {
                if (srvVrf.sctp4.packetListen(srv, ifc4, prt, null, 0, 0, srvName(), null, -1)) {
                    return true;
                }
            }
            if ((srvProto & protoIp6) != 0) {
                if (srvVrf.sctp6.packetListen(srv, ifc6, prt, null, 0, 0, srvName(), null, -1)) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * start listening on protocols
     *
     * @param srv server to start
     * @param prt port to access, 0=default
     * @return false if successful, true if error happened
     */
    public boolean genRawStart(ipPrt srv, int prt) {
        if (prt == 0) {
            prt = srvPort;
        }
        if (srvVrf == null) {
            return true;
        }
        ipFwdIface ifc4 = null;
        ipFwdIface ifc6 = null;
        if (srvIface != null) {
            if (srvIface.vrfFor == null) {
                return true;
            }
            if (srvVrf.compare(srvIface.vrfFor, srvVrf) != 0) {
                return true;
            }
            ifc4 = srvIface.fwdIf4;
            ifc6 = srvIface.fwdIf6;
        }
        if ((srvProto & protoIp4) != 0) {
            if (srvVrf.fwd4.protoAdd(srv, ifc4, null)) {
                return true;
            }
        }
        if ((srvProto & protoIp6) != 0) {
            if (srvVrf.fwd6.protoAdd(srv, ifc6, null)) {
                return true;
            }
        }
        return false;
    }

    /**
     * stop listening on protocols
     *
     * @param srv server to start
     * @param prt port to access, 0=default
     * @return false if successful, true if error happened
     */
    public boolean genRawStop(ipPrt srv, int prt) {
        if (prt == 0) {
            prt = srvPort;
        }
        if (srvVrf == null) {
            return true;
        }
        ipFwdIface ifc4 = null;
        ipFwdIface ifc6 = null;
        if (srvIface != null) {
            if (srvIface.vrfFor == null) {
                return true;
            }
            if (srvVrf.compare(srvIface.vrfFor, srvVrf) != 0) {
                return true;
            }
            ifc4 = srvIface.fwdIf4;
            ifc6 = srvIface.fwdIf6;
        }
        if ((srvProto & protoIp4) != 0) {
            srvVrf.fwd4.protoDel(srv, ifc4, null);
        }
        if ((srvProto & protoIp6) != 0) {
            srvVrf.fwd6.protoDel(srv, ifc6, null);
        }
        return false;
    }

    /**
     * stop listening on protocols
     *
     * @param prt port to access, 0=default
     * @return false if successful, true if error happened
     */
    public boolean genericStop(int prt) {
        if (prt == 0) {
            prt = srvPort;
        }
        if (srvVrf == null) {
            return true;
        }
        ipFwdIface ifc4 = null;
        ipFwdIface ifc6 = null;
        if (srvIface != null) {
            if (srvIface.vrfFor == null) {
                return true;
            }
            if (srvVrf.compare(srvIface.vrfFor, srvVrf) != 0) {
                return true;
            }
            ifc4 = srvIface.fwdIf4;
            ifc6 = srvIface.fwdIf6;
        }
        if ((srvProto & protoTcp) != 0) {
            if ((srvProto & protoIp4) != 0) {
                if (srvVrf.tcp4.listenStop(ifc4, prt, null, 0, 0)) {
                    return true;
                }
            }
            if ((srvProto & protoIp6) != 0) {
                if (srvVrf.tcp6.listenStop(ifc6, prt, null, 0, 0)) {
                    return true;
                }
            }
        }
        if ((srvProto & protoUdp) != 0) {
            if ((srvProto & protoIp4) != 0) {
                if (srvVrf.udp4.listenStop(ifc4, prt, null, 0, 0)) {
                    return true;
                }
            }
            if ((srvProto & protoIp6) != 0) {
                if (srvVrf.udp6.listenStop(ifc6, prt, null, 0, 0)) {
                    return true;
                }
            }
        }
        if ((srvProto & protoLudp) != 0) {
            if ((srvProto & protoIp4) != 0) {
                if (srvVrf.ludp4.listenStop(ifc4, prt, null, 0, 0)) {
                    return true;
                }
            }
            if ((srvProto & protoIp6) != 0) {
                if (srvVrf.ludp6.listenStop(ifc6, prt, null, 0, 0)) {
                    return true;
                }
            }
        }
        if ((srvProto & protoDccp) != 0) {
            if ((srvProto & protoIp4) != 0) {
                if (srvVrf.dccp4.listenStop(ifc4, prt, null, 0, 0)) {
                    return true;
                }
            }
            if ((srvProto & protoIp6) != 0) {
                if (srvVrf.dccp6.listenStop(ifc6, prt, null, 0, 0)) {
                    return true;
                }
            }
        }
        if ((srvProto & protoSctp) != 0) {
            if ((srvProto & protoIp4) != 0) {
                if (srvVrf.sctp4.listenStop(ifc4, prt, null, 0, 0)) {
                    return true;
                }
            }
            if ((srvProto & protoIp6) != 0) {
                if (srvVrf.sctp6.listenStop(ifc6, prt, null, 0, 0)) {
                    return true;
                }
            }
        }
        return false;
    }

    private int srvCheckAccept(ipFwdIface ifc, int prt, boolean is4, addrIP adr) {
        int res = 0;
        if ((srvProto & protoTcp) != 0) {
            if (is4) {
                res += srvVrf.tcp4.countClients(ifc, prt, adr);
            } else {
                res += srvVrf.tcp6.countClients(ifc, prt, adr);
            }
        }
        if ((srvProto & protoUdp) != 0) {
            if (is4) {
                res += srvVrf.udp4.countClients(ifc, prt, adr);
            } else {
                res += srvVrf.udp6.countClients(ifc, prt, adr);
            }
        }
        if ((srvProto & protoLudp) != 0) {
            if (is4) {
                res += srvVrf.ludp4.countClients(ifc, prt, adr);
            } else {
                res += srvVrf.ludp6.countClients(ifc, prt, adr);
            }
        }
        if ((srvProto & protoDccp) != 0) {
            if (is4) {
                res += srvVrf.dccp4.countClients(ifc, prt, adr);
            } else {
                res += srvVrf.dccp6.countClients(ifc, prt, adr);
            }
        }
        if ((srvProto & protoSctp) != 0) {
            if (is4) {
                res += srvVrf.sctp4.countClients(ifc, prt, adr);
            } else {
                res += srvVrf.sctp6.countClients(ifc, prt, adr);
            }
        }
        return res;
    }

    private boolean srvCheckAccept(addrIP adr) {
        if (srvAccRat > 0) {
            if ((bits.getTime() - srvAccLst) > srvAccInt) {
                srvAccCnt = 0;
            }
            if (srvAccCnt > srvAccRat) {
                return true;
            }
            srvAccLst = bits.getTime();
            srvAccCnt++;
        }
        if ((srvPrfLst == null) && (srvRouMap == null) && (srvRouPol == null)) {
            return false;
        }
        ipFwd fwd = srvVrf.getFwd(adr);
        tabRouteEntry<addrIP> ntry = fwd.actualU.route(adr);
        if (ntry == null) {
            return true;
        }
        if (srvPrfLst != null) {
            if (!srvPrfLst.matches(1, ntry.prefix)) {
                return true;
            }
        }
        if (srvRouMap != null) {
            tabRtrmapN rmn = srvRouMap.find(1, ntry);
            if (rmn == null) {
                return true;
            }
            if (rmn.action != tabPlcmapN.actionType.actPermit) {
                return true;
            }
        }
        if (srvRouPol != null) {
            ntry = tabRtrplc.doRpl(1, ntry, srvRouPol, true);
            if (ntry == null) {
                return true;
            }
        }
        return false;
    }

    private boolean srvCheckAccept(prtGenConn conn) {
        if (srvCheckAccept(conn.peerAddr)) {
            return true;
        }
        if (srvAccess != null) {
            if (!srvAccess.matches(conn)) {
                if (srvLogDrop) {
                    logger.info("access class dropped " + conn);
                }
                return true;
            }
        }
        if (srvTotLim > 0) {
            if (srvCheckAccept(conn.iface, conn.portLoc, conn.peerAddr.isIPv4(), null) > srvTotLim) {
                if (srvLogDrop) {
                    logger.info("total limit dropped " + conn);
                }
                return true;
            }
        }
        if (srvPerLim > 0) {
            if (srvCheckAccept(conn.iface, conn.portLoc, conn.peerAddr.isIPv4(), conn.peerAddr) > srvPerLim) {
                if (srvLogDrop) {
                    logger.info("peer limit dropped " + conn);
                }
                return true;
            }
        }
        return false;
    }

    /**
     * check if connection acceptable
     *
     * @param ifc interface packet arrived on
     * @param pck packet to check
     * @return false if acceptable, true if not
     */
    protected boolean srvCheckAccept(ipFwdIface ifc, packHolder pck) {
        if (srvCheckAccept(pck.IPsrc)) {
            return true;
        }
        if (srvAccess != null) {
            if (!srvAccess.matches(false, false, pck)) {
                if (srvLogDrop) {
                    logger.info("access class dropped " + pck.IPsrc + " " + pck.IPprt);
                }
                return true;
            }
        }
        if (srvTotLim > 0) {
            int res = 0;
            if (pck.IPsrc.isIPv4()) {
                res = srvVrf.fwd4.protos.countClients(ifc.ifwNum, pck.IPprt, pck.IPsrc);
            } else {
                res = srvVrf.fwd6.protos.countClients(ifc.ifwNum, pck.IPprt, pck.IPsrc);
            }
            if (res > srvTotLim) {
                if (srvLogDrop) {
                    logger.info("total limit dropped " + pck.IPsrc + " " + pck.IPprt);
                }
                return true;
            }
        }
        return false;
    }

    /**
     * accept one connection
     *
     * @param pipe pipeline just accepted
     * @param id connection just accepted
     * @return false if successful, true if error happened
     */
    public boolean streamAccept(pipeSide pipe, prtGenConn id) {
        if (srvCheckAccept(id)) {
            return true;
        }
        pipe = secServer.openSec(pipe, secProto & protoSec, pipeSample, srvAuther, keyrsa, keydsa, keyecdsa, certrsa, certdsa, certecdsa);
        if (pipe == null) {
            return true;
        }
        return srvAccept(pipe, id);
    }

    /**
     * get blocking mode is cloned or not
     *
     * @return true means cloned, false means dynamic
     */
    public boolean streamForceBlock() {
        return !dynBlckMod;
    }

    /**
     * accept one connection
     *
     * @param id connection just accepted
     * @return false if successful, true if error happened
     */
    public boolean datagramAccept(prtGenConn id) {
        if (srvCheckAccept(id)) {
            return true;
        }
        return srvAccept(null, id);
    }

    /**
     * notified of interface close
     *
     * @param ifc interface closed
     */
    public void closedInterface(ipFwdIface ifc) {
    }

    /**
     * get help text
     *
     * @return help text
     */
    public userHelping getHelp() {
        userHelping l = userHelping.getGenCfg();
        l.add("1 2  vrf                    set vrf to use");
        l.add("2 .    <name>               name of vrf");
        l.add("1 2  port                   set port to listen on");
        l.add("2 .    <num>                port number to use");
        l.add("1 2  access-class           set access list");
        l.add("2 .    <name>               access list name");
        l.add("1 2  access-prefix          set prefix list");
        l.add("2 .    <name>               prefix list name");
        l.add("1 2  access-rate            set route map");
        l.add("2 3    <num>                new sessions per interval");
        l.add("3 .      <num>              interval");
        l.add("1 2  access-map             set route map");
        l.add("2 .    <name>               route map name");
        l.add("1 2  access-policy          set route policy");
        l.add("2 .    <name>               route policy name");
        l.add("1 2  access-total           session limit for this server");
        l.add("2 .    <num>                number of connections");
        l.add("1 2  access-peer            per client session limit");
        l.add("2 .    <num>                number of connections");
        l.add("1 .  access-log             log dropped attemps");
        l.add("1 2  protocol               set lower protocols to use");
        l.add("2 2,.  ipv4                 use ip4 network");
        l.add("2 2,.  ipv6                 use ip6 network");
        l.add("2 2,.  tcp                  use tcp transport");
        l.add("2 2,.  udp                  use udp transport");
        l.add("2 2,.  ludp                 use ludp transport");
        l.add("2 2,.  dccp                 use dccp transport");
        l.add("2 2,.  sctp                 use sctp transport");
        l.add("1 2  interface              interface to bind to");
        l.add("2 .    <name>               name of interface");
        l.add("1 2  security               set security parameters");
        l.add("2 3    protocol             set lower protocol to use");
        l.add("3 .      ssh                select secure shell");
        l.add("3 .      tls                select transport layer security");
        l.add("3 .      dtls               select datagram transport layer security");
        l.add("3 .      telnet             select telnet protocol");
        l.add("2 3    authentication       set authentication");
        l.add("3 .      <name>             name of authentication list");
        l.add("2 3    rsakey               set rsa key");
        l.add("3 .      <name>             name of key");
        l.add("2 3    dsakey               set dsa key");
        l.add("3 .      <name>             name of key");
        l.add("2 3    ecdsakey             set ecdsa key");
        l.add("3 .      <name>             name of key");
        l.add("2 3    rsacert              set rsa certificate");
        l.add("3 .      <name>             name of certificate");
        l.add("2 3    dsacert              set dsa certificate");
        l.add("3 .      <name>             name of certificate");
        l.add("2 3    ecdsacert            set ecdsa certificate");
        l.add("3 .      <name>             name of certificate");
        srvHelp(l);
        return l;
    }

    /**
     * get running configuration
     *
     * @param beg beginning string
     * @param filter filter defaults
     * @return config text
     */
    public List<String> getShRun(String beg, boolean filter) {
        List<String> l = new ArrayList<String>();
        l.add("server " + srvName() + " " + srvName);
        beg += cmds.tabulator;
        cmds.cfgLine(l, secProto == 0, beg, "security protocol", proto2string(secProto));
        if (srvAuther == null) {
            l.add(beg + "no security authentication");
        } else {
            l.add(beg + "security authentication " + srvAuther.autName);
        }
        if (keyrsa == null) {
            l.add(beg + "no security rsakey");
        } else {
            l.add(beg + "security rsakey " + keyrsa.keyName);
        }
        if (keydsa == null) {
            l.add(beg + "no security dsakey");
        } else {
            l.add(beg + "security dsakey " + keydsa.keyName);
        }
        if (keyecdsa == null) {
            l.add(beg + "no security ecdsakey");
        } else {
            l.add(beg + "security ecdsakey " + keyecdsa.keyName);
        }
        if (certrsa == null) {
            l.add(beg + "no security rsacert");
        } else {
            l.add(beg + "security rsacert " + certrsa.crtName);
        }
        if (certdsa == null) {
            l.add(beg + "no security dsacert");
        } else {
            l.add(beg + "security dsacert " + certdsa.crtName);
        }
        if (certecdsa == null) {
            l.add(beg + "no security ecdsacert");
        } else {
            l.add(beg + "security ecdsacert " + certecdsa.crtName);
        }
        if (srvAccess != null) {
            l.add(beg + "access-class " + srvAccess.listName);
        } else {
            l.add(beg + "no access-class");
        }
        if (srvPrfLst != null) {
            l.add(beg + "access-prefix " + srvPrfLst.listName);
        } else {
            l.add(beg + "no access-prefix");
        }
        if (srvRouMap != null) {
            l.add(beg + "access-map " + srvRouMap.listName);
        } else {
            l.add(beg + "no access-map");
        }
        if (srvRouPol != null) {
            l.add(beg + "access-policy " + srvRouPol.listName);
        } else {
            l.add(beg + "no access-policy");
        }
        if (srvAccRat > 0) {
            l.add(beg + "access-rate " + srvAccRat + " " + srvAccInt);
        } else {
            l.add(beg + "no access-rate");
        }
        cmds.cfgLine(l, !srvLogDrop, beg, "access-log", "");
        l.add(beg + "access-total " + srvTotLim);
        l.add(beg + "access-peer " + srvPerLim);
        l.add(beg + "port " + srvPort);
        l.add(beg + "protocol " + proto2string(srvProto));
        srvShRun(beg, l);
        if (srvIface != null) {
            l.add(beg + "interface " + srvIface.name);
        } else {
            l.add(beg + "no interface");
        }
        if (srvVrf != null) {
            l.add(beg + "vrf " + srvVrf.name);
        } else {
            l.add(beg + "no vrf");
        }
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        if (!filter) {
            return l;
        }
        return userFilter.filterText(l, srvDefFlt());
    }

    /**
     * parse commands
     *
     * @param cmd commands
     * @return true if error happened
     */
    public boolean doCfgStr(cmds cmd) {
        String a = cmd.word();
        if (a.equals("vrf")) {
            srvDeinit();
            srvVrf = cfgAll.vrfFind(cmd.word(), false);
            if (srvVrf == null) {
                cmd.error("no such vrf exists");
                return false;
            }
            srvInit();
            return false;
        }
        if (a.equals("port")) {
            srvDeinit();
            srvPort = bits.str2num(cmd.word());
            if (srvPort < 1) {
                cmd.error("bad port number");
                return false;
            }
            srvInit();
            return false;
        }
        if (a.equals("protocol")) {
            int i = 0;
            for (;;) {
                a = cmd.word();
                if (a.length() < 1) {
                    break;
                }
                i |= string2proto(a);
            }
            srvDeinit();
            srvProto = i;
            srvInit();
            return false;
        }
        if (a.equals("interface")) {
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
            if (ifc == null) {
                cmd.error("no such interface");
                return false;
            }
            if (ifc.vrfFor == null) {
                cmd.error("not routed interface");
                return false;
            }
            srvDeinit();
            srvIface = ifc;
            srvInit();
            return false;
        }
        if (a.equals("access-log")) {
            srvLogDrop = true;
            return false;
        }
        if (a.equals("access-total")) {
            srvTotLim = bits.str2num(cmd.word());
            return false;
        }
        if (a.equals("access-peer")) {
            srvPerLim = bits.str2num(cmd.word());
            return false;
        }
        if (a.equals("access-class")) {
            cfgAceslst ntry = cfgAll.aclsFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such access list");
                return false;
            }
            srvAccess = ntry.aceslst;
            return false;
        }
        if (a.equals("access-prefix")) {
            cfgPrfxlst ntry = cfgAll.prfxFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such prefix list");
                return false;
            }
            srvPrfLst = ntry.prflst;
            return false;
        }
        if (a.equals("access-rate")) {
            srvAccRat = bits.str2num(cmd.word());
            srvAccInt = bits.str2num(cmd.word());
            return false;
        }
        if (a.equals("access-map")) {
            cfgRoump ntry = cfgAll.rtmpFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such route map");
                return false;
            }
            srvRouMap = ntry.roumap;
            return false;
        }
        if (a.equals("access-policy")) {
            cfgRouplc ntry = cfgAll.rtplFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such route policy");
                return false;
            }
            srvRouPol = ntry.rouplc;
            return false;
        }
        if (a.equals("security")) {
            String s = cmd.word();
            if (s.equals("protocol")) {
                secProto = string2proto(cmd.word());
                return false;
            }
            if (s.equals("authentication")) {
                cfgAuther lst = cfgAll.autherFind(cmd.word(), null);
                if (lst == null) {
                    cmd.error("no such auth list");
                    return false;
                }
                srvAuther = lst.getAuther();
                return false;
            }
            if (s.equals("rsakey")) {
                cfgKey<cryKeyRSA> cfg = cfgAll.keyFind(cfgAll.rsakeys, cmd.word(), false);
                if (cfg == null) {
                    cmd.error("no such key");
                    return false;
                }
                keyrsa = cfg.key;
                return false;
            }
            if (s.equals("dsakey")) {
                cfgKey<cryKeyDSA> cfg = cfgAll.keyFind(cfgAll.dsakeys, cmd.word(), false);
                if (cfg == null) {
                    cmd.error("no such key");
                    return false;
                }
                keydsa = cfg.key;
                return false;
            }
            if (s.equals("ecdsakey")) {
                cfgKey<cryKeyECDSA> cfg = cfgAll.keyFind(cfgAll.ecdsakeys, cmd.word(), false);
                if (cfg == null) {
                    cmd.error("no such key");
                    return false;
                }
                keyecdsa = cfg.key;
                return false;
            }
            if (s.equals("rsacert")) {
                cfgCert cfg = cfgAll.certFind(cmd.word(), false);
                if (cfg == null) {
                    cmd.error("no such cert");
                    return false;
                }
                certrsa = cfg.cert;
                return false;
            }
            if (s.equals("dsacert")) {
                cfgCert cfg = cfgAll.certFind(cmd.word(), false);
                if (cfg == null) {
                    cmd.error("no such cert");
                    return false;
                }
                certdsa = cfg.cert;
                return false;
            }
            if (s.equals("ecdsacert")) {
                cfgCert cfg = cfgAll.certFind(cmd.word(), false);
                if (cfg == null) {
                    cmd.error("no such cert");
                    return false;
                }
                certecdsa = cfg.cert;
                return false;
            }
            cmd.badCmd();
            return false;
        }
        if (a.equals("no")) {
            a = cmd.word();
            if (a.equals("vrf")) {
                srvDeinit();
                srvVrf = null;
                return false;
            }
            if (a.equals("port")) {
                srvDeinit();
                srvPort = srvPort();
                srvInit();
                return false;
            }
            if (a.equals("protocol")) {
                srvDeinit();
                srvProto = srvProto();
                srvInit();
                return false;
            }
            if (a.equals("interface")) {
                srvDeinit();
                srvIface = null;
                srvInit();
                return false;
            }
            if (a.equals("access-log")) {
                srvLogDrop = false;
                return false;
            }
            if (a.equals("access-total")) {
                srvTotLim = 0;
                return false;
            }
            if (a.equals("access-peer")) {
                srvPerLim = 0;
                return false;
            }
            if (a.equals("access-class")) {
                srvAccess = null;
                return false;
            }
            if (a.equals("access-prefix")) {
                srvPrfLst = null;
                return false;
            }
            if (a.equals("access-rate")) {
                srvAccRat = -1;
                srvAccInt = -1;
                return false;
            }
            if (a.equals("access-map")) {
                srvRouMap = null;
                return false;
            }
            if (a.equals("access-policy")) {
                srvRouPol = null;
                return false;
            }
        }
        if (a.equals("security")) {
            String s = cmd.word();
            if (s.equals("protocol")) {
                secProto = 0;
                return false;
            }
            if (s.equals("authentication")) {
                srvAuther = null;
                return false;
            }
            if (s.equals("rsakey")) {
                keyrsa = null;
                return false;
            }
            if (s.equals("dsakey")) {
                keydsa = null;
                return false;
            }
            if (s.equals("ecdsakey")) {
                keyecdsa = null;
                return false;
            }
            if (s.equals("rsacert")) {
                certrsa = null;
                return false;
            }
            if (s.equals("dsacert")) {
                certdsa = null;
                return false;
            }
            if (s.equals("ecdsacert")) {
                certecdsa = null;
                return false;
            }
            cmd.badCmd();
            return false;
        }
        if (srvCfgStr(cmd.copyBytes(true))) {
            cmd.badCmd();
            return true;
        }
        return false;
    }

    public int compare(servGeneric o1, servGeneric o2) {
        return o1.srvName.toLowerCase().compareTo(o2.srvName.toLowerCase());
    }

    /**
     * get prompt value
     *
     * @return prompt value
     */
    public String getPrompt() {
        return "server";
    }

}
