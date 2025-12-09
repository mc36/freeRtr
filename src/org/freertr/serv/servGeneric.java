package org.freertr.serv;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrPrefix;
import org.freertr.auth.authGeneric;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgAuther;
import org.freertr.cfg.cfgCert;
import org.freertr.cfg.cfgGeneric;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgKey;
import org.freertr.cfg.cfgRtr;
import org.freertr.cfg.cfgVrf;
import org.freertr.cry.cryCertificate;
import org.freertr.cry.cryKeyDSA;
import org.freertr.cry.cryKeyECDSA;
import org.freertr.cry.cryKeyMLDSA;
import org.freertr.cry.cryKeyRSA;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipPrt;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGen;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServP;
import org.freertr.prt.prtServS;
import org.freertr.rtr.rtrBlackhole;
import org.freertr.sec.secInfoCfg;
import org.freertr.sec.secInfoCls;
import org.freertr.sec.secInfoUtl;
import org.freertr.sec.secInfoWrk;
import org.freertr.sec.secServer;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabRouteAttr;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.logger;

/**
 * generic server implementation
 *
 * @author matecsaba
 */
public abstract class servGeneric implements cfgGeneric, Comparable<servGeneric> {

    /**
     * create instance
     */
    public servGeneric() {
    }

    /**
     * description of this server
     */
    public String srvDescr = null;

    /**
     * embed vrf name
     */
    public boolean srvEmbedVrf;

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
    public cfgVrf srvVrf;

    /**
     * interface to use
     */
    public cfgIfc srvIface;

    /**
     * limit of all clients
     */
    protected int srvTotLim;

    /**
     * limit of one client
     */
    protected int srvPerLim;

    /**
     * limit of one subnet
     */
    protected int srvNetLim;

    /**
     * gather info per accesses
     */
    protected secInfoCfg srvIpInf;

    /**
     * blackhole process
     */
    protected rtrBlackhole srvBlckhl4;

    /**
     * blackhole process
     */
    protected rtrBlackhole srvBlckhl6;

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
     * mldsa key to use
     */
    protected cryKeyMLDSA keymldsa;

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
     * mldsa certificate to use
     */
    protected cryCertificate certmldsa;

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
     * use rlogin
     */
    public final static int protoRlogin = 0x1000;

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
    public final static int protoSec = protoSsh | protoTls | protoDtls | protoTelnet | protoRlogin;

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
    public final static userFilter[] srvdefsL = {
        // generic server
        new userFilter("server .*", cmds.tabulator + cmds.negated + cmds.tabulator + "description", null),
        new userFilter("server .*", cmds.tabulator + cmds.negated + cmds.tabulator + "security protocol", null),
        new userFilter("server .*", cmds.tabulator + cmds.negated + cmds.tabulator + "security authentication", null),
        new userFilter("server .*", cmds.tabulator + cmds.negated + cmds.tabulator + "security rsakey", null),
        new userFilter("server .*", cmds.tabulator + cmds.negated + cmds.tabulator + "security dsakey", null),
        new userFilter("server .*", cmds.tabulator + cmds.negated + cmds.tabulator + "security ecdsakey", null),
        new userFilter("server .*", cmds.tabulator + cmds.negated + cmds.tabulator + "security mldsakey", null),
        new userFilter("server .*", cmds.tabulator + cmds.negated + cmds.tabulator + "security rsacert", null),
        new userFilter("server .*", cmds.tabulator + cmds.negated + cmds.tabulator + "security dsacert", null),
        new userFilter("server .*", cmds.tabulator + cmds.negated + cmds.tabulator + "security ecdsacert", null),
        new userFilter("server .*", cmds.tabulator + cmds.negated + cmds.tabulator + "security mldsacert", null),
        new userFilter("server .*", cmds.tabulator + "access-total 0", null),
        new userFilter("server .*", cmds.tabulator + "access-peer 0", null),
        new userFilter("server .*", cmds.tabulator + "access-subnet 0", null),
        new userFilter("server .*", cmds.tabulator + cmds.negated + cmds.tabulator + "access-blackhole4", null),
        new userFilter("server .*", cmds.tabulator + cmds.negated + cmds.tabulator + "access-blackhole6", null),
        new userFilter("server .*", cmds.tabulator + cmds.negated + cmds.tabulator + "access-log", null),
        new userFilter("server .*", cmds.tabulator + cmds.negated + cmds.tabulator + "interface", null),
        new userFilter("server .*", cmds.tabulator + cmds.negated + cmds.tabulator + "vrf", null)
    };

    /**
     * set name of instance
     *
     * @param s name of server
     */
    public void srvRename(String s) {
        srvName = s;
    }

    /**
     * get running configuration
     *
     * @param beg beginning string
     * @param lst list to append
     * @param filter filter defaults
     */
    public abstract void srvShRun(String beg, List<String> lst, int filter);

    /**
     * get default filter
     *
     * @return default filter
     */
    public abstract userFilter[] srvDefFlt();

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
    public abstract void srvHelp(userHelp l);

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
        if ((i & protoRlogin) != 0) {
            a += " rlogin";
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
        if (a.equals("rlogin")) {
            return protoRlogin;
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
     * @return true if no, false if have any
     */
    public boolean noneSecKeys() {
        return (keydsa == null) && (keyrsa == null) && (keyecdsa == null) && (keymldsa == null);
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
        if (noneSecKeys()) {
            return null;
        }
        return secServer.openSec(pipe, proto, sample, auther, keyrsa, keydsa, keyecdsa, keymldsa, certrsa, certdsa, certecdsa, certmldsa);
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
            if (srvIface.vrfFor.compareTo(srvVrf) != 0) {
                return true;
            }
            ifc4 = srvIface.fwdIf4;
            ifc6 = srvIface.fwdIf6;
        }
        if ((srvProto & protoTcp) != 0) {
            if ((srvProto & protoIp4) != 0) {
                if (srvVrf.tcp4.streamListen(srv, pip, ifc4, prt, null, 0, srvName(), -1, null, -1, -1)) {
                    return true;
                }
            }
            if ((srvProto & protoIp6) != 0) {
                if (srvVrf.tcp6.streamListen(srv, pip, ifc6, prt, null, 0, srvName(), -1, null, -1, -1)) {
                    return true;
                }
            }
        }
        if ((srvProto & protoUdp) != 0) {
            if ((srvProto & protoIp4) != 0) {
                if (srvVrf.udp4.streamListen(srv, pip, ifc4, prt, null, 0, srvName(), -1, null, -1, -1)) {
                    return true;
                }
            }
            if ((srvProto & protoIp6) != 0) {
                if (srvVrf.udp6.streamListen(srv, pip, ifc6, prt, null, 0, srvName(), -1, null, -1, -1)) {
                    return true;
                }
            }
        }
        if ((srvProto & protoLudp) != 0) {
            if ((srvProto & protoIp4) != 0) {
                if (srvVrf.ludp4.streamListen(srv, pip, ifc4, prt, null, 0, srvName(), -1, null, -1, -1)) {
                    return true;
                }
            }
            if ((srvProto & protoIp6) != 0) {
                if (srvVrf.ludp6.streamListen(srv, pip, ifc6, prt, null, 0, srvName(), -1, null, -1, -1)) {
                    return true;
                }
            }
        }
        if ((srvProto & protoDccp) != 0) {
            if ((srvProto & protoIp4) != 0) {
                if (srvVrf.dccp4.streamListen(srv, pip, ifc4, prt, null, 0, srvName(), -1, null, -1, -1)) {
                    return true;
                }
            }
            if ((srvProto & protoIp6) != 0) {
                if (srvVrf.dccp6.streamListen(srv, pip, ifc6, prt, null, 0, srvName(), -1, null, -1, -1)) {
                    return true;
                }
            }
        }
        if ((srvProto & protoSctp) != 0) {
            if ((srvProto & protoIp4) != 0) {
                if (srvVrf.sctp4.streamListen(srv, pip, ifc4, prt, null, 0, srvName(), -1, null, -1, -1)) {
                    return true;
                }
            }
            if ((srvProto & protoIp6) != 0) {
                if (srvVrf.sctp6.streamListen(srv, pip, ifc6, prt, null, 0, srvName(), -1, null, -1, -1)) {
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
            if (srvIface.vrfFor.compareTo(srvVrf) != 0) {
                return true;
            }
            ifc4 = srvIface.fwdIf4;
            ifc6 = srvIface.fwdIf6;
        }
        if ((srvProto & protoTcp) != 0) {
            if ((srvProto & protoIp4) != 0) {
                if (srvVrf.tcp4.packetListen(srv, ifc4, prt, null, 0, srvName(), -1, null, -1, -1)) {
                    return true;
                }
            }
            if ((srvProto & protoIp6) != 0) {
                if (srvVrf.tcp6.packetListen(srv, ifc6, prt, null, 0, srvName(), -1, null, -1, -1)) {
                    return true;
                }
            }
        }
        if ((srvProto & protoUdp) != 0) {
            if ((srvProto & protoIp4) != 0) {
                if (srvVrf.udp4.packetListen(srv, ifc4, prt, null, 0, srvName(), -1, null, -1, -1)) {
                    return true;
                }
            }
            if ((srvProto & protoIp6) != 0) {
                if (srvVrf.udp6.packetListen(srv, ifc6, prt, null, 0, srvName(), -1, null, -1, -1)) {
                    return true;
                }
            }
        }
        if ((srvProto & protoLudp) != 0) {
            if ((srvProto & protoIp4) != 0) {
                if (srvVrf.ludp4.packetListen(srv, ifc4, prt, null, 0, srvName(), -1, null, -1, -1)) {
                    return true;
                }
            }
            if ((srvProto & protoIp6) != 0) {
                if (srvVrf.ludp6.packetListen(srv, ifc6, prt, null, 0, srvName(), -1, null, -1, -1)) {
                    return true;
                }
            }
        }
        if ((srvProto & protoDccp) != 0) {
            if ((srvProto & protoIp4) != 0) {
                if (srvVrf.dccp4.packetListen(srv, ifc4, prt, null, 0, srvName(), -1, null, -1, -1)) {
                    return true;
                }
            }
            if ((srvProto & protoIp6) != 0) {
                if (srvVrf.dccp6.packetListen(srv, ifc6, prt, null, 0, srvName(), -1, null, -1, -1)) {
                    return true;
                }
            }
        }
        if ((srvProto & protoSctp) != 0) {
            if ((srvProto & protoIp4) != 0) {
                if (srvVrf.sctp4.packetListen(srv, ifc4, prt, null, 0, srvName(), -1, null, -1, -1)) {
                    return true;
                }
            }
            if ((srvProto & protoIp6) != 0) {
                if (srvVrf.sctp6.packetListen(srv, ifc6, prt, null, 0, srvName(), -1, null, -1, -1)) {
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
            if (srvIface.vrfFor.compareTo(srvVrf) != 0) {
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
            if (srvIface.vrfFor.compareTo(srvVrf) != 0) {
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
            if (srvIface.vrfFor.compareTo(srvVrf) != 0) {
                return true;
            }
            ifc4 = srvIface.fwdIf4;
            ifc6 = srvIface.fwdIf6;
        }
        if ((srvProto & protoTcp) != 0) {
            if ((srvProto & protoIp4) != 0) {
                if (srvVrf.tcp4.listenStop(ifc4, prt, null, 0)) {
                    return true;
                }
            }
            if ((srvProto & protoIp6) != 0) {
                if (srvVrf.tcp6.listenStop(ifc6, prt, null, 0)) {
                    return true;
                }
            }
        }
        if ((srvProto & protoUdp) != 0) {
            if ((srvProto & protoIp4) != 0) {
                if (srvVrf.udp4.listenStop(ifc4, prt, null, 0)) {
                    return true;
                }
            }
            if ((srvProto & protoIp6) != 0) {
                if (srvVrf.udp6.listenStop(ifc6, prt, null, 0)) {
                    return true;
                }
            }
        }
        if ((srvProto & protoLudp) != 0) {
            if ((srvProto & protoIp4) != 0) {
                if (srvVrf.ludp4.listenStop(ifc4, prt, null, 0)) {
                    return true;
                }
            }
            if ((srvProto & protoIp6) != 0) {
                if (srvVrf.ludp6.listenStop(ifc6, prt, null, 0)) {
                    return true;
                }
            }
        }
        if ((srvProto & protoDccp) != 0) {
            if ((srvProto & protoIp4) != 0) {
                if (srvVrf.dccp4.listenStop(ifc4, prt, null, 0)) {
                    return true;
                }
            }
            if ((srvProto & protoIp6) != 0) {
                if (srvVrf.dccp6.listenStop(ifc6, prt, null, 0)) {
                    return true;
                }
            }
        }
        if ((srvProto & protoSctp) != 0) {
            if ((srvProto & protoIp4) != 0) {
                if (srvVrf.sctp4.listenStop(ifc4, prt, null, 0)) {
                    return true;
                }
            }
            if ((srvProto & protoIp6) != 0) {
                if (srvVrf.sctp6.listenStop(ifc6, prt, null, 0)) {
                    return true;
                }
            }
        }
        return false;
    }

    private addrPrefix<addrIP> srvGetSubnet(boolean ipv4, addrIP adr) {
        addrPrefix<addrIP> prf;
        if (ipv4) {
            prf = new addrPrefix<addrIP>(adr, cfgAll.accessSubnet4);
        } else {
            prf = new addrPrefix<addrIP>(adr, cfgAll.accessSubnet6);
        }
        return prf;
    }

    private int srvCountPrtClients(ipFwdIface ifc, int prt, boolean ipv4, addrIP adr) {
        if (ipv4) {
            return srvVrf.fwd4.protos.countClients(ifc, prt, adr);
        } else {
            return srvVrf.fwd6.protos.countClients(ifc, prt, adr);
        }
    }

    private int srvCountPrtSubnet(boolean ipv4, ipFwdIface ifc, int prt, addrIP adr) {
        addrPrefix<addrIP> prf = srvGetSubnet(ipv4, adr);
        if (ipv4) {
            return srvVrf.fwd4.protos.countSubnet(ifc, prt, prf);
        } else {
            return srvVrf.fwd6.protos.countSubnet(ifc, prt, prf);
        }
    }

    private int srvCountClients(ipFwdIface ifc, int prt, boolean ipv4, addrIP adr) {
        int res = 0;
        if ((srvProto & protoTcp) != 0) {
            if (ipv4) {
                res += srvVrf.tcp4.countClients(ifc, prt, adr);
            } else {
                res += srvVrf.tcp6.countClients(ifc, prt, adr);
            }
        }
        if ((srvProto & protoUdp) != 0) {
            if (ipv4) {
                res += srvVrf.udp4.countClients(ifc, prt, adr);
            } else {
                res += srvVrf.udp6.countClients(ifc, prt, adr);
            }
        }
        if ((srvProto & protoLudp) != 0) {
            if (ipv4) {
                res += srvVrf.ludp4.countClients(ifc, prt, adr);
            } else {
                res += srvVrf.ludp6.countClients(ifc, prt, adr);
            }
        }
        if ((srvProto & protoDccp) != 0) {
            if (ipv4) {
                res += srvVrf.dccp4.countClients(ifc, prt, adr);
            } else {
                res += srvVrf.dccp6.countClients(ifc, prt, adr);
            }
        }
        if ((srvProto & protoSctp) != 0) {
            if (ipv4) {
                res += srvVrf.sctp4.countClients(ifc, prt, adr);
            } else {
                res += srvVrf.sctp6.countClients(ifc, prt, adr);
            }
        }
        return res;
    }

    private int srvCountSubnet(boolean ipv4, ipFwdIface ifc, int prt, addrIP adr) {
        addrPrefix<addrIP> prf = srvGetSubnet(ipv4, adr);
        int res = 0;
        if ((srvProto & protoTcp) != 0) {
            if (ipv4) {
                res += srvVrf.tcp4.countSubnet(ifc, prt, prf);
            } else {
                res += srvVrf.tcp6.countSubnet(ifc, prt, prf);
            }
        }
        if ((srvProto & protoUdp) != 0) {
            if (ipv4) {
                res += srvVrf.udp4.countSubnet(ifc, prt, prf);
            } else {
                res += srvVrf.udp6.countSubnet(ifc, prt, prf);
            }
        }
        if ((srvProto & protoLudp) != 0) {
            if (ipv4) {
                res += srvVrf.ludp4.countSubnet(ifc, prt, prf);
            } else {
                res += srvVrf.ludp6.countSubnet(ifc, prt, prf);
            }
        }
        if ((srvProto & protoDccp) != 0) {
            if (ipv4) {
                res += srvVrf.dccp4.countSubnet(ifc, prt, prf);
            } else {
                res += srvVrf.dccp6.countSubnet(ifc, prt, prf);
            }
        }
        if ((srvProto & protoSctp) != 0) {
            if (ipv4) {
                res += srvVrf.sctp4.countSubnet(ifc, prt, prf);
            } else {
                res += srvVrf.sctp6.countSubnet(ifc, prt, prf);
            }
        }
        return res;
    }

    private boolean srvCheckBlackhole(boolean ipv4, addrIP adr, int prt) {
        if (ipv4) {
            if (srvBlckhl4 != null) {
                if (srvBlckhl4.checkAddr(adr)) {
                    if (srvLogDrop) {
                        logger.info("blackhole dropped " + adr + " " + prt);
                    }
                    return true;
                }
            }
        } else {
            if (srvBlckhl6 != null) {
                if (srvBlckhl6.checkAddr(adr)) {
                    if (srvLogDrop) {
                        logger.info("blackhole dropped " + adr + " " + prt);
                    }
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * blackhole peer
     *
     * @param ipv4 address family
     * @param adr address to block
     */
    protected void srvBlackholePeer(boolean ipv4, addrIP adr) {
        if (ipv4) {
            if (srvBlckhl4 != null) {
                srvBlckhl4.blockAddr(adr);
            }
        } else {
            if (srvBlckhl6 != null) {
                srvBlckhl6.blockAddr(adr);
            }
        }
    }

    private boolean srvCheckAccept(prtGenConn conn) {
        secInfoCls cls = new secInfoCls(null, conn, null, srvVrf.getFwd(conn.peerAddr), conn.peerAddr, conn.protoNum, conn.iface.addr);
        secInfoWrk inf = new secInfoWrk(srvIpInf, cls);
        inf.doWork(true);
        if (inf.need2drop()) {
            if (srvLogDrop) {
                logger.info("access ipinfo dropped " + conn);
            }
            return true;
        }
        boolean ipv4 = conn.peerAddr.isIPv4();
        if (srvCheckBlackhole(ipv4, conn.peerAddr, conn.portLoc)) {
            return true;
        }
        if (srvTotLim > 0) {
            if (srvCountClients(conn.iface, conn.portLoc, ipv4, null) >= srvTotLim) {
                if (srvLogDrop) {
                    logger.info("total limit dropped " + conn);
                }
                return true;
            }
        }
        if (srvPerLim > 0) {
            if (srvCountClients(conn.iface, conn.portLoc, ipv4, conn.peerAddr) >= srvPerLim) {
                if (srvLogDrop) {
                    logger.info("peer limit dropped " + conn);
                }
                srvBlackholePeer(ipv4, conn.peerAddr);
                return true;
            }
        }
        if (srvNetLim > 0) {
            if (srvCountSubnet(ipv4, conn.iface, conn.portLoc, conn.peerAddr) >= srvNetLim) {
                if (srvLogDrop) {
                    logger.info("subnet limit dropped " + conn);
                }
                srvBlackholePeer(ipv4, conn.peerAddr);
                return true;
            }
        }
        return false;
    }

    /**
     * check if connection acceptable
     *
     * @param ifc interface packet arrived on
     * @param rem remote address
     * @param prt protocol to use
     * @return false if acceptable, true if not
     */
    public boolean srvCheckAcceptIp(ipFwdIface ifc, addrIP rem, ipPrt prt) {
        int pn = prt.getProtoNum();
        secInfoCls cls = new secInfoCls(null, null, prt, srvVrf.getFwd(rem), rem, pn, ifc.addr);
        secInfoWrk inf = new secInfoWrk(srvIpInf, cls);
        inf.doWork(true);
        if (inf.need2drop()) {
            if (srvLogDrop) {
                logger.info("access ipinfo dropped " + rem + " " + ifc.addr);
            }
            return true;
        }
        boolean ipv4 = rem.isIPv4();
        if (srvCheckBlackhole(ipv4, rem, pn)) {
            return true;
        }
        if (srvTotLim > 0) {
            if (srvCountPrtClients(ifc, pn, ipv4, null) >= srvTotLim) {
                if (srvLogDrop) {
                    logger.info("total limit dropped " + rem + " " + ifc.addr);
                }
                return true;
            }
        }
        if (srvPerLim > 0) {
            if (srvCountPrtClients(ifc, pn, ipv4, rem) >= srvPerLim) {
                if (srvLogDrop) {
                    logger.info("peer limit dropped " + rem + " " + ifc.addr);
                }
                srvBlackholePeer(ipv4, rem);
                return true;
            }
        }
        if (srvNetLim > 0) {
            if (srvCountPrtSubnet(ipv4, ifc, pn, rem) >= srvNetLim) {
                if (srvLogDrop) {
                    logger.info("subnet limit dropped " + rem + " " + ifc.addr);
                }
                srvBlackholePeer(ipv4, rem);
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
        pipe = secServer.openSec(pipe, secProto & protoSec, pipeSample, srvAuther, keyrsa, keydsa, keyecdsa, keymldsa, certrsa, certdsa, certecdsa, certmldsa);
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
     * get security protocols
     *
     * @param hlp list to append
     * @param cur current level
     * @param nxt next levels
     */
    public static void getSecProts(userHelp hlp, int cur, int[] nxt) {
        hlp.add(null, false, cur, nxt, "ssh", "select secure shell");
        hlp.add(null, false, cur, nxt, "tls", "select transport layer security");
        hlp.add(null, false, cur, nxt, "dtls", "select datagram transport layer security");
        hlp.add(null, false, cur, nxt, "telnet", "select telnet protocol");
        hlp.add(null, false, cur, nxt, "rlogin", "select rlogin protocol");
    }

    /**
     * get help text
     *
     * @param l help text
     */
    public void getHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "rename", "rename this server");
        l.add(null, false, 2, new int[]{-1}, "<str>", "set new name");
        l.add(null, false, 1, new int[]{2}, "description", "specify description");
        l.add(null, false, 2, new int[]{2, -1}, "<str>", "description");
        l.add(null, false, 1, new int[]{2}, "vrf", "set vrf to use");
        l.add(null, false, 2, new int[]{-1}, "<name:vrf>", "name of vrf");
        l.add(null, false, 1, new int[]{2}, "port", "set port to listen on");
        l.add(null, false, 2, new int[]{-1}, "<num>", "port number to use");
        l.add(null, false, 1, new int[]{2}, "access-total", "session limit for this server");
        l.add(null, false, 2, new int[]{-1}, "<num>", "number of connections");
        l.add(null, false, 1, new int[]{2}, "access-peer", "per client session limit");
        l.add(null, false, 2, new int[]{-1}, "<num>", "number of connections");
        l.add(null, false, 1, new int[]{2}, "access-subnet", "per subnet session limit");
        l.add(null, false, 2, new int[]{-1}, "<num>", "number of connections");
        secInfoUtl.getHelp(l, 0, "access-", null);
        l.add(null, false, 1, new int[]{2}, "access-blackhole4", "propagate and check violating prefixes");
        l.add(null, false, 2, new int[]{-1}, "<num>", "number of process");
        l.add(null, false, 1, new int[]{2}, "access-blackhole6", "propagate and check violating prefixes");
        l.add(null, false, 2, new int[]{-1}, "<num>", "number of process");
        l.add(null, false, 1, new int[]{-1}, "access-log", "log dropped attemps");
        l.add(null, false, 1, new int[]{2}, "protocol", "set lower protocols to use");
        l.add(null, false, 2, new int[]{2, -1}, "ipv4", "use ip4 network");
        l.add(null, false, 2, new int[]{2, -1}, "ipv6", "use ip6 network");
        l.add(null, false, 2, new int[]{2, -1}, "tcp", "use tcp transport");
        l.add(null, false, 2, new int[]{2, -1}, "udp", "use udp transport");
        l.add(null, false, 2, new int[]{2, -1}, "ludp", "use ludp transport");
        l.add(null, false, 2, new int[]{2, -1}, "dccp", "use dccp transport");
        l.add(null, false, 2, new int[]{2, -1}, "sctp", "use sctp transport");
        l.add(null, false, 1, new int[]{2}, "interface", "interface to bind to");
        l.add(null, false, 2, new int[]{-1}, "<name:ifc>", "name of interface");
        l.add(null, false, 1, new int[]{2}, "security", "set security parameters");
        l.add(null, false, 2, new int[]{3}, "protocol", "set lower protocol to use");
        getSecProts(l, 3, new int[]{-1});
        l.add(null, false, 2, new int[]{3}, "authentication", "set authentication");
        l.add(null, false, 3, new int[]{-1}, "<name:aaa>", "name of authentication list");
        l.add(null, false, 2, new int[]{3}, "rsakey", "set rsa key");
        l.add(null, false, 3, new int[]{-1}, "<name:rsa>", "name of key");
        l.add(null, false, 2, new int[]{3}, "dsakey", "set dsa key");
        l.add(null, false, 3, new int[]{-1}, "<name:dsa>", "name of key");
        l.add(null, false, 2, new int[]{3}, "ecdsakey", "set ecdsa key");
        l.add(null, false, 3, new int[]{-1}, "<name:ecd>", "name of key");
        l.add(null, false, 2, new int[]{3}, "mldsakey", "set mldsa key");
        l.add(null, false, 3, new int[]{-1}, "<name:mld>", "name of key");
        l.add(null, false, 2, new int[]{3}, "rsacert", "set rsa certificate");
        l.add(null, false, 3, new int[]{-1}, "<name:crt>", "name of certificate");
        l.add(null, false, 2, new int[]{3}, "dsacert", "set dsa certificate");
        l.add(null, false, 3, new int[]{-1}, "<name:crt>", "name of certificate");
        l.add(null, false, 2, new int[]{3}, "ecdsacert", "set ecdsa certificate");
        l.add(null, false, 3, new int[]{-1}, "<name:crt>", "name of certificate");
        l.add(null, false, 2, new int[]{3}, "mldsacert", "set mldsa certificate");
        l.add(null, false, 3, new int[]{-1}, "<name:crt>", "name of certificate");
        l.add(null, false, 1, new int[]{-1}, cmds.upgradeCli, "embed vrf name to router knob");
        srvHelp(l);
    }

    /**
     * get running configuration
     *
     * @param filter filter defaults
     * @return config text
     */
    public List<String> getShRun(int filter) {
        List<String> l = new ArrayList<String>();
        String a = "";
        if (srvEmbedVrf) {
            if (srvVrf != null) {
                a += " vrf " + srvVrf.name;
            }
            if (srvIface != null) {
                a += " interface " + srvIface.name;
            }
        }
        l.add("server " + srvName() + " " + srvName + a);
        cmds.cfgLine(l, srvDescr == null, cmds.tabulator, "description", srvDescr);
        cmds.cfgLine(l, secProto == 0, cmds.tabulator, "security protocol", proto2string(secProto));
        if (srvAuther == null) {
            l.add(cmds.tabulator + "no security authentication");
        } else {
            l.add(cmds.tabulator + "security authentication " + srvAuther.autName);
        }
        if (keyrsa == null) {
            l.add(cmds.tabulator + "no security rsakey");
        } else {
            l.add(cmds.tabulator + "security rsakey " + keyrsa.keyName);
        }
        if (keydsa == null) {
            l.add(cmds.tabulator + "no security dsakey");
        } else {
            l.add(cmds.tabulator + "security dsakey " + keydsa.keyName);
        }
        if (keyecdsa == null) {
            l.add(cmds.tabulator + "no security ecdsakey");
        } else {
            l.add(cmds.tabulator + "security ecdsakey " + keyecdsa.keyName);
        }
        if (keymldsa == null) {
            l.add(cmds.tabulator + "no security mldsakey");
        } else {
            l.add(cmds.tabulator + "security mldsakey " + keymldsa.keyName);
        }
        if (certrsa == null) {
            l.add(cmds.tabulator + "no security rsacert");
        } else {
            l.add(cmds.tabulator + "security rsacert " + certrsa.crtName);
        }
        if (certdsa == null) {
            l.add(cmds.tabulator + "no security dsacert");
        } else {
            l.add(cmds.tabulator + "security dsacert " + certdsa.crtName);
        }
        if (certecdsa == null) {
            l.add(cmds.tabulator + "no security ecdsacert");
        } else {
            l.add(cmds.tabulator + "security ecdsacert " + certecdsa.crtName);
        }
        if (certmldsa == null) {
            l.add(cmds.tabulator + "no security mldsacert");
        } else {
            l.add(cmds.tabulator + "security mldsacert " + certmldsa.crtName);
        }
        cmds.cfgLine(l, !srvLogDrop, cmds.tabulator, "access-log", "");
        l.add(cmds.tabulator + "access-total " + srvTotLim);
        l.add(cmds.tabulator + "access-peer " + srvPerLim);
        l.add(cmds.tabulator + "access-subnet " + srvNetLim);
        secInfoUtl.getConfig(l, srvIpInf, cmds.tabulator + "access-");
        if (srvBlckhl4 != null) {
            l.add(cmds.tabulator + "access-blackhole4 " + srvBlckhl4.rtrNum);
        } else {
            l.add(cmds.tabulator + "no access-blackhole4");
        }
        if (srvBlckhl6 != null) {
            l.add(cmds.tabulator + "access-blackhole6 " + srvBlckhl6.rtrNum);
        } else {
            l.add(cmds.tabulator + "no access-blackhole6");
        }
        l.add(cmds.tabulator + "port " + srvPort);
        l.add(cmds.tabulator + "protocol " + proto2string(srvProto));
        srvShRun(cmds.tabulator, l, filter);
        if (!srvEmbedVrf) {
            if (srvIface != null) {
                l.add(cmds.tabulator + "interface " + srvIface.name);
            } else {
                l.add(cmds.tabulator + "no interface");
            }
            if (srvVrf != null) {
                l.add(cmds.tabulator + "vrf " + srvVrf.name);
            } else {
                l.add(cmds.tabulator + "no vrf");
            }
        }
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        if ((filter & 1) == 0) {
            return l;
        }
        l = userFilter.filterText(l, srvdefsL);
        return userFilter.filterText(l, srvDefFlt());
    }

    public void doCfgStr(cmds cmd) {
        String a = cmd.word();
        if (a.equals("rename")) {
            a = cmd.word();
            srvRename(a);
            return;
        }
        if (a.equals(cmds.upgradeCli)) {
            srvEmbedVrf = true;
            return;
        }
        if (a.equals("description")) {
            srvDescr = cmd.getRemaining();
            return;
        }
        if (a.equals("vrf")) {
            srvDeinit();
            srvVrf = cfgAll.vrfFind(cmd.word(), false);
            if (srvVrf == null) {
                cmd.error("no such vrf exists");
                return;
            }
            srvInit();
            return;
        }
        if (a.equals("port")) {
            srvDeinit();
            srvPort = bits.str2num(cmd.word());
            if (srvPort < 1) {
                cmd.error("bad port number");
                return;
            }
            srvInit();
            return;
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
            return;
        }
        if (a.equals("interface")) {
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
            if (ifc == null) {
                cmd.error("no such interface");
                return;
            }
            if (ifc.vrfFor == null) {
                cmd.error("not routed interface");
                return;
            }
            srvDeinit();
            srvIface = ifc;
            srvInit();
            return;
        }
        if (a.equals("access-log")) {
            srvLogDrop = true;
            return;
        }
        if (a.equals("access-total")) {
            srvTotLim = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("access-peer")) {
            srvPerLim = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("access-subnet")) {
            srvNetLim = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("access-blackhole4")) {
            cfgRtr ntry = cfgAll.rtrFind(tabRouteAttr.routeType.blackhole4, bits.str2num(cmd.word()), false);
            if (ntry == null) {
                cmd.error("no such process");
                return;
            }
            srvBlckhl4 = ntry.blackhole;
            return;
        }
        if (a.equals("access-blackhole6")) {
            cfgRtr ntry = cfgAll.rtrFind(tabRouteAttr.routeType.blackhole6, bits.str2num(cmd.word()), false);
            if (ntry == null) {
                cmd.error("no such process");
                return;
            }
            srvBlckhl6 = ntry.blackhole;
            return;
        }
        if (a.equals("security")) {
            String s = cmd.word();
            if (s.equals("protocol")) {
                secProto = string2proto(cmd.word());
                return;
            }
            if (s.equals("authentication")) {
                cfgAuther lst = cfgAll.autherFind(cmd.word(), null);
                if (lst == null) {
                    cmd.error("no such auth list");
                    return;
                }
                srvAuther = lst.getAuther();
                return;
            }
            if (s.equals("rsakey")) {
                cfgKey<cryKeyRSA> cfg = cfgAll.keyFind(cfgAll.rsakeys, cmd.word(), false);
                if (cfg == null) {
                    cmd.error("no such key");
                    return;
                }
                keyrsa = cfg.key;
                return;
            }
            if (s.equals("dsakey")) {
                cfgKey<cryKeyDSA> cfg = cfgAll.keyFind(cfgAll.dsakeys, cmd.word(), false);
                if (cfg == null) {
                    cmd.error("no such key");
                    return;
                }
                keydsa = cfg.key;
                return;
            }
            if (s.equals("ecdsakey")) {
                cfgKey<cryKeyECDSA> cfg = cfgAll.keyFind(cfgAll.ecdsakeys, cmd.word(), false);
                if (cfg == null) {
                    cmd.error("no such key");
                    return;
                }
                keyecdsa = cfg.key;
                return;
            }
            if (s.equals("mldsakey")) {
                cfgKey<cryKeyMLDSA> cfg = cfgAll.keyFind(cfgAll.mldsakeys, cmd.word(), false);
                if (cfg == null) {
                    cmd.error("no such key");
                    return;
                }
                keymldsa = cfg.key;
                return;
            }
            if (s.equals("rsacert")) {
                cfgCert cfg = cfgAll.certFind(cmd.word(), false);
                if (cfg == null) {
                    cmd.error("no such cert");
                    return;
                }
                certrsa = cfg.cert;
                return;
            }
            if (s.equals("dsacert")) {
                cfgCert cfg = cfgAll.certFind(cmd.word(), false);
                if (cfg == null) {
                    cmd.error("no such cert");
                    return;
                }
                certdsa = cfg.cert;
                return;
            }
            if (s.equals("ecdsacert")) {
                cfgCert cfg = cfgAll.certFind(cmd.word(), false);
                if (cfg == null) {
                    cmd.error("no such cert");
                    return;
                }
                certecdsa = cfg.cert;
                return;
            }
            if (s.equals("mldsacert")) {
                cfgCert cfg = cfgAll.certFind(cmd.word(), false);
                if (cfg == null) {
                    cmd.error("no such cert");
                    return;
                }
                certmldsa = cfg.cert;
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.startsWith("access-")) {
            a = a.substring(7, a.length());
            a += " " + cmd.getRemaining();
            a = a.trim();
            cmd = new cmds("info", a);
            srvIpInf = secInfoUtl.doCfgStr(srvIpInf, cmd, false);
            return;
        }
        if (!a.equals(cmds.negated)) {
            if (srvCfgStr(cmd.copyBytes(true))) {
                cmd.badCmd();
                return;
            }
            return;
        }
        a = cmd.word();
        if (a.equals(cmds.upgradeCli)) {
            srvEmbedVrf = false;
            return;
        }
        if (a.equals("description")) {
            srvDescr = null;
            return;
        }
        if (a.equals("vrf")) {
            srvDeinit();
            srvVrf = null;
            return;
        }
        if (a.equals("port")) {
            srvDeinit();
            srvPort = srvPort();
            srvInit();
            return;
        }
        if (a.equals("protocol")) {
            srvDeinit();
            srvProto = srvProto();
            srvInit();
            return;
        }
        if (a.equals("interface")) {
            srvDeinit();
            srvIface = null;
            srvInit();
            return;
        }
        if (a.equals("access-log")) {
            srvLogDrop = false;
            return;
        }
        if (a.equals("access-total")) {
            srvTotLim = 0;
            return;
        }
        if (a.equals("access-peer")) {
            srvPerLim = 0;
            return;
        }
        if (a.equals("access-subnet")) {
            srvNetLim = 0;
            return;
        }
        if (a.equals("access-blackhole4")) {
            srvBlckhl4 = null;
            return;
        }
        if (a.equals("access-blackhole6")) {
            srvBlckhl6 = null;
            return;
        }
        if (a.equals("security")) {
            String s = cmd.word();
            if (s.equals("protocol")) {
                secProto = 0;
                return;
            }
            if (s.equals("authentication")) {
                srvAuther = null;
                return;
            }
            if (s.equals("rsakey")) {
                keyrsa = null;
                return;
            }
            if (s.equals("dsakey")) {
                keydsa = null;
                return;
            }
            if (s.equals("ecdsakey")) {
                keyecdsa = null;
                return;
            }
            if (s.equals("mldsakey")) {
                keymldsa = null;
                return;
            }
            if (s.equals("rsacert")) {
                certrsa = null;
                return;
            }
            if (s.equals("dsacert")) {
                certdsa = null;
                return;
            }
            if (s.equals("ecdsacert")) {
                certecdsa = null;
                return;
            }
            if (s.equals("mldsacert")) {
                certmldsa = null;
                return;
            }
            return;
        }
        if (!a.startsWith("access-")) {
            if (srvCfgStr(cmd.copyBytes(true))) {
                cmd.badCmd();
                return;
            }
            return;
        }
        a = a.substring(7, a.length());
        a += " " + cmd.getRemaining();
        a = a.trim();
        cmd = new cmds("info", a);
        srvIpInf = secInfoUtl.doCfgStr(srvIpInf, cmd, true);
        return;
    }

    public int compareTo(servGeneric o) {
        return srvName.toLowerCase().compareTo(o.srvName.toLowerCase());
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
