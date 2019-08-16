package cfg;

import addr.addrIP;
import addr.addrIPv4;
import addr.addrIPv6;
import addr.addrType;
import auth.authLocal;
import clnt.clntNtp;
import clnt.clntProxy;
import cry.cryBase64;
import cry.cryKeyDSA;
import cry.cryKeyECDSA;
import cry.cryKeyGeneric;
import cry.cryKeyRSA;
import ifc.ifcBridge;
import ifc.ifcBundle;
import ifc.ifcHairpin;
import ifc.ifcThread;
import java.util.ArrayList;
import java.util.List;
import line.lineThread;
import pipe.pipeSide;
import serv.servBmp2mrt;
import serv.servBstun;
import serv.servCharGen;
import serv.servDaytime;
import serv.servDcp;
import serv.servDhcp4;
import serv.servDhcp6;
import serv.servDiscard;
import serv.servDns;
import serv.servEchoS;
import serv.servEtherIp;
import serv.servForwarder;
import serv.servFtp;
import serv.servGeneric;
import serv.servGenList;
import serv.servGopher;
import serv.servGtp;
import serv.servHttp;
import serv.servIrc;
import serv.servIscsi;
import serv.servL2f;
import serv.servL2tp2;
import serv.servL2tp3;
import serv.servLoadBalancer;
import serv.servLpd;
import serv.servNtp;
import serv.servPckOdtls;
import serv.servPckOtcp;
import serv.servPckOtxt;
import serv.servPckOudp;
import serv.servPop3;
import serv.servPptp;
import serv.servQuote;
import serv.servRadius;
import serv.servRfb;
import serv.servModem;
import serv.servSip;
import serv.servSmtp;
import serv.servSnmp;
import serv.servSocks;
import serv.servStun;
import serv.servSyslog;
import serv.servTacacs;
import serv.servTelnet;
import serv.servTftp;
import serv.servTime;
import serv.servUdptn;
import serv.servHoneyPot;
import serv.servRpki;
import serv.servVxlan;
import serv.servGeneve;
import serv.servGre;
import serv.servMplsIp;
import serv.servMplsUdp;
import serv.servNetflow;
import serv.servNrpe;
import serv.servOpenflow;
import serv.servP4lang;
import serv.servUpnpFwd;
import serv.servUpnpHub;
import serv.servVoice;
import tab.tabGen;
import tab.tabNshNtry;
import tab.tabRouteEntry;
import user.userFilter;
import user.userFormat;
import user.userHelping;
import user.userReload;
import user.userUpgrade;
import util.bits;
import util.chatter;
import util.cmds;
import util.logger;
import util.verCore;
import util.version;

/**
 * configuration settings
 *
 * @author matecsaba
 */
public class cfgAll {

    /**
     * list of vdcs
     */
    public static final tabGen<cfgVdc> vdcs = new tabGen<cfgVdc>();

    /**
     * list of processes
     */
    public static final tabGen<cfgPrcss> prcs = new tabGen<cfgPrcss>();

    /**
     * list of vrfs
     */
    public static final tabGen<cfgVrf> vrfs = new tabGen<cfgVrf>();

    /**
     * list of interfaces
     */
    public static final tabGen<cfgIfc> ifaces = new tabGen<cfgIfc>();

    /**
     * list of interfaces
     */
    public static final tabGen<cfgLin> lines = new tabGen<cfgLin>();

    /**
     * list of enus
     */
    public static final tabGen<cfgMenu> menus = new tabGen<cfgMenu>();

    /**
     * list of bridges
     */
    public static final tabGen<cfgBrdg> bridges = new tabGen<cfgBrdg>();

    /**
     * list of bundles
     */
    public static final tabGen<cfgBndl> bundles = new tabGen<cfgBndl>();

    /**
     * list of hairpins
     */
    public static final tabGen<cfgHrpn> hairpins = new tabGen<cfgHrpn>();

    /**
     * list of translation rules
     */
    public static final tabGen<cfgTrnsltn> trnsltns = new tabGen<cfgTrnsltn>();

    /**
     * list of dial peers
     */
    public static final tabGen<cfgDial> dials = new tabGen<cfgDial>();

    /**
     * list of routers
     */
    public static final tabGen<cfgRtr> routers = new tabGen<cfgRtr>();

    /**
     * list of network object groups
     */
    public static final tabGen<cfgObjnet> objgrpnets = new tabGen<cfgObjnet>();

    /**
     * list of port object groups
     */
    public static final tabGen<cfgObjprt> objgrpprts = new tabGen<cfgObjprt>();

    /**
     * list of access lists
     */
    public static final tabGen<cfgAceslst> accesslsts = new tabGen<cfgAceslst>();

    /**
     * list of event managers
     */
    public static final tabGen<cfgEvntmgr> eventmgrs = new tabGen<cfgEvntmgr>();

    /**
     * list of vpdns
     */
    public static final tabGen<cfgVpdn> vpdns = new tabGen<cfgVpdn>();

    /**
     * list of prefix lists
     */
    public static final tabGen<cfgPrfxlst> prefixlsts = new tabGen<cfgPrfxlst>();

    /**
     * list of route maps
     */
    public static final tabGen<cfgRoump> routemaps = new tabGen<cfgRoump>();

    /**
     * list of route policies
     */
    public static final tabGen<cfgRouplc> routeplcs = new tabGen<cfgRouplc>();

    /**
     * list of tine maps
     */
    public static final tabGen<cfgTime> timemaps = new tabGen<cfgTime>();

    /**
     * list of policy maps
     */
    public static final tabGen<cfgPlymp> policymaps = new tabGen<cfgPlymp>();

    /**
     * list of user lists
     */
    public static final tabGen<cfgAuther> authers = new tabGen<cfgAuther>();

    /**
     * list of schedulers
     */
    public static final tabGen<cfgSched> schedulers = new tabGen<cfgSched>();

    /**
     * list of scripts
     */
    public static final tabGen<cfgScrpt> scripts = new tabGen<cfgScrpt>();

    /**
     * list of trackers
     */
    public static final tabGen<cfgTrack> trackers = new tabGen<cfgTrack>();

    /**
     * list of mtrackers
     */
    public static final tabGen<cfgMtrack> mtrackers = new tabGen<cfgMtrack>();

    /**
     * list of certificates
     */
    public static final tabGen<cfgCert> certs = new tabGen<cfgCert>();

    /**
     * list of ipsec profiles
     */
    public static final tabGen<cfgIpsec> ipsecs = new tabGen<cfgIpsec>();

    /**
     * list of proxy profiles
     */
    public static final tabGen<cfgProxy> proxys = new tabGen<cfgProxy>();

    /**
     * list of chat scripts
     */
    public static final tabGen<cfgChat> chats = new tabGen<cfgChat>();

    /**
     * list of rsa keys
     */
    public static final tabGen<cfgKey<cryKeyRSA>> rsakeys = new tabGen<cfgKey<cryKeyRSA>>();

    /**
     * list of dsa keys
     */
    public static final tabGen<cfgKey<cryKeyDSA>> dsakeys = new tabGen<cfgKey<cryKeyDSA>>();

    /**
     * list of ecdsa keys
     */
    public static final tabGen<cfgKey<cryKeyECDSA>> ecdsakeys = new tabGen<cfgKey<cryKeyECDSA>>();

    /**
     * list of ipv4 pools
     */
    public static final tabGen<cfgPool<addrIPv4>> ip4pool = new tabGen<cfgPool<addrIPv4>>();

    /**
     * list of ipv6 pools
     */
    public static final tabGen<cfgPool<addrIPv6>> ip6pool = new tabGen<cfgPool<addrIPv6>>();

    /**
     * list of xconnections
     */
    public static final tabGen<cfgXconn> xconnects = new tabGen<cfgXconn>();

    /**
     * list of iconnections
     */
    public static final tabGen<cfgIconn> iconnects = new tabGen<cfgIconn>();

    /**
     * list of aliases
     */
    public static final tabGen<cfgAlias> aliases = new tabGen<cfgAlias>();

    /**
     * echo daemons
     */
    public static servGenList<servEchoS> dmnEcho = new servGenList<servEchoS>();

    /**
     * discard daemons
     */
    public static servGenList<servDiscard> dmnDiscard = new servGenList<servDiscard>();

    /**
     * quote daemons
     */
    public static servGenList<servQuote> dmnQuote = new servGenList<servQuote>();

    /**
     * chargen daemons
     */
    public static servGenList<servCharGen> dmnCharGen = new servGenList<servCharGen>();

    /**
     * netflow daemons
     */
    public static servGenList<servNetflow> dmnNetflow = new servGenList<servNetflow>();

    /**
     * upnpfwd daemons
     */
    public static servGenList<servUpnpFwd> dmnUpnpFwd = new servGenList<servUpnpFwd>();

    /**
     * upnphub daemons
     */
    public static servGenList<servUpnpHub> dmnUpnpHub = new servGenList<servUpnpHub>();

    /**
     * openflow daemons
     */
    public static servGenList<servOpenflow> dmnOpenflow = new servGenList<servOpenflow>();

    /**
     * p4lang daemons
     */
    public static servGenList<servP4lang> dmnP4lang = new servGenList<servP4lang>();

    /**
     * forwarder daemons
     */
    public static servGenList<servForwarder> dmnForwarder = new servGenList<servForwarder>();

    /**
     * syslog daemons
     */
    public static servGenList<servSyslog> dmnSyslog = new servGenList<servSyslog>();

    /**
     * loadbalancer daemons
     */
    public static servGenList<servLoadBalancer> dmnLoadBalancer = new servGenList<servLoadBalancer>();

    /**
     * telnet daemons
     */
    public static servGenList<servTelnet> dmnTelnet = new servGenList<servTelnet>();

    /**
     * rfb daemons
     */
    public static servGenList<servRfb> dmnRfb = new servGenList<servRfb>();

    /**
     * udptn daemons
     */
    public static servGenList<servUdptn> dmnUdptn = new servGenList<servUdptn>();

    /**
     * http daemons
     */
    public static servGenList<servHttp> dmnHttp = new servGenList<servHttp>();

    /**
     * lpd daemons
     */
    public static servGenList<servLpd> dmnLpd = new servGenList<servLpd>();

    /**
     * honeypot daemons
     */
    public static servGenList<servHoneyPot> dmnHoney = new servGenList<servHoneyPot>();

    /**
     * dhcp4 daemons
     */
    public static servGenList<servDhcp4> dmnDhcp4 = new servGenList<servDhcp4>();

    /**
     * dhcp6 daemons
     */
    public static servGenList<servDhcp6> dmnDhcp6 = new servGenList<servDhcp6>();

    /**
     * dns daemons
     */
    public static servGenList<servDns> dmnDns = new servGenList<servDns>();

    /**
     * pop3 daemons
     */
    public static servGenList<servPop3> dmnPop3 = new servGenList<servPop3>();

    /**
     * smtp daemons
     */
    public static servGenList<servSmtp> dmnSmtp = new servGenList<servSmtp>();

    /**
     * sip modem daemons
     */
    public static servGenList<servModem> dmnModem = new servGenList<servModem>();

    /**
     * sip voice daemons
     */
    public static servGenList<servVoice> dmnVoice = new servGenList<servVoice>();

    /**
     * sip proxy daemons
     */
    public static servGenList<servSip> dmnSip = new servGenList<servSip>();

    /**
     * ftp daemons
     */
    public static servGenList<servFtp> dmnFtp = new servGenList<servFtp>();

    /**
     * tftp daemons
     */
    public static servGenList<servTftp> dmnTftp = new servGenList<servTftp>();

    /**
     * gopher daemons
     */
    public static servGenList<servGopher> dmnGopher = new servGenList<servGopher>();

    /**
     * ntp daemons
     */
    public static servGenList<servNtp> dmnNtp = new servGenList<servNtp>();

    /**
     * daytime daemons
     */
    public static servGenList<servDaytime> dmnDaytime = new servGenList<servDaytime>();

    /**
     * time daemons
     */
    public static servGenList<servTime> dmnTime = new servGenList<servTime>();

    /**
     * snmp daemons
     */
    public static servGenList<servSnmp> dmnSnmp = new servGenList<servSnmp>();

    /**
     * iscsi daemons
     */
    public static servGenList<servIscsi> dmnIscsi = new servGenList<servIscsi>();

    /**
     * bmp2mrt daemons
     */
    public static servGenList<servBmp2mrt> dmnBmp = new servGenList<servBmp2mrt>();

    /**
     * irc daemons
     */
    public static servGenList<servIrc> dmnIrc = new servGenList<servIrc>();

    /**
     * dcp daemons
     */
    public static servGenList<servDcp> dmnDcp = new servGenList<servDcp>();

    /**
     * socks daemons
     */
    public static servGenList<servSocks> dmnSocks = new servGenList<servSocks>();

    /**
     * rpki daemons
     */
    public static servGenList<servRpki> dmnRpki = new servGenList<servRpki>();

    /**
     * nrpe daemons
     */
    public static servGenList<servNrpe> dmnNrpe = new servGenList<servNrpe>();

    /**
     * bstun daemons
     */
    public static servGenList<servBstun> dmnBStun = new servGenList<servBstun>();

    /**
     * stun daemons
     */
    public static servGenList<servStun> dmnStun = new servGenList<servStun>();

    /**
     * pckoudp daemons
     */
    public static servGenList<servPckOudp> dmnPckOudp = new servGenList<servPckOudp>();

    /**
     * pckodtls daemons
     */
    public static servGenList<servPckOdtls> dmnPckOdtls = new servGenList<servPckOdtls>();

    /**
     * pckotcp daemons
     */
    public static servGenList<servPckOtcp> dmnPckOtcp = new servGenList<servPckOtcp>();

    /**
     * pckotxt daemons
     */
    public static servGenList<servPckOtxt> dmnPckOtxt = new servGenList<servPckOtxt>();

    /**
     * vxlan daemons
     */
    public static servGenList<servVxlan> dmnVxlan = new servGenList<servVxlan>();

    /**
     * geneve daemons
     */
    public static servGenList<servGeneve> dmnGeneve = new servGenList<servGeneve>();

    /**
     * l2f daemons
     */
    public static servGenList<servL2f> dmnL2f = new servGenList<servL2f>();

    /**
     * l2tp2 daemons
     */
    public static servGenList<servL2tp2> dmnL2tp2 = new servGenList<servL2tp2>();

    /**
     * l2tp3 daemons
     */
    public static servGenList<servL2tp3> dmnL2tp3 = new servGenList<servL2tp3>();

    /**
     * etherip daemons
     */
    public static servGenList<servEtherIp> dmnEtherIp = new servGenList<servEtherIp>();

    /**
     * gre daemons
     */
    public static servGenList<servGre> dmnGre = new servGenList<servGre>();

    /**
     * mplsip daemons
     */
    public static servGenList<servMplsIp> dmnMplsIp = new servGenList<servMplsIp>();

    /**
     * mplsudp daemons
     */
    public static servGenList<servMplsUdp> dmnMplsUdp = new servGenList<servMplsUdp>();

    /**
     * gtp daemons
     */
    public static servGenList<servGtp> dmnGtp = new servGenList<servGtp>();

    /**
     * pptp daemons
     */
    public static servGenList<servPptp> dmnPptp = new servGenList<servPptp>();

    /**
     * radius daemons
     */
    public static servGenList<servRadius> dmnRadius = new servGenList<servRadius>();

    /**
     * tacacs daemons
     */
    public static servGenList<servTacacs> dmnTacacs = new servGenList<servTacacs>();

    /**
     * name of this host
     */
    public static String hostName = "router";

    /**
     * encryption of this host
     */
    public static String passEnc = null;

    /**
     * password of this host
     */
    public static byte[] enaPass = null;

    /**
     * banner of this host
     */
    public static byte[] banner = new byte[0];

    /**
     * password stars
     */
    public static boolean passwdStars = false;

    /**
     * prefer ipv6
     */
    public static boolean preferIpv6 = false;

    /**
     * graceful reload
     */
    public static boolean graceReload = true;

    /**
     * whois server
     */
    public static String whoisServer = null;

    /**
     * chatter
     */
    public static final chatter chat = new chatter();

    /**
     * client proxy settings
     */
    public static cfgProxy clientProxy;

    /**
     * proxy of name server
     */
    public static cfgProxy nameServerProxy;

    /**
     * address of name server
     */
    public static List<addrIP> nameServerAddr = new ArrayList<addrIP>();

    /**
     * name of time server
     */
    public static clntNtp timeServerName;

    /**
     * name of time zone
     */
    public static String timeZoneName = "Z";

    /**
     * last offset to time server
     */
    public static long timeServerOffset;

    /**
     * name of mail server
     */
    public static String mailServerName;

    /**
     * username for mail server
     */
    public static String mailServerUser;

    /**
     * password for mail server
     */
    public static String mailServerPass;

    /**
     * upgrade public key
     */
    public static String upgradePubKey = null;

    /**
     * upgrade server url
     */
    public static String upgradeServer = verCore.homeUrl;

    /**
     * upgrade backup files
     */
    public static boolean upgradeBackup = false;

    /**
     * upgrade config save
     */
    public static boolean upgradeConfig = false;

    /**
     * upgrade just own key
     */
    public static boolean upgradeOwnKey = false;

    /**
     * passive mode ftp
     */
    public static boolean ftpPassive = true;

    /**
     * unreachable last
     */
    public static long unreachLst = 0;

    /**
     * unreachable interval
     */
    public static int unreachInt = 0;

    /**
     * ruin remote pmtud
     */
    public static boolean ruinPmtuD = false;

    /**
     * ipv4 checksum tx
     */
    public static boolean ipv4ChecksumTx = true;

    /**
     * ipv4 checksum rx
     */
    public static boolean ipv4ChecksumRx = true;

    /**
     * icmp4 checksum tx
     */
    public static boolean icmp4ChecksumTx = true;

    /**
     * icmp4 checksum rx
     */
    public static boolean icmp4ChecksumRx = true;

    /**
     * icmp6 checksum tx
     */
    public static boolean icmp6ChecksumTx = true;

    /**
     * icmp6 checksum rx
     */
    public static boolean icmp6ChecksumRx = true;

    /**
     * udp checksum tx
     */
    public static boolean udpChecksumTx = true;

    /**
     * udp checksum rx
     */
    public static boolean udpChecksumRx = true;

    /**
     * ludp checksum tx
     */
    public static boolean ludpChecksumTx = true;

    /**
     * ludp checksum rx
     */
    public static boolean ludpChecksumRx = true;

    /**
     * tcp checksum tx
     */
    public static boolean tcpChecksumTx = true;

    /**
     * tcp checksum rx
     */
    public static boolean tcpChecksumRx = true;

    /**
     * dccp checksum tx
     */
    public static boolean dccpChecksumTx = true;

    /**
     * dccp checksum rx
     */
    public static boolean dccpChecksumRx = true;

    /**
     * sctp checksum tx
     */
    public static boolean sctpChecksumTx = true;

    /**
     * sctp checksum rx
     */
    public static boolean sctpChecksumRx = true;

    /**
     * name of config server
     */
    public static String configServer;

    /**
     * username on config server
     */
    public static String configUser;

    /**
     * password on config server
     */
    public static String configPass;

    /**
     * automatically save configuration
     */
    public static boolean configAsave;

    /**
     * automatically backup configuration
     */
    public static boolean configAbackup;

    /**
     * backup startup configuration
     */
    public static String configBackup;

    /**
     * next reload time
     */
    public static userReload reload;

    /**
     * allow only one user to configuration
     */
    public static int configExclusive;

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        // logging
        "!logging buffered debug 512",
        "!logging monitor debug",
        "!logging format normal",
        "!no logging rotate 0",
        "!no logging syslog debug kernel",
        "!no logging file debug",
        "!no logging irc debug",
        "!banner encoded ",
        "!no password-encrypt",
        "!no enable",
        // client
        "!no client proxy",
        "!no client name-proxy",
        "!client name-server",
        "!client time-zone Z",
        "!no client time-server",
        "!no client upgrade-pubkey",
        "!client upgrade-server " + verCore.homeUrl,
        "!no client upgrade-config",
        "!no client upgrade-backup",
        "!no client upgrade-ownkey",
        "!no client whois-server",
        "!no client config-server",
        "!no client config-username",
        "!no client config-password",
        "!no client config-save",
        "!no client config-archive",
        "!no client config-backup",
        "!no client config-exclusive",
        "!no client mail-server",
        "!no client mail-username",
        "!no client mail-password",
        "!no client prefer-ipv6",
        "!no client password-stars",
        "!client graceful-reload",
        "!client ftp-passive",
        "!client unreach-interval 0",
        "!no client punish-pmtud",
        "!client ipv4-checksum both",
        "!client icmp4-checksum both",
        "!client icmp6-checksum both",
        "!client udp-checksum both",
        "!client tcp-checksum both",
        "!client ludp-checksum both",
        "!client dccp-checksum both",
        "!client sctp-checksum both",};

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    private cfgAll() {
    }

    /**
     * get client vrf
     *
     * @return client vrf, null if nothing
     */
    public static cfgVrf getClntVrf() {
        if (clientProxy == null) {
            return null;
        }
        return clientProxy.getVrf();
    }

    /**
     * get client interface
     *
     * @return client interface, null if nothing
     */
    public static cfgIfc getClntIfc() {
        if (clientProxy == null) {
            return null;
        }
        return clientProxy.getIfc();
    }

    /**
     * get client proxy
     *
     * @return client proxy, null if nothing
     */
    public static clntProxy getClntPrx() {
        if (clientProxy == null) {
            return null;
        }
        return clientProxy.proxy;
    }

    /**
     * open one connection
     *
     * @param proto protocol to use from servGeneric
     * @param addr target address
     * @param port target port
     * @param name client name
     * @return pipeline, null on error
     */
    public static pipeSide clntConnect(int proto, addrIP addr, int port, String name) {
        if (clientProxy == null) {
            return null;
        }
        return clientProxy.doConnect(proto, addr, port, name);
    }

    /**
     * find vrf by rd
     *
     * @param rd rd to find
     * @return vrf, null on error
     */
    public static cfgVrf findRd(long rd) {
        for (int i = 0; i < vrfs.size(); i++) {
            cfgVrf ntry = vrfs.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.rd == rd) {
                return ntry;
            }
        }
        return null;
    }

    /**
     * find one user list
     *
     * @param nam name of this
     * @param create type of list, null=do not create
     * @return descriptor, null if not found
     */
    public static cfgAuther autherFind(String nam, cfgAuther.methodType create) {
        nam = nam.trim();
        if (nam.length() < 1) {
            return null;
        }
        cfgAuther ntry = new cfgAuther(nam, create);
        if (create == null) {
            return authers.find(ntry);
        }
        cfgAuther old = authers.add(ntry);
        if (old != null) {
            return old;
        }
        return ntry;
    }

    /**
     * delete one vrf
     *
     * @param nam name of this
     * @return descriptor, null if not found
     */
    public static cfgAuther autherDel(String nam) {
        cfgAuther ntry = new cfgAuther(nam, null);
        return authers.del(ntry);
    }

    /**
     * find one scheduler
     *
     * @param nam name of entry
     * @param create create new on this number if not found
     * @return descriptor, null if not found
     */
    public static cfgSched schedFind(String nam, boolean create) {
        nam = nam.trim();
        if (nam.length() < 1) {
            return null;
        }
        cfgSched ntry = new cfgSched();
        ntry.name = nam;
        if (!create) {
            return schedulers.find(ntry);
        }
        cfgSched old = schedulers.add(ntry);
        if (old != null) {
            return old;
        }
        return ntry;
    }

    /**
     * delete one scheduler
     *
     * @param nam name of entry
     * @return descriptor, null if not found
     */
    public static cfgSched schedDel(String nam) {
        cfgSched ntry = new cfgSched();
        ntry.name = nam;
        return schedulers.del(ntry);
    }

    /**
     * find one script
     *
     * @param nam name of entry
     * @param create create new on this number if not found
     * @return descriptor, null if not found
     */
    public static cfgScrpt scrptFind(String nam, boolean create) {
        nam = nam.trim();
        if (nam.length() < 1) {
            return null;
        }
        cfgScrpt ntry = new cfgScrpt();
        ntry.name = nam;
        if (!create) {
            return scripts.find(ntry);
        }
        cfgScrpt old = scripts.add(ntry);
        if (old != null) {
            return old;
        }
        return ntry;
    }

    /**
     * delete one script
     *
     * @param nam name of entry
     * @return descriptor, null if not found
     */
    public static cfgScrpt scrptDel(String nam) {
        cfgScrpt ntry = new cfgScrpt();
        ntry.name = nam;
        return scripts.del(ntry);
    }

    /**
     * find one tracker
     *
     * @param nam name of entry
     * @param create create new on this number if not found
     * @return descriptor, null if not found
     */
    public static cfgTrack trackFind(String nam, boolean create) {
        nam = nam.trim();
        if (nam.length() < 1) {
            return null;
        }
        cfgTrack ntry = new cfgTrack();
        ntry.name = nam;
        ntry.worker.name = nam;
        if (!create) {
            return trackers.find(ntry);
        }
        cfgTrack old = trackers.add(ntry);
        if (old != null) {
            return old;
        }
        return ntry;
    }

    /**
     * delete one tracker
     *
     * @param nam name of entry
     * @return descriptor, null if not found
     */
    public static cfgTrack trackDel(String nam) {
        cfgTrack ntry = new cfgTrack();
        ntry.name = nam;
        return trackers.del(ntry);
    }

    /**
     * find one mtracker
     *
     * @param nam name of entry
     * @param create create new on this number if not found
     * @return descriptor, null if not found
     */
    public static cfgMtrack mtrackFind(String nam, boolean create) {
        nam = nam.trim();
        if (nam.length() < 1) {
            return null;
        }
        cfgMtrack ntry = new cfgMtrack();
        ntry.name = nam;
        ntry.worker.name = nam;
        if (!create) {
            return mtrackers.find(ntry);
        }
        cfgMtrack old = mtrackers.add(ntry);
        if (old != null) {
            return old;
        }
        return ntry;
    }

    /**
     * delete one mtracker
     *
     * @param nam name of entry
     * @return descriptor, null if not found
     */
    public static cfgMtrack mtrackDel(String nam) {
        cfgMtrack ntry = new cfgMtrack();
        ntry.name = nam;
        return mtrackers.del(ntry);
    }

    /**
     * find one certificate
     *
     * @param nam name of entry
     * @param create create new on this number if not found
     * @return descriptor, null if not found
     */
    public static cfgCert certFind(String nam, boolean create) {
        nam = nam.trim();
        if (nam.length() < 1) {
            return null;
        }
        cfgCert ntry = new cfgCert(nam);
        if (!create) {
            return certs.find(ntry);
        }
        cfgCert old = certs.add(ntry);
        if (old != null) {
            return old;
        }
        return ntry;
    }

    /**
     * delete one certificate
     *
     * @param nam name of entry
     * @return descriptor, null if not found
     */
    public static cfgCert certDel(String nam) {
        cfgCert ntry = new cfgCert(nam);
        return certs.del(ntry);
    }

    /**
     * find one ipsec profile
     *
     * @param nam name of entry
     * @param create create new on this number if not found
     * @return descriptor, null if not found
     */
    public static cfgIpsec ipsecFind(String nam, boolean create) {
        nam = nam.trim();
        if (nam.length() < 1) {
            return null;
        }
        cfgIpsec ntry = new cfgIpsec(nam);
        if (!create) {
            return ipsecs.find(ntry);
        }
        cfgIpsec old = ipsecs.add(ntry);
        if (old != null) {
            return old;
        }
        return ntry;
    }

    /**
     * delete one ipsec profile
     *
     * @param nam name of entry
     * @return descriptor, null if not found
     */
    public static cfgIpsec ipsecDel(String nam) {
        cfgIpsec ntry = new cfgIpsec(nam);
        return ipsecs.del(ntry);
    }

    /**
     * find one proxy map
     *
     * @param nam name of entry
     * @param create create new on this number if not found
     * @return descriptor, null if not found
     */
    public static cfgProxy proxyFind(String nam, boolean create) {
        nam = nam.trim();
        if (nam.length() < 1) {
            return null;
        }
        cfgProxy ntry = new cfgProxy(nam);
        if (!create) {
            return proxys.find(ntry);
        }
        cfgProxy old = proxys.add(ntry);
        if (old != null) {
            return old;
        }
        return ntry;
    }

    /**
     * delete one proxy map
     *
     * @param nam name of entry
     * @return descriptor, null if not found
     */
    public static cfgProxy proxyDel(String nam) {
        cfgProxy ntry = new cfgProxy(nam);
        return proxys.del(ntry);
    }

    /**
     * find one chat script
     *
     * @param nam name of entry
     * @param create create new on this number if not found
     * @return descriptor, null if not found
     */
    public static cfgChat chatFind(String nam, boolean create) {
        nam = nam.trim();
        if (nam.length() < 1) {
            return null;
        }
        cfgChat ntry = new cfgChat(nam);
        if (!create) {
            return chats.find(ntry);
        }
        cfgChat old = chats.add(ntry);
        if (old != null) {
            return old;
        }
        return ntry;
    }

    /**
     * delete one chat script
     *
     * @param nam name of entry
     * @return descriptor, null if not found
     */
    public static cfgChat chatDel(String nam) {
        cfgChat ntry = new cfgChat(nam);
        return chats.del(ntry);
    }

    /**
     * find one key
     *
     * @param <T> exact type
     * @param lst list of entries
     * @param nam name of entry
     * @param create create new on this number if not found
     * @return descriptor, null if not found
     */
    public static <T extends cryKeyGeneric> cfgKey<T> keyFind(
            tabGen<cfgKey<T>> lst, String nam, boolean create) {
        nam = nam.trim();
        if (nam.length() < 1) {
            return null;
        }
        cfgKey<T> ntry = new cfgKey<T>();
        ntry.name = nam;
        if (!create) {
            return lst.find(ntry);
        }
        cfgKey<T> old = lst.add(ntry);
        if (old != null) {
            return old;
        }
        return ntry;
    }

    /**
     * delete one key list
     *
     * @param <T> exact type
     * @param lst list of entries
     * @param nam name of entry
     * @return descriptor, null if not found
     */
    public static <T extends cryKeyGeneric> cfgKey<T> keyDel(
            tabGen<cfgKey<T>> lst, String nam) {
        cfgKey<T> ntry = new cfgKey<T>();
        ntry.name = nam;
        return lst.del(ntry);
    }

    /**
     * find one pool
     *
     * @param <T> exact type
     * @param lst list of entries
     * @param nam name of entry
     * @param create create new on this number if not found
     * @return descriptor, null if not found
     */
    public static <T extends addrType> cfgPool<T> poolFind(
            tabGen<cfgPool<T>> lst, String nam, boolean create) {
        nam = nam.trim();
        if (nam.length() < 1) {
            return null;
        }
        cfgPool<T> ntry = new cfgPool<T>();
        ntry.name = nam;
        if (!create) {
            return lst.find(ntry);
        }
        cfgPool<T> old = lst.add(ntry);
        if (old != null) {
            return old;
        }
        return ntry;
    }

    /**
     * delete one pool list
     *
     * @param <T> exact type
     * @param lst list of entries
     * @param nam name of entry
     * @return descriptor, null if not found
     */
    public static <T extends addrType> cfgPool<T> poolDel(
            tabGen<cfgPool<T>> lst, String nam) {
        cfgPool<T> ntry = new cfgPool<T>();
        ntry.name = nam;
        return lst.del(ntry);
    }

    /**
     * find one alias
     *
     * @param nam name of entry
     * @param typ type of entry
     * @param create create new on this number if not found
     * @return descriptor, null if not found
     */
    public static cfgAlias aliasFind(String nam, cfgAlias.aliasType typ,
            boolean create) {
        nam = nam.trim();
        if (nam.length() < 1) {
            return null;
        }
        cfgAlias ntry = new cfgAlias();
        ntry.name = nam;
        ntry.type = typ;
        if (!create) {
            return aliases.find(ntry);
        }
        cfgAlias old = aliases.add(ntry);
        if (old != null) {
            return old;
        }
        return ntry;
    }

    /**
     * delete one alias
     *
     * @param nam name of entry
     * @param typ type of entry
     * @return descriptor, null if not found
     */
    public static cfgAlias aliasDel(String nam, cfgAlias.aliasType typ) {
        cfgAlias ntry = new cfgAlias();
        ntry.name = nam;
        ntry.type = typ;
        return aliases.del(ntry);
    }

    /**
     * get helping texts
     *
     * @param typ type to get
     * @param lev beginning help level
     * @param hlp help text to append
     */
    public static void aliasHelps(cfgAlias.aliasType typ, int lev,
            userHelping hlp) {
        for (int i = 0; i < aliases.size(); i++) {
            cfgAlias ntry = aliases.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.type != typ) {
                continue;
            }
            ntry.getLines(hlp, lev);
        }
    }

    /**
     * find one vdc
     *
     * @param nam name of this
     * @param create create new on this name if not found
     * @return descriptor, null if not found
     */
    public static cfgVdc vdcFind(String nam, boolean create) {
        nam = nam.trim();
        if (nam.length() < 1) {
            return null;
        }
        cfgVdc ntry = new cfgVdc(nam);
        if (!create) {
            return vdcs.find(ntry);
        }
        cfgVdc old = vdcs.add(ntry);
        if (old != null) {
            return old;
        }
        return ntry;
    }

    /**
     * delete one vdc
     *
     * @param nam name of this
     * @return descriptor, null if not found
     */
    public static cfgVdc vdcDel(String nam) {
        cfgVdc ntry = new cfgVdc(nam);
        ntry = vdcs.del(ntry);
        if (ntry == null) {
            return null;
        }
        for (int i = 0; i < ntry.conns.size(); i++) {
            ntry.delConn(ntry.conns.get(i).name);
        }
        return ntry;
    }

    /**
     * delete interface from all vdcs
     *
     * @param ifc interface to delete
     */
    protected static void vdcNoIfc(cfgVdcIfc ifc) {
        for (int i = 0; i < vdcs.size(); i++) {
            vdcs.get(i).ifaces.del(ifc);
        }
    }

    /**
     * find one process
     *
     * @param nam name of this
     * @param create create new on this name if not found
     * @return descriptor, null if not found
     */
    public static cfgPrcss prcFind(String nam, boolean create) {
        nam = nam.trim();
        if (nam.length() < 1) {
            return null;
        }
        cfgPrcss ntry = new cfgPrcss(nam);
        if (!create) {
            return prcs.find(ntry);
        }
        cfgPrcss old = prcs.add(ntry);
        if (old != null) {
            return old;
        }
        return ntry;
    }

    /**
     * delete one process
     *
     * @param nam name of this
     * @return descriptor, null if not found
     */
    public static cfgPrcss prcDel(String nam) {
        cfgPrcss ntry = new cfgPrcss(nam);
        ntry = prcs.del(ntry);
        if (ntry == null) {
            return null;
        }
        return ntry;
    }

    /**
     * find one vrf
     *
     * @param nam name of this
     * @param create create new on this name if not found
     * @return descriptor, null if not found
     */
    public static cfgVrf vrfFind(String nam, boolean create) {
        nam = nam.trim();
        if (nam.length() < 1) {
            return null;
        }
        cfgVrf ntry = new cfgVrf(nam);
        if (!create) {
            return vrfs.find(ntry);
        }
        cfgVrf old = vrfs.add(ntry);
        if (old != null) {
            return old;
        }
        ntry.startNow();
        return ntry;
    }

    /**
     * delete one vrf
     *
     * @param nam name of this
     * @return descriptor, null if not found
     */
    public static cfgVrf vrfDel(String nam) {
        cfgVrf ntry = new cfgVrf(nam);
        ntry = vrfs.del(ntry);
        if (ntry == null) {
            return null;
        }
        for (int i = ifaces.size() - 1; i >= 0; i--) {
            cfgIfc ifc = ifaces.get(i);
            if (ifc.vrfFor == null) {
                continue;
            }
            if (ifc.vrfFor.fwd4.vrfNum != ntry.fwd4.vrfNum) {
                continue;
            }
            ifc.clear2vrf();
        }
        ntry.closeConns();
        ntry.closeUp();
        return ntry;
    }

    /**
     * add one physical interface
     *
     * @param nam name of interface
     * @param typ type of interface
     * @param thrd interface thread handler
     * @param wrkr worker threads
     * @return descriptor
     */
    public static cfgIfc ifcAdd(String nam, cfgIfc.ifaceType typ, ifcThread thrd, int wrkr) {
        nam = cfgIfc.normName(nam, false);
        if (thrd != null) {
            thrd.startLoop(wrkr);
        }
        cfgIfc ntry = new cfgIfc(nam);
        ntry.type = typ;
        ntry.thread = thrd;
        ifaces.add(ntry);
        return ntry;
    }

    /**
     * delete one interface
     *
     * @param nam name of interface
     * @param checks enable checks
     * @return false if successful, true on error
     */
    public static boolean ifcDel(String nam, boolean checks) {
        nam = cfgIfc.normName(nam);
        if (nam.length() < 1) {
            return true;
        }
        cfgIfc ntry = new cfgIfc(nam);
        if (checks) {
            ntry = ifaces.find(ntry);
            if (ntry == null) {
                return true;
            }
            if (ntry.thread != null) {
                return true;
            }
            if ((ntry.bridgeHed != null) && (ntry.bridgeIfc == null)) {
                return true;
            }
            if ((ntry.bundleHed != null) && (ntry.bundleIfc == null)) {
                return true;
            }
        }
        ntry = ifaces.del(ntry);
        if (ntry == null) {
            return true;
        }
        ntry.clear2router(ntry.rtrRip4hnd);
        ntry.clear2router(ntry.rtrRip6hnd);
        ntry.clear2router(ntry.rtrBabel4hnd);
        ntry.clear2router(ntry.rtrBabel6hnd);
        ntry.clear2router(ntry.rtrOspf4hnd);
        ntry.clear2router(ntry.rtrOspf6hnd);
        ntry.clear2router(ntry.rtrIsisHnd);
        ntry.clear2router(ntry.rtrPvrp4hnd);
        ntry.clear2router(ntry.rtrPvrp6hnd);
        ntry.clear2router(ntry.rtrLsrp4hnd);
        ntry.clear2router(ntry.rtrLsrp6hnd);
        ntry.clear2router(ntry.rtrEigrp4hnd);
        ntry.clear2router(ntry.rtrEigrp6hnd);
        ntry.clear2mpls();
        ntry.clear2ldp(4);
        ntry.clear2ldp(6);
        ntry.clear2rsvp(4);
        ntry.clear2rsvp(6);
        ntry.clear2vrf();
        ntry.clear2tunnel(true);
        ntry.clear2bridge();
        ntry.clear2bundle();
        ntry.clear2xconnect();
        ntry.clear2evcs();
        ntry.clear2transproxy();
        ntry.setup2eapolClnt(null, null);
        ntry.setup2eapolServ(null);
        ntry.setup2pppoeClnt(null);
        ntry.setup2pppoeServ(null);
        ntry.setup2pppoeRely(null);
        ntry.clear2pseudowire();
        return false;
    }

    /**
     * find one interface
     *
     * @param nam name of this
     * @param create create new on this name if not found
     * @return descriptor, null if not found
     */
    public static cfgIfc ifcFind(String nam, boolean create) {
        nam = nam.trim();
        if (nam.length() < 1) {
            return null;
        }
        nam = cfgIfc.normName(nam);
        cfgIfc ntry = new cfgIfc(nam);
        if (!create) {
            return ifaces.find(ntry);
        }
        cfgIfc prnt = ifaces.find(ntry);
        if (prnt != null) {
            return prnt;
        }
        ntry.name = cfgIfc.normName(nam, false);
        prnt = ifaces.find(ntry);
        if (prnt == null) {
            ntry.name = nam;
            if (nam.startsWith("tunnel")) {
                ntry.type = cfgIfc.ifaceType.tunnel;
                ifaces.add(ntry);
                ntry.clear2tunnel(false);
                return ntry;
            }
            if (nam.startsWith("dialer")) {
                ntry.type = cfgIfc.ifaceType.dialer;
                ifaces.add(ntry);
                ntry.initPhysical();
                return ntry;
            }
            if (nam.startsWith("sdn")) {
                ntry.type = cfgIfc.ifaceType.sdn;
                ifaces.add(ntry);
                ntry.initPhysical();
                return ntry;
            }
            if (nam.startsWith("pwether")) {
                ntry.type = cfgIfc.ifaceType.pweth;
                ifaces.add(ntry);
                ntry.initPhysical();
                return ntry;
            }
            if (nam.startsWith("virtualppp")) {
                ntry.type = cfgIfc.ifaceType.virtppp;
                ifaces.add(ntry);
                ntry.initPhysical();
                return ntry;
            }
            if (nam.startsWith("loopback")) {
                ntry.type = cfgIfc.ifaceType.loopback;
                ifaces.add(ntry);
                ntry.initLoopback();
                return ntry;
            }
            if (nam.startsWith("null")) {
                ntry.type = cfgIfc.ifaceType.nul;
                ifaces.add(ntry);
                ntry.initTemplate();
                return ntry;
            }
            if (nam.startsWith("template")) {
                ntry.type = cfgIfc.ifaceType.template;
                ifaces.add(ntry);
                ntry.initTemplate();
                return ntry;
            }
            if (nam.startsWith("access")) {
                ntry.type = cfgIfc.ifaceType.dialer;
                ifaces.add(ntry);
                ntry.initPhysical();
                return ntry;
            }
            return null;
        }
        if (prnt.parent != null) {
            return null;
        }
        ntry.name = nam;
        int i = bits.str2num(cfgIfc.normName(nam, true));
        if (i < 1) {
            return null;
        }
        ntry.vlanNum = i;
        ifaces.add(ntry);
        ntry.initSubiface(prnt);
        return ntry;
    }

    /**
     * reregister subinterfaces
     *
     * @param iface
     */
    public synchronized static void regSubifaces(cfgIfc iface) {
        for (int i = 0; i < ifaces.size(); i++) {
            cfgIfc ifc = ifaces.get(i);
            if (ifc.parent == null) {
                continue;
            }
            if (!ifc.parent.name.equals(iface.name)) {
                continue;
            }
            ifc.initSubiface(iface);
        }
    }

    /**
     * template configuration
     *
     * @param temp template interface
     * @param cmd configuration command
     */
    public synchronized static void templateConfig(cfgIfc temp, cmds cmd) {
        List<String> l = new ArrayList<String>();
        l.add("interface " + temp.name);
        l.add(" " + cmd.getRemaining().trim());
        l = userFilter.filterText(l, cfgIfc.notemplF);
        if (l.size() < 2) {
            return;
        }
        for (int i = 0; i < ifaces.size(); i++) {
            cfgIfc ifc = ifaces.get(i);
            if (ifc.template == null) {
                continue;
            }
            if (!ifc.template.name.equals(temp.name)) {
                continue;
            }
            ifc.doCfgStr(cmd.copyBytes(false));
        }
    }

    /**
     * add one physical line
     *
     * @param nam name of line
     * @param thrd line thread handler
     * @return descriptor
     */
    public static cfgLin linAdd(String nam, lineThread thrd) {
        cfgLin ntry = new cfgLin(nam);
        ntry.thread = thrd;
        lines.add(ntry);
        ntry.setup2run();
        return ntry;
    }

    /**
     * find one line
     *
     * @param nam name of this
     * @return descriptor, null if not found
     */
    public static cfgLin linFind(String nam) {
        nam = nam.trim();
        if (nam.length() < 1) {
            return null;
        }
        cfgLin ntry = new cfgLin(nam);
        return lines.find(ntry);
    }

    /**
     * add one physical line
     *
     * @param nam name of line
     * @return descriptor
     */
    public static cfgMenu menuDel(String nam) {
        cfgMenu ntry = new cfgMenu(nam);
        return menus.del(ntry);
    }

    /**
     * find one line
     *
     * @param nam name of this
     * @param create create if needed
     * @return descriptor, null if not found
     */
    public static cfgMenu menuFind(String nam, boolean create) {
        nam = nam.trim();
        if (nam.length() < 1) {
            return null;
        }
        cfgMenu ntry = new cfgMenu(nam);
        ntry = menus.find(ntry);
        if (!create) {
            return ntry;
        }
        if (ntry != null) {
            return ntry;
        }
        ntry = new cfgMenu(nam);
        menus.add(ntry);
        return ntry;
    }

    /**
     * find one access list
     *
     * @param nam name of entry
     * @param create create new on this number if not found
     * @return descriptor, null if not found
     */
    public static cfgVpdn vpdnFind(String nam, boolean create) {
        nam = nam.trim();
        if (nam.length() < 1) {
            return null;
        }
        cfgVpdn ntry = new cfgVpdn();
        ntry.name = nam;
        if (!create) {
            return vpdns.find(ntry);
        }
        cfgVpdn old = vpdns.add(ntry);
        if (old != null) {
            return old;
        }
        return ntry;
    }

    /**
     * delete one access list
     *
     * @param nam name of entry
     * @return descriptor, null if not found
     */
    public static cfgVpdn vpdnDel(String nam) {
        cfgVpdn ntry = new cfgVpdn();
        ntry.name = nam;
        ntry = vpdns.del(ntry);
        if (ntry == null) {
            return null;
        }
        ntry.stop2run();
        return ntry;
    }

    /**
     * find one object group
     *
     * @param nam name of entry
     * @param create create new on this number if not found
     * @return descriptor, null if not found
     */
    public static cfgObjnet objnetFind(String nam, boolean create) {
        nam = nam.trim();
        if (nam.length() < 1) {
            return null;
        }
        cfgObjnet ntry = new cfgObjnet();
        ntry.name = nam;
        ntry.objgrp.listName = nam;
        if (!create) {
            return objgrpnets.find(ntry);
        }
        cfgObjnet old = objgrpnets.add(ntry);
        if (old != null) {
            return old;
        }
        return ntry;
    }

    /**
     * delete one object group
     *
     * @param nam name of entry
     * @return descriptor, null if not found
     */
    public static cfgObjnet objnetDel(String nam) {
        cfgObjnet ntry = new cfgObjnet();
        ntry.name = nam;
        return objgrpnets.del(ntry);
    }

    /**
     * find one object group
     *
     * @param nam name of entry
     * @param create create new on this number if not found
     * @return descriptor, null if not found
     */
    public static cfgObjprt objprtFind(String nam, boolean create) {
        nam = nam.trim();
        if (nam.length() < 1) {
            return null;
        }
        cfgObjprt ntry = new cfgObjprt();
        ntry.name = nam;
        ntry.objgrp.listName = nam;
        if (!create) {
            return objgrpprts.find(ntry);
        }
        cfgObjprt old = objgrpprts.add(ntry);
        if (old != null) {
            return old;
        }
        return ntry;
    }

    /**
     * delete one object group
     *
     * @param nam name of entry
     * @return descriptor, null if not found
     */
    public static cfgObjprt objprtDel(String nam) {
        cfgObjprt ntry = new cfgObjprt();
        ntry.name = nam;
        return objgrpprts.del(ntry);
    }

    /**
     * find one access list
     *
     * @param nam name of entry
     * @param create create new on this number if not found
     * @return descriptor, null if not found
     */
    public static cfgAceslst aclsFind(String nam, boolean create) {
        nam = nam.trim();
        if (nam.length() < 1) {
            return null;
        }
        cfgAceslst ntry = new cfgAceslst();
        ntry.name = nam;
        ntry.aceslst.listName = nam;
        if (!create) {
            return accesslsts.find(ntry);
        }
        cfgAceslst old = accesslsts.add(ntry);
        if (old != null) {
            return old;
        }
        return ntry;
    }

    /**
     * delete one access list
     *
     * @param nam name of entry
     * @return descriptor, null if not found
     */
    public static cfgAceslst aclsDel(String nam) {
        cfgAceslst ntry = new cfgAceslst();
        ntry.name = nam;
        return accesslsts.del(ntry);
    }

    /**
     * find one event manager
     *
     * @param nam name of entry
     * @param create create new on this number if not found
     * @return descriptor, null if not found
     */
    public static cfgEvntmgr eemFind(String nam, boolean create) {
        nam = nam.trim();
        if (nam.length() < 1) {
            return null;
        }
        cfgEvntmgr ntry = new cfgEvntmgr();
        ntry.name = nam;
        ntry.script.listName = nam;
        if (!create) {
            return eventmgrs.find(ntry);
        }
        cfgEvntmgr old = eventmgrs.add(ntry);
        if (old != null) {
            return old;
        }
        return ntry;
    }

    /**
     * delete one access list
     *
     * @param nam name of entry
     * @return descriptor, null if not found
     */
    public static cfgEvntmgr eemDel(String nam) {
        cfgEvntmgr ntry = new cfgEvntmgr();
        ntry.name = nam;
        return eventmgrs.del(ntry);
    }

    /**
     * find one prefix list
     *
     * @param nam name of entry
     * @param create create new on this number if not found
     * @return descriptor, null if not found
     */
    public static cfgPrfxlst prfxFind(String nam, boolean create) {
        nam = nam.trim();
        if (nam.length() < 1) {
            return null;
        }
        cfgPrfxlst ntry = new cfgPrfxlst();
        ntry.name = nam;
        ntry.prflst.listName = nam;
        if (!create) {
            return prefixlsts.find(ntry);
        }
        cfgPrfxlst old = prefixlsts.add(ntry);
        if (old != null) {
            return old;
        }
        return ntry;
    }

    /**
     * delete one prefix list
     *
     * @param nam name of entry
     * @return descriptor, null if not found
     */
    public static cfgPrfxlst prfxDel(String nam) {
        cfgPrfxlst ntry = new cfgPrfxlst();
        ntry.name = nam;
        return prefixlsts.del(ntry);
    }

    /**
     * find one route map
     *
     * @param nam name of entry
     * @param create create new on this number if not found
     * @return descriptor, null if not found
     */
    public static cfgRoump rtmpFind(String nam, boolean create) {
        nam = nam.trim();
        if (nam.length() < 1) {
            return null;
        }
        cfgRoump ntry = new cfgRoump();
        ntry.name = nam;
        ntry.roumap.listName = nam;
        if (!create) {
            return routemaps.find(ntry);
        }
        cfgRoump old = routemaps.add(ntry);
        if (old != null) {
            return old;
        }
        return ntry;
    }

    /**
     * delete one route map
     *
     * @param nam name of entry
     * @return descriptor, null if not found
     */
    public static cfgRoump rtmpDel(String nam) {
        cfgRoump ntry = new cfgRoump();
        ntry.name = nam;
        return routemaps.del(ntry);
    }

    /**
     * find one route policy
     *
     * @param nam name of entry
     * @param create create new on this number if not found
     * @return descriptor, null if not found
     */
    public static cfgRouplc rtplFind(String nam, boolean create) {
        nam = nam.trim();
        if (nam.length() < 1) {
            return null;
        }
        cfgRouplc ntry = new cfgRouplc();
        ntry.name = nam;
        ntry.rouplc.listName = nam;
        if (!create) {
            return routeplcs.find(ntry);
        }
        cfgRouplc old = routeplcs.add(ntry);
        if (old != null) {
            return old;
        }
        return ntry;
    }

    /**
     * delete one route policy
     *
     * @param nam name of entry
     * @return descriptor, null if not found
     */
    public static cfgRouplc rtplDel(String nam) {
        cfgRouplc ntry = new cfgRouplc();
        ntry.name = nam;
        return routeplcs.del(ntry);
    }

    /**
     * find one time map
     *
     * @param nam name of entry
     * @param create create new on this number if not found
     * @return descriptor, null if not found
     */
    public static cfgTime timeFind(String nam, boolean create) {
        nam = nam.trim();
        if (nam.length() < 1) {
            return null;
        }
        cfgTime ntry = new cfgTime();
        ntry.name = nam;
        if (!create) {
            return timemaps.find(ntry);
        }
        cfgTime old = timemaps.add(ntry);
        if (old != null) {
            return old;
        }
        return ntry;
    }

    /**
     * delete one time map
     *
     * @param nam name of entry
     * @return descriptor, null if not found
     */
    public static cfgTime timeDel(String nam) {
        cfgTime ntry = new cfgTime();
        ntry.name = nam;
        return timemaps.del(ntry);
    }

    /**
     * find one policy map
     *
     * @param nam name of entry
     * @param create create new on this number if not found
     * @return descriptor, null if not found
     */
    public static cfgPlymp plmpFind(String nam, boolean create) {
        nam = nam.trim();
        if (nam.length() < 1) {
            return null;
        }
        cfgPlymp ntry = new cfgPlymp();
        ntry.name = nam;
        ntry.plcmap.listName = nam;
        if (!create) {
            return policymaps.find(ntry);
        }
        cfgPlymp old = policymaps.add(ntry);
        if (old != null) {
            return old;
        }
        return ntry;
    }

    /**
     * delete one policy map
     *
     * @param nam name of entry
     * @return descriptor, null if not found
     */
    public static cfgPlymp plmpDel(String nam) {
        cfgPlymp ntry = new cfgPlymp();
        ntry.name = nam;
        return policymaps.del(ntry);
    }

    /**
     * find one connect
     *
     * @param nam name of entry
     * @param create create new on this number if not found
     * @return descriptor, null if not found
     */
    public static cfgXconn xconFind(String nam, boolean create) {
        nam = nam.trim();
        if (nam.length() < 1) {
            return null;
        }
        cfgXconn ntry = new cfgXconn();
        ntry.name = nam;
        if (!create) {
            return xconnects.find(ntry);
        }
        cfgXconn old = xconnects.add(ntry);
        if (old != null) {
            return old;
        }
        return ntry;
    }

    /**
     * delete one connect
     *
     * @param nam name of entry
     * @return descriptor, null if not found
     */
    public static cfgXconn xconDel(String nam) {
        cfgXconn ntry = new cfgXconn();
        ntry.name = nam;
        ntry = xconnects.del(ntry);
        if (ntry == null) {
            return null;
        }
        ntry.stop2run();
        return ntry;
    }

    /**
     * find one connect
     *
     * @param nam name of entry
     * @param create create new on this number if not found
     * @return descriptor, null if not found
     */
    public static cfgIconn iconFind(String nam, boolean create) {
        nam = nam.trim();
        if (nam.length() < 1) {
            return null;
        }
        cfgIconn ntry = new cfgIconn();
        ntry.name = nam;
        if (!create) {
            return iconnects.find(ntry);
        }
        cfgIconn old = iconnects.add(ntry);
        if (old != null) {
            return old;
        }
        return ntry;
    }

    /**
     * delete one connect
     *
     * @param nam name of entry
     * @return descriptor, null if not found
     */
    public static cfgIconn iconDel(String nam) {
        cfgIconn ntry = new cfgIconn();
        ntry.name = nam;
        ntry = iconnects.del(ntry);
        if (ntry == null) {
            return null;
        }
        ntry.stop2run();
        return ntry;
    }

    /**
     * find one router
     *
     * @param typ type of routing process
     * @param num number of this
     * @param create create new on this number if not found
     * @return descriptor, null if not found
     */
    public static cfgRtr rtrFind(tabRouteEntry.routeType typ, int num,
            boolean create) {
        if (num < 1) {
            return null;
        }
        cfgRtr ntry = new cfgRtr(typ, num);
        if (!create) {
            return routers.find(ntry);
        }
        cfgRtr old = routers.add(ntry);
        if (old != null) {
            return old;
        }
        return ntry;
    }

    /**
     * delete one router
     *
     * @param typ type of routing process
     * @param num number of this
     * @return descriptor, null if not found
     */
    public static cfgRtr rtrDel(tabRouteEntry.routeType typ, int num) {
        cfgRtr ntry = new cfgRtr(typ, num);
        ntry = routers.del(ntry);
        if (ntry == null) {
            return null;
        }
        for (int i = ifaces.size() - 1; i >= 0; i--) {
            cfgIfc ifc = ifaces.get(i);
            switch (typ) {
                case rip4:
                    if (ifc.rtrRip4hnd != null) {
                        if (ifc.rtrRip4hnd.number == ntry.number) {
                            ifc.clear2router(ifc.rtrRip4hnd);
                        }
                    }
                    break;
                case rip6:
                    if (ifc.rtrRip6hnd != null) {
                        if (ifc.rtrRip6hnd.number == ntry.number) {
                            ifc.clear2router(ifc.rtrRip6hnd);
                        }
                    }
                    break;
                case ospf4:
                    if (ifc.rtrOspf4hnd != null) {
                        if (ifc.rtrOspf4hnd.number == ntry.number) {
                            ifc.clear2router(ifc.rtrOspf4hnd);
                        }
                    }
                    break;
                case ospf6:
                    if (ifc.rtrOspf6hnd != null) {
                        if (ifc.rtrOspf6hnd.number == ntry.number) {
                            ifc.clear2router(ifc.rtrOspf6hnd);
                        }
                    }
                    break;
                case isis4:
                case isis6:
                    if (ifc.rtrIsisHnd != null) {
                        if (ifc.rtrIsisHnd.number == ntry.number) {
                            ifc.clear2router(ifc.rtrIsisHnd);
                        }
                    }
                    break;
                case pvrp4:
                    if (ifc.rtrPvrp4hnd != null) {
                        if (ifc.rtrPvrp4hnd.number == ntry.number) {
                            ifc.clear2router(ifc.rtrPvrp4hnd);
                        }
                    }
                    break;
                case pvrp6:
                    if (ifc.rtrPvrp6hnd != null) {
                        if (ifc.rtrPvrp6hnd.number == ntry.number) {
                            ifc.clear2router(ifc.rtrPvrp6hnd);
                        }
                    }
                    break;
                case lsrp4:
                    if (ifc.rtrLsrp4hnd != null) {
                        if (ifc.rtrLsrp4hnd.number == ntry.number) {
                            ifc.clear2router(ifc.rtrLsrp4hnd);
                        }
                    }
                    break;
                case lsrp6:
                    if (ifc.rtrLsrp6hnd != null) {
                        if (ifc.rtrLsrp6hnd.number == ntry.number) {
                            ifc.clear2router(ifc.rtrLsrp6hnd);
                        }
                    }
                    break;
                case eigrp4:
                    if (ifc.rtrEigrp4hnd != null) {
                        if (ifc.rtrEigrp4hnd.number == ntry.number) {
                            ifc.clear2router(ifc.rtrEigrp4hnd);
                        }
                    }
                    break;
                case eigrp6:
                    if (ifc.rtrEigrp6hnd != null) {
                        if (ifc.rtrEigrp6hnd.number == ntry.number) {
                            ifc.clear2router(ifc.rtrEigrp6hnd);
                        }
                    }
                    break;
            }
        }
        ntry.closeUp();
        return ntry;
    }

    /**
     * find one bridge
     *
     * @param nam name of this
     * @param create create new on this number if not found
     * @return descriptor, null if not found
     */
    public static cfgBrdg brdgFind(String nam, boolean create) {
        nam = nam.trim();
        if (nam.length() < 1) {
            return null;
        }
        cfgBrdg ntry = new cfgBrdg(nam);
        if (!create) {
            return bridges.find(ntry);
        }
        cfgBrdg old = bridges.add(ntry);
        if (old != null) {
            return old;
        }
        cfgIfc ifc = ifcAdd(ntry.getIntName(), cfgIfc.ifaceType.bridge, null, 1);
        ntry.bridgeHed = new ifcBridge();
        ntry.bridgeHed.doStartup();
        ifc.bridgeHed = ntry;
        ifc.initBridge();
        return ntry;
    }

    /**
     * delete one bridge
     *
     * @param nam name of this
     * @return descriptor, null if not found
     */
    public static cfgBrdg brdgDel(String nam) {
        cfgBrdg ntry = new cfgBrdg(nam);
        ntry = bridges.del(ntry);
        if (ntry == null) {
            return null;
        }
        ntry.bridgeHed.doShutdown();
        for (int i = ifaces.size() - 1; i >= 0; i--) {
            cfgIfc ifc = ifaces.get(i);
            if (ifc.bridgeHed == null) {
                continue;
            }
            if (ntry.compare(ntry, ifc.bridgeHed) != 0) {
                continue;
            }
            ifc.clear2bridge();
        }
        ifcDel(ntry.getIntName(), false);
        ntry.closeUp();
        return ntry;
    }

    /**
     * find one bundle
     *
     * @param nam name of this
     * @param create create new on this number if not found
     * @return descriptor, null if not found
     */
    public static cfgBndl bndlFind(String nam, boolean create) {
        nam = nam.trim();
        if (nam.length() < 1) {
            return null;
        }
        cfgBndl ntry = new cfgBndl(nam);
        if (!create) {
            return bundles.find(ntry);
        }
        cfgBndl old = bundles.add(ntry);
        if (old != null) {
            return old;
        }
        cfgIfc ifc = ifcAdd(ntry.getIntName(), cfgIfc.ifaceType.bundle, null, 1);
        ntry.bundleHed = new ifcBundle();
        ntry.bundleHed.doStartup();
        ifc.bundleHed = ntry;
        ifc.initBundle();
        return ntry;
    }

    /**
     * delete one bundle
     *
     * @param nam name of this
     * @return descriptor, null if not found
     */
    public static cfgBndl bndlDel(String nam) {
        cfgBndl ntry = new cfgBndl(nam);
        ntry = bundles.del(ntry);
        if (ntry == null) {
            return null;
        }
        ntry.bundleHed.doShutdown();
        for (int i = ifaces.size() - 1; i >= 0; i--) {
            cfgIfc ifc = ifaces.get(i);
            if (ifc.bundleHed == null) {
                continue;
            }
            if (ntry.compare(ntry, ifc.bundleHed) != 0) {
                continue;
            }
            ifc.clear2bundle();
        }
        ifcDel(ntry.getIntName(), false);
        ntry.closeUp();
        return ntry;
    }

    /**
     * find one hairpin
     *
     * @param nam name of this
     * @param create create new on this number if not found
     * @return descriptor, null if not found
     */
    public static cfgHrpn hrpnFind(String nam, boolean create) {
        nam = nam.trim();
        if (nam.length() < 1) {
            return null;
        }
        cfgHrpn ntry = new cfgHrpn(nam);
        if (!create) {
            return hairpins.find(ntry);
        }
        cfgHrpn old = hairpins.add(ntry);
        if (old != null) {
            return old;
        }
        ntry.hairpinHed = new ifcHairpin();
        cfgIfc ifc1 = ifcAdd(ntry.getIntName(true), cfgIfc.ifaceType.hairpin, null, 1);
        cfgIfc ifc2 = ifcAdd(ntry.getIntName(false), cfgIfc.ifaceType.hairpin, null, 1);
        ifc1.hairpinHed = ntry;
        ifc2.hairpinHed = ntry;
        ifc1.initHairpin(true);
        ifc2.initHairpin(false);
        return ntry;
    }

    /**
     * delete one hairpin
     *
     * @param nam name of this
     * @return descriptor, null if not found
     */
    public static cfgHrpn hrpnDel(String nam) {
        cfgHrpn ntry = new cfgHrpn(nam);
        ntry = hairpins.del(ntry);
        if (ntry == null) {
            return null;
        }
        ifcDel(ntry.getIntName(true), false);
        ifcDel(ntry.getIntName(false), false);
        ntry.closeUp();
        return ntry;
    }

    private final static String dialFind(String str) {
        if (str == null) {
            return null;
        } else {
            return "," + str + ",";
        }
    }

    /**
     * find one dial peer
     *
     * @param calling calling number
     * @param called called number
     * @param skip skip this peer
     * @return descriptor, null if not found
     */
    public static cfgDial dialFind(String calling, String called, cfgDial skip) {
        String myNam = ",";
        String mySkp = "";
        String myAlw = null;
        if (skip != null) {
            myNam = dialFind(skip.name);
            mySkp = dialFind(skip.skipPeersOut);
            myAlw = dialFind(skip.allowPeersOut);
        }
        for (int i = 0; i < dials.size(); i++) {
            cfgDial ntry = dials.get(i);
            if (ntry == null) {
                continue;
            }
            if (skip != null) {
                if (skip.compare(skip, ntry) == 0) {
                    continue;
                }
            }
            String nam = dialFind(ntry.name);
            if (myAlw != null) {
                if (myAlw.indexOf(nam) < 0) {
                    continue;
                }
            }
            if (ntry.allowPeersIn != null) {
                if (dialFind(ntry.allowPeersIn).indexOf(myNam) < 0) {
                    continue;
                }
            }
            if (mySkp != null) {
                if (mySkp.indexOf(nam) >= 0) {
                    continue;
                }
            }
            if (ntry.skipPeersIn != null) {
                if (dialFind(ntry.skipPeersIn).indexOf(myNam) >= 0) {
                    continue;
                }
            }
            if (ntry.matches(calling, called)) {
                return ntry;
            }
        }
        return null;
    }

    /**
     * find one dial peer
     *
     * @param nam name of this
     * @param create create new on this number if not found
     * @return descriptor, null if not found
     */
    public static cfgDial dialFind(String nam, boolean create) {
        nam = nam.trim();
        if (nam.length() < 1) {
            return null;
        }
        cfgDial ntry = new cfgDial(nam);
        if (!create) {
            return dials.find(ntry);
        }
        cfgDial old = dials.add(ntry);
        if (old != null) {
            return old;
        }
        return ntry;
    }

    /**
     * delete one dial peer
     *
     * @param nam name of this
     * @return descriptor, null if not found
     */
    public static cfgDial dialDel(String nam) {
        cfgDial ntry = new cfgDial(nam);
        ntry = dials.del(ntry);
        if (ntry == null) {
            return null;
        }
        ntry.doShutdown();
        return ntry;
    }

    /**
     * find one translation rule
     *
     * @param nam name of this
     * @param create create new on this number if not found
     * @return descriptor, null if not found
     */
    public static cfgTrnsltn trnsltnFind(String nam, boolean create) {
        nam = nam.trim();
        if (nam.length() < 1) {
            return null;
        }
        cfgTrnsltn ntry = new cfgTrnsltn(nam);
        if (!create) {
            return trnsltns.find(ntry);
        }
        cfgTrnsltn old = trnsltns.add(ntry);
        if (old != null) {
            return old;
        }
        return ntry;
    }

    /**
     * delete one translation rule
     *
     * @param nam name of this
     * @return descriptor, null if not found
     */
    public static cfgTrnsltn trnsltnDel(String nam) {
        cfgTrnsltn ntry = new cfgTrnsltn(nam);
        ntry = trnsltns.del(ntry);
        if (ntry == null) {
            return null;
        }
        return ntry;
    }

    /**
     * find one server
     *
     * @param <T> type of list
     * @param srv empty instance
     * @param lst the list
     * @param nam name to find
     * @return instance, null if not found
     */
    public static <T extends servGeneric> T srvrFind(T srv, servGenList<T> lst, String nam) {
        srv.rename(nam);
        return lst.find(srv, false);
    }

    /**
     * get list of aliases
     *
     * @return list of aliases
     */
    public static userFormat getShAlias() {
        userFormat l = new userFormat("|", "type|name|command");
        for (int i = 0; i < aliases.size(); i++) {
            l.add("" + aliases.get(i));
        }
        return l;
    }

    /**
     * build list of interfaces
     *
     * @param mode mode to use
     * @return text to display
     */
    public static List<String> getShIntTxt(int mode) {
        List<String> l = new ArrayList<String>();
        for (int i = 0; i < ifaces.size(); i++) {
            cfgIfc ifc = ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            l.addAll(ifc.getShIntTxt(mode));
        }
        return l;
    }

    /**
     * build list of interfaces
     *
     * @param mode mode to use
     * @return text to display
     */
    public static userFormat getShIntTab(int mode) {
        userFormat l;
        switch (mode) {
            case 1:
                l = new userFormat("|", "interface|state|description");
                break;
            case 2:
                l = new userFormat("|", "interface|state|tx|rx|drop");
                break;
            case 3:
                l = new userFormat("|", "interface|state|vrf");
                break;
            case 4:
            case 5:
                l = new userFormat("|", "interface|state|address|netmask");
                break;
            case 6:
            case 7:
                l = new userFormat("|", "interface|hostname|iface|ipv4|ipv6");
                break;
            case 8:
                l = new userFormat("|", "interface|hostname|serial|iface|state");
                break;
            case 9:
                l = new userFormat("|", "interface|state|tx|rx|drop");
                break;
            case 10:
                l = new userFormat("|", "interface|state|tx|rx|drop");
                break;
            case 11:
                l = new userFormat("|", "interface|state|tx|rx|drop");
                break;
            case 12:
                l = new userFormat("|", "interface|state|tx|rx|drop");
                break;
            case 13:
                l = new userFormat("|", "interface|state|tx|rx|drop");
                break;
            case 14:
                l = new userFormat("|", "interface|system|port|state");
                break;
            default:
                return null;
        }
        for (int i = 0; i < ifaces.size(); i++) {
            cfgIfc ifc = ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            ifc.getShIntTab(l, mode);
        }
        return l;
    }

    /**
     * build list of trackers
     *
     * @return text to display
     */
    public static userFormat getShTracker() {
        userFormat l = new userFormat("|", "name|type|mode|target|state|changed");
        for (int i = 0; i < trackers.size(); i++) {
            cfgTrack trck = trackers.get(i);
            if (trck == null) {
                continue;
            }
            l.add(trck.worker.getShSum());
        }
        return l;
    }

    /**
     * build list of mtrackers
     *
     * @return text to display
     */
    public static userFormat getShMtracker() {
        userFormat l = new userFormat("|", "name|group|port|total|reach|bidir|changed");
        for (int i = 0; i < mtrackers.size(); i++) {
            cfgMtrack trck = mtrackers.get(i);
            if (trck == null) {
                continue;
            }
            l.add(trck.worker.getShSum());
        }
        return l;
    }

    /**
     * do things with more interfaces
     *
     * @param what what to do: 1=tunnelDest, 2=clearCntr, 3=autoBw
     */
    public static void moreInterfaces(int what) {
        for (int i = 0; i < ifaces.size(); i++) {
            cfgIfc ifc = ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            switch (what) {
                case 1:
                    ifc.tunnelDomainName();
                    break;
                case 2:
                    ifc.ethtyp.clearCounter();
                    break;
                case 3:
                    ifc.autoBandwidth();
                    break;
            }
        }
    }

    private static List<String> getGlobalRunBeg(boolean filter) {
        List<String> l = new ArrayList<String>();
        l.add("hostname " + hostName);
        cmds.cfgLine(l, verCore.release, "", "buggy", "");
        cmds.cfgLine(l, passEnc == null, "", "password-encrypt", "" + authLocal.passwdHide(passEnc));
        cmds.cfgLine(l, enaPass == null, "", "enable", authLocal.secretEncode(enaPass));
        l.add("banner encoded " + cryBase64.encodeBytes(banner));
        l.add(cmds.comment);
        l.add("logging buffered " + logger.level2string(logger.logBufLev) + " " + logger.getBufSize());
        l.add("logging monitor " + logger.level2string(logger.logPipLev));
        l.add("logging format " + logger.format2string(logger.logPosForm));
        cmds.cfgLine(l, logger.logFilNam.length() < 1, "", "logging file " + logger.level2string(logger.logFilLev), logger.logFilNam);
        cmds.cfgLine(l, logger.logRotNam.length() < 1, "", "logging rotate " + logger.logRotLim, logger.logRotNam);
        String a = "";
        for (int i = 0; i < logger.logSylHnd.size(); i++) {
            a += " " + logger.logSylHnd.get(i);
        }
        a = a.trim();
        cmds.cfgLine(l, a.length() < 1, "", "logging syslog " + logger.level2string(logger.logSylLev) + " " + servSyslog.num2facility(logger.logSylFac), a);
        a = "" + logger.logIrcHnd;
        cmds.cfgLine(l, a.length() < 1, "", "logging irc " + logger.level2string(logger.logIrcLev), a);
        l.add(cmds.comment);
        if (!filter) {
            return l;
        }
        return userFilter.filterText(l, defaultF);
    }

    private static String rxtx2string(boolean rx, boolean tx) {
        if ((rx == true) && (tx == true)) {
            return "both";
        }
        if (rx) {
            return "receive";
        }
        if (tx) {
            return "transmit";
        }
        return "none";
    }

    private static List<String> getGlobalRunEnd(boolean filter) {
        List<String> l = new ArrayList<String>();
        if (clientProxy == null) {
            l.add("no client proxy");
        } else {
            l.add("client proxy " + clientProxy.name);
        }
        if (nameServerProxy == null) {
            l.add("no client name-proxy");
        } else {
            l.add("client name-proxy " + nameServerProxy.name);
        }
        cmds.cfgLine(l, !preferIpv6, "", "client prefer-ipv6", "");
        cmds.cfgLine(l, !passwdStars, "", "client password-stars", "");
        cmds.cfgLine(l, whoisServer == null, "", "client whois-server", whoisServer);
        cmds.cfgLine(l, !graceReload, "", "client graceful-reload", "");
        l.add("client unreach-interval " + unreachInt);
        cmds.cfgLine(l, !ruinPmtuD, "", "client punish-pmtud", "");
        l.add("client ipv4-checksum " + rxtx2string(ipv4ChecksumRx, ipv4ChecksumTx));
        l.add("client icmp4-checksum " + rxtx2string(icmp4ChecksumRx, icmp4ChecksumTx));
        l.add("client icmp6-checksum " + rxtx2string(icmp6ChecksumRx, icmp6ChecksumTx));
        l.add("client udp-checksum " + rxtx2string(udpChecksumRx, udpChecksumTx));
        l.add("client tcp-checksum " + rxtx2string(tcpChecksumRx, tcpChecksumTx));
        l.add("client ludp-checksum " + rxtx2string(ludpChecksumRx, ludpChecksumTx));
        l.add("client dccp-checksum " + rxtx2string(dccpChecksumRx, dccpChecksumTx));
        l.add("client sctp-checksum " + rxtx2string(sctpChecksumRx, sctpChecksumTx));
        String a = "";
        for (int i = 0; i < nameServerAddr.size(); i++) {
            a += " " + nameServerAddr.get(i);
        }
        l.add("client name-server" + a);
        cmds.cfgLine(l, !ftpPassive, "", "client ftp-passive", "");
        cmds.cfgLine(l, upgradePubKey == null, "", "client upgrade-pubkey", "" + upgradePubKey);
        cmds.cfgLine(l, upgradeServer == null, "", "client upgrade-server", "" + upgradeServer);
        cmds.cfgLine(l, !upgradeConfig, "", "client upgrade-config", "");
        cmds.cfgLine(l, !upgradeBackup, "", "client upgrade-backup", "");
        cmds.cfgLine(l, !upgradeOwnKey, "", "client upgrade-ownkey", "");
        cmds.cfgLine(l, configServer == null, "", "client config-server", "" + configServer);
        cmds.cfgLine(l, configUser == null, "", "client config-username", "" + configUser);
        cmds.cfgLine(l, configPass == null, "", "client config-password", "" + authLocal.passwdEncode(configPass));
        cmds.cfgLine(l, configBackup == null, "", "client config-backup", "" + configBackup);
        cmds.cfgLine(l, !configAsave, "", "client config-save", "");
        cmds.cfgLine(l, !configAbackup, "", "client config-archive", "");
        cmds.cfgLine(l, configExclusive < 1, "", "client config-exclusive", "");
        cmds.cfgLine(l, timeServerName == null, "", "client time-server", "" + timeServerName);
        l.add("client time-zone " + timeZoneName);
        cmds.cfgLine(l, mailServerName == null, "", "client mail-server", "" + mailServerName);
        cmds.cfgLine(l, mailServerUser == null, "", "client mail-username", "" + mailServerUser);
        cmds.cfgLine(l, mailServerPass == null, "", "client mail-password", "" + authLocal.passwdEncode(mailServerPass));
        l.add(cmds.comment);
        if (!filter) {
            return l;
        }
        return userFilter.filterText(l, defaultF);
    }

    /**
     * build running configuration
     *
     * @param filter true to filter defaults, false to not
     * @return text to display
     */
    public static List<String> getShRun(boolean filter) {
        List<String> l = getGlobalRunBeg(filter);
        servGenList.listGetRun(l, vdcs, filter);
        servGenList.listGetRun(l, prcs, filter);
        servGenList.listGetRun(l, rsakeys, filter);
        servGenList.listGetRun(l, dsakeys, filter);
        servGenList.listGetRun(l, ecdsakeys, filter);
        servGenList.listGetRun(l, certs, filter);
        servGenList.listGetRun(l, timemaps, filter);
        servGenList.listGetRun(l, ipsecs, filter);
        servGenList.listGetRun(l, authers, filter);
        servGenList.listGetRun(l, chats, filter);
        servGenList.listGetRun(l, ip4pool, filter);
        servGenList.listGetRun(l, ip6pool, filter);
        servGenList.listGetRun(l, eventmgrs, filter);
        servGenList.listGetRun(l, objgrpnets, filter);
        servGenList.listGetRun(l, objgrpprts, filter);
        servGenList.listGetRun(l, accesslsts, filter);
        servGenList.listGetRun(l, prefixlsts, filter);
        servGenList.listGetRun(l, routemaps, filter);
        servGenList.listGetRun(l, routeplcs, filter);
        servGenList.listGetRun(l, policymaps, filter);
        servGenList.listGetRun(l, bundles, filter);
        servGenList.listGetRun(l, bridges, filter);
        servGenList.listGetRun(l, hairpins, filter);
        servGenList.listGetRun(l, menus, filter);
        servGenList.listGetRun(l, vrfs, filter);
        servGenList.listGetRun(l, routers, filter);
        servGenList.listGetRun(l, ifaces, filter);
        for (int i = 0; i < routers.size(); i++) {
            l.addAll(routers.get(i).getShRun2(filter));
        }
        servGenList.listGetRun(l, lines, filter);
        servGenList.listGetRun(l, proxys, filter);
        servGenList.listGetRun(l, vpdns, filter);
        servGenList.listGetRun(l, trackers, filter);
        servGenList.listGetRun(l, mtrackers, filter);
        servGenList.listGetRun(l, schedulers, filter);
        servGenList.listGetRun(l, scripts, filter);
        for (int i = 0; i < vrfs.size(); i++) {
            l.addAll(vrfs.get(i).getShRun2(filter));
        }
        servGenList.listGetRun(l, xconnects, filter);
        servGenList.listGetRun(l, iconnects, filter);
        servGenList.listGetRun(l, tabNshNtry.services, filter);
        servGenList.listGetRun(l, trnsltns, filter);
        servGenList.listGetRun(l, dials, filter);
        servGenList.listGetRun(l, aliases, filter);
        dmnTelnet.getShRun(l, filter);
        dmnUdptn.getShRun(l, filter);
        dmnRfb.getShRun(l, filter);
        dmnEcho.getShRun(l, filter);
        dmnDiscard.getShRun(l, filter);
        dmnQuote.getShRun(l, filter);
        dmnCharGen.getShRun(l, filter);
        dmnNetflow.getShRun(l, filter);
        dmnUpnpFwd.getShRun(l, filter);
        dmnUpnpHub.getShRun(l, filter);
        dmnOpenflow.getShRun(l, filter);
        dmnP4lang.getShRun(l, filter);
        dmnForwarder.getShRun(l, filter);
        dmnSyslog.getShRun(l, filter);
        dmnLoadBalancer.getShRun(l, filter);
        dmnSocks.getShRun(l, filter);
        dmnHttp.getShRun(l, filter);
        dmnFtp.getShRun(l, filter);
        dmnTftp.getShRun(l, filter);
        dmnGopher.getShRun(l, filter);
        dmnNtp.getShRun(l, filter);
        dmnDaytime.getShRun(l, filter);
        dmnTime.getShRun(l, filter);
        dmnSnmp.getShRun(l, filter);
        dmnIscsi.getShRun(l, filter);
        dmnBmp.getShRun(l, filter);
        dmnIrc.getShRun(l, filter);
        dmnDcp.getShRun(l, filter);
        dmnDhcp4.getShRun(l, filter);
        dmnDhcp6.getShRun(l, filter);
        dmnDns.getShRun(l, filter);
        dmnLpd.getShRun(l, filter);
        dmnHoney.getShRun(l, filter);
        dmnPop3.getShRun(l, filter);
        dmnSmtp.getShRun(l, filter);
        dmnModem.getShRun(l, filter);
        dmnVoice.getShRun(l, filter);
        dmnSip.getShRun(l, filter);
        dmnRpki.getShRun(l, filter);
        dmnNrpe.getShRun(l, filter);
        dmnBStun.getShRun(l, filter);
        dmnStun.getShRun(l, filter);
        dmnPckOudp.getShRun(l, filter);
        dmnPckOdtls.getShRun(l, filter);
        dmnPckOtcp.getShRun(l, filter);
        dmnPckOtxt.getShRun(l, filter);
        dmnVxlan.getShRun(l, filter);
        dmnGeneve.getShRun(l, filter);
        dmnL2f.getShRun(l, filter);
        dmnL2tp2.getShRun(l, filter);
        dmnL2tp3.getShRun(l, filter);
        dmnEtherIp.getShRun(l, filter);
        dmnGre.getShRun(l, filter);
        dmnMplsIp.getShRun(l, filter);
        dmnMplsUdp.getShRun(l, filter);
        dmnGtp.getShRun(l, filter);
        dmnPptp.getShRun(l, filter);
        dmnRadius.getShRun(l, filter);
        dmnTacacs.getShRun(l, filter);
        l.addAll(getGlobalRunEnd(filter));
        String s = null;
        if (filter) {
            s = "";
        } else {
            s = " date=" + bits.time2str(timeZoneName, bits.getTime() + cfgAll.timeServerOffset, 3).replaceAll(" ", "_") + " image=" + version.usrAgnt + " chksum=" + userUpgrade.calcTextHash(l);
        }
        l.add("end" + s);
        return l;
    }

}
