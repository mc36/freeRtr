package net.freertr.cfg;

import java.util.ArrayList;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrIPv4;
import net.freertr.addr.addrIPv6;
import net.freertr.addr.addrType;
import net.freertr.auth.authLocal;
import net.freertr.clnt.clntNtp;
import net.freertr.clnt.clntProxy;
import net.freertr.cry.cryBase64;
import net.freertr.cry.cryKeyDSA;
import net.freertr.cry.cryKeyECDSA;
import net.freertr.cry.cryKeyGeneric;
import net.freertr.cry.cryKeyRSA;
import net.freertr.ifc.ifcBridge;
import net.freertr.ifc.ifcBundle;
import net.freertr.ifc.ifcHairpin;
import net.freertr.ifc.ifcThread;
import net.freertr.line.lineThread;
import net.freertr.serv.servAmt;
import net.freertr.serv.servBmp2mrt;
import net.freertr.serv.servBstun;
import net.freertr.serv.servCharGen;
import net.freertr.serv.servDaytime;
import net.freertr.serv.servDcp;
import net.freertr.serv.servDhcp4;
import net.freertr.serv.servDhcp6;
import net.freertr.serv.servDiscard;
import net.freertr.serv.servDns;
import net.freertr.serv.servEchoS;
import net.freertr.serv.servEtherIp;
import net.freertr.serv.servForwarder;
import net.freertr.serv.servFtp;
import net.freertr.serv.servGenList;
import net.freertr.serv.servGeneric;
import net.freertr.serv.servGeneve;
import net.freertr.serv.servGopher;
import net.freertr.serv.servGre;
import net.freertr.serv.servGtp;
import net.freertr.serv.servHoneyPot;
import net.freertr.serv.servHttp;
import net.freertr.serv.servIrc;
import net.freertr.serv.servIscsi;
import net.freertr.serv.servL2f;
import net.freertr.serv.servL2tp2;
import net.freertr.serv.servL2tp3;
import net.freertr.serv.servLoadBalancer;
import net.freertr.serv.servLpd;
import net.freertr.serv.servModem;
import net.freertr.serv.servMplsIp;
import net.freertr.serv.servMplsOam;
import net.freertr.serv.servMplsUdp;
import net.freertr.serv.servMultiplexer;
import net.freertr.serv.servNetflow;
import net.freertr.serv.servNrpe;
import net.freertr.serv.servNtp;
import net.freertr.serv.servOpenflow;
import net.freertr.serv.servPktmux;
import net.freertr.serv.servP4lang;
import net.freertr.serv.servPcep;
import net.freertr.serv.servPckOdtls;
import net.freertr.serv.servPckOtcp;
import net.freertr.serv.servPckOtxt;
import net.freertr.serv.servPckOudp;
import net.freertr.serv.servPop3;
import net.freertr.serv.servPptp;
import net.freertr.serv.servPrometheus;
import net.freertr.serv.servQuote;
import net.freertr.serv.servRadius;
import net.freertr.serv.servRfb;
import net.freertr.serv.servRpki;
import net.freertr.serv.servSdwan;
import net.freertr.serv.servSip;
import net.freertr.serv.servSmtp;
import net.freertr.serv.servSnmp;
import net.freertr.serv.servSocks;
import net.freertr.serv.servStreamingMdt;
import net.freertr.serv.servStun;
import net.freertr.serv.servSyslog;
import net.freertr.serv.servTacacs;
import net.freertr.serv.servTelnet;
import net.freertr.serv.servTftp;
import net.freertr.serv.servTime;
import net.freertr.serv.servTwamp;
import net.freertr.serv.servUdpFwd;
import net.freertr.serv.servUdptn;
import net.freertr.serv.servUni2multi;
import net.freertr.serv.servUpnpFwd;
import net.freertr.serv.servUpnpHub;
import net.freertr.serv.servVoice;
import net.freertr.serv.servVxlan;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabNshEntry;
import net.freertr.tab.tabRouteAttr;
import net.freertr.user.userFilter;
import net.freertr.user.userFormat;
import net.freertr.user.userHelping;
import net.freertr.user.userReload;
import net.freertr.user.userUpgrade;
import net.freertr.util.bits;
import net.freertr.util.chatter;
import net.freertr.util.cmds;
import net.freertr.util.logger;
import net.freertr.util.verCore;
import net.freertr.util.version;

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
     * list of sessions
     */
    public static final tabGen<cfgSessn> sessns = new tabGen<cfgSessn>();

    /**
     * list of checks
     */
    public static final tabGen<cfgCheck> checks = new tabGen<cfgCheck>();

    /**
     * list of sensors
     */
    public static final tabGen<cfgSensor> sensors = new tabGen<cfgSensor>();

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
     * list of telemetry destinations
     */
    public static final tabGen<cfgTlmtry> tlmtrydst = new tabGen<cfgTlmtry>();

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
    public static final servGenList<servEchoS> dmnEcho = new servGenList<servEchoS>();

    /**
     * discard daemons
     */
    public static final servGenList<servDiscard> dmnDiscard = new servGenList<servDiscard>();

    /**
     * quote daemons
     */
    public static final servGenList<servQuote> dmnQuote = new servGenList<servQuote>();

    /**
     * chargen daemons
     */
    public static final servGenList<servCharGen> dmnCharGen = new servGenList<servCharGen>();

    /**
     * netflow daemons
     */
    public static final servGenList<servNetflow> dmnNetflow = new servGenList<servNetflow>();

    /**
     * udpfwd daemons
     */
    public static final servGenList<servUdpFwd> dmnUdpFwd = new servGenList<servUdpFwd>();

    /**
     * upnpfwd daemons
     */
    public static final servGenList<servUpnpFwd> dmnUpnpFwd = new servGenList<servUpnpFwd>();

    /**
     * upnphub daemons
     */
    public static final servGenList<servUpnpHub> dmnUpnpHub = new servGenList<servUpnpHub>();

    /**
     * openflow daemons
     */
    public static final servGenList<servOpenflow> dmnOpenflow = new servGenList<servOpenflow>();

    /**
     * pktmux daemons
     */
    public static final servGenList<servPktmux> dmnPktmux = new servGenList<servPktmux>();

    /**
     * p4lang daemons
     */
    public static final servGenList<servP4lang> dmnP4lang = new servGenList<servP4lang>();

    /**
     * forwarder daemons
     */
    public static final servGenList<servForwarder> dmnForwarder = new servGenList<servForwarder>();

    /**
     * syslog daemons
     */
    public static final servGenList<servSyslog> dmnSyslog = new servGenList<servSyslog>();

    /**
     * loadbalancer daemons
     */
    public static final servGenList<servLoadBalancer> dmnLoadBalancer = new servGenList<servLoadBalancer>();

    /**
     * multiplexer daemons
     */
    public static final servGenList<servMultiplexer> dmnMultiplexer = new servGenList<servMultiplexer>();

    /**
     * telnet daemons
     */
    public static final servGenList<servTelnet> dmnTelnet = new servGenList<servTelnet>();

    /**
     * rfb daemons
     */
    public static final servGenList<servRfb> dmnRfb = new servGenList<servRfb>();

    /**
     * udptn daemons
     */
    public static final servGenList<servUdptn> dmnUdptn = new servGenList<servUdptn>();

    /**
     * http daemons
     */
    public static final servGenList<servHttp> dmnHttp = new servGenList<servHttp>();

    /**
     * lpd daemons
     */
    public static final servGenList<servLpd> dmnLpd = new servGenList<servLpd>();

    /**
     * honeypot daemons
     */
    public static final servGenList<servHoneyPot> dmnHoney = new servGenList<servHoneyPot>();

    /**
     * dhcp4 daemons
     */
    public static final servGenList<servDhcp4> dmnDhcp4 = new servGenList<servDhcp4>();

    /**
     * dhcp6 daemons
     */
    public static final servGenList<servDhcp6> dmnDhcp6 = new servGenList<servDhcp6>();

    /**
     * dns daemons
     */
    public static final servGenList<servDns> dmnDns = new servGenList<servDns>();

    /**
     * pop3 daemons
     */
    public static final servGenList<servPop3> dmnPop3 = new servGenList<servPop3>();

    /**
     * smtp daemons
     */
    public static final servGenList<servSmtp> dmnSmtp = new servGenList<servSmtp>();

    /**
     * sip modem daemons
     */
    public static final servGenList<servModem> dmnModem = new servGenList<servModem>();

    /**
     * sip voice daemons
     */
    public static final servGenList<servVoice> dmnVoice = new servGenList<servVoice>();

    /**
     * sip proxy daemons
     */
    public static final servGenList<servSip> dmnSip = new servGenList<servSip>();

    /**
     * ftp daemons
     */
    public static final servGenList<servFtp> dmnFtp = new servGenList<servFtp>();

    /**
     * tftp daemons
     */
    public static final servGenList<servTftp> dmnTftp = new servGenList<servTftp>();

    /**
     * gopher daemons
     */
    public static final servGenList<servGopher> dmnGopher = new servGenList<servGopher>();

    /**
     * ntp daemons
     */
    public static final servGenList<servNtp> dmnNtp = new servGenList<servNtp>();

    /**
     * daytime daemons
     */
    public static final servGenList<servDaytime> dmnDaytime = new servGenList<servDaytime>();

    /**
     * time daemons
     */
    public static final servGenList<servTime> dmnTime = new servGenList<servTime>();

    /**
     * snmp daemons
     */
    public static final servGenList<servSnmp> dmnSnmp = new servGenList<servSnmp>();

    /**
     * iscsi daemons
     */
    public static final servGenList<servIscsi> dmnIscsi = new servGenList<servIscsi>();

    /**
     * bmp2mrt daemons
     */
    public static final servGenList<servBmp2mrt> dmnBmp = new servGenList<servBmp2mrt>();

    /**
     * irc daemons
     */
    public static final servGenList<servIrc> dmnIrc = new servGenList<servIrc>();

    /**
     * dcp daemons
     */
    public static final servGenList<servDcp> dmnDcp = new servGenList<servDcp>();

    /**
     * sdwan daemons
     */
    public static final servGenList<servSdwan> dmnSdwan = new servGenList<servSdwan>();

    /**
     * pcep daemons
     */
    public static final servGenList<servPcep> dmnPcep = new servGenList<servPcep>();

    /**
     * socks daemons
     */
    public static final servGenList<servSocks> dmnSocks = new servGenList<servSocks>();

    /**
     * rpki daemons
     */
    public static final servGenList<servRpki> dmnRpki = new servGenList<servRpki>();

    /**
     * nrpe daemons
     */
    public static final servGenList<servNrpe> dmnNrpe = new servGenList<servNrpe>();

    /**
     * prometheus daemons
     */
    public static final servGenList<servPrometheus> dmnPrometheus = new servGenList<servPrometheus>();

    /**
     * streaming telemetry daemons
     */
    public static final servGenList<servStreamingMdt> dmnStreamingMdt = new servGenList<servStreamingMdt>();

    /**
     * bstun daemons
     */
    public static final servGenList<servBstun> dmnBStun = new servGenList<servBstun>();

    /**
     * stun daemons
     */
    public static final servGenList<servStun> dmnStun = new servGenList<servStun>();

    /**
     * pckoudp daemons
     */
    public static final servGenList<servPckOudp> dmnPckOudp = new servGenList<servPckOudp>();

    /**
     * pckodtls daemons
     */
    public static final servGenList<servPckOdtls> dmnPckOdtls = new servGenList<servPckOdtls>();

    /**
     * pckotcp daemons
     */
    public static final servGenList<servPckOtcp> dmnPckOtcp = new servGenList<servPckOtcp>();

    /**
     * pckotxt daemons
     */
    public static final servGenList<servPckOtxt> dmnPckOtxt = new servGenList<servPckOtxt>();

    /**
     * vxlan daemons
     */
    public static final servGenList<servVxlan> dmnVxlan = new servGenList<servVxlan>();

    /**
     * geneve daemons
     */
    public static final servGenList<servGeneve> dmnGeneve = new servGenList<servGeneve>();

    /**
     * l2f daemons
     */
    public static final servGenList<servL2f> dmnL2f = new servGenList<servL2f>();

    /**
     * l2tp2 daemons
     */
    public static final servGenList<servL2tp2> dmnL2tp2 = new servGenList<servL2tp2>();

    /**
     * l2tp3 daemons
     */
    public static final servGenList<servL2tp3> dmnL2tp3 = new servGenList<servL2tp3>();

    /**
     * etherip daemons
     */
    public static final servGenList<servEtherIp> dmnEtherIp = new servGenList<servEtherIp>();

    /**
     * gre daemons
     */
    public static final servGenList<servGre> dmnGre = new servGenList<servGre>();

    /**
     * mplsip daemons
     */
    public static final servGenList<servMplsIp> dmnMplsIp = new servGenList<servMplsIp>();

    /**
     * mplsudp daemons
     */
    public static final servGenList<servMplsUdp> dmnMplsUdp = new servGenList<servMplsUdp>();

    /**
     * mplsoam daemons
     */
    public static final servGenList<servMplsOam> dmnMplsOam = new servGenList<servMplsOam>();

    /**
     * twamp daemons
     */
    public static final servGenList<servTwamp> dmnTwamp = new servGenList<servTwamp>();

    /**
     * amt daemons
     */
    public static final servGenList<servAmt> dmnAmt = new servGenList<servAmt>();

    /**
     * uni2multi daemons
     */
    public static final servGenList<servUni2multi> dmnUni2mul = new servGenList<servUni2multi>();

    /**
     * gtp daemons
     */
    public static final servGenList<servGtp> dmnGtp = new servGenList<servGtp>();

    /**
     * pptp daemons
     */
    public static final servGenList<servPptp> dmnPptp = new servGenList<servPptp>();

    /**
     * radius daemons
     */
    public static final servGenList<servRadius> dmnRadius = new servGenList<servRadius>();

    /**
     * tacacs daemons
     */
    public static final servGenList<servTacacs> dmnTacacs = new servGenList<servTacacs>();

    /**
     * name of this host
     */
    public static String hostName = "router";

    /**
     * domain of this host
     */
    public static String domainName = null;

    /**
     * end format
     */
    public static int endForm = 0;

    /**
     * encryption of this host
     */
    public static String passEnh = null;

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
     * minimum tls version to use
     */
    public static int tlsVerMin = 0;

    /**
     * maximum tls version to use
     */
    public static int tlsVerMax = 4;

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
     * proxy to use
     */
    public static clntProxy whoisProxy;

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
     * proxy to use
     */
    public static clntProxy timeProxy;

    /**
     * last offset to time server
     */
    public static long timeServerOffset;

    /**
     * proxy to use
     */
    public static clntProxy tftpProxy;

    /**
     * proxy to use
     */
    public static clntProxy httpProxy;

    /**
     * access subnet prefix length
     */
    public static int accessSubnet4 = 120;

    /**
     * access subnet prefix length
     */
    public static int accessSubnet6 = 64;

    /**
     * proxy to use
     */
    public static clntProxy mailProxy;

    /**
     * name of mail server
     */
    public static String mailServerName = null;

    /**
     * username for mail server
     */
    public static String mailServerUser = null;

    /**
     * password for mail server
     */
    public static String mailServerPass = null;

    /**
     * upgrade public key
     */
    public static String upgradePubKey = null;

    /**
     * upgrade server url
     */
    public static String upgradeServer = verCore.homeUrl;

    /**
     * upgrade script
     */
    public static cfgScrpt upgradeScript = null;

    /**
     * upgrade backup files
     */
    public static boolean upgradeBackup = false;

    /**
     * upgrade revert time
     */
    public static int upgradeRevert = 0;

    /**
     * upgrade config save
     */
    public static boolean upgradeConfig = false;

    /**
     * upgrade just own key
     */
    public static boolean upgradeOwnKey = false;

    /**
     * label range beginning
     */
    public static int labelRangeBeg = 0x20;

    /**
     * label range ending
     */
    public static int labelRangeEnd = 0xffff0;

    /**
     * cpu hog check
     */
    public static int cpuhogCheck = 0;

    /**
     * interface stall check
     */
    public static int ifaceStallCheck = 60000;

    /**
     * redundancy keepalive time
     */
    public static int redundancyKeep = 500;

    /**
     * redundancy hold time
     */
    public static int redundancyHold = 5 * 500;

    /**
     * redundancy init time
     */
    public static int redundancyInit = 10 * 500;

    /**
     * passive mode ftp
     */
    public static boolean ftpPassive = true;

    /**
     * proxy to use
     */
    public static clntProxy ftpProxy;

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
     * udp range beginning
     */
    public static int udpRangeMin = 0x8000;

    /**
     * udp range beginning
     */
    public static int udpRangeMax = 0xf000;

    /**
     * ludp checksum tx
     */
    public static boolean ludpChecksumTx = true;

    /**
     * ludp checksum rx
     */
    public static boolean ludpChecksumRx = true;

    /**
     * ludp range beginning
     */
    public static int ludpRangeMin = 0x8000;

    /**
     * ludp range beginning
     */
    public static int ludpRangeMax = 0xf000;

    /**
     * tcp maximum segment
     */
    public static int tcpMaxSegment = 1024;

    /**
     * tcp window scale
     */
    public static int tcpWinScale = 1;

    /**
     * tcp timestamps
     */
    public static boolean tcpTimStmp = true;

    /**
     * tcp checksum tx
     */
    public static boolean tcpChecksumTx = true;

    /**
     * tcp checksum rx
     */
    public static boolean tcpChecksumRx = true;

    /**
     * tcp range beginning
     */
    public static int tcpRangeMin = 0x8000;

    /**
     * tcp range beginning
     */
    public static int tcpRangeMax = 0xf000;

    /**
     * dccp checksum tx
     */
    public static boolean dccpChecksumTx = true;

    /**
     * dccp checksum rx
     */
    public static boolean dccpChecksumRx = true;

    /**
     * dccp range beginning
     */
    public static int dccpRangeMin = 0x8000;

    /**
     * dccp range beginning
     */
    public static int dccpRangeMax = 0xf000;

    /**
     * sctp checksum tx
     */
    public static boolean sctpChecksumTx = true;

    /**
     * sctp checksum rx
     */
    public static boolean sctpChecksumRx = true;

    /**
     * sctp range beginning
     */
    public static int sctpRangeMin = 0x8000;

    /**
     * sctp range beginning
     */
    public static int sctpRangeMax = 0xf000;

    /**
     * l2tp2 hello ticks
     */
    public static int l2tp2hello = 5;

    /**
     * l2tp2 retry ticks
     */
    public static int l2tp2retry = 8;

    /**
     * l2tp3 hello ticks
     */
    public static int l2tp3hello = 5;

    /**
     * l2tp3 retry ticks
     */
    public static int l2tp3retry = 8;

    /**
     * shame texts
     */
    public static servQuote clientShamer;

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
    public final static String[] defaultL = {
        // logging
        "!logging buffered debug 512",
        "!logging monitor debug",
        "!logging format normal",
        "!no logging milliseconds",
        "!no logging proxy",
        "!no logging rotate",
        "!no logging syslog debug kernel",
        "!no logging file debug",
        "!no logging irc debug",
        "!banner encoded ",
        "!no password-encrypt",
        "!no enable",
        // client
        "!client label-range 32 1048560",
        "!client cpuhog 0",
        "!client ifacestall 60000",
        "!client redundancy 500 2500 5000",
        "!client l2tp2-timer 5 8",
        "!client l2tp3-timer 5 8",
        "!no client bullying",
        "!no client proxy",
        "!no client domain-name",
        "!no client name-proxy",
        "!client name-server",
        "!client time-zone Z",
        "!no client time-proxy",
        "!no client tftp-proxy",
        "!no client http-proxy",
        "!client access-subnet-ipv4 120",
        "!client access-subnet-ipv6 64",
        "!client tls-version 1 4",
        "!no client time-server",
        "!no client upgrade-pubkey",
        "!client upgrade-server " + verCore.homeUrl,
        "!no client upgrade-config",
        "!no client upgrade-backup",
        "!no client upgrade-revert",
        "!no client upgrade-script",
        "!no client upgrade-ownkey",
        "!no client whois-proxy",
        "!no client whois-server",
        "!no client config-server",
        "!no client config-username",
        "!no client config-password",
        "!no client config-save",
        "!no client config-archive",
        "!no client config-backup",
        "!no client config-exclusive",
        "!no client mail-proxy",
        "!no client mail-server",
        "!no client mail-username",
        "!no client mail-password",
        "!no client prefer-ipv6",
        "!no client password-stars",
        "!no client end-format",
        "!client graceful-reload",
        "!client ftp-passive",
        "!no client ftp-proxy",
        "!client ipv4-checksum both",
        "!client icmp4-checksum both",
        "!client icmp6-checksum both",
        "!client udp-checksum both",
        "!client udp-portrange 32768 61440",
        "!client tcp-segments 1024",
        "!client tcp-winscale 1",
        "!client tcp-timestamp",
        "!client tcp-checksum both",
        "!client tcp-portrange 32768 61440",
        "!client ludp-checksum both",
        "!client ludp-portrange 32768 61440",
        "!client dccp-checksum both",
        "!client dccp-portrange 32768 61440",
        "!client sctp-checksum both",
        "!client sctp-portrange 32768 61440",};

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
     * @param other optional other to return
     * @return client proxy, null if nothing
     */
    public static clntProxy getClntPrx(clntProxy other) {
        if (other != null) {
            return other;
        }
        if (clientProxy == null) {
            return null;
        }
        return clientProxy.proxy;
    }

    /**
     * get my fqdn
     *
     * @return name
     */
    public static String getFqdn() {
        if (domainName == null) {
            return hostName;
        } else {
            return hostName + "." + domainName;
        }
    }

    /**
     * find vrf by rd
     *
     * @param ipv4 ipv4
     * @param rd rd to find
     * @return vrf, null on error
     */
    public static cfgVrf findRd(boolean ipv4, long rd) {
        for (int i = 0; i < vrfs.size(); i++) {
            cfgVrf ntry = vrfs.get(i);
            if (ntry == null) {
                continue;
            }
            if (ipv4 && (ntry.fwd4.rd == rd)) {
                return ntry;
            }
            if (!ipv4 && (ntry.fwd6.rd == rd)) {
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
        ntry.allocThisVrf();
        cfgVrf old = vrfs.add(ntry);
        if (old != null) {
            return old;
        }
        ntry.startThisVrf();
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
            ifc.clear2routing(true, true);
            ifc.clear2vrf();
        }
        ntry.closeAllConns();
        ntry.stopThisVrf();
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
        nam = cfgIfc.dissectName(nam)[0];
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
        String pnm[] = cfgIfc.dissectName(nam);
        nam = pnm[0] + pnm[1];
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
        ntry.clear2routing(true, true);
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
        ntry.setup2pppoeServ(null, null);
        ntry.setup2pppoeRely(null, null);
        ntry.clear2pseudowire();
        if (ntry.vlanNum != 0) {
            return false;
        }
        for (int i = ifaces.size() - 1; i >= 0; i--) {
            cfgIfc cur = ifaces.get(i);
            if (cur == null) {
                continue;
            }
            if (cur.vlanNum == 0) {
                continue;
            }
            if (cur.parent != ntry) {
                continue;
            }
            ifcDel(cur.name, checks);
        }
        return false;
    }

    /**
     * find one interface
     *
     * @param nam name of this
     * @param create 0=find, 1=create new on this name if not found, 2=as 1, but
     * for cloning
     * @return descriptor, null if not found
     */
    public static cfgIfc ifcFind(String nam, int create) {
        String pnm[] = cfgIfc.dissectName(nam);
        if (pnm[0].length() < 1) {
            return null;
        }
        nam = pnm[0] + pnm[1];
        cfgIfc ntry = new cfgIfc(nam);
        if (create < 1) {
            return ifaces.find(ntry);
        }
        cfgIfc old = ifaces.add(ntry);
        if (old != null) {
            if (create < 2) {
                return old;
            } else {
                return null;
            }
        }
        if (pnm[1].length() > 0) {
            old = ifaces.find(new cfgIfc(pnm[0]));
            if (old == null) {
                ifaces.del(ntry);
                return null;
            }
            ntry.vlanNum = bits.str2num(pnm[1].substring(1, pnm[1].length()));
            ntry.initSubiface(old);
            return ntry;
        }
        if (nam.startsWith("tunnel")) {
            ntry.type = cfgIfc.ifaceType.tunnel;
            ntry.clear2tunnel(false);
            return ntry;
        }
         if (nam.startsWith("dialer")) {
            ntry.type = cfgIfc.ifaceType.dialer;
            ntry.initPhysical();
            return ntry;
        }
        if (nam.startsWith("sdn")) {
            ntry.type = cfgIfc.ifaceType.sdn;
            ntry.initPhysical();
            return ntry;
        }
        if (nam.startsWith("pwether")) {
            ntry.type = cfgIfc.ifaceType.pweth;
            ntry.initPhysical();
            return ntry;
        }
        if (nam.startsWith("virtualppp")) {
            ntry.type = cfgIfc.ifaceType.virtppp;
            ntry.initPhysical();
            return ntry;
        }
        if (nam.startsWith("loopback")) {
            ntry.type = cfgIfc.ifaceType.loopback;
            ntry.initLoopback();
            return ntry;
        }
        if (nam.startsWith("null")) {
            ntry.type = cfgIfc.ifaceType.nul;
            ntry.initTemplate();
            return ntry;
        }
        if (nam.startsWith("template")) {
            ntry.type = cfgIfc.ifaceType.template;
            ntry.initTemplate();
            return ntry;
        }
        if (nam.startsWith("access")) {
            ntry.type = cfgIfc.ifaceType.dialer;
            ntry.initPhysical();
            return ntry;
        }
        ifaces.del(ntry);
        return null;
    }

    /**
     * reregister subinterfaces
     *
     * @param iface interface
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
     * find one telemetry destination
     *
     * @param nam name of entry
     * @param create create new on this number if not found
     * @return descriptor, null if not found
     */
    public static cfgTlmtry tlmdsFind(String nam, boolean create) {
        nam = nam.trim();
        if (nam.length() < 1) {
            return null;
        }
        cfgTlmtry ntry = new cfgTlmtry();
        ntry.name = nam;
        if (!create) {
            return tlmtrydst.find(ntry);
        }
        cfgTlmtry old = tlmtrydst.add(ntry);
        if (old != null) {
            return old;
        }
        return ntry;
    }

    /**
     * delete one telemetry export
     *
     * @param nam name of entry
     * @return descriptor, null if not found
     */
    public static cfgTlmtry tlmdsDel(String nam) {
        cfgTlmtry ntry = new cfgTlmtry();
        ntry.name = nam;
        return tlmtrydst.del(ntry);
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
     * delete one event manager
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
    public static cfgRtr rtrFind(tabRouteAttr.routeType typ, int num,
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
    public static cfgRtr rtrDel(tabRouteAttr.routeType typ, int num) {
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
                default:
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
        ntry.startWork();
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
        ntry.stopWork();
        return ntry;
    }

    /**
     * find one session
     *
     * @param nam name of this
     * @param create create new on this number if not found
     * @return descriptor, null if not found
     */
    public static cfgSessn sessnFind(String nam, boolean create) {
        nam = nam.trim();
        if (nam.length() < 1) {
            return null;
        }
        cfgSessn ntry = new cfgSessn(nam);
        if (!create) {
            return sessns.find(ntry);
        }
        cfgSessn old = sessns.add(ntry);
        if (old != null) {
            return old;
        }
        ntry.connects.name = ntry.name;
        ntry.connects.startTimer();
        return ntry;
    }

    /**
     * delete one session
     *
     * @param nam name of this
     * @return descriptor, null if not found
     */
    public static cfgSessn sessnDel(String nam) {
        cfgSessn ntry = new cfgSessn(nam);
        ntry = sessns.del(ntry);
        if (ntry == null) {
            return null;
        }
        ntry.connects.stopTimer();
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
     * find one check
     *
     * @param nam name of this
     * @param create create new on this number if not found
     * @return descriptor, null if not found
     */
    public static cfgCheck checkFind(String nam, boolean create) {
        nam = nam.trim();
        if (nam.length() < 1) {
            return null;
        }
        cfgCheck ntry = new cfgCheck(nam);
        if (!create) {
            return checks.find(ntry);
        }
        cfgCheck old = checks.add(ntry);
        if (old != null) {
            return old;
        }
        return ntry;
    }

    /**
     * delete one check
     *
     * @param nam name of this
     * @return descriptor, null if not found
     */
    public static cfgCheck checkDel(String nam) {
        cfgCheck ntry = new cfgCheck(nam);
        ntry = checks.del(ntry);
        if (ntry == null) {
            return null;
        }
        return ntry;
    }

    /**
     * find one sensor
     *
     * @param nam name of this
     * @param create create new on this number if not found
     * @return descriptor, null if not found
     */
    public static cfgSensor sensorFind(String nam, boolean create) {
        nam = nam.trim();
        if (nam.length() < 1) {
            return null;
        }
        cfgSensor ntry = new cfgSensor(nam);
        if (!create) {
            return sensors.find(ntry);
        }
        cfgSensor old = sensors.add(ntry);
        if (old != null) {
            return old;
        }
        return ntry;
    }

    /**
     * delete one sensor
     *
     * @param nam name of this
     * @return descriptor, null if not found
     */
    public static cfgSensor sensorDel(String nam) {
        cfgSensor ntry = new cfgSensor(nam);
        ntry = sensors.del(ntry);
        if (ntry == null) {
            return null;
        }
        ntry.stopWork();
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
        srv.srvRename(nam);
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
            case 15:
            case 19:
                l = new userFormat("|", "interface|state|tx|rx|drop");
                break;
            case 3:
                l = new userFormat("|", "interface|state|bandwidth|vrf");
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
            case 17:
            case 21:
                l = new userFormat("|", "interface|state|tx|rx|drop");
                break;
            case 10:
            case 23:
            case 25:
                l = new userFormat("|", "interface|state|tx|rx|drop");
                break;
            case 11:
            case 16:
            case 20:
                l = new userFormat("|", "interface|state|tx|rx|drop");
                break;
            case 12:
            case 18:
            case 22:
                l = new userFormat("|", "interface|state|tx|rx|drop");
                break;
            case 13:
            case 24:
            case 26:
                l = new userFormat("|", "interface|state|tx|rx|drop");
                break;
            case 14:
                l = new userFormat("|", "interface|system|port|state");
                break;
            case 27:
                l = new userFormat("|", "interface|state|mtu|macsec|sgt|changes|for|since");
                break;
            case 28:
                l = new userFormat("|", "interface|state|input|output|drop");
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
        userFormat l = new userFormat("|", "name|type|mode|target|state|changes|changed");
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
     * @param what what to do: 1=tunnelDest, 2=clearSwCntr, 3=autoBw, 4=flwTrc,
     * 5=clearHwCntr, 6=clearAllCntr, 7=countHwCntr
     * @return value
     */
    public static int moreInterfaces(int what) {
        int res = 0;
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
                    ifc.ethtyp.clearSwCounter();
                    break;
                case 3:
                    ifc.autoBandwidth();
                    break;
                case 4:
                    ifc.followTracker();
                    break;
                case 5:
                    ifc.ethtyp.clearHwCounter();
                    break;
                case 6:
                    ifc.ethtyp.clearSwCounter();
                    ifc.ethtyp.clearHwCounter();
                    break;
                case 7:
                    if (ifc.ethtyp.hwCntr != null) {
                        res++;
                    }
                    break;
            }
        }
        return res;
    }

    private static List<String> getGlobalRunBeg(int filter) {
        List<String> l = new ArrayList<String>();
        l.add("hostname " + hostName);
        cmds.cfgLine(l, verCore.release, "", "buggy", "");
        cmds.cfgLine(l, passEnc == null, "", "password-encrypt", "" + authLocal.passwdHide(passEnc, (filter & 2) != 0));
        cmds.cfgLine(l, enaPass == null, "", "enable", authLocal.secretEncode(enaPass, (filter & 2) != 0));
        l.add("banner encoded " + cryBase64.encodeBytes(banner));
        l.add(cmds.comment);
        l.add("client label-range " + labelRangeBeg + " " + labelRangeEnd);
        cmds.cfgLine(l, !logger.logMillis, "", "logging milliseconds", "");
        l.add("logging buffered " + logger.level2string(logger.logBufLev) + " " + logger.getBufSize());
        l.add("logging monitor " + logger.level2string(logger.logPipLev));
        l.add("logging format " + logger.format2string(logger.logPosForm));
        String a = logger.fileName();
        cmds.cfgLine(l, a == null, "", "logging file " + logger.level2string(logger.logFilLev), a);
        a = logger.fileRotate();
        cmds.cfgLine(l, a == null, "", "logging rotate", a);
        cmds.cfgLine(l, logger.logProxy == null, "", "logging proxy", "" + logger.logProxy);
        a = "";
        for (int i = 0; i < logger.logSylHnd.size(); i++) {
            a += " " + logger.logSylHnd.get(i);
        }
        a = a.trim();
        cmds.cfgLine(l, a.length() < 1, "", "logging syslog " + logger.level2string(logger.logSylLev) + " " + servSyslog.num2facility(logger.logSylFac), a);
        a = "" + logger.logIrcHnd;
        cmds.cfgLine(l, a.length() < 1, "", "logging irc " + logger.level2string(logger.logIrcLev), a);
        l.add(cmds.comment);
        if ((filter & 1) == 0) {
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

    private static List<String> getGlobalRunEnd(int filter) {
        List<String> l = new ArrayList<String>();
        l.add("client ifacestall " + ifaceStallCheck);
        l.add("client redundancy " + redundancyKeep + " " + redundancyHold + " " + redundancyInit);
        l.add("client cpuhog " + cpuhogCheck);
        if (clientShamer == null) {
            l.add("no client bullying");
        } else {
            l.add("client bullying " + clientShamer.srvName);
        }
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
        cmds.cfgLine(l, domainName == null, "", "client domain-name", domainName);
        cmds.cfgLine(l, !preferIpv6, "", "client prefer-ipv6", "");
        cmds.cfgLine(l, !passwdStars, "", "client password-stars", "");
        cmds.cfgLine(l, whoisServer == null, "", "client whois-server", whoisServer);
        cmds.cfgLine(l, whoisProxy == null, "", "client whois-proxy", "" + whoisProxy);
        cmds.cfgLine(l, !graceReload, "", "client graceful-reload", "");
        l.add("client ipv4-checksum " + rxtx2string(ipv4ChecksumRx, ipv4ChecksumTx));
        l.add("client icmp4-checksum " + rxtx2string(icmp4ChecksumRx, icmp4ChecksumTx));
        l.add("client icmp6-checksum " + rxtx2string(icmp6ChecksumRx, icmp6ChecksumTx));
        l.add("client udp-checksum " + rxtx2string(udpChecksumRx, udpChecksumTx));
        l.add("client udp-portrange " + udpRangeMin + " " + udpRangeMax);
        l.add("client tcp-checksum " + rxtx2string(tcpChecksumRx, tcpChecksumTx));
        l.add("client tcp-segments " + tcpMaxSegment);
        l.add("client tcp-winscale " + tcpWinScale);
        cmds.cfgLine(l, !tcpTimStmp, "", "client tcp-timestamp", "");
        l.add("client tcp-portrange " + tcpRangeMin + " " + tcpRangeMax);
        l.add("client ludp-checksum " + rxtx2string(ludpChecksumRx, ludpChecksumTx));
        l.add("client ludp-portrange " + ludpRangeMin + " " + ludpRangeMax);
        l.add("client dccp-checksum " + rxtx2string(dccpChecksumRx, dccpChecksumTx));
        l.add("client dccp-portrange " + dccpRangeMin + " " + dccpRangeMax);
        l.add("client sctp-checksum " + rxtx2string(sctpChecksumRx, sctpChecksumTx));
        l.add("client sctp-portrange " + sctpRangeMin + " " + sctpRangeMax);
        l.add("client l2tp2-timer " + l2tp2hello + " " + l2tp2retry);
        l.add("client l2tp3-timer " + l2tp3hello + " " + l2tp3retry);
        String a = "";
        for (int i = 0; i < nameServerAddr.size(); i++) {
            a += " " + nameServerAddr.get(i);
        }
        l.add("client name-server" + a);
        cmds.cfgLine(l, !ftpPassive, "", "client ftp-passive", "");
        cmds.cfgLine(l, ftpProxy == null, "", "client ftp-proxy", "" + ftpProxy);
        cmds.cfgLine(l, upgradePubKey == null, "", "client upgrade-pubkey", "" + upgradePubKey);
        cmds.cfgLine(l, upgradeServer == null, "", "client upgrade-server", "" + upgradeServer);
        cmds.cfgLine(l, !upgradeConfig, "", "client upgrade-config", "");
        cmds.cfgLine(l, !upgradeBackup, "", "client upgrade-backup", "");
        cmds.cfgLine(l, upgradeRevert == 0, "", "client upgrade-revert", "" + upgradeRevert);
        cmds.cfgLine(l, !upgradeOwnKey, "", "client upgrade-ownkey", "");
        if (upgradeScript == null) {
            l.add("no client upgrade-script");
        } else {
            l.add("client upgrade-script " + upgradeScript.name);
        }
        cmds.cfgLine(l, configServer == null, "", "client config-server", "" + configServer);
        cmds.cfgLine(l, configUser == null, "", "client config-username", "" + configUser);
        cmds.cfgLine(l, configPass == null, "", "client config-password", "" + authLocal.passwdEncode(configPass, (filter & 2) != 0));
        cmds.cfgLine(l, configBackup == null, "", "client config-backup", "" + configBackup);
        cmds.cfgLine(l, !configAsave, "", "client config-save", "");
        cmds.cfgLine(l, !configAbackup, "", "client config-archive", "");
        cmds.cfgLine(l, configExclusive < 1, "", "client config-exclusive", "");
        l.add("client access-subnet-ipv4 " + accessSubnet4);
        l.add("client access-subnet-ipv6 " + accessSubnet6);
        l.add("client tls-version " + tlsVerMin + " " + tlsVerMax);
        cmds.cfgLine(l, timeServerName == null, "", "client time-server", "" + timeServerName);
        l.add("client time-zone " + timeZoneName);
        cmds.cfgLine(l, timeProxy == null, "", "client time-proxy", "" + timeProxy);
        cmds.cfgLine(l, tftpProxy == null, "", "client tftp-proxy", "" + tftpProxy);
        cmds.cfgLine(l, httpProxy == null, "", "client http-proxy", "" + httpProxy);
        cmds.cfgLine(l, mailProxy == null, "", "client mail-proxy", "" + mailProxy);
        cmds.cfgLine(l, mailServerName == null, "", "client mail-server", "" + mailServerName);
        cmds.cfgLine(l, mailServerUser == null, "", "client mail-username", "" + mailServerUser);
        cmds.cfgLine(l, mailServerPass == null, "", "client mail-password", "" + authLocal.passwdEncode(mailServerPass, (filter & 2) != 0));
        a = "";
        if ((endForm & 0x1) != 0) {
            a += " date";
        }
        if ((endForm & 0x2) != 0) {
            a += " image";
        }
        if ((endForm & 0x4) != 0) {
            a += " chksum";
        }
        cmds.cfgLine(l, endForm == 0, "", "client end-format", a.trim());
        l.add(cmds.comment);
        if ((filter & 1) == 0) {
            return l;
        }
        return userFilter.filterText(l, defaultF);
    }

    /**
     * build running configuration
     *
     * @param filter defaults
     * @return text to display
     */
    public static List<String> getShRun(int filter) {
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
        servGenList.listGetRun(l, sessns, filter);
        servGenList.listGetRun(l, menus, filter);
        servGenList.listGetRun(l, schedulers, filter);
        servGenList.listGetRun(l, scripts, filter);
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
        servGenList.listGetRun(l, vrfs, filter);
        for (int i = 0; i < routers.size(); i++) {
            l.addAll(routers.get(i).getShRun1(filter));
        }
        servGenList.listGetRun(l, ifaces, filter);
        for (int i = 0; i < routers.size(); i++) {
            l.addAll(routers.get(i).getShRun2(filter));
        }
        servGenList.listGetRun(l, lines, filter);
        servGenList.listGetRun(l, proxys, filter);
        servGenList.listGetRun(l, vpdns, filter);
        servGenList.listGetRun(l, trackers, filter);
        servGenList.listGetRun(l, mtrackers, filter);
        for (int i = 0; i < vrfs.size(); i++) {
            l.addAll(vrfs.get(i).getShRun2(filter));
        }
        servGenList.listGetRun(l, xconnects, filter);
        servGenList.listGetRun(l, iconnects, filter);
        servGenList.listGetRun(l, tabNshEntry.services, filter);
        servGenList.listGetRun(l, checks, filter);
        servGenList.listGetRun(l, sensors, filter);
        servGenList.listGetRun(l, tlmtrydst, filter);
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
        dmnUdpFwd.getShRun(l, filter);
        dmnUpnpFwd.getShRun(l, filter);
        dmnUpnpHub.getShRun(l, filter);
        dmnOpenflow.getShRun(l, filter);
        dmnPktmux.getShRun(l, filter);
        dmnP4lang.getShRun(l, filter);
        dmnForwarder.getShRun(l, filter);
        dmnSyslog.getShRun(l, filter);
        dmnLoadBalancer.getShRun(l, filter);
        dmnMultiplexer.getShRun(l, filter);
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
        dmnSdwan.getShRun(l, filter);
        dmnPcep.getShRun(l, filter);
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
        dmnPrometheus.getShRun(l, filter);
        dmnStreamingMdt.getShRun(l, filter);
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
        dmnMplsOam.getShRun(l, filter);
        dmnTwamp.getShRun(l, filter);
        dmnAmt.getShRun(l, filter);
        dmnUni2mul.getShRun(l, filter);
        dmnGtp.getShRun(l, filter);
        dmnPptp.getShRun(l, filter);
        dmnRadius.getShRun(l, filter);
        dmnTacacs.getShRun(l, filter);
        l.addAll(getGlobalRunEnd(filter));
        String s = "";
        if ((endForm & 0x1) != 0) {
            s += " date=" + bits.time2str(timeZoneName, bits.getTime() + cfgAll.timeServerOffset, 3).replaceAll(" ", "_");
        }
        if ((endForm & 0x2) != 0) {
            s += " image=" + version.usrAgnt;
        }
        if ((endForm & 0x4) != 0) {
            s += " chksum=" + userUpgrade.calcTextHash(l);
        }
        l.add("end" + s);
        return l;
    }

}
