package org.freertr.cfg;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrIPv6;
import org.freertr.addr.addrType;
import org.freertr.auth.authLocal;
import org.freertr.clnt.clntNtp;
import org.freertr.clnt.clntProxy;
import org.freertr.enc.encBase64;
import org.freertr.cry.cryKeyDSA;
import org.freertr.cry.cryKeyECDSA;
import org.freertr.cry.cryKeyGeneric;
import org.freertr.cry.cryKeyMLDSA;
import org.freertr.cry.cryKeyRSA;
import org.freertr.ifc.ifcBridge;
import org.freertr.ifc.ifcBundle;
import org.freertr.ifc.ifcHairpin;
import org.freertr.ifc.ifcThread;
import org.freertr.line.lineThread;
import org.freertr.serv.servAmt;
import org.freertr.serv.servBmp2mrt;
import org.freertr.serv.servBstun;
import org.freertr.serv.servCharGen;
import org.freertr.serv.servDaytime;
import org.freertr.serv.servDcp;
import org.freertr.serv.servDhcp4;
import org.freertr.serv.servDhcp6;
import org.freertr.serv.servDiscard;
import org.freertr.serv.servDns;
import org.freertr.serv.servEchoS;
import org.freertr.serv.servEtherIp;
import org.freertr.serv.servForwarder;
import org.freertr.serv.servFtp;
import org.freertr.serv.servGenList;
import org.freertr.serv.servGeneric;
import org.freertr.serv.servGeneve;
import org.freertr.serv.servGopher;
import org.freertr.serv.servGre;
import org.freertr.serv.servGtp;
import org.freertr.serv.servHoneyPot;
import org.freertr.serv.servHttp;
import org.freertr.serv.servImap4;
import org.freertr.serv.servIrc;
import org.freertr.serv.servIscsi;
import org.freertr.serv.servL2f;
import org.freertr.serv.servL2tp2;
import org.freertr.serv.servL2tp3;
import org.freertr.serv.servLoadBalancer;
import org.freertr.serv.servLpd;
import org.freertr.serv.servModem;
import org.freertr.serv.servMplsIp;
import org.freertr.serv.servMplsOam;
import org.freertr.serv.servMplsUdp;
import org.freertr.serv.servMrt2bgp;
import org.freertr.serv.servMultiplexer;
import org.freertr.serv.servNetflow;
import org.freertr.serv.servNrpe;
import org.freertr.serv.servNtp;
import org.freertr.serv.servOpenflow;
import org.freertr.serv.servPktmux;
import org.freertr.serv.servP4lang;
import org.freertr.serv.servPcep;
import org.freertr.serv.servPckOdtls;
import org.freertr.serv.servPckOtcp;
import org.freertr.serv.servPckOtxt;
import org.freertr.serv.servPckOudp;
import org.freertr.serv.servPlan9;
import org.freertr.serv.servPop3;
import org.freertr.serv.servPptp;
import org.freertr.serv.servPrometheus;
import org.freertr.serv.servQuote;
import org.freertr.serv.servRadius;
import org.freertr.serv.servRfb;
import org.freertr.serv.servRpki;
import org.freertr.serv.servRtpStat;
import org.freertr.serv.servSdwan;
import org.freertr.serv.servSip;
import org.freertr.serv.servSmtp;
import org.freertr.serv.servSnmp;
import org.freertr.serv.servSocks;
import org.freertr.serv.servStack;
import org.freertr.serv.servStreamingMdt;
import org.freertr.serv.servStun;
import org.freertr.serv.servSyslog;
import org.freertr.serv.servTacacs;
import org.freertr.serv.servTelnet;
import org.freertr.serv.servTftp;
import org.freertr.serv.servTime;
import org.freertr.serv.servTwamp;
import org.freertr.serv.servUdpFwd;
import org.freertr.serv.servUdptn;
import org.freertr.serv.servUni2multi;
import org.freertr.serv.servUni2uni;
import org.freertr.serv.servUpnpFwd;
import org.freertr.serv.servUpnpHub;
import org.freertr.serv.servVoice;
import org.freertr.serv.servVxlan;
import org.freertr.serv.servWhois;
import org.freertr.serv.servXotPad;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabNshEntry;
import org.freertr.tab.tabRouteAttr;
import org.freertr.tab.tabRouteIface;
import org.freertr.user.userFilter;
import org.freertr.user.userFormat;
import org.freertr.user.userHelp;
import org.freertr.user.userLine;
import org.freertr.user.userReload;
import org.freertr.user.userUpgrade;
import org.freertr.util.bits;
import org.freertr.util.chatter;
import org.freertr.util.cmds;
import org.freertr.util.logger;
import org.freertr.util.version;

/**
 * configuration settings
 *
 * @author matecsaba
 */
public class cfgAll {

    /**
     * list of vdcs
     */
    public final static tabGen<cfgVdc> vdcs = new tabGen<cfgVdc>();

    /**
     * list of processes
     */
    public final static tabGen<cfgPrcss> prcs = new tabGen<cfgPrcss>();

    /**
     * list of vrfs
     */
    public final static tabGen<cfgVrf> vrfs = new tabGen<cfgVrf>();

    /**
     * list of interfaces
     */
    public final static tabGen<cfgIfc> ifaces = new tabGen<cfgIfc>();

    /**
     * list of lines
     */
    public final static tabGen<cfgLin> lines = new tabGen<cfgLin>();

    /**
     * console template
     */
    public final static cfgCons con0 = new cfgCons();

    /**
     * list of key menus
     */
    public final static tabGen<cfgMenuK> menuk = new tabGen<cfgMenuK>();

    /**
     * list of tui menus
     */
    public final static tabGen<cfgMenuT> menut = new tabGen<cfgMenuT>();

    /**
     * list of bridges
     */
    public final static tabGen<cfgBrdg> bridges = new tabGen<cfgBrdg>();

    /**
     * list of bundles
     */
    public final static tabGen<cfgBndl> bundles = new tabGen<cfgBndl>();

    /**
     * list of hairpins
     */
    public final static tabGen<cfgHrpn> hairpins = new tabGen<cfgHrpn>();

    /**
     * list of vnets
     */
    public final static tabGen<cfgVnet> vnets = new tabGen<cfgVnet>();

    /**
     * list of translation rules
     */
    public final static tabGen<cfgTrnsltn> trnsltns = new tabGen<cfgTrnsltn>();

    /**
     * list of dial peers
     */
    public final static tabGen<cfgDial> dials = new tabGen<cfgDial>();

    /**
     * list of sessions
     */
    public final static tabGen<cfgSessn> sessns = new tabGen<cfgSessn>();

    /**
     * list of checks
     */
    public final static tabGen<cfgCheck> checks = new tabGen<cfgCheck>();

    /**
     * list of sensors
     */
    public final static tabGen<cfgSensor> sensors = new tabGen<cfgSensor>();

    /**
     * list of routers
     */
    public final static tabGen<cfgRtr> routers = new tabGen<cfgRtr>();

    /**
     * list of network object groups
     */
    public final static tabGen<cfgObjnet> objgrpnets = new tabGen<cfgObjnet>();

    /**
     * list of port object groups
     */
    public final static tabGen<cfgObjprt> objgrpprts = new tabGen<cfgObjprt>();

    /**
     * list of access lists
     */
    public final static tabGen<cfgAceslst> accesslsts = new tabGen<cfgAceslst>();

    /**
     * list of event managers
     */
    public final static tabGen<cfgEvntmgr> eventmgrs = new tabGen<cfgEvntmgr>();

    /**
     * list of telemetry destinations
     */
    public final static tabGen<cfgTlmtry> tlmtrydst = new tabGen<cfgTlmtry>();

    /**
     * list of vpdns
     */
    public final static tabGen<cfgVpdn> vpdns = new tabGen<cfgVpdn>();

    /**
     * list of prefix lists
     */
    public final static tabGen<cfgPrfxlst> prefixlsts = new tabGen<cfgPrfxlst>();

    /**
     * list of route maps
     */
    public final static tabGen<cfgRoump> routemaps = new tabGen<cfgRoump>();

    /**
     * list of route policies
     */
    public final static tabGen<cfgRouplc> routeplcs = new tabGen<cfgRouplc>();

    /**
     * list of tine maps
     */
    public final static tabGen<cfgTime> timemaps = new tabGen<cfgTime>();

    /**
     * list of policy maps
     */
    public final static tabGen<cfgPlymp> policymaps = new tabGen<cfgPlymp>();

    /**
     * list of user lists
     */
    public final static tabGen<cfgAuther> authers = new tabGen<cfgAuther>();

    /**
     * list of schedulers
     */
    public final static tabGen<cfgSched> schedulers = new tabGen<cfgSched>();

    /**
     * list of scripts
     */
    public final static tabGen<cfgScrpt> scripts = new tabGen<cfgScrpt>();

    /**
     * list of trackers
     */
    public final static tabGen<cfgTrack> trackers = new tabGen<cfgTrack>();

    /**
     * list of mtrackers
     */
    public final static tabGen<cfgMtrack> mtrackers = new tabGen<cfgMtrack>();

    /**
     * list of certificates
     */
    public final static tabGen<cfgCert> certs = new tabGen<cfgCert>();

    /**
     * list of ipsec profiles
     */
    public final static tabGen<cfgIpsec> ipsecs = new tabGen<cfgIpsec>();

    /**
     * list of proxy profiles
     */
    public final static tabGen<cfgProxy> proxys = new tabGen<cfgProxy>();

    /**
     * list of chat scripts
     */
    public final static tabGen<cfgChat> chats = new tabGen<cfgChat>();

    /**
     * list of rsa keys
     */
    public final static tabGen<cfgKey<cryKeyRSA>> rsakeys = new tabGen<cfgKey<cryKeyRSA>>();

    /**
     * list of dsa keys
     */
    public final static tabGen<cfgKey<cryKeyDSA>> dsakeys = new tabGen<cfgKey<cryKeyDSA>>();

    /**
     * list of ecdsa keys
     */
    public final static tabGen<cfgKey<cryKeyECDSA>> ecdsakeys = new tabGen<cfgKey<cryKeyECDSA>>();

    /**
     * list of mldsa keys
     */
    public final static tabGen<cfgKey<cryKeyMLDSA>> mldsakeys = new tabGen<cfgKey<cryKeyMLDSA>>();

    /**
     * list of ipv4 pools
     */
    public final static tabGen<cfgPool<addrIPv4>> ip4pool = new tabGen<cfgPool<addrIPv4>>();

    /**
     * list of ipv6 pools
     */
    public final static tabGen<cfgPool<addrIPv6>> ip6pool = new tabGen<cfgPool<addrIPv6>>();

    /**
     * list of xconnections
     */
    public final static tabGen<cfgXconn> xconnects = new tabGen<cfgXconn>();

    /**
     * list of aliases
     */
    public final static tabGen<cfgAlias> aliases = new tabGen<cfgAlias>();

    /**
     * echo daemons
     */
    public final static servGenList<servEchoS> dmnEcho = new servGenList<servEchoS>();

    /**
     * discard daemons
     */
    public final static servGenList<servDiscard> dmnDiscard = new servGenList<servDiscard>();

    /**
     * quote daemons
     */
    public final static servGenList<servQuote> dmnQuote = new servGenList<servQuote>();

    /**
     * chargen daemons
     */
    public final static servGenList<servCharGen> dmnCharGen = new servGenList<servCharGen>();

    /**
     * netflow daemons
     */
    public final static servGenList<servNetflow> dmnNetflow = new servGenList<servNetflow>();

    /**
     * udpfwd daemons
     */
    public final static servGenList<servUdpFwd> dmnUdpFwd = new servGenList<servUdpFwd>();

    /**
     * upnpfwd daemons
     */
    public final static servGenList<servUpnpFwd> dmnUpnpFwd = new servGenList<servUpnpFwd>();

    /**
     * upnphub daemons
     */
    public final static servGenList<servUpnpHub> dmnUpnpHub = new servGenList<servUpnpHub>();

    /**
     * openflow daemons
     */
    public final static servGenList<servOpenflow> dmnOpenflow = new servGenList<servOpenflow>();

    /**
     * pktmux daemons
     */
    public final static servGenList<servPktmux> dmnPktmux = new servGenList<servPktmux>();

    /**
     * p4lang daemons
     */
    public final static servGenList<servP4lang> dmnP4lang = new servGenList<servP4lang>();

    /**
     * stack daemons
     */
    public final static servGenList<servStack> dmnStack = new servGenList<servStack>();

    /**
     * forwarder daemons
     */
    public final static servGenList<servForwarder> dmnForwarder = new servGenList<servForwarder>();

    /**
     * syslog daemons
     */
    public final static servGenList<servSyslog> dmnSyslog = new servGenList<servSyslog>();

    /**
     * loadbalancer daemons
     */
    public final static servGenList<servLoadBalancer> dmnLoadBalancer = new servGenList<servLoadBalancer>();

    /**
     * multiplexer daemons
     */
    public final static servGenList<servMultiplexer> dmnMultiplexer = new servGenList<servMultiplexer>();

    /**
     * telnet daemons
     */
    public final static servGenList<servTelnet> dmnTelnet = new servGenList<servTelnet>();

    /**
     * xotpad daemons
     */
    public final static servGenList<servXotPad> dmnXotpad = new servGenList<servXotPad>();

    /**
     * rfb daemons
     */
    public final static servGenList<servRfb> dmnRfb = new servGenList<servRfb>();

    /**
     * udptn daemons
     */
    public final static servGenList<servUdptn> dmnUdptn = new servGenList<servUdptn>();

    /**
     * http daemons
     */
    public final static servGenList<servHttp> dmnHttp = new servGenList<servHttp>();

    /**
     * lpd daemons
     */
    public final static servGenList<servLpd> dmnLpd = new servGenList<servLpd>();

    /**
     * honeypot daemons
     */
    public final static servGenList<servHoneyPot> dmnHoney = new servGenList<servHoneyPot>();

    /**
     * whois daemons
     */
    public final static servGenList<servWhois> dmnWhois = new servGenList<servWhois>();

    /**
     * dhcp4 daemons
     */
    public final static servGenList<servDhcp4> dmnDhcp4 = new servGenList<servDhcp4>();

    /**
     * dhcp6 daemons
     */
    public final static servGenList<servDhcp6> dmnDhcp6 = new servGenList<servDhcp6>();

    /**
     * dns daemons
     */
    public final static servGenList<servDns> dmnDns = new servGenList<servDns>();

    /**
     * pop3 daemons
     */
    public final static servGenList<servPop3> dmnPop3 = new servGenList<servPop3>();

    /**
     * imap4 daemons
     */
    public final static servGenList<servImap4> dmnImap4 = new servGenList<servImap4>();

    /**
     * smtp daemons
     */
    public final static servGenList<servSmtp> dmnSmtp = new servGenList<servSmtp>();

    /**
     * sip modem daemons
     */
    public final static servGenList<servModem> dmnModem = new servGenList<servModem>();

    /**
     * sip voice daemons
     */
    public final static servGenList<servVoice> dmnVoice = new servGenList<servVoice>();

    /**
     * sip proxy daemons
     */
    public final static servGenList<servSip> dmnSip = new servGenList<servSip>();

    /**
     * ftp daemons
     */
    public final static servGenList<servFtp> dmnFtp = new servGenList<servFtp>();

    /**
     * tftp daemons
     */
    public final static servGenList<servTftp> dmnTftp = new servGenList<servTftp>();

    /**
     * gopher daemons
     */
    public final static servGenList<servGopher> dmnGopher = new servGenList<servGopher>();

    /**
     * plan9 daemons
     */
    public final static servGenList<servPlan9> dmnPlan9 = new servGenList<servPlan9>();

    /**
     * ntp daemons
     */
    public final static servGenList<servNtp> dmnNtp = new servGenList<servNtp>();

    /**
     * daytime daemons
     */
    public final static servGenList<servDaytime> dmnDaytime = new servGenList<servDaytime>();

    /**
     * rtpstat daemons
     */
    public final static servGenList<servRtpStat> dmnRtpStat = new servGenList<servRtpStat>();

    /**
     * time daemons
     */
    public final static servGenList<servTime> dmnTime = new servGenList<servTime>();

    /**
     * snmp daemons
     */
    public final static servGenList<servSnmp> dmnSnmp = new servGenList<servSnmp>();

    /**
     * iscsi daemons
     */
    public final static servGenList<servIscsi> dmnIscsi = new servGenList<servIscsi>();

    /**
     * bmp2mrt daemons
     */
    public final static servGenList<servBmp2mrt> dmnBmp = new servGenList<servBmp2mrt>();

    /**
     * irc daemons
     */
    public final static servGenList<servIrc> dmnIrc = new servGenList<servIrc>();

    /**
     * dcp daemons
     */
    public final static servGenList<servDcp> dmnDcp = new servGenList<servDcp>();

    /**
     * sdwan daemons
     */
    public final static servGenList<servSdwan> dmnSdwan = new servGenList<servSdwan>();

    /**
     * pcep daemons
     */
    public final static servGenList<servPcep> dmnPcep = new servGenList<servPcep>();

    /**
     * socks daemons
     */
    public final static servGenList<servSocks> dmnSocks = new servGenList<servSocks>();

    /**
     * rpki daemons
     */
    public final static servGenList<servRpki> dmnRpki = new servGenList<servRpki>();

    /**
     * nrpe daemons
     */
    public final static servGenList<servNrpe> dmnNrpe = new servGenList<servNrpe>();

    /**
     * prometheus daemons
     */
    public final static servGenList<servPrometheus> dmnPrometheus = new servGenList<servPrometheus>();

    /**
     * streaming telemetry daemons
     */
    public final static servGenList<servStreamingMdt> dmnStreamingMdt = new servGenList<servStreamingMdt>();

    /**
     * mrt2bgp daemons
     */
    public final static servGenList<servMrt2bgp> dmnMrt2bgp = new servGenList<servMrt2bgp>();

    /**
     * bstun daemons
     */
    public final static servGenList<servBstun> dmnBStun = new servGenList<servBstun>();

    /**
     * stun daemons
     */
    public final static servGenList<servStun> dmnStun = new servGenList<servStun>();

    /**
     * pckoudp daemons
     */
    public final static servGenList<servPckOudp> dmnPckOudp = new servGenList<servPckOudp>();

    /**
     * pckodtls daemons
     */
    public final static servGenList<servPckOdtls> dmnPckOdtls = new servGenList<servPckOdtls>();

    /**
     * pckotcp daemons
     */
    public final static servGenList<servPckOtcp> dmnPckOtcp = new servGenList<servPckOtcp>();

    /**
     * pckotxt daemons
     */
    public final static servGenList<servPckOtxt> dmnPckOtxt = new servGenList<servPckOtxt>();

    /**
     * vxlan daemons
     */
    public final static servGenList<servVxlan> dmnVxlan = new servGenList<servVxlan>();

    /**
     * geneve daemons
     */
    public final static servGenList<servGeneve> dmnGeneve = new servGenList<servGeneve>();

    /**
     * l2f daemons
     */
    public final static servGenList<servL2f> dmnL2f = new servGenList<servL2f>();

    /**
     * l2tp2 daemons
     */
    public final static servGenList<servL2tp2> dmnL2tp2 = new servGenList<servL2tp2>();

    /**
     * l2tp3 daemons
     */
    public final static servGenList<servL2tp3> dmnL2tp3 = new servGenList<servL2tp3>();

    /**
     * etherip daemons
     */
    public final static servGenList<servEtherIp> dmnEtherIp = new servGenList<servEtherIp>();

    /**
     * gre daemons
     */
    public final static servGenList<servGre> dmnGre = new servGenList<servGre>();

    /**
     * mplsip daemons
     */
    public final static servGenList<servMplsIp> dmnMplsIp = new servGenList<servMplsIp>();

    /**
     * mplsudp daemons
     */
    public final static servGenList<servMplsUdp> dmnMplsUdp = new servGenList<servMplsUdp>();

    /**
     * mplsoam daemons
     */
    public final static servGenList<servMplsOam> dmnMplsOam = new servGenList<servMplsOam>();

    /**
     * twamp daemons
     */
    public final static servGenList<servTwamp> dmnTwamp = new servGenList<servTwamp>();

    /**
     * amt daemons
     */
    public final static servGenList<servAmt> dmnAmt = new servGenList<servAmt>();

    /**
     * uni2multi daemons
     */
    public final static servGenList<servUni2multi> dmnUni2mul = new servGenList<servUni2multi>();

    /**
     * uni2uni daemons
     */
    public final static servGenList<servUni2uni> dmnUni2uni = new servGenList<servUni2uni>();

    /**
     * gtp daemons
     */
    public final static servGenList<servGtp> dmnGtp = new servGenList<servGtp>();

    /**
     * pptp daemons
     */
    public final static servGenList<servPptp> dmnPptp = new servGenList<servPptp>();

    /**
     * radius daemons
     */
    public final static servGenList<servRadius> dmnRadius = new servGenList<servRadius>();

    /**
     * tacacs daemons
     */
    public final static servGenList<servTacacs> dmnTacacs = new servGenList<servTacacs>();

    /**
     * name of this host
     */
    public static String hostName = "router";

    /**
     * domain of this host
     */
    public static String domainName = null;

    /**
     * allow domain lookup
     */
    public static boolean domainLookup = false;

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
     * locale of this host
     */
    public static String locale = null;

    /**
     * running in a vdc
     */
    public static boolean invdc = false;

    /**
     * enable experimental features
     */
    public static boolean buggy = true;

    /**
     * minimum tls version to use
     */
    public static int tlsVerMin = 0;

    /**
     * maximum tls version to use
     */
    public static int tlsVerMax = 4;

    /**
     * minimum ssh group to use
     */
    public static int sshGrpMin = 1024;

    /**
     * maximum ssh group to use
     */
    public static int sshGrpMax = 4096;

    /**
     * user agent to fake
     */
    public static String sshAgent = null;

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
     * whois option
     */
    public static String whoisOption = null;

    /**
     * whois file
     */
    public static String whoisFile = null;

    /**
     * list of asn tools
     */
    public static List<String> whoisOnline = null;

    /**
     * proxy to use
     */
    public static clntProxy whoisProxy;

    /**
     * chatter
     */
    public final static chatter chat = new chatter();

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
     * user agent to fake
     */
    public static String httpAgent = null;

    /**
     * access subnet prefix length
     */
    public static int accessSubnet4 = 120;

    /**
     * access subnet prefix length
     */
    public static int accessSubnet6 = 64;

    /**
     * access subnet prefix length
     */
    public static int accessSupnet4 = 104;

    /**
     * access subnet prefix length
     */
    public static int accessSupnet6 = 18;

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
    public static String upgradeServer = version.homeUrl;

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
     * treat traceback as exception
     */
    public static boolean tracebackStops = false;

    /**
     * always on debugs
     */
    public static List<String> alwaysDebugs = new ArrayList<String>();

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
     * redundancy takeover time
     */
    public static int redundancyTake = 300 * 1000;

    /**
     * passive mode ftp
     */
    public static boolean ftpPassive = true;

    /**
     * proxy to use
     */
    public static clntProxy ftpProxy;

    /**
     * proxy to use
     */
    public static clntProxy pop3proxy;

    /**
     * ipv4 sending ttl value
     */
    public static int ipv4sendingTTL = 255;

    /**
     * ipv4 sending tos value
     */
    public static int ipv4sendingTOS = 0;

    /**
     * ipv6 sending ttl value
     */
    public static int ipv6sendingTTL = 255;

    /**
     * ipv6 sending tos value
     */
    public static int ipv6sendingTOS = 0;

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
     * tcp minimum segment
     */
    public static int tcpSegmentMin = 1024;

    /**
     * tcp maximum segment
     */
    public static int tcpSegmentMax = 1024;

    /**
     * tcp window scale
     */
    public static int tcpWinScale = 1;

    /**
     * tcp timestamps
     */
    public static boolean tcpTimStmp = false;

    /**
     * tcp ecn
     */
    public static boolean tcpEcn = false;

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
     * worker timer
     */
    public static int tcpTimeWork = 1000;

    /**
     * send keepalives
     */
    public static boolean tcpKeepalive = false;

    /**
     * keepalive timer
     */
    public static int tcpTimeAlive = 60 * 1000;

    /**
     * timeout while closing
     */
    public static int tcpTimeFin = 45 * 1000;

    /**
     * timeout while connecting
     */
    public static int tcpTimeSyn = 30 * 1000;

    /**
     * timeout while open
     */
    public static int tcpTimeOpen = 300 * 1000;

    /**
     * timeout while closing with data
     */
    public static int tcpTimeClose = 120 * 1000;

    /**
     * delayed timer
     */
    public static int tcpTimeLater = 3 * 1000;

    /**
     * now timeout
     */
    public static int tcpTimeNow = 100;

    /**
     * maximum timeout
     */
    public static int tcpTimeMax = 8 * 1000;

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
     * pastebin location
     */
    public static String pasteBin = null;

    /**
     * capture location
     */
    public static String capturePath = null;

    /**
     * l2tp2 hello ticks
     */
    public static int l2fTimer = 5000;

    /**
     * l2tp2 retry ticks
     */
    public static int l2fRetry = 8;

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
     * custom defaults text
     */
    public static userFilter[] custDefs = new userFilter[0];

    /**
     * loaded snmp mibs
     */
    public static userFilter snmpMibs[] = new userFilter[0];

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        // logging
        new userFilter("", "logging buffered debug 512", null),
        new userFilter("", "logging monitor debug", null),
        new userFilter("", "logging format normal", null),
        new userFilter("", cmds.negated + cmds.tabulator + "logging milliseconds", null),
        new userFilter("", cmds.negated + cmds.tabulator + "logging proxy", null),
        new userFilter("", cmds.negated + cmds.tabulator + "logging rotate", null),
        new userFilter("", cmds.negated + cmds.tabulator + "logging syslog debug kernel", null),
        new userFilter("", cmds.negated + cmds.tabulator + "logging file debug", null),
        new userFilter("", cmds.negated + cmds.tabulator + "logging irc debug", null),
        new userFilter("", cmds.negated + cmds.tabulator + "logging tracestop", null),
        new userFilter("", "banner encoded ", null),
        new userFilter("", cmds.negated + cmds.tabulator + "password-encrypt", null),
        new userFilter("", cmds.negated + cmds.tabulator + "enable", null),
        new userFilter("", cmds.negated + cmds.tabulator + "locale", null),
        // client
        new userFilter("", cmds.negated + cmds.tabulator + "client pastebin", null),
        new userFilter("", cmds.negated + cmds.tabulator + "client capture-path", null),
        new userFilter("", "client label-range 32 1048560", null),
        new userFilter("", "client cpuhog 0", null),
        new userFilter("", "client ifacestall 60000", null),
        new userFilter("", "client redundancy-keepalive 500", null),
        new userFilter("", "client redundancy-hold 2500", null),
        new userFilter("", "client redundancy-initial 5000", null),
        new userFilter("", "client redundancy-takeover 300000", null),
        new userFilter("", "client l2f-timer 5000 8", null),
        new userFilter("", "client l2tp2-timer 5 8", null),
        new userFilter("", "client l2tp3-timer 5 8", null),
        new userFilter("", cmds.negated + cmds.tabulator + "client bullying", null),
        new userFilter("", cmds.negated + cmds.tabulator + "client proxy", null),
        new userFilter("", cmds.negated + cmds.tabulator + "client domain-name", null),
        new userFilter("", cmds.negated + cmds.tabulator + "client domain-lookup", null),
        new userFilter("", cmds.negated + cmds.tabulator + "client name-proxy", null),
        new userFilter("", "client name-server", null),
        new userFilter("", "client time-zone Z", null),
        new userFilter("", cmds.negated + cmds.tabulator + "client time-proxy", null),
        new userFilter("", cmds.negated + cmds.tabulator + "client tftp-proxy", null),
        new userFilter("", cmds.negated + cmds.tabulator + "client http-proxy", null),
        new userFilter("", cmds.negated + cmds.tabulator + "client http-agent", null),
        new userFilter("", "client access-subnet-ipv4 120", null),
        new userFilter("", "client access-subnet-ipv6 64", null),
        new userFilter("", "client access-supernet-ipv4 104", null),
        new userFilter("", "client access-supernet-ipv6 18", null),
        new userFilter("", "client tls-version 1 4", null),
        new userFilter("", "client ssh-group 1024 4096", null),
        new userFilter("", cmds.negated + cmds.tabulator + "client ssh-agent", null),
        new userFilter("", cmds.negated + cmds.tabulator + "client time-server", null),
        new userFilter("", cmds.negated + cmds.tabulator + "client upgrade-pubkey", null),
        new userFilter("", "client upgrade-server " + version.homeUrl, null),
        new userFilter("", cmds.negated + cmds.tabulator + "client upgrade-config", null),
        new userFilter("", cmds.negated + cmds.tabulator + "client upgrade-backup", null),
        new userFilter("", cmds.negated + cmds.tabulator + "client upgrade-revert", null),
        new userFilter("", cmds.negated + cmds.tabulator + "client upgrade-script", null),
        new userFilter("", cmds.negated + cmds.tabulator + "client upgrade-ownkey", null),
        new userFilter("", cmds.negated + cmds.tabulator + "client whois-proxy", null),
        new userFilter("", cmds.negated + cmds.tabulator + "client whois-server", null),
        new userFilter("", cmds.negated + cmds.tabulator + "client whois-option", null),
        new userFilter("", cmds.negated + cmds.tabulator + "client whois-file", null),
        new userFilter("", cmds.negated + cmds.tabulator + "client config-server", null),
        new userFilter("", cmds.negated + cmds.tabulator + "client config-username", null),
        new userFilter("", cmds.negated + cmds.tabulator + "client config-password", null),
        new userFilter("", cmds.negated + cmds.tabulator + "client config-save", null),
        new userFilter("", cmds.negated + cmds.tabulator + "client config-archive", null),
        new userFilter("", cmds.negated + cmds.tabulator + "client config-backup", null),
        new userFilter("", cmds.negated + cmds.tabulator + "client config-exclusive", null),
        new userFilter("", cmds.negated + cmds.tabulator + "client mail-proxy", null),
        new userFilter("", cmds.negated + cmds.tabulator + "client mail-server", null),
        new userFilter("", cmds.negated + cmds.tabulator + "client mail-username", null),
        new userFilter("", cmds.negated + cmds.tabulator + "client mail-password", null),
        new userFilter("", cmds.negated + cmds.tabulator + "client prefer-ipv6", null),
        new userFilter("", cmds.negated + cmds.tabulator + "client end-format", null),
        new userFilter("", "client graceful-reload", null),
        new userFilter("", "client ftp-passive", null),
        new userFilter("", cmds.negated + cmds.tabulator + "client ftp-proxy", null),
        new userFilter("", cmds.negated + cmds.tabulator + "client pop3-proxy", null),
        new userFilter("", "client ipv4-tos 0", null),
        new userFilter("", "client ipv4-ttl 255", null),
        new userFilter("", "client ipv6-tos 0", null),
        new userFilter("", "client ipv6-ttl 255", null),
        new userFilter("", "client ipv4-checksum both", null),
        new userFilter("", "client icmp4-checksum both", null),
        new userFilter("", "client icmp6-checksum both", null),
        new userFilter("", "client udp-checksum both", null),
        new userFilter("", "client udp-portrange 32768 61440", null),
        new userFilter("", "client tcp-segments 1024 1024", null),
        new userFilter("", "client tcp-winscale 1", null),
        new userFilter("", cmds.negated + cmds.tabulator + "client tcp-timestamp", null),
        new userFilter("", cmds.negated + cmds.tabulator + "client tcp-ecn", null),
        new userFilter("", cmds.negated + cmds.tabulator + "client tcp-keepalive", null),
        new userFilter("", "client tcp-timer work 1000", null),
        new userFilter("", "client tcp-timer alive 60000", null),
        new userFilter("", "client tcp-timer fin 45000", null),
        new userFilter("", "client tcp-timer syn 30000", null),
        new userFilter("", "client tcp-timer open 300000", null),
        new userFilter("", "client tcp-timer close 120000", null),
        new userFilter("", "client tcp-timer later 3000", null),
        new userFilter("", "client tcp-timer now 100", null),
        new userFilter("", "client tcp-timer max 8000", null),
        new userFilter("", "client tcp-checksum both", null),
        new userFilter("", "client tcp-portrange 32768 61440", null),
        new userFilter("", "client ludp-checksum both", null),
        new userFilter("", "client ludp-portrange 32768 61440", null),
        new userFilter("", "client dccp-checksum both", null),
        new userFilter("", "client dccp-portrange 32768 61440", null),
        new userFilter("", "client sctp-checksum both", null),
        new userFilter("", "client sctp-portrange 32768 61440", null)
    };

    private cfgAll() {
    }

    /**
     * check for extra vdc privileges
     *
     * @return true to forbid, false to allow
     */
    public static boolean evalVdcPrivs() {
        if (!invdc) {
            return false;
        }
        return !buggy;
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
        if (ipv4) {
            for (int i = 0; i < vrfs.size(); i++) {
                cfgVrf ntry = vrfs.get(i);
                if (ntry == null) {
                    continue;
                }
                if (ntry.fwd4.rd == rd) {
                    return ntry;
                }
            }
        } else {
            for (int i = 0; i < vrfs.size(); i++) {
                cfgVrf ntry = vrfs.get(i);
                if (ntry == null) {
                    continue;
                }
                if (ntry.fwd6.rd == rd) {
                    return ntry;
                }
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
        cfgSched ntry = new cfgSched(nam);
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
        cfgSched ntry = new cfgSched(nam);
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
        cfgScrpt ntry = new cfgScrpt(nam);
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
        cfgScrpt ntry = new cfgScrpt(nam);
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
        cfgTrack ntry = new cfgTrack(nam);
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
        cfgTrack ntry = new cfgTrack(nam);
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
        cfgMtrack ntry = new cfgMtrack(nam);
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
        cfgMtrack ntry = new cfgMtrack(nam);
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
        cfgKey<T> ntry = new cfgKey<T>(nam);
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
        cfgKey<T> ntry = new cfgKey<T>(nam);
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
        cfgPool<T> ntry = new cfgPool<T>(nam);
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
        cfgPool<T> ntry = new cfgPool<T>(nam);
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
        cfgAlias ntry = new cfgAlias(nam, typ);
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
        cfgAlias ntry = new cfgAlias(nam, typ);
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
            userHelp hlp) {
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
        ntry.closeAllConns(false);
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
     * @return descriptor, null on error
     */
    public static cfgIfc ifcAdd(String nam, tabRouteIface.ifaceType typ, ifcThread thrd, int wrkr) {
        String pnm[] = cfgIfc.dissectName(nam);
        if (pnm == null) {
            return null;
        }
        if (thrd != null) {
            thrd.startLoop(wrkr);
        }
        pnm[3] = "";
        pnm[4] = "";
        cfgIfc ntry = new cfgIfc(pnm);
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
        if (pnm == null) {
            return true;
        }
        cfgIfc ntry = new cfgIfc(pnm);
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
        if (pnm == null) {
            return null;
        }
        nam = pnm[0] + pnm[1] + pnm[2] + pnm[3] + pnm[4];
        cfgIfc ntry = new cfgIfc(pnm);
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
        if (pnm[4].length() > 0) {
            ntry.vlanNum = bits.str2num(pnm[4].substring(1, pnm[4].length()));
            pnm[4] = "";
            old = ifaces.find(new cfgIfc(pnm));
            if (old == null) {
                ifaces.del(ntry);
                return null;
            }
            ntry.initSubiface(old);
            return ntry;
        }
        if (pnm[3].length() > 0) {
            ntry.vlanNum = bits.str2num(pnm[3].substring(1, pnm[3].length()));
            pnm[3] = "";
            pnm[4] = "";
            old = ifaces.find(new cfgIfc(pnm));
            if (old == null) {
                ifaces.del(ntry);
                return null;
            }
            ntry.initSubiface(old);
            return ntry;
        }
        if (nam.startsWith("tunnel")) {
            ntry.type = tabRouteIface.ifaceType.tunnel;
            ntry.clear2tunnel(false);
            return ntry;
        }
        if (nam.startsWith("dialer")) {
            ntry.type = tabRouteIface.ifaceType.dialer;
            ntry.initPhysical();
            return ntry;
        }
        if (nam.startsWith("sdn")) {
            ntry.type = tabRouteIface.ifaceType.sdn;
            ntry.initPhysical();
            return ntry;
        }
        if (nam.startsWith("pwether")) {
            ntry.type = tabRouteIface.ifaceType.pweth;
            ntry.initPhysical();
            return ntry;
        }
        if (nam.startsWith("virtualppp")) {
            ntry.type = tabRouteIface.ifaceType.virtppp;
            ntry.initPhysical();
            return ntry;
        }
        if (nam.startsWith("loopback")) {
            ntry.type = tabRouteIface.ifaceType.loopback;
            ntry.initLoopback();
            return ntry;
        }
        if (nam.startsWith("null")) {
            ntry.type = tabRouteIface.ifaceType.nul;
            ntry.initTemplate();
            return ntry;
        }
        if (nam.startsWith("template")) {
            ntry.type = tabRouteIface.ifaceType.template;
            ntry.initTemplate();
            return ntry;
        }
        if (nam.startsWith("access")) {
            ntry.type = tabRouteIface.ifaceType.dialer;
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
     * add one menu
     *
     * @param nam name of line
     * @return descriptor
     */
    public static cfgMenuK menuKdel(String nam) {
        cfgMenuK ntry = new cfgMenuK(nam);
        return menuk.del(ntry);
    }

    /**
     * find one menu
     *
     * @param nam name of this
     * @param create create if needed
     * @return descriptor, null if not found
     */
    public static cfgMenuK menuKfind(String nam, boolean create) {
        nam = nam.trim();
        if (nam.length() < 1) {
            return null;
        }
        cfgMenuK ntry = new cfgMenuK(nam);
        ntry = menuk.find(ntry);
        if (!create) {
            return ntry;
        }
        if (ntry != null) {
            return ntry;
        }
        ntry = new cfgMenuK(nam);
        menuk.add(ntry);
        return ntry;
    }

    /**
     * add one menu
     *
     * @param nam name of line
     * @return descriptor
     */
    public static cfgMenuT menuTdel(String nam) {
        cfgMenuT ntry = new cfgMenuT(nam);
        return menut.del(ntry);
    }

    /**
     * find one menu
     *
     * @param nam name of this
     * @param create create if needed
     * @return descriptor, null if not found
     */
    public static cfgMenuT menuTfind(String nam, boolean create) {
        nam = nam.trim();
        if (nam.length() < 1) {
            return null;
        }
        cfgMenuT ntry = new cfgMenuT(nam);
        ntry = menut.find(ntry);
        if (!create) {
            return ntry;
        }
        if (ntry != null) {
            return ntry;
        }
        ntry = new cfgMenuT(nam);
        menut.add(ntry);
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
        cfgVpdn ntry = new cfgVpdn(nam);
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
        cfgVpdn ntry = new cfgVpdn(nam);
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
        cfgObjnet ntry = new cfgObjnet(nam);
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
        cfgObjnet ntry = new cfgObjnet(nam);
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
        cfgObjprt ntry = new cfgObjprt(nam);
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
        cfgObjprt ntry = new cfgObjprt(nam);
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
        cfgAceslst ntry = new cfgAceslst(nam);
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
        cfgAceslst ntry = new cfgAceslst(nam);
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
        cfgTlmtry ntry = new cfgTlmtry(nam);
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
        cfgTlmtry ntry = new cfgTlmtry(nam);
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
        cfgEvntmgr ntry = new cfgEvntmgr(nam);
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
        cfgEvntmgr ntry = new cfgEvntmgr(nam);
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
        cfgPrfxlst ntry = new cfgPrfxlst(nam);
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
        cfgPrfxlst ntry = new cfgPrfxlst(nam);
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
        cfgRoump ntry = new cfgRoump(nam);
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
        cfgRoump ntry = new cfgRoump(nam);
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
        cfgRouplc ntry = new cfgRouplc(nam);
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
        cfgRouplc ntry = new cfgRouplc(nam);
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
        cfgTime ntry = new cfgTime(nam);
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
        cfgTime ntry = new cfgTime(nam);
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
        cfgPlymp ntry = new cfgPlymp(nam);
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
        cfgPlymp ntry = new cfgPlymp(nam);
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
        cfgXconn ntry = new cfgXconn(nam);
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
        cfgXconn ntry = new cfgXconn(nam);
        ntry = xconnects.del(ntry);
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
    public static cfgRtr rtrFind(tabRouteAttr.routeType typ, int num, boolean create) {
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
                case rift4:
                    if (ifc.rtrRift4hnd != null) {
                        if (ifc.rtrRift4hnd.number == ntry.number) {
                            ifc.clear2router(ifc.rtrRift4hnd);
                        }
                    }
                    break;
                case rift6:
                    if (ifc.rtrRift6hnd != null) {
                        if (ifc.rtrRift6hnd.number == ntry.number) {
                            ifc.clear2router(ifc.rtrRift6hnd);
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
        cfgIfc ifc = ifcAdd(ntry.getIntName(), tabRouteIface.ifaceType.bridge, null, 1);
        if (ifc == null) {
            bridges.del(ntry);
            return null;
        }
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
            if (ntry.compareTo(ifc.bridgeHed) != 0) {
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
        cfgIfc ifc = ifcAdd(ntry.getIntName(), tabRouteIface.ifaceType.bundle, null, 1);
        if (ifc == null) {
            bundles.del(ntry);
            return null;
        }
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
            if (ntry.compareTo(ifc.bundleHed) != 0) {
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
        cfgIfc ifc1 = ifcAdd(ntry.getIntName(true), tabRouteIface.ifaceType.hairpin, null, 1);
        if (ifc1 == null) {
            hairpins.del(ntry);
            return null;
        }
        cfgIfc ifc2 = ifcAdd(ntry.getIntName(false), tabRouteIface.ifaceType.hairpin, null, 1);
        if (ifc2 == null) {
            hairpins.del(ntry);
            return null;
        }
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
     * find one access list
     *
     * @param nam name of entry
     * @param create create new on this number if not found
     * @return descriptor, null if not found
     */
    public static cfgVnet vnetFind(String nam, boolean create) {
        nam = nam.trim();
        if (nam.length() < 1) {
            return null;
        }
        cfgVnet ntry = new cfgVnet(nam);
        if (!create) {
            return vnets.find(ntry);
        }
        cfgVnet old = vnets.add(ntry);
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
    public static cfgVnet vnetDel(String nam) {
        cfgVnet ntry = new cfgVnet(nam);
        ntry = vnets.del(ntry);
        if (ntry == null) {
            return null;
        }
        ntry.stopNow();
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
                if (skip.compareTo(ntry) == 0) {
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
                l = new userFormat("|", "interface|state|mtu|promisc|macsec|sgt|changes|for|since");
                break;
            case 28:
                l = new userFormat("|", "interface|state|input|output|drop");
                break;
            case 29:
            case 30:
            case 31:
                l = new userFormat("|", "interface|state|tx|rx|drop");
                break;
            case 32:
            case 33:
            case 34:
            case 35:
                l = new userFormat("|", "name|enabled");
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
        userFormat l = new userFormat("|", "name|type|mode|target|state|changes|took|changed");
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
     * build list of checks
     *
     * @return text to display
     */
    public static userFormat getShCheck() {
        userFormat l = new userFormat("|", "name|state|asked|reply|times|last|times|last", "4|2pass|2fail");
        for (int i = 0; i < cfgAll.checks.size(); i++) {
            cfgCheck ntry = cfgAll.checks.get(i);
            if (ntry == null) {
                continue;
            }
            l.add(ntry.getShSum());
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
        cmds.cfgLine(l, !buggy, "", "buggy", "");
        cmds.cfgLine(l, locale == null, "", "locale", locale);
        cmds.cfgLine(l, passEnc == null, "", "password-encrypt", "" + authLocal.passwdHide(passEnc, (filter & 2) != 0));
        cmds.cfgLine(l, enaPass == null, "", "enable", authLocal.secretEncode(enaPass, (filter & 2) != 0));
        l.add("banner encoded " + encBase64.encodeBytes(banner));
        l.add(cmds.comment);
        l.add("client label-range " + labelRangeBeg + " " + labelRangeEnd);
        for (int i = 0; i < alwaysDebugs.size(); i++) {
            l.add("logging debug " + alwaysDebugs.get(i));
        }
        cmds.cfgLine(l, !tracebackStops, "", "logging tracestop", "");
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
        l.add("client redundancy-keepalive " + redundancyKeep);
        l.add("client redundancy-hold " + redundancyHold);
        l.add("client redundancy-initial " + redundancyInit);
        l.add("client redundancy-takeover " + redundancyTake);
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
        cmds.cfgLine(l, !domainLookup, "", "client domain-lookup", "");
        cmds.cfgLine(l, !preferIpv6, "", "client prefer-ipv6", "");
        cmds.cfgLine(l, whoisServer == null, "", "client whois-server", whoisServer);
        cmds.cfgLine(l, whoisOption == null, "", "client whois-option", whoisOption);
        cmds.cfgLine(l, whoisFile == null, "", "client whois-file", whoisFile);
        if (whoisOnline != null) {
            String a = "";
            for (int i = 0; i < whoisOnline.size(); i++) {
                a += " " + whoisOnline.get(i);
            }
            l.add("client whois-online" + a);
        }
        cmds.cfgLine(l, whoisProxy == null, "", "client whois-proxy", "" + whoisProxy);
        cmds.cfgLine(l, !graceReload, "", "client graceful-reload", "");
        l.add("client ipv4-tos " + ipv4sendingTOS);
        l.add("client ipv4-ttl " + ipv4sendingTTL);
        l.add("client ipv6-tos " + ipv6sendingTOS);
        l.add("client ipv6-ttl " + ipv6sendingTTL);
        l.add("client ipv4-checksum " + rxtx2string(ipv4ChecksumRx, ipv4ChecksumTx));
        l.add("client icmp4-checksum " + rxtx2string(icmp4ChecksumRx, icmp4ChecksumTx));
        l.add("client icmp6-checksum " + rxtx2string(icmp6ChecksumRx, icmp6ChecksumTx));
        l.add("client udp-checksum " + rxtx2string(udpChecksumRx, udpChecksumTx));
        l.add("client udp-portrange " + udpRangeMin + " " + udpRangeMax);
        l.add("client tcp-checksum " + rxtx2string(tcpChecksumRx, tcpChecksumTx));
        l.add("client tcp-segments " + tcpSegmentMin + " " + tcpSegmentMax);
        l.add("client tcp-winscale " + tcpWinScale);
        cmds.cfgLine(l, !tcpTimStmp, "", "client tcp-timestamp", "");
        cmds.cfgLine(l, !tcpEcn, "", "client tcp-ecn", "");
        cmds.cfgLine(l, !tcpKeepalive, "", "client tcp-keepalive", "");
        cmds.cfgLine(l, capturePath == null, "", "client capture-path", capturePath);
        cmds.cfgLine(l, pasteBin == null, "", "client pastebin", pasteBin);
        l.add("client tcp-timer work " + tcpTimeWork);
        l.add("client tcp-timer alive " + tcpTimeAlive);
        l.add("client tcp-timer fin " + tcpTimeFin);
        l.add("client tcp-timer syn " + tcpTimeSyn);
        l.add("client tcp-timer open " + tcpTimeOpen);
        l.add("client tcp-timer close " + tcpTimeClose);
        l.add("client tcp-timer later " + tcpTimeLater);
        l.add("client tcp-timer now " + tcpTimeNow);
        l.add("client tcp-timer max " + tcpTimeMax);
        l.add("client tcp-portrange " + tcpRangeMin + " " + tcpRangeMax);
        l.add("client ludp-checksum " + rxtx2string(ludpChecksumRx, ludpChecksumTx));
        l.add("client ludp-portrange " + ludpRangeMin + " " + ludpRangeMax);
        l.add("client dccp-checksum " + rxtx2string(dccpChecksumRx, dccpChecksumTx));
        l.add("client dccp-portrange " + dccpRangeMin + " " + dccpRangeMax);
        l.add("client sctp-checksum " + rxtx2string(sctpChecksumRx, sctpChecksumTx));
        l.add("client sctp-portrange " + sctpRangeMin + " " + sctpRangeMax);
        l.add("client l2f-timer " + l2fTimer + " " + l2fRetry);
        l.add("client l2tp2-timer " + l2tp2hello + " " + l2tp2retry);
        l.add("client l2tp3-timer " + l2tp3hello + " " + l2tp3retry);
        String a = "";
        for (int i = 0; i < nameServerAddr.size(); i++) {
            a += " " + nameServerAddr.get(i);
        }
        l.add("client name-server" + a);
        cmds.cfgLine(l, !ftpPassive, "", "client ftp-passive", "");
        cmds.cfgLine(l, ftpProxy == null, "", "client ftp-proxy", "" + ftpProxy);
        cmds.cfgLine(l, pop3proxy == null, "", "client pop3-proxy", "" + pop3proxy);
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
        l.add("client access-supernet-ipv4 " + accessSupnet4);
        l.add("client access-supernet-ipv6 " + accessSupnet6);
        l.add("client tls-version " + tlsVerMin + " " + tlsVerMax);
        l.add("client ssh-group " + sshGrpMin + " " + sshGrpMax);
        cmds.cfgLine(l, sshAgent == null, "", "client ssh-agent", sshAgent);
        cmds.cfgLine(l, timeServerName == null, "", "client time-server", "" + timeServerName);
        l.add("client time-zone " + timeZoneName);
        cmds.cfgLine(l, timeProxy == null, "", "client time-proxy", "" + timeProxy);
        cmds.cfgLine(l, tftpProxy == null, "", "client tftp-proxy", "" + tftpProxy);
        cmds.cfgLine(l, httpAgent == null, "", "client http-agent", httpAgent);
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
        if ((endForm & 0x8) != 0) {
            a += " user";
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
        servGenList.listGetRun(l, mldsakeys, filter);
        servGenList.listGetRun(l, certs, filter);
        servGenList.listGetRun(l, timemaps, filter);
        servGenList.listGetRun(l, ipsecs, filter);
        servGenList.listGetRun(l, authers, filter);
        servGenList.listGetRun(l, chats, filter);
        servGenList.listGetRun(l, sessns, filter);
        servGenList.listGetRun(l, menuk, filter);
        servGenList.listGetRun(l, menut, filter);
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
        servGenList.listGetRun(l, vnets, filter);
        servGenList.listGetRun(l, vrfs, filter);
        for (int i = 0; i < routers.size(); i++) {
            l.addAll(routers.get(i).getShRun1(filter));
        }
        servGenList.listGetRun(l, ifaces, filter);
        for (int i = 0; i < routers.size(); i++) {
            l.addAll(routers.get(i).getShRun2(filter));
        }
        l.addAll(con0.getShRun(filter));
        servGenList.listGetRun(l, lines, filter);
        servGenList.listGetRun(l, proxys, filter);
        servGenList.listGetRun(l, vpdns, filter);
        servGenList.listGetRun(l, trackers, filter);
        servGenList.listGetRun(l, mtrackers, filter);
        for (int i = 0; i < vrfs.size(); i++) {
            l.addAll(vrfs.get(i).getShRun2(filter));
        }
        servGenList.listGetRun(l, xconnects, filter);
        servGenList.listGetRun(l, tabNshEntry.services, filter);
        servGenList.listGetRun(l, checks, filter);
        servGenList.listGetRun(l, sensors, filter);
        servGenList.listGetRun(l, tlmtrydst, filter);
        servGenList.listGetRun(l, trnsltns, filter);
        servGenList.listGetRun(l, dials, filter);
        servGenList.listGetRun(l, aliases, filter);
        dmnTelnet.getShRun(l, filter);
        dmnXotpad.getShRun(l, filter);
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
        dmnStack.getShRun(l, filter);
        dmnForwarder.getShRun(l, filter);
        dmnSyslog.getShRun(l, filter);
        dmnLoadBalancer.getShRun(l, filter);
        dmnMultiplexer.getShRun(l, filter);
        dmnSocks.getShRun(l, filter);
        dmnHttp.getShRun(l, filter);
        dmnFtp.getShRun(l, filter);
        dmnTftp.getShRun(l, filter);
        dmnGopher.getShRun(l, filter);
        dmnPlan9.getShRun(l, filter);
        dmnNtp.getShRun(l, filter);
        dmnDaytime.getShRun(l, filter);
        dmnRtpStat.getShRun(l, filter);
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
        dmnWhois.getShRun(l, filter);
        dmnPop3.getShRun(l, filter);
        dmnImap4.getShRun(l, filter);
        dmnSmtp.getShRun(l, filter);
        dmnModem.getShRun(l, filter);
        dmnVoice.getShRun(l, filter);
        dmnSip.getShRun(l, filter);
        dmnRpki.getShRun(l, filter);
        dmnNrpe.getShRun(l, filter);
        dmnPrometheus.getShRun(l, filter);
        dmnStreamingMdt.getShRun(l, filter);
        dmnMrt2bgp.getShRun(l, filter);
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
        dmnUni2uni.getShRun(l, filter);
        dmnGtp.getShRun(l, filter);
        dmnPptp.getShRun(l, filter);
        dmnRadius.getShRun(l, filter);
        dmnTacacs.getShRun(l, filter);
        l.addAll(getGlobalRunEnd(filter));
        String s = "";
        if ((endForm & 0x1) != 0) {
            s += " date=" + bits.time2str(timeZoneName, bits.getTime() + timeServerOffset, 3).replaceAll(" ", "_");
        }
        if ((endForm & 0x2) != 0) {
            s += " image=" + cfgInit.versionAgent;
        }
        if ((endForm & 0x4) != 0) {
            s += " chksum=" + userUpgrade.calcTextHash(l);
        }
        if ((endForm & 0x8) != 0) {
            s += " user=" + userLine.prevConfiger.replaceAll(" ", "_");
        }
        l.add("end" + s);
        return l;
    }

}
