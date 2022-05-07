package net.freertr.cfg;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.Thread.UncaughtExceptionHandler;
import java.net.URL;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import net.freertr.auth.authLocal;
import net.freertr.clnt.clntDns;
import net.freertr.ifc.ifcThread;
import net.freertr.ifc.ifcUdpInt;
import net.freertr.ip.ipFwdTab;
import net.freertr.line.lineTcpLine;
import net.freertr.pipe.pipeConnect;
import net.freertr.pipe.pipeConsole;
import net.freertr.pipe.pipeImage;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeReader;
import net.freertr.pipe.pipeSetting;
import net.freertr.pipe.pipeSide;
import net.freertr.pipe.pipeWindow;
import net.freertr.prt.prtLocTcp;
import net.freertr.prt.prtRedun;
import net.freertr.prt.prtWatch;
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
import net.freertr.serv.servEchoP;
import net.freertr.serv.servEchoS;
import net.freertr.serv.servEtherIp;
import net.freertr.serv.servForwarder;
import net.freertr.serv.servFtp;
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
import net.freertr.user.userConfig;
import net.freertr.user.userExec;
import net.freertr.user.userFilter;
import net.freertr.user.userFonts1;
import net.freertr.user.userHelping;
import net.freertr.user.userLine;
import net.freertr.user.userNetconf;
import net.freertr.user.userReader;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.counter;
import net.freertr.util.debugger;
import net.freertr.util.history;
import net.freertr.util.logBuf;
import net.freertr.util.logger;
import net.freertr.util.uniResLoc;
import net.freertr.util.version;

/**
 * hardware configuration
 *
 * @author matecsaba
 */
public class cfgInit implements Runnable {

    /**
     * create instance
     */
    private cfgInit() {
    }

    /**
     * sw config end
     */
    public static final String swCfgEnd = "sw.txt";

    /**
     * hw config end
     */
    public static final String hwCfgEnd = "hw.txt";

    /**
     * redundancy priority
     */
    public static int redunPrio;

    /**
     * set until boot completes
     */
    public static boolean booting = true;

    /**
     * time when started
     */
    public static long started = -1;

    /**
     * last reload code
     */
    public static int lastReloadCode;

    /**
     * read-write path name
     */
    public static String rwPath;

    /**
     * hw config file in use
     */
    public static String cfgFileHw;

    /**
     * sw config file in use
     */
    public static String cfgFileSw;

    /**
     * hardware serial number
     */
    public static String hwIdNum;

    /**
     * hardware serial number
     */
    public static String hwSnNum;

    /**
     * jvm parameters
     */
    public static String jvmParam = "";

    /**
     * timer history
     */
    public static history timerHistory;

    /**
     * memory history
     */
    public static history memoryHistory;

    /**
     * loaded snmp mibs
     */
    public static final tabGen<userFilter> snmpMibs = new tabGen<userFilter>();

    /**
     * list of physical interfaces
     */
    public static final tabGen<cfgVdcIfc> ifaceLst = new tabGen<cfgVdcIfc>();

    /**
     * list of started vdcs
     */
    public static final tabGen<cfgVdc> vdcLst = new tabGen<cfgVdc>();

    /**
     * no stall check
     */
    public static boolean noStallCheck = false;

    /**
     * vdc port range
     */
    public static int vdcPortBeg = 10240;

    /**
     * vdc port range
     */
    public static int vdcPortEnd = 32768;

    private static final tabGen<cfgInitMime> types = new tabGen<cfgInitMime>();

    private static long jvmStarted = -1;

    private static boolean jvmSetup = false;

    private final static String[] needInit = {
        "interface .*",
        "access-list .*",
        "prefix-list .*",
        "policy-map .*",
        "route-map .*",
        "route-policy .*",
        "proxy-profile .*",
        "vdc definition .*",
        "vrf definition .*",
        "aaa .*"
    };

    private final static String[] needIface = {
        "interface .*! vrf forwarding .*",
        "interface .*! ipv4 address .*",
        "interface .*! ipv6 address .*"
    };

    private final static String[] jvmMagic = {
        "java.net.preferIPv4Stack=true",
        "java.net.preferIPv6Addresses=false"
    };

    private final static int bootLogo = 0x1fd;

    /**
     * get mime type of an extesion
     *
     * @param s extension possibly starting with dot.
     * @return mime type
     */
    public static String findMimeType(String s) {
        if (s.startsWith("//")) {
            return s.substring(2, s.length());
        }
        s = s.trim().toLowerCase();
        int i = s.lastIndexOf(".");
        if (i >= 0) {
            s = s.substring(i + 1, s.length());
        }
        cfgInitMime ntry = new cfgInitMime(s);
        ntry = types.find(ntry);
        if (ntry != null) {
            return ntry.mime;
        }
        ntry = new cfgInitMime("*");
        ntry = types.find(ntry);
        if (ntry != null) {
            return ntry.mime;
        }
        return "*/*";
    }

    /**
     * get http url
     *
     * @param url url
     * @return text read
     */
    public static List<String> httpGet(String url) {
        if (url == null) {
            url = "";
        }
        if (uniResLoc.parseOne(url).proto.length() < 1) {
            return bits.txt2buf(url);
        }
        setupJVM();
        try {
            List<String> res = new ArrayList<String>();
            InputStream strm = new URL(url).openStream();
            BufferedReader rd = new BufferedReader(new InputStreamReader(strm));
            while (rd.ready()) {
                res.add(rd.readLine());
            }
            rd.close();
            strm.close();
            return res;
        } catch (Exception e) {
            return null;
        }
    }

    private static void setupJVM() {
        if (jvmSetup) {
            return;
        }
        jvmSetup = true;
        try {
            Thread.setDefaultUncaughtExceptionHandler(new cfgInitHandler());
        } catch (Exception e) {
            logger.error("error catching jvm");
        }
        for (int o = 0; o < jvmMagic.length; o++) {
            String s = jvmMagic[o];
            int i = s.indexOf("=");
            String c = s.substring(i + 1, s.length());
            s = s.substring(0, i);
            try {
                System.setProperty(s, c);
            } catch (Exception e) {
                logger.error("error setting jvm:" + s + "=" + c);
            }
        }
    }

    private static String doTrimmer(String s) {
        byte[] b = s.getBytes();
        for (int i = 0; i < s.length(); i++) {
            int o = b[i] & 0xff;
            if ((o <= 31) || (o >= 127)) {
                o = 95;
            }
            b[i] = (byte) o;
        }
        return new String(b).trim();
    }

    /**
     * execute hw commands
     *
     * @param read commands
     * @param defs defaults
     * @param inhs inheritables
     * @param cfgs configs
     */
    public static void executeHWcommands(List<String> read, List<String> defs, List<String> inhs, List<String> cfgs) {
        if (read == null) {
            return;
        }
        for (int cnt = 0; cnt < read.size(); cnt++) {
            String s = doTrimmer(read.get(cnt));
            if (s.length() < 1) {
                continue;
            }
            if (s.startsWith(cmds.comment)) {
                continue;
            }
            if (debugger.cfgInitHw) {
                logger.debug("cmd " + s);
            }
            cmds cmd = new cmds("hw", s);
            s = cmd.word().toLowerCase();
            if (s.equals("hwid")) {
                hwIdNum = cmd.getRemaining();
                continue;
            }
            if (s.equals("hwsn")) {
                hwSnNum = cmd.getRemaining();
                continue;
            }
            if (s.equals("jvm")) {
                jvmParam = cmd.getRemaining();
                continue;
            }
            if (s.equals("debug")) {
                debugger.setByName(cmd, true);
                continue;
            }
            if (s.equals("url")) {
                cfgAll.upgradeServer = cmd.getRemaining();
                continue;
            }
            if (s.equals("key")) {
                cfgAll.upgradePubKey = cmd.getRemaining();
                continue;
            }
            if (s.equals("enc")) {
                cfgAll.passEnh = authLocal.passwdDecode(cmd.word());
                continue;
            }
            if (s.equals("hidevrf")) {
                cfgVrf vrf = cfgAll.vrfFind(cmd.word(), true);
                if (vrf == null) {
                    continue;
                }
                vrf.hidden = true;
                continue;
            }
            if (s.equals("hideifc")) {
                cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
                if (ifc == null) {
                    continue;
                }
                ifc.hidden = true;
                continue;
            }
            if (s.equals("tcp2vrf")) {
                int loc = bits.str2num(cmd.word());
                cfgVrf vrf = cfgAll.vrfFind(cmd.word(), true);
                int rem = bits.str2num(cmd.word());
                String adr = cmd.word();
                prtLocTcp.startServer(loc, vrf, rem, adr);
                continue;
            }
            if (s.equals("prio")) {
                redunPrio = bits.str2num(cmd.word());
                continue;
            }
            if (s.equals("def")) {
                s = cmd.getRemaining();
                defs.add(s);
                cfgAll.defaultF.add(new userFilter("", s, null));
                continue;
            }
            if (s.equals("cfg")) {
                s = cmd.getRemaining();
                cfgs.add(s);
                continue;
            }
            if (s.equals("dcfg")) {
                s = cmd.getRemaining();
                cfgs.add(s);
                defs.add(s);
                cfgAll.defaultF.add(new userFilter("", s, null));
                continue;
            }
            if (s.equals("port")) {
                vdcPortBeg = bits.str2num(cmd.word());
                vdcPortEnd = bits.str2num(cmd.word());
                continue;
            }
            if (s.equals("nostall")) {
                noStallCheck = true;
                continue;
            }
            if (s.equals("rwpath")) {
                rwPath = cmd.getRemaining();
                if (!rwPath.endsWith("/")) {
                    rwPath += "/";
                }
                continue;
            }
            if (s.equals("prcpar")) {
                cfgPrcss prc = new cfgPrcss(cmd.word());
                prc = cfgAll.prcs.find(prc);
                if (prc == null) {
                    continue;
                }
                for (;;) {
                    s = cmd.word();
                    if (s.length() < 1) {
                        break;
                    }
                    boolean neg = s.startsWith("no");
                    if (neg) {
                        s = s.substring(2, s.length());
                    }
                    if (s.equals("hid")) {
                        prc.hidden = !neg;
                        continue;
                    }
                    if (s.equals("act")) {
                        prc.logAct = !neg;
                        continue;
                    }
                    if (s.equals("con")) {
                        prc.logCon = !neg;
                        continue;
                    }
                    if (s.equals("col")) {
                        if (neg) {
                            prc.logCol = null;
                            continue;
                        }
                        prc.logCol = new logBuf(bits.str2num(cmd.word()));
                        continue;
                    }
                }
                continue;
            }
            if (s.equals("proc")) {
                cfgPrcss prc = new cfgPrcss(cmd.word());
                prc.hidden = true;
                prc.logAct = true;
                prc.execName = cmd.getRemaining();
                cfgPrcss old = cfgAll.prcs.put(prc);
                prc.startNow();
                if (old == null) {
                    continue;
                }
                old.stopNow();
                continue;
            }
            if (s.equals("int")) {
                String old = cmd.getRemaining();
                String nam = cmd.word();
                s = cmd.word().toLowerCase();
                int stat = 0;
                if (s.equals("stat")) {
                    stat = 1;
                }
                if (s.equals("red")) {
                    stat = 2;
                }
                if (s.equals("dog")) {
                    stat = 3;
                }
                if (stat != 0) {
                    s = cmd.word().toLowerCase();
                }
                cfgIfc.ifaceType typ = cfgIfc.string2type(s);
                if (typ == null) {
                    continue;
                }
                String mac = cmd.word();
                String loop = cmd.word();
                int loc = bits.str2num(cmd.word());
                String peer = cmd.word();
                int rem = bits.str2num(cmd.word());
                int thrd = bits.str2num(cmd.word());
                ifcUdpInt hdr = new ifcUdpInt(loop, loc, peer, rem, mac,
                        typ != cfgIfc.ifaceType.ether, stat == 1);
                switch (stat) {
                    case 2:
                        hdr.booter = true;
                        prtRedun.ifcAdd(nam, hdr);
                        break;
                    case 3:
                        hdr.booter = true;
                        prtWatch.ifcAdd(nam, hdr, mac);
                        break;
                    default:
                        ifaceLst.add(new cfgVdcIfc(cfgIfc.normName(nam, false), old));
                        cfgIfc ifc = cfgAll.ifcAdd(nam, typ, hdr, thrd);
                        ifc.initPhysical();
                        if (debugger.cfgInitHw) {
                            logger.debug("iface " + hdr);
                        }
                        break;
                }
                continue;
            }
            if (s.equals("line")) {
                String nam = cmd.word();
                boolean nomon = nam.equals("nomon");
                if (nomon) {
                    nam = cmd.word();
                }
                String loop = cmd.word();
                int loc = bits.str2num(cmd.word());
                String peer = cmd.word();
                int rem = bits.str2num(cmd.word());
                lineTcpLine hdr = new lineTcpLine(loop, loc, peer, rem);
                cfgLin lin = cfgAll.linAdd(nam, hdr);
                if (debugger.cfgInitHw) {
                    logger.debug("line " + hdr);
                }
                if (nomon) {
                    continue;
                }
                List<String> logo = version.shLogo(bootLogo);
                for (int i = 0; i < logo.size(); i++) {
                    lin.sendLine(logo.get(i));
                }
                lin.runner.setMon(true);
                continue;
            }
            if (s.equals("netconf")) {
                s = cmd.getRemaining();
                List<String> txt = httpGet(s);
                if (txt == null) {
                    continue;
                }
                inhs.add(cmd.getOriginal());
                int bg = -1;
                int md = 0;
                for (int p = 0; p < txt.size(); p++) {
                    String a = txt.get(p);
                    if (a.startsWith("sensor ")) {
                        bg = p;
                        md = 1;
                        continue;
                    }
                    if (a.startsWith("config ")) {
                        bg = p;
                        md = 2;
                        continue;
                    }
                    if (!a.equals(".")) {
                        continue;
                    }
                    cmd = new cmds("", txt.get(bg));
                    cmd.word();
                    switch (md) {
                        case 1:
                            cfgSensor tl = new cfgSensor(cmd.getRemaining());
                            tl.hidden = true;
                            for (int i = bg + 1; i < p; i++) {
                                cmd = new cmds("", txt.get(i));
                                tl.doCfgStr(cmd);
                            }
                            cfgAll.sensors.put(tl);
                            if (debugger.cfgInitHw) {
                                logger.debug("netconf sensor " + tl.name);
                            }
                            break;
                        case 2:
                            a = cmd.word();
                            userNetconf.makeYang(txt, bg + 1, p);
                            if (debugger.cfgInitHw) {
                                logger.debug("netconf config " + a);
                            }
                            break;
                        default:
                            break;
                    }
                    md = 0;
                    continue;
                }
                continue;
            }
            if (s.equals("snmp")) {
                s = cmd.getRemaining();
                List<String> txt = httpGet(s);
                if (txt == null) {
                    continue;
                }
                inhs.add(cmd.getOriginal());
                int bg = -1;
                for (int p = 0; p < txt.size(); p++) {
                    String a = txt.get(p);
                    if (a.startsWith("oid ")) {
                        bg = p;
                    }
                    if (!a.equals(".")) {
                        continue;
                    }
                    cmd = new cmds("", txt.get(bg));
                    cmd.word();
                    a = cmd.word();
                    userFilter sn = new userFilter(a, cmd.getRemaining(), new ArrayList<String>());
                    sn.listing.addAll(txt.subList(bg + 1, p));
                    snmpMibs.put(sn);
                    if (debugger.cfgInitHw) {
                        logger.debug("snmp " + sn);
                    }
                }
                continue;
            }
            logger.info((cnt + 1) + ":" + cmd.getOriginal());
        }
    }

    /**
     * execute sw commands
     *
     * @param cs commands
     * @param quiet do not log errors
     * @return number of errors
     */
    public static int executeSWcommands(List<String> cs, boolean quiet) {
        if (cs == null) {
            return 0;
        }
        if (debugger.cfgInitSw) {
            logger.debug("applying sw config");
        }
        int err = 0;
        pipeLine pl = new pipeLine(65536, false);
        pipeSide psS = pl.getSide();
        pipeSide psC = pl.getSide();
        userReader rd = new userReader(psC, null);
        psC.settingsPut(pipeSetting.height, 0);
        userConfig uc = new userConfig(psC, rd);
        psS.lineRx = pipeSide.modTyp.modeCRorLF;
        psC.lineTx = pipeSide.modTyp.modeCRLF;
        psS.setTime(100000);
        for (int o = 0; o < cs.size(); o++) {
            String a = cs.get(o);
            int i = a.indexOf(cmds.comment);
            if (i >= 0) {
                a = a.substring(0, i);
            }
            a = doTrimmer(a);
            if (a.length() < 1) {
                continue;
            }
            if (debugger.cfgInitSw) {
                logger.debug("cmd " + a);
            }
            String beg = "line " + (o + 1) + ": \"" + a + "\" : ";
            userHelping hl = uc.getHelping(false, true, true);
            rd.setContext(hl, "");
            String b = hl.repairLine(a);
            if (b.length() < 1) {
                err++;
                if (quiet) {
                    continue;
                }
                logger.info(beg + "no such command");
                continue;
            }
            try {
                uc.executeCommand(b);
            } catch (Exception e) {
                err++;
                logger.info(beg + logger.dumpException(e));
            }
            i = psS.ready2rx();
            if (i < 1) {
                continue;
            }
            err++;
            byte[] buf = new byte[i];
            i = psS.nonBlockGet(buf, 0, buf.length);
            b = new String(buf, 0, i);
            b = b.replaceAll("\r", " ");
            b = b.replaceAll("\n", " ");
            if (quiet) {
                continue;
            }
            logger.info(beg + b);
        }
        return err;
    }

    private static void doInit(List<String> hw, List<String> sw, pipeSide cons) {
        if (jvmStarted > 0) {
            logger.info("overlapping boot eliminated");
            return;
        }
        jvmStarted = bits.getTime();
        started = bits.getTime();
        logger.info("booting");
        setupJVM();
        String s = bits.lst2str(bits.txt2buf(version.myReloadFile()), " ");
        int i = s.indexOf("#");
        if (i > 0) {
            s = s.substring(i + 1, s.length());
        }
        i = s.indexOf("=");
        if (i > 0) {
            s = s.substring(0, i);
        }
        lastReloadCode = bits.str2num(s);
        if (hw == null) {
            logger.info("no hw config found");
            hw = new ArrayList<String>();
        }
        if (sw == null) {
            logger.info("no sw config found");
            sw = new ArrayList<String>();
        }
        for (int o = 0; o < version.mimetypes.length; o++) {
            s = version.mimetypes[o];
            i = s.indexOf(" ");
            String a = s.substring(i + 1, s.length()).trim();
            s = s.substring(0, i).trim();
            types.add(new cfgInitMime(s, a));
        }
        cfgIfc.ifaceNames = new userHelping();
        cfgIfc.ifaceNames.add(null, "1 . loopback      ifc");
        cfgIfc.ifaceNames.add(null, "1 . null          ifc");
        cfgIfc.ifaceNames.add(null, "1 . template      ifc");
        cfgIfc.ifaceNames.add(null, "1 . dialer        ifc");
        cfgIfc.ifaceNames.add(null, "1 . sdn           ifc");
        cfgIfc.ifaceNames.add(null, "1 . pwether       ifc");
        cfgIfc.ifaceNames.add(null, "1 . virtualppp    ifc");
        cfgIfc.ifaceNames.add(null, "1 . access        ifc");
        cfgIfc.ifaceNames.add(null, "1 . bvi           ifc");
        cfgIfc.ifaceNames.add(null, "1 . bundle        ifc");
        cfgIfc.ifaceNames.add(null, "1 . tunnel        ifc");
        cfgIfc.ifaceNames.add(null, "1 . hairpin       ifc");
        cfgIfc.ifaceNames.add(null, "1 . atm           ifc");
        cfgIfc.ifaceNames.add(null, "1 . arcnet        ifc");
        cfgIfc.ifaceNames.add(null, "1 . infiniband    ifc");
        cfgIfc.ifaceNames.add(null, "1 . ethernet      ifc");
        cfgIfc.ifaceNames.add(null, "1 . serial        ifc");
        cfgIfc.ifaceNames.add(null, "1 . cellular      ifc");
        cfgIfc.ifaceNames.add(null, "1 . wireless      ifc");
        cfgIfc.notemplF = createFilter(cfgIfc.notemplL);
        cfgIfc.nocloneF = createFilter(cfgIfc.nocloneL);
        cfgLin.linedefF = createFilter(cfgLin.linedefL);
        cfgMenu.defaultF = createFilter(cfgMenu.defaultL);
        cfgAll.defaultF = createFilter(cfgAll.defaultL);
        cfgIpsec.defaultF = createFilter(cfgIpsec.defaultL);
        cfgAuther.defaultF = createFilter(cfgAuther.defaultL);
        cfgVdc.defaultF = createFilter(cfgVdc.defaultL);
        cfgPrcss.defaultF = createFilter(cfgPrcss.defaultL);
        cfgVrf.defaultF = createFilter(cfgVrf.defaultL);
        cfgAlias.defaultF = createFilter(cfgAlias.defaultL);
        cfgCert.defaultF = createFilter(cfgCert.defaultL);
        cfgChat.defaultF = createFilter(cfgChat.defaultL);
        cfgHrpn.defaultF = createFilter(cfgHrpn.defaultL);
        cfgKey.defaultF = createFilter(cfgKey.defaultL);
        cfgAceslst.defaultF = createFilter(cfgAceslst.defaultL);
        cfgObjnet.defaultF = createFilter(cfgObjnet.defaultL);
        cfgObjprt.defaultF = createFilter(cfgObjprt.defaultL);
        cfgPrfxlst.defaultF = createFilter(cfgPrfxlst.defaultL);
        cfgBndl.defaultF = createFilter(cfgBndl.defaultL);
        cfgBrdg.defaultF = createFilter(cfgBrdg.defaultL);
        cfgTrnsltn.defaultF = createFilter(cfgTrnsltn.defaultL);
        cfgDial.defaultF = createFilter(cfgDial.defaultL);
        cfgSessn.defaultF = createFilter(cfgSessn.defaultL);
        cfgCheck.defaultF = createFilter(cfgCheck.defaultL);
        cfgSensor.defaultF = createFilter(cfgSensor.defaultL);
        cfgRoump.defaultF = createFilter(cfgRoump.defaultL);
        cfgRouplc.defaultF = createFilter(cfgRouplc.defaultL);
        cfgTime.defaultF = createFilter(cfgTime.defaultL);
        cfgPlymp.defaultF = createFilter(cfgPlymp.defaultL);
        cfgRtr.defaultF = createFilter(cfgRtr.defaultL);
        cfgIfc.defaultF = createFilter(cfgIfc.defaultL);
        cfgLin.defaultF = createFilter(cfgLin.defaultL, cfgLin.linedefF);
        cfgSched.defaultF = createFilter(cfgSched.defaultL);
        cfgScrpt.defaultF = createFilter(cfgScrpt.defaultL);
        cfgTlmtry.defaultF = createFilter(cfgTlmtry.defaultL);
        cfgEvntmgr.defaultF = createFilter(cfgEvntmgr.defaultL);
        cfgTrack.defaultF = createFilter(cfgTrack.defaultL);
        cfgMtrack.defaultF = createFilter(cfgMtrack.defaultL);
        cfgProxy.defaultF = createFilter(cfgProxy.defaultL);
        cfgVpdn.defaultF = createFilter(cfgVpdn.defaultL);
        cfgIconn.defaultF = createFilter(cfgIconn.defaultL);
        cfgXconn.defaultF = createFilter(cfgXconn.defaultL);
        tabGen<userFilter> srvdefsF = createFilter(servGeneric.srvdefsL);
        servBstun.defaultF = createFilter(servBstun.defaultL, srvdefsF, cfgLin.linedefF);
        servRpki.defaultF = createFilter(servRpki.defaultL, srvdefsF);
        servNrpe.defaultF = createFilter(servNrpe.defaultL, srvdefsF);
        servPrometheus.defaultF = createFilter(servPrometheus.defaultL, srvdefsF);
        servStreamingMdt.defaultF = createFilter(servStreamingMdt.defaultL, srvdefsF);
        servCharGen.defaultF = createFilter(servCharGen.defaultL, srvdefsF);
        servOpenflow.defaultF = createFilter(servOpenflow.defaultL, srvdefsF);
        servPktmux.defaultF = createFilter(servPktmux.defaultL, srvdefsF);
        servP4lang.defaultF = createFilter(servP4lang.defaultL, srvdefsF);
        servDaytime.defaultF = createFilter(servDaytime.defaultL, srvdefsF);
        servDcp.defaultF = createFilter(servDcp.defaultL, srvdefsF);
        servSdwan.defaultF = createFilter(servSdwan.defaultL, srvdefsF);
        servPcep.defaultF = createFilter(servPcep.defaultL, srvdefsF);
        servIrc.defaultF = createFilter(servIrc.defaultL, srvdefsF);
        servDhcp4.defaultF = createFilter(servDhcp4.defaultL, srvdefsF);
        servDhcp6.defaultF = createFilter(servDhcp6.defaultL, srvdefsF);
        servDiscard.defaultF = createFilter(servDiscard.defaultL, srvdefsF);
        servDns.defaultF = createFilter(servDns.defaultL, srvdefsF);
        servNetflow.defaultF = createFilter(servNetflow.defaultL, srvdefsF);
        servUdpFwd.defaultF = createFilter(servUdpFwd.defaultL, srvdefsF);
        servUpnpFwd.defaultF = createFilter(servUpnpFwd.defaultL, srvdefsF);
        servUpnpHub.defaultF = createFilter(servUpnpHub.defaultL, srvdefsF);
        servEchoP.defaultF = createFilter(servEchoP.defaultL, srvdefsF);
        servEchoS.defaultF = createFilter(servEchoS.defaultL, srvdefsF);
        servForwarder.defaultF = createFilter(servForwarder.defaultL, srvdefsF);
        servFtp.defaultF = createFilter(servFtp.defaultL, srvdefsF);
        servGopher.defaultF = createFilter(servGopher.defaultL, srvdefsF);
        servGtp.defaultF = createFilter(servGtp.defaultL, srvdefsF);
        servHoneyPot.defaultF = createFilter(servHoneyPot.defaultL, srvdefsF);
        servHttp.defaultF = createFilter(servHttp.defaultL, srvdefsF);
        servIscsi.defaultF = createFilter(servIscsi.defaultL, srvdefsF);
        servBmp2mrt.defaultF = createFilter(servBmp2mrt.defaultL, srvdefsF);
        servVxlan.defaultF = createFilter(servVxlan.defaultL, srvdefsF);
        servGeneve.defaultF = createFilter(servGeneve.defaultL, srvdefsF);
        servL2f.defaultF = createFilter(servL2f.defaultL, srvdefsF);
        servL2tp2.defaultF = createFilter(servL2tp2.defaultL, srvdefsF);
        servL2tp3.defaultF = createFilter(servL2tp3.defaultL, srvdefsF);
        servEtherIp.defaultF = createFilter(servEtherIp.defaultL, srvdefsF);
        servGre.defaultF = createFilter(servGre.defaultL, srvdefsF);
        servMplsIp.defaultF = createFilter(servMplsIp.defaultL, srvdefsF);
        servMplsUdp.defaultF = createFilter(servMplsUdp.defaultL, srvdefsF);
        servMplsOam.defaultF = createFilter(servMplsOam.defaultL, srvdefsF);
        servTwamp.defaultF = createFilter(servTwamp.defaultL, srvdefsF);
        servAmt.defaultF = createFilter(servAmt.defaultL, srvdefsF);
        servUni2multi.defaultF = createFilter(servUni2multi.defaultL, srvdefsF);
        servLoadBalancer.defaultF = createFilter(servLoadBalancer.defaultL, srvdefsF);
        servMultiplexer.defaultF = createFilter(servMultiplexer.defaultL, srvdefsF);
        servLpd.defaultF = createFilter(servLpd.defaultL, srvdefsF);
        servNtp.defaultF = createFilter(servNtp.defaultL, srvdefsF);
        servPckOdtls.defaultF = createFilter(servPckOdtls.defaultL, srvdefsF);
        servPckOtcp.defaultF = createFilter(servPckOtcp.defaultL, srvdefsF);
        servPckOtxt.defaultF = createFilter(servPckOtxt.defaultL, srvdefsF);
        servPckOudp.defaultF = createFilter(servPckOudp.defaultL, srvdefsF);
        servPop3.defaultF = createFilter(servPop3.defaultL, srvdefsF);
        servPptp.defaultF = createFilter(servPptp.defaultL, srvdefsF);
        servQuote.defaultF = createFilter(servQuote.defaultL, srvdefsF);
        servRadius.defaultF = createFilter(servRadius.defaultL, srvdefsF);
        servRfb.defaultF = createFilter(servRfb.defaultL, srvdefsF, cfgLin.linedefF);
        servModem.defaultF = createFilter(servModem.defaultL, srvdefsF, cfgLin.linedefF);
        servVoice.defaultF = createFilter(servVoice.defaultL, srvdefsF, cfgLin.linedefF);
        servSip.defaultF = createFilter(servSip.defaultL, srvdefsF);
        servSmtp.defaultF = createFilter(servSmtp.defaultL, srvdefsF);
        servSnmp.defaultF = createFilter(servSnmp.defaultL, srvdefsF);
        servSocks.defaultF = createFilter(servSocks.defaultL, srvdefsF);
        servStun.defaultF = createFilter(servStun.defaultL, srvdefsF);
        servSyslog.defaultF = createFilter(servSyslog.defaultL, srvdefsF);
        servTacacs.defaultF = createFilter(servTacacs.defaultL, srvdefsF);
        servTelnet.defaultF = createFilter(servTelnet.defaultL, srvdefsF, cfgLin.linedefF);
        servTftp.defaultF = createFilter(servTftp.defaultL, srvdefsF);
        servTime.defaultF = createFilter(servTime.defaultL, srvdefsF);
        servUdptn.defaultF = createFilter(servUdptn.defaultL, srvdefsF, cfgLin.linedefF);
        List<String> sdefs = new ArrayList<String>();
        for (i = 0; i < cfgAll.defaultF.size(); i++) {
            userFilter ntry = cfgAll.defaultF.get(i);
            if (ntry.section.length() > 0) {
                continue;
            }
            sdefs.add(ntry.command);
        }
        List<String> inis = new ArrayList<String>();
        List<userFilter> secs = userFilter.text2section(sw);
        for (i = 0; i < needInit.length; i++) {
            inis.addAll(userFilter.getSecList(secs, needInit[i], cmds.tabulator + cmds.finish));
        }
        List<String> ints = userFilter.section2text(userFilter.filter2text(secs, createFilter(needIface)), true);
        List<String> hcfgs = new ArrayList<String>();
        List<String> hdefs = new ArrayList<String>();
        List<String> inhs = new ArrayList<String>();
        logger.info("initializing hardware");
        try {
            executeHWcommands(hw, hdefs, inhs, hcfgs);
        } catch (Exception e) {
            logger.exception(e);
        }
        logger.info("applying defaults");
        try {
            executeSWcommands(sdefs, false);
        } catch (Exception e) {
            logger.traceback(e);
        }
        try {
            executeSWcommands(hdefs, false);
        } catch (Exception e) {
            logger.traceback(e);
        }
        try {
            executeSWcommands(inis, true);
        } catch (Exception e) {
            logger.traceback(e);
        }
        try {
            executeSWcommands(ints, true);
        } catch (Exception e) {
            logger.traceback(e);
        }
        logger.info("applying configuration");
        int res = 0;
        try {
            res = executeSWcommands(sw, false);
        } catch (Exception e) {
            logger.traceback(e);
        }
        if (res > 0) {
            logger.error(res + " errors found");
        }
        try {
            executeSWcommands(hcfgs, true);
        } catch (Exception e) {
            logger.traceback(e);
        }
        int step = cfgAll.vdcs.size();
        if (step > 0) {
            step = (vdcPortEnd - vdcPortBeg) / step;
        } else {
            step = 1024;
        }
        for (i = 0; i < cfgAll.vdcs.size(); i++) {
            cfgVdc ntry = cfgAll.vdcs.get(i).copyBytes();
            vdcLst.add(ntry);
            int o = (i * step) + vdcPortBeg;
            ntry.startNow(hdefs, inhs, o, o + step);
        }
        try {
            prtRedun.doInit(cons);
        } catch (Exception e) {
            logger.exception(e);
        }
        started = bits.getTime();
        booting = false;
        new Thread(new cfgInit()).start();
        logger.info("boot completed");
    }

    private static tabGen<userFilter> createFilter(String[] lst) {
        tabGen<userFilter> res = new tabGen<userFilter>();
        for (int o = 0; o < lst.length; o++) {
            String s = lst[o];
            int i = s.indexOf("!");
            String c = s.substring(i + 1, s.length());
            s = s.substring(0, i);
            res.add(new userFilter(s, c, null));
        }
        return res;
    }

    private static void addFilters(tabGen<userFilter> trg,
            tabGen<userFilter> src) {
        for (int i = 0; i < src.size(); i++) {
            userFilter ntry = src.get(i);
            trg.add(ntry);
        }
    }

    private static tabGen<userFilter> createFilter(String[] lst,
            tabGen<userFilter> s1) {
        tabGen<userFilter> r = createFilter(lst);
        addFilters(r, s1);
        return r;
    }

    private static tabGen<userFilter> createFilter(String[] lst,
            tabGen<userFilter> s1, tabGen<userFilter> s2) {
        tabGen<userFilter> r = createFilter(lst);
        addFilters(r, s1);
        addFilters(r, s2);
        return r;
    }

    /**
     * stop router
     *
     * @param clean clean exit
     * @param code exit code, negative just updates reload file
     * @param reason reason string
     */
    public static void stopRouter(boolean clean, int code, String reason) {
        boolean fake = code < 0;
        if (fake) {
            code = -code;
        }
        try {
            bits.buf2txt(true, bits.str2lst("code#" + code + "=" + reason), version.myReloadFile());
        } catch (Exception e) {
        }
        if (fake) {
            lastReloadCode = code;
            return;
        }
        if (clean && cfgAll.graceReload) {
            for (int i = 0; i < cfgAll.vrfs.size(); i++) {
                try {
                    cfgAll.vrfs.get(i).closeAllConns();
                } catch (Exception e) {
                }
            }
            prtRedun.doShut();
            prtWatch.doShut();
            bits.sleep(100);
        }
        for (int i = 0; i < vdcLst.size(); i++) {
            try {
                vdcLst.get(i).stopNow();
            } catch (Exception e) {
            }
        }
        for (int i = 0; i < cfgAll.prcs.size(); i++) {
            try {
                cfgAll.prcs.get(i).stopNow();
            } catch (Exception e) {
            }
        }
        logger.error("shutdown code=" + code + " reason=" + reason);
        logger.fileName(null);
        System.exit(code);
    }

    /**
     * start applet
     *
     * @param url config url
     * @return image
     */
    public static pipeImage doApplet(String url) {
        pipeLine pl = new pipeLine(65536, false);
        pipeImage img = new pipeImage(pl.getSide(), 80, 25, userFonts1.fontDefault(), userFonts1.colorData);
        pipeSide ps = pl.getSide();
        ps.lineTx = pipeSide.modTyp.modeCRLF;
        ps.lineRx = pipeSide.modTyp.modeCRorLF;
        ps.setTime(0);
        logger.pipeStart(ps);
        List<String> logo = version.shLogo(bootLogo);
        for (int i = 0; i < logo.size(); i++) {
            ps.linePut(logo.get(i));
        }
        doInit(null, httpGet(url), null);
        userLine lin = new userLine();
        lin.execTimeOut = 0;
        lin.createHandler(ps, "applet", 2);
        img.doRound(true);
        img.doImage();
        return img;
    }

    /**
     * do main task
     *
     * @param args parameters
     */
    public static void doMain(String[] args) {
        String s = "";
        if (args.length > 0) {
            s = args[0];
        }
        if (s.startsWith("router")) {
            boolean con = false;
            boolean win = false;
            boolean add = false;
            String hwN = args[1];
            String swN = null;
            for (int i = 6; i < s.length(); i++) {
                String a = "" + s.charAt(i);
                if (a.equals("s")) {
                    hwN = args[1];
                    swN = args[2];
                    add = true;
                }
                if (a.equals("a")) {
                    hwN = null;
                    swN = args[1];
                }
                if (a.equals("c")) {
                    con = true;
                }
                if (a.equals("w")) {
                    win = true;
                }
            }
            pipeSide pipCon = null;
            pipeSide pipWin = null;
            if (con) {
                pipCon = pipeConsole.create();
                logger.pipeStart(pipCon);
            }
            if (win) {
                pipWin = pipeWindow.createOne(80, 25, userFonts1.fontDefault(), userFonts1.colorData);
                logger.pipeStart(pipWin);
            }
            if (swN == null) {
                swN = hwN + swCfgEnd;
                hwN += hwCfgEnd;
            }
            List<String> logo = version.shLogo(bootLogo);
            for (int i = 0; i < logo.size(); i++) {
                if (pipCon != null) {
                    pipCon.linePut(logo.get(i));
                }
                if (pipWin != null) {
                    pipWin.linePut(logo.get(i));
                }
            }
            cfgFileHw = hwN;
            cfgFileSw = swN;
            List<String> hwT = httpGet(cfgFileHw);
            if (add) {
                for (int i = 3; i < args.length; i++) {
                    List<String> lst = httpGet(args[i]);
                    hwT.addAll(lst);
                }
            }
            List<String> swT = httpGet(cfgFileSw);
            doInit(hwT, swT, pipCon);
            if (pipCon != null) {
                userLine lin = new userLine();
                lin.execTimeOut = 0;
                lin.createHandler(pipCon, "console", 2);
            }
            if (pipWin != null) {
                userLine lin = new userLine();
                lin.execTimeOut = 0;
                lin.createHandler(pipWin, "window", 2);
            }
            return;
        }
        if (s.equals("show")) {
            s = "";
            for (int i = 0; i < args.length; i++) {
                s += " " + args[i];
            }
            pipeLine pl = new pipeLine(1024 * 1024, false);
            pipeSide pip = pl.getSide();
            pip.lineTx = pipeSide.modTyp.modeCRLF;
            pip.lineRx = pipeSide.modTyp.modeCRorLF;
            userReader rdr = new userReader(pip, null);
            pip.settingsPut(pipeSetting.height, 0);
            userExec exe = new userExec(pip, rdr);
            exe.privileged = true;
            s = exe.repairCommand(s);
            try {
                exe.executeCommand(s);
            } catch (Exception e) {
                logger.exception(e);
            }
            pip = pl.getSide();
            pl.setClose();
            pipeReader rd = new pipeReader();
            rd.setLineMode(pipeSide.modTyp.modeCRtryLF);
            pipeConnect.connect(pip, rd.getPipe(), true);
            rd.waitFor();
            List<String> res = rd.getResult();
            for (int i = 0; i < res.size(); i++) {
                putln(res.get(i));
            }
            return;
        }
        boolean b = s.equals("exec");
        if (b || s.equals("test")) {
            s = "";
            for (int i = b ? 1 : 0; i < args.length; i++) {
                s += " " + args[i];
            }
            pipeSide pip = pipeConsole.create();
            logger.pipeStart(pip);
            userReader rdr = new userReader(pip, null);
            pip.settingsPut(pipeSetting.height, 0);
            userExec exe = new userExec(pip, rdr);
            exe.privileged = true;
            s = exe.repairCommand(s);
            try {
                exe.executeCommand(s);
            } catch (Exception e) {
                logger.exception(e);
            }
            stopRouter(true, 1, "finished");
            return;
        }
        putln("java -jar " + version.getFileName() + " <parameters>");
        putln("parameters:");
        userHelping hlp = new userHelping();
        hlp.add(null, "1 2 router         start router background");
        hlp.add(null, "2 .   <cfg>        config url");
        hlp.add(null, "1 2 routerc        start router with console");
        hlp.add(null, "2 .   <cfg>        config url");
        hlp.add(null, "1 2 routerw        start router with window");
        hlp.add(null, "2 .   <cfg>        config url");
        hlp.add(null, "1 2 routercw       start router with console and window");
        hlp.add(null, "2 .   <cfg>        config url");
        hlp.add(null, "1 2 routers        start router from separate configs");
        hlp.add(null, "2 3   <hwcfg>      config url");
        hlp.add(null, "3 4,.   <swcfg>    config url");
        hlp.add(null, "4 4,.     [hwcfg]  config url");
        hlp.add(null, "1 2 routera        start router with sw config only");
        hlp.add(null, "2 .   <swcfg>      config url");
        hlp.add(null, "1 2 test           execute test command");
        hlp.add(null, "2 .   <cmd>        command to execute");
        hlp.add(null, "1 2 show           execute show command");
        hlp.add(null, "2 .   <cmd>        command to execute");
        hlp.add(null, "1 2 exec           execute exec command");
        hlp.add(null, "2 .   <cmd>        command to execute");
        List<String> res = hlp.getUsage();
        for (int i = 0; i < res.size(); i++) {
            putln(res.get(i));
        }
    }

    private static void putln(String s) {
        System.out.println(s);
    }

    public void run() {
        int rnd = 0;
        counter cntr = new counter();
        cntr.byteRx = bits.getTime() / 8;
        timerHistory = new history(cntr);
        Runtime rt = Runtime.getRuntime();
        long oldM = rt.freeMemory() / 8;
        cntr.byteRx = oldM;
        memoryHistory = new history(cntr);
        for (;;) {
            try {
                rnd += 1;
                bits.sleep(1000);
                if (debugger.prtWatchEvnt) {
                    logger.debug("health check");
                }
                ifcThread.checkIfaces();
                ipFwdTab.checkVrfs();
                cntr.byteRx = bits.getTime() / 8;
                timerHistory.update(cntr);
                oldM += rt.freeMemory() / 8;
                cntr.byteRx = oldM;
                memoryHistory.update(cntr);
                if ((rnd % 120) == 0) {
                    clntDns.purgeLocalCache(false);
                }
            } catch (Exception e) {
                logger.exception(e);
            }
        }
    }

}

class cfgInitMime implements Comparator<cfgInitMime> {

    protected final String ext;

    protected final String mime;

    public cfgInitMime(String e) {
        ext = e;
        mime = "*";
    }

    public cfgInitMime(String e, String m) {
        ext = e;
        mime = m;
    }

    public int compare(cfgInitMime o1, cfgInitMime o2) {
        return o1.ext.toLowerCase().compareTo(o2.ext.toLowerCase());
    }

}

class cfgInitHandler implements UncaughtExceptionHandler {

    public void uncaughtException(Thread t, Throwable e) {
        logger.exception(e);
    }

}
