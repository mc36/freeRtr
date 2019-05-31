package cfg;

import clnt.clntDns;
import ifc.ifcThread;
import ifc.ifcUdpInt;
import ip.ipFwdTab;
import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.Thread.UncaughtExceptionHandler;
import java.net.URL;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import line.lineTcpLine;
import pipe.pipeConsole;
import pipe.pipeImage;
import pipe.pipeLine;
import pipe.pipeSide;
import pipe.pipeWindow;
import prt.prtLocTcp;
import prt.prtRedun;
import prt.prtWatch;
import serv.servBstun;
import serv.servCharGen;
import serv.servDaytime;
import serv.servDcp;
import serv.servDhcp4;
import serv.servDhcp6;
import serv.servDiscard;
import serv.servDns;
import serv.servEchoP;
import serv.servEchoS;
import serv.servForwarder;
import serv.servFtp;
import serv.servGeneric;
import serv.servGopher;
import serv.servGtp;
import serv.servHoneyPot;
import serv.servHttp;
import serv.servIrc;
import serv.servIscsi;
import serv.servBmp2mrt;
import serv.servEtherIp;
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
import serv.servRpki;
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
import user.userConfig;
import user.userExec;
import user.userFilter;
import user.userHelping;
import user.userLine;
import user.userReader;
import user.userFonts1;
import util.bits;
import util.cmds;
import util.counter;
import util.debugger;
import util.history;
import util.logger;
import util.uniResLoc;
import util.version;

/**
 * hardware configuration
 *
 * @author matecsaba
 */
public class cfgInit implements Runnable {

    /**
     * sw config end
     */
    public static final String swCfgEnd = "sw.txt";

    /**
     * hw config end
     */
    public static final String hwCfgEnd = "hw.txt";

    /**
     * set until boot completes
     */
    public static boolean booting = true;

    /**
     * time when started
     */
    public static long jvmStarted = -1;

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

    private static boolean jvmSetup = false;

    private final static String needInit[] = {"interface .*",
        "policy-map .*",
        "route-map .*",
        "route-policy .*",
        "proxy-profile .*",
        "vdc definition .*",
        "vrf definition .*",
        "aaa .*"};

    private final static String jvmMagic[] = {"java.net.preferIPv4Stack=true",
        "java.net.preferIPv6Addresses=false"};

    /**
     * get mime type of an extesion
     *
     * @param s extension possibly starting with dot.
     * @return mime type
     */
    public static String findMimeType(String s) {
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
     * @param cmds commands
     * @param defs defaults
     * @param mibs mibs
     */
    public static void executeHWcommands(List<String> cmds, List<String> defs, List<String> mibs) {
        if (cmds == null) {
            return;
        }
        for (int cnt = 0; cnt < cmds.size(); cnt++) {
            String s = doTrimmer(cmds.get(cnt));
            if (s.length() < 1) {
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
            if (s.equals("tcp2vrf")) {
                int loc = bits.str2num(cmd.word());
                cfgVrf vrf = cfgAll.vrfFind(cmd.word(), true);
                int rem = bits.str2num(cmd.word());
                String adr = cmd.word();
                prtLocTcp.startServer(loc, vrf, rem, adr);
                continue;
            }
            if (s.equals("def")) {
                s = cmd.getRemaining();
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
                List<String> buf = version.shLogo(0x1fd);
                for (int i = 0; i < buf.size(); i++) {
                    lin.sendLine(buf.get(i));
                }
                lin.runner.setMon(true);
                continue;
            }
            if (s.equals("snmp")) {
                s = cmd.getRemaining();
                List<String> txt = httpGet(s);
                if (txt == null) {
                    continue;
                }
                mibs.add(s);
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
                    userFilter sn = new userFilter(a, cmd.getRemaining(),
                            new ArrayList<String>());
                    sn.listing.addAll(txt.subList(bg + 1, p));
                    snmpMibs.add(sn);
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
        userReader rd = new userReader(psC, 1023);
        userConfig uc = new userConfig(psC, rd);
        psS.lineRx = pipeSide.modTyp.modeCRorLF;
        psC.lineTx = pipeSide.modTyp.modeCRLF;
        psS.timeout = 100000;
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
            String beg = (o + 1) + ":";
            userHelping hl = uc.getHelping();
            rd.setContext(hl, "");
            String b = hl.repairLine(a);
            if (b.length() < 1) {
                logger.info(beg + a);
                continue;
            }
            try {
                uc.executeCommand(b);
            } catch (Exception e) {
                logger.info(beg + logger.dumpException(e));
            }
            if (psS.ready2rx() < 1) {
                continue;
            }
            err++;
            if (quiet) {
                continue;
            }
            logger.info(beg + a);
            for (;;) {
                if (psS.ready2rx() < 1) {
                    break;
                }
                b = psS.lineGet(1);
                if (b.length() < 1) {
                    continue;
                }
                logger.info(b);
            }
        }
        return err;
    }

    /**
     * initialize the router
     *
     * @param hw hw config
     * @param sw sw config
     */
    private static void doInit(List<String> hw, List<String> sw) {
        if (jvmStarted > 0) {
            logger.info("overlapping boot eliminated");
            return;
        }
        jvmStarted = bits.getTime();
        logger.info("booting");
        setupJVM();
        if (hw == null) {
            logger.info("no hw config found");
            hw = new ArrayList<String>();
        }
        if (sw == null) {
            logger.info("no sw config found");
            sw = new ArrayList<String>();
        }
        for (int o = 0; o < version.mimetypes.length; o++) {
            String s = version.mimetypes[o];
            int i = s.indexOf(" ");
            String c = s.substring(i + 1, s.length()).trim();
            s = s.substring(0, i).trim();
            types.add(new cfgInitMime(s, c));
        }
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
        cfgBndl.defaultF = createFilter(cfgBndl.defaultL);
        cfgBrdg.defaultF = createFilter(cfgBrdg.defaultL);
        cfgTrnsltn.defaultF = createFilter(cfgTrnsltn.defaultL);
        cfgDial.defaultF = createFilter(cfgDial.defaultL);
        cfgRoump.defaultF = createFilter(cfgRoump.defaultL);
        cfgTime.defaultF = createFilter(cfgTime.defaultL);
        cfgPlymp.defaultF = createFilter(cfgPlymp.defaultL);
        cfgRtr.defaultF = createFilter(cfgRtr.defaultL);
        cfgIfc.defaultF = createFilter(cfgIfc.defaultL);
        cfgLin.defaultF = createFilter(cfgLin.defaultL, cfgLin.linedefF);
        cfgSched.defaultF = createFilter(cfgSched.defaultL);
        cfgScrpt.defaultF = createFilter(cfgScrpt.defaultL);
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
        servCharGen.defaultF = createFilter(servCharGen.defaultL, srvdefsF);
        servOpenflow.defaultF = createFilter(servOpenflow.defaultL, srvdefsF);
        servP4lang.defaultF = createFilter(servP4lang.defaultL, srvdefsF);
        servDaytime.defaultF = createFilter(servDaytime.defaultL, srvdefsF);
        servDcp.defaultF = createFilter(servDcp.defaultL, srvdefsF);
        servIrc.defaultF = createFilter(servIrc.defaultL, srvdefsF);
        servDhcp4.defaultF = createFilter(servDhcp4.defaultL, srvdefsF);
        servDhcp6.defaultF = createFilter(servDhcp6.defaultL, srvdefsF);
        servDiscard.defaultF = createFilter(servDiscard.defaultL, srvdefsF);
        servDns.defaultF = createFilter(servDns.defaultL, srvdefsF);
        servNetflow.defaultF = createFilter(servNetflow.defaultL, srvdefsF);
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
        servLoadBalancer.defaultF = createFilter(servLoadBalancer.defaultL, srvdefsF);
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
        for (int o = 0; o < cfgAll.defaultF.size(); o++) {
            userFilter ntry = cfgAll.defaultF.get(o);
            if (ntry.section.length() > 0) {
                continue;
            }
            sdefs.add(ntry.command);
        }
        List<String> inis = new ArrayList<String>();
        for (int i = 0; i < needInit.length; i++) {
            inis.addAll(userFilter.getSecList(userFilter.text2section(sw),
                    needInit[i], cmds.tabulator + cmds.finish));
        }
        List<String> defs = new ArrayList<String>();
        List<String> mibs = new ArrayList<String>();
        try {
            logger.info("initializing hardware");
            executeHWcommands(hw, defs, mibs);
            prtRedun.doInit();
        } catch (Exception e) {
            logger.exception(e);
        }
        try {
            logger.info("applying defaults");
            executeSWcommands(sdefs, false);
            executeSWcommands(defs, false);
            executeSWcommands(inis, true);
        } catch (Exception e) {
            logger.traceback(e);
        }
        for (int i = 0; i < 4; i++) {
            logger.info("applying configuration");
            int res = 0;
            try {
                res = executeSWcommands(sw, false);
            } catch (Exception e) {
                logger.traceback(e);
            }
            if (res < 1) {
                break;
            }
            logger.error(res + " errors found");
        }
        int step = cfgAll.vdcs.size();
        if (step > 0) {
            step = (vdcPortEnd - vdcPortBeg) / step;
        } else {
            step = 1024;
        }
        for (int i = 0; i < cfgAll.vdcs.size(); i++) {
            cfgVdc ntry = cfgAll.vdcs.get(i).copyBytes();
            vdcLst.add(ntry);
            int o = (i * step) + vdcPortBeg;
            ntry.startNow(defs, mibs, o, o + step);
        }
        booting = false;
        new Thread(new cfgInit()).start();
        logger.info("done");
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
     * @param code exit code
     * @param reason reason string
     */
    public static void stopRouter(boolean clean, int code, String reason) {
        bits.buf2txt(true, bits.str2lst("code#" + code + "=" + reason), version.myReloadFile());
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
        if (clean && cfgAll.graceReload) {
            for (int i = 0; i < cfgAll.vrfs.size(); i++) {
                try {
                    cfgAll.vrfs.get(i).closeConns();
                } catch (Exception e) {
                }
            }
            prtRedun.doShut();
            prtWatch.doShut();
            bits.sleep(300);
        }
        logger.error("shutdown code=" + code + " reason=" + reason);
        logger.fileStart("");
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
        pipeImage img = new pipeImage(pl.getSide(), 80, 25,
                userFonts1.font8x16data, userFonts1.colorData);
        pipeSide pip = pl.getSide();
        logger.pipeStart(pip);
        doInit(null, httpGet(url));
        userLine lin = new userLine();
        lin.execTimeOut = 0;
        lin.createHandler(pip, "applet", true);
        img.doRound(true);
        img.doImage();
        return img;
    }

    /**
     * do main task
     *
     * @param args parameters
     */
    public static void doMain(String args[]) {
        String s = "";
        if (args.length > 0) {
            s = args[0];
        }
        if (s.startsWith("router")) {
            boolean con = false;
            boolean win = false;
            String hwN = args[1];
            String swN = null;
            for (int i = 6; i < s.length(); i++) {
                String a = "" + s.charAt(i);
                if (a.equals("s")) {
                    hwN = args[1];
                    swN = args[2];
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
                pipWin = pipeWindow.create(80, 25, userFonts1.font8x16data,
                        userFonts1.colorData);
                logger.pipeStart(pipWin);
            }
            if (swN == null) {
                swN = hwN + swCfgEnd;
                hwN += hwCfgEnd;
            }
            cfgFileHw = hwN;
            cfgFileSw = swN;
            List<String> hwT = httpGet(cfgFileHw);
            List<String> swT = httpGet(cfgFileSw);
            doInit(hwT, swT);
            if (con) {
                userLine lin = new userLine();
                lin.execTimeOut = 0;
                lin.createHandler(pipCon, "console", true);
            }
            if (win) {
                userLine lin = new userLine();
                lin.execTimeOut = 0;
                lin.createHandler(pipWin, "window", true);
            }
            return;
        }
        boolean b = s.equals("exec");
        if (b || s.equals("show") || s.equals("test")) {
            s = "";
            for (int i = b ? 1 : 0; i < args.length; i++) {
                s += " " + args[i];
            }
            pipeSide pip = pipeConsole.create();
            logger.pipeStart(pip);
            userReader rdr = new userReader(pip, 1023);
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
        hlp.add("1 2 router         start router background");
        hlp.add("2 .   <cfg>        config url");
        hlp.add("1 2 routerc        start router with console");
        hlp.add("2 .   <cfg>        config url");
        hlp.add("1 2 routerw        start router with window");
        hlp.add("2 .   <cfg>        config url");
        hlp.add("1 2 routercw       start router with console and window");
        hlp.add("2 .   <cfg>        config url");
        hlp.add("1 2 routers        start router from separate configs");
        hlp.add("2 3   <hwcfg>      config url");
        hlp.add("3 .     <swcfg>    config url");
        hlp.add("1 2 routera        start router with sw config");
        hlp.add("2 .   <swcfg>      config url");
        hlp.add("1 2 test           execute test command");
        hlp.add("2 .   <cmd>        command to execute");
        hlp.add("1 2 show           execute show command");
        hlp.add("2 .   <cmd>        command to execute");
        hlp.add("1 2 exec           execute exec command");
        hlp.add("2 .   <cmd>        command to execute");
        List<String> res = hlp.getUsage(1);
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
