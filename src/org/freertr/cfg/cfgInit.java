package org.freertr.cfg;

import java.io.BufferedReader;
import java.io.File;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.Thread.UncaughtExceptionHandler;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import org.freertr.auth.authLocal;
import org.freertr.clnt.clntDns;
import org.freertr.ifc.ifcThread;
import org.freertr.ifc.ifcUdpInt;
import org.freertr.ip.ipFwdTab;
import org.freertr.line.lineTcpLine;
import org.freertr.pipe.pipeConnect;
import org.freertr.pipe.pipeConsole;
import org.freertr.pipe.pipeImage;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeReader;
import org.freertr.pipe.pipeSetting;
import org.freertr.pipe.pipeSide;
import org.freertr.pipe.pipeWindow;
import org.freertr.prt.prtLocTcp;
import org.freertr.prt.prtRedun;
import org.freertr.prt.prtWatch;
import org.freertr.serv.servOpenflow;
import org.freertr.serv.servP4lang;
import org.freertr.enc.encUrl;
import org.freertr.ip.ipRtr;
import org.freertr.pipe.pipeShell;
import org.freertr.serv.servStack;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabRouteAttr;
import org.freertr.tab.tabRouteIface;
import org.freertr.user.userConfig;
import org.freertr.user.userExec;
import org.freertr.user.userFilter;
import org.freertr.user.userFlash;
import org.freertr.user.userFonts;
import org.freertr.user.userHelp;
import org.freertr.user.userHwdet;
import org.freertr.user.userNetconf;
import org.freertr.user.userRead;
import org.freertr.user.userScreen;
import org.freertr.user.userUpgrade;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.history;
import org.freertr.util.logBuf;
import org.freertr.util.logger;
import org.freertr.util.version;

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
    public final static String swCfgEnd = "sw.txt";

    /**
     * hw config end
     */
    public final static String hwCfgEnd = "hw.txt";

    /**
     * 9.1.1
     */
    public final static String versionNumber = version.year + "." + version.month + "." + version.day;

    /**
     * ros v9.1.1-rel
     */
    public final static String versionName = version.name + " v" + versionNumber + version.state;

    /**
     * ros v9.1.1-rel, done by me.
     */
    public final static String versionFull = versionName + ", done by " + version.author + ".";

    /**
     * ros/9.1.1-rel
     */
    public final static String versionAgent = version.name + "/" + versionNumber + version.state;

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
     * state save file in use
     */
    public static String stateFile;

    /**
     * hardware serial number
     */
    public static String hwIdNum;

    /**
     * hardware serial number
     */
    public static String hwSnNum;

    /**
     * hostname of parent
     */
    public static String prntNam;

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
     * list of physical interfaces
     */
    public final static tabGen<cfgVdcIfc> ifaceLst = new tabGen<cfgVdcIfc>();

    /**
     * list of started vdcs
     */
    public final static tabGen<cfgVdc> vdcLst = new tabGen<cfgVdc>();

    /**
     * list of started vnets
     */
    public final static tabGen<cfgVnet> vnetLst = new tabGen<cfgVnet>();

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

    /**
     * interface names
     */
    public static userHelp ifaceNames = new userHelp();

    private static List<String> stateLast = new ArrayList<String>();

    private final static tabGen<cfgInitMime> types = new tabGen<cfgInitMime>();

    private static long jvmStarted = -1;

    private static boolean jvmSetup = false;

    private final static String[] needInit = {
        "interface .*",
        "aaa .*",
        "vrf definition .*",
        "access-list .*",
        "prefix-list .*",
        "policy-map .*",
        "route-map .*",
        "route-policy .*",
        "proxy-profile .*",
        "vdc definition .*",
        "server dhcp4 .*",
        "server dhcp6 .*",};

    private final static String[] needFull = {
        "vnet .*",};

    private final static userFilter[] needIface = {
        new userFilter("interface .*", cmds.tabulator + "vrf forwarding .*", null),
        new userFilter("interface .*", cmds.tabulator + "ipv4 address .*", null),
        new userFilter("interface .*", cmds.tabulator + "ipv6 address .*", null)
    };

    private final static String[] jvmMagic = {
        "java.net.preferIPv4Stack=true",
        "java.net.preferIPv6Addresses=false"
    };

    private final static int bootLogo = 0x1fd;

    /**
     * find in secret list
     *
     * @param a string to find
     * @return found, null if nothing
     */
    public static List<String> secretsFind(String a) {
        for (int i = 0; i < version.secrets.length; i++) {
            if (!a.equals(version.secrets[i][0])) {
                continue;
            }
            ArrayList<String> l = new ArrayList<String>();
            bits.array2list(l, version.secrets[i]);
            l.remove(0);
            return l;
        }
        return null;
    }

    /**
     * get java executable
     *
     * @return path of jvms
     */
    public static String getJvmExec() {
        return getSysProp("java.home") + "/bin/java";
    }

    /**
     * get archive date
     *
     * @return date of jar
     */
    public static long getFileDate() {
        return new File(getFileName()).lastModified();
    }

    /**
     * get archive name
     *
     * @return pathname jar filename
     */
    public static String getFileName() {
        return getSysProp("java.class.path");
    }

    /**
     * get archive path name
     *
     * @return filename without extension
     */
    public static String myPathName() {
        String s = getFileName();
        int i = s.lastIndexOf(".");
        int o = s.lastIndexOf("/");
        if (o < 0) {
            o = 0;
        }
        if (i < o) {
            return "rtr";
        }
        return s.substring(0, i);
    }

    /**
     * get read-write path name
     *
     * @return path
     */
    public static String getRWpath() {
        String a = rwPath;
        if (a == null) {
            a = cfgFileSw;
        }
        if (a == null) {
            a = cfgFileHw;
        }
        if (a == null) {
            a = "./";
        }
        int i = a.lastIndexOf("/");
        if (i < 0) {
            a = "./";
        } else {
            a = a.substring(0, i + 1);
        }
        return a;
    }

    /**
     * get state file name
     *
     * @return filename without path
     */
    public static String myStateFile() {
        if (stateFile != null) {
            return stateFile;
        }
        return getRWpath() + "state.txt";
    }

    /**
     * get reload file name
     *
     * @return filename without path
     */
    public static String myReloadFile() {
        return getRWpath() + "reload.log";
    }

    /**
     * get errors file name
     *
     * @return filename without path
     */
    public static String myErrorFile() {
        return getRWpath() + "errors.log";
    }

    /**
     * get memory info
     *
     * @return memory
     */
    public static String getMemoryInfo() {
        Runtime rt = Runtime.getRuntime();
        return bits.toUser(rt.totalMemory()) + "/" + bits.toUser(rt.maxMemory());
    }

    /**
     * get kernel name
     *
     * @return name of kernel
     */
    public static String getKernelName() {
        return getSysProp("os.name").trim() + " v" + getSysProp("os.version").trim();
    }

    /**
     * get vm name
     *
     * @return name of vm
     */
    public static String getVMname() {
        return getJavaVer("java.vm").trim();
    }

    /**
     * get cpu name
     *
     * @return name of cpu
     */
    public static String getCPUname() {
        return (Runtime.getRuntime().availableProcessors() + "*" + getSysProp("os.arch")).trim();
    }

    /**
     * get hardware forwarder
     *
     * @return offload info
     */
    public static String getHWfwd1liner() {
        servStack stk = cfgAll.dmnStack.get(0);
        if (stk != null) {
            return prtRedun.getShGenOneLiner() + stk.getShGenOneLiner();
        }
        servP4lang p4l = cfgAll.dmnP4lang.get(0);
        if (p4l != null) {
            return prtRedun.getShGenOneLiner() + p4l.getShGenOneLiner();
        }
        servOpenflow ovs = cfgAll.dmnOpenflow.get(0);
        if (ovs != null) {
            return prtRedun.getShGenOneLiner() + ovs.getShGenOneLiner();
        }
        return prtRedun.getShGenOneLiner() + "swonly";
    }

    /**
     * get show platform text
     *
     * @return list
     */
    public static List<String> getShPlat() {
        List<String> sa = new ArrayList<String>();
        sa.add(versionFull);
        sa.add("");
        Runtime rt = Runtime.getRuntime();
        sa.add("name: " + cfgAll.hostName + ", prnt: " + prntNam + ", hwid: " + hwIdNum + " hwsn: " + hwSnNum);
        sa.add("hwfw: " + getHWfwd1liner());
        sa.add("uptime: since " + bits.time2str(cfgAll.timeZoneName, started + cfgAll.timeServerOffset, 3) + ", for " + bits.timePast(started));
        sa.add("pid: " + pipeShell.myProcessNum() + ", reload: " + bits.lst2str(bits.txt2buf(myReloadFile()), " "));
        sa.add("hwc: " + cfgFileHw + ", swc: " + cfgFileSw);
        sa.add("class: v" + getSysProp("java.class.version") + " @ " + getFileName() + ", rwp: " + getRWpath());
        sa.add("cpu: " + getCPUname() + ", mem: free=" + bits.toUser(rt.freeMemory()) + ", max=" + bits.toUser(rt.maxMemory()) + ", used=" + bits.toUser(rt.totalMemory()));
        long l = pipeShell.getKernelUptime();
        sa.add("host: " + getKernelName() + ", since " + bits.time2str(cfgAll.timeZoneName, l + cfgAll.timeServerOffset, 3) + ", for " + bits.timePast(l));
        sa.add("java: " + getJavaVer("java") + " @ " + getSysProp("java.home"));
        sa.add("jspec: " + getJavaVer("java.specification"));
        sa.add("vm: " + getVMname());
        sa.add("vmspec: " + getJavaVer("java.vm.specification"));
        return sa;
    }

    /**
     * get show logo text
     *
     * @param head needed extra lines
     * @return list
     */
    public static List<String> getShLogo(int head) {
        List<String> sa = new ArrayList<String>();
        if ((head & 1) != 0) {
            sa.add("");
        }
        if ((head & 2) != 0) {
            sa.add(versionFull);
        }
        if ((head & 4) != 0) {
            sa.add("");
        }
        if ((head & 8) != 0) {
            bits.array2list(sa, version.logo);
        }
        if ((head & 16) != 0) {
            sa.add("");
        }
        if ((head & 32) != 0) {
            sa.add(versionFull);
        }
        if ((head & 64) != 0) {
            sa.add("");
        }
        if ((head & 128) != 0) {
            bits.array2list(sa, version.license);
        }
        if ((head & 256) != 0) {
            sa.add("");
        }
        if ((head & 512) != 0) {
            sa.add(versionNumber);
        }
        if ((head & 1024) != 0) {
            sa.add(bits.time2str(cfgAll.timeZoneName, getFileDate(), 3));
        }
        if ((head & 2048) != 0) {
            sa.add(bits.time2str(cfgAll.timeZoneName, getFileDate(), 4));
        }
        if ((head & 4096) != 0) {
            sa.add(versionAgent);
        }
        if ((head & 8192) != 0) {
            sa.add(version.homeUrl);
        }
        if ((head & 16384) != 0) {
            bits.array2list(sa, version.quotes);
        }
        return sa;
    }

    /**
     * get mime type of an extesion
     *
     * @param s extension possibly starting with dot.
     * @return mime type
     */
    public final static String findMimeType(String s) {
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
     * name of backup configuration
     *
     * @return null if disabled, file name
     */
    public final static String getBackupCfgName() {
        if (cfgAll.configBackup == null) {
            return null;
        }
        String a = cfgAll.configBackup;
        if (a.length() > 0) {
            return a;
        }
        a = cfgFileSw;
        int i = a.lastIndexOf(".");
        if (i > 0) {
            a = a.substring(0, i);
        }
        a = a + userUpgrade.bakExt;
        return a;
    }

    /**
     * get http url
     *
     * @param url url
     * @return text read
     */
    public final static List<String> httpGet(String url) {
        if (url == null) {
            url = "";
        }
        if (encUrl.parseOne(url).proto.length() < 1) {
            return bits.txt2buf(url);
        }
        setupJVM();
        try {
            List<String> res = new ArrayList<String>();
            InputStream strm = new URI(url).toURL().openStream();
            BufferedReader rd = new BufferedReader(new InputStreamReader(strm));
            for (;;) {
                String a = rd.readLine();
                if (a == null) {
                    break;
                }
                res.add(a);
            }
            rd.close();
            strm.close();
            return res;
        } catch (Exception e) {
            return null;
        }
    }

    private static String getJavaVer(String s) {
        String vnd = getSysProp(s + ".vendor");
        String nam = getSysProp(s + ".name");
        String ver = getSysProp(s + ".version");
        if (nam != null) {
            nam = " (" + nam + ")";
        } else {
            nam = "";
        }
        return vnd + nam + " v" + ver;
    }

    private static String getSysProp(String s) {
        try {
            return System.getProperty(s);
        } catch (Exception e) {
            return "?";
        }
    }

    private final static void setupJVM() {
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
        types.add(new cfgInitMime("html", "text/html"));
        types.add(new cfgInitMime("htm", "text/html"));
        types.add(new cfgInitMime("css", "text/css"));
        types.add(new cfgInitMime("rtf", "text/richtext"));
        types.add(new cfgInitMime("text", "text/plain"));
        types.add(new cfgInitMime("txt", "text/plain"));
        types.add(new cfgInitMime("csv", "text/csv"));
        types.add(new cfgInitMime("md", "text/markdown"));
        types.add(new cfgInitMime("*", "text/plain"));
        types.add(new cfgInitMime("webp", "image/webp"));
        types.add(new cfgInitMime("gif", "image/gif"));
        types.add(new cfgInitMime("jpeg", "image/jpeg"));
        types.add(new cfgInitMime("jpg", "image/jpeg"));
        types.add(new cfgInitMime("tiff", "image/tiff"));
        types.add(new cfgInitMime("tif", "image/tiff"));
        types.add(new cfgInitMime("bmp", "image/bmp"));
        types.add(new cfgInitMime("png", "image/png"));
        types.add(new cfgInitMime("svg", "image/svg+xml"));
        types.add(new cfgInitMime("ico", "image/x-icon"));
        types.add(new cfgInitMime("pbm", "image/x-portable-bitmap"));
        types.add(new cfgInitMime("pgm", "image/x-portable-graymap"));
        types.add(new cfgInitMime("pnm", "image/x-portable-anymap"));
        types.add(new cfgInitMime("ppm", "image/x-portable-pixmap"));
        types.add(new cfgInitMime("xbm", "image/x-xbitmap"));
        types.add(new cfgInitMime("xpm", "image/x-xpixmap"));
        types.add(new cfgInitMime("webm", "video/webm"));
        types.add(new cfgInitMime("mjpeg", "video/x-motion-jpeg"));
        types.add(new cfgInitMime("avi", "video/msvideo"));
        types.add(new cfgInitMime("mov", "video/quicktime"));
        types.add(new cfgInitMime("qt", "video/quicktime"));
        types.add(new cfgInitMime("mpeg", "video/mpeg"));
        types.add(new cfgInitMime("mpg", "video/mpeg"));
        types.add(new cfgInitMime("mp4", "video/mp4"));
        types.add(new cfgInitMime("mkv", "video/x-matroska"));
        types.add(new cfgInitMime("3gp", "video/3gpp"));
        types.add(new cfgInitMime("3g2", "video/3gpp2"));
        types.add(new cfgInitMime("ogv", "video/ogg"));
        types.add(new cfgInitMime("weba", "audio/weba"));
        types.add(new cfgInitMime("aif", "audio/x-aiff"));
        types.add(new cfgInitMime("aiff", "audio/x-aiff"));
        types.add(new cfgInitMime("wav", "audio/wav"));
        types.add(new cfgInitMime("midi", "audio/midi"));
        types.add(new cfgInitMime("mid", "audio/midi"));
        types.add(new cfgInitMime("rmi", "audio/midi"));
        types.add(new cfgInitMime("ram", "audio/x-pn-realaudio"));
        types.add(new cfgInitMime("rpm", "audio/x-pn-realaudio-plugin"));
        types.add(new cfgInitMime("ra", "audio/x-realaudio"));
        types.add(new cfgInitMime("rm", "audio/x-pn-realaudio"));
        types.add(new cfgInitMime("mp3", "audio/mpeg"));
        types.add(new cfgInitMime("oga", "audio/ogg"));
        types.add(new cfgInitMime("flac", "audio/flac"));
        types.add(new cfgInitMime("aac", "audio/aac"));
        types.add(new cfgInitMime("bin", "application/octet-stream"));
        types.add(new cfgInitMime("jar", "application/java-archive"));
        types.add(new cfgInitMime("doc", "application/msword"));
        types.add(new cfgInitMime("docx", "application/msword"));
        types.add(new cfgInitMime("dvi", "application/x-dvi"));
        types.add(new cfgInitMime("eps", "application/postscript"));
        types.add(new cfgInitMime("ps", "application/postscript"));
        types.add(new cfgInitMime("gz", "application/x-gzip"));
        types.add(new cfgInitMime("bz2", "application/x-bzip2"));
        types.add(new cfgInitMime("js", "application/javascript"));
        types.add(new cfgInitMime("latex", "application/x-latex"));
        types.add(new cfgInitMime("lzh", "application/x-lzh"));
        types.add(new cfgInitMime("pdf", "application/pdf"));
        types.add(new cfgInitMime("epub", "application/epub+zip"));
        types.add(new cfgInitMime("swf", "application/x-shockwave-flash"));
        types.add(new cfgInitMime("tar", "application/tar"));
        types.add(new cfgInitMime("tcl", "application/x-tcl"));
        types.add(new cfgInitMime("tex", "application/x-tex"));
        types.add(new cfgInitMime("tgz", "application/x-gzip"));
        types.add(new cfgInitMime("zip", "application/zip"));
        types.add(new cfgInitMime("xml", "application/xml"));
        types.add(new cfgInitMime("ogg", "application/ogg"));
        types.add(new cfgInitMime("wml", "text/vnd.wap.wml"));
        types.add(new cfgInitMime("wbmp", "image/vnd.wap.wbmp"));
        ifaceNames.add(null, false, 1, new int[]{-1}, "loopback", "ifc");
        ifaceNames.add(null, false, 1, new int[]{-1}, "null", "ifc");
        ifaceNames.add(null, false, 1, new int[]{-1}, "template", "ifc");
        ifaceNames.add(null, false, 1, new int[]{-1}, "dialer", "ifc");
        ifaceNames.add(null, false, 1, new int[]{-1}, "sdn", "ifc");
        ifaceNames.add(null, false, 1, new int[]{-1}, "pwether", "ifc");
        ifaceNames.add(null, false, 1, new int[]{-1}, "virtualppp", "ifc");
        ifaceNames.add(null, false, 1, new int[]{-1}, "access", "ifc");
        ifaceNames.add(null, false, 1, new int[]{-1}, "bvi", "ifc");
        ifaceNames.add(null, false, 1, new int[]{-1}, "bundle", "ifc");
        ifaceNames.add(null, false, 1, new int[]{-1}, "tunnel", "ifc");
        ifaceNames.add(null, false, 1, new int[]{-1}, "hairpin", "ifc");
        ifaceNames.add(null, false, 1, new int[]{-1}, "atm", "ifc");
        ifaceNames.add(null, false, 1, new int[]{-1}, "arcnet", "ifc");
        ifaceNames.add(null, false, 1, new int[]{-1}, "infiniband", "ifc");
        ifaceNames.add(null, false, 1, new int[]{-1}, "ethernet", "ifc");
        ifaceNames.add(null, false, 1, new int[]{-1}, "serial", "ifc");
        ifaceNames.add(null, false, 1, new int[]{-1}, "cellular", "ifc");
        ifaceNames.add(null, false, 1, new int[]{-1}, "wireless", "ifc");
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
    public final static void executeHWcommands(List<String> read, List<String> defs, List<String> inhs, List<String> cfgs) {
        if (read == null) {
            return;
        }
        List<userFilter> hdefs = new ArrayList<userFilter>();
        List<userFilter> mibs = new ArrayList<userFilter>();
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
            if (s.equals("limited")) {
                cfgAll.invdc = true;
                continue;
            }
            if (s.equals("save")) {
                stateFile = cmd.getRemaining();
                continue;
            }
            if (s.equals("hwid")) {
                hwIdNum = cmd.getRemaining();
                continue;
            }
            if (s.equals("hwsn")) {
                hwSnNum = cmd.getRemaining();
                continue;
            }
            if (s.equals("prnt")) {
                prntNam = cmd.getRemaining();
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
                cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
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
                String bind = cmd.word();
                String fake = cmd.word();
                prtLocTcp.startServer(loc, vrf, rem, bind, fake);
                continue;
            }
            if (s.equals("prio")) {
                redunPrio = bits.str2num(cmd.word());
                continue;
            }
            if (s.equals("def")) {
                s = cmd.getRemaining();
                defs.add(s);
                hdefs.add(new userFilter("", s, null));
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
                hdefs.add(new userFilter("", s, null));
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
                    boolean neg = s.startsWith(cmds.negated);
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
            if (s.equals("vnet")) {
                cfgVnet prc = new cfgVnet(cmd.word());
                prc.hidden = true;
                prc.side1.logAct = true;
                prc.side2.logAct = true;
                prc.side1.ifcTyp = userHwdet.string2type(cmd.word());
                prc.side2.ifcTyp = prc.side1.ifcTyp;
                s = cmd.word().trim();
                if (s.length() > 0) {
                    String pnm[] = cfgIfc.dissectName(s);
                    if (pnm == null) {
                        continue;
                    }
                    prc.side1.locNam = pnm[0] + pnm[1] + pnm[2];
                }
                s = cmd.word().trim();
                if (s.length() > 0) {
                    prc.side2.conNam = s;
                }
                cfgAll.vnets.put(prc);
                continue;
            }
            if (s.equals("int")) {
                String old = cmd.getRemaining();
                String nam = cmd.word();
                String pnm[] = cfgIfc.dissectName(nam);
                if (pnm == null) {
                    continue;
                }
                if (pnm[3].length() > 0) {
                    continue;
                }
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
                tabRouteIface.ifaceType typ = cfgIfc.string2type(s);
                if (typ == null) {
                    continue;
                }
                String mac = cmd.word();
                String loop = cmd.word();
                int loc = bits.str2num(cmd.word());
                String peer = cmd.word();
                int rem = bits.str2num(cmd.word());
                s = cmd.word();
                int thrd = bits.str2num(s);
                ifcUdpInt hdr = new ifcUdpInt(loop, loc, peer, rem, mac,
                        typ != tabRouteIface.ifaceType.ether, stat == 1);
                switch (stat) {
                    case 2:
                        hdr.booter = true;
                        prtRedun.ifcAdd(nam, hdr, s);
                        break;
                    case 3:
                        hdr.booter = true;
                        prtWatch.ifcAdd(nam, hdr, mac);
                        break;
                    default:
                        cfgIfc ifc = cfgAll.ifcAdd(nam, typ, hdr, thrd);
                        if (ifc == null) {
                            continue;
                        }
                        cfgVdcIfc ntry = new cfgVdcIfc(ifc.name, old);
                        ntry.portL = loc;
                        ntry.portR = rem;
                        ifaceLst.add(ntry);
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
                List<String> logo = getShLogo(bootLogo);
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
                    mibs.add(sn);
                    if (debugger.cfgInitHw) {
                        logger.debug("snmp " + sn);
                    }
                }
                continue;
            }
            logger.info((cnt + 1) + ":" + cmd.getOriginal());
        }
        cfgAll.custDefs = add2filter(cfgAll.custDefs, hdefs);
        cfgAll.snmpMibs = add2filter(cfgAll.snmpMibs, hdefs);
    }

    private static userFilter[] add2filter(userFilter[] trg, List<userFilter> src) {
        if (src.size() < 1) {
            return trg;
        }
        for (int i = 0; i < trg.length; i++) {
            src.add(trg[i]);
        }
        userFilter[] res = new userFilter[src.size()];
        for (int i = 0; i < res.length; i++) {
            res[i] = src.get(i);
        }
        return res;
    }

    /**
     * execute sw commands
     *
     * @param cs commands
     * @param quiet do not log errors
     * @return number of errors
     */
    public final static int executeSWcommands(List<String> cs, boolean quiet) {
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
        userRead rd = new userRead(psC, null);
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
            userHelp hl = uc.getHelping(false, true, true);
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
                logger.info(beg + logger.dumpException(e, " at line " + err));
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

    private final static void doInit(List<String> hw, List<String> sw, pipeSide cons) {
        if (jvmStarted > 0) {
            logger.info("overlapping boot eliminated");
            return;
        }
        jvmStarted = bits.getTime();
        started = bits.getTime();
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
        List<String> sdefs = new ArrayList<String>();
        for (int i = 0; i < cfgAll.defaultF.length; i++) {
            userFilter ntry = cfgAll.defaultF[i];
            if (ntry.section.length() > 0) {
                continue;
            }
            sdefs.add(ntry.command);
        }
        List<String> inis = new ArrayList<String>();
        List<userFilter> secs = userFilter.text2section(sw);
        for (int i = 0; i < needInit.length; i++) {
            inis.addAll(userFilter.getSecList(secs, needInit[i], cmds.tabulator + cmds.finish));
        }
        for (int i = 0; i < needFull.length; i++) {
            inis.addAll(userFilter.section2text(userFilter.getSection(secs, needFull[i], true, false, false), true));
        }
        List<String> ints = userFilter.section2text(userFilter.filter2text(secs, needIface), true);
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
        for (int i = 0; i < cfgAll.vnets.size(); i++) {
            cfgVnet ntry = cfgAll.vnets.get(i).copyBytes();
            ntry.startNow(vdcPortBeg + (i * 4));
            vnetLst.add(ntry);
        }
        vdcPortBeg += (vnetLst.size() * 4);
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
        int p = cfgAll.vdcs.size();
        if (p > 0) {
            p = (vdcPortEnd - vdcPortBeg) / p;
        } else {
            p = 1024;
        }
        for (int i = 0; i < cfgAll.vdcs.size(); i++) {
            cfgVdc ntry = cfgAll.vdcs.get(i).copyBytes();
            vdcLst.add(ntry);
            int o = (i * p) + vdcPortBeg;
            ntry.startNow(hdefs, inhs, o, o + p);
        }
        cfgAll.con0.line.execTimeOut = 0;
        try {
            prtRedun.doInit(cons);
        } catch (Exception e) {
            logger.exception(e);
        }
        stateLoad();
        started = bits.getTime();
        booting = false;
        new Thread(new cfgInit()).start();
        logger.info("boot completed");
    }

    private final static void stateLoad() {
        List<String> txt = bits.txt2buf(myStateFile());
        if (txt == null) {
            return;
        }
        userFlash.delete(myStateFile());
        int o = 0;
        for (int i = 0; i < txt.size(); i++) {
            cmds cmd = new cmds("rst", txt.get(i));
            tabRouteAttr.routeType t = cfgRtr.name2num(cmd.word());
            if (t == null) {
                continue;
            }
            cfgRtr c = cfgAll.rtrFind(t, bits.str2num(cmd.word()), false);
            if (c == null) {
                continue;
            }
            ipRtr r = c.getRouter();
            if (r == null) {
                continue;
            }
            boolean b = true;
            try {
                b = r.routerStateSet(cmd);
            } catch (Exception e) {
                logger.traceback(e);
            }
            if (b) {
                continue;
            }
            o++;
        }
        logger.info("restored " + o + " of " + txt.size());
    }

    /**
     * generate state data
     */
    public final static List<String> stateData() {
        List<String> res = new ArrayList<String>();
        for (int i = 0; i < cfgAll.routers.size(); i++) {
            cfgRtr c = cfgAll.routers.get(i);
            if (c == null) {
                continue;
            }
            ipRtr e = c.getRouter();
            if (e == null) {
                continue;
            }
            e.routerStateGet(res);
        }
        return res;
    }

    /**
     * save state
     */
    public final static void stateSave() {
        List<String> res = stateData();
        boolean e = res.size() == stateLast.size();
        if (e) {
            for (int i = 0; i < res.size(); i++) {
                e = res.get(i).equals(stateLast.get(i));
                if (!e) {
                    break;
                }
            }
        }
        if (e) {
            return;
        }
        stateLast = res;
        bits.buf2txt(true, res, myStateFile());
        prtRedun.doState();
    }

    /**
     * stop router
     *
     * @param clean clean exit
     * @param code exit code, negative just updates reload file, 22 already used
     * @param reason reason string
     */
    public final static void stopRouter(boolean clean, int code, String reason) {
        boolean fake = code < 0;
        if (fake) {
            code = -code;
        }
        try {
            bits.buf2txt(true, bits.str2lst("code#" + code + "=" + reason), myReloadFile());
        } catch (Exception e) {
        }
        if (fake) {
            return;
        }
        try {
            debugger.setAll(false);
        } catch (Exception e) {
        }
        if (clean && cfgAll.graceReload) {
            for (int i = 0; i < cfgAll.vrfs.size(); i++) {
                try {
                    cfgAll.vrfs.get(i).closeAllConns(true);
                } catch (Exception e) {
                }
            }
            prtRedun.doShut();
            prtWatch.doShut();
            bits.sleep(100);
        }
        for (int i = 0; i < vnetLst.size(); i++) {
            try {
                vnetLst.get(i).stopNow();
            } catch (Exception e) {
            }
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
    public final static pipeImage doApplet(String url) {
        pipeLine pl = new pipeLine(65536, false);
        pipeImage img = new pipeImage(pl.getSide(), 80, 25, userFonts.font8x16(), userFonts.colorData);
        pipeSide ps = pl.getSide();
        ps.lineTx = pipeSide.modTyp.modeCRLF;
        ps.lineRx = pipeSide.modTyp.modeCRorLF;
        ps.setTime(0);
        logger.pipeStart(ps);
        List<String> logo = getShLogo(bootLogo);
        for (int i = 0; i < logo.size(); i++) {
            ps.linePut(logo.get(i));
        }
        doInit(null, httpGet(url), null);
        cfgAll.con0.line.createHandler(ps, "applet", 2);
        img.scr.doRound(true);
        img.doImage();
        return img;
    }

    /**
     * do main task
     *
     * @param args parameters
     */
    public final static void doMain(String[] args) {
        String s = "";
        if (args.length > 0) {
            s = args[0];
        }
        if (s.startsWith("router")) {
            boolean det = false;
            boolean con = false;
            boolean win = false;
            String hwN = args[1];
            String swN = null;
            for (int i = 6; i < s.length(); i++) {
                String a = "" + s.charAt(i);
                if (a.equals("s")) {
                    hwN = args[1];
                    swN = args[2];
                    continue;
                }
                if (a.equals("a")) {
                    hwN = null;
                    swN = args[1];
                    continue;
                }
                if (a.equals("c")) {
                    con = true;
                    continue;
                }
                if (a.equals("w")) {
                    win = true;
                    continue;
                }
                if (a.equals("d")) {
                    det = true;
                    continue;
                }
            }
            pipeSide pipCon = null;
            pipeSide pipWin = null;
            if (con) {
                pipCon = pipeConsole.create();
                logger.pipeStart(pipCon);
            }
            if (win) {
                pipWin = pipeWindow.createOne(80, 25, userFonts.font8x16(), userFonts.colorData);
                logger.pipeStart(pipWin);
            }
            if (swN == null) {
                swN = hwN + swCfgEnd;
                hwN += hwCfgEnd;
            }
            List<String> logo = getShLogo(bootLogo);
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
            List<String> swT = httpGet(cfgFileSw);
            doInit(hwT, swT, pipCon);
            if (pipCon != null) {
                if (det) {
                    userScreen.updtSiz(pipCon);
                }
                cfgAll.con0.line.createHandler(pipCon, "console", 2);
            }
            if (pipWin != null) {
                cfgAll.con0.line.createHandler(pipWin, "window", 2);
            }
            return;
        }
        setupJVM();
        if (s.startsWith("cfgexec")) {
            boolean det = false;
            for (int i = 7; i < s.length(); i++) {
                String a = "" + s.charAt(i);
                if (a.equals("d")) {
                    det = true;
                    continue;
                }
            }
            cfgFileSw = args[1];
            s = "";
            for (int i = 2; i < args.length; i++) {
                s += " " + args[i];
            }
            pipeSide pip = pipeConsole.create();
            logger.pipeStart(pip);
            List<String> logo = getShLogo(bootLogo);
            for (int i = 0; i < logo.size(); i++) {
                pip.linePut(logo.get(i));
            }
            List<String> swT = httpGet(cfgFileSw);
            doInit(null, swT, pip);
            logger.pipeStart(pip);
            userRead rdr = new userRead(pip, null);
            pip.settingsPut(pipeSetting.height, 0);
            if (det) {
                userScreen.updtSiz(pip);
            }
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
        if (s.equals("show")) {
            s = "";
            for (int i = 0; i < args.length; i++) {
                s += " " + args[i];
            }
            pipeLine pl = new pipeLine(1024 * 1024, false);
            pipeSide pip = pl.getSide();
            pip.lineTx = pipeSide.modTyp.modeCRLF;
            pip.lineRx = pipeSide.modTyp.modeCRorLF;
            userRead rdr = new userRead(pip, null);
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
        boolean b = s.startsWith("exec");
        if (b || s.startsWith("test")) {
            boolean det = false;
            for (int i = 4; i < s.length(); i++) {
                String a = "" + s.charAt(i);
                if (a.equals("d")) {
                    det = true;
                    continue;
                }
            }
            s = "";
            int i = b ? 1 : 0;
            for (; i < args.length; i++) {
                s += " " + args[i];
            }
            pipeSide pip = pipeConsole.create();
            logger.pipeStart(pip);
            userRead rdr = new userRead(pip, null);
            pip.settingsPut(pipeSetting.height, 0);
            if (det) {
                userScreen.updtSiz(pip);
            }
            userExec exe = new userExec(pip, rdr);
            exe.privileged = true;
            s = exe.repairCommand(s);
            try {
                exe.executeCommand(s);
            } catch (Exception e) {
                logger.exception(e);
            }
            stopRouter(true, 18, "finished");
            return;
        }
        putln("java -jar " + getFileName() + " <parameters>");
        putln("parameters:");
        userHelp hlp = new userHelp();
        hlp.add(null, false, 1, new int[]{2}, "router", "start router background");
        hlp.add(null, false, 2, new int[]{-1}, "<cfg>", "config url");
        hlp.add(null, false, 1, new int[]{2}, "routerc", "start router with console");
        hlp.add(null, false, 2, new int[]{-1}, "<cfg>", "config url");
        hlp.add(null, false, 1, new int[]{2}, "routerw", "start router with window");
        hlp.add(null, false, 2, new int[]{-1}, "<cfg>", "config url");
        hlp.add(null, false, 1, new int[]{2}, "routercw", "start router with console and window");
        hlp.add(null, false, 2, new int[]{-1}, "<cfg>", "config url");
        hlp.add(null, false, 1, new int[]{2}, "routers", "start router from separate configs");
        hlp.add(null, false, 2, new int[]{3}, "<hwcfg>", "config url");
        hlp.add(null, false, 3, new int[]{-1}, "<swcfg>", "config url");
        hlp.add(null, false, 1, new int[]{2}, "routera", "start router with sw config only");
        hlp.add(null, false, 2, new int[]{-1}, "<swcfg>", "config url");
        hlp.add(null, false, 1, new int[]{2}, "test", "execute test command");
        hlp.add(null, false, 2, new int[]{-1}, "<cmd>", "command to execute");
        hlp.add(null, false, 1, new int[]{2}, "show", "execute show command");
        hlp.add(null, false, 2, new int[]{-1}, "<cmd>", "command to execute");
        hlp.add(null, false, 1, new int[]{2}, "exec", "execute exec command");
        hlp.add(null, false, 2, new int[]{-1}, "<cmd>", "command to execute");
        hlp.add(null, false, 1, new int[]{2}, "cfgexec", "execute exec command");
        hlp.add(null, false, 2, new int[]{3}, "<swcfg>", "config url");
        hlp.add(null, false, 3, new int[]{3, -1}, "<cmd>", "command to execute");
        List<String> res = hlp.getUsage();
        for (int i = 0; i < res.size(); i++) {
            putln(res.get(i));
        }
    }

    private final static void putln(String s) {
        System.out.println(s);
    }

    public void run() {
        int rnd = 0;
        counter cntr = new counter();
        cntr.byteRx = bits.getTime() / 8;
        timerHistory = new history(cntr);
        Runtime rt = Runtime.getRuntime();
        cntr.byteRx = rt.freeMemory() / 8;
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
                timerHistory.update(cntr, true);
                cntr.byteRx = rt.freeMemory() / 8;
                memoryHistory.update(cntr, false);
                if ((rnd % 60) != 0) {
                    continue;
                }
                ipFwdTab.alertVrfs();
                clntDns.purgeLocalCache(false);
                stateSave();
            } catch (Exception e) {
                logger.exception(e);
            }
        }
    }

}

class cfgInitMime implements Comparable<cfgInitMime> {

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

    public int compareTo(cfgInitMime o) {
        return ext.toLowerCase().compareTo(o.ext.toLowerCase());
    }

}

class cfgInitHandler implements UncaughtExceptionHandler {

    public void uncaughtException(Thread t, Throwable e) {
        logger.exception(e);
    }

}
