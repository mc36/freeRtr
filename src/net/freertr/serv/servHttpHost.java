package net.freertr.serv;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.RandomAccessFile;
import java.lang.reflect.Method;
import java.net.URI;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.auth.authGeneric;
import net.freertr.cfg.cfgAceslst;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgAuther;
import net.freertr.cfg.cfgIfc;
import net.freertr.cfg.cfgInit;
import net.freertr.cfg.cfgProxy;
import net.freertr.cfg.cfgScrpt;
import net.freertr.cfg.cfgTrnsltn;
import net.freertr.clnt.clntProxy;
import net.freertr.enc.encMarkDown;
import net.freertr.pipe.pipeSide;
import net.freertr.enc.encUrl;
import net.freertr.enc.encXml;
import net.freertr.pipe.pipeConnect;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSetting;
import net.freertr.sec.secWebsock;
import net.freertr.tab.tabAceslstN;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabListing;
import net.freertr.tab.tabRouteIface;
import net.freertr.user.userConfig;
import net.freertr.user.userExec;
import net.freertr.user.userFlash;
import net.freertr.user.userFormat;
import net.freertr.user.userHelping;
import net.freertr.user.userReader;
import net.freertr.user.userScript;
import net.freertr.user.userTerminal;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.debugger;
import net.freertr.util.logger;
import net.freertr.util.version;

/**
 * http virtual host
 *
 * @author matecsaba
 */
public class servHttpHost implements Runnable, Comparator<servHttpHost> {

    /**
     * create instance
     *
     * @param h host
     */
    public servHttpHost(String h) {
        host = h;
    }

    /**
     * allow nothing
     */
    public final static int apiBitsNothing = 0;

    /**
     * allow something
     */
    public final static int apiBitsSomething = 0x01;

    /**
     * allow exec commands
     */
    public final static int apiBitsExec = 0x02;

    /**
     * allow config commands
     */
    public final static int apiBitsConfig = 0x04;

    /**
     * allow ip info commands
     */
    public final static int apiBitsIpinfo = 0x08;

    protected final static void dumpXml(String s) {
        if (!debugger.servHttpXml) {
            return;
        }
        dumpXml(encXml.parseOne(s.replaceAll("\r", "").replaceAll("\n", "")));
    }

    protected final static void dumpXml(encXml xml) {
        if (!debugger.servHttpXml) {
            return;
        }
        List<String> l = xml.show();
        for (int i = 0; i < l.size(); i++) {
            logger.debug("xml " + l.get(i));
        }
    }

    /**
     * convert string to api bits
     *
     * @param cmd commands
     * @return api bits
     */
    public static final int string2apiBits(cmds cmd) {
        int i = apiBitsNothing;
        for (;;) {
            String a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            if (a.equals("exec")) {
                i |= apiBitsExec;
                continue;
            }
            if (a.equals("config")) {
                i |= 4;
                continue;
            }
            if (a.equals("ipinfo")) {
                i |= 8;
                continue;
            }
        }
        if (i == apiBitsSomething) {
            return apiBitsNothing;
        }
        return i | apiBitsSomething;
    }

    /**
     * convert api bits to string
     *
     * @param i bits to convert
     * @return api bits
     */
    public static final String apiBits2string(int i) {
        if ((i & apiBitsSomething) == 0) {
            return "bug=" + i;
        }
        String s = "";
        if ((i & apiBitsExec) != 0) {
            s += " exec";
        }
        if ((i & apiBitsConfig) != 0) {
            s += " config";
        }
        if ((i & apiBitsIpinfo) != 0) {
            s += " ipinfo";
        }
        return s;
    }

    /**
     * subconnect to string
     *
     * @param subconn subconnect mode
     * @return config string
     */
    protected final static String subconn2string(int subconn) {
        String s = "";
        if ((subconn & 0x1) != 0) {
            s += " strip-path";
        }
        if ((subconn & 0x2) != 0) {
            s += " strip-name";
        }
        if ((subconn & 0x4) != 0) {
            s += " strip-ext";
        }
        if ((subconn & 0x8) != 0) {
            s += " strip-param";
        }
        if ((subconn & 0x10) != 0) {
            s += " keep-cred";
        }
        if ((subconn & 0x20) != 0) {
            s += " keep-host";
        }
        if ((subconn & 0x40) != 0) {
            s += " keep-path";
        }
        return s;
    }

    protected static final boolean checkNoHeaders(String s) {
        return new File(s + ".noheaders").exists();
    }

    /**
     * read up subconnect modes
     *
     * @param neg negated
     * @param cmd commands to read
     * @return subconnect mode
     */
    protected final static int string2subconn(boolean neg, cmds cmd) {
        if (neg) {
            return 0;
        }
        int res = 0;
        for (;;) {
            String a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            if (a.equals("strip-path")) {
                res |= 0x1;
                continue;
            }
            if (a.equals("strip-name")) {
                res |= 0x2;
                continue;
            }
            if (a.equals("strip-ext")) {
                res |= 0x4;
                continue;
            }
            if (a.equals("strip-param")) {
                res |= 0x8;
                continue;
            }
            if (a.equals("keep-cred")) {
                res |= 0x10;
                continue;
            }
            if (a.equals("keep-host")) {
                res |= 0x20;
                continue;
            }
            if (a.equals("keep-path")) {
                res |= 0x40;
                continue;
            }
        }
        return res;
    }

    /**
     * name of server
     */
    public final String host;

    /**
     * asked
     */
    public int askNum;

    /**
     * asked
     */
    public long askTim;

    /**
     * path of root directory
     */
    public String path = null;

    /**
     * path of backup directory
     */
    public String backupPath = null;

    /**
     * number of backups to keep
     */
    public int backupCount = 0;

    /**
     * url to redirect to
     */
    public String redir;

    /**
     * log to syslog
     */
    public boolean logging;

    /**
     * proxy for reconnection
     */
    public clntProxy reconnP;

    /**
     * url to reconnect to
     */
    public String reconnT;

    /**
     * restrict url
     */
    public int subconn;

    /**
     * translate url
     */
    public List<cfgTrnsltn> translate;

    /**
     * proxy for stream
     */
    public clntProxy streamP;

    /**
     * url to stream
     */
    public String streamT;

    /**
     * pipe of stream
     */
    public pipeSide streamS;

    /**
     * content type of stream
     */
    public String streamM;

    /**
     * list of receivers
     */
    public List<pipeSide> streamC;

    /**
     * proxy for multiple access
     */
    public clntProxy multiAccP;

    /**
     * urls to multiple access
     */
    public String multiAccT;

    /**
     * page style
     */
    public List<String> style;

    /**
     * speed limit
     */
    public int speedLimit;

    /**
     * serve index for dirs
     */
    public boolean autoIndex = true;

    /**
     * convert markdown files
     */
    public boolean allowMarkdown = false;

    /**
     * directory listing allowed
     */
    public int allowList;

    /**
     * script search allowed
     */
    public String searchScript;

    /**
     * script running allowed
     */
    public int allowScript;

    /**
     * api calls allowed
     */
    public int allowApi;

    /**
     * ip info configuration
     */
    public servHoneyPotCfg ipInfo;

    /**
     * image map decode allowed
     */
    public boolean allowImgMap;

    /**
     * web socket allowed
     */
    public boolean allowWebSck;

    /**
     * image streaming allowed
     */
    public boolean allowMediaStrm;

    /**
     * class running allowed
     */
    public URLClassLoader allowClass;

    /**
     * uploading allowed
     */
    public boolean allowUpload;

    /**
     * webdav allowed
     */
    public boolean allowWebDav;

    /**
     * sstp allowed
     */
    public cfgIfc allowSstp;

    /**
     * anyconnect allowed
     */
    public cfgIfc allowAnyconn;

    /**
     * fortinet allowed
     */
    public cfgIfc allowForti;

    /**
     * authentication list
     */
    public authGeneric authenticList;

    /**
     * access list
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> accessList;

    public int compare(servHttpHost o1, servHttpHost o2) {
        return o1.host.toLowerCase().compareTo(o2.host.toLowerCase());
    }

    /**
     * start streaming
     */
    public void reStream() {
        if (streamS != null) {
            if (streamS.isClosed() == 0) {
                return;
            }
        }
        new Thread(this).start();
    }

    public void run() {
        try {
            doStream();
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

    private void doStream() {
        if (debugger.servHttpTraf) {
            logger.debug("startup");
        }
        bits.sleep(1000);
        encUrl srvUrl = encUrl.parseOne(streamT);
        addrIP adr = userTerminal.justResolv(srvUrl.server, streamP.prefer);
        if (adr == null) {
            return;
        }
        pipeSide cnn = streamP.doConnect(servGeneric.protoTcp, adr, srvUrl.getPort(new servHttp().srvPort()), "http");
        if (cnn == null) {
            return;
        }
        streamS = cnn;
        if (debugger.servHttpTraf) {
            logger.debug("conned");
        }
        cnn.lineTx = pipeSide.modTyp.modeCRLF;
        cnn.lineRx = pipeSide.modTyp.modeCRtryLF;
        cnn.linePut("GET " + srvUrl.toURL(false, false, true, true) + " HTTP/1.1");
        cnn.linePut("User-Agent: " + version.usrAgnt + " [streaming]");
        cnn.linePut("Host: " + srvUrl.server);
        cnn.linePut("Accept: */*");
        cnn.linePut("Accept-Language: *");
        cnn.linePut("Accept-Charset: *");
        cnn.linePut("Accept-Encoding: identity");
        cnn.linePut("Connection: Close");
        cnn.linePut("");
        for (;;) {
            String s = cnn.lineGet(1);
            if (s == null) {
                break;
            }
            if (s.length() < 1) {
                break;
            }
            if (debugger.servHttpTraf) {
                logger.debug("rx: " + s);
            }
        }
        if (debugger.servHttpTraf) {
            logger.debug("serving");
        }
        for (;;) {
            byte[] buf = new byte[1024];
            int siz = cnn.blockingGet(buf, 0, buf.length);
            if (siz < 1) {
                if (cnn.isClosed() != 0) {
                    break;
                }
                bits.sleep(1000);
                continue;
            }
            int i = streamC.size() - 1;
            if (i < 0) {
                break;
            }
            for (; i >= 0; i--) {
                pipeSide pip = streamC.get(i);
                if (pip.isClosed() == 0) {
                    pip.nonBlockPut(buf, 0, siz);
                    continue;
                }
                streamC.remove(i);
                pip.setClose();
            }
        }
        cnn.setClose();
        for (int i = streamC.size() - 1; i >= 0; i--) {
            streamC.get(i).setClose();
        }
        if (debugger.servHttpTraf) {
            logger.debug("stopped");
        }
    }

    /**
     * get config
     *
     * @param beg beginning
     * @param l list
     * @param filter default filter
     */
    protected void getConfig(String beg, List<String> l, int filter) {
        String a = beg + "host " + host;
        l.add(a + " path " + path);
        if (style != null) {
            for (int i = 0; i < style.size(); i++) {
                l.add(a + " style " + style.get(i));
            }
        }
        if (redir != null) {
            l.add(a + " redir " + redir);
        }
        if (logging) {
            l.add(a + " logging");
        }
        if (reconnT != null) {
            l.add(a + " reconn " + reconnP.name + " " + reconnT);
        }
        if (translate != null) {
            String s = "";
            for (int i = 0; i < translate.size(); i++) {
                s += " " + translate.get(i).name;
            }
            l.add(a + " translate" + s);
        }
        if (subconn != 0) {
            String s = subconn2string(subconn);
            l.add(a + " subconn" + s);
        }
        if (streamT != null) {
            l.add(a + " stream " + streamM + " " + streamP.name + " " + streamT);
        }
        if (multiAccT != null) {
            l.add(a + " multiacc " + multiAccP.name + " " + multiAccT);
        }
        if (allowList != 0) {
            String s = "";
            if ((allowList & 2) != 0) {
                s += " readme";
            }
            if ((allowList & 4) != 0) {
                s += " stats";
            }
            l.add(a + " dirlist" + s);
        }
        if (allowMarkdown) {
            l.add(a + " markdown");
        }
        if (speedLimit > 0) {
            l.add(a + " speed-limit " + speedLimit);
        }
        if (!autoIndex) {
            l.add(a + " noindex");
        }
        if (searchScript != null) {
            l.add(a + " search-script " + searchScript);
        }
        if (allowScript != 0) {
            String s = "";
            if ((allowScript & 2) != 0) {
                s += " exec";
            }
            if ((allowScript & 4) != 0) {
                s += " config";
            }
            l.add(a + " script" + s);
        }
        if (allowApi != apiBitsNothing) {
            l.add(a + " api" + apiBits2string(allowApi));
        }
        if (ipInfo != null) {
            ipInfo.doGetCfg(a, l, false);
        }
        if (allowImgMap) {
            l.add(a + " imagemap");
        }
        if (allowWebSck) {
            l.add(a + " websock");
        }
        if (allowWebDav) {
            l.add(a + " webdav");
        }
        if (allowMediaStrm) {
            l.add(a + " mediastream");
        }
        if (allowClass != null) {
            l.add(a + " class");
        }
        if (allowUpload) {
            l.add(a + " upload");
        }
        if (backupPath != null) {
            l.add(a + " backup " + backupCount + " " + backupPath);
        }
        if (allowSstp != null) {
            l.add(a + " sstp " + allowSstp.name);
        }
        if (allowAnyconn != null) {
            l.add(a + " anyconn " + allowAnyconn.name);
        }
        if (allowForti != null) {
            l.add(a + " forti " + allowForti.name);
        }
        if (authenticList != null) {
            l.add(a + " authentication " + authenticList.autName);
        }
        if (accessList != null) {
            l.add(a + " access-class " + accessList.listName);
        }
    }

    /**
     * do configuration
     *
     * @param negated negated
     * @param a command read
     * @param cmd commands to read
     * @return true on error, false on success
     */
    protected boolean doConfig(boolean negated, String a, cmds cmd) {
        if (a.equals("style")) {
            a = cmd.getRemaining();
            if (negated) {
                if (style == null) {
                    return false;
                }
                style.remove(a);
                return false;
            }
            if (style == null) {
                style = new ArrayList<String>();
            }
            if (style.indexOf(a) >= 0) {
                return false;
            }
            style.add(a);
            return false;
        }
        if (a.equals("redir")) {
            if (negated) {
                redir = null;
                return false;
            }
            redir = cmd.word();
            return false;
        }
        if (a.equals("logging")) {
            logging = !negated;
            return false;
        }
        if (a.equals("reconn")) {
            if (negated) {
                reconnP = null;
                reconnT = null;
                return false;
            }
            cfgProxy prx = cfgAll.proxyFind(cmd.word(), false);
            if (prx == null) {
                cmd.error("no such proxy");
                return false;
            }
            reconnP = prx.proxy;
            reconnT = cmd.word();
            return false;
        }
        if (a.equals("translate")) {
            if (negated) {
                translate = null;
                return false;
            }
            translate = new ArrayList<cfgTrnsltn>();
            for (;;) {
                a = cmd.word();
                if (a.length() < 1) {
                    break;
                }
                cfgTrnsltn trn = cfgAll.trnsltnFind(a, false);
                if (trn == null) {
                    cmd.error("no such rule");
                    continue;
                }
                translate.add(trn);
            }
            return false;
        }
        if (a.equals("subconn")) {
            subconn = string2subconn(negated, cmd);
            return false;
        }
        if (a.equals("stream")) {
            if (negated) {
                streamP = null;
                streamT = null;
                streamM = null;
                if (streamS == null) {
                    return false;
                }
                streamS.setClose();
                streamS = null;
                return false;
            }
            a = cmd.word();
            cfgProxy prx = cfgAll.proxyFind(cmd.word(), false);
            if (prx == null) {
                cmd.error("no such proxy");
                return false;
            }
            streamM = a;
            streamP = prx.proxy;
            streamT = cmd.word();
            if (streamC == null) {
                streamC = new ArrayList<pipeSide>();
            }
            if (streamS == null) {
                return false;
            }
            streamS.setClose();
            streamS = null;
            return false;
        }
        if (a.equals("multiacc")) {
            if (negated) {
                multiAccP = null;
                multiAccT = null;
                return false;
            }
            cfgProxy prx = cfgAll.proxyFind(cmd.word(), false);
            if (prx == null) {
                cmd.error("no such proxy");
                return false;
            }
            multiAccP = prx.proxy;
            multiAccT = cmd.getRemaining();
            return false;
        }
        if (a.equals("speed-limit")) {
            if (negated) {
                speedLimit = 0;
                return false;
            }
            speedLimit = bits.str2num(cmd.word());
            return false;
        }
        if (a.equals("noindex")) {
            autoIndex = negated;
            return false;
        }
        if (a.equals("markdown")) {
            allowMarkdown = !negated;
            return false;
        }
        if (a.equals("dirlist")) {
            if (negated) {
                allowList = 0;
                return false;
            }
            allowList = 1;
            for (;;) {
                a = cmd.word();
                if (a.length() < 1) {
                    break;
                }
                if (a.equals("readme")) {
                    allowList |= 2;
                    continue;
                }
                if (a.equals("stats")) {
                    allowList |= 4;
                    continue;
                }
            }
            return false;
        }
        if (a.equals("api")) {
            if (negated) {
                allowApi = apiBitsNothing;
                return false;
            }
            allowApi = string2apiBits(cmd);
            return false;
        }
        if (a.equals("ipinfo")) {
            if (negated) {
                ipInfo = null;
                return false;
            }
            ipInfo = new servHoneyPotCfg();
            ipInfo.doCfgStr(cmd, negated);
            return false;
        }
        if (a.equals("search-script")) {
            if (negated) {
                searchScript = null;
                return false;
            }
            searchScript = cmd.word();
            return false;
        }
        if (a.equals("script")) {
            if (negated) {
                allowScript = 0;
                return false;
            }
            allowScript = 1;
            for (;;) {
                a = cmd.word();
                if (a.length() < 1) {
                    break;
                }
                if (a.equals("exec")) {
                    allowScript |= 2;
                    continue;
                }
                if (a.equals("config")) {
                    allowScript |= 4;
                    continue;
                }
            }
            return false;
        }
        if (a.equals("imagemap")) {
            allowImgMap = !negated;
            return false;
        }
        if (a.equals("websock")) {
            allowWebSck = !negated;
            return false;
        }
        if (a.equals("webdav")) {
            allowWebDav = !negated;
            return false;
        }
        if (a.equals("mediastream")) {
            allowMediaStrm = !negated;
            return false;
        }
        if (a.equals("class")) {
            if (negated) {
                allowClass = null;
                return false;
            }
            try {
                URL url = new URI("file://" + path).toURL();
                URL[] urls = new URL[1];
                urls[0] = url;
                allowClass = new URLClassLoader(urls);
            } catch (Exception e) {
                allowClass = null;
            }
            return false;
        }
        if (a.equals("upload")) {
            allowUpload = !negated;
            return false;
        }
        if (a.equals("backup")) {
            if (negated) {
                backupCount = 0;
                backupPath = null;
                return false;
            }
            backupCount = bits.str2num(cmd.word());
            backupPath = "/" + encUrl.normalizePath(cmd.word() + "/");
            return false;
        }
        if (a.equals("sstp")) {
            if (negated) {
                allowSstp = null;
                return false;
            }
            allowSstp = cfgAll.ifcFind(cmd.word(), 0);
            if (allowSstp == null) {
                cmd.error("no such interface");
                return false;
            }
            if (allowSstp.type != tabRouteIface.ifaceType.dialer) {
                cmd.error("not dialer interface");
                allowSstp = null;
                return false;
            }
            return false;
        }
        if (a.equals("anyconn")) {
            if (negated) {
                allowAnyconn = null;
                return false;
            }
            allowAnyconn = cfgAll.ifcFind(cmd.word(), 0);
            if (allowAnyconn == null) {
                cmd.error("no such interface");
                return false;
            }
            if (allowAnyconn.type != tabRouteIface.ifaceType.dialer) {
                cmd.error("not dialer interface");
                allowAnyconn = null;
                return false;
            }
            return false;
        }
        if (a.equals("forti")) {
            if (negated) {
                allowForti = null;
                return false;
            }
            allowForti = cfgAll.ifcFind(cmd.word(), 0);
            if (allowForti == null) {
                cmd.error("no such interface");
                return false;
            }
            if (allowForti.type != tabRouteIface.ifaceType.dialer) {
                cmd.error("not dialer interface");
                allowForti = null;
                return false;
            }
            return false;
        }
        if (a.equals("access-class")) {
            if (negated) {
                accessList = null;
                return false;
            }
            cfgAceslst lst = cfgAll.aclsFind(cmd.word(), false);
            if (lst == null) {
                cmd.error("no such access list");
                return false;
            }
            accessList = lst.aceslst;
            return false;
        }
        if (a.equals("authentication")) {
            if (negated) {
                authenticList = null;
                return false;
            }
            cfgAuther lst = cfgAll.autherFind(cmd.word(), null);
            if (lst == null) {
                cmd.error("no such auth list");
                return false;
            }
            authenticList = lst.getAuther();
            return false;
        }
        return true;
    }

    /**
     * get style of host
     *
     * @return style
     */
    protected String getStyle() {
        if (style == null) {
            return "";
        }
        String s = "<style>\n";
        for (int o = 0; o < style.size(); o++) {
            String a = style.get(o);
            if (!a.startsWith("@import ")) {
                s += " " + a + "\n";
                continue;
            }
            List<String> l = bits.txt2buf(path + a.substring(8, a.length()));
            if (l == null) {
                continue;
            }
            for (int i = 0; i < l.size(); i++) {
                s += " " + l.get(i) + "\n";
            }
        }
        return s + "</style>\n";
    }

    private final static String semi2comma(String a) {
        return a.replaceAll(";", ",");
    }

    protected void updateVisitors(servHttpConn cn, String pn) {
        pn = path + pn + ".visitors";
        if (!new File(pn).exists()) {
            return;
        }
        String a = cn.peer + ";" + logger.getTimestamp() + ";" + semi2comma(cn.gotAgent) + ";" + semi2comma(cn.gotReferer) + "\n";
        bits.byteSave(false, a.getBytes(), pn);
    }

    protected boolean sendOneWebSck(servHttpConn cn, String pn) {
        pn = path + pn + ".websock";
        if (!new File(pn).exists()) {
            return true;
        }
        List<String> l = bits.txt2buf(pn);
        if (l == null) {
            return true;
        }
        if (l.size() < 5) {
            return true;
        }
        cfgProxy prx = cfgAll.proxyFind(l.get(0), false);
        if (prx == null) {
            cn.sendRespError(502, "bad proxy profile");
            return false;
        }
        addrIP adr = userTerminal.justResolv(l.get(1), prx.proxy.prefer);
        if (adr == null) {
            cn.sendRespError(502, "bad target hostname");
            return false;
        }
        pipeSide pip = prx.proxy.doConnect(servGeneric.protoTcp, adr, bits.str2num(l.get(2)), "websock");
        if (pip == null) {
            cn.sendRespError(502, "failed to connect");
            return false;
        }
        cn.sendLn("HTTP/1.1 101 switching protocol");
        cn.sendLn("Upgrade: websocket");
        cn.sendLn("Connection: Upgrade");
        cn.sendLn("Sec-WebSocket-Accept: " + secWebsock.calcHash(cn.gotWebsock));
        cn.sendLn("Sec-WebSocket-Protocol: " + l.get(3));
        cn.sendLn("");
        secWebsock wsk = new secWebsock(cn.pipe, new pipeLine(cn.lower.bufSiz, false));
        wsk.binary = l.get(4).equals("bin");
        wsk.startServer();
        pipeConnect.connect(pip, wsk.getPipe(), true);
        cn.gotKeep = false;
        cn.pipe = null;
        return false;
    }

    private String parseFileName(servHttpConn cn, String s) {
        int i = s.lastIndexOf(".");
        if (i < 0) {
            return s;
        }
        cn.addHeader("Content-Disposition: attachment; filename=\"" + s + "\"");
        return s.substring(i + 1, s.length());
    }

    protected boolean sendOneClass(servHttpConn cn, String s) {
        byte[] res = null;
        try {
            if (!new File(path + s).exists()) {
                return true;
            }
            Class<?> cls = allowClass.loadClass(cn.gotUrl.filPath + cn.gotUrl.filName);
            Object obj = cls.getDeclaredConstructor().newInstance();
            Method[] mth = cls.getDeclaredMethods();
            int o = -1;
            for (int i = 0; i < mth.length; i++) {
                if (!mth[i].getName().equals("httpRequest")) {
                    continue;
                }
                o = i;
                break;
            }
            if (o < 0) {
                return true;
            }
            String[] par = new String[cn.gotUrl.param.size()];
            for (int i = 0; i < par.length; i++) {
                par[i] = "" + cn.gotUrl.param.get(i);
            }
            ByteArrayOutputStream buf = new ByteArrayOutputStream();
            obj = mth[o].invoke(obj, cn.gotUrl.toURL(true, false, false, true), path + s, "" + cn.peer, cn.gotAgent, cn.gotAuth, par, buf);
            s = (String) obj;
            res = buf.toByteArray();
        } catch (Exception e) {
            logger.traceback(e, cn.gotUrl.dump() + " peer=" + cn.peer);
            return true;
        }
        if (debugger.servHttpTraf) {
            logger.debug("res=" + s + " bytes=" + res.length);
        }
        if (s == null) {
            return true;
        }
        if (!s.equals("//file//")) {
            s = parseFileName(cn, s);
            cn.sendTextHeader("200 ok", cfgInit.findMimeType(s), res);
            return false;
        }
        s = new String(res);
        int i = s.indexOf("\n");
        String a;
        if (i < 0) {
            a = s;
        } else {
            a = s.substring(0, i);
            s = s.substring(i + 1, s.length());
        }
        i = s.indexOf("\n");
        if (i < 0) {
            s = "." + parseFileName(cn, s);
        } else {
            parseFileName(cn, s.substring(0, i));
            s = s.substring(i + 1, s.length());
        }
        i = s.indexOf("\n");
        int m;
        if (i < 0) {
            m = speedLimit;
        } else {
            m = bits.str2num(s.substring(i + 1, s.length()));
            s = s.substring(0, i);
        }
        if (!a.startsWith("/")) {
            a = path + a;
        }
        return sendBinFile(cn, a, s, m);
    }

    protected boolean sendOneImgMap(servHttpConn cn, String s) {
        List<String> buf = bits.txt2buf(path + s);
        if (buf == null) {
            return true;
        }
        if (cn.gotUrl.param.size() < 1) {
            return true;
        }
        s = "" + cn.gotUrl.param.get(0);
        int i = s.indexOf("=");
        if (i < 0) {
            return true;
        }
        s = s.substring(0, i);
        i = s.indexOf(",");
        if (i < 0) {
            return true;
        }
        int x = bits.str2num(s.substring(0, i));
        int y = bits.str2num(s.substring(i + 1, s.length()));
        for (i = 0; i < buf.size(); i++) {
            cmds cmd = new cmds("line", buf.get(i));
            s = cmd.word().toLowerCase();
            if (s.equals("rectangle")) {
                int bx = bits.str2num(cmd.word());
                int by = bits.str2num(cmd.word());
                int ex = bits.str2num(cmd.word());
                int ey = bits.str2num(cmd.word());
                if (x < bx) {
                    continue;
                }
                if (x > ex) {
                    continue;
                }
                if (y < by) {
                    continue;
                }
                if (y > ey) {
                    continue;
                }
                cn.sendFoundAt(cmd.getRemaining());
                return false;
            }
            if (s.equals("default")) {
                cn.sendFoundAt(cmd.getRemaining());
                return false;
            }
        }
        return true;
    }

    protected boolean sendOneScript(servHttpConn cn, String fn) {
        List<String> l = bits.txt2buf(path + fn);
        if (l == null) {
            return true;
        }
        return sendOneScript(cn, l);
    }

    protected boolean sendOneScript(servHttpConn cn, List<String> l) {
        pipeLine pl = new pipeLine(1024 * 1024, false);
        pipeSide pip = pl.getSide();
        pip.setTime(60000);
        pip.lineTx = pipeSide.modTyp.modeCRLF;
        pip.lineRx = pipeSide.modTyp.modeCRorLF;
        userScript t = new userScript(pip, "");
        t.addLines(l);
        t.allowConfig = (allowScript & 4) != 0;
        t.allowExec = (allowScript & 2) != 0;
        t.currDir = path;
        pip = pl.getSide();
        pip.lineTx = pipeSide.modTyp.modeCR;
        pip.lineRx = pipeSide.modTyp.modeCRorLF;
        pip.linePut("prot=" + cn.gotUrl.proto);
        pip.linePut("serv=" + cn.gotUrl.server);
        pip.linePut("path=" + cn.gotUrl.toPathName());
        pip.linePut("agnt=" + cn.gotAgent);
        pip.linePut("refr=" + cn.gotReferer);
        if (cn.gotAuth != null) {
            pip.linePut("auth=" + cn.gotAuth);
        }
        pip.linePut("clnt=" + cn.peer);
        for (int i = 0; i < cn.gotUrl.param.size(); i++) {
            pip.linePut("par." + cn.gotUrl.param.get(i));
        }
        for (int i = 0; i < cn.gotCook.size(); i++) {
            pip.linePut("cok." + cn.gotCook.get(i));
        }
        pip.linePut(".");
        t.cmdAll();
        pl.setClose();
        String s = pip.strGet(1024 * 1024);
        if (s == null) {
            s = "";
        }
        cn.sendTextHeader("200 ok", "text/html", s.getBytes());
        return false;
    }

    protected boolean sendOneMarkdown(servHttpConn cn, String s) {
        List<String> l = bits.txt2buf(path + s);
        if (l == null) {
            return true;
        }
        String rsp = servHttp.htmlHead + getStyle() + "<title>" + s + "</title></head><body>\n";
        rsp += encMarkDown.md2html(l);
        rsp += "</body></html>\n";
        cn.sendTextHeader("200 ok", "text/html", rsp.getBytes());
        return false;
    }

    protected boolean sendOneApi(servHttpConn cn, String s) {
        if (allowApi == servHttpHost.apiBitsNothing) {
            return true;
        }
        if ((allowApi & servHttpHost.apiBitsSomething) == 0) {
            return true;
        }
        cmds cmd = new cmds("api", s);
        cmd.word("/");
        s = cmd.word("/");
        if (debugger.servHttpTraf) {
            logger.debug("api queried cnd=" + s + " prm=" + cmd.getRemaining() + " from " + cn.peer);
        }
        if (((allowApi & servHttpHost.apiBitsIpinfo) != 0) && s.equals("ipinfo")) {
            addrIP adr = null;
            boolean hck = false;
            boolean det = false;
            for (;;) {
                s = cmd.word("/");
                if (s.length() < 1) {
                    break;
                }
                if (s.equals("addr")) {
                    adr = new addrIP();
                    adr.fromString(cmd.word());
                    continue;
                }
                if (s.equals("hack")) {
                    hck = true;
                    continue;
                }
                if (s.equals("detail")) {
                    det = true;
                    continue;
                }
                if (s.equals("short")) {
                    det = false;
                    continue;
                }
            }
            if (adr == null) {
                adr = cn.peer.copyBytes();
            }
            String r = "real ipinfo goes here\r\n";///////////////////////

            cn.sendTextHeader("200 ok", "text/plain", r.getBytes());
            return false;
        }
        if (((allowApi & servHttpHost.apiBitsExec) != 0) && s.equals("exec")) {
            String r = "";
            String e = new String(pipeSide.getEnding(pipeSide.modTyp.modeCRLF));
            for (;;) {
                s = cmd.word("/");
                if (s.length() < 1) {
                    break;
                }
                pipeLine pl = new pipeLine(1024 * 1024, false);
                pipeSide pip = pl.getSide();
                pip.lineTx = pipeSide.modTyp.modeCRLF;
                pip.lineRx = pipeSide.modTyp.modeCRorLF;
                userReader rdr = new userReader(pip, null);
                pip.settingsPut(pipeSetting.tabMod, userFormat.tableMode.raw);
                pip.settingsPut(pipeSetting.height, 0);
                userExec exe = new userExec(pip, rdr);
                exe.privileged = (allowApi & servHttpHost.apiBitsConfig) != 0;
                pip.setTime(60000);
                String a = exe.repairCommand(s);
                r += "#" + a + e;
                exe.executeCommand(a);
                pip = pl.getSide();
                pl.setClose();
                s = pip.strGet(1024 * 1024);
                if (s == null) {
                    continue;
                }
                r += s;
            }
            cn.sendTextHeader("200 ok", "text/plain", r.getBytes());
            return false;
        }
        if (((allowApi & servHttpHost.apiBitsConfig) != 0) && s.equals("config")) {
            pipeLine pl = new pipeLine(65535, false);
            pipeSide pip = pl.getSide();
            pip.lineTx = pipeSide.modTyp.modeCRLF;
            pip.lineRx = pipeSide.modTyp.modeCRorLF;
            userReader rdr = new userReader(pip, null);
            pip.settingsPut(pipeSetting.tabMod, userFormat.tableMode.raw);
            pip.settingsPut(pipeSetting.height, 0);
            userConfig cfg = new userConfig(pip, rdr);
            pip.setTime(60000);
            for (;;) {
                s = cmd.word("/");
                if (s.length() < 1) {
                    break;
                }
                userHelping hlp = cfg.getHelping(false, true, true);
                rdr.setContext(hlp, "");
                String b = hlp.repairLine(s);
                if (b.length() < 1) {
                    pip.linePut("bad: " + s);
                    continue;
                }
                pip.linePut("#" + b);
                cfg.executeCommand(b);
            }
            pip = pl.getSide();
            pl.setClose();
            s = pip.strGet(65535);
            if (s == null) {
                s = "";
            }
            cn.sendTextHeader("200 ok", "text/plain", s.getBytes());
            return false;
        }
        return true;
    }

    protected boolean sendOneStream(servHttpConn cn, String s, String a) {
        cn.gotKeep = false;
        s = path + s;
        cn.sendRespHeader("200 streaming", -1, cfgInit.findMimeType(a));
        if (cn.gotHead) {
            return false;
        }
        long os = new File(s).length() - 65536;
        if (os < 0) {
            os = 0;
        }
        long ot = -1;
        for (;;) {
            if (cn.pipe.isClosed() != 0) {
                break;
            }
            File f = new File(s);
            if (!f.exists()) {
                break;
            }
            long ns = f.length();
            long nt = f.lastModified();
            if ((ns == os) && (nt == ot)) {
                bits.sleep(100);
                continue;
            }
            byte[] buf;
            try {
                RandomAccessFile fr = new RandomAccessFile(f, "r");
                ns = fr.length();
                if (ns < os) {
                    os = 0;
                }
                buf = new byte[(int) (ns - os)];
                fr.seek(os);
                fr.read(buf);
                fr.close();
            } catch (Exception e) {
                return true;
            }
            ot = nt;
            os = ns;
            cn.pipe.morePut(buf, 0, buf.length);
        }
        cn.pipe.setClose();
        return false;
    }

    protected boolean sendOneMotion(servHttpConn cn, String s, String a) {
        cn.gotKeep = false;
        s = path + s;
        final String bnd = "someRandomBoundaryStringThatWontOccurs";
        cn.sendRespHeader("200 streaming", -1, "multipart/x-mixed-replace;boundary=" + bnd);
        if (cn.gotHead) {
            return false;
        }
        long os = -1;
        long ot = -1;
        for (;;) {
            if (cn.pipe.isClosed() != 0) {
                break;
            }
            File f = new File(s);
            if (!f.exists()) {
                break;
            }
            long ns = f.length();
            long nt = f.lastModified();
            if ((ns == os) && (nt == ot)) {
                bits.sleep(100);
                continue;
            }
            ot = nt;
            os = ns;
            byte[] buf;
            try {
                RandomAccessFile fr = new RandomAccessFile(f, "r");
                int siz = (int) fr.length();
                buf = new byte[siz];
                fr.read(buf);
                fr.close();
            } catch (Exception e) {
                return true;
            }
            cn.sendLn("--" + bnd);
            cn.sendRespHeader(null, buf.length, cfgInit.findMimeType(a));
            cn.pipe.morePut(buf, 0, buf.length);
        }
        cn.pipe.setClose();
        return false;
    }

    protected boolean sendOneFile(servHttpConn cn, String s, String a) {
        if (searchScript != null) {
            cfgScrpt scr = cfgAll.scrptFind(searchScript + s, false);
            if (scr != null) {
                return sendOneScript(cn, scr.getText());
            }
        }
        if (allowMarkdown) {
            if (a.equals(".md")) {
                return sendOneMarkdown(cn, s);
            }
        }
        if (allowScript != 0) {
            if (a.equals(".tcl")) {
                return sendOneScript(cn, s);
            }
        }
        if (allowClass != null) {
            if (a.equals(".class")) {
                return sendOneClass(cn, s);
            }
        }
        if (allowImgMap) {
            if (a.equals(".imgmap")) {
                return sendOneImgMap(cn, s);
            }
        }
        if (allowMediaStrm) {
            if (a.startsWith(".stream-")) {
                return sendOneStream(cn, s, "." + a.substring(8, a.length()));
            }
        }
        if (allowMediaStrm) {
            if (a.startsWith(".motion-")) {
                return sendOneMotion(cn, s, "." + a.substring(8, a.length()));
            }
        }
        return sendBinFile(cn, path + s, a, speedLimit);
    }

    protected boolean sendOneDir(servHttpConn cn, String s) {
        if (autoIndex) {
            if (!sendOneFile(cn, s + "index.html", ".html")) {
                return false;
            }
            if (!sendOneFile(cn, s + "index.txt", ".txt")) {
                return false;
            }
            if (!sendOneFile(cn, s + "index.md", ".md")) {
                return false;
            }
            if (!sendOneFile(cn, s + "index.class", ".class")) {
                return false;
            }
            if (!sendOneFile(cn, s + "index.tcl", ".tcl")) {
                return false;
            }
        }
        if (allowList == 0) {
            cn.sendRespError(404, "not found");
            return true;
        }
        File[] fl = userFlash.dirList(path + s);
        if (fl == null) {
            return true;
        }
        String rsp = servHttp.htmlHead + getStyle() + "<title>dirlist</title></head><body>\n";
        if ((allowList & 2) != 0) {
            rsp += encMarkDown.txt2html(bits.txt2buf(path + s + "readme.txt"));
            rsp += encMarkDown.md2html(bits.txt2buf(path + s + "readme.md"));
        }
        rsp += "<b>directory listing of " + host + "/" + s + " at " + cfgAll.getFqdn() + ":</b><br/><br/>\n";
        rsp += "<table><thead><tr><td><b>date</b></td><td><b>size</b></td><td><b>name</b></td></tr></thead><tbody>\n";
        rsp += "<tr><td>-</td><td>dir</td><td><a href=\"/\">root</a></td></tr>\n";
        rsp += "<tr><td>-</td><td>dir</td><td><a href=\"../\">parent</a></td></tr>\n";
        tabGen<servHttpDirs> stats = new tabGen<servHttpDirs>();
        servHttpDirs totalF = new servHttpDirs(".files.");
        servHttpDirs totalD = new servHttpDirs(".dirs.");
        for (int i = 0; i < fl.length; i++) {
            File f = fl[i];
            String a = f.getName();
            String b = a;
            long len = f.length();
            long mod = f.lastModified();
            if ((allowList & 4) != 0) {
                servHttpDirs ntry;
                int o = a.lastIndexOf(".");
                if (o < 0) {
                    ntry = new servHttpDirs(".empty.");
                } else {
                    ntry = new servHttpDirs(a.substring(o + 1, a.length()));
                }
                servHttpDirs old = stats.add(ntry);
                if (old != null) {
                    ntry = old;
                }
                ntry.update(len, mod);
            }
            String c;
            if (f.isDirectory()) {
                a += "/";
                c = "dir";
                totalD.update(len, mod);
            } else {
                c = "" + len;
                totalF.update(len, mod);
            }
            a = a.replaceAll(":", "%3A");
            rsp += "<tr><td>" + bits.time2str(cfgAll.timeZoneName, mod, 3) + "</td><td>" + c + "</td><td><a href=\"" + a + "\">" + b + "</a></td></tr>\n";
        }
        rsp += "</tbody></table><br/>\n";
        if ((allowList & 4) != 0) {
            rsp += "<br/><table><thead><tr><td><b>extension</b></td><td><b>count</b></td><td><b>bytes</b></td><td><b>smallest</b></td><td><b>biggest</b></td><td><b>oldest</b></td><td><b>newest</b></td></tr></thead><tbody>\n";
            rsp += totalD;
            rsp += totalF;
            for (int i = 0; i < stats.size(); i++) {
                rsp += stats.get(i);
            }
            rsp += "</tbody></table><br/>";
        }
        rsp += "<i>generated by </i><b>" + version.namVer + "</b>.</body></html>\n";
        cn.sendTextHeader("200 ok", "text/html", rsp.getBytes());
        dumpXml(rsp);
        return false;
    }

    protected boolean sendBinFile(servHttpConn cn, String s, String a, int m) {
        RandomAccessFile fr;
        long siz;
        try {
            File f = new File(s);
            if (f.isDirectory()) {
                cn.sendFoundAt(cn.gotUrl.toURL(true, false, false, false) + "/");
                return false;
            }
            fr = new RandomAccessFile(f, "r");
            siz = f.length();
        } catch (Exception e) {
            return true;
        }
        long pos = 0;
        long ranB = -1;
        long ranE = -1;
        if (checkNoHeaders(s)) {
            cn.gotKeep = false;
            cn.gotHead = false;
            cn.gotRange = null;
        }
        if (cn.gotRange != null) {
            cn.gotRange = cn.gotRange.replaceAll(" ", "");
            if (!cn.gotRange.startsWith("bytes=")) {
                cn.gotRange = "";
            } else {
                cn.gotRange = cn.gotRange.substring(6, cn.gotRange.length());
            }
            int i = cn.gotRange.indexOf("-");
            if (i < 0) {
                cn.gotRange = null;
            } else if (i == 0) {
                ranB = bits.str2long(cn.gotRange.substring(1, cn.gotRange.length()));
                ranE = siz - 1;
            } else {
                ranB = bits.str2long(cn.gotRange.substring(0, i));
                ranE = bits.str2long(cn.gotRange.substring(i + 1, cn.gotRange.length()));
            }
            if (ranB < 0) {
                ranB = 0;
            }
            if (ranB >= siz) {
                ranB = siz;
            }
            if (ranE >= siz) {
                ranE = siz - 1;
            }
            if (ranE <= ranB) {
                ranE = siz - 1;
            }
        }
        if (cn.gotRange == null) {
            if (!checkNoHeaders(s)) {
                cn.sendRespHeader("200 ok", siz, cfgInit.findMimeType(a));
            }
        } else {
            cn.addHeader("Content-Range: bytes " + ranB + "-" + ranE + "/" + siz);
            if (!checkNoHeaders(s)) {
                cn.sendRespHeader("206 partial", ranE - ranB + 1, cfgInit.findMimeType(a));
            }
            pos = ranB;
            siz = ranE + 1;
        }
        if (cn.gotHead) {
            siz = 0;
        }
        int don = 0;
        for (; pos < siz;) {
            final int max = 8192;
            long rndl = siz - pos;
            if (rndl > max) {
                rndl = max;
            }
            int rndi = (int) rndl;
            byte[] buf = new byte[rndi];
            try {
                fr.seek(pos);
                fr.read(buf, 0, rndi);
            } catch (Exception e) {
                cn.pipe.setClose();
                break;
            }
            if (cn.pipe.morePut(buf, 0, rndi) != rndi) {
                cn.pipe.setClose();
                break;
            }
            pos += buf.length;
            if (m < 1) {
                continue;
            }
            don += rndi;
            if (don < m) {
                continue;
            }
            bits.sleep(1000);
            don = 0;
        }
        try {
            fr.close();
        } catch (Exception e) {
        }
        return false;
    }

}
