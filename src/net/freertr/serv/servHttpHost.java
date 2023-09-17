package net.freertr.serv;

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
import net.freertr.cfg.cfgProxy;
import net.freertr.cfg.cfgTrnsltn;
import net.freertr.clnt.clntProxy;
import net.freertr.pipe.pipeSide;
import net.freertr.enc.encUrl;
import net.freertr.tab.tabAceslstN;
import net.freertr.tab.tabListing;
import net.freertr.tab.tabRouteIface;
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

}
