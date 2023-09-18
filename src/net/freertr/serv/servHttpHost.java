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
import net.freertr.auth.authResult;
import net.freertr.cfg.cfgAceslst;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgAuther;
import net.freertr.cfg.cfgIfc;
import net.freertr.cfg.cfgInit;
import net.freertr.cfg.cfgProxy;
import net.freertr.cfg.cfgScrpt;
import net.freertr.cfg.cfgTrnsltn;
import net.freertr.clnt.clntProxy;
import net.freertr.enc.encBase64;
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
public class servHttpHost implements Comparator<servHttpHost> {

    /**
     * create instance
     *
     * @param h host
     */
    public servHttpHost(String h) {
        host = h;
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
     * streamer handler
     */
    public servHttpStrm streamR;

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
    protected void reStream(servHttpConn cn) {
        cn.gotKeep = false;
        cn.sendRespHeader("200 restreaming", -1, streamM);
        streamC.add(cn.pipe);
        cn.pipe = null;
        if (streamS != null) {
            if (streamS.isClosed() == 0) {
                return;
            }
        }
        if (streamR != null) {
            return;
        }
        streamR = new servHttpStrm(this);
        new Thread(streamR).start();
    }

    protected void doMultAcc(servHttpConn cn) {
        cmds cmd = new cmds("hst", multiAccT);
        List<encUrl> urls = new ArrayList<encUrl>();
        for (;;) {
            String a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            encUrl srvUrl = encUrl.parseOne(a);
            doTranslate(cn, srvUrl);
            doSubconn(cn, srvUrl);
            urls.add(srvUrl);
        }
        addrIP[] adrs = new addrIP[urls.size()];
        for (int i = 0; i < adrs.length; i++) {
            adrs[i] = userTerminal.justResolv(urls.get(i).server, multiAccP.prefer);
        }
        pipeSide[] cons = new pipeSide[adrs.length];
        for (int i = 0; i < adrs.length; i++) {
            cons[i] = null;
            if (adrs[i] == null) {
                continue;
            }
            cons[i] = multiAccP.doConnect(servGeneric.protoTcp, adrs[i], urls.get(i).getPort(cn.lower.srvPort()), "http");
        }
        pipeSide fin = null;
        for (int i = 0; i < cons.length; i++) {
            if (cons[i] == null) {
                continue;
            }
            if (fin != null) {
                fin.setClose();
            }
            fin = cons[i];
            pipeSide.modTyp old = fin.lineTx;
            fin.lineTx = pipeSide.modTyp.modeCRLF;
            fin.linePut(cn.gotCmd.toUpperCase() + " " + urls.get(i).toURL(false, false, true, true) + " HTTP/1.1");
            fin.linePut("User-Agent: " + cn.gotAgent + " [" + version.usrAgnt + " by " + cn.peer + "]");
            fin.linePut("X-Forwarded-For: " + cn.peer);
            fin.linePut("Referer: " + cn.gotReferer);
            fin.linePut("Host: " + cn.gotUrl.server);
            fin.linePut("Accept: */*");
            fin.linePut("Accept-Language: *");
            fin.linePut("Accept-Charset: *");
            fin.linePut("Accept-Encoding: identity");
            if (cn.gotRange != null) {
                fin.linePut("Range: " + cn.gotRange);
            }
            fin.linePut("Connection: Close");
            fin.linePut("");
            fin.lineTx = old;
        }
        if (fin == null) {
            cn.sendRespError(504, "gateways timeout");
            return;
        }
        pipeConnect.connect(cn.pipe, fin, true);
        cn.pipe = null;
    }

    protected void doRedir(servHttpConn cn) {
        encUrl srvUrl = encUrl.parseOne(redir);
        doTranslate(cn, srvUrl);
        doSubconn(cn, srvUrl);
        cn.sendFoundAt(srvUrl.toURL(true, true, true, false));
    }

    protected void doReconn(servHttpConn cn) {
        encUrl srvUrl = encUrl.parseOne(reconnT);
        doTranslate(cn, srvUrl);
        doSubconn(cn, srvUrl);
        addrIP adr = userTerminal.justResolv(srvUrl.server, reconnP.prefer);
        if (adr == null) {
            cn.sendRespError(502, "bad gateway");
            return;
        }
        pipeSide cnn = reconnP.doConnect(servGeneric.protoTcp, adr, srvUrl.getPort(cn.lower.srvPort()), "http");
        if (cnn == null) {
            cn.sendRespError(504, "gateway timeout");
            return;
        }
        if (debugger.servHttpTraf) {
            logger.debug("reconnect " + srvUrl.toURL(true, false, true, true));
        }
        pipeSide.modTyp old = cnn.lineTx;
        cnn.lineTx = pipeSide.modTyp.modeCRLF;
        cnn.linePut(cn.gotCmd.toUpperCase() + " " + srvUrl.toURL(false, false, true, true) + " HTTP/1.1");
        cnn.linePut("User-Agent: " + cn.gotAgent + " [" + version.usrAgnt + " by " + cn.peer + "]");
        cnn.linePut("X-Forwarded-For: " + cn.peer);
        cnn.linePut("Referer: " + cn.gotReferer);
        cnn.linePut("Host: " + srvUrl.server);
        cnn.linePut("Accept: */*");
        cnn.linePut("Accept-Language: *");
        cnn.linePut("Accept-Charset: *");
        cnn.linePut("Accept-Encoding: identity");
        if (cn.gotRange != null) {
            cnn.linePut("Range: " + cn.gotRange);
        }
        cnn.linePut("Connection: Close");
        cnn.linePut("");
        cnn.lineTx = old;
        pipeConnect.connect(cn.pipe, cnn, true);
        cn.pipe = null;
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
            String s = servHttpUtil.subconn2string(subconn);
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
        if (allowApi != servHttpUtil.apiBitsNothing) {
            l.add(a + " api" + servHttpUtil.apiBits2string(allowApi));
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
            subconn = servHttpUtil.string2subconn(negated, cmd);
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
                allowApi = servHttpUtil.apiBitsNothing;
                return false;
            }
            allowApi = servHttpUtil.string2apiBits(cmd);
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

    private final String parseFileName(servHttpConn cn, String s) {
        int i = s.lastIndexOf(".");
        if (i < 0) {
            return s;
        }
        cn.addHdr("Content-Disposition: attachment; filename=\"" + s + "\"");
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

    protected boolean sendOneMarkdown(servHttpConn cn, String s) {
        List<String> l = bits.txt2buf(path + s);
        if (l == null) {
            return true;
        }
        String rsp = servHttp.htmlHead + servHttpUtil.getStyle(cn) + "<title>" + s + "</title></head><body>\n";
        rsp += encMarkDown.md2html(l);
        rsp += "</body></html>\n";
        cn.sendTextHeader("200 ok", "text/html", rsp.getBytes());
        return false;
    }

    protected boolean sendOneApi(servHttpConn cn, String s) {
        if (allowApi == servHttpUtil.apiBitsNothing) {
            return true;
        }
        if ((allowApi & servHttpUtil.apiBitsSomething) == 0) {
            return true;
        }
        cmds cmd = new cmds("api", s);
        cmd.word("/");
        s = cmd.word("/");
        if (debugger.servHttpTraf) {
            logger.debug("api queried cnd=" + s + " prm=" + cmd.getRemaining() + " from " + cn.peer);
        }
        if (((allowApi & servHttpUtil.apiBitsIpinfo) != 0) && s.equals("ipinfo")) {
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
        if (((allowApi & servHttpUtil.apiBitsExec) != 0) && s.equals("exec")) {
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
                exe.privileged = (allowApi & servHttpUtil.apiBitsConfig) != 0;
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
        if (((allowApi & servHttpUtil.apiBitsConfig) != 0) && s.equals("config")) {
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
                return servHttpUtil.sendOneScript(cn, this, scr.getText());
            }
        }
        if (allowMarkdown) {
            if (a.equals(".md")) {
                return sendOneMarkdown(cn, s);
            }
        }
        if (allowScript != 0) {
            if (a.equals(".tcl")) {
                List<String> l = bits.txt2buf(path + s);
                if (l == null) {
                    return true;
                }
                return servHttpUtil.sendOneScript(cn, this, l);
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
        String rsp = servHttp.htmlHead + servHttpUtil.getStyle(cn) + "<title>dirlist</title></head><body>\n";
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
        servHttpUtil.dumpXml(rsp);
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
        if (servHttpUtil.checkNoHeaders(s)) {
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
            if (!servHttpUtil.checkNoHeaders(s)) {
                cn.sendRespHeader("200 ok", siz, cfgInit.findMimeType(a));
            }
        } else {
            cn.addHdr("Content-Range: bytes " + ranB + "-" + ranE + "/" + siz);
            if (!servHttpUtil.checkNoHeaders(s)) {
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

    protected boolean checkUserAuth(String got) {
        if (got == null) {
            return true;
        }
        authResult res = authenticList.authUserPass(decodeAuth(got, true), decodeAuth(got, false));
        if (res.result != authResult.authSuccessful) {
            return true;
        }
        return false;
    }

    protected final static String decodeAuth(String got, boolean usr) {
        if (got == null) {
            return null;
        }
        int i = got.indexOf(" ");
        if (i < 0) {
            return null;
        }
        got = got.substring(i, got.length()).trim();
        got = encBase64.decodeString(got);
        i = got.indexOf(":");
        if (i < 0) {
            return null;
        }
        if (usr) {
            return got.substring(0, i);
        } else {
            return got.substring(i + 1, got.length());
        }
    }

    protected void doTranslate(servHttpConn cn, encUrl srvUrl) {
        if (translate == null) {
            return;
        }
        String a = cfgTrnsltn.doTranslate(translate, cn.gotUrl.toURL(true, true, true, true));
        srvUrl.fromString(a);
    }

    protected void doSubconn(servHttpConn cn, encUrl srvUrl) {
        if ((subconn & 0x1) == 0) {
            srvUrl.filPath = cn.gotUrl.filPath;
        }
        if ((subconn & 0x2) == 0) {
            srvUrl.filName = cn.gotUrl.filName;
        }
        if ((subconn & 0x4) == 0) {
            srvUrl.filExt = cn.gotUrl.filExt;
        }
        if ((subconn & 0x8) == 0) {
            srvUrl.param = cn.gotUrl.param;
        }
        if ((subconn & 0x10) != 0) {
            srvUrl.username = cn.gotUrl.username;
            srvUrl.password = cn.gotUrl.password;
        }
        if ((subconn & 0x20) != 0) {
            srvUrl.server = cn.gotUrl.server;
        }
        if ((subconn & 0x40) != 0) {
            srvUrl.filPath = (srvUrl.filPath + "/" + cn.gotUrl.filPath).replaceAll("//", "/");
        }
    }

    protected final static boolean doConnect(servHttpConn cn) {
        if (!cn.gotCmd.equals("connect")) {
            return false;
        }
        if (cn.gotHost != null) {
            if (cn.gotHost.allowAnyconn != null) {
                servHttpAnyconn ntry = new servHttpAnyconn(cn);
                ntry.doStart(cn.gotHost);
                return true;
            }
        }
        if (cn.lower.proxy == null) {
            cn.sendRespError(405, "not allowed");
            return true;
        }
        cn.gotUrl.fromString("tcp://" + cn.gotUrl.orig);
        addrIP adr = userTerminal.justResolv(cn.gotUrl.server, cn.lower.proxy.prefer);
        if (adr == null) {
            cn.sendRespError(502, "bad gateway");
            return true;
        }
        pipeSide cnn = cn.lower.proxy.doConnect(servGeneric.protoTcp, adr, cn.gotUrl.getPort(cn.lower.srvPort()), "http");
        if (cnn == null) {
            cn.sendRespError(504, "gateway timeout");
            return true;
        }
        cn.sendRespHeader("200 connected", -1, null);
        pipeConnect.connect(cn.pipe, cnn, true);
        cn.pipe = null;
        return true;
    }

    protected final static String webdavProp(String n, File f, boolean typ, boolean len, boolean tag, boolean mod, boolean crt, boolean dsp, boolean cnt, boolean usd, boolean fre) {
        if (!f.exists()) {
            return "";
        }
        boolean dir = f.isDirectory();
        String a = "<D:response>";
        if (dir && (n.length() > 0)) {
            n += "/";
        }
        a += "<D:href>/" + n + "</D:href>";
        a += "<D:propstat><D:prop>";
        if (typ) {
            if (dir) {
                a += "<D:resourcetype><D:collection/></D:resourcetype>";
            } else {
                a += "<D:resourcetype/>";
            }
        }
        if (len) {
            a += "<D:getcontentlength>" + f.length() + "</D:getcontentlength>";
        }
        if (tag) {
            a += "<D:getetag>W/\"" + f.length() + "-" + f.lastModified() + "\"</D:getetag>";
        }
        if (mod) {
            a += "<D:getlastmodified>" + bits.time2str("GMT", f.lastModified(), 4) + "</D:getlastmodified>";
        }
        if (usd) {
            a += "<D:quota-used-bytes>" + (f.getTotalSpace() - f.getFreeSpace()) + "</D:quota-used-bytes>";
        }
        if (fre) {
            a += "<D:quota-available-bytes>" + f.getFreeSpace() + "</D:quota-available-bytes>";
        }
        if (crt) {
            a += "<D:creationdate>" + bits.time2str("Z", f.lastModified(), 3).replaceAll(" ", "T") + "Z</D:creationdate>";
        }
        if (dsp) {
            a += "<D:displayname><![CDATA[" + n + "]]></D:displayname>";
        }
        if (cnt) {
            a += "<D:getcontenttype>" + cfgInit.findMimeType(n) + "</D:getcontenttype>";
        }
        a += "</D:prop><D:status>HTTP/1.1 200 ok</D:status></D:propstat>";
        a += "</D:response>\n";
        return a;
    }

    protected void serveRequest(servHttpConn cn) {
        String pn = cn.gotUrl.toPathName();
        if (cn.gotHost == null) {
            cn.sendRespError(404, "not found");
            return;
        }
        if (accessList != null) {
            if (!accessList.matches(cn.conn)) {
                cn.sendRespError(401, "forbidden");
                return;
            }
        }
        askNum++;
        askTim = bits.getTime();
        if (logging) {
            logger.info(cn.peer + " accessed " + cn.gotUrl.toURL(true, false, true, true));
        }
        if (allowForti != null) {
            servHttpForti ntry = new servHttpForti(cn);
            ntry.serveReq(cn.gotHost);
            return;
        }
        if (allowAnyconn != null) {
            servHttpAnyconn ntry = new servHttpAnyconn(cn);
            ntry.serveReq(cn.gotHost);
            return;
        }
        if (authenticList != null) {
            if (checkUserAuth(cn.gotAuth)) {
                cn.addHdr("WWW-Authenticate: Basic realm=login");
                cn.sendRespError(401, "unauthorized");
                return;
            }
            cn.gotAuth = decodeAuth(cn.gotAuth, true);
        } else {
            cn.gotAuth = null;
        }
        if (streamT != null) {
            reStream(cn);
            return;
        }
        if (multiAccT != null) {
            doMultAcc(cn);
            return;
        }
        if (reconnT != null) {
            doReconn(cn);
            return;
        }
        if (redir != null) {
            doRedir(cn);
            return;
        }
        if (cn.gotCmd.equals("options")) {
            String a = "";
            if (allowUpload) {
                a += ", PUT";
            }
            if (allowWebDav) {
                a += ", PROPFIND";
                if (allowUpload) {
                    a += ", DELETE, COPY, MOVE, PROTPATCH";
                }
                cn.addHdr("DAV: 1");
            }
            if (cn.lower.proxy != null) {
                a += ", CONNECT";
            }
            if (allowSstp != null) {
                a += ", SSTP_DUPLEX_POST";
            }
            cn.addHdr("Allow: GET, POST, HEAD, OPTIONS" + a);
            cn.sendRespHeader("200 ok", 0, null);
            return;
        }
        if (cn.gotCmd.equals("propfind")) {
            if (!cn.gotHost.allowWebDav) {
                cn.sendRespError(405, "not allowed");
                return;
            }
            String a = new String(cn.gotBytes).replaceAll("\r", "").replaceAll("\n", "");
            if (a.length() < 1) {
                a = "<?xml><propfind><allprop>";
            }
            encXml xml = encXml.parseOne(a);
            servHttpUtil.dumpXml(xml);
            String beg = "/?xml/propfind/prop/";
            boolean typ = false;
            boolean len = false;
            boolean tag = false;
            boolean mod = false;
            boolean crt = false;
            boolean dsp = false;
            boolean cnt = false;
            boolean usd = false;
            boolean fre = false;
            for (int i = 0; i < xml.data.size(); i++) {
                a = xml.data.get(i).name.toLowerCase();
                if (a.equals("/?xml/propfind/allprop")) {
                    typ = true;
                    len = true;
                    tag = true;
                    mod = true;
                    crt = true;
                    dsp = true;
                    cnt = true;
                    usd = true;
                    fre = true;
                    continue;
                }
                if (!a.startsWith(beg)) {
                    continue;
                }
                a = a.substring(beg.length(), a.length());
                typ |= a.equals("resourcetype");
                len |= a.equals("getcontentlength");
                tag |= a.equals("getetag");
                mod |= a.equals("getlastmodified");
                crt |= a.equals("creationdate");
                dsp |= a.equals("displayname");
                cnt |= a.equals("getcontenttype");
                usd |= a.equals("quota-used-bytes");
                fre |= a.equals("quota-available-bytes");
            }
            a = "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n";
            a += "<D:multistatus xmlns:D=\"DAV:\">\n";
            if (cn.gotDepth) {
                if ((!pn.endsWith("/")) && (pn.length() > 0)) {
                    pn += "/";
                }
                a += webdavProp(pn, new File(cn.gotHost.path + pn), typ, len, tag, mod, crt, dsp, cnt, usd, fre);
                File[] fl = userFlash.dirList(cn.gotHost.path + pn);
                if (fl == null) {
                    fl = new File[0];
                }
                for (int i = 0; i < fl.length; i++) {
                    a += webdavProp(pn + fl[i].getName(), fl[i], typ, len, tag, mod, crt, dsp, cnt, usd, fre);
                }
            } else {
                a += webdavProp(pn, new File(cn.gotHost.path + pn), typ, len, tag, mod, crt, dsp, cnt, usd, fre);
            }
            a += "</D:multistatus>\n";
            cn.sendRespHeader("207 multi-status", a.length(), "text/xml");
            cn.pipe.strPut(a);
            servHttpUtil.dumpXml(a);
            return;
        }
        if (cn.gotCmd.equals("proppatch")) {
            if ((!cn.gotHost.allowWebDav) || (!cn.gotHost.allowUpload)) {
                cn.sendRespError(405, "not allowed");
                return;
            }
            String a = "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n";
            a += "<D:multistatus xmlns:D=\"DAV:\">\n";
            a += webdavProp(pn, new File(cn.gotHost.path + pn), true, true, true, true, true, true, true, true, true);
            a += "</D:multistatus>\n";
            cn.sendRespHeader("207 multi-status", a.length(), "text/xml");
            cn.pipe.strPut(a);
            servHttpUtil.dumpXml(a);
            return;
        }
        if (cn.gotCmd.equals("mkcol")) {
            if ((!cn.gotHost.allowWebDav) || (!cn.gotHost.allowUpload)) {
                cn.sendRespError(405, "not allowed");
                return;
            }
            if (userFlash.mkdir(cn.gotHost.path + pn)) {
                cn.sendRespHeader("409 conflict", 0, null);
            } else {
                cn.sendRespHeader("201 created", 0, null);
            }
            return;
        }
        if (cn.gotCmd.equals("delete")) {
            if ((!cn.gotHost.allowWebDav) || (!cn.gotHost.allowUpload)) {
                cn.sendRespError(405, "not allowed");
                return;
            }
            if (userFlash.delete(cn.gotHost.path + pn)) {
                cn.sendRespHeader("409 conflict", 0, null);
            } else {
                cn.sendRespHeader("201 created", 0, null);
            }
            return;
        }
        if (cn.gotCmd.equals("copy")) {
            if ((!cn.gotHost.allowWebDav) || (!cn.gotHost.allowUpload) || (cn.gotDstntn == null)) {
                cn.sendRespError(405, "not allowed");
                return;
            }
            encUrl url = encUrl.parseOne(cn.gotDstntn);
            url.normalizePath();
            if (userFlash.copy(cn.gotHost.path + pn, path + url.toPathName(), false)) {
                cn.sendRespHeader("409 conflict", 0, null);
            } else {
                cn.sendRespHeader("201 created", 0, null);
            }
            return;
        }
        if (cn.gotCmd.equals("move")) {
            if ((!cn.gotHost.allowWebDav) || (!cn.gotHost.allowUpload) || (cn.gotDstntn == null)) {
                cn.sendRespError(405, "not allowed");
                return;
            }
            encUrl url = encUrl.parseOne(cn.gotDstntn);
            url.normalizePath();
            if (userFlash.rename(cn.gotHost.path + pn, path + url.toPathName(), false, false)) {
                cn.sendRespHeader("409 conflict", 0, null);
            } else {
                cn.sendRespHeader("201 created", 0, null);
            }
            return;
        }
        if (cn.gotCmd.equals("put")) {
            if (!cn.gotHost.allowUpload) {
                cn.sendRespError(405, "not allowed");
                return;
            }
            if (backupPath != null) {
                String a = backupPath + pn;
                for (int i = backupCount - 1; i >= 0; i--) {
                    userFlash.rename(a + i, a + (i + 1), true, false);
                }
                if (backupCount > 0) {
                    a += "0";
                }
                userFlash.rename(cn.gotHost.path + pn, a, true, false);
            }
            servHttpUtil.updateVisitors(cn, pn);
            bits.byteSave(true, cn.gotBytes, path + pn);
            cn.sendRespError(200, "saved");
            return;
        }
        if (cn.gotCmd.equals("sstp_duplex_post")) {
            if (allowSstp == null) {
                cn.sendRespError(405, "not allowed");
                return;
            }
            cn.addHdr("Content-Length: 18446744073709551615");
            cn.sendRespHeader("200 ok", -1, null);
            servHttpSstp ntry = new servHttpSstp(cn);
            ntry.doStart();
            return;
        }
        if (cn.gotCmd.equals("post")) {
            cn.gotCmd = "get";
        }
        if (cn.gotCmd.equals("head")) {
            cn.gotCmd = "get";
            cn.gotHead = true;
        }
        if (!cn.gotCmd.equals("get")) {
            cn.sendRespError(501, "not implemented");
            return;
        }
        servHttpUtil.updateVisitors(cn, pn);
        boolean b = true;
        if (allowWebSck && (cn.gotWebsock != null)) {
            if (!servHttpUtil.sendOneWebSck(cn, pn)) {
                return;
            }
        }
        if (cn.gotUrl.filPath.startsWith(".api./")) {
            b = sendOneApi(cn, pn);
            if (!b) {
                return;
            }
            cn.sendRespError(404, "bad api");
            return;
        }
        if (cn.gotUrl.toFileName().length() > 0) {
            b = sendOneFile(cn, pn, cn.gotUrl.filExt);
        } else {
            b = sendOneDir(cn, pn);
        }
        if (!b) {
            return;
        }
        cn.sendRespError(404, "not found");
    }

}
