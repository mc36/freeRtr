package org.freertr.serv;

import java.io.File;
import java.net.URI;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.List;
import org.freertr.auth.authGeneric;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgAuther;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgProxy;
import org.freertr.cfg.cfgScrpt;
import org.freertr.cfg.cfgTrnsltn;
import org.freertr.clnt.clntProxy;
import org.freertr.pipe.pipeSide;
import org.freertr.enc.encUrl;
import org.freertr.enc.encXml;
import org.freertr.prt.prtTcp;
import org.freertr.sec.secInfoCfg;
import org.freertr.sec.secInfoCls;
import org.freertr.sec.secInfoUtl;
import org.freertr.sec.secInfoWrk;
import org.freertr.tab.tabRouteIface;
import org.freertr.user.userFlash;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.logger;

/**
 * http virtual host
 *
 * @author matecsaba
 */
public class servHttpHost implements Comparable<servHttpHost> {

    /**
     * parent of server
     */
    public final servHttp parent;

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
     * url to cache to
     */
    public String cacheT;

    /**
     * path to cache to
     */
    public clntProxy cacheP;

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
     * webring list
     */
    public List<String> webring;

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
    public secInfoCfg ipInfo;

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
     * gather info per accesses
     */
    protected secInfoCfg accessControl;

    /**
     * create instance
     *
     * @param l lower
     * @param h host
     */
    public servHttpHost(servHttp l, String h) {
        parent = l;
        host = h;
    }

    public String toString() {
        return "" + host;
    }

    public int compareTo(servHttpHost o) {
        return host.toLowerCase().compareTo(o.host.toLowerCase());
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
        if (webring != null) {
            l.add(a + " webring " + webring.get(0));
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
        if (cacheT != null) {
            l.add(a + " cache " + cacheP.name + " " + cacheT);
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
        secInfoUtl.getConfig(l, ipInfo, a + " ipinfo ");
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
        if (accessControl != null) {
            secInfoUtl.getConfig(l, accessControl, a + " access-");
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
        if (a.equals("webring")) {
            if (negated) {
                webring = null;
                return false;
            }
            a = cmd.word();
            List<String> res = bits.txt2buf(a);
            if (res == null) {
                cmd.error("no such file");
                return false;
            }
            if (res.size() < 1) {
                cmd.error("empty file");
                return false;
            }
            res.add(0, a);
            webring = res;
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
        if (a.equals("cache")) {
            if (negated) {
                cacheP = null;
                cacheT = null;
                return false;
            }
            cfgProxy prx = cfgAll.proxyFind(cmd.word(), false);
            if (prx == null) {
                cmd.error("no such proxy");
                return false;
            }
            cacheP = prx.proxy;
            cacheT = cmd.word();
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
        if (a.equals("ipinfo")) {
            ipInfo = secInfoUtl.doCfgStr(ipInfo, cmd, negated);
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
        if (a.startsWith("access-")) {
            a = a.substring(7, a.length());
            a += " " + cmd.getRemaining();
            a = a.trim();
            cmd = new cmds("info", a);
            accessControl = secInfoUtl.doCfgStr(accessControl, cmd, negated);
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
     * send one file
     *
     * @param cn connection
     * @param s filename
     * @param a extension
     * @return false on success, true on error
     */
    protected boolean sendOneFile(servHttpConn cn, String s, String a) {
        if (searchScript != null) {
            cfgScrpt scr = cfgAll.scrptFind(searchScript + s, false);
            if (scr != null) {
                return servHttpUtil.sendOneScript(cn, this, scr.getText());
            }
        }
        if (allowMarkdown) {
            if (a.equals(".md")) {
                return servHttpUtil.sendOneMarkdown(cn, s);
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
                return servHttpUtil.sendOneClass(cn, s);
            }
        }
        if (allowImgMap) {
            if (a.equals(".imgmap")) {
                return servHttpUtil.sendOneImgMap(cn, s);
            }
        }
        if (allowMediaStrm) {
            if (a.startsWith(".stream-")) {
                return servHttpUtil.sendOneStream(cn, s, "." + a.substring(8, a.length()));
            }
        }
        if (allowMediaStrm) {
            if (a.startsWith(".motion-")) {
                return servHttpUtil.sendOneMotion(cn, path + s, "." + a.substring(8, a.length()));
            }
        }
        return servHttpUtil.sendBinFile(cn, path + s, a, speedLimit);
    }

    /**
     * serve one request
     *
     * @param cn connection
     */
    protected void serveRequest(servHttpConn cn) {
        String pn = cn.gotUrl.toPathName();
        if (cn.gotHost == null) {
            cn.sendRespError(404, "not found");
            return;
        }
        if (accessControl != null) {
            secInfoCls cls = new secInfoCls(null, null, null, cn.lower.srvVrf.getFwd(cn.peer), cn.peer, prtTcp.protoNum, cn.conn.iface.addr);
            secInfoWrk wrk = new secInfoWrk(accessControl, cls);
            wrk.doWork(false);
            if (wrk.need2drop()) {
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
            if (servHttpUtil.checkUserAuth(cn)) {
                cn.addHdr("WWW-Authenticate: Basic realm=login");
                cn.sendRespError(401, "unauthorized");
                return;
            }
            cn.gotAuth = servHttpUtil.decodeAuth(cn.gotAuth, true);
        } else {
            cn.gotAuth = null;
        }
        if (streamT != null) {
            servHttpUtil.reStream(cn);
            return;
        }
        if (multiAccT != null) {
            servHttpUtil.doMultAcc(cn);
            return;
        }
        if (reconnT != null) {
            servHttpUtil.doReconn(cn);
            return;
        }
        if (cacheT != null) {
            servHttpUtil.doCache(cn);
            return;
        }
        if (redir != null) {
            servHttpUtil.doRedir(cn);
            return;
        }
        if (webring != null) {
            servHttpUtil.doWebring(cn);
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
                a += servHttpUtil.webdavProp(pn, new File(cn.gotHost.path + pn), typ, len, tag, mod, crt, dsp, cnt, usd, fre);
                File[] fl = userFlash.dirList(cn.gotHost.path + pn);
                if (fl == null) {
                    fl = new File[0];
                }
                for (int i = 0; i < fl.length; i++) {
                    a += servHttpUtil.webdavProp(pn + fl[i].getName(), fl[i], typ, len, tag, mod, crt, dsp, cnt, usd, fre);
                }
            } else {
                a += servHttpUtil.webdavProp(pn, new File(cn.gotHost.path + pn), typ, len, tag, mod, crt, dsp, cnt, usd, fre);
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
            a += servHttpUtil.webdavProp(pn, new File(cn.gotHost.path + pn), true, true, true, true, true, true, true, true, true);
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
        if (allowWebSck && (cn.gotWebsock != null)) {
            if (!servHttpUtil.sendOneWebSck(cn, pn)) {
                return;
            }
            return;
        }
        if (cn.gotUrl.filPath.startsWith(".api./")) {
            if (!servHttpUtil.sendOneApi(cn, pn)) {
                return;
            }
            cn.sendRespError(404, "bad api");
            return;
        }
        boolean b = true;
        if (cn.gotUrl.toFileName().length() > 0) {
            b = sendOneFile(cn, pn, cn.gotUrl.filExt);
        } else {
            b = servHttpUtil.sendOneDir(cn, pn);
        }
        if (!b) {
            return;
        }
        cn.sendRespError(404, "not found");
    }

}
