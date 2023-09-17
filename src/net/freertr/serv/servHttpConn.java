package net.freertr.serv;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.zip.Deflater;
import net.freertr.addr.addrIP;
import net.freertr.auth.authResult;
import net.freertr.cfg.cfgInit;
import net.freertr.enc.encBase64;
import net.freertr.pipe.pipeConnect;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGenConn;
import net.freertr.sec.secHttp2;
import net.freertr.user.userFlash;
import net.freertr.user.userTerminal;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.debugger;
import net.freertr.enc.encXml;
import net.freertr.enc.encUrl;
import net.freertr.util.logger;
import net.freertr.util.version;

/**
 * http connection
 *
 * @author matecsaba
 */
public class servHttpConn implements Runnable {

    /**
     * parent
     */
    protected servHttp lower;

    /**
     * pipe
     */
    protected pipeSide pipe;

    /**
     * address
     */
    protected addrIP peer;

    /**
     * connection
     */
    protected prtGenConn conn;

    /**
     * got command
     */
    protected String gotCmd;

    /**
     * got head request
     */
    protected boolean gotHead;

    /**
     * got url
     */
    protected encUrl gotUrl;

    /**
     * got authentication
     */
    protected String gotAuth;

    /**
     * got host
     */
    protected servHttpHost gotHost;

    /**
     * got protocol
     */
    protected int gotVer;

    /**
     * got keepalive
     */
    protected boolean gotKeep;

    /**
     * got depth
     */
    protected boolean gotDepth;

    /**
     * got compression, 1=deflate, 2=gzip
     */
    protected int gotCompr;

    /**
     * got destination
     */
    protected String gotDstntn;

    /**
     * got cookies
     */
    protected List<String> gotCook;

    /**
     * got content
     */
    protected byte[] gotBytes;

    /**
     * got agent
     */
    protected String gotAgent;

    /**
     * got referrer
     */
    protected String gotReferer;

    /**
     * got websocket
     */
    protected String gotWebsock;

    /**
     * got range
     */
    protected String gotRange;

    /**
     * headers to send
     */
    private List<String> headers;

    /**
     * through tls port
     */
    private boolean secured;

    /**
     * create instance
     *
     * @param parent parent
     * @param stream pipeline
     * @param id connection
     */
    public servHttpConn(servHttp parent, pipeSide stream, prtGenConn id) {
        lower = parent;
        pipe = stream;
        pipe.lineRx = pipeSide.modTyp.modeCRtryLF;
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        peer = new addrIP();
        peer.setAddr(id.peerAddr);
        conn = id;
        secured = id.portLoc == lower.secondPort;
        new Thread(this).start();
    }

    protected void sendLn(String a) {
        if (debugger.servHttpTraf) {
            logger.debug("tx '" + a + "'");
        }
        pipe.linePut(a);
    }

    protected void addHdr(String s) {
        headers.add(s);
    }

    protected void sendRespHeader(String head, long size, String type) {
        if (head != null) {
            sendLn("HTTP/" + (gotVer / 10) + "." + (gotVer % 10) + " " + head);
        }
        sendLn("Server: " + version.usrAgnt);
        if (type != null) {
            sendLn("Content-Type: " + type);
        }
        if (!gotKeep || lower.singleRequest) {
            sendLn("Connection: Close");
        } else {
            sendLn("Connection: Keep-Alive");
            sendLn("Keep-Alive: TimeOut=60, Max=25");
            if (size < 0) {
                size = 0;
            }
        }
        if (size >= 0) {
            sendLn("Content-Length: " + size);
        }
        for (int i = 0; i < headers.size(); i++) {
            sendLn(headers.get(i));
        }
        sendLn("");
    }

    protected void sendTextHeader(String head, String type, byte[] buf1) {
        if (gotHead) {
            sendRespHeader(head, buf1.length, type);
            return;
        }
        if (gotCompr == 0) {
            sendRespHeader(head, buf1.length, type);
            pipe.morePut(buf1, 0, buf1.length);
            return;
        }
        Deflater cmp;
        String enc;
        byte[] buf3;
        byte[] buf4;
        switch (gotCompr) {
            case 1:
                cmp = new Deflater();
                enc = "deflate";
                buf3 = new byte[0];
                buf4 = new byte[0];
                break;
            case 2:
                cmp = new Deflater(Deflater.DEFAULT_COMPRESSION, true);
                enc = "gzip";
                buf3 = servHttp.getGzipHdr();
                buf4 = servHttp.getGzipTrl(buf1);
                break;
            default:
                sendRespHeader(head, buf1.length, type);
                pipe.morePut(buf1, 0, buf1.length);
                return;
        }
        cmp.setInput(buf1);
        cmp.finish();
        byte[] buf2 = new byte[buf1.length];
        int i = cmp.deflate(buf2);
        if (i >= buf2.length) {
            sendRespHeader(head, buf1.length, type);
            pipe.morePut(buf1, 0, buf1.length);
            return;
        }
        headers.add("Content-Encoding: " + enc);
        sendRespHeader(head, buf3.length + i + buf4.length, type);
        pipe.morePut(buf3, 0, buf3.length);
        pipe.morePut(buf2, 0, i);
        pipe.morePut(buf4, 0, buf4.length);
    }

    private String getStyle() {
        if (gotHost == null) {
            return "";
        }
        return gotHost.getStyle();
    }

    protected void sendRespError(int code, String text) {
        gotKeep = false;
        String s;
        if (lower.error == null) {
            s = servHttp.htmlHead + getStyle() + "<title>error</title></head><body>error: " + text + "</body></html>";
        } else {
            s = "" + lower.error;
            s = s.replaceAll("<errorcode>", "" + code);
            s = s.replaceAll("<errortext>", "" + text);
        }
        sendRespHeader(code + " " + text, s.length(), "text/html");
        if (gotHead) {
            return;
        }
        pipe.strPut(s);
    }

    protected void sendFoundAt(String where) {
        gotKeep = false;
        String s = servHttp.htmlHead + getStyle() + "<title>moved</title></head><body>moved to <a href=\"" + where + "\">" + where + "</a>. you will be redirected.</body></html>\n";
        headers.add("Location: " + where);
        sendRespHeader("301 moved", s.length(), "text/html");
        if (gotHead) {
            return;
        }
        pipe.strPut(s);
    }

    private String webdavProp(String n, File f, boolean typ, boolean len, boolean tag, boolean mod, boolean crt, boolean dsp, boolean cnt, boolean usd, boolean fre) {
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

    private boolean readRequest() {
        gotUrl = encUrl.parseOne("null://");
        gotVer = 0;
        gotHead = false;
        gotKeep = false;
        gotDepth = false;
        gotCompr = 0;
        int gotSize = 0;
        String gotType = "";
        if (pipe == null) {
            gotCmd = "";
        } else {
            gotCmd = pipe.lineGet(1);
        }
        String gotExpect = null;
        String gotUpgrade = null;
        gotAuth = null;
        gotCook = new ArrayList<String>();
        headers = new ArrayList<String>();
        gotBytes = null;
        gotDstntn = null;
        gotAgent = "";
        gotRange = null;
        gotReferer = "";
        gotWebsock = null;
        if (gotCmd.length() < 1) {
            return true;
        }
        if (debugger.servHttpTraf) {
            logger.debug("rx '" + gotCmd + "'");
        }
        if (gotCmd.equals(secHttp2.magicCmd)) {
            gotCmd = "";
            secHttp2 ht2 = new secHttp2(pipe, new pipeLine(lower.bufSiz, false));
            if (ht2.startServer(true)) {
                return true;
            }
            pipe = ht2.getPipe();
            gotCmd = pipe.lineGet(1);
            if (gotCmd.length() < 1) {
                return true;
            }
            if (debugger.servHttpTraf) {
                logger.debug("rx '" + gotCmd + "'");
            }
        }
        int i = gotCmd.toLowerCase().lastIndexOf(" http/");
        if (i > 0) {
            String s = gotCmd.substring(i + 6, gotCmd.length());
            gotCmd = gotCmd.substring(0, i);
            i = s.indexOf(".");
            if (i < 0) {
                gotVer = bits.str2num(s) * 10;
            } else {
                gotVer = bits.str2num(s.substring(0, i) + s.substring(i + 1, s.length()));
            }
        }
        if ((gotVer < 10) || (gotVer > 11)) {
            gotVer = 10;
        }
        i = gotCmd.indexOf(" ");
        if (i > 0) {
            String s = gotCmd.substring(i + 1, gotCmd.length());
            gotCmd = gotCmd.substring(0, i);
            gotUrl.fromString(s);
        }
        gotCmd = gotCmd.trim().toLowerCase();
        for (;;) {
            String s = pipe.lineGet(1);
            i = s.indexOf(":");
            String a;
            if (i < 0) {
                a = s;
                s = "";
            } else {
                a = s.substring(0, i);
                s = s.substring(i + 1, s.length());
            }
            a = a.trim().toLowerCase();
            s = s.trim();
            if (debugger.servHttpTraf) {
                logger.debug("rx " + a + ":'" + s + "'");
            }
            if (a.length() < 1) {
                break;
            }
            if (a.equals("connection")) {
                s = s.toLowerCase();
                gotKeep |= s.indexOf("keep") >= 0;
                gotKeep |= s.indexOf("te") >= 0;
                continue;
            }
            if (a.equals("depth")) {
                gotDepth |= s.indexOf("1") >= 0;
                continue;
            }
            if (a.equals("accept-encoding")) {
                if (s.indexOf("deflate") >= 0) {
                    gotCompr = 1;
                }
                if (s.indexOf("gzip") >= 0) {
                    gotCompr = 2;
                }
                continue;
            }
            if (a.equals("content-length")) {
                gotSize = bits.str2num(s);
                continue;
            }
            if (a.equals("content-type")) {
                gotType = s;
                continue;
            }
            if (a.equals("destination")) {
                gotDstntn = s;
                continue;
            }
            if (a.equals("range")) {
                gotRange = s;
                continue;
            }
            if (a.equals("x-forwarded-for") || a.equals("x-client-ip") || a.equals("true-client-ip")) {
                i = s.indexOf(",");
                if (i >= 0) {
                    s = s.substring(0, i);
                }
                addrIP adr = new addrIP();
                if (adr.fromString(s)) {
                    continue;
                }
                peer.setAddr(adr);
                continue;
            }
            if (a.equals("user-agent")) {
                gotAgent = s;
                continue;
            }
            if (a.equals("expect")) {
                gotExpect = s;
                continue;
            }
            if (a.equals("upgrade")) {
                gotUpgrade = s;
                continue;
            }
            if (a.equals("sec-websocket-key")) {
                gotWebsock = s;
                continue;
            }
            if (a.equals("referer")) {
                gotReferer = s;
                continue;
            }
            if (a.equals("authorization")) {
                gotAuth = s;
                continue;
            }
            if (a.equals("cookie")) {
                for (;;) {
                    s = s.trim();
                    i = s.indexOf(";");
                    if (i < 0) {
                        break;
                    }
                    gotCook.add(s.substring(0, i).trim());
                    s = s.substring(i + 1, s.length()).trim();
                }
                if (s.length() > 0) {
                    gotCook.add(s);
                }
                continue;
            }
            if (a.equals("host")) {
                encUrl srv = encUrl.parseOne("http://" + s + "/");
                gotUrl.server = srv.server;
                if (gotUrl.port < 0) {
                    gotUrl.port = srv.port;
                }
                if (gotUrl.proto.length() < 1) {
                    gotUrl.proto = srv.proto;
                }
                continue;
            }
        }
        if (gotExpect != null) {
            sendRespHeader(gotExpect.replaceAll("-", " "), 0, "text/plain");
        }
        if (gotSize > 0) {
            gotBytes = new byte[gotSize];
            if (pipe.moreGet(gotBytes, 0, gotBytes.length) < gotSize) {
                return true;
            }
            if (debugger.servHttpTraf) {
                logger.debug("readed " + gotBytes.length + " bytes");
                if (debugger.clntHttpTraf) {
                    logger.debug(bits.byteDump(gotBytes, 0, -1));
                }
            }
        } else {
            gotBytes = new byte[0];
        }
        if (gotType.equals("application/x-www-form-urlencoded")) {
            String s = new String(gotBytes);
            gotBytes = new byte[0];
            encUrl srv = encUrl.parseOne("http://x/y?" + s);
            gotUrl.param.addAll(srv.param);
        }
        gotUrl.normalizePath();
        if (gotUpgrade == null) {
            return false;
        }
        if (gotUpgrade.toLowerCase().startsWith("h2")) {
            headers.add("Upgrade: " + gotUpgrade);
            headers.add("Connection: Upgrade");
            sendRespHeader("101 switch protocol", -1, null);
            secHttp2 ht2 = new secHttp2(pipe, new pipeLine(lower.bufSiz, false));
            if (ht2.startServer(false)) {
                return true;
            }
            pipe = ht2.getPipe();
            return false;
        }
        if (!gotUpgrade.toLowerCase().startsWith("tls/")) {
            return false;
        }
        if (lower.noneSecKeys()) {
            return false;
        }
        headers.add("Upgrade: " + gotUpgrade + ", HTTP/1.1");
        sendRespHeader("101 switch protocol", -1, null);
        headers.clear();
        pipeSide res = lower.negoSecSess(pipe, servGeneric.protoTls, new pipeLine(lower.bufSiz, false), null);
        if (res == null) {
            return true;
        }
        res.lineRx = pipeSide.modTyp.modeCRtryLF;
        res.lineTx = pipeSide.modTyp.modeCRLF;
        pipe = res;
        secured = true;
        return false;
    }

    private void serveRequest() {
        gotHost = lower.findHost(gotUrl.server);
        if (gotCmd.equals("connect")) {
            if (gotHost != null) {
                if (gotHost.allowAnyconn != null) {
                    servHttpAnyconn ntry = new servHttpAnyconn(this, gotHost);
                    return;
                }
            }
            if (lower.proxy == null) {
                sendRespError(405, "not allowed");
                return;
            }
            gotUrl.fromString("tcp://" + gotUrl.orig);
            addrIP adr = userTerminal.justResolv(gotUrl.server, lower.proxy.prefer);
            if (adr == null) {
                sendRespError(502, "bad gateway");
                return;
            }
            pipeSide cnn = lower.proxy.doConnect(servGeneric.protoTcp, adr, gotUrl.getPort(lower.srvPort()), "http");
            if (cnn == null) {
                sendRespError(504, "gateway timeout");
                return;
            }
            sendRespHeader("200 connected", -1, null);
            pipeConnect.connect(pipe, cnn, true);
            pipe = null;
            return;
        }
        String pn = gotUrl.toPathName();
        if (gotHost == null) {
            sendRespError(404, "not found");
            return;
        }
        if (gotHost.accessList != null) {
            if (!gotHost.accessList.matches(conn)) {
                sendRespError(401, "forbidden");
                return;
            }
        }
        gotHost.askNum++;
        gotHost.askTim = bits.getTime();
        if (gotHost.logging) {
            logger.info(peer + " accessed " + gotUrl.toURL(true, false, true, true));
        }
        if (gotHost.allowForti != null) {
            if (gotUrl.toPathName().equals("remote/logincheck")) {
                headers.add("Set-Cookie: SVPNCOOKIE=" + encBase64.encodeString(gotUrl.getParam("username") + "|" + gotUrl.getParam("credential")) + "; path=/; secure; httponly");
                sendTextHeader("200 OK", "text/html", "<html></html>".getBytes());
                return;
            }
            if (gotUrl.toPathName().equals("remote/index")) {
                sendTextHeader("200 OK", "text/html", "<html></html>".getBytes());
                return;
            }
            if (gotUrl.toPathName().equals("remote/fortisslvpn")) {
                sendTextHeader("200 OK", "text/html", "<html></html>".getBytes());
                return;
            }
            if (!gotUrl.toPathName().equals("remote/fortisslvpn_xml")) {
                sendRespError(404, "not found");
                return;
            }
            if (gotCook.size() < 1) {
                sendRespError(401, "unauthorized");
                return;
            }
            String a = gotCook.get(0);
            int i = a.indexOf("=");
            if (i < 0) {
                sendRespError(401, "unauthorized");
                return;
            }
            a = encBase64.decodeString(a.substring(i + 1, a.length()));
            if (a == null) {
                sendRespError(401, "unauthorized");
                return;
            }
            i = a.indexOf("|");
            if (i < 0) {
                sendRespError(401, "unauthorized");
                return;
            }
            authResult res = gotHost.authenticList.authUserPass(a.substring(0, i), a.substring(i + 1, a.length()));
            if (res.result != authResult.authSuccessful) {
                sendRespError(401, "unauthorized");
                return;
            }
            sendTextHeader("200 OK", "text/html", "<html></html>".getBytes());
            for (;;) {
                a = pipe.lineGet(1);
                if (debugger.servHttpTraf) {
                    logger.debug("rx '" + a + "'");
                }
                if (a.length() < 1) {
                    break;
                }
            }
            servHttpForti ntry = new servHttpForti(pipe);
            ntry.ifc = gotHost.allowForti.cloneStart(ntry);
            ntry.doStart();
            gotKeep = false;
            pipe = null;
            return;
        }
        if (gotHost.allowAnyconn != null) {
            gotUrl.port = lower.srvPort;
            if (lower.secProto != 0) {
                gotUrl.proto = "https";
            } else {
                gotUrl.proto = "http";
            }
            headers.add("Set-Cookie: webvpncontext=00@defctx; path=/; Secure");
            if (gotUrl.toPathName().length() <= 0) {
                headers.add("Location: " + gotUrl.toURL(true, false, false, false) + "webvpn.html");
                sendRespHeader("303 see other", -1, "text/html");
                return;
            }
            if (gotUrl.toPathName().equals("1/index.html")) {
                sendTextHeader("200 ok", "text/html", "<html></html>".getBytes());
                return;
            }
            if (gotUrl.toPathName().equals("1/Linux")) {
                sendTextHeader("200 ok", "text/html", "<html></html>".getBytes());
                return;
            }
            if (gotUrl.toPathName().equals("1/Linux_64")) {
                sendTextHeader("200 ok", "text/html", "<html></html>".getBytes());
                return;
            }
            if (gotUrl.toPathName().equals("1/Windows")) {
                sendTextHeader("200 ok", "text/html", "<html></html>".getBytes());
                return;
            }
            if (gotUrl.toPathName().equals("1/Darwin_i386")) {
                sendTextHeader("200 ok", "text/html", "<html></html>".getBytes());
                return;
            }
            if (gotUrl.toPathName().equals("1/VPNManifest.xml")) {
                sendTextHeader("200 ok", "text/xml", (encXml.header + "\n<vpn rev=\"1.0\">\n</vpn>\n").getBytes());
                return;
            }
            if (gotUrl.toPathName().equals("1/binaries/update.txt")) {
                sendTextHeader("200 ok", "text/html", "0,0,0000\\n".getBytes());
                return;
            }
            if (gotUrl.toPathName().equals("logout")) {
                sendTextHeader("200 ok", "text/html", "<html></html>".getBytes());
                return;
            }
            if (gotUrl.toPathName().startsWith(" CSCOT /")) {
                sendTextHeader("200 ok", "text/html", "<html></html>".getBytes());
                return;
            }
            if (!gotUrl.toPathName().equals("webvpn.html")) {
                sendRespError(404, "not found");
                return;
            }
            String s = new String(gotBytes);
            gotBytes = new byte[0];
            encUrl srv = encUrl.parseOne("http://x/y?" + s);
            gotUrl.param.addAll(srv.param);
            gotUrl.username = gotUrl.getParam("username");
            gotUrl.password = gotUrl.getParam("password");
            if (gotUrl.username == null) {
                gotUrl.username = "";
            }
            if (gotUrl.password == null) {
                gotUrl.password = "";
            }
            authResult res = gotHost.authenticList.authUserPass(gotUrl.username, gotUrl.password);
            if (res.result != authResult.authSuccessful) {
                headers.add("X-Transcend-Version: 1");
                sendTextHeader("200 ok", "text/xml", (encXml.header + "\n<auth id=\"main\"><title>login</title><message>enter username and password</message><form method=\"post\" action=\"webvpn.html\"><input type=\"text\" label=\"username:\" name=\"username\" value=\"\" /><input type=\"password\" label=\"password:\" name=\"password\" value=\"\" /><input type=\"submit\" name=\"login\" value=\"login\" /></form></auth>").getBytes());
                return;
            }
            headers.add("Set-Cookie: webvpn=00@0168430307@00071@3702439125@3326207229@defctx; path=/; Secure");
            headers.add("Set-Cookie: webvpnc=bu:0/&p:t&iu:1/&sh:%s; path=/; Secure");
            sendTextHeader("200 ok", "text/xml", (encXml.header + "\n<auth id=\"success\"><title>vpn</title><message>success</message><success/></auth>").getBytes());
            return;
        }
        if (gotHost.authenticList != null) {
            if (gotHost.checkUserAuth(gotAuth)) {
                headers.add("WWW-Authenticate: Basic realm=login");
                sendRespError(401, "unauthorized");
                return;
            }
            gotAuth = servHttpHost.decodeAuth(gotAuth, true);
        } else {
            gotAuth = null;
        }
        if (gotHost.streamT != null) {
            gotKeep = false;
            sendRespHeader("200 restreaming", -1, gotHost.streamM);
            gotHost.streamC.add(pipe);
            pipe = null;
            gotHost.reStream();
            return;
        }
        if (gotHost.multiAccT != null) {
            cmds cmd = new cmds("hst", gotHost.multiAccT);
            List<encUrl> urls = new ArrayList<encUrl>();
            for (;;) {
                String a = cmd.word();
                if (a.length() < 1) {
                    break;
                }
                encUrl srvUrl = encUrl.parseOne(a);
                gotHost.doTranslate(this, srvUrl);
                gotHost.doSubconn(this, srvUrl);
                urls.add(srvUrl);
            }
            addrIP[] adrs = new addrIP[urls.size()];
            for (int i = 0; i < adrs.length; i++) {
                adrs[i] = userTerminal.justResolv(urls.get(i).server, gotHost.multiAccP.prefer);
            }
            pipeSide[] cons = new pipeSide[adrs.length];
            for (int i = 0; i < adrs.length; i++) {
                cons[i] = null;
                if (adrs[i] == null) {
                    continue;
                }
                cons[i] = gotHost.multiAccP.doConnect(servGeneric.protoTcp, adrs[i], urls.get(i).getPort(lower.srvPort()), "http");
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
                fin.linePut(gotCmd.toUpperCase() + " " + urls.get(i).toURL(false, false, true, true) + " HTTP/1.1");
                fin.linePut("User-Agent: " + gotAgent + " [" + version.usrAgnt + " by " + peer + "]");
                fin.linePut("X-Forwarded-For: " + peer);
                fin.linePut("Referer: " + gotReferer);
                fin.linePut("Host: " + gotUrl.server);
                fin.linePut("Accept: */*");
                fin.linePut("Accept-Language: *");
                fin.linePut("Accept-Charset: *");
                fin.linePut("Accept-Encoding: identity");
                if (gotRange != null) {
                    fin.linePut("Range: " + gotRange);
                }
                fin.linePut("Connection: Close");
                fin.linePut("");
                fin.lineTx = old;
            }
            if (fin == null) {
                sendRespError(504, "gateways timeout");
                return;
            }
            pipeConnect.connect(pipe, fin, true);
            pipe = null;
            return;
        }
        if (gotHost.reconnT != null) {
            encUrl srvUrl = encUrl.parseOne(gotHost.reconnT);
            gotHost.doTranslate(this, srvUrl);
            gotHost.doSubconn(this, srvUrl);
            addrIP adr = userTerminal.justResolv(srvUrl.server, gotHost.reconnP.prefer);
            if (adr == null) {
                sendRespError(502, "bad gateway");
                return;
            }
            pipeSide cnn = gotHost.reconnP.doConnect(servGeneric.protoTcp, adr, srvUrl.getPort(lower.srvPort()), "http");
            if (cnn == null) {
                sendRespError(504, "gateway timeout");
                return;
            }
            if (debugger.servHttpTraf) {
                logger.debug("reconnect " + srvUrl.toURL(true, false, true, true));
            }
            pipeSide.modTyp old = cnn.lineTx;
            cnn.lineTx = pipeSide.modTyp.modeCRLF;
            cnn.linePut(gotCmd.toUpperCase() + " " + srvUrl.toURL(false, false, true, true) + " HTTP/1.1");
            cnn.linePut("User-Agent: " + gotAgent + " [" + version.usrAgnt + " by " + peer + "]");
            cnn.linePut("X-Forwarded-For: " + peer);
            cnn.linePut("Referer: " + gotReferer);
            cnn.linePut("Host: " + srvUrl.server);
            cnn.linePut("Accept: */*");
            cnn.linePut("Accept-Language: *");
            cnn.linePut("Accept-Charset: *");
            cnn.linePut("Accept-Encoding: identity");
            if (gotRange != null) {
                cnn.linePut("Range: " + gotRange);
            }
            cnn.linePut("Connection: Close");
            cnn.linePut("");
            cnn.lineTx = old;
            pipeConnect.connect(pipe, cnn, true);
            pipe = null;
            return;
        }
        if (gotHost.redir != null) {
            encUrl srvUrl = encUrl.parseOne(gotHost.redir);
            gotHost.doTranslate(this, srvUrl);
            gotHost.doSubconn(this, srvUrl);
            sendFoundAt(srvUrl.toURL(true, true, true, false));
            return;
        }
        if (gotCmd.equals("options")) {
            String a = "";
            if (gotHost.allowUpload) {
                a += ", PUT";
            }
            if (gotHost.allowWebDav) {
                a += ", PROPFIND";
                if (gotHost.allowUpload) {
                    a += ", DELETE, COPY, MOVE, PROTPATCH";
                }
                headers.add("DAV: 1");
            }
            if (lower.proxy != null) {
                a += ", CONNECT";
            }
            if (gotHost.allowSstp != null) {
                a += ", SSTP_DUPLEX_POST";
            }
            headers.add("Allow: GET, POST, HEAD, OPTIONS" + a);
            sendRespHeader("200 ok", 0, null);
            return;
        }
        if (gotCmd.equals("propfind")) {
            if (!gotHost.allowWebDav) {
                sendRespError(405, "not allowed");
                return;
            }
            String a = new String(gotBytes).replaceAll("\r", "").replaceAll("\n", "");
            if (a.length() < 1) {
                a = "<?xml><propfind><allprop>";
            }
            encXml xml = encXml.parseOne(a);
            servHttpHost.dumpXml(xml);
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
            if (gotDepth) {
                if ((!pn.endsWith("/")) && (pn.length() > 0)) {
                    pn += "/";
                }
                a += webdavProp(pn, new File(gotHost.path + pn), typ, len, tag, mod, crt, dsp, cnt, usd, fre);
                File[] fl = userFlash.dirList(gotHost.path + pn);
                if (fl == null) {
                    fl = new File[0];
                }
                for (int i = 0; i < fl.length; i++) {
                    a += webdavProp(pn + fl[i].getName(), fl[i], typ, len, tag, mod, crt, dsp, cnt, usd, fre);
                }
            } else {
                a += webdavProp(pn, new File(gotHost.path + pn), typ, len, tag, mod, crt, dsp, cnt, usd, fre);
            }
            a += "</D:multistatus>\n";
            sendRespHeader("207 multi-status", a.length(), "text/xml");
            pipe.strPut(a);
            servHttpHost.dumpXml(a);
            return;
        }
        if (gotCmd.equals("proppatch")) {
            if ((!gotHost.allowWebDav) || (!gotHost.allowUpload)) {
                sendRespError(405, "not allowed");
                return;
            }
            String a = "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n";
            a += "<D:multistatus xmlns:D=\"DAV:\">\n";
            a += webdavProp(pn, new File(gotHost.path + pn), true, true, true, true, true, true, true, true, true);
            a += "</D:multistatus>\n";
            sendRespHeader("207 multi-status", a.length(), "text/xml");
            pipe.strPut(a);
            servHttpHost.dumpXml(a);
            return;
        }
        if (gotCmd.equals("mkcol")) {
            if ((!gotHost.allowWebDav) || (!gotHost.allowUpload)) {
                sendRespError(405, "not allowed");
                return;
            }
            if (userFlash.mkdir(gotHost.path + pn)) {
                sendRespHeader("409 conflict", 0, null);
            } else {
                sendRespHeader("201 created", 0, null);
            }
            return;
        }
        if (gotCmd.equals("delete")) {
            if ((!gotHost.allowWebDav) || (!gotHost.allowUpload)) {
                sendRespError(405, "not allowed");
                return;
            }
            if (userFlash.delete(gotHost.path + pn)) {
                sendRespHeader("409 conflict", 0, null);
            } else {
                sendRespHeader("201 created", 0, null);
            }
            return;
        }
        if (gotCmd.equals("copy")) {
            if ((!gotHost.allowWebDav) || (!gotHost.allowUpload) || (gotDstntn == null)) {
                sendRespError(405, "not allowed");
                return;
            }
            encUrl url = encUrl.parseOne(gotDstntn);
            url.normalizePath();
            if (userFlash.copy(gotHost.path + pn, gotHost.path + url.toPathName(), false)) {
                sendRespHeader("409 conflict", 0, null);
            } else {
                sendRespHeader("201 created", 0, null);
            }
            return;
        }
        if (gotCmd.equals("move")) {
            if ((!gotHost.allowWebDav) || (!gotHost.allowUpload) || (gotDstntn == null)) {
                sendRespError(405, "not allowed");
                return;
            }
            encUrl url = encUrl.parseOne(gotDstntn);
            url.normalizePath();
            if (userFlash.rename(gotHost.path + pn, gotHost.path + url.toPathName(), false, false)) {
                sendRespHeader("409 conflict", 0, null);
            } else {
                sendRespHeader("201 created", 0, null);
            }
            return;
        }
        if (gotCmd.equals("put")) {
            if (!gotHost.allowUpload) {
                sendRespError(405, "not allowed");
                return;
            }
            if (gotHost.backupPath != null) {
                String a = gotHost.backupPath + pn;
                for (int i = gotHost.backupCount - 1; i >= 0; i--) {
                    userFlash.rename(a + i, a + (i + 1), true, false);
                }
                if (gotHost.backupCount > 0) {
                    a += "0";
                }
                userFlash.rename(gotHost.path + pn, a, true, false);
            }
            gotHost.updateVisitors(this, pn);
            bits.byteSave(true, gotBytes, gotHost.path + pn);
            sendRespError(200, "saved");
            return;
        }
        if (gotCmd.equals("sstp_duplex_post")) {
            if (gotHost.allowSstp == null) {
                sendRespError(405, "not allowed");
                return;
            }
            headers.add("Content-Length: 18446744073709551615");
            sendRespHeader("200 ok", -1, null);
            new servHttpSstp(pipe, this);
            gotKeep = false;
            pipe = null;
            return;
        }
        if (gotCmd.equals("post")) {
            gotCmd = "get";
        }
        if (gotCmd.equals("head")) {
            gotCmd = "get";
            gotHead = true;
        }
        if (!gotCmd.equals("get")) {
            sendRespError(501, "not implemented");
            return;
        }
        gotHost.updateVisitors(this, pn);
        boolean b = true;
        if (gotHost.allowWebSck) {
            if ((gotWebsock != null)) {
                if (!gotHost.sendOneWebSck(this, pn)) {
                    return;
                }
            }
        }
        if (gotUrl.filPath.startsWith(".api./")) {
            b = gotHost.sendOneApi(this, pn);
            if (!b) {
                return;
            }
            sendRespError(404, "bad api");
            return;
        }
        if (gotUrl.toFileName().length() > 0) {
            b = gotHost.sendOneFile(this, pn, gotUrl.filExt);
        } else {
            b = gotHost.sendOneDir(this, pn);
        }
        if (!b) {
            return;
        }
        sendRespError(404, "not found");
    }

    public void run() {
        try {
            if (secured) {
                pipeSide res = lower.negoSecSess(pipe, servGeneric.protoTls, new pipeLine(lower.bufSiz, false), null);
                if (res == null) {
                    return;
                }
                res.lineRx = pipeSide.modTyp.modeCRtryLF;
                res.lineTx = pipeSide.modTyp.modeCRLF;
                pipe = res;
            }
            for (;;) {
                if (readRequest()) {
                    break;
                }
                if (debugger.servHttpTraf) {
                    logger.debug("cmd=" + gotCmd + " ver=" + gotVer + " url="
                            + gotUrl.dump() + " keep=" + gotKeep + " "
                            + bits.lst2str(gotCook, " "));
                }
                serveRequest();
                if (!gotKeep) {
                    break;
                }
                if (lower.singleRequest) {
                    break;
                }
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        try {
            pipe.setClose();
        } catch (Exception e) {
        }
    }

}
