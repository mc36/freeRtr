package org.freertr.serv;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.RandomAccessFile;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.auth.authResult;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgInit;
import org.freertr.cfg.cfgScrpt;
import org.freertr.cfg.cfgTrnsltn;
import org.freertr.clnt.clntDns;
import org.freertr.enc.encBase64;
import org.freertr.enc.encMarkDown;
import org.freertr.enc.encUrl;
import org.freertr.enc.encXml;
import org.freertr.pipe.pipeConnect;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSetting;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtTcp;
import org.freertr.sec.secInfoCls;
import org.freertr.sec.secInfoUtl;
import org.freertr.sec.secInfoWrk;
import org.freertr.sec.secWebsock;
import org.freertr.tab.tabGen;
import org.freertr.user.userConfig;
import org.freertr.user.userExec;
import org.freertr.user.userFlash;
import org.freertr.user.userFormat;
import org.freertr.user.userHelp;
import org.freertr.user.userRead;
import org.freertr.user.userScript;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.debugger;
import org.freertr.util.logger;

/**
 * http utilities
 *
 * @author matecsaba
 */
public class servHttpUtil {

    private servHttpUtil() {
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
     * allow show commands
     */
    public final static int apiBitsShow = 0x10;

    /**
     * allow script commands
     */
    public final static int apiBitsScript = 0x20;

    /**
     * dump one xml
     *
     * @param s xml to dump
     */
    protected final static void dumpXml(String s) {
        if (!debugger.servHttpXml) {
            return;
        }
        dumpXml(encXml.parseOne(s.replaceAll("\r", "").replaceAll("\n", "")));
    }

    /**
     * dump one xml
     *
     * @param xml to dump
     */
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
    public final static int string2apiBits(cmds cmd) {
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
                i |= apiBitsConfig;
                continue;
            }
            if (a.equals("ipinfo")) {
                i |= apiBitsIpinfo;
                continue;
            }
            if (a.equals("show")) {
                i |= apiBitsShow;
                continue;
            }
            if (a.equals("script")) {
                i |= apiBitsScript;
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
    public final static String apiBits2string(int i) {
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
        if ((i & apiBitsShow) != 0) {
            s += " show";
        }
        if ((i & apiBitsScript) != 0) {
            s += " script";
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
     * check for no headers
     *
     * @param s prefix
     * @return true if found, false if not
     */
    protected final static boolean checkNoHeaders(String s) {
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

    private final static String semi2comma(String a) {
        return a.replaceAll(";", ",");
    }

    /**
     * update visitors file
     *
     * @param cn connection to use
     * @param pn pathname to update
     */
    protected final static void updateVisitors(servHttpConn cn, String pn) {
        pn = cn.gotHost.path + pn + ".visitors";
        if (!new File(pn).exists()) {
            return;
        }
        String a = cn.peer + ";" + logger.getTimestamp() + ";" + semi2comma(cn.gotAgent) + ";" + semi2comma(cn.gotReferer) + "\n";
        bits.byteSave(false, a.getBytes(), pn);
    }

    /**
     * send one websocket
     *
     * @param cn connection to use
     * @param pn pathname
     * @return true on error, false on success
     */
    protected final static boolean sendOneWebSck(servHttpConn cn, String pn) {
        pn = cn.gotHost.path + pn + ".websock";
        if (!new File(pn).exists()) {
            cn.sendRespError(404, "not found");
            return true;
        }
        List<String> l = bits.txt2buf(pn);
        if (l == null) {
            cn.sendRespError(404, "error reading");
            return true;
        }
        if (l.size() < 3) {
            cn.sendRespError(502, "bad description");
            return true;
        }
        servGeneric ntry = servGenList.srvFind(l.get(0), l.get(1), false);
        if (ntry == null) {
            cn.sendRespError(502, "bad server");
            return false;
        }
        secWebsock wsk = new secWebsock(cn.pipe, new pipeLine(cn.lower.bufSiz, false));
        wsk.binary = l.get(2).equals("binary");
        if (ntry.srvAccept(wsk.getPipe(), cn.conn)) {
            cn.sendRespError(502, "server refused");
            return false;
        }
        cn.sendLn("HTTP/1.1 101 switching protocol");
        cn.sendLn("Upgrade: websocket");
        cn.sendLn("Connection: Upgrade");
        cn.sendLn("Sec-WebSocket-Accept: " + secWebsock.calcHash(cn.gotWebsock));
        cn.sendLn("Sec-WebSocket-Protocol: " + l.get(2));
        cn.sendLn("");
        wsk.startServer();
        cn.gotKeep = false;
        cn.pipe = null;
        return false;
    }

    /**
     * get style of host
     *
     * @param cn connection to use
     * @return style
     */
    protected final static String getStyle(servHttpConn cn) {
        if (cn.gotHost == null) {
            return "";
        }
        if (cn.gotHost.style == null) {
            return "";
        }
        String s = "<style>\n";
        for (int o = 0; o < cn.gotHost.style.size(); o++) {
            String a = cn.gotHost.style.get(o);
            if (!a.startsWith("@import ")) {
                s += " " + a + "\n";
                continue;
            }
            List<String> l = bits.txt2buf(cn.gotHost.path + a.substring(8, a.length()));
            if (l == null) {
                continue;
            }
            for (int i = 0; i < l.size(); i++) {
                s += " " + l.get(i) + "\n";
            }
        }
        return s + "</style>\n";
    }

    /**
     * run one script
     *
     * @param cn connection to use
     * @param cfg config to use
     * @param l script to run
     * @return true on error, false on success
     */
    protected final static boolean sendOneScript(servHttpConn cn, servHttpHost cfg, List<String> l) {
        pipeLine pl = new pipeLine(1024 * 1024, false);
        pipeSide pip = pl.getSide();
        pip.setTime(60000);
        pip.lineTx = pipeSide.modTyp.modeCRLF;
        pip.lineRx = pipeSide.modTyp.modeCRorLF;
        userScript t = new userScript(pip, "");
        t.addLines(l);
        t.allowConfig = (cfg.allowScript & 4) != 0;
        t.allowExec = (cfg.allowScript & 2) != 0;
        t.currDir = cfg.path;
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

    /**
     * decode authentication string
     *
     * @param got string got
     * @param usr true=user, false=password
     * @return decoded string, null on error
     */
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

    /**
     * check user auth data
     *
     * @param cn connection to use
     * @return false on success, true on error
     */
    protected final static boolean checkUserAuth(servHttpConn cn) {
        if (cn.gotAuth == null) {
            return true;
        }
        String usr = decodeAuth(cn.gotAuth, true);
        String pwd = decodeAuth(cn.gotAuth, false);
        authResult res = cn.gotHost.authenticList.authUserPass(usr, pwd);
        if (res.result != authResult.authSuccessful) {
            return true;
        }
        return false;
    }

    /**
     * apply translations
     *
     * @param cn connection to use
     * @param url url to update
     */
    protected final static void doTranslate(servHttpConn cn, encUrl url) {
        if (cn.gotHost.translate == null) {
            return;
        }
        String a = cfgTrnsltn.doTranslate(cn.gotHost.translate, cn.gotUrl.toURL(true, true, true, true));
        url.fromString(a);
    }

    /**
     * apply subconnect
     *
     * @param cn connection to use
     * @param url url to update
     */
    protected final static void doSubconn(servHttpConn cn, encUrl url) {
        if ((cn.gotHost.subconn & 0x1) == 0) {
            url.filPath = cn.gotUrl.filPath;
        }
        if ((cn.gotHost.subconn & 0x2) == 0) {
            url.filName = cn.gotUrl.filName;
        }
        if ((cn.gotHost.subconn & 0x4) == 0) {
            url.filExt = cn.gotUrl.filExt;
        }
        if ((cn.gotHost.subconn & 0x8) == 0) {
            url.param = cn.gotUrl.param;
        }
        if ((cn.gotHost.subconn & 0x10) != 0) {
            url.username = cn.gotUrl.username;
            url.password = cn.gotUrl.password;
        }
        if ((cn.gotHost.subconn & 0x20) != 0) {
            url.server = cn.gotUrl.server;
        }
        if ((cn.gotHost.subconn & 0x40) != 0) {
            url.filPath = (url.filPath + "/" + cn.gotUrl.filPath).replaceAll("//", "/");
        }
    }

    /**
     * do connect
     *
     * @param cn connection to use
     * @return false if not, true if done
     */
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
        addrIP adr = clntDns.justResolv(cn.gotUrl.server, cn.lower.proxy.prefer);
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

    /**
     * get webdav property
     *
     * @param n filename
     * @param f file to check
     * @param typ type
     * @param len length
     * @param tag tag
     * @param mod modified
     * @param crt creation
     * @param dsp display
     * @param cnt content type
     * @param usd disk usage
     * @param fre disk free
     * @return string
     */
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

    /**
     * parse file name
     *
     * @param cn connection to use
     * @param s string to read
     * @return string parsed
     */
    protected final static String parseFileName(servHttpConn cn, String s) {
        int i = s.lastIndexOf(".");
        if (i < 0) {
            return s;
        }
        cn.addHdr("Content-Disposition: attachment; filename=\"" + s + "\"");
        return s.substring(i + 1, s.length());
    }

    /**
     * send one multipart content
     *
     * @param cn connection to use
     * @param s filename
     * @param a mime type
     * @return true on success, false on error
     */
    protected final static boolean sendOneMotion(servHttpConn cn, String s, String a) {
        cn.gotKeep = false;
        s = cn.gotHost.path + s;
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
        cn.clsPip();
        return false;
    }

    /**
     * start streaming
     *
     * @param cn connection to use
     */
    protected final static void reStream(servHttpConn cn) {
        cn.gotKeep = false;
        cn.sendRespHeader("200 restreaming", -1, cn.gotHost.streamM);
        cn.gotHost.streamC.add(cn.pipe);
        cn.pipe = null;
        if (cn.gotHost.streamS != null) {
            if (cn.gotHost.streamS.isClosed() == 0) {
                return;
            }
        }
        if (cn.gotHost.streamR != null) {
            return;
        }
        cn.gotHost.streamR = new servHttpStrm(cn.gotHost);
        cn.gotHost.streamR.doStart();
    }

    /**
     * do multiple access
     *
     * @param cn connection to use
     */
    protected final static void doMultAcc(servHttpConn cn) {
        cmds cmd = new cmds("hst", cn.gotHost.multiAccT);
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
            adrs[i] = clntDns.justResolv(urls.get(i).server, cn.gotHost.multiAccP.prefer);
        }
        pipeSide[] cons = new pipeSide[adrs.length];
        for (int i = 0; i < adrs.length; i++) {
            cons[i] = null;
            if (adrs[i] == null) {
                continue;
            }
            cons[i] = cn.gotHost.multiAccP.doConnect(servGeneric.protoTcp, adrs[i], urls.get(i).getPort(cn.lower.srvPort()), "http");
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
            fin.linePut("User-Agent: " + cn.gotAgent + " [" + cfgInit.versionAgent + " by " + cn.peer + "]");
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

    /**
     * do reconnect access
     *
     * @param cn connection to use
     */
    protected final static void doReconn(servHttpConn cn) {
        encUrl srvUrl = encUrl.parseOne(cn.gotHost.reconnT);
        doTranslate(cn, srvUrl);
        doSubconn(cn, srvUrl);
        addrIP adr = clntDns.justResolv(srvUrl.server, cn.gotHost.reconnP.prefer);
        if (adr == null) {
            cn.sendRespError(502, "bad gateway");
            return;
        }
        pipeSide cnn = cn.gotHost.reconnP.doConnect(servGeneric.protoTcp, adr, srvUrl.getPort(cn.lower.srvPort()), "http");
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
        cnn.linePut("User-Agent: " + cn.gotAgent + " [" + cfgInit.versionAgent + " by " + cn.peer + "]");
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
     * send one class file
     *
     * @param cn connection to use
     * @param s file name
     * @return false on success, true on error
     */
    protected final static boolean sendOneClass(servHttpConn cn, String s) {
        byte[] res = null;
        try {
            if (!new File(cn.gotHost.path + s).exists()) {
                return true;
            }
            Class<?> cls = cn.gotHost.allowClass.loadClass(cn.gotUrl.filPath + cn.gotUrl.filName);
            Class<?>[] mpl = {String.class, String.class, String.class, String.class, String.class, String[].class, ByteArrayOutputStream.class};
            Method mth = cls.getDeclaredMethod("httpRequest", mpl);
            Object obj = cls.getDeclaredConstructor().newInstance();
            String[] par = new String[cn.gotUrl.param.size()];
            for (int i = 0; i < par.length; i++) {
                par[i] = "" + cn.gotUrl.param.get(i);
            }
            ByteArrayOutputStream buf = new ByteArrayOutputStream();
            obj = mth.invoke(obj, cn.gotUrl.toURL(true, false, false, true), cn.gotHost.path + s, "" + cn.peer, cn.gotAgent, cn.gotAuth, par, buf);
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
            m = cn.gotHost.speedLimit;
        } else {
            m = bits.str2num(s.substring(i + 1, s.length()));
            s = s.substring(0, i);
        }
        if (!a.startsWith("/")) {
            a = cn.gotHost.path + a;
        }
        return sendBinFile(cn, a, s, m);
    }

    /**
     * send one image map
     *
     * @param cn connection to use
     * @param s filename
     * @return false on success, true on error
     */
    protected final static boolean sendOneImgMap(servHttpConn cn, String s) {
        List<String> buf = bits.txt2buf(cn.gotHost.path + s);
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

    /**
     * send one markdown
     *
     * @param cn connection to use
     * @param s filename
     * @return false on success, true on error
     */
    protected final static boolean sendOneMarkdown(servHttpConn cn, String s) {
        List<String> l = bits.txt2buf(cn.gotHost.path + s);
        if (l == null) {
            return true;
        }
        String rsp = servHttp.htmlHead + getStyle(cn) + "<title>" + s + "</title></head><body>\n";
        rsp += encMarkDown.md2html(l);
        rsp += "</body></html>\n";
        cn.sendTextHeader("200 ok", "text/html", rsp.getBytes());
        return false;
    }

    /**
     * send one redirection
     *
     * @param cn connection to use
     */
    protected final static void doRedir(servHttpConn cn) {
        encUrl srvUrl = encUrl.parseOne(cn.gotHost.redir);
        doTranslate(cn, srvUrl);
        doSubconn(cn, srvUrl);
        cn.sendFoundAt(srvUrl.toURL(true, true, true, false));
    }

    /**
     * send one redirection
     *
     * @param cn connection to use
     */
    protected final static void doWebring(servHttpConn cn) {
        encUrl srvUrl = encUrl.parseOne(cn.gotHost.webring.get(bits.random(1, cn.gotHost.webring.size())));
        doTranslate(cn, srvUrl);
        doSubconn(cn, srvUrl);
        cn.sendFoundAt(srvUrl.toURL(true, true, true, false));
    }

    /**
     * send one streaming
     *
     * @param cn connection to use
     * @param s filename
     * @param a mime type
     * @return false on success, true on error
     */
    protected final static boolean sendOneStream(servHttpConn cn, String s, String a) {
        cn.gotKeep = false;
        s = cn.gotHost.path + s;
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
        cn.clsPip();
        return false;
    }

    /**
     * send one directory
     *
     * @param cn connection to use
     * @param s filename
     * @return false on success true on error
     */
    protected final static boolean sendOneDir(servHttpConn cn, String s) {
        if (cn.gotHost.autoIndex) {
            if (!cn.gotHost.sendOneFile(cn, s + "index.html", ".html")) {
                return false;
            }
            if (!cn.gotHost.sendOneFile(cn, s + "index.txt", ".txt")) {
                return false;
            }
            if (!cn.gotHost.sendOneFile(cn, s + "index.md", ".md")) {
                return false;
            }
            if (!cn.gotHost.sendOneFile(cn, s + "index.class", ".class")) {
                return false;
            }
            if (!cn.gotHost.sendOneFile(cn, s + "index.tcl", ".tcl")) {
                return false;
            }
        }
        if (cn.gotHost.allowList == 0) {
            cn.sendRespError(404, "not found");
            return true;
        }
        File[] fl = userFlash.dirList(cn.gotHost.path + s);
        if (fl == null) {
            return true;
        }
        String rsp = servHttp.htmlHead + getStyle(cn) + "<title>dirlist</title></head><body>\n";
        if ((cn.gotHost.allowList & 2) != 0) {
            rsp += encMarkDown.txt2html(bits.txt2buf(cn.gotHost.path + s + "readme.txt"));
            rsp += encMarkDown.md2html(bits.txt2buf(cn.gotHost.path + s + "readme.md"));
        }
        rsp += "<b>directory listing of " + cn.gotHost.host + "/" + s + " at " + cfgAll.getFqdn() + ":</b><br/><br/>\n";
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
            if ((cn.gotHost.allowList & 4) != 0) {
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
        if ((cn.gotHost.allowList & 4) != 0) {
            rsp += "<br/><table><thead><tr><td><b>extension</b></td><td><b>count</b></td><td><b>bytes</b></td><td><b>smallest</b></td><td><b>biggest</b></td><td><b>oldest</b></td><td><b>newest</b></td></tr></thead><tbody>\n";
            rsp += totalD;
            rsp += totalF;
            for (int i = 0; i < stats.size(); i++) {
                rsp += stats.get(i);
            }
            rsp += "</tbody></table><br/>";
        }
        rsp += "<i>generated by </i><b>" + cfgInit.versionName + "</b>.</body></html>\n";
        cn.sendTextHeader("200 ok", "text/html", rsp.getBytes());
        dumpXml(rsp);
        return false;
    }

    /**
     * send one binary file
     *
     * @param cn connection to use
     * @param s filename
     * @param a mime type
     * @param m speed limit
     * @return false on success, true on error
     */
    protected final static boolean sendBinFile(servHttpConn cn, String s, String a, int m) {
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
            cn.addHdr("Content-Range: bytes " + ranB + "-" + ranE + "/" + siz);
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
                cn.clsPip();
                break;
            }
            if (cn.pipe.morePut(buf, 0, rndi) != rndi) {
                cn.clsPip();
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

    /**
     * perform one exec
     *
     * @param beg beginning
     * @param cmd commands
     * @param prv privilege
     * @return results
     */
    protected final static byte[] doOneExec(String beg, cmds cmd, boolean prv) {
        String r = "";
        String e = new String(pipeSide.getEnding(pipeSide.modTyp.modeCRLF));
        for (;;) {
            String s = cmd.word("/");
            if (s.length() < 1) {
                break;
            }
            s = beg + s;
            pipeLine pl = new pipeLine(1024 * 1024, false);
            pipeSide pip = pl.getSide();
            pip.lineTx = pipeSide.modTyp.modeCRLF;
            pip.lineRx = pipeSide.modTyp.modeCRorLF;
            userRead rdr = new userRead(pip, null);
            pip.settingsPut(pipeSetting.tabMod, userFormat.tableMode.raw);
            pip.settingsPut(pipeSetting.height, 0);
            userExec exe = new userExec(pip, rdr);
            exe.privileged = prv;
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
        return r.getBytes();
    }

    /**
     * send one api
     *
     * @param cn connection to use
     * @param s commands
     * @return false on success true on error
     */
    protected final static boolean sendOneApi(servHttpConn cn, String s) {
        if (cn.gotHost.allowApi == apiBitsNothing) {
            return true;
        }
        if ((cn.gotHost.allowApi & apiBitsSomething) == 0) {
            return true;
        }
        cmds cmd = new cmds("api", s);
        cmd.word("/");
        s = cmd.word("/");
        if (debugger.servHttpTraf) {
            logger.debug("api queried cmd=" + s + " prm=" + cmd.getRemaining() + " from " + cn.peer);
        }
        if (((cn.gotHost.allowApi & apiBitsIpinfo) != 0) && s.equals("ipinfo")) {
            if (cn.gotHost.ipInfo == null) {
                return true;
            }
            secInfoCls cls = new secInfoCls(null, null, null, cn.lower.srvVrf.getFwd(cn.peer), cn.peer, prtTcp.protoNum, cn.conn.iface.addr);
            secInfoWrk wrk = new secInfoWrk(cn.gotHost.ipInfo, cls);
            wrk.doHttpUrl(cmd.getRemaining());
            wrk.doWork(false);
            wrk.need2drop();
            List<String> r = wrk.getRouteHtml();
            String a = wrk.getContentType();
            byte[] b = secInfoUtl.getRouteAscii(r);
            cn.sendTextHeader("200 ok", a, b);
            return false;
        }
        if (((cn.gotHost.allowApi & apiBitsShow) != 0) && s.equals("show")) {
            byte[] r = doOneExec("show ", cmd, false);
            cn.sendTextHeader("200 ok", "text/plain", r);
            return false;
        }
        if (((cn.gotHost.allowApi & apiBitsScript) != 0) && s.equals("script")) {
            cfgScrpt scr = cfgAll.scrptFind(cmd.word("/"), false);
            if (scr == null) {
                return true;
            }
            return sendOneScript(cn, cn.gotHost, scr.getText());
        }
        if (((cn.gotHost.allowApi & apiBitsExec) != 0) && s.equals("exec")) {
            byte[] r = doOneExec("", cmd, (cn.gotHost.allowApi & apiBitsConfig) != 0);
            cn.sendTextHeader("200 ok", "text/plain", r);
            return false;
        }
        if (((cn.gotHost.allowApi & apiBitsConfig) != 0) && s.equals("config")) {
            pipeLine pl = new pipeLine(65535, false);
            pipeSide pip = pl.getSide();
            pip.lineTx = pipeSide.modTyp.modeCRLF;
            pip.lineRx = pipeSide.modTyp.modeCRorLF;
            userRead rdr = new userRead(pip, null);
            pip.settingsPut(pipeSetting.tabMod, userFormat.tableMode.raw);
            pip.settingsPut(pipeSetting.height, 0);
            userConfig cfg = new userConfig(pip, rdr);
            pip.setTime(60000);
            for (;;) {
                s = cmd.word("/");
                if (s.length() < 1) {
                    break;
                }
                userHelp hlp = cfg.getHelping(false, true, true);
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

}
