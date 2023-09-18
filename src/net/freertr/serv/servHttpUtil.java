package net.freertr.serv;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.RandomAccessFile;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgInit;
import net.freertr.cfg.cfgProxy;
import net.freertr.cfg.cfgTrnsltn;
import net.freertr.enc.encBase64;
import net.freertr.enc.encMarkDown;
import net.freertr.enc.encUrl;
import net.freertr.enc.encXml;
import net.freertr.pipe.pipeConnect;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.sec.secWebsock;
import net.freertr.user.userScript;
import net.freertr.user.userTerminal;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.debugger;
import net.freertr.util.logger;
import net.freertr.util.version;

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

    protected static final void doTranslate(servHttpConn cn, encUrl srvUrl) {
        if (cn.gotHost.translate == null) {
            return;
        }
        String a = cfgTrnsltn.doTranslate(cn.gotHost.translate, cn.gotUrl.toURL(true, true, true, true));
        srvUrl.fromString(a);
    }

    protected static final void doSubconn(servHttpConn cn, encUrl srvUrl) {
        if ((cn.gotHost.subconn & 0x1) == 0) {
            srvUrl.filPath = cn.gotUrl.filPath;
        }
        if ((cn.gotHost.subconn & 0x2) == 0) {
            srvUrl.filName = cn.gotUrl.filName;
        }
        if ((cn.gotHost.subconn & 0x4) == 0) {
            srvUrl.filExt = cn.gotUrl.filExt;
        }
        if ((cn.gotHost.subconn & 0x8) == 0) {
            srvUrl.param = cn.gotUrl.param;
        }
        if ((cn.gotHost.subconn & 0x10) != 0) {
            srvUrl.username = cn.gotUrl.username;
            srvUrl.password = cn.gotUrl.password;
        }
        if ((cn.gotHost.subconn & 0x20) != 0) {
            srvUrl.server = cn.gotUrl.server;
        }
        if ((cn.gotHost.subconn & 0x40) != 0) {
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

    protected final static String parseFileName(servHttpConn cn, String s) {
        int i = s.lastIndexOf(".");
        if (i < 0) {
            return s;
        }
        cn.addHdr("Content-Disposition: attachment; filename=\"" + s + "\"");
        return s.substring(i + 1, s.length());
    }

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
        cn.pipe.setClose();
        return false;
    }

    /**
     * start streaming
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

    protected final static void doMultAcc(servHttpConn cn) {
        cmds cmd = new cmds("hst", cn.gotHost.multiAccT);
        List<encUrl> urls = new ArrayList<encUrl>();
        for (;;) {
            String a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            encUrl srvUrl = encUrl.parseOne(a);
            servHttpUtil.doTranslate(cn, srvUrl);
            servHttpUtil.doSubconn(cn, srvUrl);
            urls.add(srvUrl);
        }
        addrIP[] adrs = new addrIP[urls.size()];
        for (int i = 0; i < adrs.length; i++) {
            adrs[i] = userTerminal.justResolv(urls.get(i).server, cn.gotHost.multiAccP.prefer);
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

    protected final static void doReconn(servHttpConn cn) {
        encUrl srvUrl = encUrl.parseOne(cn.gotHost.reconnT);
        servHttpUtil.doTranslate(cn, srvUrl);
        servHttpUtil.doSubconn(cn, srvUrl);
        addrIP adr = userTerminal.justResolv(srvUrl.server, cn.gotHost.reconnP.prefer);
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

    protected final static boolean sendOneClass(servHttpConn cn, String s) {
        byte[] res = null;
        try {
            if (!new File(cn.gotHost.path + s).exists()) {
                return true;
            }
            Class<?> cls = cn.gotHost.allowClass.loadClass(cn.gotUrl.filPath + cn.gotUrl.filName);
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
            obj = mth[o].invoke(obj, cn.gotUrl.toURL(true, false, false, true), cn.gotHost.path + s, "" + cn.peer, cn.gotAgent, cn.gotAuth, par, buf);
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
            s = servHttpUtil.parseFileName(cn, s);
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
            s = "." + servHttpUtil.parseFileName(cn, s);
        } else {
            servHttpUtil.parseFileName(cn, s.substring(0, i));
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
        return cn.gotHost.sendBinFile(cn, a, s, m);
    }

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

    protected final static boolean sendOneMarkdown(servHttpConn cn, String s) {
        List<String> l = bits.txt2buf(cn.gotHost.path + s);
        if (l == null) {
            return true;
        }
        String rsp = servHttp.htmlHead + servHttpUtil.getStyle(cn) + "<title>" + s + "</title></head><body>\n";
        rsp += encMarkDown.md2html(l);
        rsp += "</body></html>\n";
        cn.sendTextHeader("200 ok", "text/html", rsp.getBytes());
        return false;
    }

}
