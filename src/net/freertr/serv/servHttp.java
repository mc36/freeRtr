package net.freertr.serv;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.RandomAccessFile;
import java.lang.reflect.Method;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.zip.Deflater;
import net.freertr.addr.addrEmpty;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrType;
import net.freertr.auth.authGeneric;
import net.freertr.auth.authResult;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgAuther;
import net.freertr.cfg.cfgIfc;
import net.freertr.cfg.cfgInit;
import net.freertr.cfg.cfgProxy;
import net.freertr.cfg.cfgScrpt;
import net.freertr.cfg.cfgTrnsltn;
import net.freertr.clnt.clntProxy;
import net.freertr.cry.cryBase64;
import net.freertr.cry.cryHashCrc32;
import net.freertr.ifc.ifcDn;
import net.freertr.ifc.ifcEther;
import net.freertr.ifc.ifcNull;
import net.freertr.ifc.ifcUp;
import net.freertr.pack.packAnyconn;
import net.freertr.pack.packForti;
import net.freertr.pack.packHolder;
import net.freertr.pack.packSstp;
import net.freertr.pipe.pipeConnect;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSetting;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServS;
import net.freertr.sec.secHttp2;
import net.freertr.sec.secWebsock;
import net.freertr.tab.tabGen;
import net.freertr.user.userConfig;
import net.freertr.user.userExec;
import net.freertr.user.userFilter;
import net.freertr.user.userFlash;
import net.freertr.user.userFormat;
import net.freertr.user.userHelping;
import net.freertr.user.userReader;
import net.freertr.user.userScript;
import net.freertr.user.userTerminal;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.counter;
import net.freertr.util.debugger;
import net.freertr.util.extMrkLng;
import net.freertr.util.logger;
import net.freertr.util.markDown;
import net.freertr.util.state;
import net.freertr.util.uniResLoc;
import net.freertr.util.version;

/**
 * hypertext transfer protocol (rfc2616) server
 *
 * @author matecsaba
 */
public class servHttp extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servHttp() {
    }

    /**
     * port number
     */
    public static final int clearPort = 80;

    /**
     * secure port
     */
    public static final int securePort = 443;

    /**
     * html 401 transitive
     */
    public static final String htmlHead = "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" />\n<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" />\n";

    /**
     * list of hosts
     */
    protected tabGen<servHttpServ> hosts = new tabGen<servHttpServ>();

    /**
     * proxy to use
     */
    protected clntProxy proxy;

    /**
     * second port to use
     */
    protected int secondPort = -1;

    /**
     * error message
     */
    protected String error;

    /**
     * single request
     */
    protected boolean singleRequest;

    /**
     * buffer size
     */
    protected int bufSiz = 65536;

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        "server http .*! port " + clearPort,
        "server http .*! protocol " + proto2string(protoAllStrm),
        "server http .*! no proxy",
        "server http .*! no error",
        "server http .*! no single-request",
        "server http .*! buffer 65536",
        "server http .*! no second-port",};

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    /**
     * get defaults filter
     *
     * @return filter
     */
    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    /**
     * get gzip header
     *
     * @return header
     */
    public static byte[] getGzipHdr() {
        byte[] res = new byte[10];
        res[0] = 0x1f; // magic
        res[1] = (byte) 0x8b; // magic
        res[2] = Deflater.DEFLATED; // deflate
        res[3] = 0; // flags
        res[4] = 0; // mtime
        res[5] = 0; // mtime
        res[6] = 0; // mtime
        res[7] = 0; // mtime
        res[8] = 0; // extra flags
        res[9] = (byte) 0xff; // os
        return res;
    }

    /**
     * get gzip trailer
     *
     * @param unc uncompressed data
     * @return trailer
     */
    public static byte[] getGzipTrl(byte[] unc) {
        byte[] res = new byte[8];
        cryHashCrc32 crc = new cryHashCrc32(cryHashCrc32.polyCrc32i);
        crc.init();
        crc.update(unc);
        bits.lsbPutD(res, 0, bits.msbGetD(crc.finish(), 0));
        bits.lsbPutD(res, 4, unc.length);
        return res;
    }

    /**
     * find one host
     *
     * @param s host name
     * @return host descriptor, null if not found
     */
    protected servHttpServ findHost(String s) {
        servHttpServ ntry = new servHttpServ();
        ntry.host = s;
        ntry = hosts.find(ntry);
        if (ntry != null) {
            return ntry;
        }
        for (int i = hosts.size() - 1; i >= 0; i--) {
            ntry = hosts.get(i);
            if (!ntry.host.startsWith("*")) {
                continue;
            }
            if (!s.endsWith(ntry.host.substring(1, ntry.host.length()))) {
                continue;
            }
            return ntry;
        }
        return null;
    }

    /**
     * start connection
     *
     * @param pipe pipeline
     * @param id connection
     * @return false on success, true on error
     */
    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.setTime(120000);
        new servHttpConn(this, pipe, id);
        return false;
    }

    /**
     * get config
     *
     * @param beg beginning
     * @param l list
     */
    public void srvShRun(String beg, List<String> l, int filter) {
        if (proxy == null) {
            l.add(beg + "no proxy");
        } else {
            l.add(beg + "proxy " + proxy.name);
        }
        if (error == null) {
            l.add(beg + "no error");
        } else {
            l.add(beg + "error " + error);
        }
        cmds.cfgLine(l, secondPort < 0, beg, "second-port", "" + secondPort);
        l.add(beg + "buffer " + bufSiz);
        cmds.cfgLine(l, !singleRequest, beg, "single-request", "");
        for (int hn = 0; hn < hosts.size(); hn++) {
            servHttpServ ntry = hosts.get(hn);
            if (ntry == null) {
                continue;
            }
            String a = beg + "host " + ntry.host;
            l.add(a + " path " + ntry.path);
            if (ntry.style != null) {
                for (int i = 0; i < ntry.style.size(); i++) {
                    l.add(a + " style " + ntry.style.get(i));
                }
            }
            if (ntry.redir != null) {
                l.add(a + " redir " + ntry.redir);
            }
            if (ntry.logging) {
                l.add(a + " logging");
            }
            if (ntry.reconnT != null) {
                l.add(a + " reconn " + ntry.reconnP.name + " " + ntry.reconnT);
            }
            if (ntry.translate != null) {
                String s = "";
                for (int i = 0; i < ntry.translate.size(); i++) {
                    s += " " + ntry.translate.get(i).name;
                }
                l.add(a + " translate" + s);
            }
            if (ntry.subconn != 0) {
                String s = "";
                if ((ntry.subconn & 0x1) != 0) {
                    s += " strip-path";
                }
                if ((ntry.subconn & 0x2) != 0) {
                    s += " strip-name";
                }
                if ((ntry.subconn & 0x4) != 0) {
                    s += " strip-ext";
                }
                if ((ntry.subconn & 0x8) != 0) {
                    s += " strip-param";
                }
                if ((ntry.subconn & 0x10) != 0) {
                    s += " keep-cred";
                }
                if ((ntry.subconn & 0x20) != 0) {
                    s += " keep-host";
                }
                if ((ntry.subconn & 0x40) != 0) {
                    s += " keep-path";
                }
                l.add(a + " subconn" + s);
            }
            if (ntry.streamT != null) {
                l.add(a + " stream " + ntry.streamM + " " + ntry.streamP.name + " " + ntry.streamT);
            }
            if (ntry.multiAccT != null) {
                l.add(a + " multiacc " + ntry.multiAccP.name + " " + ntry.multiAccT);
            }
            if (ntry.allowList != 0) {
                String s = "";
                if ((ntry.allowList & 2) != 0) {
                    s += " readme";
                }
                if ((ntry.allowList & 4) != 0) {
                    s += " stats";
                }
                l.add(a + " dirlist" + s);
            }
            if (ntry.allowMarkdown) {
                l.add(a + " markdown");
            }
            if (ntry.speedLimit > 0) {
                l.add(a + " speed-limit " + ntry.speedLimit);
            }
            if (!ntry.autoIndex) {
                l.add(a + " noindex");
            }
            if (ntry.allowScript != 0) {
                String s = "";
                if ((ntry.allowScript & 2) != 0) {
                    s += " exec";
                }
                if ((ntry.allowScript & 4) != 0) {
                    s += " config";
                }
                if ((ntry.allowScript & 8) != 0) {
                    s += " local";
                }
                l.add(a + " script" + s);
            }
            if (ntry.allowApi != 0) {
                String s = "";
                if ((ntry.allowApi & 2) != 0) {
                    s += " exec";
                }
                if ((ntry.allowApi & 4) != 0) {
                    s += " config";
                }
                l.add(a + " api" + s);
            }
            if (ntry.allowImgMap) {
                l.add(a + " imagemap");
            }
            if (ntry.allowWebSck) {
                l.add(a + " websock");
            }
            if (ntry.allowWebDav) {
                l.add(a + " webdav");
            }
            if (ntry.allowMediaStrm) {
                l.add(a + " mediastream");
            }
            if (ntry.allowClass != null) {
                l.add(a + " class");
            }
            if (ntry.allowUpload) {
                l.add(a + " upload");
            }
            if (ntry.backupPath != null) {
                l.add(a + " backup " + ntry.backupCount + " " + ntry.backupPath);
            }
            if (ntry.allowSstp != null) {
                l.add(a + " sstp " + ntry.allowSstp.name);
            }
            if (ntry.allowAnyconn != null) {
                l.add(a + " anyconn " + ntry.allowAnyconn.name);
            }
            if (ntry.allowForti != null) {
                l.add(a + " forti " + ntry.allowForti.name);
            }
            if (ntry.authenticList != null) {
                l.add(a + " authentication " + ntry.authenticList.autName);
            }
        }
    }

    /**
     * configure
     *
     * @param cmd command
     * @return false on success, true on error
     */
    public boolean srvCfgStr(cmds cmd) {
        String a = cmd.word();
        boolean negated = false;
        if (a.equals("no")) {
            negated = true;
            a = cmd.word();
        }
        if (a.equals("second-port")) {
            if (negated) {
                secondPort = -1;
                return false;
            }
            secondPort = bits.str2num(cmd.word());
            return false;
        }
        if (a.equals("buffer")) {
            bufSiz = bits.str2num(cmd.word());
            return false;
        }
        if (a.equals("single-request")) {
            singleRequest = !negated;
            return false;
        }
        if (a.equals("proxy")) {
            if (negated) {
                proxy = null;
                return false;
            }
            cfgProxy prx = cfgAll.proxyFind(cmd.word(), false);
            if (prx == null) {
                cmd.error("no such proxy");
                return false;
            }
            proxy = prx.proxy;
            return false;
        }
        if (a.equals("error")) {
            error = cmd.getRemaining();
            if (negated) {
                error = null;
                return false;
            }
            return false;
        }
        if (!a.equals("host")) {
            return true;
        }
        servHttpServ ntry = new servHttpServ();
        ntry.host = cmd.word();
        servHttpServ old = hosts.add(ntry);
        if (old != null) {
            ntry = old;
        }
        a = cmd.word();
        if (a.length() < 1) {
            return false;
        }
        if (a.equals("path")) {
            if (negated) {
                hosts.del(ntry);
                return false;
            }
            ntry.path = "/" + uniResLoc.normalizePath(cmd.word() + "/");
            return false;
        }
        if (a.equals("style")) {
            if (negated) {
                ntry.style = null;
                return false;
            }
            if (ntry.style == null) {
                ntry.style = new ArrayList<String>();
            }
            ntry.style.add(cmd.getRemaining());
            return false;
        }
        if (a.equals("redir")) {
            if (negated) {
                ntry.redir = null;
                return false;
            }
            ntry.redir = cmd.word();
            return false;
        }
        if (a.equals("logging")) {
            ntry.logging = !negated;
            return false;
        }
        if (a.equals("reconn")) {
            if (negated) {
                ntry.reconnP = null;
                ntry.reconnT = null;
                return false;
            }
            cfgProxy prx = cfgAll.proxyFind(cmd.word(), false);
            if (prx == null) {
                cmd.error("no such proxy");
                return false;
            }
            ntry.reconnP = prx.proxy;
            ntry.reconnT = cmd.word();
            return false;
        }
        if (a.equals("translate")) {
            if (negated) {
                ntry.translate = null;
                return false;
            }
            ntry.translate = new ArrayList<cfgTrnsltn>();
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
                ntry.translate.add(trn);
            }
            return false;
        }
        if (a.equals("subconn")) {
            ntry.subconn = 0;
            if (negated) {
                return false;
            }
            for (;;) {
                a = cmd.word();
                if (a.length() < 1) {
                    break;
                }
                if (a.equals("strip-path")) {
                    ntry.subconn |= 0x1;
                    continue;
                }
                if (a.equals("strip-name")) {
                    ntry.subconn |= 0x2;
                    continue;
                }
                if (a.equals("strip-ext")) {
                    ntry.subconn |= 0x4;
                    continue;
                }
                if (a.equals("strip-param")) {
                    ntry.subconn |= 0x8;
                    continue;
                }
                if (a.equals("keep-cred")) {
                    ntry.subconn |= 0x10;
                    continue;
                }
                if (a.equals("keep-host")) {
                    ntry.subconn |= 0x20;
                    continue;
                }
                if (a.equals("keep-path")) {
                    ntry.subconn |= 0x40;
                    continue;
                }
            }
            return false;
        }
        if (a.equals("stream")) {
            if (negated) {
                ntry.streamP = null;
                ntry.streamT = null;
                ntry.streamM = null;
                if (ntry.streamS == null) {
                    return false;
                }
                ntry.streamS.setClose();
                ntry.streamS = null;
                return false;
            }
            a = cmd.word();
            cfgProxy prx = cfgAll.proxyFind(cmd.word(), false);
            if (prx == null) {
                cmd.error("no such proxy");
                return false;
            }
            ntry.streamM = a;
            ntry.streamP = prx.proxy;
            ntry.streamT = cmd.word();
            if (ntry.streamC == null) {
                ntry.streamC = new ArrayList<pipeSide>();
            }
            if (ntry.streamS == null) {
                return false;
            }
            ntry.streamS.setClose();
            ntry.streamS = null;
            return false;
        }
        if (a.equals("multiacc")) {
            if (negated) {
                ntry.multiAccP = null;
                ntry.multiAccT = null;
                return false;
            }
            cfgProxy prx = cfgAll.proxyFind(cmd.word(), false);
            if (prx == null) {
                cmd.error("no such proxy");
                return false;
            }
            ntry.multiAccP = prx.proxy;
            ntry.multiAccT = cmd.getRemaining();
            return false;
        }
        if (a.equals("speed-limit")) {
            if (negated) {
                ntry.speedLimit = 0;
                return false;
            }
            ntry.speedLimit = bits.str2num(cmd.word());
            return false;
        }
        if (a.equals("noindex")) {
            ntry.autoIndex = negated;
            return false;
        }
        if (a.equals("markdown")) {
            ntry.allowMarkdown = !negated;
            return false;
        }
        if (a.equals("dirlist")) {
            if (negated) {
                ntry.allowList = 0;
                return false;
            }
            ntry.allowList = 1;
            for (;;) {
                a = cmd.word();
                if (a.length() < 1) {
                    break;
                }
                if (a.equals("readme")) {
                    ntry.allowList |= 2;
                    continue;
                }
                if (a.equals("stats")) {
                    ntry.allowList |= 4;
                    continue;
                }
            }
            return false;
        }
        if (a.equals("api")) {
            if (negated) {
                ntry.allowApi = 0;
                return false;
            }
            ntry.allowApi = 1;
            for (;;) {
                a = cmd.word();
                if (a.length() < 1) {
                    break;
                }
                if (a.equals("exec")) {
                    ntry.allowApi |= 2;
                    continue;
                }
                if (a.equals("config")) {
                    ntry.allowApi |= 4;
                    continue;
                }
            }
            return false;
        }
        if (a.equals("script")) {
            if (negated) {
                ntry.allowScript = 0;
                return false;
            }
            ntry.allowScript = 1;
            for (;;) {
                a = cmd.word();
                if (a.length() < 1) {
                    break;
                }
                if (a.equals("exec")) {
                    ntry.allowScript |= 2;
                    continue;
                }
                if (a.equals("config")) {
                    ntry.allowScript |= 4;
                    continue;
                }
                if (a.equals("local")) {
                    ntry.allowScript |= 8;
                    continue;
                }
            }
            return false;
        }
        if (a.equals("imagemap")) {
            ntry.allowImgMap = !negated;
            return false;
        }
        if (a.equals("websock")) {
            ntry.allowWebSck = !negated;
            return false;
        }
        if (a.equals("webdav")) {
            ntry.allowWebDav = !negated;
            return false;
        }
        if (a.equals("mediastream")) {
            ntry.allowMediaStrm = !negated;
            return false;
        }
        if (a.equals("class")) {
            if (negated) {
                ntry.allowClass = null;
                return false;
            }
            try {
                URL url = new URL("file://" + ntry.path);
                URL[] urls = new URL[1];
                urls[0] = url;
                ntry.allowClass = new URLClassLoader(urls);
            } catch (Exception e) {
                ntry.allowClass = null;
            }
            return false;
        }
        if (a.equals("upload")) {
            ntry.allowUpload = !negated;
            return false;
        }
        if (a.equals("backup")) {
            if (negated) {
                ntry.backupCount = 0;
                ntry.backupPath = null;
                return false;
            }
            ntry.backupCount = bits.str2num(cmd.word());
            ntry.backupPath = "/" + uniResLoc.normalizePath(cmd.word() + "/");
            return false;
        }
        if (a.equals("sstp")) {
            if (negated) {
                ntry.allowSstp = null;
                return false;
            }
            ntry.allowSstp = cfgAll.ifcFind(cmd.word(), 0);
            if (ntry.allowSstp == null) {
                cmd.error("no such interface");
                return false;
            }
            if (ntry.allowSstp.type != cfgIfc.ifaceType.dialer) {
                cmd.error("not dialer interface");
                ntry.allowSstp = null;
                return false;
            }
            return false;
        }
        if (a.equals("anyconn")) {
            if (negated) {
                ntry.allowAnyconn = null;
                return false;
            }
            ntry.allowAnyconn = cfgAll.ifcFind(cmd.word(), 0);
            if (ntry.allowAnyconn == null) {
                cmd.error("no such interface");
                return false;
            }
            if (ntry.allowAnyconn.type != cfgIfc.ifaceType.dialer) {
                cmd.error("not dialer interface");
                ntry.allowAnyconn = null;
                return false;
            }
            return false;
        }
        if (a.equals("forti")) {
            if (negated) {
                ntry.allowForti = null;
                return false;
            }
            ntry.allowForti = cfgAll.ifcFind(cmd.word(), 0);
            if (ntry.allowForti == null) {
                cmd.error("no such interface");
                return false;
            }
            if (ntry.allowForti.type != cfgIfc.ifaceType.dialer) {
                cmd.error("not dialer interface");
                ntry.allowForti = null;
                return false;
            }
            return false;
        }
        if (a.equals("authentication")) {
            if (negated) {
                ntry.authenticList = null;
                return false;
            }
            cfgAuther lst = cfgAll.autherFind(cmd.word(), null);
            if (lst == null) {
                cmd.error("no such auth list");
                return false;
            }
            ntry.authenticList = lst.getAuther();
            return false;
        }
        return true;
    }

    /**
     * get help
     *
     * @param l list
     */
    public void srvHelp(userHelping l) {
        l.add(null, "1 .  single-request                 one request per connection");
        l.add(null, "1 2  buffer                         set buffer size on connection");
        l.add(null, "2 .    <num>                        buffer in bytes");
        l.add(null, "1 2  proxy                          enable proxy support");
        l.add(null, "2 .    <name:prx>                   proxy profile");
        l.add(null, "1 2  second-port                    enable dual binding");
        l.add(null, "2 .    <num>                        secure port");
        l.add(null, "1 2  error                          set error message");
        l.add(null, "2 2,.  <str>                        error message");
        l.add(null, "1 2  host                           define one virtual server");
        List<String> lst = new ArrayList<String>();
        for (int i = 0; i < hosts.size(); i++) {
            lst.add(hosts.get(i).host);
        }
        l.add(lst, "2 3,.  <name:loc>                   name of server, * for any");
        l.add(null, "3 4      path                       set server root");
        l.add(null, "4 .        <str>                    root directory of server");
        l.add(null, "3 4      redir                      set redirect path");
        l.add(null, "4 .        <url>                    url to redirect to");
        l.add(null, "3 .      logging                    log to syslog");
        l.add(null, "3 4      reconn                     reconnect to server");
        l.add(null, "4 5        <name:prx>               proxy profile");
        l.add(null, "5 .          <str>                  server to redirect to");
        l.add(null, "3 4      translate                  translate the url");
        l.add(null, "4 4,.      <num:trn>                translation rule to use");
        l.add(null, "3 4      subconn                    reconnect only to the url");
        l.add(null, "4 4,.      strip-path               strip path");
        l.add(null, "4 4,.      strip-name               strip filename");
        l.add(null, "4 4,.      strip-ext                strip extension");
        l.add(null, "4 4,.      strip-param              strip parameters");
        l.add(null, "4 4,.      keep-cred                keep credentinals");
        l.add(null, "4 4,.      keep-host                keep hostname");
        l.add(null, "4 4,.      keep-path                append path");
        l.add(null, "3 4      stream                     stream from server");
        l.add(null, "4 5        <str>                    content type");
        l.add(null, "5 6          <name:prx>             proxy profile");
        l.add(null, "6 .            <str>                server to stream from");
        l.add(null, "3 4      multiacc                   access multiple servers");
        l.add(null, "4 5        <name:prx>               proxy profile");
        l.add(null, "5 5,.        <str>                  server to access");
        l.add(null, "3 .      markdown                   allow markdown conversion");
        l.add(null, "3 .      noindex                    disallow index for directory");
        l.add(null, "3 4      speed-limit                limit download speeds");
        l.add(null, "4 .        <num>                    bytes per second");
        l.add(null, "3 4,.    dirlist                    allow directory listing");
        l.add(null, "4 4,.      readme                   put readme in front of listing");
        l.add(null, "4 4,.      stats                    put statistics after listing");
        l.add(null, "3 4,.    script                     allow script running");
        l.add(null, "4 4,.      exec                     allow exec commands");
        l.add(null, "4 4,.      config                   allow config commands");
        l.add(null, "4 4,.      local                    allow scripts defined in configuration");
        l.add(null, "3 4,.    api                        allow api calls");
        l.add(null, "4 4,.      exec                     allow exec commands");
        l.add(null, "4 4,.      config                   allow config commands");
        l.add(null, "3 .      imagemap                   allow image map processing");
        l.add(null, "3 .      websock                    allow websocket processing");
        l.add(null, "3 .      webdav                     allow webdav processing");
        l.add(null, "3 .      mediastream                allow media streaming");
        l.add(null, "3 .      class                      allow class running");
        l.add(null, "3 .      upload                     allow upload files");
        l.add(null, "3 4      backup                     backup uploaded files");
        l.add(null, "4 5        <num>                    number of backups to keep");
        l.add(null, "5 .          <str>                  root directory of backup");
        l.add(null, "3 4      sstp                       allow sstp clients");
        l.add(null, "4 .        <name:ifc>               name of interface");
        l.add(null, "3 4      anyconn                    allow anyconnect clients");
        l.add(null, "4 .        <name:ifc>               name of interface");
        l.add(null, "3 4      forti                      allow fortinet clients");
        l.add(null, "4 .        <name:ifc>               name of interface");
        l.add(null, "3 4      authentication             require authentication to access");
        l.add(null, "4 .        <name:aaa>               authentication list");
        l.add(null, "3 4      style                      set page style tags");
        l.add(null, "4 4,.      <text>                   text to send");
    }

    /**
     * get name
     *
     * @return name
     */
    public String srvName() {
        return "http";
    }

    /**
     * get port
     *
     * @return port
     */
    public int srvPort() {
        return clearPort;
    }

    /**
     * get protocol
     *
     * @return protocol
     */
    public int srvProto() {
        return protoAllStrm;
    }

    /**
     * initialize
     *
     * @return false on success, true on error
     */
    public boolean srvInit() {
        if (secondPort > 0) {
            if (genStrmStart(this, new pipeLine(bufSiz, false), secondPort)) {
                return true;
            }
        }
        return genStrmStart(this, new pipeLine(bufSiz, false), 0);
    }

    /**
     * deinitialize
     *
     * @return false on success, true on error
     */
    public boolean srvDeinit() {
        if (secondPort > 0) {
            if (genericStop(secondPort)) {
                return true;
            }
        }
        return genericStop(0);
    }

    /**
     * get show
     *
     * @return result
     */
    public userFormat getShow() {
        userFormat res = new userFormat("|", "host|hit|last");
        for (int i = 0; i < hosts.size(); i++) {
            servHttpServ ntry = hosts.get(i);
            res.add(ntry.host + "|" + ntry.askNum + "|" + bits.timePast(ntry.askTim));
        }
        return res;
    }

}

class servHttpServ implements Runnable, Comparator<servHttpServ> {

    /**
     * name of server
     */
    public String host = "";

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
    public String path = "/";

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
     * script running allowed
     */
    public int allowScript;

    /**
     * api calls allowed
     */
    public int allowApi;

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

    public int compare(servHttpServ o1, servHttpServ o2) {
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
        uniResLoc srvUrl = uniResLoc.parseOne(streamT);
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
        cnn.linePut("GET " + srvUrl.toURL(false, false, true) + " HTTP/1.1");
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

}

class servHttpConn implements Runnable {

    protected servHttp lower;

    protected pipeSide pipe;

    protected addrIP peer = new addrIP();

    protected String gotCmd; // command

    protected boolean gotHead; // just header

    protected uniResLoc gotUrl; // url

    protected String gotAuth; // authentication

    protected servHttpServ gotHost; // requested host

    protected int gotVer; // protocol version

    protected boolean gotKeep; // keepalive

    protected boolean gotDepth; // depth

    protected int gotCompr; // compression, 1=deflate, 2=gzip

    protected String gotDstntn; // destination

    protected List<String> gotCook; // rx cookie

    protected byte[] gotBytes; // oontent read

    protected String gotAgent; // user agent

    protected String gotReferer; // referer

    protected String gotWebsock; // websocket key

    protected String gotRange; // range

    private List<String> headers; // tx headers

    private boolean secured;

    public servHttpConn(servHttp parent, pipeSide stream, prtGenConn id) {
        lower = parent;
        pipe = stream;
        pipe.lineRx = pipeSide.modTyp.modeCRtryLF;
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        peer.setAddr(id.peerAddr);
        secured = id.portLoc == lower.secondPort;
        new Thread(this).start();
    }

    private void sendLn(String a) {
        if (debugger.servHttpTraf) {
            logger.debug("tx '" + a + "'");
        }
        pipe.linePut(a);
    }

    private void sendRespHeader(String head, long size, String type) {
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

    private void sendTextHeader(String head, String type, byte[] buf1) {
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
        if (gotHost.style == null) {
            return "";
        }
        String s = "<style>\n";
        for (int o = 0; o < gotHost.style.size(); o++) {
            String a = gotHost.style.get(o);
            if (!a.startsWith("@import ")) {
                s += " " + a + "\n";
                continue;
            }
            List<String> l = bits.txt2buf(gotHost.path + a.substring(8, a.length()));
            if (l == null) {
                continue;
            }
            for (int i = 0; i < l.size(); i++) {
                s += " " + l.get(i) + "\n";
            }
        }
        return s + "</style>\n";
    }

    private void sendRespError(int code, String text) {
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

    private void sendFoundAt(String where) {
        gotKeep = false;
        String s = servHttp.htmlHead + getStyle() + "<title>moved</title></head><body>moved to <a href=\"" + where + "\">" + where + "</a>. you will be redirected.</body></html>\n";
        headers.add("Location: " + where);
        sendRespHeader("301 moved", s.length(), "text/html");
        if (gotHead) {
            return;
        }
        pipe.strPut(s);
    }

    private boolean checkNoHeaders(String s) {
        return new File(s + ".noheaders").exists();
    }

    private void updateVisitors(String s) {
        s = gotHost.path + s + ".visitors";
        if (!new File(s).exists()) {
            return;
        }
        String a = peer + ";" + logger.getTimestamp() + ";" + gotAgent.replaceAll(";", ",") + ";" + gotReferer.replaceAll(";", ",") + "\n";
        bits.byteSave(false, a.getBytes(), s);
    }

    private boolean sendOneWebSck(String s) {
        s = gotHost.path + s + ".websock";
        if (!new File(s).exists()) {
            return true;
        }
        List<String> l = bits.txt2buf(s);
        if (l == null) {
            return true;
        }
        if (l.size() < 5) {
            return true;
        }
        cfgProxy prx = cfgAll.proxyFind(l.get(0), false);
        if (prx == null) {
            sendRespError(502, "bad proxy profile");
            return false;
        }
        addrIP adr = userTerminal.justResolv(l.get(1), prx.proxy.prefer);
        if (adr == null) {
            sendRespError(502, "bad target hostname");
            return false;
        }
        pipeSide pip = prx.doConnect(servGeneric.protoTcp, adr, bits.str2num(l.get(2)), "websock");
        if (pip == null) {
            sendRespError(502, "failed to connect");
            return false;
        }
        sendLn("HTTP/1.1 101 switching protocol");
        sendLn("Upgrade: websocket");
        sendLn("Connection: Upgrade");
        sendLn("Sec-WebSocket-Accept: " + secWebsock.calcHash(gotWebsock));
        sendLn("Sec-WebSocket-Protocol: " + l.get(3));
        sendLn("");
        secWebsock wsk = new secWebsock(pipe, new pipeLine(lower.bufSiz, false));
        wsk.binary = l.get(4).equals("bin");
        wsk.startServer();
        pipeConnect.connect(pip, wsk.getPipe(), true);
        gotKeep = false;
        pipe = null;
        return false;
    }

    private boolean sendOneApi(String s) {
        cmds cmd = new cmds("api", s);
        cmd.word("/");
        s = cmd.word("/");
        if (((gotHost.allowApi & 2) != 0) && s.equals("exec")) {
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
                exe.privileged = (gotHost.allowApi & 4) != 0;
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
            sendTextHeader("200 ok", "text/plain", r.getBytes());
            return false;
        }
        if (((gotHost.allowApi & 4) != 0) && s.equals("config")) {
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
            sendTextHeader("200 ok", "text/plain", s.getBytes());
            return false;
        }
        return true;
    }

    private boolean sendOneMarkdown(String s) {
        List<String> l = bits.txt2buf(gotHost.path + s);
        if (l == null) {
            return true;
        }
        String rsp = servHttp.htmlHead + getStyle() + "<title>" + s + "</title></head><body>\n";
        rsp += markDown.md2html(l);
        rsp += "</body></html>\n";
        sendTextHeader("200 ok", "text/html", rsp.getBytes());
        return false;
    }

    private boolean sendOneScript(String s) {
        List<String> l = bits.txt2buf(gotHost.path + s);
        if (l == null) {
            return true;
        }
        return sendOneScript(l);
    }

    private boolean sendOneScript(List<String> l) {
        pipeLine pl = new pipeLine(1024 * 1024, false);
        pipeSide pip = pl.getSide();
        pip.setTime(60000);
        pip.lineTx = pipeSide.modTyp.modeCRLF;
        pip.lineRx = pipeSide.modTyp.modeCRorLF;
        userScript t = new userScript(pip, "");
        t.addLines(l);
        t.allowConfig = (gotHost.allowScript & 4) != 0;
        t.allowExec = (gotHost.allowScript & 2) != 0;
        t.currDir = gotHost.path;
        pip = pl.getSide();
        pip.lineTx = pipeSide.modTyp.modeCR;
        pip.lineRx = pipeSide.modTyp.modeCRorLF;
        pip.linePut("prot=" + gotUrl.proto);
        pip.linePut("serv=" + gotUrl.server);
        pip.linePut("path=" + gotUrl.toPathName());
        pip.linePut("agnt=" + gotAgent);
        pip.linePut("refr=" + gotReferer);
        if (gotAuth != null) {
            pip.linePut("auth=" + gotAuth);
        }
        pip.linePut("clnt=" + peer);
        for (int i = 0; i < gotUrl.param.size(); i++) {
            pip.linePut("par." + gotUrl.param.get(i));
        }
        for (int i = 0; i < gotCook.size(); i++) {
            pip.linePut("cok." + gotCook.get(i));
        }
        pip.linePut(".");
        t.cmdAll();
        pl.setClose();
        String s = pip.strGet(1024 * 1024);
        if (s == null) {
            s = "";
        }
        sendTextHeader("200 ok", "text/html", s.getBytes());
        return false;
    }

    private String parseFileName(String s) {
        int i = s.lastIndexOf(".");
        if (i < 0) {
            return s;
        }
        headers.add("Content-Disposition: attachment; filename=\"" + s + "\"");
        return s.substring(i + 1, s.length());
    }

    private boolean sendOneClass(String s) {
        byte[] res = null;
        try {
            if (!new File(gotHost.path + s).exists()) {
                return true;
            }
            Class<?> cls = gotHost.allowClass.loadClass(gotUrl.filPath + gotUrl.filName);
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
            String[] par = new String[gotUrl.param.size()];
            for (int i = 0; i < par.length; i++) {
                par[i] = "" + gotUrl.param.get(i);
            }
            ByteArrayOutputStream buf = new ByteArrayOutputStream();
            obj = mth[o].invoke(obj, gotUrl.toURL(true, false, false), gotHost.path + s, "" + peer, gotAgent, gotAuth, par, buf);
            s = (String) obj;
            res = buf.toByteArray();
        } catch (Exception e) {
            logger.traceback(e);
            return true;
        }
        if (debugger.servHttpTraf) {
            logger.debug("res=" + s + " bytes=" + res.length);
        }
        if (s == null) {
            return true;
        }
        if (!s.equals("//file//")) {
            s = parseFileName(s);
            sendTextHeader("200 ok", cfgInit.findMimeType(s), res);
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
            s = "." + parseFileName(s);
        } else {
            parseFileName(s.substring(0, i));
            s = s.substring(i + 1, s.length());
        }
        i = s.indexOf("\n");
        int m;
        if (i < 0) {
            m = gotHost.speedLimit;
        } else {
            m = bits.str2num(s.substring(i + 1, s.length()));
            s = s.substring(0, i);
        }
        if (!a.startsWith("/")) {
            a = gotHost.path + a;
        }
        return sendBinFile(a, s, m);
    }

    private boolean sendOneImgMap(String s) {
        List<String> buf = bits.txt2buf(gotHost.path + s);
        if (buf == null) {
            return true;
        }
        if (gotUrl.param.size() < 1) {
            return true;
        }
        s = "" + gotUrl.param.get(0);
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
                sendFoundAt(cmd.getRemaining());
                return false;
            }
            if (s.equals("default")) {
                sendFoundAt(cmd.getRemaining());
                return false;
            }
        }
        return true;
    }

    private boolean sendOneStream(String s, String a) {
        gotKeep = false;
        s = gotHost.path + s;
        sendRespHeader("200 streaming", -1, cfgInit.findMimeType(a));
        if (gotHead) {
            return false;
        }
        long os = new File(s).length() - 65536;
        if (os < 0) {
            os = 0;
        }
        long ot = -1;
        for (;;) {
            if (pipe.isClosed() != 0) {
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
            pipe.morePut(buf, 0, buf.length);
        }
        pipe.setClose();
        return false;
    }

    private boolean sendOneMotion(String s, String a) {
        gotKeep = false;
        s = gotHost.path + s;
        final String bnd = "someRandomBoundaryStringThatWontOccurs";
        sendRespHeader("200 streaming", -1,
                "multipart/x-mixed-replace;boundary=" + bnd);
        if (gotHead) {
            return false;
        }
        long os = -1;
        long ot = -1;
        for (;;) {
            if (pipe.isClosed() != 0) {
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
            sendLn("--" + bnd);
            sendRespHeader(null, buf.length, cfgInit.findMimeType(a));
            pipe.morePut(buf, 0, buf.length);
        }
        pipe.setClose();
        return false;
    }

    private boolean sendBinFile(String s, String a, int m) {
        RandomAccessFile fr;
        long siz;
        try {
            File f = new File(s);
            if (f.isDirectory()) {
                sendFoundAt(gotUrl.toURL(true, false, false) + "/");
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
            gotKeep = false;
            gotHead = false;
            gotRange = null;
        }
        if (gotRange != null) {
            gotRange = gotRange.replaceAll(" ", "");
            if (!gotRange.startsWith("bytes=")) {
                gotRange = "";
            } else {
                gotRange = gotRange.substring(6, gotRange.length());
            }
            int i = gotRange.indexOf("-");
            if (i < 0) {
                gotRange = null;
            } else if (i == 0) {
                ranB = bits.str2long(gotRange.substring(1, gotRange.length()));
                ranE = siz - 1;
            } else {
                ranB = bits.str2long(gotRange.substring(0, i));
                ranE = bits.str2long(gotRange.substring(i + 1, gotRange.length()));
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
        if (gotRange == null) {
            if (!checkNoHeaders(s)) {
                sendRespHeader("200 ok", siz, cfgInit.findMimeType(a));
            }
        } else {
            headers.add("Content-Range: bytes " + ranB + "-" + ranE + "/" + siz);
            if (!checkNoHeaders(s)) {
                sendRespHeader("206 partial", ranE - ranB + 1, cfgInit.findMimeType(a));
            }
            pos = ranB;
            siz = ranE + 1;
        }
        if (gotHead) {
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
                pipe.setClose();
                break;
            }
            if (pipe.morePut(buf, 0, rndi) != rndi) {
                pipe.setClose();
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

    private boolean sendOneFile(String s, String a) {
        if ((gotHost.allowScript & 8) != 0) {
            cfgScrpt scr = cfgAll.scrptFind(s, false);
            if (scr != null) {
                return sendOneScript(scr.getText());
            }
        }
        if ((gotHost.allowMarkdown) && a.equals(".md")) {
            return sendOneMarkdown(s);
        }
        if ((gotHost.allowApi != 0) && s.startsWith(".api./")) {
            return sendOneApi(s);
        }
        if ((gotHost.allowScript != 0) && a.equals(".tcl")) {
            return sendOneScript(s);
        }
        if ((gotHost.allowClass != null) && a.equals(".class")) {
            return sendOneClass(s);
        }
        if ((gotHost.allowImgMap) && a.equals(".imgmap")) {
            return sendOneImgMap(s);
        }
        if ((gotHost.allowMediaStrm) && a.startsWith(".stream-")) {
            return sendOneStream(s, "." + a.substring(8, a.length()));
        }
        if ((gotHost.allowMediaStrm) && a.startsWith(".motion-")) {
            return sendOneMotion(s, "." + a.substring(8, a.length()));
        }
        return sendBinFile(gotHost.path + s, a, gotHost.speedLimit);
    }

    private boolean sendOneDir(String s) {
        if (gotHost.autoIndex) {
            if (!sendOneFile(s + "index.html", ".html")) {
                return false;
            }
            if (!sendOneFile(s + "index.txt", ".txt")) {
                return false;
            }
            if (!sendOneFile(s + "index.md", ".md")) {
                return false;
            }
            if (!sendOneFile(s + "index.class", ".class")) {
                return false;
            }
            if (!sendOneFile(s + "index.tcl", ".tcl")) {
                return false;
            }
        }
        if (gotHost.allowList == 0) {
            sendRespError(404, "not found");
            return true;
        }
        File[] fl = userFlash.dirList(gotHost.path + s);
        if (fl == null) {
            return true;
        }
        String rsp = servHttp.htmlHead + getStyle() + "<title>dirlist</title></head><body>\n";
        if ((gotHost.allowList & 2) != 0) {
            rsp += markDown.txt2html(bits.txt2buf(gotHost.path + s + "readme.txt"));
            rsp += markDown.md2html(bits.txt2buf(gotHost.path + s + "readme.md"));
        }
        rsp += "<b>directory listing of " + gotHost.host + "/" + s + " at " + cfgAll.getFqdn() + ":</b><br/><br/>\n";
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
            if ((gotHost.allowList & 4) != 0) {
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
            rsp += "<tr><td>" + bits.time2str(cfgAll.timeZoneName, mod, 3) + "</td><td>" + c + "</td><td><a href=\"" + a + "\">" + b + "</a></td></tr>\n";
        }
        rsp += "</tbody></table><br/>\n";
        if ((gotHost.allowList & 4) != 0) {
            rsp += "<br/><table><thead><tr><td><b>extension</b></td><td><b>count</b></td><td><b>bytes</b></td><td><b>smallest</b></td><td><b>biggest</b></td><td><b>oldest</b></td><td><b>newest</b></td></tr></thead><tbody>\n";
            rsp += totalD;
            rsp += totalF;
            for (int i = 0; i < stats.size(); i++) {
                rsp += stats.get(i);
            }
            rsp += "</tbody></table><br/>";
        }
        rsp += "<i>generated by </i><b>" + version.namVer + "</b>.</body></html>\n";
        sendTextHeader("200 ok", "text/html", rsp.getBytes());
        dumpXml(rsp);
        return false;
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

    private String decodeAuth(String got, boolean usr) {
        if (got == null) {
            return null;
        }
        int i = got.indexOf(" ");
        if (i < 0) {
            return null;
        }
        got = got.substring(i, got.length()).trim();
        got = cryBase64.decodeString(got);
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

    private boolean checkUserAuth(String got) {
        if (got == null) {
            return true;
        }
        authResult res = gotHost.authenticList.authUserPass(
                decodeAuth(got, true), decodeAuth(got, false));
        if (res.result != authResult.authSuccessful) {
            return true;
        }
        return false;
    }

    private void dumpXml(String s) {
        if (!debugger.servHttpXml) {
            return;
        }
        dumpXml(extMrkLng.parseOne(s.replaceAll("\r", "").replaceAll("\n", "")));
    }

    private void dumpXml(extMrkLng xml) {
        if (!debugger.servHttpXml) {
            return;
        }
        List<String> l = xml.show();
        for (int i = 0; i < l.size(); i++) {
            logger.debug("xml " + l.get(i));
        }
    }

    private boolean readRequest() {
        gotUrl = uniResLoc.parseOne("null://");
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
                uniResLoc srv = uniResLoc.parseOne("http://" + s + "/");
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
            uniResLoc srv = uniResLoc.parseOne("http://x/y?" + s);
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

    private void doTranslate(uniResLoc srvUrl) {
        if (gotHost.translate == null) {
            return;
        }
        String a = cfgTrnsltn.doTranslate(gotHost.translate, gotUrl.toURL(true, true, true));
        srvUrl.fromString(a);
    }

    private void doSubconn(uniResLoc srvUrl) {
        if ((gotHost.subconn & 0x1) == 0) {
            srvUrl.filPath = gotUrl.filPath;
        }
        if ((gotHost.subconn & 0x2) == 0) {
            srvUrl.filName = gotUrl.filName;
        }
        if ((gotHost.subconn & 0x4) == 0) {
            srvUrl.filExt = gotUrl.filExt;
        }
        if ((gotHost.subconn & 0x8) == 0) {
            srvUrl.param = gotUrl.param;
        }
        if ((gotHost.subconn & 0x10) != 0) {
            srvUrl.username = gotUrl.username;
            srvUrl.password = gotUrl.password;
        }
        if ((gotHost.subconn & 0x20) != 0) {
            srvUrl.server = gotUrl.server;
        }
        if ((gotHost.subconn & 0x40) != 0) {
            srvUrl.filPath = (srvUrl.filPath + "/" + gotUrl.filPath).replaceAll("//", "/");
        }
    }

    private void serveRequest() {
        gotHost = lower.findHost(gotUrl.server);
        if (gotCmd.equals("connect")) {
            if (gotHost != null) {
                if (gotHost.allowAnyconn != null) {
                    servHttpAnyconn ntry = new servHttpAnyconn(pipe, this);
                    ntry.ifc = gotHost.allowAnyconn.cloneStart(ntry);
                    headers.add("X-CSTP-Version: 1");
                    if (ntry.ifc.ip4polA != null) {
                        headers.add("X-CSTP-Address: " + ntry.ifc.ip4polA);
                        headers.add("X-CSTP-Netmask: 255.255.255.255");
                    }
                    if (ntry.ifc.ip6polA != null) {
                        headers.add("X-CSTP-Address-IP6: " + ntry.ifc.ip6polA + "/128");
                    }
                    headers.add("X-CSTP-Keep: false");
                    headers.add("X-CSTP-Lease-Duration: 43200");
                    headers.add("X-CSTP-MTU: 1500");
                    headers.add("X-CSTP-DPD: 300");
                    headers.add("X-CSTP-Disconnected-Timeout: 2100");
                    headers.add("X-CSTP-Idle-Timeout: 2100");
                    headers.add("X-CSTP-Session-Timeout: 0");
                    headers.add("X-CSTP-Keepalive: 30");
                    sendRespHeader("200 ok", -1, null);
                    ntry.doStart();
                    gotKeep = false;
                    pipe = null;
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
        gotHost.askNum++;
        gotHost.askTim = bits.getTime();
        if (gotHost.logging) {
            logger.info(peer + " accessed " + gotUrl.toURL(true, false, true));
        }
        if (gotHost.allowForti != null) {
            if (gotUrl.toPathName().equals("remote/logincheck")) {
                headers.add("Set-Cookie: SVPNCOOKIE=" + cryBase64.encodeString(gotUrl.getParam("username") + "|" + gotUrl.getParam("credential")) + "; path=/; secure; httponly");
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
            a = cryBase64.decodeString(a.substring(i + 1, a.length()));
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
                headers.add("Location: " + gotUrl.toURL(true, false, false) + "webvpn.html");
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
                sendTextHeader("200 ok", "text/xml", (extMrkLng.header + "\n<vpn rev=\"1.0\">\n</vpn>\n").getBytes());
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
            uniResLoc srv = uniResLoc.parseOne("http://x/y?" + s);
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
                sendTextHeader("200 ok", "text/xml", (extMrkLng.header + "\n<auth id=\"main\"><title>login</title><message>enter username and password</message><form method=\"post\" action=\"webvpn.html\"><input type=\"text\" label=\"username:\" name=\"username\" value=\"\" /><input type=\"password\" label=\"password:\" name=\"password\" value=\"\" /><input type=\"submit\" name=\"login\" value=\"login\" /></form></auth>").getBytes());
                return;
            }
            headers.add("Set-Cookie: webvpn=00@0168430307@00071@3702439125@3326207229@defctx; path=/; Secure");
            headers.add("Set-Cookie: webvpnc=bu:0/&p:t&iu:1/&sh:%s; path=/; Secure");
            sendTextHeader("200 ok", "text/xml", (extMrkLng.header + "\n<auth id=\"success\"><title>vpn</title><message>success</message><success/></auth>").getBytes());
            return;
        }
        if (gotHost.authenticList != null) {
            if (checkUserAuth(gotAuth)) {
                headers.add("WWW-Authenticate: Basic realm=login");
                sendRespError(401, "unauthorized");
                return;
            }
            gotAuth = decodeAuth(gotAuth, true);
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
            List<uniResLoc> urls = new ArrayList<uniResLoc>();
            for (;;) {
                String a = cmd.word();
                if (a.length() < 1) {
                    break;
                }
                uniResLoc srvUrl = uniResLoc.parseOne(a);
                doTranslate(srvUrl);
                doSubconn(srvUrl);
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
                fin.linePut(gotCmd.toUpperCase() + " " + urls.get(i).toURL(false, false, true) + " HTTP/1.1");
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
            uniResLoc srvUrl = uniResLoc.parseOne(gotHost.reconnT);
            doTranslate(srvUrl);
            doSubconn(srvUrl);
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
                logger.debug("reconnect " + srvUrl.toURL(true, false, true));
            }
            pipeSide.modTyp old = cnn.lineTx;
            cnn.lineTx = pipeSide.modTyp.modeCRLF;
            cnn.linePut(gotCmd.toUpperCase() + " " + srvUrl.toURL(false, false, true) + " HTTP/1.1");
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
            uniResLoc srvUrl = uniResLoc.parseOne(gotHost.redir);
            doTranslate(srvUrl);
            doSubconn(srvUrl);
            sendFoundAt(srvUrl.toURL(true, true, true));
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
            extMrkLng xml = extMrkLng.parseOne(a);
            dumpXml(xml);
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
                for (int i = 0; i < fl.length; i++) {
                    a += webdavProp(pn + fl[i].getName(), fl[i], typ, len, tag, mod, crt, dsp, cnt, usd, fre);
                }
            } else {
                a += webdavProp(pn, new File(gotHost.path + pn), typ, len, tag, mod, crt, dsp, cnt, usd, fre);
            }
            a += "</D:multistatus>\n";
            sendRespHeader("207 multi-status", a.length(), "text/xml");
            pipe.strPut(a);
            dumpXml(a);
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
            dumpXml(a);
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
            uniResLoc url = uniResLoc.parseOne(gotDstntn);
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
            uniResLoc url = uniResLoc.parseOne(gotDstntn);
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
            updateVisitors(pn);
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
        updateVisitors(pn);
        boolean b = true;
        if ((gotHost.allowWebSck) && (gotWebsock != null)) {
            if (!sendOneWebSck(pn)) {
                return;
            }
        }
        if (gotUrl.toFileName().length() > 0) {
            b = sendOneFile(pn, gotUrl.filExt);
        } else {
            b = sendOneDir(pn);
        }
        if (b) {
            sendRespError(404, "not found");
            return;
        }
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

class servHttpSstp implements Runnable, ifcDn {

    public counter cntr = new counter();

    private pipeSide pipe;

    private ifcUp upper = new ifcNull();

    private cfgIfc ifc = null;

    private servHttpConn lower;

    public servHttpSstp(pipeSide conn, servHttpConn parent) {
        lower = parent;
        pipe = conn;
        new Thread(this).start();
    }

    public void run() {
        try {
            doStart();
        } catch (Exception e) {
            logger.traceback(e);
        }
        pipe.setClose();
        if (ifc != null) {
            ifc.cloneStop();
        }
    }

    public void doStart() {
        packSstp pckS = new packSstp(pipe);
        packHolder pckB = pckS.recvPack();
        if (pckB == null) {
            return;
        }
        if (pckS.parseCtrl(pckB)) {
            return;
        }
        if (debugger.servHttpTraf) {
            logger.debug("rx " + pckS.dump());
        }
        if (pckS.parseConnReq()) {
            return;
        }
        pckS.fillConnAck();
        pckS.createConnAck(pckB);
        pckS.sendCtrl(pckB);
        if (debugger.servHttpTraf) {
            logger.debug("tx " + pckS.dump());
        }
        ifc = lower.gotHost.allowSstp.cloneStart(this);
        for (;;) {
            if (doRound()) {
                break;
            }
        }
    }

    public boolean doRound() {
        packSstp pckS = new packSstp(pipe);
        packHolder pckB = pckS.recvPack();
        if (pckB == null) {
            return true;
        }
        if (pckS.control) {
            pckS.parseCtrl(pckB);
            if (debugger.servHttpTraf) {
                logger.debug("rx " + pckS.dump());
            }
            return false;
        }
        upper.recvPack(pckB);
        return false;
    }

    public void sendPack(packHolder pck) {
        packSstp pckS = new packSstp(pipe);
        pck.putDefaults();
        pckS.sendData(pck);
    }

    public counter getCounter() {
        return cntr;
    }

    public void closeDn() {
        pipe.setClose();
    }

    public void flapped() {
        pipe.setClose();
    }

    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
    }

    public state.states getState() {
        return state.states.up;
    }

    public void setFilter(boolean promisc) {
    }

    public addrType getHwAddr() {
        return new addrEmpty();
    }

    public int getMTUsize() {
        return 1504;
    }

    public long getBandwidth() {
        return 8000000;
    }

    public String toString() {
        return "sstp";
    }

}

class servHttpAnyconn implements Runnable, ifcDn {

    public counter cntr = new counter();

    public cfgIfc ifc = null;

    private pipeSide pipe;

    private ifcUp upper = new ifcNull();

    private servHttpConn lower;

    public servHttpAnyconn(pipeSide conn, servHttpConn parent) {
        lower = parent;
        pipe = conn;
    }

    public void doStart() {
        new Thread(this).start();
    }

    public void run() {
        if (ifc.ip4polA != null) {
            ifc.addr4changed(ifc.addr4, ifc.mask4, ifc.ip4polA);
        }
        if (ifc.ip6polA != null) {
            ifc.addr6changed(ifc.addr6, ifc.mask6, ifc.ip6polA);
        }
        try {
            for (;;) {
                if (doRound()) {
                    break;
                }
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        pipe.setClose();
        ifc.cloneStop();
    }

    public boolean doRound() {
        packAnyconn pckS = new packAnyconn(pipe);
        packHolder pckB = new packHolder(true, true);
        if (pckS.recvPack(pckB)) {
            return true;
        }
        switch (pckS.msgTyp) {
            case packAnyconn.typData:
                int i = ifcEther.guessEtherType(pckB);
                if (i < 0) {
                    logger.info("got bad protocol from " + lower.peer);
                    break;
                }
                pckB.msbPutW(0, i); // ethertype
                pckB.putSkip(2);
                pckB.merge2beg();
                upper.recvPack(pckB);
                break;
            case packAnyconn.typDpdReq:
                pckS.msgTyp = packAnyconn.typDpdRep;
                pckS.sendPack(pckB);
                break;
            case packAnyconn.typDpdRep:
                break;
            case packAnyconn.typKeep:
                break;
            case packAnyconn.typTerm:
                return true;
            case packAnyconn.typDisc:
                return true;
            default:
                logger.info("unknown type: " + pckS.msgTyp);
                break;
        }
        return false;
    }

    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        if (ifcEther.stripEtherType(pck)) {
            cntr.drop(pck, counter.reasons.badProto);
            return;
        }
        packAnyconn ps = new packAnyconn(pipe);
        ps.msgTyp = packAnyconn.typData;
        pck.putDefaults();
        ps.sendPack(pck);
    }

    public counter getCounter() {
        return cntr;
    }

    public void closeDn() {
        pipe.setClose();
    }

    public void flapped() {
        pipe.setClose();
    }

    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
    }

    public state.states getState() {
        return state.states.up;
    }

    public void setFilter(boolean promisc) {
    }

    public addrType getHwAddr() {
        return new addrEmpty();
    }

    public int getMTUsize() {
        return 1504;
    }

    public long getBandwidth() {
        return 8000000;
    }

    public String toString() {
        return "anyconn";
    }

}

class servHttpForti implements Runnable, ifcDn {

    public counter cntr = new counter();

    public cfgIfc ifc = null;

    private pipeSide pipe;

    private ifcUp upper = new ifcNull();

    public servHttpForti(pipeSide conn) {
        pipe = conn;
    }

    public void doStart() {
        new Thread(this).start();
    }

    public void run() {
        if (ifc.ip4polA != null) {
            ifc.addr4changed(ifc.addr4, ifc.mask4, ifc.ip4polA);
        }
        if (ifc.ip6polA != null) {
            ifc.addr6changed(ifc.addr6, ifc.mask6, ifc.ip6polA);
        }
        try {
            for (;;) {
                if (doRound()) {
                    break;
                }
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        pipe.setClose();
        ifc.cloneStop();
    }

    public boolean doRound() {
        packForti pckS = new packForti(pipe);
        packHolder pckB = new packHolder(true, true);
        if (pckS.recvPack(pckB)) {
            return true;
        }
        pckB.msbPutW(0, 0xff03);
        pckB.putSkip(2);
        pckB.merge2beg();
        upper.recvPack(pckB);
        return false;
    }

    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        packForti ps = new packForti(pipe);
        pck.putDefaults();
        pck.getSkip(2);
        ps.sendPack(pck);
    }

    public counter getCounter() {
        return cntr;
    }

    public void closeDn() {
        pipe.setClose();
    }

    public void flapped() {
        pipe.setClose();
    }

    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
    }

    public state.states getState() {
        return state.states.up;
    }

    public void setFilter(boolean promisc) {
    }

    public addrType getHwAddr() {
        return new addrEmpty();
    }

    public int getMTUsize() {
        return 1504;
    }

    public long getBandwidth() {
        return 8000000;
    }

    public String toString() {
        return "forti";
    }

}

class servHttpDirs implements Comparator<servHttpDirs> {

    public String name;

    public int count;

    public long bytes;

    public long sizMin;

    public long sizMax;

    public long timMin;

    public long timMax;

    public servHttpDirs(String n) {
        name = n;
        sizMin = Long.MAX_VALUE;
        sizMax = Long.MIN_VALUE;
        timMin = Long.MAX_VALUE;
        timMax = Long.MIN_VALUE;
    }

    public int compare(servHttpDirs o1, servHttpDirs o2) {
        return o1.name.compareTo(o2.name);
    }

    public String toString() {
        String smi = "" + sizMin;
        String sma = "" + sizMax;
        String tmi = bits.time2str(cfgAll.timeZoneName, timMin, 3);
        String tma = bits.time2str(cfgAll.timeZoneName, timMax, 3);
        if (count < 1) {
            smi = "-";
            sma = "-";
            tmi = "-";
            tma = "-";
        }
        return "<tr><td>" + name + "</td><td>" + count + "</td><td>" + bytes + "</td><td>" + smi + "</td><td>" + sma + "</td><td>" + tmi + "</td><td>" + tma + "</td></tr>\n";
    }

    public void update(long siz, long tim) {
        count++;
        bytes += siz;
        if (siz < sizMin) {
            sizMin = siz;
        }
        if (siz > sizMax) {
            sizMax = siz;
        }
        if (tim < timMin) {
            timMin = tim;
        }
        if (tim > timMax) {
            timMax = tim;
        }
    }

}
