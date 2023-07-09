package net.freertr.serv;

import java.net.URI;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.List;
import java.util.zip.Deflater;
import net.freertr.cfg.cfgAceslst;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgAuther;
import net.freertr.cfg.cfgProxy;
import net.freertr.cfg.cfgTrnsltn;
import net.freertr.clnt.clntProxy;
import net.freertr.cry.cryHashCrc32;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServS;
import net.freertr.enc.encUrl;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabRouteIface;
import net.freertr.user.userFilter;
import net.freertr.user.userFormat;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;

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
    protected tabGen<servHttpHost> hosts = new tabGen<servHttpHost>();

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
     * default server path
     */
    protected String defaultPath;

    /**
     * default subconnect
     */
    protected int defaultSubcon;

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
        "server http .*! no def-path",
        "server http .*! no def-subconn",
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
    protected servHttpHost findHost(String s) {
        servHttpHost ntry = new servHttpHost(s);
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

    private final static int string2subconn(boolean neg, cmds cmd) {
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

    private final static String subconn2string(int subconn) {
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
        cmds.cfgLine(l, defaultPath == null, beg, "def-path", "" + defaultPath);
        cmds.cfgLine(l, defaultSubcon == 0, beg, "def-subconn", "" + subconn2string(defaultSubcon));
        for (int hn = 0; hn < hosts.size(); hn++) {
            servHttpHost ntry = hosts.get(hn);
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
                String s = subconn2string(ntry.subconn);
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
            if (ntry.searchScript != null) {
                l.add(a + " search-script " + ntry.searchScript);
            }
            if (ntry.allowScript != 0) {
                String s = "";
                if ((ntry.allowScript & 2) != 0) {
                    s += " exec";
                }
                if ((ntry.allowScript & 4) != 0) {
                    s += " config";
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
            if (ntry.accessList != null) {
                l.add(a + " access-class " + ntry.accessList.listName);
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
        if (a.equals("def-path")) {
            if (negated) {
                defaultPath = null;
            } else {
                defaultPath = cmd.word();
            }
            return false;
        }
        if (a.equals("def-subconn")) {
            defaultSubcon = string2subconn(negated, cmd);
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
        servHttpHost ntry = new servHttpHost(cmd.word());
        servHttpHost old = hosts.add(ntry);
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
            ntry.path = "/" + encUrl.normalizePath(cmd.word() + "/");
            return false;
        }
        if (ntry.path == null) {
            if (defaultPath == null) {
                ntry.path = "/data/notfound/";
            } else {
                ntry.path = defaultPath;
            }
            ntry.subconn = defaultSubcon;
        }
        if (a.equals("style")) {
            a = cmd.getRemaining();
            if (negated) {
                if (ntry.style == null) {
                    return false;
                }
                ntry.style.remove(a);
                return false;
            }
            if (ntry.style == null) {
                ntry.style = new ArrayList<String>();
            }
            if (ntry.style.indexOf(a) >= 0) {
                return false;
            }
            ntry.style.add(a);
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
            ntry.subconn = string2subconn(negated, cmd);
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
        if (a.equals("search-script")) {
            if (negated) {
                ntry.searchScript = null;
                return false;
            }
            ntry.searchScript = cmd.word();
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
                URL url = new URI("file://" + ntry.path).toURL();
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
            ntry.backupPath = "/" + encUrl.normalizePath(cmd.word() + "/");
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
            if (ntry.allowSstp.type != tabRouteIface.ifaceType.dialer) {
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
            if (ntry.allowAnyconn.type != tabRouteIface.ifaceType.dialer) {
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
            if (ntry.allowForti.type != tabRouteIface.ifaceType.dialer) {
                cmd.error("not dialer interface");
                ntry.allowForti = null;
                return false;
            }
            return false;
        }
        if (a.equals("access-class")) {
            if (negated) {
                ntry.accessList = null;
                return false;
            }
            cfgAceslst lst = cfgAll.aclsFind(cmd.word(), false);
            if (lst == null) {
                cmd.error("no such access list");
                return false;
            }
            ntry.accessList = lst.aceslst;
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
        l.add(lst, "2 3     <name:loc>                   name of server, * for any");
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
        l.add(null, "3 4,.    api                        allow api calls");
        l.add(null, "4 4,.      exec                     allow exec commands");
        l.add(null, "4 4,.      config                   allow config commands");
        l.add(null, "3 4      search-script              allow scripts defined in configuration");
        l.add(null, "4 .        <str>                    prefix");
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
        l.add(null, "3 4      access-class               require ip to access");
        l.add(null, "4 .        <name:acl>               access list");
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
            servHttpHost ntry = hosts.get(i);
            res.add(ntry.host + "|" + ntry.askNum + "|" + bits.timePast(ntry.askTim));
        }
        return res;
    }

}
