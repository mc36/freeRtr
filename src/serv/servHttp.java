package serv;

import addr.addrEmpty;
import addr.addrIP;
import addr.addrType;
import auth.authGeneric;
import auth.authResult;
import cfg.cfgAll;
import cfg.cfgAuther;
import cfg.cfgIfc;
import cfg.cfgInit;
import cfg.cfgProxy;
import clnt.clntProxy;
import cry.cryBase64;
import ifc.ifcDn;
import ifc.ifcEther;
import ifc.ifcNull;
import ifc.ifcUp;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.RandomAccessFile;
import java.lang.reflect.Method;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import pack.packAnyconn;
import pack.packHolder;
import pack.packSstp;
import pipe.pipeConnect;
import pipe.pipeDiscard;
import pipe.pipeLine;
import pipe.pipeSide;
import prt.prtGenConn;
import prt.prtServS;
import sec.secWebsock;
import tab.tabGen;
import user.userFilter;
import user.userFlash;
import user.userHelping;
import user.userScript;
import user.userTerminal;
import util.bits;
import util.cmds;
import util.counter;
import util.debugger;
import util.extMrkLng;
import util.logger;
import util.state;
import util.uniResLoc;
import util.version;

/**
 * hypertext transfer protocol (rfc2616) server
 *
 * @author matecsaba
 */
public class servHttp extends servGeneric implements prtServS {

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
    public static final String html401tr = "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">";

    /**
     * list of hosts
     */
    protected tabGen<servHttpServ> hosts = new tabGen<servHttpServ>();

    /**
     * proxy to use
     */
    protected clntProxy proxy;

    /**
     * error message
     */
    protected String error;

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "server http .*! port " + clearPort,
        "server http .*! protocol " + proto2string(protoAllStrm),
        "server http .*! no proxy",
        "server http .*! no error",
        "server http .*! host .* nostyle",
        "server http .*! host .* noredir",
        "server http .*! host .* noreconn",
        "server http .*! host .* nologging",
        "server http .*! host .* nosubconn",
        "server http .*! host .* noimagemap",
        "server http .*! host .* nowebsock",
        "server http .*! host .* nomediastream",
        "server http .*! host .* nostream",
        "server http .*! host .* nomultiacc",
        "server http .*! host .* nodirlist",
        "server http .*! host .* noscript",
        "server http .*! host .* noclass",
        "server http .*! host .* noupload",
        "server http .*! host .* nobackup",
        "server http .*! host .* nowebdav",
        "server http .*! host .* nosstp",
        "server http .*! host .* noanyconn",
        "server http .*! host .* noauthentication"
    };

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
        ntry = new servHttpServ();
        ntry.host = "*";
        return hosts.find(ntry);
    }

    /**
     * start connection
     *
     * @param pipe pipeline
     * @param id connection
     * @return false on success, true on error
     */
    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.timeout = 120000;
        new servHttpConn(this, pipe, id);
        return false;
    }

    /**
     * get config
     *
     * @param beg beginning
     * @param l list
     */
    public void srvShRun(String beg, List<String> l) {
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
        for (int hn = 0; hn < hosts.size(); hn++) {
            servHttpServ ntry = hosts.get(hn);
            if (ntry == null) {
                continue;
            }
            String a = beg + "host " + ntry.host;
            l.add(a + " path " + ntry.path);
            if (ntry.style != null) {
                l.add(a + " style " + ntry.style);
            } else {
                l.add(a + " nostyle");
            }
            if (ntry.redir != null) {
                l.add(a + " redir " + ntry.redir);
            } else {
                l.add(a + " noredir");
            }
            if (ntry.logging) {
                l.add(a + " logging");
            } else {
                l.add(a + " nologging");
            }
            if (ntry.reconnT != null) {
                l.add(a + " reconn " + ntry.reconnP.name + " " + ntry.reconnT);
            } else {
                l.add(a + " noreconn");
            }
            if (ntry.subconn != 0) {
                l.add(a + " subconn " + ntry.subconn);
            } else {
                l.add(a + " nosubconn");
            }
            if (ntry.streamT == null) {
                l.add(a + " nostream");
            } else {
                l.add(a + " stream " + ntry.streamM + " " + ntry.streamP.name
                        + " " + ntry.streamT);
            }
            if (ntry.multiAccT == null) {
                l.add(a + " nomultiacc");
            } else {
                l.add(a + " multiacc " + ntry.multiAccP.name + " "
                        + ntry.multiAccT);
            }
            if (ntry.allowList) {
                l.add(a + " dirlist");
            } else {
                l.add(a + " nodirlist");
            }
            if (ntry.allowScript) {
                l.add(a + " script");
            } else {
                l.add(a + " noscript");
            }
            if (ntry.allowImgMap) {
                l.add(a + " imagemap");
            } else {
                l.add(a + " noimagemap");
            }
            if (ntry.allowWebSck) {
                l.add(a + " websock");
            } else {
                l.add(a + " nowebsock");
            }
            if (ntry.allowWebDav) {
                l.add(a + " webdav");
            } else {
                l.add(a + " nowebdav");
            }
            if (ntry.allowMediaStrm) {
                l.add(a + " mediastream");
            } else {
                l.add(a + " nomediastream");
            }
            if (ntry.allowClass != null) {
                l.add(a + " class");
            } else {
                l.add(a + " noclass");
            }
            if (ntry.allowUpload) {
                l.add(a + " upload");
            } else {
                l.add(a + " noupload");
            }
            if (ntry.backupPath != null) {
                l.add(a + " backup " + ntry.backupCount + " " + ntry.backupPath);
            } else {
                l.add(a + " nobackup");
            }
            if (ntry.allowSstp == null) {
                l.add(a + " nosstp");
            } else {
                l.add(a + " sstp " + ntry.allowSstp.name);
            }
            if (ntry.allowAnyconn == null) {
                l.add(a + " noanyconn");
            } else {
                l.add(a + " anyconn " + ntry.allowAnyconn.name);
            }
            if (ntry.authenticList != null) {
                l.add(a + " authentication " + ntry.authenticList.autName);
            } else {
                l.add(a + " noauthentication");
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
        if (a.equals("delhost")) {
            servHttpServ ntry = new servHttpServ();
            ntry.host = cmd.word();
            if (hosts.del(ntry) == null) {
                cmd.error("no such server");
                return false;
            }
            return false;
        }
        if (a.equals("proxy")) {
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
            return false;
        }
        if (a.equals("no")) {
            a = cmd.word();
            if (a.equals("proxy")) {
                proxy = null;
                return false;
            }
            if (a.equals("error")) {
                error = null;
                return false;
            }
            cmd.badCmd();
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
            ntry.path = "/" + uniResLoc.normalizePath(cmd.word() + "/");
            return false;
        }
        if (a.equals("nopath")) {
            ntry.path = "/";
            return false;
        }
        if (a.equals("style")) {
            ntry.style = cmd.getRemaining();
            return false;
        }
        if (a.equals("nostyle")) {
            ntry.style = null;
            return false;
        }
        if (a.equals("redir")) {
            ntry.redir = cmd.word();
            return false;
        }
        if (a.equals("noredir")) {
            ntry.redir = null;
            return false;
        }
        if (a.equals("logging")) {
            ntry.logging = true;
            return false;
        }
        if (a.equals("nologging")) {
            ntry.logging = false;
            return false;
        }
        if (a.equals("reconn")) {
            cfgProxy prx = cfgAll.proxyFind(cmd.word(), false);
            if (prx == null) {
                cmd.error("no such proxy");
                return false;
            }
            ntry.reconnP = prx.proxy;
            ntry.reconnT = cmd.word();
            return false;
        }
        if (a.equals("noreconn")) {
            ntry.reconnP = null;
            ntry.reconnT = null;
            return false;
        }
        if (a.equals("subconn")) {
            ntry.subconn = bits.str2num(cmd.word());
            return false;
        }
        if (a.equals("nosubconn")) {
            ntry.subconn = 0;
            return false;
        }
        if (a.equals("stream")) {
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
        if (a.equals("nostream")) {
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
        if (a.equals("multiacc")) {
            cfgProxy prx = cfgAll.proxyFind(cmd.word(), false);
            if (prx == null) {
                cmd.error("no such proxy");
                return false;
            }
            ntry.multiAccP = prx.proxy;
            ntry.multiAccT = cmd.getRemaining();
            return false;
        }
        if (a.equals("nomultiacc")) {
            ntry.multiAccP = null;
            ntry.multiAccT = null;
            return false;
        }
        if (a.equals("dirlist")) {
            ntry.allowList = true;
            return false;
        }
        if (a.equals("nodirlist")) {
            ntry.allowList = false;
            return false;
        }
        if (a.equals("script")) {
            ntry.allowScript = true;
            return false;
        }
        if (a.equals("noscript")) {
            ntry.allowScript = false;
            return false;
        }
        if (a.equals("imagemap")) {
            ntry.allowImgMap = true;
            return false;
        }
        if (a.equals("websock")) {
            ntry.allowWebSck = true;
            return false;
        }
        if (a.equals("webdav")) {
            ntry.allowWebDav = true;
            return false;
        }
        if (a.equals("noimagemap")) {
            ntry.allowImgMap = false;
            return false;
        }
        if (a.equals("nowebsock")) {
            ntry.allowWebSck = false;
            return false;
        }
        if (a.equals("nowebdav")) {
            ntry.allowWebDav = false;
            return false;
        }
        if (a.equals("mediastream")) {
            ntry.allowMediaStrm = true;
            return false;
        }
        if (a.equals("nomediastream")) {
            ntry.allowMediaStrm = false;
            return false;
        }
        if (a.equals("class")) {
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
        if (a.equals("noclass")) {
            ntry.allowClass = null;
            return false;
        }
        if (a.equals("upload")) {
            ntry.allowUpload = true;
            return false;
        }
        if (a.equals("noupload")) {
            ntry.allowUpload = false;
            return false;
        }
        if (a.equals("backup")) {
            ntry.backupCount = bits.str2num(cmd.word());
            ntry.backupPath = "/" + uniResLoc.normalizePath(cmd.word() + "/");
            return false;
        }
        if (a.equals("nobackup")) {
            ntry.backupCount = 0;
            ntry.backupPath = null;
            return false;
        }
        if (a.equals("sstp")) {
            ntry.allowSstp = cfgAll.ifcFind(cmd.word(), false);
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
        if (a.equals("nosstp")) {
            ntry.allowSstp = null;
            return false;
        }
        if (a.equals("anyconn")) {
            ntry.allowAnyconn = cfgAll.ifcFind(cmd.word(), false);
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
        if (a.equals("noanyconn")) {
            ntry.allowAnyconn = null;
            return false;
        }
        if (a.equals("authentication")) {
            cfgAuther lst = cfgAll.autherFind(cmd.word(), null);
            if (lst == null) {
                cmd.error("no such auth list");
                return false;
            }
            ntry.authenticList = lst.getAuther();
            return false;
        }
        if (a.equals("noauthentication")) {
            ntry.authenticList = null;
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
        l.add("1 2  delhost                        delete one virtual server");
        l.add("2 .    <name>                       name of server");
        l.add("1 2  proxy                          enable proxy support");
        l.add("2 .    <name>                       proxy profile");
        l.add("1 2  error                          set error message");
        l.add("2 2,.  <name>                       error message");
        l.add("1 2  host                           define one virtual server");
        l.add("2 3,.  <name>                       name of server, * for any");
        l.add("3 4      path                       set server root");
        l.add("4 .        <name>                   root directory of server");
        l.add("3 .      nopath                     clear server root");
        l.add("3 4      redir                      set redirect path");
        l.add("4 .        <url>                    url to redirect to");
        l.add("3 .      noredir                    disable redirect");
        l.add("3 .      logging                    log to syslog");
        l.add("3 .      nologging                  stop logging to syslog");
        l.add("3 4      reconn                     reconnect to server");
        l.add("4 5        <name>                   proxy profile");
        l.add("5 .          <name>                 server to redirect to");
        l.add("3 .      noreconn                   disable reconnect");
        l.add("3 4      subconn                    reconnect only to the url");
        l.add("4 .        <num>                    bitmask what to revert");
        l.add("3 .      nosubconn                  allow anything");
        l.add("3 4      stream                     stream from server");
        l.add("4 5        <name>                   content type");
        l.add("5 6          <name>                 proxy profile");
        l.add("6 .            <name>               server to stream from");
        l.add("3 .      nostream                   disable streaming");
        l.add("3 4      multiacc                   access multiple servers");
        l.add("4 5        <name>                   proxy profile");
        l.add("5 5,.        <name>                 server to access");
        l.add("3 .      nomultiacc                 disable multiple access");
        l.add("3 .      dirlist                    allow directory listing");
        l.add("3 .      nodirlist                  forbid directory listing");
        l.add("3 .      script                     allow script running");
        l.add("3 .      noscript                   forbid script running");
        l.add("3 .      imagemap                   allow image map processing");
        l.add("3 .      noimagemap                 forbid image map processing");
        l.add("3 .      websock                    allow websocket processing");
        l.add("3 .      nowebsock                  forbid websocket processing");
        l.add("3 .      webdav                     allow webdav processing");
        l.add("3 .      nowebdav                   forbid webdav processing");
        l.add("3 .      mediastream                allow media streaming");
        l.add("3 .      nomediastream              forbid media streaming");
        l.add("3 .      class                      allow class running");
        l.add("3 .      noclass                    forbid class running");
        l.add("3 .      upload                     allow upload files");
        l.add("3 .      noupload                   forbid upload files");
        l.add("3 4      backup                     backup uploaded files");
        l.add("4 5        <num>                    number of backups to keep");
        l.add("5 .          <name>                 root directory of backup");
        l.add("3 .      nobackup                   overwrite uploaded files");
        l.add("3 4      sstp                       allow sstp clients");
        l.add("4 .        <name>                   name of interface");
        l.add("3 4      anyconn                    allow anyconnect clients");
        l.add("4 .        <name>                   name of interface");
        l.add("3 .      nosstp                     forbid sstp client");
        l.add("3 .      noanyconn                  forbid anyconnect clients");
        l.add("3 .      noconn                     forbid direct connect to server");
        l.add("3 4      authentication             require authentication to access");
        l.add("4 .        <name>                   authentication list");
        l.add("3 .      noauthentication           disable authentication");
        l.add("3 4      style                      set page style tags");
        l.add("4 4,.      <text>                   text to send");
        l.add("3 .      nostyle                    clear page style tags");
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
        return genStrmStart(this, new pipeLine(65536, false), 0);
    }

    /**
     * deinitialize
     *
     * @return false on success, true on error
     */
    public boolean srvDeinit() {
        return genericStop(0);
    }

}

class servHttpServ implements Runnable, Comparator<servHttpServ> {

    /**
     * name of server
     */
    public String host = "";

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
    public String style;

    /**
     * directory listing allowed
     */
    public boolean allowList;

    /**
     * script running allowed
     */
    public boolean allowScript;

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
        addrIP adr = userTerminal.justResolv(srvUrl.server, 0);
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
        cnn.linePut("GET " + srvUrl.toURL(false, true) + " HTTP/1.1");
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

    protected prtGenConn conn;

    protected String gotCmd; // command

    protected boolean gotHead; // just header

    protected uniResLoc gotUrl; // url

    protected String gotAuth; // authentication

    protected servHttpServ gotHost; // requested host

    protected int gotVer; // protocol version

    protected boolean gotKeep; // keepalive

    protected boolean gotDepth; // depth

    protected String gotDstntn; // destination

    protected List<String> gotCook; // rx cookie

    protected byte[] gotBytes; // oontent read

    protected String gotAgent; // user agent

    protected String gotReferer; // referer

    protected String gotWebsock; // websocket key

    protected String gotRange; // range

    private List<String> headers; // tx headers

    public servHttpConn(servHttp parent, pipeSide stream, prtGenConn id) {
        lower = parent;
        pipe = stream;
        conn = id;
        pipe.lineRx = pipeSide.modTyp.modeCRtryLF;
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
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
        if (size >= 0) {
            sendLn("Content-Length: " + size);
        }
        if (!gotKeep) {
            sendLn("Connection: Close");
        } else {
            sendLn("Connection: Keep-Alive");
            sendLn("Keep-Alive: TimeOut=60, Max=25");
        }
        for (int i = 0; i < headers.size(); i++) {
            sendLn(headers.get(i));
        }
        sendLn("");
    }

    private void sendHtmlHeader(String head, String body) {
        sendRespHeader(head, body.length(), "text/html");
        if (gotHead) {
            return;
        }
        pipe.strPut(body);
    }

    private String getStyle() {
        if (gotHost == null) {
            return "";
        }
        if (gotHost.style == null) {
            return "";
        }
        return " " + gotHost.style;
    }

    private void sendRespError(int code, String text) {
        gotKeep = false;
        String s;
        if (lower.error == null) {
            s = servHttp.html401tr
                    + "\n<html><head><title>error</title></head><body"
                    + getStyle() + ">error: " + text + "</body></html>\n";
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
        String s = servHttp.html401tr
                + "\n<html><head><title>moved</title></head><body" + getStyle()
                + ">moved to <a href=\"" + where + "\">" + where
                + "</a>. will be redirected.</body></html>\n";
        headers.add("Location: " + where);
        sendRespHeader("302 found", s.length(), "text/html");
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
        String a = conn.peerAddr
                + ";"
                + bits.time2str(cfgAll.timeZoneName, bits.getTime()
                        + cfgAll.timeServerOffset, 3) + ";"
                + gotAgent.replaceAll(";", ",") + ";"
                + gotReferer.replaceAll(";", ",") + "\n";
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
        addrIP adr = userTerminal.justResolv(l.get(1), 0);
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
        secWebsock wsk = new secWebsock(pipe, new pipeLine(65536, false));
        wsk.binary = l.get(4).equals("bin");
        wsk.startServer();
        pipeConnect.connect(pip, wsk.getPipe(), true);
        gotKeep = false;
        pipe = null;
        return false;
    }

    private boolean sendOneScript(String s) {
        List<String> l = bits.txt2buf(gotHost.path + s);
        if (l == null) {
            return true;
        }
        pipeLine pl = new pipeLine(1024 * 1024, false);
        pipeSide pip = pl.getSide();
        pip.timeout = 60000;
        pip.lineTx = pipeSide.modTyp.modeCRLF;
        pip.lineRx = pipeSide.modTyp.modeCRorLF;
        userScript t = new userScript(pip, "");
        t.addLines(l);
        t.allowConfig = true;
        t.allowExec = true;
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
        pip.linePut("clnt=" + conn.peerAddr);
        for (int i = 0; i < gotUrl.param.size(); i++) {
            pip.linePut("par." + gotUrl.param.get(i));
        }
        for (int i = 0; i < gotCook.size(); i++) {
            pip.linePut("cok." + gotCook.get(i));
        }
        pip.linePut(".");
        t.cmdAll();
        pl.setClose();
        s = pip.strGet(1024 * 1024);
        if (s == null) {
            s = "";
        }
        sendHtmlHeader("200 ok", s);
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
            Class<?> cls = gotHost.allowClass.loadClass(gotUrl.filPath
                    + gotUrl.filName);
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
            obj = mth[o].invoke(obj, gotUrl.toURL(true, false), gotHost.path
                    + s, "" + conn.peerAddr, gotAgent, par, buf);
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
        if (res.length == 0) {
            int i = s.lastIndexOf("\"");
            String a = s.substring(0, i);
            if (i < 0) {
                return true;
            }
            s = s.substring(i + 1, s.length());
            s = parseFileName(s);
            if (!a.startsWith("/")) {
                a = gotHost.path + a;
            }
            return sendBinFile(a, "." + s);
        }
        s = parseFileName(s);
        sendRespHeader("200 ok", res.length, cfgInit.findMimeType(s));
        if (gotHead) {
            return false;
        }
        pipe.morePut(res, 0, res.length);
        return false;
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

    private boolean sendBinFile(String s, String a) {
        RandomAccessFile fr;
        long siz;
        try {
            File f;
            f = new File(s);
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
                ranB = bits.str2num(gotRange.substring(1, gotRange.length()));
                ranE = siz - 1;
            } else {
                ranB = bits.str2num(gotRange.substring(0, i));
                ranE = bits.str2num(gotRange.substring(i + 1, gotRange.length()));
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
        for (; pos < siz;) {
            final int max = 8192;
            long rndl = siz - pos;
            if (rndl > max) {
                rndl = max;
            }
            int rndi = (int) rndl;
            byte buf[] = new byte[rndi];
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
        }
        try {
            fr.close();
        } catch (Exception e) {
        }
        return false;
    }

    private boolean sendOneFile(String s, String a) {
        if ((gotHost.allowScript) && a.equals(".tcl")) {
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
        return sendBinFile(gotHost.path + s, a);
    }

    private boolean sendOneDir(String s) {
        if ((s.lastIndexOf("/") + 1) < s.length()) {
            sendFoundAt(s + "/");
            return false;
        }
        if (!sendOneFile(s + "index.html", ".html")) {
            return false;
        }
        if (!gotHost.allowList) {
            sendRespError(404, "not found");
            return true;
        }
        File[] fl = userFlash.dirList(gotHost.path + s);
        if (fl == null) {
            return true;
        }
        String rsp = servHttp.html401tr
                + "\n<html><head><title>dirlist</title></head><body"
                + getStyle() + ">\n";
        rsp += "directory listing of " + gotHost.host + "/" + s + " at "
                + cfgAll.hostName + ":<br/><br/>\n";
        rsp += "<table border=1><tr><td><b>date</b></td><td><b>size</b></td><td><b>name</b></td></tr>\n";
        rsp += "<tr><td>-</td><td>dir</td><td><a href=\"/\">root</a></td></tr>\n";
        rsp += "<tr><td>-</td><td>dir</td><td><a href=\"../\">parent</a></td></tr>\n";
        for (int i = 0; i < fl.length; i++) {
            File f = fl[i];
            String a = f.getName();
            String b = a;
            String c = "" + f.length();
            if (f.isDirectory()) {
                a += "/";
                c = "dir";
            }
            rsp += "<tr><td>"
                    + bits.time2str(cfgAll.timeZoneName, f.lastModified(), 3)
                    + "</td><td>" + c + "</td><td><a href=\"" + a + "\">" + b
                    + "</a></td></tr>\n";
        }
        rsp += "</table><br/><br/><i>generated by </i><b>" + version.namVer
                + "</b>.</body></html>\n";
        sendHtmlHeader("200 ok", rsp);
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
        if (gotCmd.equals("")) {
            return true;
        }
        if (debugger.servHttpTraf) {
            logger.debug("rx '" + gotCmd + "'");
        }
        int i = gotCmd.toLowerCase().lastIndexOf(" http/");
        if (i > 0) {
            String s = gotCmd.substring(i + 6, gotCmd.length());
            gotCmd = gotCmd.substring(0, i);
            i = s.indexOf(".");
            gotVer = bits.str2num(s.substring(0, i)
                    + s.substring(i + 1, s.length()));
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
        if (!gotUpgrade.toLowerCase().startsWith("tls/")) {
            return false;
        }
        if (lower.noneSecKeys()) {
            return false;
        }
        headers.add("Upgrade: " + gotUpgrade + ", HTTP/1.1");
        sendRespHeader("101 switch protocol", -1, null);
        headers.clear();
        pipeSide res = lower.negoSecSess(pipe, servGeneric.protoTls, new pipeLine(65536, false), null);
        if (res == null) {
            return true;
        }
        res.lineRx = pipeSide.modTyp.modeCRtryLF;
        res.lineTx = pipeSide.modTyp.modeCRLF;
        pipe = res;
        return false;
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
            addrIP adr = userTerminal.justResolv(gotUrl.server, 0);
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
        if (gotHost.logging) {
            logger.info(conn.peerAddr + " accessed " + gotUrl.toURL(false, true));
        }
        if (gotHost.allowAnyconn != null) {
            gotUrl.port = lower.srvPort;
            if (lower.secProto != 0) {
                gotUrl.proto = "https";
            } else {
                gotUrl.proto = "http";
            }
            headers.add("Set-Cookie: webvpncontext=00@defctx; path=/; Secure");
            if (gotUrl.toPathName().equals("")) {
                headers.add("Location: " + gotUrl.toURL(false, false) + "webvpn.html");
                sendRespHeader("303 see other", -1, "text/html");
                return;
            }
            if (gotUrl.toPathName().equals("1/index.html")) {
                sendHtmlHeader("200 ok", "<html></html>");
                return;
            }
            if (gotUrl.toPathName().equals("1/Linux")) {
                sendHtmlHeader("200 ok", "<html></html>");
                return;
            }
            if (gotUrl.toPathName().equals("1/Linux_64")) {
                sendHtmlHeader("200 ok", "<html></html>");
                return;
            }
            if (gotUrl.toPathName().equals("1/Windows")) {
                sendHtmlHeader("200 ok", "<html></html>");
                return;
            }
            if (gotUrl.toPathName().equals("1/Darwin_i386")) {
                sendHtmlHeader("200 ok", "<html></html>");
                return;
            }
            if (gotUrl.toPathName().equals("1/VPNManifest.xml")) {
                sendHtmlHeader("200 ok", "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<vpn rev=\"1.0\">\n</vpn>\n");
                return;
            }
            if (gotUrl.toPathName().equals("1/binaries/update.txt")) {
                sendHtmlHeader("200 ok", "0,0,0000\\n");
                return;
            }
            if (gotUrl.toPathName().equals("logout")) {
                sendHtmlHeader("200 ok", "<html></html>");
                return;
            }
            if (gotUrl.toPathName().startsWith(" CSCOT /")) {
                sendHtmlHeader("200 ok", "<html></html>");
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
                sendHtmlHeader("200 ok", "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<auth id=\"main\"><title>login</title><message>enter username and password</message><form method=\"post\" action=\"webvpn.html\"><input type=\"text\" label=\"username:\" name=\"username\" value=\"\" /><input type=\"password\" label=\"password:\" name=\"password\" value=\"\" /><input type=\"submit\" name=\"login\" value=\"login\" /></form></auth>");
                return;
            }
            headers.add("Set-Cookie: webvpn=00@0168430307@00071@3702439125@3326207229@defctx; path=/; Secure");
            headers.add("Set-Cookie: webvpnc=bu:0/&p:t&iu:1/&sh:%s; path=/; Secure");
            sendHtmlHeader("200 ok", "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<auth id=\"success\"><title>vpn</title><message>success</message><success/></auth>");
            return;
        }
        if (gotHost.authenticList != null) {
            if (checkUserAuth(gotAuth)) {
                headers.add("WWW-Authenticate: Basic realm=login");
                sendRespError(401, "unauthorized");
                return;
            }
            gotAuth = decodeAuth(gotAuth, true);
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
                urls.add(uniResLoc.parseOne(a));
            }
            addrIP adrs[] = new addrIP[urls.size()];
            for (int i = 0; i < adrs.length; i++) {
                adrs[i] = userTerminal.justResolv(urls.get(i).server, 0);
            }
            pipeSide cons[] = new pipeSide[adrs.length];
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
                fin.linePut(gotCmd.toUpperCase() + " " + gotUrl.toURL(false, true) + " HTTP/1.1");
                fin.linePut("User-Agent: " + gotAgent + " [" + version.usrAgnt + " by " + conn.peerAddr + "]");
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
            addrIP adr = userTerminal.justResolv(srvUrl.server, 0);
            if (adr == null) {
                sendRespError(502, "bad gateway");
                return;
            }
            pipeSide cnn = gotHost.reconnP.doConnect(servGeneric.protoTcp, adr, srvUrl.getPort(lower.srvPort()), "http");
            if (cnn == null) {
                sendRespError(504, "gateway timeout");
                return;
            }
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
                srvUrl.server = gotUrl.server;
            }
            if ((gotHost.subconn & 0x20) != 0) {
                srvUrl.filPath = (srvUrl.filPath + "/" + gotUrl.filPath).replaceAll("//", "/");
            }
            if (debugger.servHttpTraf) {
                logger.debug("reconnect " + srvUrl.toURL(false, true));
            }
            pipeSide.modTyp old = cnn.lineTx;
            cnn.lineTx = pipeSide.modTyp.modeCRLF;
            cnn.linePut(gotCmd.toUpperCase() + " " + srvUrl.toURL(false, true) + " HTTP/1.1");
            cnn.linePut("User-Agent: " + gotAgent + " [" + version.usrAgnt + " by " + conn.peerAddr + "]");
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
            sendFoundAt(gotHost.redir);
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
            pipeLine pip = new pipeLine(32768, false);
            pipeDiscard.discard(pip.getSide());
            userFlash fls = new userFlash(pip.getSide());
            if (fls.copy(gotHost.path + pn, gotHost.path + url.toPathName())) {
                sendRespHeader("409 conflict", 0, null);
            } else {
                sendRespHeader("201 created", 0, null);
            }
            pip.setClose();
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
                    logger.info("got bad protocol from " + lower.conn.peerAddr);
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
