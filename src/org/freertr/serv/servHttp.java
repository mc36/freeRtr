package org.freertr.serv;

import java.util.ArrayList;
import java.util.List;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgProxy;
import org.freertr.clnt.clntProxy;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServS;
import org.freertr.enc.encUrl;
import org.freertr.sec.secInfoUtl;
import org.freertr.tab.tabGen;
import org.freertr.user.userFilter;
import org.freertr.user.userFormat;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;

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
    public final static int clearPort = 80;

    /**
     * secure port
     */
    public final static int securePort = 443;

    /**
     * html 401 transitive
     */
    public final static String htmlHead = "<!DOCTYPE html><html lang=\"en\"><head><meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" /><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" />";

    /**
     * list of hosts
     */
    protected tabGen<servHttpHost> hosts = new tabGen<servHttpHost>();

    /**
     * list of bots
     */
    protected List<String> bots = new ArrayList<String>();

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
    protected String defPath = defHostPat;

    /**
     * default subconnect
     */
    protected int defSubcon;

    /**
     * buffer size
     */
    protected int bufSiz = 65536;

    /**
     * default host path
     */
    public final static String defHostPat = "/data/notfound/";

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("server http .*", cmds.tabulator + "port " + clearPort, null),
        new userFilter("server http .*", cmds.tabulator + "protocol " + proto2string(protoAllStrm), null),
        new userFilter("server http .*", cmds.tabulator + cmds.negated + cmds.tabulator + "proxy", null),
        new userFilter("server http .*", cmds.tabulator + cmds.negated + cmds.tabulator + "error", null),
        new userFilter("server http .*", cmds.tabulator + cmds.negated + cmds.tabulator + "single-request", null),
        new userFilter("server http .*", cmds.tabulator + "def-path " + defHostPat, null),
        new userFilter("server http .*", cmds.tabulator + "def-subconn", null),
        new userFilter("server http .*", cmds.tabulator + "buffer 65536", null),
        new userFilter("server http .*", cmds.tabulator + cmds.negated + cmds.tabulator + "second-port", null)
    };

    /**
     * get defaults filter
     *
     * @return filter
     */
    public userFilter[] srvDefFlt() {
        return defaultF;
    }

    /**
     * find one host
     *
     * @param s agent name
     * @return true if found
     */
    protected boolean findBot(String s) {
        for (int i = 0; i < bots.size(); i++) {
            String a = bots.get(i);
            if (s.matches(a)) {
                return true;
            }
        }
        return false;
    }

    /**
     * find one host
     *
     * @param s host name
     * @return host descriptor, null if not found
     */
    protected servHttpHost findHost(String s) {
        servHttpHost ntry = new servHttpHost(this, s);
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
     * @param filter default filter
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
        l.add(beg + "def-path " + defPath);
        l.add(beg + "def-subconn" + servHttpUtil.subconn2string(defSubcon));
        cmds.cfgLine(l, !singleRequest, beg, "single-request", "");
        for (int i = 0; i < bots.size(); i++) {
            l.add(beg + "bad-agent " + bots.get(i));
        }
        for (int hn = 0; hn < hosts.size(); hn++) {
            servHttpHost ntry = hosts.get(hn);
            if (ntry == null) {
                continue;
            }
            ntry.getConfig(beg, l, filter);
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
        if (a.equals(cmds.negated)) {
            negated = true;
            a = cmd.word();
        }
        if (a.equals("bad-agent")) {
            a = cmd.getRemaining();
            if (negated) {
                bots.remove(a);
                return false;
            }
            if (bots.indexOf(a) >= 0) {
                return false;
            }
            bots.add(a);
            return false;
        }
        if (a.equals("second-port")) {
            srvDeinit();
            if (negated) {
                secondPort = -1;
                srvInit();
                return false;
            }
            secondPort = bits.str2num(cmd.word());
            srvInit();
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
                defPath = defHostPat;
            } else {
                defPath = cmd.word();
            }
            return false;
        }
        if (a.equals("def-subconn")) {
            defSubcon = servHttpUtil.string2subconn(negated, cmd);
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
        servHttpHost ntry = new servHttpHost(this, cmd.word());
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
            ntry.path = defPath;
            ntry.subconn = defSubcon;
        }
        return ntry.doConfig(negated, a, cmd);
    }

    private final static void getSubconnHelp(int b, userHelp l) {
        l.add(null, false, b, new int[]{b, -1}, "strip-path", "strip path");
        l.add(null, false, b, new int[]{b, -1}, "strip-name", "strip filename");
        l.add(null, false, b, new int[]{b, -1}, "strip-ext", "strip extension");
        l.add(null, false, b, new int[]{b, -1}, "strip-param", "strip parameters");
        l.add(null, false, b, new int[]{b, -1}, "keep-cred", "keep credentinals");
        l.add(null, false, b, new int[]{b, -1}, "keep-host", "keep hostname");
        l.add(null, false, b, new int[]{b, -1}, "keep-path", "append path");
    }

    /**
     * get help
     *
     * @param l list
     */
    public void srvHelp(userHelp l) {
        l.add(null, false, 1, new int[]{-1}, "single-request", "one request per connection");
        l.add(null, false, 1, new int[]{2}, "buffer", "set buffer size on connection");
        l.add(null, false, 2, new int[]{-1}, "<num>", "buffer in bytes");
        l.add(null, false, 1, new int[]{2}, "proxy", "enable proxy support");
        l.add(null, false, 2, new int[]{-1}, "<name:prx>", "proxy profile");
        l.add(null, false, 1, new int[]{2}, "second-port", "enable dual binding");
        l.add(null, false, 2, new int[]{-1}, "<num>", "secure port");
        l.add(null, false, 1, new int[]{2}, "bad-agent", "set host default path");
        l.add(null, false, 2, new int[]{2, -1}, "<str>", "user agent to deny");
        l.add(null, false, 1, new int[]{2}, "def-path", "set host default path");
        l.add(null, false, 2, new int[]{-1}, "<str>", "path on the disk");
        l.add(null, false, 1, new int[]{2}, "def-subconn", "set host default subconnect");
        getSubconnHelp(2, l);
        l.add(null, false, 1, new int[]{2}, "error", "set error message");
        l.add(null, false, 2, new int[]{2, -1}, "<str>", "error message");
        l.add(null, false, 1, new int[]{2}, "host", "define one virtual server");
        List<String> lst = new ArrayList<String>();
        for (int i = 0; i < hosts.size(); i++) {
            lst.add(hosts.get(i).host);
        }
        l.add(lst, false, 2, new int[]{3}, "<name:loc>", "name of server, * for any");
        l.add(null, false, 3, new int[]{4}, "path", "set server root");
        l.add(null, false, 4, new int[]{-1}, "<str>", "root directory of server");
        l.add(null, false, 3, new int[]{4}, "redir", "set redirect path");
        l.add(null, false, 4, new int[]{-1}, "<url>", "url to redirect to");
        l.add(null, false, 3, new int[]{4}, "webring", "set redirect file");
        l.add(null, false, 4, new int[]{-1}, "<file>", "path to redirect list");
        l.add(null, false, 3, new int[]{-1}, "logging", "log to syslog");
        l.add(null, false, 3, new int[]{4}, "reconn", "reconnect to server");
        l.add(null, false, 4, new int[]{5}, "<name:prx>", "proxy profile");
        l.add(null, false, 5, new int[]{-1}, "<str>", "server to redirect to");
        l.add(null, false, 3, new int[]{4}, "cache", "cache a server");
        l.add(null, false, 4, new int[]{5}, "<name:prx>", "proxy profile");
        l.add(null, false, 5, new int[]{-1}, "<str>", "server to cache from");
        l.add(null, false, 3, new int[]{4}, "translate", "translate the url");
        l.add(null, false, 4, new int[]{4, -1}, "<num:trn>", "translation rule to use");
        l.add(null, false, 3, new int[]{4}, "subconn", "reconnect only to the url");
        getSubconnHelp(4, l);
        l.add(null, false, 3, new int[]{4}, "stream", "stream from server");
        l.add(null, false, 4, new int[]{5}, "<str>", "content type");
        l.add(null, false, 5, new int[]{6}, "<name:prx>", "proxy profile");
        l.add(null, false, 6, new int[]{-1}, "<str>", "server to stream from");
        l.add(null, false, 3, new int[]{4}, "multiacc", "access multiple servers");
        l.add(null, false, 4, new int[]{5}, "<name:prx>", "proxy profile");
        l.add(null, false, 5, new int[]{5, -1}, "<str>", "server to access");
        l.add(null, false, 3, new int[]{-1}, "markdown", "allow markdown conversion");
        l.add(null, false, 3, new int[]{-1}, "noindex", "disallow index for directory");
        l.add(null, false, 3, new int[]{4}, "speed-limit", "limit download speeds");
        l.add(null, false, 4, new int[]{-1}, "<num>", "bytes per second");
        l.add(null, false, 3, new int[]{4, -1}, "dirlist", "allow directory listing");
        l.add(null, false, 4, new int[]{4, -1}, "readme", "put readme in front of listing");
        l.add(null, false, 4, new int[]{4, -1}, "stats", "put statistics after listing");
        l.add(null, false, 3, new int[]{4, -1}, "script", "allow script running");
        l.add(null, false, 4, new int[]{4, -1}, "exec", "allow exec commands");
        l.add(null, false, 4, new int[]{4, -1}, "config", "allow config commands");
        l.add(null, false, 3, new int[]{4, -1}, "api", "allow api calls");
        l.add(null, false, 4, new int[]{4, -1}, "exec", "allow exec commands");
        l.add(null, false, 4, new int[]{4, -1}, "show", "allow show commands");
        l.add(null, false, 4, new int[]{4, -1}, "script", "allow script commands");
        l.add(null, false, 4, new int[]{4, -1}, "config", "allow config commands");
        l.add(null, false, 4, new int[]{4, -1}, "ipinfo", "allow ip info commands");
        secInfoUtl.getHelp(l, 3, "ipinfo", "allow ipinfo api");
        l.add(null, false, 3, new int[]{4}, "search-script", "allow scripts defined in configuration");
        l.add(null, false, 4, new int[]{-1}, "<str>", "prefix");
        l.add(null, false, 3, new int[]{-1}, "imagemap", "allow image map processing");
        l.add(null, false, 3, new int[]{-1}, "websock", "allow websocket processing");
        l.add(null, false, 3, new int[]{-1}, "webdav", "allow webdav processing");
        l.add(null, false, 3, new int[]{-1}, "mediastream", "allow media streaming");
        l.add(null, false, 3, new int[]{-1}, "class", "allow class running");
        l.add(null, false, 3, new int[]{-1}, "upload", "allow upload files");
        l.add(null, false, 3, new int[]{4}, "backup", "backup uploaded files");
        l.add(null, false, 4, new int[]{5}, "<num>", "number of backups to keep");
        l.add(null, false, 5, new int[]{-1}, "<str>", "root directory of backup");
        l.add(null, false, 3, new int[]{4}, "sstp", "allow sstp clients");
        l.add(null, false, 4, new int[]{-1}, "<name:ifc>", "name of interface");
        l.add(null, false, 3, new int[]{4}, "anyconn", "allow anyconnect clients");
        l.add(null, false, 4, new int[]{-1}, "<name:ifc>", "name of interface");
        l.add(null, false, 3, new int[]{4}, "forti", "allow fortinet clients");
        l.add(null, false, 4, new int[]{-1}, "<name:ifc>", "name of interface");
        l.add(null, false, 3, new int[]{4}, "authentication", "require authentication to access");
        l.add(null, false, 4, new int[]{-1}, "<name:aaa>", "authentication list");
        l.add(null, false, 3, new int[]{4}, "style", "set page style tags");
        l.add(null, false, 4, new int[]{4, -1}, "<text>", "text to send");
        secInfoUtl.getHelp(l, 2, "access-", null);
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
    public userFormat getShStat() {
        userFormat res = new userFormat("|", "host|hit|last");
        for (int i = 0; i < hosts.size(); i++) {
            servHttpHost ntry = hosts.get(i);
            res.add(ntry.host + "|" + ntry.askNum + "|" + bits.timePast(ntry.askTim));
        }
        return res;
    }

    /**
     * get show
     *
     * @param s host to use
     * @return result
     */
    public List<String> getShZone(String s) {
        List<String> res = new ArrayList<String>();
        for (int i = 0; i < hosts.size(); i++) {
            servHttpHost ntry = hosts.get(i);
            String a = ntry.host;
            String b = "rr " + a + " cname " + s;
            int o = a.indexOf(".");
            if (o > 0) {
                b = "zone " + a.substring(o + 1, a.length()) + " " + b;
            }
            res.add(b);
        }
        return res;
    }

}
