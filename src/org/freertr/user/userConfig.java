package org.freertr.user;

import org.freertr.pipe.pipeScreen;
import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrIPv6;
import org.freertr.addr.addrIpx;
import org.freertr.auth.authGeneric;
import org.freertr.auth.authLocal;
import org.freertr.auth.authResult;
import org.freertr.cfg.cfgAceslst;
import org.freertr.cfg.cfgAlias;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgAuther;
import org.freertr.cfg.cfgBndl;
import org.freertr.cfg.cfgBrdg;
import org.freertr.cfg.cfgCert;
import org.freertr.cfg.cfgChat;
import org.freertr.cfg.cfgCheck;
import org.freertr.cfg.cfgDial;
import org.freertr.cfg.cfgEvntmgr;
import org.freertr.cfg.cfgGeneric;
import org.freertr.cfg.cfgHrpn;
import org.freertr.cfg.cfgInit;
import org.freertr.cfg.cfgKey;
import org.freertr.cfg.cfgMenuK;
import org.freertr.cfg.cfgMenuT;
import org.freertr.cfg.cfgMtrack;
import org.freertr.cfg.cfgObjnet;
import org.freertr.cfg.cfgObjprt;
import org.freertr.cfg.cfgPlymp;
import org.freertr.cfg.cfgPool;
import org.freertr.cfg.cfgPrcss;
import org.freertr.cfg.cfgPrfxlst;
import org.freertr.cfg.cfgProxy;
import org.freertr.cfg.cfgRoump;
import org.freertr.cfg.cfgRouplc;
import org.freertr.cfg.cfgRtr;
import org.freertr.cfg.cfgSched;
import org.freertr.cfg.cfgScrpt;
import org.freertr.cfg.cfgSensor;
import org.freertr.cfg.cfgSessn;
import org.freertr.cfg.cfgTime;
import org.freertr.cfg.cfgTlmtry;
import org.freertr.cfg.cfgTrack;
import org.freertr.cfg.cfgTrnsltn;
import org.freertr.cfg.cfgVdc;
import org.freertr.cfg.cfgVnet;
import org.freertr.cfg.cfgVpdn;
import org.freertr.cfg.cfgVrf;
import org.freertr.cfg.cfgXconn;
import org.freertr.clnt.clntIrc;
import org.freertr.clnt.clntNetflow;
import org.freertr.clnt.clntNtp;
import org.freertr.clnt.clntSyslog;
import org.freertr.clnt.clntWhois;
import org.freertr.enc.encBase64;
import org.freertr.cry.cryCertificate;
import org.freertr.cry.cryKeyDSA;
import org.freertr.cry.cryKeyECDSA;
import org.freertr.cry.cryKeyGeneric;
import org.freertr.cry.cryKeyMLDSA;
import org.freertr.cry.cryKeyRSA;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdRoute;
import org.freertr.ipx.ipxFwd;
import org.freertr.pipe.pipeSetting;
import org.freertr.pipe.pipeSide;
import org.freertr.serv.servGenList;
import org.freertr.serv.servGeneric;
import org.freertr.serv.servQuote;
import org.freertr.serv.servSyslog;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabNatCfgN;
import org.freertr.tab.tabNshEntry;
import org.freertr.tab.tabPbrN;
import org.freertr.tab.tabRouteAttr;
import org.freertr.tab.tabRouteEntry;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.version;

/**
 * process config commands
 *
 * @author matecsaba
 */
public class userConfig {

    /**
     * authenticated username
     */
    public String username = "<nobody>";

    /**
     * authorization list
     */
    public authGeneric authorization;

    /**
     * expand variables
     */
    public boolean needExpand;

    /**
     * commit buffer
     */
    public List<String> commits;

    private pipeSide pipe; // pipe to use

    private userRead reader; // reader of the user

    private cmds cmd; // currently processed string

    private cfgGeneric submode;

    /**
     * constructs new reader for a pipeline
     *
     * @param pip pipeline to use as input
     * @param rdr reader to use as input
     */
    public userConfig(pipeSide pip, userRead rdr) {
        pipe = pip;
        reader = rdr;
        resetMode();
    }

    /**
     * reset current mode to global config
     */
    public void resetMode() {
        submode = null;
    }

    /**
     * get help text for exec commands
     *
     * @param needEdit need editor
     * @param needShow need show
     * @param needGen need generic
     * @return helping instance
     */
    public userHelp getHelping(boolean needEdit, boolean needShow, boolean needGen) {
        userHelp l = new userHelp();
        l.expand = needExpand;
        if (needEdit) {
            l.add(null, false, 1, new int[]{-1}, "editor", "edit the current section");
        }
        if (needShow) {
            l.add(null, false, 1, new int[]{2}, "show", "running system information");
            userExec.getHelpShow(l, true);
            userExec.getHelpPipes(l, 110, true);
        }
        if (needGen) {
            userHelp.getCfgGen(l);
        }
        if (submode == null) {
            getHelpGlobal(l);
        } else {
            submode.getHelp(l);
        }
        return l;
    }

    /**
     * get current prompt
     *
     * @return prompt value
     */
    public String getPrompt() {
        if (submode == null) {
            return "(cfg)";
        } else {
            return "(cfg-" + submode.getPrompt() + ")";
        }
    }

    /**
     * execute one command
     *
     * @param a the command to execute
     * @return status of operation, false to continue processing
     */
    public boolean executeCommand(String a) {
        if (a == null) {
            return false;
        }
        cmd = new cmds("config", a);
        cmd.pipe = pipe;
        if (authorization != null) {
            authResult ntry = authorization.authUserCommand(username, a);
            if (ntry.result != authResult.authSuccessful) {
                cmd.error("not authorized to configure that");
                return false;
            }
        }
        if (debugger.userConfigEvnt) {
            logger.debug(cmd.getOriginal());
        }
        a = cmd.word();
        if (a.length() < 1) {
            return false;
        }
        if (a.equals(cmds.finish)) {
            if (submode == null) {
                return true;
            }
            if (commits != null) {
                commits.add(cmds.tabulator + cmds.finish);
            }
            resetMode();
            return false;
        }
        if (a.equals("end")) {
            resetMode();
            return true;
        }
        if (a.equals("editor")) {
            if (commits != null) {
                return false;
            }
            if (submode == null) {
                cmd.error("not allowed here");
                return false;
            }
            List<String> c1 = submode.getShRun(1);
            List<String> c2 = new ArrayList<String>();
            c2.addAll(c1);
            userEditor edt = new userEditor(new pipeScreen(cmd.pipe), c2, "current", false);
            if (edt.doEdit()) {
                return false;
            }
            List<String> c3 = userFilter.getDiffs(c1, c2);
            reader.putStrArr(bits.lst2lin(c3, false));
            int res = cfgInit.executeSWcommands(c3, false);
            reader.putStrArr(bits.str2lst("errors=" + res));
            c3 = userFilter.getDiffs(c2, c1);
            reader.putStrArr(c3);
            return false;
        }
        if (a.equals("show")) {
            if (pipe.settingsGet(pipeSetting.times, false)) {
                pipe.linePut(logger.getTimestamp());
            }
            userShow shw = new userShow();
            cmd = reader.setFilter(cmd);
            shw.cmd = cmd;
            shw.rdr = reader;
            shw.hlp = getHelping(false, false, false);
            shw.cfg = submode;
            shw.cmt = commits;
            cfgAlias alias = shw.doer();
            if (alias == null) {
                return false;
            }
            userExec e = new userExec(pipe, reader);
            alias.doCommands(e, cmd);
            return false;
        }
        if (a.equals("do")) {
            if (pipe.settingsGet(pipeSetting.times, false)) {
                pipe.linePut(logger.getTimestamp());
            }
            userExec exe = new userExec(pipe, reader);
            exe.privileged = true;
            exe.username = username;
            exe.authorization = authorization;
            a = exe.repairCommand(cmd.getRemaining());
            exe.executeCommand(a);
            return false;
        }
        cmd = cmd.copyBytes(true);
        if (submode != null) {
            if (commits != null) {
                commits.add(cmds.tabulator + cmd.getOriginal());
                return false;
            }
            submode.doCfgStr(cmd);
            return false;
        }
        if (a.equals("commit")) {
            if (commits == null) {
                return false;
            }
            reader.putStrArr(commits);
            int res = cfgInit.executeSWcommands(commits, false);
            commits.clear();
            reader.putStrArr(bits.str2lst("errors=" + res));
            return false;
        }
        doGlobal();
        if (cmd.barked > 0) {
            return false;
        }
        if (commits == null) {
            return false;
        }
        commits.add(cmd.getOriginal());
        return false;
    }

    private void getHelpGlobal(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "hostname", "set name of system");
        l.add(null, false, 2, new int[]{-1}, "<str>", "name of system");
        l.add(null, false, 1, new int[]{-1}, "buggy", "enable dangerous things");
        l.add(null, false, 1, new int[]{-1}, "commit", "commit config changes");
        l.add(null, false, 1, new int[]{2}, "locale", "set locale of system");
        l.add(null, false, 2, new int[]{-1}, "<str>", "enable password");
        l.add(null, false, 1, new int[]{2}, "enable", "set enable password");
        l.add(null, false, 2, new int[]{-1}, "<str>", "enable password");
        l.add(null, false, 1, new int[]{2}, "password-encrypt", "set password encryption key");
        l.add(null, false, 2, new int[]{-1}, "<str>", "encryption key");
        l.add(null, false, 1, new int[]{2}, "banner", "banner of system");
        l.add(null, false, 2, new int[]{3}, "set", "set banner");
        l.add(null, false, 3, new int[]{3, -1}, "<cmd>", "first line of banner");
        l.add(null, false, 2, new int[]{3}, "add", "add banner");
        l.add(null, false, 3, new int[]{3, -1}, "<cmd>", "add line to banner");
        l.add(null, false, 2, new int[]{3}, "encoded", "set banner");
        l.add(null, false, 3, new int[]{3, -1}, "<cmd>", "encoded banner");
        l.add(null, false, 2, new int[]{-1}, "editor", "edit the banner");
        l.add(null, false, 2, new int[]{3}, "text", "add text file");
        l.add(null, false, 3, new int[]{3, -1}, "<str>", "file name");
        l.add(null, false, 2, new int[]{3}, "image", "add image file");
        l.add(null, false, 3, new int[]{3, -1}, "<str>", "file name");
        l.add(null, false, 2, new int[]{3}, "movie", "add movie file");
        l.add(null, false, 3, new int[]{3, -1}, "<str>", "file name");
        l.add(null, false, 2, new int[]{3}, "pixel", "add movie file");
        l.add(null, false, 3, new int[]{3, -1}, "<str>", "file name");
        l.add(null, false, 1, new int[]{2}, "logging", "set logging parameters");
        l.add(null, false, 2, new int[]{3}, "debug", "always on debugging");
        debugger.getHelping(l, 3);
        l.add(null, false, 2, new int[]{-1}, "tracestop", "stop on traceback");
        l.add(null, false, 2, new int[]{-1}, "milliseconds", "millisecond logging");
        l.add(null, false, 2, new int[]{3}, "proxy", "set proxy to use");
        l.add(null, false, 3, new int[]{-1}, "<name:prx>", "proxy profile");
        l.add(null, false, 2, new int[]{3}, "buffered", "buffered logging");
        l.add(null, false, 3, new int[]{4}, "debug", "debugging messages");
        l.add(null, false, 3, new int[]{4}, "informational", "informational messages");
        l.add(null, false, 3, new int[]{4}, "warning", "warning messages");
        l.add(null, false, 3, new int[]{4}, "error", "error messages");
        l.add(null, false, 3, new int[]{4}, "exception", "exception messages");
        l.add(null, false, 4, new int[]{-1}, "<num>", "number of lines");
        l.add(null, false, 2, new int[]{3}, "file", "file logging");
        l.add(null, false, 3, new int[]{4, -1}, "debug", "debugging messages");
        l.add(null, false, 3, new int[]{4, -1}, "informational", "informational messages");
        l.add(null, false, 3, new int[]{4, -1}, "warning", "warning messages");
        l.add(null, false, 3, new int[]{4, -1}, "error", "error messages");
        l.add(null, false, 3, new int[]{4, -1}, "exception", "exception messages");
        l.add(null, false, 4, new int[]{-1}, "<str>", "name of file");
        l.add(null, false, 2, new int[]{3}, "rotate", "log file rotation");
        l.add(null, false, 3, new int[]{4}, "<num>", "maximum file size");
        l.add(null, false, 4, new int[]{5, -1}, "<str>", "name of second file");
        l.add(null, false, 5, new int[]{-1}, "<num>", "ms between backup");
        l.add(null, false, 2, new int[]{3}, "syslog", "syslog logging");
        l.add(null, false, 3, new int[]{4}, "debug", "debugging messages");
        l.add(null, false, 3, new int[]{4}, "informational", "informational messages");
        l.add(null, false, 3, new int[]{4}, "warning", "warning messages");
        l.add(null, false, 3, new int[]{4}, "error", "error messages");
        l.add(null, false, 3, new int[]{4}, "exception", "exception messages");
        l.add(null, false, 4, new int[]{5}, "kernel", "facility");
        l.add(null, false, 4, new int[]{5}, "user", "facility");
        l.add(null, false, 4, new int[]{5}, "mail", "facility");
        l.add(null, false, 4, new int[]{5}, "system", "facility");
        l.add(null, false, 4, new int[]{5}, "security1", "facility");
        l.add(null, false, 4, new int[]{5}, "syslogd", "facility");
        l.add(null, false, 4, new int[]{5}, "lpd", "facility");
        l.add(null, false, 4, new int[]{5}, "news", "facility");
        l.add(null, false, 4, new int[]{5}, "uucp", "facility");
        l.add(null, false, 4, new int[]{5}, "clock1", "facility");
        l.add(null, false, 4, new int[]{5}, "security2", "facility");
        l.add(null, false, 4, new int[]{5}, "ftp", "facility");
        l.add(null, false, 4, new int[]{5}, "ntp", "facility");
        l.add(null, false, 4, new int[]{5}, "logaudit", "facility");
        l.add(null, false, 4, new int[]{5}, "logalert", "facility");
        l.add(null, false, 4, new int[]{5}, "clock2", "facility");
        l.add(null, false, 4, new int[]{5}, "local0", "facility");
        l.add(null, false, 4, new int[]{5}, "local1", "facility");
        l.add(null, false, 4, new int[]{5}, "local2", "facility");
        l.add(null, false, 4, new int[]{5}, "local3", "facility");
        l.add(null, false, 4, new int[]{5}, "local4", "facility");
        l.add(null, false, 4, new int[]{5}, "local5", "facility");
        l.add(null, false, 4, new int[]{5}, "local6", "facility");
        l.add(null, false, 4, new int[]{5}, "local7", "facility");
        l.add(null, false, 5, new int[]{5, -1}, "<addr>", "address of host");
        l.add(null, false, 2, new int[]{3}, "irc", "irc logging");
        l.add(null, false, 3, new int[]{4, -1}, "debug", "debugging messages");
        l.add(null, false, 3, new int[]{4, -1}, "informational", "informational messages");
        l.add(null, false, 3, new int[]{4, -1}, "warning", "warning messages");
        l.add(null, false, 3, new int[]{4, -1}, "error", "error messages");
        l.add(null, false, 3, new int[]{4, -1}, "exception", "exception messages");
        l.add(null, false, 4, new int[]{5}, "<str>", "name of host");
        l.add(null, false, 5, new int[]{-1}, "<str>", "name of channel");
        l.add(null, false, 2, new int[]{3}, "monitor", "terminal logging");
        l.add(null, false, 3, new int[]{-1}, "debug", "debugging messages");
        l.add(null, false, 3, new int[]{-1}, "informational", "informational messages");
        l.add(null, false, 3, new int[]{-1}, "warning", "warning messages");
        l.add(null, false, 3, new int[]{-1}, "error", "error messages");
        l.add(null, false, 3, new int[]{-1}, "exception", "exception messages");
        l.add(null, false, 2, new int[]{3}, "format", "logging format");
        l.add(null, false, 3, new int[]{-1}, "none", "not log source at all");
        l.add(null, false, 3, new int[]{-1}, "brief", "log only class name");
        l.add(null, false, 3, new int[]{-1}, "normal", "log class, file, line number");
        l.add(null, false, 3, new int[]{-1}, "full", "log full stack trace");
        l.add(null, false, 1, new int[]{2}, "vrf", "configure a virtual routing forwarding");
        l.add(null, false, 2, new int[]{3}, "definition", "create new or update existing vrf");
        l.add(null, false, 3, new int[]{-1}, "<name:vrf>", "name of vrf");
        l.add(null, false, 1, new int[]{2}, "vdc", "configure a virtual device context");
        l.add(null, false, 2, new int[]{3}, "definition", "create new or update existing vdc");
        l.add(null, false, 3, new int[]{-1}, "<name:vdc>", "name of vdc");
        l.add(null, false, 1, new int[]{2}, "process", "configure a external process");
        l.add(null, false, 2, new int[]{3}, "definition", "create new or update existing process");
        l.add(null, false, 3, new int[]{-1}, "<name:prc>", "name of process");
        l.add(null, false, 1, new int[]{2}, "interface", "select an interface to configure");
        l.add(null, false, 2, new int[]{-1}, "<name:ifc>", "name of interface");
        l.add(null, false, 1, new int[]{-1}, "console0", "select a line to configure");
        l.add(null, false, 1, new int[]{2}, "line", "select a line to configure");
        l.add(null, false, 2, new int[]{-1}, "<name:lin>", "name of line");
        l.add(null, false, 1, new int[]{2}, "bridge", "transparent bridging parameters");
        l.add(null, false, 2, new int[]{-1}, "<num>", "number of bridge group");
        l.add(null, false, 1, new int[]{2}, "bundle", "interface bundle parameters");
        l.add(null, false, 2, new int[]{-1}, "<num>", "number of bundle group");
        l.add(null, false, 1, new int[]{2}, "hairpin", "interface hairpin parameters");
        l.add(null, false, 2, new int[]{-1}, "<num>", "number of hairpin group");
        l.add(null, false, 1, new int[]{2}, "vnet", "external hairpin parameters");
        l.add(null, false, 2, new int[]{-1}, "<num>", "number of hairpin group");
        l.add(null, false, 1, new int[]{2}, "session", "stateful session parameters");
        l.add(null, false, 2, new int[]{-1}, "<name:ses>", "name of session");
        l.add(null, false, 1, new int[]{2}, "check", "check parameters");
        l.add(null, false, 2, new int[]{-1}, "<name:chk>", "name of check");
        l.add(null, false, 1, new int[]{2}, "sensor", "sensor parameters");
        l.add(null, false, 2, new int[]{-1}, "<name:sns>", "name of sensor");
        l.add(null, false, 1, new int[]{2}, "dial-peer", "dial peer parameters");
        l.add(null, false, 2, new int[]{-1}, "<num>", "number of peer");
        l.add(null, false, 1, new int[]{2}, "translation-rule", "translation rule parameters");
        l.add(null, false, 2, new int[]{-1}, "<num>", "number of peer");
        l.add(null, false, 1, new int[]{2}, "nsh", "specify service chaining");
        l.add(null, false, 2, new int[]{3}, "<num>", "service path");
        l.add(null, false, 3, new int[]{4}, "<num>", "service index");
        l.add(null, false, 4, new int[]{4, -1}, "drop", "drop packets");
        l.add(null, false, 4, new int[]{4, -1}, "rawpack", "output as raw packet, witout nsh header");
        l.add(null, false, 4, new int[]{4, -1}, "keephdr", "keep original layer2 addresses");
        l.add(null, false, 4, new int[]{5}, "interface", "forward as nsh");
        l.add(null, false, 5, new int[]{6}, "<name:ifc>", "target interface");
        l.add(null, false, 6, new int[]{4, -1}, "<addr>", "target mac address");
        l.add(null, false, 4, new int[]{5}, "route", "route normally");
        l.add(null, false, 5, new int[]{4, -1}, "<name:vrf>", "target vrf");
        l.add(null, false, 4, new int[]{5}, "tunnel", "tunnel in ip");
        l.add(null, false, 5, new int[]{6}, "<name:vrf>", "target vrf");
        l.add(null, false, 6, new int[]{7}, "<name:ifc>", "source interface");
        l.add(null, false, 7, new int[]{4, -1}, "<addr>", "target ip address");
        l.add(null, false, 4, new int[]{5}, "switch", "switch service");
        l.add(null, false, 5, new int[]{6}, "<num>", "new service path");
        l.add(null, false, 6, new int[]{4, -1}, "<num>", "new service index");
        l.add(null, false, 1, new int[]{2}, "client", "specify address of name server");
        l.add(null, false, 2, new int[]{3}, "cpuhog", "specify cpuhog parameters");
        l.add(null, false, 3, new int[]{-1}, "<num>", "percentage");
        l.add(null, false, 2, new int[]{3}, "label-range", "specify label range parameters");
        l.add(null, false, 3, new int[]{4}, "<num>", "beginning");
        l.add(null, false, 4, new int[]{-1}, "<num>", "ending");
        l.add(null, false, 2, new int[]{3}, "ifacestall", "specify interface stall check");
        l.add(null, false, 3, new int[]{-1}, "<num>", "timeout in ms");
        l.add(null, false, 2, new int[]{3}, "capture-path", "specify default capture location");
        l.add(null, false, 3, new int[]{-1}, "<str>", "path on the flash");
        l.add(null, false, 2, new int[]{3}, "pastebin", "specify default pastebin service");
        l.add(null, false, 3, new int[]{-1}, "<str>", "url on the internet");
        l.add(null, false, 2, new int[]{3}, "l2f-timer", "specify l2f parameters");
        l.add(null, false, 3, new int[]{4}, "<num>", "retransmit in ms");
        l.add(null, false, 4, new int[]{-1}, "<num>", "retry times");
        l.add(null, false, 2, new int[]{3}, "l2tp2-timer", "specify l2tp2 parameters");
        l.add(null, false, 3, new int[]{4}, "<num>", "hello after ticks");
        l.add(null, false, 4, new int[]{-1}, "<num>", "retry ticks");
        l.add(null, false, 2, new int[]{3}, "l2tp3-timer", "specify l2tp3 parameters");
        l.add(null, false, 3, new int[]{4}, "<num>", "hello after ticks");
        l.add(null, false, 4, new int[]{-1}, "<num>", "retry ticks");
        l.add(null, false, 2, new int[]{3}, "redundancy-keepalive", "specify redundancy keepalive time");
        l.add(null, false, 3, new int[]{-1}, "<num>", "time in ms");
        l.add(null, false, 2, new int[]{3}, "redundancy-hold", "specify redundancy hold time");
        l.add(null, false, 3, new int[]{-1}, "<num>", "time in ms");
        l.add(null, false, 2, new int[]{3}, "redundancy-initial", "specify redundancy initial time");
        l.add(null, false, 3, new int[]{-1}, "<num>", "time in ms");
        l.add(null, false, 2, new int[]{3}, "redundancy-takeover", "specify redundancy takeover time");
        l.add(null, false, 3, new int[]{-1}, "<num>", "time in ms");
        l.add(null, false, 2, new int[]{3}, "proxy", "specify proxy profile");
        l.add(null, false, 3, new int[]{-1}, "<name:prx>", "name of profile");
        l.add(null, false, 2, new int[]{3}, "bullying", "specify shame quote source");
        l.add(cfgAll.dmnQuote.listServers(), false, 3, new int[]{-1}, "<name:loc>", "name of server");
        l.add(null, false, 2, new int[]{-1}, "domain-lookup", "allow dns queries");
        l.add(null, false, 2, new int[]{3}, "domain-name", "specify domain name");
        l.add(null, false, 3, new int[]{-1}, "<str>", "name of domain");
        l.add(null, false, 2, new int[]{3}, "name-proxy", "specify proxy profile");
        l.add(null, false, 3, new int[]{-1}, "<name:prx>", "name of profile");
        l.add(null, false, 2, new int[]{3}, "name-server", "specify address of name server");
        l.add(null, false, 3, new int[]{3, -1}, "<addr>", "address of server");
        l.add(null, false, 2, new int[]{-1}, "upgrade-config", "automatically save configuration on upgrade");
        l.add(null, false, 2, new int[]{3}, "upgrade-revert", "specify time after revert if unreachable");
        l.add(null, false, 3, new int[]{-1}, "<num>", "time in ms");
        l.add(null, false, 2, new int[]{-1}, "upgrade-backup", "automatically backup image on upgrade");
        l.add(null, false, 2, new int[]{-1}, "upgrade-ownkey", "use just the configured key");
        l.add(null, false, 2, new int[]{3}, "upgrade-server", "specify url of upgrade server");
        l.add(null, false, 3, new int[]{-1}, "<str>", "url of server");
        l.add(null, false, 2, new int[]{3}, "upgrade-script", "specify script to run on upgrade");
        l.add(null, false, 3, new int[]{-1}, "<name:scr>", "name of script");
        l.add(null, false, 2, new int[]{3}, "upgrade-pubkey", "specify key of upgrade");
        l.add(null, false, 3, new int[]{-1}, "<text>", "public key");
        l.add(null, false, 2, new int[]{3}, "config-server", "specify url of config server");
        l.add(null, false, 3, new int[]{-1}, "<str>", "url of server");
        l.add(null, false, 2, new int[]{3}, "config-username", "specify username on config server");
        l.add(null, false, 3, new int[]{-1}, "<text>", "set username");
        l.add(null, false, 2, new int[]{3}, "config-password", "specify password on config server");
        l.add(null, false, 3, new int[]{-1}, "<text>", "set password");
        l.add(null, false, 2, new int[]{3, -1}, "config-backup", "specify backup config file");
        l.add(null, false, 3, new int[]{3, -1}, "<text>", "file to use");
        l.add(null, false, 2, new int[]{-1}, "config-save", "automatically save configuration");
        l.add(null, false, 2, new int[]{-1}, "config-archive", "automatically archive configuration");
        l.add(null, false, 2, new int[]{-1}, "config-exclusive", "allow only one user in configuration mode");
        l.add(null, false, 2, new int[]{-1}, "graceful-reload", "close sessions before reload");
        l.add(null, false, 2, new int[]{3, -1}, "end-format", "specify end format");
        l.add(null, false, 3, new int[]{3, -1}, "date", "append date");
        l.add(null, false, 3, new int[]{3, -1}, "image", "append image");
        l.add(null, false, 3, new int[]{3, -1}, "chksum", "append chksum");
        l.add(null, false, 3, new int[]{3, -1}, "user", "append user");
        l.add(null, false, 3, new int[]{3, -1}, "none", "append nothing");
        l.add(null, false, 2, new int[]{3}, "whois-online", "set online information");
        l.add(null, false, 3, new int[]{3, -1}, "<str>", "server name");
        l.add(null, false, 2, new int[]{3}, "whois-server", "set whois server");
        l.add(null, false, 3, new int[]{-1}, "<str>", "server name");
        l.add(null, false, 2, new int[]{3}, "whois-option", "set whois option");
        l.add(null, false, 3, new int[]{-1}, "<str>", "server name");
        l.add(null, false, 2, new int[]{3}, "whois-file", "set whois file");
        l.add(null, false, 3, new int[]{-1}, "<str>", "server name");
        l.add(null, false, 2, new int[]{3}, "whois-proxy", "specify proxy profile");
        l.add(null, false, 3, new int[]{-1}, "<name:prx>", "name of profile");
        l.add(null, false, 2, new int[]{-1}, "password-stars", "type stars in passwords");
        l.add(null, false, 2, new int[]{-1}, "prefer-ipv6", "prefer ipv6 for domains");
        l.add(null, false, 2, new int[]{-1}, "prefer-ipv4", "prefer ipv4 for domains");
        l.add(null, false, 2, new int[]{3}, "ipv4-tos", "set ipv4 default tos");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value");
        l.add(null, false, 2, new int[]{3}, "ipv4-ttl", "set ipv4 default ttl");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value");
        l.add(null, false, 2, new int[]{3}, "ipv6-tos", "set ipv6 default tos");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value");
        l.add(null, false, 2, new int[]{3}, "ipv6-ttl", "set ipv6 default ttl");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value");
        l.add(null, false, 2, new int[]{3}, "ipv4-checksum", "set ipv4 checksum mode");
        l.add(null, false, 2, new int[]{3}, "icmp4-checksum", "set icmp4 checksum mode");
        l.add(null, false, 2, new int[]{3}, "icmp6-checksum", "set icmp6 checksum mode");
        l.add(null, false, 2, new int[]{3}, "udp-checksum", "set udp checksum mode");
        l.add(null, false, 2, new int[]{3}, "tcp-checksum", "set tcp checksum mode");
        l.add(null, false, 2, new int[]{3}, "ludp-checksum", "set ludp checksum mode");
        l.add(null, false, 2, new int[]{3}, "dccp-checksum", "set dccp checksum mode");
        l.add(null, false, 2, new int[]{3}, "sctp-checksum", "set sctp checksum mode");
        l.add(null, false, 3, new int[]{-1}, "both", "both generate and check");
        l.add(null, false, 3, new int[]{-1}, "transmit", "only generate, not check");
        l.add(null, true, 3, new int[]{-1}, "receive", "only check, not generate");
        l.add(null, true, 3, new int[]{-1}, "none", "nor generate nor check");
        l.add(null, false, 2, new int[]{-1}, "tcp-timestamp", "set tcp timestamping");
        l.add(null, false, 2, new int[]{-1}, "tcp-ecn", "set tcp ecn");
        l.add(null, false, 2, new int[]{-1}, "tcp-keepalive", "set tcp keepalive");
        l.add(null, false, 2, new int[]{3}, "tcp-timer", "set tcp timestamping");
        l.add(null, false, 3, new int[]{4}, "work", "set tcp work");
        l.add(null, false, 4, new int[]{-1}, "<num>", "ms");
        l.add(null, false, 3, new int[]{4}, "alive", "set tcp keepalive");
        l.add(null, false, 4, new int[]{-1}, "<num>", "ms");
        l.add(null, false, 3, new int[]{4}, "fin", "set tcp finish");
        l.add(null, false, 4, new int[]{-1}, "<num>", "ms");
        l.add(null, false, 3, new int[]{4}, "syn", "set tcp startup");
        l.add(null, false, 4, new int[]{-1}, "<num>", "ms");
        l.add(null, false, 3, new int[]{4}, "open", "set tcp inactivity");
        l.add(null, false, 4, new int[]{-1}, "<num>", "ms");
        l.add(null, false, 3, new int[]{4}, "close", "set tcp inactivity");
        l.add(null, false, 4, new int[]{-1}, "<num>", "ms");
        l.add(null, false, 3, new int[]{4}, "later", "set tcp retransmit");
        l.add(null, false, 4, new int[]{-1}, "<num>", "ms");
        l.add(null, false, 3, new int[]{4}, "now", "set tcp transmit");
        l.add(null, false, 4, new int[]{-1}, "<num>", "ms");
        l.add(null, false, 3, new int[]{4}, "max", "set tcp fallback");
        l.add(null, false, 4, new int[]{-1}, "<num>", "ms");
        l.add(null, false, 2, new int[]{3}, "tcp-segments", "set tcp segment size");
        l.add(null, false, 3, new int[]{4}, "<num>", "minimum bytes");
        l.add(null, false, 4, new int[]{-1}, "<num>", "maximum bytes");
        l.add(null, false, 2, new int[]{3}, "tcp-winscale", "set tcp window scale");
        l.add(null, false, 3, new int[]{-1}, "<num>", "factor");
        l.add(null, false, 2, new int[]{3}, "udp-portrange", "set udp client range");
        l.add(null, false, 3, new int[]{4}, "<num>", "lower bound");
        l.add(null, false, 4, new int[]{-1}, "<num>", "upper bound");
        l.add(null, false, 2, new int[]{3}, "tcp-portrange", "set tcp client range");
        l.add(null, false, 3, new int[]{4}, "<num>", "lower bound");
        l.add(null, false, 4, new int[]{-1}, "<num>", "upper bound");
        l.add(null, false, 2, new int[]{3}, "ludp-portrange", "set ludp client range");
        l.add(null, false, 3, new int[]{4}, "<num>", "lower bound");
        l.add(null, false, 4, new int[]{-1}, "<num>", "upper bound");
        l.add(null, false, 2, new int[]{3}, "dccp-portrange", "set dccp client range");
        l.add(null, false, 3, new int[]{4}, "<num>", "lower bound");
        l.add(null, false, 4, new int[]{-1}, "<num>", "upper bound");
        l.add(null, false, 2, new int[]{3}, "sctp-portrange", "set sctp client range");
        l.add(null, false, 3, new int[]{4}, "<num>", "lower bound");
        l.add(null, false, 4, new int[]{-1}, "<num>", "upper bound");
        l.add(null, false, 2, new int[]{3}, "access-subnet-ipv4", "access subnet length");
        l.add(null, false, 3, new int[]{-1}, "<num>", "bits");
        l.add(null, false, 2, new int[]{3}, "access-subnet-ipv6", "access subnet length");
        l.add(null, false, 3, new int[]{-1}, "<num>", "bits");
        l.add(null, false, 2, new int[]{3}, "access-supernet-ipv4", "access subnet length");
        l.add(null, false, 3, new int[]{-1}, "<num>", "bits");
        l.add(null, false, 2, new int[]{3}, "access-supernet-ipv6", "access subnet length");
        l.add(null, false, 3, new int[]{-1}, "<num>", "bits");
        l.add(null, false, 2, new int[]{-1}, "ftp-passive", "use passive mode ftp");
        l.add(null, false, 2, new int[]{-1}, "ftp-active", "use active mode ftp");
        l.add(null, false, 2, new int[]{3}, "ftp-proxy", "specify proxy profile");
        l.add(null, false, 3, new int[]{-1}, "<name:prx>", "name of profile");
        l.add(null, false, 2, new int[]{3}, "pop3-proxy", "specify proxy profile");
        l.add(null, false, 3, new int[]{-1}, "<name:prx>", "name of profile");
        l.add(null, false, 2, new int[]{3}, "tls-version", "specify tls version");
        l.add(null, false, 3, new int[]{4}, "<num>", "forced minimum version");
        l.add(null, false, 4, new int[]{-1}, "<num>", "forced maximum version");
        l.add(null, false, 2, new int[]{3}, "ssh-agent", "specify ssh agent to fake");
        l.add(null, false, 3, new int[]{3, -1}, "<str>", "name of faker");
        l.add(null, false, 2, new int[]{3}, "ssh-group", "specify ssh group");
        l.add(null, false, 3, new int[]{4}, "<num>", "forced minimum size");
        l.add(null, false, 4, new int[]{-1}, "<num>", "forced maximum size");
        l.add(null, false, 2, new int[]{3}, "time-server", "specify name of time server");
        l.add(null, false, 3, new int[]{-1}, "<str>", "name of server");
        l.add(null, false, 2, new int[]{3}, "time-zone", "specify time zone");
        l.add(null, false, 3, new int[]{-1}, "<str>", "name of time zone");
        l.add(null, false, 2, new int[]{3}, "time-proxy", "specify proxy profile");
        l.add(null, false, 3, new int[]{-1}, "<name:prx>", "name of profile");
        l.add(null, false, 2, new int[]{3}, "tftp-proxy", "specify proxy profile");
        l.add(null, false, 3, new int[]{-1}, "<name:prx>", "name of profile");
        l.add(null, false, 2, new int[]{3}, "http-agent", "specify http agent to fake");
        l.add(null, false, 3, new int[]{3, -1}, "<str>", "name of faker");
        l.add(null, false, 2, new int[]{3}, "http-proxy", "specify proxy profile");
        l.add(null, false, 3, new int[]{-1}, "<name:prx>", "name of profile");
        l.add(null, false, 2, new int[]{3}, "mail-proxy", "specify proxy profile");
        l.add(null, false, 3, new int[]{-1}, "<name:prx>", "name of profile");
        l.add(null, false, 2, new int[]{3}, "mail-server", "specify name of mail server");
        l.add(null, false, 3, new int[]{-1}, "<str>", "name of server");
        l.add(null, false, 2, new int[]{3}, "mail-username", "specify username on mail server");
        l.add(null, false, 3, new int[]{-1}, "<str>", "set username");
        l.add(null, false, 2, new int[]{3}, "mail-password", "specify password on mail server");
        l.add(null, false, 3, new int[]{-1}, "<text>", "set password");
        l.add(null, false, 1, new int[]{2}, "ipx", "ipx config commands");
        l.add(null, false, 2, new int[]{3}, "route", "configure static routes");
        l.add(null, false, 3, new int[]{4}, "<name:vrf>", "name of routing table");
        l.add(null, false, 4, new int[]{5}, "<network>", "destination network");
        l.add(null, false, 5, new int[]{6}, "<mask>", "destination mask");
        l.add(null, false, 6, new int[]{-1}, "<nexthop>", "forwarding router's address");
        l.add(null, false, 1, new int[]{2}, "ipv4", "internet protocol config commands");
        getHelpIpX(l);
        l.add(null, false, 2, new int[]{3}, "pool", "configure address pool");
        l.add(null, false, 3, new int[]{4}, "<name:pl4>", "name of address pool");
        l.add(null, false, 4, new int[]{5}, "<first>", "first address in pool");
        l.add(null, false, 5, new int[]{6}, "<inc>", "increment in address format");
        l.add(null, false, 6, new int[]{-1}, "<num>", "number of addresses");
        l.add(null, false, 1, new int[]{2}, "ipv6", "internet protocol config commands");
        getHelpIpX(l);
        l.add(null, false, 2, new int[]{3}, "pool", "configure address pool");
        l.add(null, false, 3, new int[]{4}, "<name:pl6>", "name of address pool");
        l.add(null, false, 4, new int[]{5}, "<first>", "first address in pool");
        l.add(null, false, 5, new int[]{6}, "<inc>", "increment in address format");
        l.add(null, false, 6, new int[]{-1}, "<num>", "number of addresses");
        l.add(null, false, 1, new int[]{2}, "scheduler", "configure a scheduler");
        l.add(null, false, 2, new int[]{-1}, "<name:sch>", "name of scheduler");
        l.add(null, false, 1, new int[]{2}, "script", "configure a script");
        l.add(null, false, 2, new int[]{-1}, "<name:scr>", "name of script");
        l.add(null, false, 1, new int[]{2}, "tracker", "configure a tracker");
        l.add(null, false, 2, new int[]{-1}, "<name:trk>", "name of tracker");
        l.add(null, false, 1, new int[]{2}, "mtracker", "configure a mtracker");
        l.add(null, false, 2, new int[]{-1}, "<name:mtr>", "name of mtracker");
        l.add(null, false, 1, new int[]{2}, "alias", "configure a command alias");
        l.add(null, false, 2, new int[]{3}, "exec", "exec alias");
        l.add(null, false, 2, new int[]{3}, "show", "show alias");
        l.add(null, false, 2, new int[]{3}, "clear", "clear alias");
        l.add(null, false, 2, new int[]{3}, "packet", "packet alias");
        l.add(null, false, 2, new int[]{3}, "flash", "flash alias");
        l.add(null, false, 2, new int[]{3}, "test", "test alias");
        l.add(null, false, 3, new int[]{4}, "<str>", "name of new command");
        l.add(null, false, 4, new int[]{-1}, "hidden", "hide the command");
        l.add(null, false, 4, new int[]{-1}, "error-free", "check errors before the 2nd command");
        l.add(null, false, 4, new int[]{5}, "command", "specify command to execute");
        l.add(null, false, 5, new int[]{5, -1}, "<cmd>", "command");
        l.add(null, false, 4, new int[]{5}, "cmd2nd", "specify 2nd command to execute");
        l.add(null, false, 5, new int[]{5, -1}, "<cmd>", "command");
        l.add(null, false, 4, new int[]{5}, "cmd3rd", "specify 3rd command to execute");
        l.add(null, false, 5, new int[]{5, -1}, "<cmd>", "command");
        l.add(null, false, 4, new int[]{5}, "cmd4th", "specify 4th command to execute");
        l.add(null, false, 5, new int[]{5, -1}, "<cmd>", "command");
        l.add(null, false, 4, new int[]{5}, "cmd5th", "specify 5th command to execute");
        l.add(null, false, 5, new int[]{5, -1}, "<cmd>", "command");
        l.add(null, false, 4, new int[]{5}, "description", "specify help description");
        l.add(null, false, 5, new int[]{5, -1}, "<text>", "help text");
        l.add(null, false, 4, new int[]{-1}, "sticky-onlychanged", "execute only if change happened");
        l.add(null, false, 4, new int[]{-1}, "sticky-onlysuccess", "remember the choice only if succeeded");
        l.add(null, false, 4, new int[]{-1}, "sticky-persistent", "no need to execute during bootup");
        l.add(null, false, 4, new int[]{5}, "sticky-extpersist", "read state from file during bootup");
        l.add(null, false, 5, new int[]{5, -1}, "<text>", "file name");
        l.add(null, false, 4, new int[]{5}, "sticky-param", "specify sticky parameter");
        l.add(null, false, 5, new int[]{5, -1}, "<text>", "parameter text");
        l.add(null, false, 4, new int[]{5}, "default-param", "specify default parameter");
        l.add(null, false, 5, new int[]{5, -1}, "<text>", "parameter text");
        l.add(null, false, 4, new int[]{5}, "parameter", "specify parameter existence");
        l.add(null, false, 5, new int[]{-1}, "forbidden", "no parameters");
        l.add(null, false, 5, new int[]{-1}, "required", "need parameters");
        l.add(null, false, 5, new int[]{-1}, "optional", "parameters allowed");
        l.add(null, false, 4, new int[]{5}, "param2nd", "specify 2nd parameter existence");
        l.add(null, false, 5, new int[]{-1}, "forbidden", "no parameters");
        l.add(null, false, 5, new int[]{-1}, "required", "need parameters");
        l.add(null, false, 5, new int[]{-1}, "optional", "parameters allowed");
        l.add(null, false, 4, new int[]{5}, "param3rd", "specify 3rd parameter existence");
        l.add(null, false, 5, new int[]{-1}, "forbidden", "no parameters");
        l.add(null, false, 5, new int[]{-1}, "required", "need parameters");
        l.add(null, false, 5, new int[]{-1}, "optional", "parameters allowed");
        l.add(null, false, 4, new int[]{5}, "param4th", "specify 4th parameter existence");
        l.add(null, false, 5, new int[]{-1}, "forbidden", "no parameters");
        l.add(null, false, 5, new int[]{-1}, "required", "need parameters");
        l.add(null, false, 5, new int[]{-1}, "optional", "parameters allowed");
        l.add(null, false, 4, new int[]{5}, "param5th", "specify 5th parameter existence");
        l.add(null, false, 5, new int[]{-1}, "forbidden", "no parameters");
        l.add(null, false, 5, new int[]{-1}, "required", "need parameters");
        l.add(null, false, 5, new int[]{-1}, "optional", "parameters allowed");
        l.add(null, false, 1, new int[]{2}, "router", "enable a routing protocol");
        cfgRtr.getRouterList(l, 0, " to configure");
        l.add(null, false, 3, new int[]{4, -1}, "<num:rtr>", "process id");
        l.add(null, false, 4, new int[]{5}, "vrf", "bind a vrf");
        l.add(null, false, 5, new int[]{-1}, "<name:vrf>", "vrf to bind to");
        l.add(null, false, 1, new int[]{2}, "chat-script", "build a chat script");
        l.add(null, false, 2, new int[]{-1}, "<name:cht>", "name of script");
        l.add(null, false, 1, new int[]{2}, "object-group", "build an object group");
        l.add(null, false, 2, new int[]{3}, "network", "network entries");
        l.add(null, false, 3, new int[]{-1}, "<name:ogn>", "name of object group");
        l.add(null, false, 2, new int[]{3}, "port", "port entries");
        l.add(null, false, 3, new int[]{-1}, "<name:ogp>", "name of object group");
        l.add(null, false, 1, new int[]{2}, "access-list", "build an access list");
        l.add(null, false, 2, new int[]{-1}, "<name:acl>", "name of access list");
        l.add(null, false, 1, new int[]{2}, "telemetry", "telemetry configuration");
        l.add(null, false, 2, new int[]{-1}, "<name:tlm>", "name of destination");
        l.add(null, false, 1, new int[]{2}, "event-manager", "build an event manager");
        l.add(null, false, 2, new int[]{-1}, "<name:eem>", "name of event manager");
        l.add(null, false, 1, new int[]{2}, "prefix-list", "build a prefix list");
        l.add(null, false, 2, new int[]{-1}, "<name:pl>", "name of prefix list");
        l.add(null, false, 1, new int[]{2}, "route-map", "build a route map");
        l.add(null, false, 2, new int[]{-1}, "<name:rm>", "name of route map");
        l.add(null, false, 1, new int[]{2}, "route-policy", "build a route policy");
        l.add(null, false, 2, new int[]{-1}, "<name:rpl>", "name of route policy");
        l.add(null, false, 1, new int[]{2}, "policy-map", "build a policy map");
        l.add(null, false, 2, new int[]{-1}, "<name:pm>", "name of policy map");
        l.add(null, false, 1, new int[]{2}, "aaa", "authentication configuration");
        l.add(null, false, 2, new int[]{3}, "userlist", "build a user list");
        l.add(null, false, 3, new int[]{-1}, "<name:aaa>", "name of authenticator");
        l.add(null, false, 2, new int[]{3}, "radius", "set up a radius client");
        l.add(null, false, 3, new int[]{-1}, "<name:aaa>", "name of authenticator");
        l.add(null, false, 2, new int[]{3}, "ldap", "set up a ldap client");
        l.add(null, false, 3, new int[]{-1}, "<name:aaa>", "name of authenticator");
        l.add(null, false, 2, new int[]{3}, "tacacs", "set up a tacacs client");
        l.add(null, false, 3, new int[]{-1}, "<name:aaa>", "name of authenticator");
        l.add(null, false, 2, new int[]{3}, "list", "set up an aaa list");
        l.add(null, false, 3, new int[]{-1}, "<name:aaa>", "name of authenticator");
        l.add(null, false, 1, new int[]{2}, "vpdn", "vpdn client parameters");
        l.add(null, false, 2, new int[]{-1}, "<name:vpd>", "name of client");
        l.add(null, false, 1, new int[]{2}, "proxy-profile", "proxy profile parameters");
        l.add(null, false, 2, new int[]{-1}, "<name:prx>", "name of profile");
        l.add(null, false, 1, new int[]{2}, "time-map", "time map parameters");
        l.add(null, false, 2, new int[]{-1}, "<name:tm>", "name of profile");
        l.add(null, false, 1, new int[]{2}, "crypto", "cryptographic configuration");
        l.add(null, false, 2, new int[]{3}, "ipsec", "ipsec profile");
        l.add(null, false, 3, new int[]{-1}, "<name:ips>", "name of profile");
        l.add(null, false, 2, new int[]{3}, "rsakey", "rsa key");
        l.add(null, false, 3, new int[]{4}, "<name:rsa>", "name of key");
        l.add(null, false, 4, new int[]{5}, "import", "import key");
        l.add(null, false, 5, new int[]{-1}, "<text>", "base64 encoded private key");
        l.add(null, false, 4, new int[]{5}, "external", "load key from file");
        l.add(null, false, 5, new int[]{-1}, "<text>", "file name");
        l.add(null, false, 4, new int[]{5, -1}, "generate", "generate new key");
        l.add(null, false, 5, new int[]{-1}, "[size]", "key size in bits");
        l.add(null, false, 4, new int[]{-1}, "zeroize", "delete the key");
        l.add(null, false, 4, new int[]{-1}, "editor", "import in editor");
        l.add(null, false, 2, new int[]{3}, "dsakey", "dsa key");
        l.add(null, false, 3, new int[]{4}, "<name:dsa>", "name of key");
        l.add(null, false, 4, new int[]{5}, "import", "import key");
        l.add(null, false, 5, new int[]{-1}, "<text>", "base64 encoded private key");
        l.add(null, false, 4, new int[]{5}, "external", "load key from file");
        l.add(null, false, 5, new int[]{-1}, "<text>", "file name");
        l.add(null, false, 4, new int[]{5, -1}, "generate", "generate new key");
        l.add(null, false, 5, new int[]{-1}, "[size]", "key size in bits");
        l.add(null, false, 4, new int[]{-1}, "zeroize", "delete the key");
        l.add(null, false, 4, new int[]{-1}, "editor", "import in editor");
        l.add(null, false, 2, new int[]{3}, "ecdsakey", "ecdsa key");
        l.add(null, false, 3, new int[]{4}, "<name:ecd>", "name of key");
        l.add(null, false, 4, new int[]{5}, "import", "import key");
        l.add(null, false, 5, new int[]{-1}, "<text>", "base64 encoded private key");
        l.add(null, false, 4, new int[]{5}, "external", "load key from file");
        l.add(null, false, 5, new int[]{-1}, "<text>", "file name");
        l.add(null, false, 4, new int[]{5, -1}, "generate", "generate new key");
        l.add(null, false, 5, new int[]{-1}, "[size]", "key size in bits");
        l.add(null, false, 4, new int[]{-1}, "zeroize", "delete the key");
        l.add(null, false, 4, new int[]{-1}, "editor", "import in editor");
        l.add(null, false, 2, new int[]{3}, "mldsakey", "mldsa key");
        l.add(null, false, 3, new int[]{4}, "<name:mld>", "name of key");
        l.add(null, false, 4, new int[]{5}, "import", "import key");
        l.add(null, false, 5, new int[]{-1}, "<text>", "base64 encoded private key");
        l.add(null, false, 4, new int[]{5}, "external", "load key from file");
        l.add(null, false, 5, new int[]{-1}, "<text>", "file name");
        l.add(null, false, 4, new int[]{5, -1}, "generate", "generate new key");
        l.add(null, false, 5, new int[]{-1}, "[size]", "key size in bits");
        l.add(null, false, 4, new int[]{-1}, "zeroize", "delete the key");
        l.add(null, false, 4, new int[]{-1}, "editor", "import in editor");
        l.add(null, false, 2, new int[]{3}, "certificate", "certificate");
        l.add(null, false, 3, new int[]{4}, "<name:crt>", "name of certificate");
        l.add(null, false, 4, new int[]{5}, "import", "import certificate");
        l.add(null, false, 5, new int[]{6}, "rsa", "rsa key");
        l.add(null, false, 6, new int[]{7}, "<name:rsa>", "name of key");
        l.add(null, false, 7, new int[]{-1}, "<text>", "base64 encoded certificate");
        l.add(null, false, 5, new int[]{6}, "dsa", "dsa key");
        l.add(null, false, 6, new int[]{7}, "<name:dsa>", "name of key");
        l.add(null, false, 7, new int[]{-1}, "<text>", "base64 encoded certificate");
        l.add(null, false, 5, new int[]{6}, "ecdsa", "ecdsa key");
        l.add(null, false, 6, new int[]{7}, "<name:ecd>", "name of key");
        l.add(null, false, 7, new int[]{-1}, "<text>", "base64 encoded certificate");
        l.add(null, false, 5, new int[]{6}, "mldsa", "mldsa key");
        l.add(null, false, 6, new int[]{7}, "<name:mld>", "name of key");
        l.add(null, false, 7, new int[]{-1}, "<text>", "base64 encoded certificate");
        l.add(null, false, 4, new int[]{5}, "external", "load certificate from file");
        l.add(null, false, 5, new int[]{6}, "rsa", "rsa key");
        l.add(null, false, 6, new int[]{7}, "<name:rsa>", "name of key");
        l.add(null, false, 7, new int[]{-1}, "<text>", "file name");
        l.add(null, false, 5, new int[]{6}, "dsa", "dsa key");
        l.add(null, false, 6, new int[]{7}, "<name:dsa>", "name of key");
        l.add(null, false, 7, new int[]{-1}, "<text>", "file name");
        l.add(null, false, 5, new int[]{6}, "ecdsa", "ecdsa key");
        l.add(null, false, 6, new int[]{7}, "<name:ecd>", "name of key");
        l.add(null, false, 7, new int[]{-1}, "<text>", "file name");
        l.add(null, false, 5, new int[]{6}, "mldsa", "mldsa key");
        l.add(null, false, 6, new int[]{7}, "<name:mld>", "name of key");
        l.add(null, false, 7, new int[]{-1}, "<text>", "base64 encoded certificate");
        l.add(null, false, 4, new int[]{5}, "generate", "generate new certificate");
        l.add(null, false, 5, new int[]{6}, "rsa", "rsa key");
        l.add(null, false, 6, new int[]{7, -1}, "<name:rsa>", "name of key");
        l.add(null, false, 7, new int[]{8, -1}, "<text>", "identifier to give");
        l.add(null, false, 8, new int[]{-1}, "<num>", "validity in days");
        l.add(null, false, 5, new int[]{6}, "dsa", "dsa key");
        l.add(null, false, 6, new int[]{7, -1}, "<name:dsa>", "name of key");
        l.add(null, false, 7, new int[]{8, -1}, "<text>", "identifier to give");
        l.add(null, false, 8, new int[]{-1}, "<num>", "validity in days");
        l.add(null, false, 5, new int[]{6}, "ecdsa", "ecdsa key");
        l.add(null, false, 6, new int[]{7, -1}, "<name:ecd>", "name of key");
        l.add(null, false, 7, new int[]{8, -1}, "<text>", "identifier to give");
        l.add(null, false, 8, new int[]{-1}, "<num>", "validity in days");
        l.add(null, false, 5, new int[]{6}, "mldsa", "mldsa key");
        l.add(null, false, 6, new int[]{7}, "<name:mld>", "name of key");
        l.add(null, false, 7, new int[]{8, -1}, "<text>", "identifier to give");
        l.add(null, false, 8, new int[]{-1}, "<num>", "validity in days");
        l.add(null, false, 4, new int[]{-1}, "zeroize", "delete the certificate");
        l.add(null, false, 4, new int[]{5}, "editor", "import in editor");
        l.add(null, false, 5, new int[]{6}, "rsa", "rsa key");
        l.add(null, false, 6, new int[]{-1}, "<name:rsa>", "name of key");
        l.add(null, false, 5, new int[]{6}, "dsa", "dsa key");
        l.add(null, false, 6, new int[]{-1}, "<name:dsa>", "name of key");
        l.add(null, false, 5, new int[]{6}, "ecdsa", "ecdsa key");
        l.add(null, false, 6, new int[]{-1}, "<name:ecd>", "name of key");
        l.add(null, false, 5, new int[]{6}, "mldsa", "mldsa key");
        l.add(null, false, 6, new int[]{-1}, "<name:mld>", "name of key");
        l.add(null, false, 1, new int[]{2}, "xconnect", "define one protocol cross connection");
        l.add(null, false, 2, new int[]{-1}, "<name:xcn>", "name of connection");
        l.add(null, false, 1, new int[]{2}, "menu", "define one menu");
        l.add(null, false, 2, new int[]{3}, "key", "letter based");
        l.add(null, false, 3, new int[]{-1}, "<name:mnk>", "name of menu");
        l.add(null, false, 2, new int[]{3}, "tui", "tui based");
        l.add(null, false, 3, new int[]{-1}, "<name:mnt>", "name of menu");
        userHelp k = new userHelp();
        k.expand = true;
        k.add(null, false, 4, new int[]{5}, "vrf", "bind a vrf");
        k.add(null, false, 5, new int[]{4, -1}, "<name:vrf>", "vrf to bind to");
        k.add(null, false, 4, new int[]{5}, "interface", "bind an interface");
        k.add(null, false, 5, new int[]{4, -1}, "<name:ifc>", "interface to bind to");
        l.add(null, false, 1, new int[]{2}, "server", "create new or update existing server process");
        servGenList.srvHelp(l, 2, "to configure", k);
    }

    private byte[] cmdGetRem() {
        return bits.byteConcat(cmd.getRemaining().getBytes(), pipeSide.getEnding(pipeSide.modTyp.modeCRLF));
    }

    private void doGlobal() {
        String a = cmd.word();
        if (a.equals("vdc")) {
            if (cfgAll.evalVdcPrivs()) {
                cmd.error("not in a vdc");
                return;
            }
            a = cmd.word();
            if (a.equals("definition")) {
                a = cmd.word();
                submode = cfgAll.vdcFind(a, commits == null);
                if (submode == null) {
                    cmd.error("bad vdc name");
                    return;
                }
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("process")) {
            if (cfgAll.evalVdcPrivs()) {
                cmd.error("not in a vdc");
                return;
            }
            a = cmd.word();
            if (a.equals("definition")) {
                a = cmd.word();
                submode = cfgAll.prcFind(a, commits == null);
                if (submode == null) {
                    cmd.error("bad process name");
                    return;
                }
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("vrf")) {
            a = cmd.word();
            if (a.equals("definition")) {
                a = cmd.word();
                submode = cfgAll.vrfFind(a, commits == null);
                if (submode == null) {
                    cmd.error("bad vrf name");
                    return;
                }
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("interface")) {
            submode = cfgAll.ifcFind(cmd.word(), commits == null ? 1 : 0);
            if (submode == null) {
                cmd.error("no such interface");
                return;
            }
            return;
        }
        if (a.equals("bridge")) {
            submode = cfgAll.brdgFind(cmd.word(), commits == null);
            if (submode == null) {
                cmd.error("invalid bridge number");
                return;
            }
            return;
        }
        if (a.equals("bundle")) {
            submode = cfgAll.bndlFind(cmd.word(), commits == null);
            if (submode == null) {
                cmd.error("invalid bundle number");
                return;
            }
            return;
        }
        if (a.equals("hairpin")) {
            submode = cfgAll.hrpnFind(cmd.word(), commits == null);
            if (submode == null) {
                cmd.error("invalid hairpin number");
                return;
            }
            return;
        }
        if (a.equals("session")) {
            submode = cfgAll.sessnFind(cmd.word(), commits == null);
            if (submode == null) {
                cmd.error("invalid session name");
                return;
            }
            return;
        }
        if (a.equals("check")) {
            submode = cfgAll.checkFind(cmd.word(), commits == null);
            if (submode == null) {
                cmd.error("invalid check name");
                return;
            }
            return;
        }
        if (a.equals("sensor")) {
            submode = cfgAll.sensorFind(cmd.word(), commits == null);
            if (submode == null) {
                cmd.error("invalid sensor name");
                return;
            }
            return;
        }
        if (a.equals("dial-peer")) {
            submode = cfgAll.dialFind(cmd.word(), commits == null);
            if (submode == null) {
                cmd.error("invalid dial peer number");
                return;
            }
            return;
        }
        if (a.equals("translation-rule")) {
            submode = cfgAll.trnsltnFind(cmd.word(), commits == null);
            if (submode == null) {
                cmd.error("invalid translation rule number");
                return;
            }
            return;
        }
        if (a.equals("router")) {
            tabRouteAttr.routeType o = cfgRtr.name2num(cmd.word());
            if (o == null) {
                cmd.error("invalid routing protocol");
                return;
            }
            cfgRtr rtr = cfgAll.rtrFind(o, bits.str2num(cmd.word()), commits == null);
            if (rtr == null) {
                cmd.error("bad process number");
                return;
            }
            submode = rtr;
            if (commits != null) {
                return;
            }
            cmds c = cmd.copyBytes(false);
            if (!cmd.word().equals("vrf")) {
                return;
            }
            a = c.getRemaining();
            userHelp hlp = getHelping(false, true, true);
            reader.setContext(hlp, "");
            String b = hlp.repairLine(a);
            if (b.length() < 1) {
                pipe.linePut("bad: " + a);
                reader.setContext(hlp, "");
                return;
            }
            rtr.embedVrf = true;
            rtr.doCfgStr(c);
            return;
        }
        if (a.equals("scheduler")) {
            submode = cfgAll.schedFind(cmd.word(), commits == null);
            if (submode == null) {
                cmd.error("bad scheduler name");
                return;
            }
            return;
        }
        if (a.equals("script")) {
            submode = cfgAll.scrptFind(cmd.word(), commits == null);
            if (submode == null) {
                cmd.error("bad script name");
                return;
            }
            return;
        }
        if (a.equals("tracker")) {
            submode = cfgAll.trackFind(cmd.word(), commits == null);
            if (submode == null) {
                cmd.error("bad track name");
                return;
            }
            return;
        }
        if (a.equals("mtracker")) {
            submode = cfgAll.mtrackFind(cmd.word(), commits == null);
            if (submode == null) {
                cmd.error("bad mtrack name");
                return;
            }
            return;
        }
        if (a.equals("chat-script")) {
            submode = cfgAll.chatFind(cmd.word(), commits == null);
            if (submode == null) {
                cmd.error("bad script name");
                return;
            }
            return;
        }
        if (a.equals("object-group")) {
            a = cmd.word();
            if (a.equals("network")) {
                submode = cfgAll.objnetFind(cmd.word(), commits == null);
                if (submode == null) {
                    cmd.error("bad object group name");
                    return;
                }
                return;
            }
            if (a.equals("port")) {
                submode = cfgAll.objprtFind(cmd.word(), commits == null);
                if (submode == null) {
                    cmd.error("bad object group name");
                    return;
                }
                return;
            }
            return;
        }
        if (a.equals("access-list")) {
            submode = cfgAll.aclsFind(cmd.word(), commits == null);
            if (submode == null) {
                cmd.error("bad access list name");
                return;
            }
            return;
        }
        if (a.equals("telemetry")) {
            submode = cfgAll.tlmdsFind(cmd.word(), commits == null);
            if (submode == null) {
                cmd.error("bad destination name");
                return;
            }
            return;
        }
        if (a.equals("event-manager")) {
            submode = cfgAll.eemFind(cmd.word(), commits == null);
            if (submode == null) {
                cmd.error("bad event manager name");
                return;
            }
            return;
        }
        if (a.equals("xconnect")) {
            submode = cfgAll.xconFind(cmd.word(), commits == null);
            if (submode == null) {
                cmd.error("bad connect name");
                return;
            }
            return;
        }
        if (a.equals("vnet")) {
            submode = cfgAll.vnetFind(cmd.word(), commits == null);
            if (submode == null) {
                cmd.error("bad vnet name");
                return;
            }
            return;
        }
        if (a.equals("vpdn")) {
            submode = cfgAll.vpdnFind(cmd.word(), commits == null);
            if (submode == null) {
                cmd.error("bad vpdn name");
                return;
            }
            return;
        }
        if (a.equals("proxy-profile")) {
            submode = cfgAll.proxyFind(cmd.word(), commits == null);
            if (submode == null) {
                cmd.error("bad proxy name");
                return;
            }
            return;
        }
        if (a.equals("time-map")) {
            submode = cfgAll.timeFind(cmd.word(), commits == null);
            if (submode == null) {
                cmd.error("bad time name");
                return;
            }
            return;
        }
        if (a.equals("prefix-list")) {
            submode = cfgAll.prfxFind(cmd.word(), commits == null);
            if (submode == null) {
                cmd.error("bad prefix list name");
                return;
            }
            return;
        }
        if (a.equals("route-map")) {
            submode = cfgAll.rtmpFind(cmd.word(), commits == null);
            if (submode == null) {
                cmd.error("bad route map name");
                return;
            }
            return;
        }
        if (a.equals("route-policy")) {
            submode = cfgAll.rtplFind(cmd.word(), commits == null);
            if (submode == null) {
                cmd.error("bad route policy name");
                return;
            }
            return;
        }
        if (a.equals("policy-map")) {
            submode = cfgAll.plmpFind(cmd.word(), commits == null);
            if (submode == null) {
                cmd.error("bad policy map name");
                return;
            }
            return;
        }
        if (a.equals("console0")) {
            submode = cfgAll.con0;
            return;
        }
        if (a.equals("line")) {
            submode = cfgAll.linFind(cmd.word());
            if (submode == null) {
                cmd.error("invalid line name");
                return;
            }
            return;
        }
        if (a.equals("menu")) {
            a = cmd.word();
            if (a.equals("key")) {
                submode = cfgAll.menuKfind(cmd.word(), commits == null);
                if (submode == null) {
                    cmd.error("invalid menu name");
                    return;
                }
                return;
            }
            if (a.equals("tui")) {
                submode = cfgAll.menuTfind(cmd.word(), commits == null);
                if (submode == null) {
                    cmd.error("invalid menu name");
                    return;
                }
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("aaa")) {
            cfgAuther.methodType i = cfgAuther.string2auther(cmd.word());
            if (i == null) {
                cmd.badCmd();
                return;
            }
            if (commits != null) {
                i = null;
            }
            submode = cfgAll.autherFind(cmd.word(), i);
            if (submode == null) {
                cmd.error("invalid authenticator name");
                return;
            }
            return;
        }
        if (a.equals("server")) {
            a = cmd.word();
            servGeneric srv = servGenList.srvFind(a, cmd.word(), commits == null);
            if (srv == null) {
                cmd.error("invalid server");
                return;
            }
            submode = srv;
            if (commits != null) {
                return;
            }
            boolean b = false;
            for (;;) {
                a = cmd.word();
                if (a.length() < 1) {
                    break;
                }
                if (a.equals("vrf")) {
                    a = cmd.word();
                    srv.srvVrf = cfgAll.vrfFind(a, false);
                    b = true;
                    continue;
                }
                if (a.equals("interface")) {
                    a = cmd.word();
                    srv.srvIface = cfgAll.ifcFind(a, 0);
                    b = true;
                    continue;
                }
            }
            if (!b) {
                return;
            }
            srv.srvEmbedVrf |= b;
            srv.srvInit();
            return;
        }
        if (a.equals("crypto")) {
            a = cmd.word();
            if (a.equals("ipsec")) {
                submode = cfgAll.ipsecFind(cmd.word(), commits == null);
                if (submode == null) {
                    cmd.error("bad profile name");
                    return;
                }
                return;
            }
            if (commits != null) {
                return;
            }
            if (a.equals("rsakey")) {
                cryptoDoKey(cfgAll.rsakeys, new cryKeyRSA());
                return;
            }
            if (a.equals("dsakey")) {
                cryptoDoKey(cfgAll.dsakeys, new cryKeyDSA());
                return;
            }
            if (a.equals("ecdsakey")) {
                cryptoDoKey(cfgAll.ecdsakeys, new cryKeyECDSA());
                return;
            }
            if (a.equals("mldsakey")) {
                cryptoDoKey(cfgAll.mldsakeys, new cryKeyMLDSA());
                return;
            }
            if (a.equals("certificate")) {
                String nam = cmd.word();
                a = cmd.word();
                if (a.equals("zeroize")) {
                    cfgAll.certDel(nam);
                    return;
                }
                if (a.equals("editor")) {
                    cryKeyGeneric k = findKey();
                    if (k == null) {
                        return;
                    }
                    cryCertificate c = new cryCertificate();
                    c.crtName = nam;
                    List<String> txt = new ArrayList<String>();
                    userEditor e = new userEditor(new pipeScreen(cmd.pipe), txt, "cert", false);
                    if (e.doEdit()) {
                        return;
                    }
                    if (c.pemReadLst(txt)) {
                        cmd.error("error decoding");
                        return;
                    }
                    c.key = k;
                    cfgCert cfg = cfgAll.certFind(nam, true);
                    cfg.cert = c;
                    cfg.key = k;
                    return;
                }
                if (a.equals("import")) {
                    cryKeyGeneric k = findKey();
                    if (k == null) {
                        return;
                    }
                    cryCertificate c = new cryCertificate();
                    c.crtName = nam;
                    a = authLocal.passwdDecode(cmd.word());
                    if (a == null) {
                        cmd.error("error reading");
                        return;
                    }
                    if (c.pemReadStr(a)) {
                        cmd.error("error decoding");
                        return;
                    }
                    c.key = k;
                    cfgCert cfg = cfgAll.certFind(nam, true);
                    cfg.cert = c;
                    cfg.key = k;
                    return;
                }
                if (a.equals("external")) {
                    cryKeyGeneric k = findKey();
                    if (k == null) {
                        return;
                    }
                    a = cmd.word();
                    cryCertificate c = new cryCertificate();
                    c.crtName = nam;
                    List<String> t = bits.txt2buf(a);
                    if (t == null) {
                        cmd.error("not found");
                        return;
                    }
                    if (c.pemReadLst(t)) {
                        cmd.error("error decoding");
                        return;
                    }
                    c.key = k;
                    cfgCert cfg = cfgAll.certFind(nam, true);
                    cfg.cert = c;
                    cfg.key = k;
                    cfg.filNam = a;
                    return;
                }
                if (a.equals("generate")) {
                    cryKeyGeneric k = findKey();
                    if (k == null) {
                        return;
                    }
                    a = cmd.word();
                    if (a.length() < 1) {
                        a = cfgAll.getFqdn();
                    }
                    int i = bits.str2num(cmd.word());
                    if (i < 1) {
                        i = 3650;
                    }
                    cryCertificate c = cryCertificate.createSelfSigned(k, a, i);
                    c.crtName = nam;
                    cfgCert cfg = cfgAll.certFind(nam, true);
                    cfg.cert = c;
                    cfg.key = k;
                    return;
                }
                cmd.badCmd();
                return;
            }
            cmd.badCmd();
            return;
        }
        if (commits != null) {
            return;
        }
        if (a.equals("ipv4")) {
            doCmdIp4();
            return;
        }
        if (a.equals("ipv6")) {
            doCmdIp6();
            return;
        }
        if (a.equals("nsh")) {
            int p = bits.str2num(cmd.word());
            int i = bits.str2num(cmd.word());
            tabNshEntry ntry = new tabNshEntry(p, i);
            ntry.doCfgStr(cmd);
            tabNshEntry.services.put(ntry);
            return;
        }
        if (a.equals("ipx")) {
            doCmdIpx();
            return;
        }
        if (a.equals("logging")) {
            doCmdLogging();
            return;
        }
        if (a.equals("client")) {
            doCmdClient();
            return;
        }
        if (a.equals("alias")) {
            a = cmd.word();
            cfgAlias ntry = cfgAll.aliasFind(cmd.word(), cfgAlias.string2type(a), true);
            ntry.doCfgStr(cmd);
            return;
        }
        if (a.equals("hostname")) {
            cfgAll.hostName = cmd.word();
            return;
        }
        if (a.equals("locale")) {
            cfgAll.locale = cmd.word();
            return;
        }
        if (a.equals("enable")) {
            cfgAll.enaPass = authLocal.secretDecode(cmd.word());
            return;
        }
        if (a.equals("password-encrypt")) {
            cfgAll.passEnc = authLocal.passwdDecode(cmd.word());
            return;
        }
        if (a.equals("buggy")) {
            cfgAll.buggy = true;
            return;
        }
        if (a.equals("banner")) {
            a = cmd.word();
            if (a.equals("set")) {
                cfgAll.bannerEnc = cmdGetRem();
                return;
            }
            if (a.equals("add")) {
                cfgAll.bannerEnc = bits.byteConcat(cfgAll.bannerEnc, cmdGetRem());
                return;
            }
            if (a.equals("encoded")) {
                byte[] buf = encBase64.decodeBytes(cmd.getRemaining());
                if (buf == null) {
                    cmd.error("error decoding");
                    return;
                }
                cfgAll.bannerEnc = buf;
                return;
            }
            if (a.equals("text")) {
                cfgAll.bannerTxt = cmd.getRemaining();
                return;
            }
            if (a.equals("pixel")) {
                cfgAll.bannerPix = cmd.getRemaining();
                return;
            }
            if (a.equals("image")) {
                cfgAll.bannerImg = cmd.getRemaining();
                return;
            }
            if (a.equals("movie")) {
                cfgAll.bannerMov = cmd.getRemaining();
                return;
            }
            if (!a.equals("editor")) {
                cmd.badCmd();
                return;
            }
            List<String> txt = new ArrayList<String>();
            a = "";
            for (int i = 0; i < cfgAll.bannerEnc.length; i++) {
                byte[] buf = new byte[1];
                buf[0] = cfgAll.bannerEnc[i];
                if (buf[0] == 13) {
                    txt.add(a);
                    a = "";
                }
                if (buf[0] < 32) {
                    continue;
                }
                a = a + new String(buf);
            }
            txt.add(a);
            userEditor e = new userEditor(new pipeScreen(pipe), txt, "banner", false);
            if (e.doEdit()) {
                return;
            }
            String s = "";
            for (int i = 0; i < txt.size(); i++) {
                byte[] buf = pipeSide.getEnding(pipeSide.modTyp.modeCRLF);
                s += txt.get(i) + new String(buf);
            }
            byte[] buf = s.getBytes();
            cfgAll.bannerEnc = buf;
            return;
        }
        if (!a.equals(cmds.negated)) {
            cmd.badCmd();
            return;
        }
        a = cmd.word();
        if (a.equals("locale")) {
            cfgAll.locale = null;
            return;
        }
        if (a.equals("enable")) {
            cfgAll.enaPass = null;
            return;
        }
        if (a.equals("password-encrypt")) {
            cfgAll.passEnc = null;
            return;
        }
        if (a.equals("buggy")) {
            cfgAll.buggy = false;
            return;
        }
        if (a.equals("vdc")) {
            a = cmd.word();
            if (a.equals("definition")) {
                a = cmd.word();
                cfgVdc vdc = cfgAll.vdcDel(a);
                if (vdc == null) {
                    cmd.error("no such vdc");
                    return;
                }
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("process")) {
            a = cmd.word();
            if (a.equals("definition")) {
                a = cmd.word();
                cfgPrcss prc = cfgAll.prcDel(a);
                if (prc == null) {
                    cmd.error("no such process");
                    return;
                }
                prc.stopNow();
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("banner")) {
            a = cmd.word();
            if (a.equals("encoded")) {
                cfgAll.bannerEnc = new byte[0];
                return;
            }
            if (a.equals("add")) {
                cfgAll.bannerEnc = new byte[0];
                return;
            }
            if (a.equals("set")) {
                cfgAll.bannerEnc = new byte[0];
                return;
            }
            if (a.equals("text")) {
                cfgAll.bannerTxt = null;
                return;
            }
            if (a.equals("pixel")) {
                cfgAll.bannerPix = null;
                return;
            }
            if (a.equals("image")) {
                cfgAll.bannerImg = null;
                return;
            }
            if (a.equals("movie")) {
                cfgAll.bannerMov = null;
                return;
            }
            return;
        }
        if (a.equals("vrf")) {
            a = cmd.word();
            if (a.equals("definition")) {
                a = cmd.word();
                cfgVrf vrf = cfgAll.vrfDel(a);
                if (vrf == null) {
                    cmd.error("no such vrf");
                    return;
                }
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("bridge")) {
            cfgBrdg ntry = cfgAll.brdgDel(cmd.word());
            if (ntry == null) {
                cmd.error("invalid bridge number");
                return;
            }
            return;
        }
        if (a.equals("bundle")) {
            cfgBndl ntry = cfgAll.bndlDel(cmd.word());
            if (ntry == null) {
                cmd.error("invalid bundle number");
                return;
            }
            return;
        }
        if (a.equals("hairpin")) {
            cfgHrpn ntry = cfgAll.hrpnDel(cmd.word());
            if (ntry == null) {
                cmd.error("invalid hairpin number");
                return;
            }
            return;
        }
        if (a.equals("session")) {
            cfgSessn ntry = cfgAll.sessnDel(cmd.word());
            if (ntry == null) {
                cmd.error("invalid session name");
                return;
            }
            return;
        }
        if (a.equals("check")) {
            cfgCheck ntry = cfgAll.checkDel(cmd.word());
            if (ntry == null) {
                cmd.error("invalid check name");
                return;
            }
            return;
        }
        if (a.equals("sensor")) {
            cfgSensor ntry = cfgAll.sensorDel(cmd.word());
            if (ntry == null) {
                cmd.error("invalid sensor name");
                return;
            }
            return;
        }
        if (a.equals("dial-peer")) {
            cfgDial ntry = cfgAll.dialDel(cmd.word());
            if (ntry == null) {
                cmd.error("invalid dial peer number");
                return;
            }
            return;
        }
        if (a.equals("translation-rule")) {
            cfgTrnsltn ntry = cfgAll.trnsltnDel(cmd.word());
            if (ntry == null) {
                cmd.error("invalid translation rule number");
                return;
            }
            return;
        }
        if (a.equals("nsh")) {
            int p = bits.str2num(cmd.word());
            int i = bits.str2num(cmd.word());
            tabNshEntry ntry = new tabNshEntry(p, i);
            if (tabNshEntry.services.del(ntry) == null) {
                cmd.error("invalid nsh number");
                return;
            }
            return;
        }
        if (a.equals("router")) {
            tabRouteAttr.routeType o = cfgRtr.name2num(cmd.word());
            if (o == null) {
                cmd.error("invalid routing protocol");
                return;
            }
            cfgRtr ntry = cfgAll.rtrDel(o, bits.str2num(cmd.word()));
            if (ntry == null) {
                cmd.error("bad process number");
                return;
            }
            return;
        }
        if (a.equals("interface")) {
            if (cfgAll.ifcDel(cmd.word(), true)) {
                cmd.error("error deleting interface");
                return;
            }
            return;
        }
        if (a.equals("scheduler")) {
            cfgSched ntry = cfgAll.schedDel(cmd.word());
            if (ntry == null) {
                cmd.error("no such scheduler");
                return;
            }
            ntry.doCfgStr(new cmds("cfg", "stop"));
            return;
        }
        if (a.equals("script")) {
            cfgScrpt ntry = cfgAll.scrptDel(cmd.word());
            if (ntry == null) {
                cmd.error("no such script");
                return;
            }
            ntry.doCfgStr(new cmds("cfg", "stop"));
            return;
        }
        if (a.equals("tracker")) {
            cfgTrack ntry = cfgAll.trackDel(cmd.word());
            if (ntry == null) {
                cmd.error("no such tracker");
                return;
            }
            ntry.doCfgStr(new cmds("cfg", "stop"));
            return;
        }
        if (a.equals("mtracker")) {
            cfgMtrack ntry = cfgAll.mtrackDel(cmd.word());
            if (ntry == null) {
                cmd.error("no such mtracker");
                return;
            }
            ntry.doCfgStr(new cmds("cfg", "stop"));
            return;
        }
        if (a.equals("ipx")) {
            doCmdNoIpx();
            return;
        }
        if (a.equals("ipv4")) {
            doCmdNoIp4();
            return;
        }
        if (a.equals("ipv6")) {
            doCmdNoIp6();
            return;
        }
        if (a.equals("alias")) {
            a = cmd.word();
            cfgAlias ntry = cfgAll.aliasFind(cmd.word(), cfgAlias.string2type(a), true);
            cmd = new cmds("c", cmds.negated + cmds.tabulator + cmd.getRemaining());
            ntry.doCfgStr(cmd);
            if (ntry.command.length() > 0) {
                return;
            }
            cfgAll.aliasDel(ntry.name, cfgAlias.string2type(a));
            return;
        }
        if (a.equals("logging")) {
            doCmdNoLogging();
            return;
        }
        if (a.equals("crypto")) {
            a = cmd.word();
            if (a.equals("ipsec")) {
                cfgAll.ipsecDel(cmd.word());
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("chat-script")) {
            cfgChat prf = cfgAll.chatDel(cmd.word());
            if (prf == null) {
                cmd.error("no such script");
                return;
            }
            return;
        }
        if (a.equals("object-group")) {
            a = cmd.word();
            if (a.equals("network")) {
                cfgObjnet prf = cfgAll.objnetDel(cmd.word());
                if (prf == null) {
                    cmd.error("no such object group");
                    return;
                }
                return;
            }
            if (a.equals("port")) {
                cfgObjprt prf = cfgAll.objprtDel(cmd.word());
                if (prf == null) {
                    cmd.error("no such object group");
                    return;
                }
                return;
            }
            return;
        }
        if (a.equals("access-list")) {
            cfgAceslst prf = cfgAll.aclsDel(cmd.word());
            if (prf == null) {
                cmd.error("no such access list");
                return;
            }
            return;
        }
        if (a.equals("telemetry")) {
            cfgTlmtry prf = cfgAll.tlmdsDel(cmd.word());
            if (prf == null) {
                cmd.error("no such destination");
                return;
            }
            prf.worker.stopWork();
            return;
        }
        if (a.equals("event-manager")) {
            cfgEvntmgr prf = cfgAll.eemDel(cmd.word());
            if (prf == null) {
                cmd.error("no such event manager");
                return;
            }
            return;
        }
        if (a.equals("xconnect")) {
            cfgXconn prf = cfgAll.xconDel(cmd.word());
            if (prf == null) {
                cmd.error("no such connect");
                return;
            }
            return;
        }
        if (a.equals("vnet")) {
            cfgVnet prf = cfgAll.vnetDel(cmd.word());
            if (prf == null) {
                cmd.error("bad vnet name");
                return;
            }
            return;
        }
        if (a.equals("vpdn")) {
            cfgVpdn prf = cfgAll.vpdnDel(cmd.word());
            if (prf == null) {
                cmd.error("no such vpdn");
                return;
            }
            return;
        }
        if (a.equals("proxy-profile")) {
            cfgProxy prf = cfgAll.proxyDel(cmd.word());
            if (prf == null) {
                cmd.error("no such profile");
                return;
            }
            return;
        }
        if (a.equals("time-map")) {
            cfgTime prf = cfgAll.timeDel(cmd.word());
            if (prf == null) {
                cmd.error("no such profile");
                return;
            }
            return;
        }
        if (a.equals("prefix-list")) {
            cfgPrfxlst prf = cfgAll.prfxDel(cmd.word());
            if (prf == null) {
                cmd.error("no such prefix list");
                return;
            }
            return;
        }
        if (a.equals("route-map")) {
            cfgRoump prf = cfgAll.rtmpDel(cmd.word());
            if (prf == null) {
                cmd.error("no such route map");
                return;
            }
            return;
        }
        if (a.equals("route-policy")) {
            cfgRouplc prf = cfgAll.rtplDel(cmd.word());
            if (prf == null) {
                cmd.error("no such route policy");
                return;
            }
            return;
        }
        if (a.equals("policy-map")) {
            cfgPlymp prf = cfgAll.plmpDel(cmd.word());
            if (prf == null) {
                cmd.error("no such policy map");
                return;
            }
            return;
        }
        if (a.equals("aaa")) {
            cfgAuther.methodType i = cfgAuther.string2auther(cmd.word());
            if (i == null) {
                cmd.badCmd();
                return;
            }
            cfgAuther prf = cfgAll.autherDel(cmd.word());
            if (prf == null) {
                cmd.error("no such authenticator");
                return;
            }
            return;
        }
        if (a.equals("line")) {
            cmd.error("cannot delete physical line");
            return;
        }
        if (a.equals("menu")) {
            a = cmd.word();
            if (a.equals("key")) {
                cfgMenuK prf = cfgAll.menuKdel(cmd.word());
                if (prf == null) {
                    cmd.error("no such menu");
                    return;
                }
                return;
            }
            if (a.equals("tui")) {
                cfgMenuT prf = cfgAll.menuTdel(cmd.word());
                if (prf == null) {
                    cmd.error("no such menu");
                    return;
                }
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("server")) {
            a = cmd.word();
            servGeneric ntry = servGenList.srvDel(a, cmd.word());
            if (ntry == null) {
                cmd.error("invalid server");
                return;
            }
            return;
        }
        if (a.equals("client")) {
            doCmdNoClient();
            return;
        }
        cmd.badCmd();
    }

    private int parseUpRxtx() {
        String a = cmd.word();
        if (a.equals("receive")) {
            return 1;
        }
        if (a.equals("transmit")) {
            return 2;
        }
        if (a.equals("both")) {
            return 3;
        }
        if (a.equals("none")) {
            return 0;
        }
        return 3;
    }

    private void parseUpMcast(int p, boolean b) {
        cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
        if (vrf == null) {
            cmd.error("no such vrf");
            return;
        }
        ipFwd fwd;
        if (p == 4) {
            fwd = vrf.fwd4;
        } else {
            fwd = vrf.fwd6;
        }
        String a = cmd.word();
        if (a.equals("join-group")) {
            addrIP a1 = new addrIP();
            addrIP a2 = new addrIP();
            if (a1.fromString(cmd.word())) {
                cmd.error("bad group address");
                return;
            }
            if (!a1.isMulticast()) {
                cmd.error("not a multicast address");
                return;
            }
            if (a2.fromString(cmd.word())) {
                cmd.error("bad source address");
                return;
            }
            if (b) {
                fwd.mcastAddFloodIfc(a1, a2, null, -2);
            } else {
                fwd.mcastDelFloodIfc(a1, a2, null);
            }
            return;
        }
        cmd.badCmd();
    }

    private cfgVrf parseUpPbr(int p, tabPbrN ntry) {
        cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
        if (vrf == null) {
            cmd.error("no such vrf");
            return null;
        }
        ipFwd fwd;
        if (p == 4) {
            fwd = vrf.fwd4;
        } else {
            fwd = vrf.fwd6;
        }
        String s = cmd.getRemaining();
        String a = cmd.word();
        if (a.equals("reindex")) {
            int i = bits.str2num(cmd.word());
            fwd.pbrCfg.reindex(i, bits.str2num(cmd.word()));
            return null;
        }
        ntry.sequence = fwd.pbrCfg.nextseq();
        if (ntry.fromString(p, s)) {
            return null;
        }
        ntry.matcher.copyCores(fwd.pbrCfg);
        return vrf;
    }

    private cfgVrf parseUpNat(int p, tabNatCfgN ntry, boolean neg) {
        cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
        if (vrf == null) {
            cmd.error("no such vrf");
            return null;
        }
        ipFwd fwd;
        if (p == 4) {
            fwd = vrf.fwd4;
        } else {
            fwd = vrf.fwd6;
        }
        String s = cmd.getRemaining();
        String a = cmd.word();
        if (a.equals("reindex")) {
            if (neg) {
                return null;
            }
            int i = bits.str2num(cmd.word());
            fwd.natCfg.reindex(i, bits.str2num(cmd.word()));
            return null;
        }
        ntry.sequence = fwd.natCfg.nextseq();
        int res = ntry.fromString(p, s, neg);
        switch (res) {
            case 0: // entry
                if (ntry.origSrcList != null) {
                    ntry.origSrcList.copyCores(fwd.natCfg);
                }
                return vrf;
            case 1: // error
                return null;
            default:
                break;
        }
        tabNatCfgN old = fwd.natCfg.find(ntry);
        if (old == null) {
            return null;
        }
        old.fromString(p, s, neg);
        return null;
    }

    private void parseUpFlow(int ver, boolean create) {
        cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
        if (vrf == null) {
            cmd.error("no such vrf");
            return;
        }
        ipFwd fwd;
        if (ver == 4) {
            fwd = vrf.fwd4;
        } else {
            fwd = vrf.fwd6;
        }
        String a = cmd.word();
        if (a.equals("parameters")) {
            if (fwd.netflow == null) {
                cmd.error("not enabled");
                return;
            }
            fwd.netflow.session.fromString(cmd);
            return;
        }
        if (fwd.netflow != null) {
            fwd.netflow.stopTimer();
        }
        fwd.netflow = null;
        if (!create) {
            return;
        }
        if (a.equals("collect")) {
            fwd.netflow = new clntNetflow(ver);
            fwd.netflow.startTimer();
            return;
        }
        if (!a.equals("export")) {
            cmd.badCmd();
            return;
        }
        clntNetflow flw = new clntNetflow(ver);
        cfgProxy prx = cfgAll.proxyFind(cmd.word(), false);
        if (prx == null) {
            cmd.error("no such profile");
            return;
        }
        flw.proxy = prx.proxy;
        flw.trgAddr = new addrIP();
        if (flw.trgAddr.fromString(cmd.word())) {
            cmd.error("bad address");
            return;
        }
        flw.trgPort = bits.str2num(cmd.word());
        flw.startTimer();
        fwd.netflow = flw;
    }

    private void parseUpPool(int ver, boolean create) {
        String nam = cmd.word();
        addrIPv4 net4 = new addrIPv4();
        addrIPv4 inc4 = new addrIPv4();
        addrIPv6 net6 = new addrIPv6();
        addrIPv6 inc6 = new addrIPv6();
        if (ver == 4) {
            if (net4.fromString(cmd.word())) {
                return;
            }
            if (inc4.fromString(cmd.word())) {
                return;
            }
        } else {
            if (net6.fromString(cmd.word())) {
                return;
            }
            if (inc6.fromString(cmd.word())) {
                return;
            }
        }
        int num = bits.str2num(cmd.word());
        if (!create) {
            if (ver == 4) {
                cfgAll.poolDel(cfgAll.ip4pool, nam);
            } else {
                cfgAll.poolDel(cfgAll.ip6pool, nam);
            }
            return;
        }
        if (ver == 4) {
            cfgPool<addrIPv4> pool = cfgAll.poolFind(cfgAll.ip4pool, nam, true);
            pool.setup(ver, net4, inc4, num);
            pool.pool.poolName = nam;
            pool.name = nam;
        } else {
            cfgPool<addrIPv6> pool = cfgAll.poolFind(cfgAll.ip6pool, nam, true);
            pool.setup(ver, net6, inc6, num);
            pool.pool.poolName = nam;
            pool.name = nam;
        }
    }

    private void getHelpIpX(userHelp l) {
        l.add(null, false, 2, new int[]{3}, "multicast", "configure multicast parameters");
        l.add(null, false, 3, new int[]{4}, "<name:vrf>", "name of routing table");
        l.add(null, false, 4, new int[]{5}, "join-group", "unconditionally process multicast traffic");
        l.add(null, false, 5, new int[]{6}, "<addr>", "group address");
        l.add(null, false, 6, new int[]{-1}, "<addr>", "source address");
        l.add(null, false, 2, new int[]{3}, "flow", "configure netflow parameters");
        l.add(null, false, 3, new int[]{4}, "<name:vrf>", "name of routing table");
        l.add(null, false, 4, new int[]{-1}, "collect", "just collect");
        l.add(null, false, 4, new int[]{5, -1}, "parameters", "session parameters");
        l.add(null, false, 5, new int[]{6}, "timeout", "set timeout");
        l.add(null, false, 6, new int[]{5, -1}, "<num>", "timeout in ms");
        l.add(null, false, 5, new int[]{6}, "sessions", "set session limit");
        l.add(null, false, 6, new int[]{5, -1}, "<num>", "number of sessions");
        l.add(null, false, 5, new int[]{6}, "rate", "specify translation rate");
        l.add(null, false, 6, new int[]{5, -1}, "<nam:pm>", "name of policy map");
        l.add(null, false, 5, new int[]{5, -1}, "before", "log on session start");
        l.add(null, false, 5, new int[]{5, -1}, "after", "log on session stop");
        l.add(null, false, 4, new int[]{5}, "export", "collect and export");
        l.add(null, false, 5, new int[]{6}, "<name:prx>", "proxy profile");
        l.add(null, false, 6, new int[]{7}, "<addr>", "target address");
        l.add(null, false, 7, new int[]{-1}, "<num>", "port number");
        l.add(null, false, 2, new int[]{3}, "pbr", "configure policy based routing");
        l.add(null, false, 3, new int[]{4, 6}, "<name:vrf>", "name of routing table");
        l.add(null, false, 4, new int[]{5, -1}, "reindex", "reindex pbrs");
        l.add(null, false, 5, new int[]{7, -1}, "[num]", "initial number to start with");
        l.add(null, false, 7, new int[]{-1}, "[num]", "increment number");
        l.add(null, false, 4, new int[]{5}, "sequence", "sequence number");
        l.add(null, false, 5, new int[]{6}, "<num>", "number");
        l.add(null, false, 6, new int[]{7}, "<name:acl>", "access list name");
        l.add(null, false, 7, new int[]{8, -1}, "<name:vrf>", "target vrf");
        l.add(null, false, 8, new int[]{9}, "interface", "set target interface");
        l.add(null, false, 9, new int[]{8, -1}, "<name:ifc>", "interface name");
        l.add(null, false, 8, new int[]{9}, "nexthop", "set target address");
        l.add(null, false, 9, new int[]{8, -1}, "<addr>", "target address");
        l.add(null, false, 8, new int[]{9}, "nsh", "set target service");
        l.add(null, false, 9, new int[]{10}, "<num>", "service path");
        l.add(null, false, 10, new int[]{8, -1}, "<num>", "service index");
        l.add(null, false, 2, new int[]{3}, "nat", "configure network address translation");
        l.add(null, false, 3, new int[]{4, 6}, "<name:vrf>", "name of routing table");
        l.add(null, false, 4, new int[]{5, -1}, "reindex", "reindex nats");
        l.add(null, false, 5, new int[]{7, -1}, "[num]", "initial number to start with");
        l.add(null, false, 7, new int[]{-1}, "[num]", "increment number");
        l.add(null, false, 4, new int[]{5}, "sequence", "sequence number");
        l.add(null, false, 5, new int[]{6}, "<num>", "number");
        l.add(null, false, 6, new int[]{-1}, "log-translations", "turn on logging");
        l.add(null, false, 6, new int[]{7}, "filter", "specify filter acl");
        l.add(null, false, 7, new int[]{-1}, "<name:acl>", "access list name");
        l.add(null, false, 6, new int[]{7}, "timeout", "specify timeout");
        l.add(null, false, 7, new int[]{-1}, "<num>", "time in ms");
        l.add(null, false, 6, new int[]{7}, "sessions", "specify translation limit");
        l.add(null, false, 7, new int[]{-1}, "<num>", "number of translations");
        l.add(null, false, 6, new int[]{7}, "rate", "specify translation rate");
        l.add(null, false, 7, new int[]{-1}, "<nam:pm>", "name of policy map");
        l.add(null, false, 6, new int[]{7}, "randomize", "randomize source port");
        l.add(null, false, 7, new int[]{8}, "<num>", "lowest port number");
        l.add(null, false, 8, new int[]{-1}, "<num>", "highest port number");
        l.add(null, false, 6, new int[]{7}, "srclist", "source address translation");
        l.add(null, false, 7, new int[]{8}, "<name:acl>", "access list name");
        l.add(null, false, 8, new int[]{9}, "interface", "translated interface");
        l.add(null, false, 9, new int[]{-1}, "<name:ifc>", "translated interface");
        l.add(null, false, 8, new int[]{9}, "pool", "translated pool");
        l.add(null, false, 9, new int[]{-1}, "<name:pol>", "translated pool");
        l.add(null, false, 8, new int[]{-1}, "<new>", "translated address");
        l.add(null, false, 6, new int[]{7}, "source", "source address translation");
        l.add(null, false, 7, new int[]{8}, "<orig>", "original address");
        l.add(null, false, 8, new int[]{9}, "interface", "translated interface");
        l.add(null, false, 9, new int[]{-1}, "<name:ifc>", "translated interface");
        l.add(null, false, 8, new int[]{9}, "pool", "translated pool");
        l.add(null, false, 9, new int[]{-1}, "<name:pol>", "translated pool");
        l.add(null, false, 8, new int[]{-1}, "<new>", "translated address");
        l.add(null, false, 7, new int[]{8}, "interface", "original interface");
        l.add(null, false, 8, new int[]{9}, "<name:ifc>", "original interface");
        l.add(null, false, 9, new int[]{10}, "interface", "translated interface");
        l.add(null, false, 10, new int[]{-1}, "<name:ifc>", "translated interface");
        l.add(null, false, 9, new int[]{10}, "pool", "translated pool");
        l.add(null, false, 10, new int[]{-1}, "<name:pol>", "translated pool");
        l.add(null, false, 9, new int[]{-1}, "<new>", "translated address");
        l.add(null, false, 6, new int[]{7}, "target", "target address translation");
        l.add(null, false, 7, new int[]{8}, "<orig>", "original address");
        l.add(null, false, 8, new int[]{9}, "interface", "translated interface");
        l.add(null, false, 9, new int[]{-1}, "<name:ifc>", "translated interface");
        l.add(null, false, 8, new int[]{-1}, "<new>", "translated address");
        l.add(null, false, 7, new int[]{8}, "interface", "original interface");
        l.add(null, false, 8, new int[]{9}, "<name:ifc>", "original interface");
        l.add(null, false, 9, new int[]{10}, "interface", "translated interface");
        l.add(null, false, 10, new int[]{-1}, "<name:ifc>", "translated interface");
        l.add(null, false, 9, new int[]{-1}, "<new>", "translated address");
        l.add(null, false, 6, new int[]{7}, "srcport", "source address translation");
        l.add(null, false, 7, new int[]{8}, "<proto>", "protocol number");
        l.add(null, false, 8, new int[]{9}, "<orig>", "original address");
        l.add(null, false, 9, new int[]{10}, "<orig>", "original port");
        l.add(null, false, 10, new int[]{11}, "interface", "translated interface");
        l.add(null, false, 11, new int[]{12}, "<name:ifc>", "translated interface");
        l.add(null, false, 12, new int[]{-1}, "<new>", "translated port");
        l.add(null, false, 10, new int[]{11}, "<new>", "translated address");
        l.add(null, false, 11, new int[]{-1}, "<new>", "translated port");
        l.add(null, false, 8, new int[]{9}, "interface", "original interface");
        l.add(null, false, 9, new int[]{10}, "<name:ifc>", "original interface");
        l.add(null, false, 10, new int[]{11}, "<orig>", "original port");
        l.add(null, false, 11, new int[]{12}, "interface", "translated interface");
        l.add(null, false, 12, new int[]{13}, "<name:ifc>", "translated interface");
        l.add(null, false, 13, new int[]{-1}, "<new>", "translated port");
        l.add(null, false, 11, new int[]{12}, "<new>", "translated address");
        l.add(null, false, 12, new int[]{-1}, "<new>", "translated port");
        l.add(null, false, 6, new int[]{7}, "trgport", "target address translation");
        l.add(null, false, 7, new int[]{8}, "<proto>", "protocol number");
        l.add(null, false, 8, new int[]{9}, "<orig>", "original address");
        l.add(null, false, 9, new int[]{10}, "<orig>", "original port");
        l.add(null, false, 10, new int[]{11}, "interface", "translated interface");
        l.add(null, false, 11, new int[]{12}, "<name:ifc>", "translated interface");
        l.add(null, false, 12, new int[]{-1}, "<new>", "translated port");
        l.add(null, false, 10, new int[]{11}, "<new>", "translated address");
        l.add(null, false, 11, new int[]{-1}, "<new>", "translated port");
        l.add(null, false, 8, new int[]{9}, "interface", "original interface");
        l.add(null, false, 9, new int[]{10}, "<name:ifc>", "original interface");
        l.add(null, false, 10, new int[]{11}, "<orig>", "original port");
        l.add(null, false, 11, new int[]{12}, "interface", "translated interface");
        l.add(null, false, 12, new int[]{13}, "<name:ifc>", "translated interface");
        l.add(null, false, 13, new int[]{-1}, "<new>", "translated port");
        l.add(null, false, 11, new int[]{12}, "<new>", "translated address");
        l.add(null, false, 12, new int[]{-1}, "<new>", "translated port");
        l.add(null, false, 6, new int[]{7}, "srcpref", "source address translation");
        l.add(null, false, 7, new int[]{8}, "<orig>", "original address");
        l.add(null, false, 8, new int[]{9}, "<new>", "translated address");
        l.add(null, false, 9, new int[]{-1}, "<mask>", "address mask");
        l.add(null, false, 6, new int[]{7}, "trgpref", "target address translation");
        l.add(null, false, 7, new int[]{8}, "<orig>", "original address");
        l.add(null, false, 8, new int[]{9}, "<new>", "translated address");
        l.add(null, false, 9, new int[]{-1}, "<mask>", "address mask");
        l.add(null, false, 2, new int[]{3}, "route", "configure static unicast routes");
        l.add(null, false, 3, new int[]{4}, "<name:vrf>", "name of routing table");
        l.add(null, false, 4, new int[]{5}, "<network>", "destination network");
        l.add(null, false, 5, new int[]{6}, "<mask>", "destination mask");
        l.add(null, false, 6, new int[]{7, -1}, "<nexthop>", "forwarding router's address");
        l.add(null, false, 7, new int[]{7, -1}, "recurigp", "use recursive nexthop");
        l.add(null, false, 7, new int[]{7, -1}, "recurbgp", "use recursive nexthop");
        l.add(null, false, 7, new int[]{7, -1}, "recurvpn", "use recursive nexthop");
        l.add(null, false, 7, new int[]{7, -1}, "mplsimp", "use mpls implicit null");
        l.add(null, false, 7, new int[]{7, -1}, "mplsexp", "use mpls explicit null");
        l.add(null, false, 7, new int[]{8}, "distance", "set distance metric");
        l.add(null, false, 8, new int[]{7, -1}, "<dist>", "distance value");
        l.add(null, false, 7, new int[]{8}, "metric", "set metric value");
        l.add(null, false, 8, new int[]{7, -1}, "<met>", "metric value");
        l.add(null, false, 7, new int[]{8}, "tag", "set tag value");
        l.add(null, false, 8, new int[]{7, -1}, "<tag>", "tag value");
        l.add(null, false, 7, new int[]{8}, "id", "set id value");
        l.add(null, false, 8, new int[]{7, -1}, "<id>", "id value");
        l.add(null, false, 7, new int[]{8}, "tracker", "set tracker to check");
        l.add(null, false, 8, new int[]{7, -1}, "<name:trk>", "tracker name");
        l.add(null, false, 7, new int[]{8}, "interface", "force to interface");
        l.add(null, false, 8, new int[]{7, -1}, "<name:ifc>", "interface name");
        l.add(null, false, 7, new int[]{8}, "route-map", "set parameters from route map");
        l.add(null, false, 8, new int[]{7, -1}, "<name:rm>", "name of route map");
        l.add(null, false, 7, new int[]{8}, "route-policy", "set parameters from route policy");
        l.add(null, false, 8, new int[]{7, -1}, "<name:rpl>", "name of route policy");
        l.add(null, false, 2, new int[]{3}, "mroute", "configure static multicast routes");
        l.add(null, false, 3, new int[]{4}, "<name:vrf>", "name of routing table");
        l.add(null, false, 4, new int[]{5}, "<network>", "destination network");
        l.add(null, false, 5, new int[]{6}, "<mask>", "destination mask");
        l.add(null, false, 6, new int[]{7, -1}, "<nexthop>", "forwarding router's address");
        l.add(null, false, 7, new int[]{7, -1}, "recurigp", "use recursive nexthop");
        l.add(null, false, 7, new int[]{7, -1}, "recurbgp", "use recursive nexthop");
        l.add(null, false, 7, new int[]{7, -1}, "recurvpn", "use recursive nexthop");
        l.add(null, false, 7, new int[]{8}, "distance", "set distance metric");
        l.add(null, false, 8, new int[]{7, -1}, "<dist>", "distance value");
        l.add(null, false, 7, new int[]{8}, "metric", "set metric value");
        l.add(null, false, 8, new int[]{7, -1}, "<met>", "metric value");
        l.add(null, false, 7, new int[]{8}, "tag", "set tag value");
        l.add(null, false, 8, new int[]{7, -1}, "<tag>", "tag value");
        l.add(null, false, 7, new int[]{8}, "id", "set id value");
        l.add(null, false, 8, new int[]{7, -1}, "<id>", "id value");
        l.add(null, false, 7, new int[]{8}, "tracker", "set tracker to check");
        l.add(null, false, 8, new int[]{7, -1}, "<name:trk>", "tracker name");
        l.add(null, false, 7, new int[]{8}, "interface", "force to interface");
        l.add(null, false, 8, new int[]{7, -1}, "<name:ifc>", "interface name");
        l.add(null, false, 7, new int[]{8}, "route-map", "set parameters from route map");
        l.add(null, false, 8, new int[]{7, -1}, "<name:rm>", "name of route map");
        l.add(null, false, 7, new int[]{8}, "route-policy", "set parameters from route policy");
        l.add(null, false, 8, new int[]{7, -1}, "<name:rpl>", "name of route policy");
    }

    private void doCmdIpx() {
        String a = cmd.word();
        if (a.equals("route")) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                return;
            }
            tabRouteEntry<addrIpx> ntry = ipxFwd.staticParse(cmd);
            if (ntry == null) {
                return;
            }
            vrf.ipx.staticAdd(ntry);
            return;
        }
        cmd.badCmd();
    }

    private void doCmdNoIpx() {
        String a = cmd.word();
        if (a.equals("route")) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                return;
            }
            tabRouteEntry<addrIpx> ntry = ipxFwd.staticParse(cmd);
            if (ntry == null) {
                return;
            }
            vrf.ipx.staticDel(ntry);
            return;
        }
        cmd.badCmd();
    }

    private void doCmdIp4() {
        String a = cmd.word();
        if (a.equals("multicast")) {
            parseUpMcast(4, true);
            return;
        }
        if (a.equals("flow")) {
            parseUpFlow(4, true);
            return;
        }
        if (a.equals("nat")) {
            tabNatCfgN red = new tabNatCfgN();
            cfgVrf vrf = parseUpNat(4, red, false);
            if (vrf == null) {
                return;
            }
            vrf.fwd4.natCfg.add(red);
            vrf.fwd4.routerStaticChg();
            return;
        }
        if (a.equals("pbr")) {
            tabPbrN red = new tabPbrN();
            cfgVrf vrf = parseUpPbr(4, red);
            if (vrf == null) {
                return;
            }
            vrf.fwd4.pbrCfg.add(red);
            return;
        }
        if (a.equals("pool")) {
            parseUpPool(4, true);
            return;
        }
        if (a.equals("route")) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                cmd.error("no such vrf");
                return;
            }
            ipFwdRoute red = new ipFwdRoute();
            if (red.fromString(4, cmd)) {
                return;
            }
            vrf.fwd4.staticDel(true, red);
            vrf.fwd4.staticAdd(true, red);
            return;
        }
        if (a.equals("mroute")) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                cmd.error("no such vrf");
                return;
            }
            ipFwdRoute red = new ipFwdRoute();
            if (red.fromString(4, cmd)) {
                return;
            }
            vrf.fwd4.staticDel(false, red);
            vrf.fwd4.staticAdd(false, red);
            return;
        }
        cmd.badCmd();
    }

    private void doCmdNoIp4() {
        String a = cmd.word();
        if (a.equals("multicast")) {
            parseUpMcast(4, false);
            return;
        }
        if (a.equals("flow")) {
            parseUpFlow(4, false);
            return;
        }
        if (a.equals("nat")) {
            tabNatCfgN red = new tabNatCfgN();
            cfgVrf vrf = parseUpNat(4, red, true);
            if (vrf == null) {
                return;
            }
            vrf.fwd4.natCfg.del(red);
            return;
        }
        if (a.equals("pbr")) {
            tabPbrN red = new tabPbrN();
            cfgVrf vrf = parseUpPbr(4, red);
            if (vrf == null) {
                return;
            }
            vrf.fwd4.pbrCfg.del(red);
            return;
        }
        if (a.equals("pool")) {
            parseUpPool(4, false);
            return;
        }
        if (a.equals("route")) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                cmd.error("no such vrf");
                return;
            }
            ipFwdRoute red = new ipFwdRoute();
            if (red.fromString(4, cmd)) {
                return;
            }
            vrf.fwd4.staticDel(true, red);
            return;
        }
        if (a.equals("mroute")) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                cmd.error("no such vrf");
                return;
            }
            ipFwdRoute red = new ipFwdRoute();
            if (red.fromString(4, cmd)) {
                return;
            }
            vrf.fwd4.staticDel(false, red);
            return;
        }
        cmd.badCmd();
    }

    private void doCmdIp6() {
        String a = cmd.word();
        if (a.equals("multicast")) {
            parseUpMcast(6, true);
            return;
        }
        if (a.equals("flow")) {
            parseUpFlow(6, true);
            return;
        }
        if (a.equals("nat")) {
            tabNatCfgN red = new tabNatCfgN();
            cfgVrf vrf = parseUpNat(6, red, false);
            if (vrf == null) {
                return;
            }
            vrf.fwd6.natCfg.add(red);
            vrf.fwd6.routerStaticChg();
            return;
        }
        if (a.equals("pbr")) {
            tabPbrN red = new tabPbrN();
            cfgVrf vrf = parseUpPbr(6, red);
            if (vrf == null) {
                return;
            }
            vrf.fwd6.pbrCfg.add(red);
            return;
        }
        if (a.equals("pool")) {
            parseUpPool(6, true);
            return;
        }
        if (a.equals("route")) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                cmd.error("no such vrf");
                return;
            }
            ipFwdRoute red = new ipFwdRoute();
            if (red.fromString(6, cmd)) {
                return;
            }
            vrf.fwd6.staticDel(true, red);
            vrf.fwd6.staticAdd(true, red);
            return;
        }
        if (a.equals("mroute")) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                cmd.error("no such vrf");
                return;
            }
            ipFwdRoute red = new ipFwdRoute();
            if (red.fromString(6, cmd)) {
                return;
            }
            vrf.fwd6.staticDel(false, red);
            vrf.fwd6.staticAdd(false, red);
            return;
        }
        cmd.badCmd();
    }

    private void doCmdNoIp6() {
        String a = cmd.word();
        if (a.equals("multicast")) {
            parseUpMcast(6, false);
            return;
        }
        if (a.equals("flow")) {
            parseUpFlow(6, false);
            return;
        }
        if (a.equals("nat")) {
            tabNatCfgN red = new tabNatCfgN();
            cfgVrf vrf = parseUpNat(6, red, true);
            if (vrf == null) {
                return;
            }
            vrf.fwd6.natCfg.del(red);
            return;
        }
        if (a.equals("pbr")) {
            tabPbrN red = new tabPbrN();
            cfgVrf vrf = parseUpPbr(6, red);
            if (vrf == null) {
                return;
            }
            vrf.fwd6.pbrCfg.del(red);
            return;
        }
        if (a.equals("pool")) {
            parseUpPool(6, false);
            return;
        }
        if (a.equals("route")) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                cmd.error("no such vrf");
                return;
            }
            ipFwdRoute red = new ipFwdRoute();
            if (red.fromString(6, cmd)) {
                return;
            }
            vrf.fwd6.staticDel(true, red);
            return;
        }
        if (a.equals("mroute")) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                cmd.error("no such vrf");
                return;
            }
            ipFwdRoute red = new ipFwdRoute();
            if (red.fromString(6, cmd)) {
                return;
            }
            vrf.fwd6.staticDel(false, red);
            return;
        }
        cmd.badCmd();
    }

    private void doCmdNoLogging() {
        String s = cmd.word();
        if (s.equals("debug")) {
            s = cmd.getRemaining().trim();
            if (cfgAll.alwaysDebugs.indexOf(s) < 0) {
                return;
            }
            cfgAll.alwaysDebugs.remove(s);
            debugger.setByName(cmd, false);
            return;
        }
        if (s.equals("tracestop")) {
            cfgAll.tracebackStops = false;
            return;
        }
        if (s.equals("milliseconds")) {
            logger.logMillis = false;
            return;
        }
        if (s.equals("buffered")) {
            logger.logBufLev = logger.string2level("");
            logger.bufferStart(0);
            return;
        }
        if (s.equals("monitor")) {
            logger.logPipLev = logger.string2level("");
            return;
        }
        if (s.equals("file")) {
            logger.fileName(null);
            return;
        }
        if (s.equals("proxy")) {
            logger.logProxy = null;
            return;
        }
        if (s.equals("rotate")) {
            logger.fileRotate(null, 0, 0);
            return;
        }
        if (s.equals("syslog")) {
            for (int i = 0; i < logger.logSylHnd.size(); i++) {
                logger.logSylHnd.get(i).logStop();
            }
            logger.logSylHnd = new ArrayList<clntSyslog>();
            return;
        }
        if (s.equals("irc")) {
            logger.logIrcHnd.logStop();
            logger.logIrcHnd = new clntIrc(null, null);
            return;
        }
        if (s.equals("format")) {
            logger.logPosForm = logger.string2format("");
            return;
        }
        cmd.badCmd();
    }

    private void doCmdLogging() {
        String s = cmd.word();
        if (s.equals("debug")) {
            s = cmd.getRemaining().trim();
            if (cfgAll.alwaysDebugs.indexOf(s) >= 0) {
                return;
            }
            cfgAll.alwaysDebugs.add(s);
            debugger.setByName(cmd, true);
            return;
        }
        if (s.equals("tracestop")) {
            cfgAll.tracebackStops = true;
            return;
        }
        if (s.equals("milliseconds")) {
            logger.logMillis = true;
            return;
        }
        if (s.equals("buffered")) {
            logger.logBufLev = logger.string2level(cmd.word());
            logger.bufferStart(bits.str2num(cmd.word()));
            return;
        }
        if (s.equals("monitor")) {
            logger.logPipLev = logger.string2level(cmd.word());
            return;
        }
        if (s.equals("file")) {
            logger.logFilLev = logger.string2level(cmd.word());
            logger.fileName(cmd.word());
            return;
        }
        if (s.equals("proxy")) {
            cfgProxy prx = cfgAll.proxyFind(cmd.word(), false);
            if (prx == null) {
                cmd.error("no such proxy");
                return;
            }
            logger.logProxy = prx.proxy;
            return;
        }
        if (s.equals("rotate")) {
            int siz = bits.str2num(cmd.word());
            s = cmd.word();
            int tim = bits.str2num(cmd.word());
            logger.fileRotate(s, siz, tim);
            return;
        }
        if (s.equals("syslog")) {
            for (int i = 0; i < logger.logSylHnd.size(); i++) {
                logger.logSylHnd.get(i).logStop();
            }
            logger.logSylHnd = new ArrayList<clntSyslog>();
            logger.logSylLev = logger.string2level(cmd.word());
            logger.logSylFac = servSyslog.facility2num(cmd.word());
            for (;;) {
                String a = cmd.word();
                if (a.length() < 1) {
                    break;
                }
                addrIP adr = new addrIP();
                if (adr.fromString(a)) {
                    continue;
                }
                logger.logSylHnd.add(new clntSyslog(adr, logger.logSylFac));
            }
            return;
        }
        if (s.equals("irc")) {
            logger.logIrcHnd.logStop();
            logger.logIrcLev = logger.string2level(cmd.word());
            s = cmd.word();
            logger.logIrcHnd = new clntIrc(s, cmd.word());
            logger.logIrcHnd.logStart();
            return;
        }
        if (s.equals("format")) {
            logger.logPosForm = logger.string2format(cmd.word());
            return;
        }
        cmd.badCmd();
    }

    private void doCmdNoClient() {
        String s = cmd.word();
        if (s.equals("pastebin")) {
            cfgAll.pasteBin = null;
            return;
        }
        if (s.equals("capture-path")) {
            cfgAll.capturePath = null;
            return;
        }
        if (s.equals("bullying")) {
            cfgAll.clientShamer = null;
            return;
        }
        if (s.equals("whois-online")) {
            cfgAll.whoisOnline = null;
            return;
        }
        if (s.equals("whois-server")) {
            cfgAll.whoisServer = null;
            return;
        }
        if (s.equals("whois-option")) {
            cfgAll.whoisOption = null;
            return;
        }
        if (s.equals("whois-file")) {
            cfgAll.whoisFile = null;
            clntWhois.purgeLocalCache();
            return;
        }
        if (s.equals("whois-proxy")) {
            cfgAll.whoisProxy = null;
            return;
        }
        if (s.equals("end-format")) {
            cfgAll.endForm = 0;
            return;
        }
        if (s.equals("graceful-reload")) {
            cfgAll.graceReload = false;
            return;
        }
        if (s.equals("tcp-ecn")) {
            cfgAll.tcpEcn = false;
            return;
        }
        if (s.equals("tcp-keepalive")) {
            cfgAll.tcpKeepalive = false;
            return;
        }
        if (s.equals("tcp-timestamp")) {
            cfgAll.tcpTimStmp = false;
            return;
        }
        if (s.equals("prefer-ipv6")) {
            cfgAll.preferIpv6 = false;
            return;
        }
        if (s.equals("prefer-ipv4")) {
            cfgAll.preferIpv6 = true;
            return;
        }
        if (s.equals("ftp-passive")) {
            cfgAll.ftpPassive = false;
            return;
        }
        if (s.equals("ftp-active")) {
            cfgAll.ftpPassive = true;
            return;
        }
        if (s.equals("ftp-proxy")) {
            cfgAll.ftpProxy = null;
            return;
        }
        if (s.equals("pop3-proxy")) {
            cfgAll.pop3proxy = null;
            return;
        }
        if (s.equals("ssh-agent")) {
            cfgAll.sshAgent = null;
            return;
        }
        if (s.equals("proxy")) {
            cfgAll.clientProxy = null;
            return;
        }
        if (s.equals("domain-lookup")) {
            cfgAll.domainLookup = false;
            return;
        }
        if (s.equals("domain-name")) {
            cfgAll.domainName = null;
            return;
        }
        if (s.equals("name-proxy")) {
            cfgAll.nameServerProxy = null;
            return;
        }
        if (s.equals("name-server")) {
            cfgAll.nameServerAddr = new ArrayList<addrIP>();
            return;
        }
        if (s.equals("upgrade-config")) {
            cfgAll.upgradeConfig = false;
            return;
        }
        if (s.equals("upgrade-revert")) {
            cfgAll.upgradeRevert = 0;
            return;
        }
        if (s.equals("upgrade-backup")) {
            cfgAll.upgradeBackup = false;
            return;
        }
        if (s.equals("upgrade-ownkey")) {
            cfgAll.upgradeOwnKey = false;
            return;
        }
        if (s.equals("upgrade-server")) {
            cfgAll.upgradeServer = version.homeUrl;
            return;
        }
        if (s.equals("upgrade-script")) {
            cfgAll.upgradeScript = null;
            return;
        }
        if (s.equals("upgrade-pubkey")) {
            cfgAll.upgradePubKey = null;
            return;
        }
        if (s.equals("config-server")) {
            cfgAll.configServer = null;
            return;
        }
        if (s.equals("config-username")) {
            cfgAll.configUser = null;
            return;
        }
        if (s.equals("config-password")) {
            cfgAll.configPass = null;
            return;
        }
        if (s.equals("config-save")) {
            cfgAll.configAsave = false;
            return;
        }
        if (s.equals("config-archive")) {
            cfgAll.configAbackup = false;
            return;
        }
        if (s.equals("config-backup")) {
            cfgAll.configBackup = null;
            return;
        }
        if (s.equals("config-exclusive")) {
            cfgAll.configExclusive.set(0);
            return;
        }
        if (s.equals("time-server")) {
            if (cfgAll.timeServerName != null) {
                cfgAll.timeServerName.stopWork();
            }
            cfgAll.timeServerName = null;
            return;
        }
        if (s.equals("time-zone")) {
            cfgAll.timeZoneName = "Z";
            return;
        }
        if (s.equals("time-proxy")) {
            cfgAll.timeProxy = null;
            return;
        }
        if (s.equals("tftp-proxy")) {
            cfgAll.tftpProxy = null;
            return;
        }
        if (s.equals("http-agent")) {
            cfgAll.httpAgent = null;
            return;
        }
        if (s.equals("http-proxy")) {
            cfgAll.httpProxy = null;
            return;
        }
        if (s.equals("mail-proxy")) {
            cfgAll.mailProxy = null;
            return;
        }
        if (s.equals("mail-server")) {
            cfgAll.mailServerName = null;
            return;
        }
        if (s.equals("mail-username")) {
            cfgAll.mailServerUser = null;
            return;
        }
        if (s.equals("mail-password")) {
            cfgAll.mailServerPass = null;
            return;
        }
        cmd.badCmd();
    }

    private void doCmdClient() {
        String a = cmd.word();
        if (a.equals("pastebin")) {
            cfgAll.pasteBin = cmd.word();
            return;
        }
        if (a.equals("capture-path")) {
            cfgAll.capturePath = cmd.word();
            return;
        }
        if (a.equals("l2f-timer")) {
            cfgAll.l2fTimer = bits.str2num(cmd.word());
            cfgAll.l2fRetry = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("l2tp2-timer")) {
            cfgAll.l2tp2hello = bits.str2num(cmd.word());
            cfgAll.l2tp2retry = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("l2tp3-timer")) {
            cfgAll.l2tp3hello = bits.str2num(cmd.word());
            cfgAll.l2tp3retry = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("bullying")) {
            servQuote ntry = new servQuote();
            ntry.srvName = cmd.word();
            ntry = cfgAll.dmnQuote.find(ntry, false);
            if (ntry == null) {
                cmd.error("no such server");
                return;
            }
            cfgAll.clientShamer = ntry;
            return;
        }
        if (a.equals("label-range")) {
            cfgAll.labelRangeBeg = bits.str2num(cmd.word());
            cfgAll.labelRangeEnd = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("cpuhog")) {
            cfgAll.cpuhogCheck = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("ifacestall")) {
            cfgAll.ifaceStallCheck = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("redundancy-keepalive")) {
            cfgAll.redundancyKeep = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("redundancy-hold")) {
            cfgAll.redundancyHold = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("redundancy-initial")) {
            cfgAll.redundancyInit = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("redundancy-takeover")) {
            cfgAll.redundancyTake = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("whois-online")) {
            cfgAll.whoisOnline = new ArrayList<String>();
            for (;;) {
                a = cmd.word();
                if (a.length() < 1) {
                    break;
                }
                cfgAll.whoisOnline.add(a);
            }
            if (cfgAll.whoisOnline.size() > 0) {
                return;
            }
            cfgAll.whoisOnline = null;
            return;
        }
        if (a.equals("whois-server")) {
            cfgAll.whoisServer = cmd.getRemaining();
            return;
        }
        if (a.equals("whois-option")) {
            cfgAll.whoisOption = cmd.getRemaining();
            return;
        }
        if (a.equals("whois-file")) {
            cfgAll.whoisFile = cmd.getRemaining();
            clntWhois.purgeLocalCache();
            return;
        }
        if (a.equals("whois-proxy")) {
            cfgProxy prx = cfgAll.proxyFind(cmd.word(), false);
            if (prx == null) {
                cmd.error("no such proxy");
                return;
            }
            cfgAll.whoisProxy = prx.proxy;
            return;
        }
        if (a.equals("end-format")) {
            cfgAll.endForm = 0;
            for (;;) {
                a = cmd.word();
                if (a.length() < 1) {
                    break;
                }
                if (a.equals("date")) {
                    cfgAll.endForm |= 0x1;
                    continue;
                }
                if (a.equals("image")) {
                    cfgAll.endForm |= 0x2;
                    continue;
                }
                if (a.equals("chksum")) {
                    cfgAll.endForm |= 0x4;
                    continue;
                }
                if (a.equals("user")) {
                    cfgAll.endForm |= 0x8;
                    continue;
                }
                if (a.equals("none")) {
                    cfgAll.endForm = 0;
                    continue;
                }
            }
            return;
        }
        if (a.equals("graceful-reload")) {
            cfgAll.graceReload = true;
            return;
        }
        if (a.equals("prefer-ipv6")) {
            cfgAll.preferIpv6 = true;
            return;
        }
        if (a.equals("prefer-ipv4")) {
            cfgAll.preferIpv6 = false;
            return;
        }
        if (a.equals("ipv4-tos")) {
            cfgAll.ipv4sendingTOS = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("ipv4-ttl")) {
            cfgAll.ipv4sendingTTL = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("ipv6-tos")) {
            cfgAll.ipv6sendingTOS = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("ipv6-ttl")) {
            cfgAll.ipv6sendingTTL = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("ipv4-checksum")) {
            int i = parseUpRxtx();
            cfgAll.ipv4ChecksumRx = (i & 1) != 0;
            cfgAll.ipv4ChecksumTx = (i & 2) != 0;
            return;
        }
        if (a.equals("icmp4-checksum")) {
            int i = parseUpRxtx();
            cfgAll.icmp4ChecksumRx = (i & 1) != 0;
            cfgAll.icmp4ChecksumTx = (i & 2) != 0;
            return;
        }
        if (a.equals("icmp6-checksum")) {
            int i = parseUpRxtx();
            cfgAll.icmp6ChecksumRx = (i & 1) != 0;
            cfgAll.icmp6ChecksumTx = (i & 2) != 0;
            return;
        }
        if (a.equals("udp-checksum")) {
            int i = parseUpRxtx();
            cfgAll.udpChecksumRx = (i & 1) != 0;
            cfgAll.udpChecksumTx = (i & 2) != 0;
            return;
        }
        if (a.equals("udp-portrange")) {
            cfgAll.udpRangeMin = bits.str2num(cmd.word());
            cfgAll.udpRangeMax = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("tcp-timer")) {
            a = cmd.word();
            if (a.equals("work")) {
                cfgAll.tcpTimeWork = bits.str2num(cmd.word());
                return;
            }
            if (a.equals("alive")) {
                cfgAll.tcpTimeAlive = bits.str2num(cmd.word());
                return;
            }
            if (a.equals("fin")) {
                cfgAll.tcpTimeFin = bits.str2num(cmd.word());
                return;
            }
            if (a.equals("syn")) {
                cfgAll.tcpTimeSyn = bits.str2num(cmd.word());
                return;
            }
            if (a.equals("open")) {
                cfgAll.tcpTimeOpen = bits.str2num(cmd.word());
                return;
            }
            if (a.equals("close")) {
                cfgAll.tcpTimeClose = bits.str2num(cmd.word());
                return;
            }
            if (a.equals("later")) {
                cfgAll.tcpTimeLater = bits.str2num(cmd.word());
                return;
            }
            if (a.equals("now")) {
                cfgAll.tcpTimeNow = bits.str2num(cmd.word());
                return;
            }
            if (a.equals("max")) {
                cfgAll.tcpTimeMax = bits.str2num(cmd.word());
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("tcp-ecn")) {
            cfgAll.tcpEcn = true;
            return;
        }
        if (a.equals("tcp-keepalive")) {
            cfgAll.tcpKeepalive = true;
            return;
        }
        if (a.equals("tcp-timestamp")) {
            cfgAll.tcpTimStmp = true;
            return;
        }
        if (a.equals("tcp-segments")) {
            cfgAll.tcpSegmentMin = bits.str2num(cmd.word());
            cfgAll.tcpSegmentMax = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("tcp-winscale")) {
            cfgAll.tcpWinScale = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("tcp-checksum")) {
            int i = parseUpRxtx();
            cfgAll.tcpChecksumRx = (i & 1) != 0;
            cfgAll.tcpChecksumTx = (i & 2) != 0;
            return;
        }
        if (a.equals("tcp-portrange")) {
            cfgAll.tcpRangeMin = bits.str2num(cmd.word());
            cfgAll.tcpRangeMax = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("ludp-checksum")) {
            int i = parseUpRxtx();
            cfgAll.ludpChecksumRx = (i & 1) != 0;
            cfgAll.ludpChecksumTx = (i & 2) != 0;
            return;
        }
        if (a.equals("ludp-portrange")) {
            cfgAll.ludpRangeMin = bits.str2num(cmd.word());
            cfgAll.ludpRangeMax = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("dccp-checksum")) {
            int i = parseUpRxtx();
            cfgAll.dccpChecksumRx = (i & 1) != 0;
            cfgAll.dccpChecksumTx = (i & 2) != 0;
            return;
        }
        if (a.equals("dccp-portrange")) {
            cfgAll.dccpRangeMin = bits.str2num(cmd.word());
            cfgAll.dccpRangeMax = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("sctp-checksum")) {
            int i = parseUpRxtx();
            cfgAll.sctpChecksumRx = (i & 1) != 0;
            cfgAll.sctpChecksumTx = (i & 2) != 0;
            return;
        }
        if (a.equals("sctp-portrange")) {
            cfgAll.sctpRangeMin = bits.str2num(cmd.word());
            cfgAll.sctpRangeMax = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("ftp-passive")) {
            cfgAll.ftpPassive = true;
            return;
        }
        if (a.equals("ftp-active")) {
            cfgAll.ftpPassive = false;
            return;
        }
        if (a.equals("ftp-proxy")) {
            cfgProxy prx = cfgAll.proxyFind(cmd.word(), false);
            if (prx == null) {
                cmd.error("no such proxy");
                return;
            }
            cfgAll.ftpProxy = prx.proxy;
            return;
        }
        if (a.equals("pop3-proxy")) {
            cfgProxy prx = cfgAll.proxyFind(cmd.word(), false);
            if (prx == null) {
                cmd.error("no such proxy");
                return;
            }
            cfgAll.pop3proxy = prx.proxy;
            return;
        }
        if (a.equals("tls-version")) {
            cfgAll.tlsVerMin = bits.str2num(cmd.word());
            cfgAll.tlsVerMax = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("ssh-group")) {
            cfgAll.sshGrpMin = bits.str2num(cmd.word());
            cfgAll.sshGrpMax = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("ssh-agent")) {
            cfgAll.sshAgent = cmd.getRemaining();
            return;
        }
        if (a.equals("proxy")) {
            cfgAll.clientProxy = cfgAll.proxyFind(cmd.word(), false);
            if (cfgAll.clientProxy == null) {
                cmd.error("no such profile");
                return;
            }
            return;
        }
        if (a.equals("domain-lookup")) {
            cfgAll.domainLookup = true;
            return;
        }
        if (a.equals("domain-name")) {
            cfgAll.domainName = cmd.word();
            return;
        }
        if (a.equals("name-proxy")) {
            cfgAll.nameServerProxy = cfgAll.proxyFind(cmd.word(), false);
            if (cfgAll.nameServerProxy == null) {
                cmd.error("no such profile");
                return;
            }
            return;
        }
        if (a.equals("name-server")) {
            cfgAll.nameServerAddr = new ArrayList<addrIP>();
            for (;;) {
                a = cmd.word();
                if (a.length() < 1) {
                    break;
                }
                addrIP adr = new addrIP();
                if (adr.fromString(a)) {
                    continue;
                }
                cfgAll.nameServerAddr.add(adr);
            }
            return;
        }
        if (a.equals("upgrade-config")) {
            cfgAll.upgradeConfig = true;
            return;
        }
        if (a.equals("upgrade-revert")) {
            cfgAll.upgradeRevert = bits.str2num(cmd.word());
            userUpgrade.startReverter();
            return;
        }
        if (a.equals("upgrade-backup")) {
            cfgAll.upgradeBackup = true;
            return;
        }
        if (a.equals("upgrade-ownkey")) {
            cfgAll.upgradeOwnKey = true;
            return;
        }
        if (a.equals("upgrade-script")) {
            cfgScrpt ntry = cfgAll.scrptFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such script");
                return;
            }
            cfgAll.upgradeScript = ntry;
            return;
        }
        if (a.equals("upgrade-server")) {
            cfgAll.upgradeServer = cmd.word();
            return;
        }
        if (a.equals("upgrade-pubkey")) {
            cfgAll.upgradePubKey = cmd.word();
            return;
        }
        if (a.equals("config-server")) {
            cfgAll.configServer = cmd.word();
            return;
        }
        if (a.equals("config-username")) {
            cfgAll.configUser = cmd.word();
            return;
        }
        if (a.equals("config-password")) {
            cfgAll.configPass = authLocal.passwdDecode(cmd.word());
            return;
        }
        if (a.equals("config-save")) {
            cfgAll.configAsave = true;
            return;
        }
        if (a.equals("config-archive")) {
            cfgAll.configAbackup = true;
            return;
        }
        if (a.equals("config-backup")) {
            cfgAll.configBackup = cmd.getRemaining();
            return;
        }
        if (a.equals("config-exclusive")) {
            cfgAll.configExclusive.set(1);
            return;
        }
        if (a.equals("access-subnet-ipv4")) {
            cfgAll.accessSubnet4 = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("access-subnet-ipv6")) {
            cfgAll.accessSubnet6 = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("access-supernet-ipv4")) {
            cfgAll.accessSupnet4 = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("access-supernet-ipv6")) {
            cfgAll.accessSupnet6 = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("time-server")) {
            if (cfgAll.timeServerName != null) {
                cfgAll.timeServerName.stopWork();
            }
            cfgAll.timeServerName = new clntNtp(cmd.word());
            cfgAll.timeServerName.startWork();
            return;
        }
        if (a.equals("time-zone")) {
            cfgAll.timeZoneName = cmd.word();
            return;
        }
        if (a.equals("time-proxy")) {
            cfgProxy prx = cfgAll.proxyFind(cmd.word(), false);
            if (prx == null) {
                cmd.error("no such proxy");
                return;
            }
            cfgAll.timeProxy = prx.proxy;
            return;
        }
        if (a.equals("tftp-proxy")) {
            cfgProxy prx = cfgAll.proxyFind(cmd.word(), false);
            if (prx == null) {
                cmd.error("no such proxy");
                return;
            }
            cfgAll.tftpProxy = prx.proxy;
            return;
        }
        if (a.equals("http-agent")) {
            cfgAll.httpAgent = cmd.getRemaining();
            return;
        }
        if (a.equals("http-proxy")) {
            cfgProxy prx = cfgAll.proxyFind(cmd.word(), false);
            if (prx == null) {
                cmd.error("no such proxy");
                return;
            }
            cfgAll.httpProxy = prx.proxy;
            return;
        }
        if (a.equals("mail-proxy")) {
            cfgProxy prx = cfgAll.proxyFind(cmd.word(), false);
            if (prx == null) {
                cmd.error("no such proxy");
                return;
            }
            cfgAll.mailProxy = prx.proxy;
            return;
        }
        if (a.equals("mail-server")) {
            cfgAll.mailServerName = cmd.word();
            return;
        }
        if (a.equals("mail-username")) {
            cfgAll.mailServerUser = cmd.word();
            return;
        }
        if (a.equals("mail-password")) {
            cfgAll.mailServerPass = authLocal.passwdDecode(cmd.word());
            return;
        }
        cmd.badCmd();
    }

    private <T extends cryKeyGeneric> void cryptoDoKey(tabGen<cfgKey<T>> lst, T key) {
        String nam = cmd.word();
        String a = cmd.word();
        if (a.equals("zeroize")) {
            cfgAll.keyDel(lst, nam);
            return;
        }
        if (a.equals("editor")) {
            List<String> txt = new ArrayList<String>();
            userEditor e = new userEditor(new pipeScreen(cmd.pipe), txt, "key", false);
            if (e.doEdit()) {
                return;
            }
            if (key.pemReadLst(txt, false)) {
                cmd.error("error decoding");
                return;
            }
            key.keyName = nam;
            cfgKey<T> cfg = cfgAll.keyFind(lst, nam, true);
            cfg.key = key;
            return;
        }
        if (a.equals("import")) {
            a = authLocal.passwdDecode(cmd.word());
            if (a == null) {
                cmd.error("error reading");
                return;
            }
            if (key.pemReadStr(a, false)) {
                cmd.error("error decoding");
                return;
            }
            key.keyName = nam;
            cfgKey<T> cfg = cfgAll.keyFind(lst, nam, true);
            cfg.key = key;
            return;
        }
        if (a.equals("external")) {
            a = cmd.word();
            List<String> t = bits.txt2buf(a);
            if (t == null) {
                cmd.error("not found");
                return;
            }
            if (key.pemReadLst(t, false)) {
                cmd.error("error decoding");
                return;
            }
            key.keyName = nam;
            cfgKey<T> cfg = cfgAll.keyFind(lst, nam, true);
            cfg.key = key;
            cfg.filNam = a;
            return;
        }
        if (a.equals("generate")) {
            int i = bits.str2num(cmd.word());
            if (i < 1) {
                i = 512;
            }
            for (;;) {
                if (key.keyMakeSize(i)) {
                    continue;
                }
                if (key.keyVerify()) {
                    continue;
                }
                break;
            }
            key.keyName = nam;
            cfgKey<T> cfg = cfgAll.keyFind(lst, nam, true);
            cfg.key = key;
            return;
        }
    }

    private cryKeyGeneric findKey() {
        cryKeyGeneric k = null;
        String t = cmd.word();
        if (t.equals("rsa")) {
            cfgKey<cryKeyRSA> cfg = cfgAll.keyFind(cfgAll.rsakeys, cmd.word(), false);
            if (cfg != null) {
                k = cfg.key;
            }
        }
        if (t.equals("dsa")) {
            cfgKey<cryKeyDSA> cfg = cfgAll.keyFind(cfgAll.dsakeys, cmd.word(), false);
            if (cfg != null) {
                k = cfg.key;
            }
        }
        if (t.equals("ecdsa")) {
            cfgKey<cryKeyECDSA> cfg = cfgAll.keyFind(cfgAll.ecdsakeys, cmd.word(), false);
            if (cfg != null) {
                k = cfg.key;
            }
        }
        if (t.equals("mldsa")) {
            cfgKey<cryKeyMLDSA> cfg = cfgAll.keyFind(cfgAll.mldsakeys, cmd.word(), false);
            if (cfg != null) {
                k = cfg.key;
            }
        }
        if (k == null) {
            cmd.error("key not found");
        }
        return k;
    }

}
