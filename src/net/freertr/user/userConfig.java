package net.freertr.user;

import java.util.ArrayList;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrIPv4;
import net.freertr.addr.addrIPv6;
import net.freertr.addr.addrIpx;
import net.freertr.auth.authGeneric;
import net.freertr.auth.authLocal;
import net.freertr.auth.authResult;
import net.freertr.cfg.cfgAceslst;
import net.freertr.cfg.cfgAlias;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgAuther;
import net.freertr.cfg.cfgBndl;
import net.freertr.cfg.cfgBrdg;
import net.freertr.cfg.cfgCert;
import net.freertr.cfg.cfgChat;
import net.freertr.cfg.cfgCheck;
import net.freertr.cfg.cfgDial;
import net.freertr.cfg.cfgEvntmgr;
import net.freertr.cfg.cfgGeneric;
import net.freertr.cfg.cfgHrpn;
import net.freertr.cfg.cfgIconn;
import net.freertr.cfg.cfgInit;
import net.freertr.cfg.cfgKey;
import net.freertr.cfg.cfgMenu;
import net.freertr.cfg.cfgMtrack;
import net.freertr.cfg.cfgObjnet;
import net.freertr.cfg.cfgObjprt;
import net.freertr.cfg.cfgPlymp;
import net.freertr.cfg.cfgPool;
import net.freertr.cfg.cfgPrcss;
import net.freertr.cfg.cfgPrfxlst;
import net.freertr.cfg.cfgProxy;
import net.freertr.cfg.cfgRoump;
import net.freertr.cfg.cfgRouplc;
import net.freertr.cfg.cfgRtr;
import net.freertr.cfg.cfgSched;
import net.freertr.cfg.cfgScrpt;
import net.freertr.cfg.cfgSensor;
import net.freertr.cfg.cfgSessn;
import net.freertr.cfg.cfgTime;
import net.freertr.cfg.cfgTlmtry;
import net.freertr.cfg.cfgTrack;
import net.freertr.cfg.cfgTrnsltn;
import net.freertr.cfg.cfgVdc;
import net.freertr.cfg.cfgVpdn;
import net.freertr.cfg.cfgVrf;
import net.freertr.cfg.cfgXconn;
import net.freertr.clnt.clntIrc;
import net.freertr.clnt.clntNetflow;
import net.freertr.clnt.clntNtp;
import net.freertr.clnt.clntSyslog;
import net.freertr.cry.cryBase64;
import net.freertr.cry.cryCertificate;
import net.freertr.cry.cryKeyDSA;
import net.freertr.cry.cryKeyECDSA;
import net.freertr.cry.cryKeyGeneric;
import net.freertr.cry.cryKeyRSA;
import net.freertr.ip.ipFwd;
import net.freertr.ip.ipFwdRoute;
import net.freertr.ipx.ipxFwd;
import net.freertr.pipe.pipeSetting;
import net.freertr.pipe.pipeSide;
import net.freertr.serv.servAmt;
import net.freertr.serv.servBmp2mrt;
import net.freertr.serv.servBstun;
import net.freertr.serv.servCharGen;
import net.freertr.serv.servDaytime;
import net.freertr.serv.servDcp;
import net.freertr.serv.servDhcp4;
import net.freertr.serv.servDhcp6;
import net.freertr.serv.servDiscard;
import net.freertr.serv.servDns;
import net.freertr.serv.servEchoS;
import net.freertr.serv.servEtherIp;
import net.freertr.serv.servForwarder;
import net.freertr.serv.servFtp;
import net.freertr.serv.servGenList;
import net.freertr.serv.servGeneric;
import net.freertr.serv.servGeneve;
import net.freertr.serv.servGopher;
import net.freertr.serv.servGre;
import net.freertr.serv.servGtp;
import net.freertr.serv.servHoneyPot;
import net.freertr.serv.servHttp;
import net.freertr.serv.servIrc;
import net.freertr.serv.servIscsi;
import net.freertr.serv.servL2f;
import net.freertr.serv.servL2tp2;
import net.freertr.serv.servL2tp3;
import net.freertr.serv.servLoadBalancer;
import net.freertr.serv.servLpd;
import net.freertr.serv.servModem;
import net.freertr.serv.servMplsIp;
import net.freertr.serv.servMplsOam;
import net.freertr.serv.servMplsUdp;
import net.freertr.serv.servMultiplexer;
import net.freertr.serv.servNetflow;
import net.freertr.serv.servNrpe;
import net.freertr.serv.servNtp;
import net.freertr.serv.servOpenflow;
import net.freertr.serv.servPktmux;
import net.freertr.serv.servP4lang;
import net.freertr.serv.servPcep;
import net.freertr.serv.servPckOdtls;
import net.freertr.serv.servPckOtcp;
import net.freertr.serv.servPckOtxt;
import net.freertr.serv.servPckOudp;
import net.freertr.serv.servPop3;
import net.freertr.serv.servPptp;
import net.freertr.serv.servPrometheus;
import net.freertr.serv.servQuote;
import net.freertr.serv.servRadius;
import net.freertr.serv.servRfb;
import net.freertr.serv.servRpki;
import net.freertr.serv.servSdwan;
import net.freertr.serv.servSip;
import net.freertr.serv.servSmtp;
import net.freertr.serv.servSnmp;
import net.freertr.serv.servSocks;
import net.freertr.serv.servStreamingMdt;
import net.freertr.serv.servStun;
import net.freertr.serv.servSyslog;
import net.freertr.serv.servTacacs;
import net.freertr.serv.servTelnet;
import net.freertr.serv.servTftp;
import net.freertr.serv.servTime;
import net.freertr.serv.servTwamp;
import net.freertr.serv.servUdpFwd;
import net.freertr.serv.servUdptn;
import net.freertr.serv.servUni2multi;
import net.freertr.serv.servUpnpFwd;
import net.freertr.serv.servUpnpHub;
import net.freertr.serv.servVoice;
import net.freertr.serv.servVxlan;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabNatCfgN;
import net.freertr.tab.tabNshEntry;
import net.freertr.tab.tabPbrN;
import net.freertr.tab.tabRouteAttr;
import net.freertr.tab.tabRouteEntry;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.debugger;
import net.freertr.util.logger;
import net.freertr.util.verCore;

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
    
    private pipeSide pipe; // pipe to use

    private userReader reader; // reader of the user

    private cmds cmd; // currently processed string

    private modes modeV; // mode value

    private servGeneric modeDserver;

    private cfgGeneric modeDconfig;

    private enum modes {

        global, server, config

    }

    /**
     * constructs new reader for a pipeline
     *
     * @param pip pipeline to use as input
     * @param rdr reader to use as input
     */
    public userConfig(pipeSide pip, userReader rdr) {
        pipe = pip;
        reader = rdr;
        resetMode();
    }

    /**
     * reset current mode to global config
     */
    public void resetMode() {
        modeV = modes.global;
        modeDserver = null;
        modeDconfig = null;
    }

    /**
     * get help text for exec commands
     *
     * @param needEdit need editor
     * @param needShow need show
     * @param needGen need generic
     * @return helping instance
     */
    public userHelping getHelping(boolean needEdit, boolean needShow, boolean needGen) {
        userHelping l = new userHelping();
        l.expand = needExpand;
        if (needEdit) {
            l.add(null, "1 .    editor              edit the current section");
        }
        if (needShow) {
            userHelping.getCfgHelp(l);
        }
        if (needGen) {
            userHelping.getCfgGen(l);
        }
        switch (modeV) {
            case global:
                getHelpGlobal(l);
                return l;
            case server:
                modeDserver.getHelp(l);
                return l;
            case config:
                modeDconfig.getHelp(l);
                return l;
            default:
                resetMode();
                return l;
        }
    }

    /**
     * get current prompt
     *
     * @return prompt value
     */
    public String getPrompt() {
        switch (modeV) {
            case global:
                return "(cfg)";
            case server:
                return "(cfg-" + modeDserver.getPrompt() + ")";
            case config:
                return "(cfg-" + modeDconfig.getPrompt() + ")";
            default:
                resetMode();
                return "(bad)";
        }
    }

    private cfgGeneric getCurrConfiger() {
        switch (modeV) {
            case server:
                return modeDserver;
            case config:
                return modeDconfig;
            case global:
                return null;
            default:
                return null;
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
            a = "";
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
            if (modeV == modes.global) {
                return true;
            }
            resetMode();
            return false;
        }
        if (a.equals("end")) {
            resetMode();
            return true;
        }
        if (a.equals("editor")) {
            if (authorization != null) {
                authResult ntry = authorization.authUserCommand(username, cmd.getRemaining());
                if (ntry.result != authResult.authSuccessful) {
                    cmd.error("not authorized to edit this");
                    return false;
                }
            }
            cfgGeneric cur = getCurrConfiger();
            if (cur == null) {
                cmd.error("not allowed here");
                return false;
            }
            List<String> c1 = cur.getShRun(1);
            List<String> c2 = new ArrayList<String>();
            c2.addAll(c1);
            userEditor edt = new userEditor(new userScreen(cmd.pipe), c2, "current", false);
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
            shw.cfg = getCurrConfiger();
            if (authorization != null) {
                authResult ntry = authorization.authUserCommand(username, cmd.getRemaining());
                if (ntry.result != authResult.authSuccessful) {
                    cmd.error("not authorized to show that");
                    return false;
                }
            }
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
            exe.authorization = authorization;
            exe.username = username;
            a = exe.repairCommand(cmd.getRemaining());
            if (authorization != null) {
                authResult ntry = authorization.authUserCommand(username, a);
                if (ntry.result != authResult.authSuccessful) {
                    cmd.error("not authorized to do that");
                    return false;
                }
            }
            exe.executeCommand(a);
            return false;
        }
        cmd = cmd.copyBytes(true);
        switch (modeV) {
            case global:
                doGlobal();
                return false;
            case server:
                modeDserver.doCfgStr(cmd);
                return false;
            case config:
                modeDconfig.doCfgStr(cmd);
                return false;
            default:
                cmd.badCmd();
                resetMode();
                return false;
        }
    }

    /**
     * execute one command
     *
     * @return status of operation, see at one command
     */
    public boolean doCommand() {
        reader.setContext(getHelping(true, true, true), cfgAll.hostName + getPrompt() + "#");
        String s = reader.readLine(cmds.finish);
        if (s == null) {
            return true;
        }
        return executeCommand(s);
    }

    private void getHelpGlobal(userHelping l) {
        l.add(null, "1  2  hostname                       set name of system");
        l.add(null, "2  .    <str>                        name of system");
        l.add(null, "1  .  buggy                          enable dangerous things");
        l.add(null, "1  2  enable                         set enable password");
        l.add(null, "2  .    <str>                        enable password");
        l.add(null, "1  2  password-encrypt               set password encryption key");
        l.add(null, "2  .    <str>                        encryption key");
        l.add(null, "1  2  banner                         banner of system");
        l.add(null, "2  3    set                          set banner");
        l.add(null, "3  3,.    <cmd>                      first line of banner");
        l.add(null, "2  3    add                          add banner");
        l.add(null, "3  3,.    <cmd>                      add line to banner");
        l.add(null, "2  3    encoded                      set banner");
        l.add(null, "3  3,.    <cmd>                      encoded banner");
        l.add(null, "1  2  logging                        set logging parameters");
        l.add(null, "2  .    milliseconds                 millisecond logging");
        l.add(null, "2  3    proxy                        set proxy to use");
        l.add(null, "3  .      <name:prx>                 proxy profile");
        l.add(null, "2  3    buffered                     buffered logging");
        l.add(null, "3  4      debug                      debugging messages");
        l.add(null, "3  4      informational              informational messages");
        l.add(null, "3  4      warning                    warning messages");
        l.add(null, "3  4      error                      error messages");
        l.add(null, "3  4      exception                  exception messages");
        l.add(null, "4  .        <num>                    number of lines");
        l.add(null, "2  3    file                         file logging");
        l.add(null, "3  4,.    debug                      debugging messages");
        l.add(null, "3  4,.    informational              informational messages");
        l.add(null, "3  4,.    warning                    warning messages");
        l.add(null, "3  4,.    error                      error messages");
        l.add(null, "3  4,.    exception                  exception messages");
        l.add(null, "4  .        <str>                    name of file");
        l.add(null, "2  3    rotate                       log file rotation");
        l.add(null, "3  4      <num>                      maximum file size");
        l.add(null, "4  5,.      <str>                    name of second file");
        l.add(null, "5  .          <num>                  ms between backup");
        l.add(null, "2  3    syslog                       syslog logging");
        l.add(null, "3  4      debug                      debugging messages");
        l.add(null, "3  4      informational              informational messages");
        l.add(null, "3  4      warning                    warning messages");
        l.add(null, "3  4      error                      error messages");
        l.add(null, "3  4      exception                  exception messages");
        l.add(null, "4  5        kernel                   facility");
        l.add(null, "4  5        user                     facility");
        l.add(null, "4  5        mail                     facility");
        l.add(null, "4  5        system                   facility");
        l.add(null, "4  5        security1                facility");
        l.add(null, "4  5        syslogd                  facility");
        l.add(null, "4  5        lpd                      facility");
        l.add(null, "4  5        news                     facility");
        l.add(null, "4  5        uucp                     facility");
        l.add(null, "4  5        clock1                   facility");
        l.add(null, "4  5        security2                facility");
        l.add(null, "4  5        ftp                      facility");
        l.add(null, "4  5        ntp                      facility");
        l.add(null, "4  5        logaudit                 facility");
        l.add(null, "4  5        logalert                 facility");
        l.add(null, "4  5        clock2                   facility");
        l.add(null, "4  5        local0                   facility");
        l.add(null, "4  5        local1                   facility");
        l.add(null, "4  5        local2                   facility");
        l.add(null, "4  5        local3                   facility");
        l.add(null, "4  5        local4                   facility");
        l.add(null, "4  5        local5                   facility");
        l.add(null, "4  5        local6                   facility");
        l.add(null, "4  5        local7                   facility");
        l.add(null, "5  5,.        <addr>                 address of host");
        l.add(null, "2  3    irc                          irc logging");
        l.add(null, "3  4,.    debug                      debugging messages");
        l.add(null, "3  4,.    informational              informational messages");
        l.add(null, "3  4,.    warning                    warning messages");
        l.add(null, "3  4,.    error                      error messages");
        l.add(null, "3  4,.    exception                  exception messages");
        l.add(null, "4  5        <str>                    name of host");
        l.add(null, "5  .          <str>                  name of channel");
        l.add(null, "2  3    monitor                      terminal logging");
        l.add(null, "3  .      debug                      debugging messages");
        l.add(null, "3  .      informational              informational messages");
        l.add(null, "3  .      warning                    warning messages");
        l.add(null, "3  .      error                      error messages");
        l.add(null, "3  .      exception                  exception messages");
        l.add(null, "2  3    format                       logging format");
        l.add(null, "3  .      none                       not log source at all");
        l.add(null, "3  .      brief                      log only class name");
        l.add(null, "3  .      normal                     log class, file, line number");
        l.add(null, "3  .      full                       log full stack trace");
        l.add(null, "1  2  vrf                            configure a virtual routing forwarding");
        l.add(null, "2  3    definition                   create new or update existing vrf");
        l.add(null, "3  .      <name:vrf>                 name of vrf");
        l.add(null, "1  2  vdc                            configure a virtual device context");
        l.add(null, "2  3    definition                   create new or update existing vdc");
        l.add(null, "3  .      <name:vdc>                 name of vdc");
        l.add(null, "1  2  process                        configure a external process");
        l.add(null, "2  3    definition                   create new or update existing process");
        l.add(null, "3  .      <name:prc>                 name of process");
        l.add(null, "1  2  interface                      select an interface to configure");
        l.add(null, "2  .    <name:ifc>                   name of interface");
        l.add(null, "1  2  line                           select a line to configure");
        l.add(null, "2  .    <name:lin>                   name of line");
        l.add(null, "1  2  bridge                         transparent bridging parameters");
        l.add(null, "2  .    <num>                        number of bridge group");
        l.add(null, "1  2  bundle                         interface bundle parameters");
        l.add(null, "2  .    <num>                        number of bundle group");
        l.add(null, "1  2  hairpin                        interface hairpin parameters");
        l.add(null, "2  .    <num>                        number of hairpin group");
        l.add(null, "1  2  session                        stateful session parameters");
        l.add(null, "2  .    <name:ses>                   name of session");
        l.add(null, "1  2  check                          check parameters");
        l.add(null, "2  .    <name:chk>                   name of check");
        l.add(null, "1  2  sensor                         sensor parameters");
        l.add(null, "2  .    <name:sns>                   name of sensor");
        l.add(null, "1  2  dial-peer                      dial peer parameters");
        l.add(null, "2  .    <num>                        number of peer");
        l.add(null, "1  2  translation-rule               translation rule parameters");
        l.add(null, "2  .    <num>                        number of peer");
        l.add(null, "1  2  nsh                            specify service chaining");
        l.add(null, "2  3    <num>                        service path");
        l.add(null, "3  4      <num>                      service index");
        l.add(null, "4  4,.      drop                     drop packets");
        l.add(null, "4  4,.      rawpack                  output as raw packet, witout nsh header");
        l.add(null, "4  4,.      keephdr                  keep original layer2 addresses");
        l.add(null, "4  5        interface                forward as nsh");
        l.add(null, "5  6          <name:ifc>             target interface");
        l.add(null, "6  4,.          <addr>               target mac address");
        l.add(null, "4  5        route                    route normally");
        l.add(null, "5  4,.        <name:vrf>             target vrf");
        l.add(null, "4  5        switch                   switch service");
        l.add(null, "5  6          <num>                  new service path");
        l.add(null, "6  4,.          <num>                new service index");
        l.add(null, "1  2  client                         specify address of name server");
        l.add(null, "2  3    cpuhog                       specify cpuhog parameters");
        l.add(null, "3  .      <num>                      percentage");
        l.add(null, "2  3    ifacestall                   specify interface stall check");
        l.add(null, "3  .      <num>                      timeout in ms");
        l.add(null, "2  3    redundancy                   specify redundancy parameters");
        l.add(null, "3  4      <num>                      keepalive in ms");
        l.add(null, "4  5        <num>                    hold time in ms");
        l.add(null, "5  .          <num>                  init time in ms");
        l.add(null, "2  3    proxy                        specify proxy profile");
        l.add(null, "3  .      <name:prx>                 name of profile");
        l.add(null, "2  3    bullying                     specify shame quote source");
        l.add(cfgAll.dmnQuote.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    domain-name                  specify domain name");
        l.add(null, "3  .      <str>                      name of domain");
        l.add(null, "2  3    name-proxy                   specify proxy profile");
        l.add(null, "3  .      <name:prx>                 name of profile");
        l.add(null, "2  3    name-server                  specify address of name server");
        l.add(null, "3  3,.    <addr>                     address of server");
        l.add(null, "2  .    upgrade-config               automatically save configuration on upgrade");
        l.add(null, "2  3    upgrade-revert               specify time after revert if unreachable");
        l.add(null, "3  .      <num>                      time in ms");
        l.add(null, "2  .    upgrade-backup               automatically backup image on upgrade");
        l.add(null, "2  .    upgrade-ownkey               use just the configured key");
        l.add(null, "2  3    upgrade-server               specify url of upgrade server");
        l.add(null, "3  .      <str>                      url of server");
        l.add(null, "2  3    upgrade-script               specify script to run on upgrade");
        l.add(null, "3  .      <name:scr>                 name of script");
        l.add(null, "2  3    upgrade-pubkey               specify key of upgrade");
        l.add(null, "3  .      <text>                     public key");
        l.add(null, "2  3    config-server                specify url of config server");
        l.add(null, "3  .      <str>                      url of server");
        l.add(null, "2  3    config-username              specify username on config server");
        l.add(null, "3  .      <text>                     set username");
        l.add(null, "2  3    config-password              specify password on config server");
        l.add(null, "3  .      <text>                     set password");
        l.add(null, "2  3,.  config-backup                specify backup config file");
        l.add(null, "3  3,.    <text>                     file to use");
        l.add(null, "2  .    config-save                  automatically save configuration");
        l.add(null, "2  .    config-archive               automatically archive configuration");
        l.add(null, "2  .    config-exclusive             allow only one user in configuration mode");
        l.add(null, "2  .    graceful-reload              close sessions before reload");
        l.add(null, "2  3,.  end-format                   specify end format");
        l.add(null, "3  3,.    date                       append date");
        l.add(null, "3  3,.    image                      append image");
        l.add(null, "3  3,.    chksum                     append chksum");
        l.add(null, "3  3,.    none                       append nothing");
        l.add(null, "2  3    whois-server                 set whois server");
        l.add(null, "3  .      <str>                      server name");
        l.add(null, "2  3    whois-proxy                  specify proxy profile");
        l.add(null, "3  .      <name:prx>                 name of profile");
        l.add(null, "2  .    password-stars               type stars in passwords");
        l.add(null, "2  .    prefer-ipv6                  prefer ipv6 for domains");
        l.add(null, "2  .    prefer-ipv4                  prefer ipv4 for domains");
        l.add(null, "2  3    ipv4-checksum                set ipv4 checksum mode");
        l.add(null, "2  3    icmp4-checksum               set icmp4 checksum mode");
        l.add(null, "2  3    icmp6-checksum               set icmp6 checksum mode");
        l.add(null, "2  3    udp-checksum                 set udp checksum mode");
        l.add(null, "2  3    tcp-checksum                 set tcp checksum mode");
        l.add(null, "2  3    ludp-checksum                set ludp checksum mode");
        l.add(null, "2  3    dccp-checksum                set dccp checksum mode");
        l.add(null, "2  3    sctp-checksum                set sctp checksum mode");
        l.add(null, "3  .      both                       both generate and check");
        l.add(null, ".3 .      receive                    only check, not generate");
        l.add(null, ".3 .      transmit                   only generate, not check");
        l.add(null, ".3 .      none                       not generate nor check");
        l.add(null, ".2  .    tcp-timestamp               set tcp timestamping");
        l.add(null, ".2  3    tcp-segments                set tcp segment size");
        l.add(null, ".3  .      <num>                      bytes");
        l.add(null, ".2  3    tcp-winscale                set tcp window scale");
        l.add(null, ".3  .      <num>                      factor");
        l.add(null, "2  3    udp-portrange                set udp client range");
        l.add(null, "3  4      <num>                      lower bound");
        l.add(null, "4  .        <num>                    upper bound");
        l.add(null, "2  3    tcp-portrange                set tcp client range");
        l.add(null, "3  4      <num>                      lower bound");
        l.add(null, "4  .        <num>                    upper bound");
        l.add(null, "2  3    ludp-portrange               set ludp client range");
        l.add(null, "3  4      <num>                      lower bound");
        l.add(null, "4  .        <num>                    upper bound");
        l.add(null, "2  3    dccp-portrange               set dccp client range");
        l.add(null, "3  4      <num>                      lower bound");
        l.add(null, "4  .        <num>                    upper bound");
        l.add(null, "2  3    sctp-portrange               set sctp client range");
        l.add(null, "3  4      <num>                      lower bound");
        l.add(null, "4  .        <num>                    upper bound");
        l.add(null, "2  3    access-subnet-ipv4           access subnet length");
        l.add(null, "3  .      <num>                      bits");
        l.add(null, "2  3    access-subnet-ipv6           access subnet length");
        l.add(null, "3  .      <num>                      bits");
        l.add(null, "2  .    ftp-passive                  use passive mode ftp");
        l.add(null, "2  .    ftp-active                   use active mode ftp");
        l.add(null, "2  3    ftp-proxy                    specify proxy profile");
        l.add(null, "3  .      <name:prx>                 name of profile");
        l.add(null, "2  3    tls-version                  specify tls version");
        l.add(null, "3  4      <num>                      forced minimum version");
        l.add(null, "4  .        <num>                    forced maximum version");
        l.add(null, "2  3    time-server                  specify name of time server");
        l.add(null, "3  .      <str>                      name of server");
        l.add(null, "2  3    time-zone                    specify time zone");
        l.add(null, "3  .      <str>                      name of time zone");
        l.add(null, "2  3    time-proxy                   specify proxy profile");
        l.add(null, "3  .      <name:prx>                 name of profile");
        l.add(null, "2  3    tftp-proxy                   specify proxy profile");
        l.add(null, "3  .      <name:prx>                 name of profile");
        l.add(null, "2  3    http-proxy                   specify proxy profile");
        l.add(null, "3  .      <name:prx>                 name of profile");
        l.add(null, "2  3    mail-proxy                   specify proxy profile");
        l.add(null, "3  .      <name:prx>                 name of profile");
        l.add(null, "2  3    mail-server                  specify name of mail server");
        l.add(null, "3  .      <str>                      name of server");
        l.add(null, "2  3    mail-username                specify username on mail server");
        l.add(null, "3  .      <str>                      set username");
        l.add(null, "2  3    mail-password                specify password on mail server");
        l.add(null, "3  .      <text>                     set password");
        l.add(null, "1  2  ipx                            ipx config commands");
        l.add(null, "2  3    route                        configure static routes");
        l.add(null, "3  4      <name:vrf>                 name of routing table");
        l.add(null, "4  5        <network>                destination network");
        l.add(null, "5  6          <mask>                 destination mask");
        l.add(null, "6  .            <nexthop>            forwarding router's address");
        l.add(null, "1  2  ipv4                           internet protocol config commands");
        getHelpIpX(l);
        l.add(null, "2  3    pool                         configure address pool");
        l.add(null, "3  4      <name:pl4>                 name of address pool");
        l.add(null, "4  5        <first>                  first address in pool");
        l.add(null, "5  6          <inc>                  increment in address format");
        l.add(null, "6  .            <num>                number of addresses");
        l.add(null, "1  2  ipv6                           internet protocol config commands");
        getHelpIpX(l);
        l.add(null, "2  3    pool                         configure address pool");
        l.add(null, "3  4      <name:pl6>                 name of address pool");
        l.add(null, "4  5        <first>                  first address in pool");
        l.add(null, "5  6          <inc>                  increment in address format");
        l.add(null, "6  .            <num>                number of addresses");
        l.add(null, "1  2  scheduler                      configure a scheduler");
        l.add(null, "2  .    <name:sch>                   name of scheduler");
        l.add(null, "1  2  script                         configure a script");
        l.add(null, "2  .    <name:scr>                   name of script");
        l.add(null, "1  2  tracker                        configure a tracker");
        l.add(null, "2  .    <name:trk>                   name of tracker");
        l.add(null, "1  2  mtracker                       configure a mtracker");
        l.add(null, "2  .    <name:mtr>                   name of mtracker");
        l.add(null, "1  2  alias                          configure a command alias");
        l.add(null, "2  3    exec                         exec alias");
        l.add(null, "2  3    show                         show alias");
        l.add(null, "2  3    clear                        clear alias");
        l.add(null, "2  3    packet                       packet alias");
        l.add(null, "2  3    flash                        flash alias");
        l.add(null, "2  3    test                         test alias");
        l.add(null, "3  4      <str>                      name of new command");
        l.add(null, "4  .        hidden                   hide the command");
        l.add(null, "4  .        error-free               check errors before the 2nd command");
        l.add(null, "4  5        command                  specify command to execute");
        l.add(null, "5  5,.        <cmd>                  command");
        l.add(null, "4  5        cmd2nd                   specify 2nd command to execute");
        l.add(null, "5  5,.        <cmd>                  command");
        l.add(null, "4  5        cmd3rd                   specify 3rd command to execute");
        l.add(null, "5  5,.        <cmd>                  command");
        l.add(null, "4  5        cmd4th                   specify 4th command to execute");
        l.add(null, "5  5,.        <cmd>                  command");
        l.add(null, "4  5        cmd5th                   specify 5th command to execute");
        l.add(null, "5  5,.        <cmd>                  command");
        l.add(null, "4  5        description              specify help description");
        l.add(null, "5  5,.        <text>                 help text");
        l.add(null, "4  .        sticky-onlychanged       execute only if change happened");
        l.add(null, "4  .        sticky-onlysuccess       remember the choice only if succeeded");
        l.add(null, "4  .        sticky-persistent        no need to execute during bootup");
        l.add(null, "4  5        sticky-extpersist        read state from file during bootup");
        l.add(null, "5  5,.        <text>                 file name");
        l.add(null, "4  5        sticky-param             specify sticky parameter");
        l.add(null, "5  5,.        <text>                 parameter text");
        l.add(null, "4  5        default-param            specify default parameter");
        l.add(null, "5  5,.        <text>                 parameter text");
        l.add(null, "4  5        parameter                specify parameter existence");
        l.add(null, "5  .          forbidden              no parameters");
        l.add(null, "5  .          required               need parameters");
        l.add(null, "5  .          optional               parameters allowed");
        l.add(null, "4  5        param2nd                 specify 2nd parameter existence");
        l.add(null, "5  .          forbidden              no parameters");
        l.add(null, "5  .          required               need parameters");
        l.add(null, "5  .          optional               parameters allowed");
        l.add(null, "4  5        param3rd                 specify 3rd parameter existence");
        l.add(null, "5  .          forbidden              no parameters");
        l.add(null, "5  .          required               need parameters");
        l.add(null, "5  .          optional               parameters allowed");
        l.add(null, "4  5        param4th                 specify 4th parameter existence");
        l.add(null, "5  .          forbidden              no parameters");
        l.add(null, "5  .          required               need parameters");
        l.add(null, "5  .          optional               parameters allowed");
        l.add(null, "4  5        param5th                 specify 5th parameter existence");
        l.add(null, "5  .          forbidden              no parameters");
        l.add(null, "5  .          required               need parameters");
        l.add(null, "5  .          optional               parameters allowed");
        l.add(null, "1  2  router                         enable a routing protocol");
        cfgRtr.getRouterList(l, 0, "");
        l.add(null, "3  .      <num>                      process id");
        l.add(null, "1  2  chat-script                    build a chat script");
        l.add(null, "2  .    <name:cht>                   name of script");
        l.add(null, "1  2  object-group                   build an object group");
        l.add(null, "2  3    network                      network entries");
        l.add(null, "3  .      <name:ogn>                 name of object group");
        l.add(null, "2  3    port                         port entries");
        l.add(null, "3  .      <name:ogp>                 name of object group");
        l.add(null, "1  2  access-list                    build an access list");
        l.add(null, "2  .    <name:acl>                   name of access list");
        l.add(null, "1  2  telemetry                      telemetry configuration");
        l.add(null, "2  .    <name:tlm>                   name of destination");
        l.add(null, "1  2  event-manager                  build an event manager");
        l.add(null, "2  .    <name:eem>                   name of event manager");
        l.add(null, "1  2  prefix-list                    build a prefix list");
        l.add(null, "2  .    <name:pl>                    name of prefix list");
        l.add(null, "1  2  route-map                      build a route map");
        l.add(null, "2  .    <name:rm>                    name of route map");
        l.add(null, "1  2  route-policy                   build a route policy");
        l.add(null, "2  .    <name:rpl>                   name of route policy");
        l.add(null, "1  2  policy-map                     build a policy map");
        l.add(null, "2  .    <name:pm>                    name of policy map");
        l.add(null, "1  2  aaa                            authentication configuration");
        l.add(null, "2  3    userlist                     build a user list");
        l.add(null, "3  .      <name:aaa>                 name of authenticator");
        l.add(null, "2  3    radius                       set up a radius client");
        l.add(null, "3  .      <name:aaa>                 name of authenticator");
        l.add(null, "2  3    tacacs                       set up a tacacs client");
        l.add(null, "3  .      <name:aaa>                 name of authenticator");
        l.add(null, "2  3    list                         set up an aaa list");
        l.add(null, "3  .      <name:aaa>                 name of authenticator");
        l.add(null, "1  2  vpdn                           vpdn client parameters");
        l.add(null, "2  .    <name:vpd>                   name of client");
        l.add(null, "1  2  proxy-profile                  proxy profile parameters");
        l.add(null, "2  .    <name:prx>                   name of profile");
        l.add(null, "1  2  time-map                       time map parameters");
        l.add(null, "2  .    <name:tm>                    name of profile");
        l.add(null, "1  2  crypto                         cryptographic configuration");
        l.add(null, "2  3    ipsec                        ipsec profile");
        l.add(null, "3  .      <name:ips>                 name of profile");
        l.add(null, "2  3    rsakey                       rsa key");
        l.add(null, "3  4      <name:rsa>                 name of key");
        l.add(null, "4  5        import                   import key");
        l.add(null, "5  .          <text>                 base64 encoded private key");
        l.add(null, "4  5        external                 load key from file");
        l.add(null, "5  .          <text>                 file name");
        l.add(null, "4  5,.      generate                 generate new key");
        l.add(null, "5  .          [size]                 key size in bits");
        l.add(null, "4  .        zeroize                  delete the key");
        l.add(null, "4  .        editor                   import in editor");
        l.add(null, "2  3    dsakey                       dsa key");
        l.add(null, "3  4      <name:dsa>                 name of key");
        l.add(null, "4  5        import                   import key");
        l.add(null, "5  .          <text>                 base64 encoded private key");
        l.add(null, "4  5        external                 load key from file");
        l.add(null, "5  .          <text>                 file name");
        l.add(null, "4  5,.      generate                 generate new key");
        l.add(null, "5  .          [size]                 key size in bits");
        l.add(null, "4  .        zeroize                  delete the key");
        l.add(null, "4  .        editor                   import in editor");
        l.add(null, "2  3    ecdsakey                     ecdsa key");
        l.add(null, "3  4      <name:ecd>                 name of key");
        l.add(null, "4  5        import                   import key");
        l.add(null, "5  .          <text>                 base64 encoded private key");
        l.add(null, "4  5        external                 load key from file");
        l.add(null, "5  .          <text>                 file name");
        l.add(null, "4  5,.      generate                 generate new key");
        l.add(null, "5  .          [size]                 key size in bits");
        l.add(null, "4  .        zeroize                  delete the key");
        l.add(null, "4  .        editor                   import in editor");
        l.add(null, "2  3    certificate                  certificate");
        l.add(null, "3  4      <name:crt>                 name of certificate");
        l.add(null, "4  5        import                   import certificate");
        l.add(null, "5  6          rsa                    rsa key");
        l.add(null, "6  7            <name:rsa>           name of key");
        l.add(null, "7  .              <text>             base64 encoded certificate");
        l.add(null, "5  6          dsa                    dsa key");
        l.add(null, "6  7            <name:dsa>           name of key");
        l.add(null, "7  .              <text>             base64 encoded certificate");
        l.add(null, "5  6          ecdsa                  ecdsa key");
        l.add(null, "6  7            <name:ecd>           name of key");
        l.add(null, "7  .              <text>             base64 encoded certificate");
        l.add(null, "4  5        external                 load certificate from file");
        l.add(null, "5  6          rsa                    rsa key");
        l.add(null, "6  7            <name:rsa>           name of key");
        l.add(null, "7  .              <text>             file name");
        l.add(null, "5  6          dsa                    dsa key");
        l.add(null, "6  7            <name:dsa>           name of key");
        l.add(null, "7  .              <text>             file name");
        l.add(null, "5  6          ecdsa                  ecdsa key");
        l.add(null, "6  7            <name:ecd>           name of key");
        l.add(null, "7  .              <text>             file name");
        l.add(null, "4  5        generate                 generate new certificate");
        l.add(null, "5  6          rsa                    rsa key");
        l.add(null, "6  7,.          <name:rsa>           name of key");
        l.add(null, "7  8,.            <text>             identifier to give");
        l.add(null, "8  .                <num>            validity in days");
        l.add(null, "5  6          dsa                    dsa key");
        l.add(null, "6  7,.          <name:dsa>           name of key");
        l.add(null, "7  8,.            <text>             identifier to give");
        l.add(null, "8  .                <num>            validity in days");
        l.add(null, "5  6          ecdsa                  ecdsa key");
        l.add(null, "6  7,.          <name:ecd>           name of key");
        l.add(null, "7  8,.            <text>             identifier to give");
        l.add(null, "8  .                <num>            validity in days");
        l.add(null, "4  .        zeroize                  delete the certificate");
        l.add(null, "4  5        editor                   import in editor");
        l.add(null, "5  6          rsa                    rsa key");
        l.add(null, "6  .            <name:rsa>           name of key");
        l.add(null, "5  6          dsa                    dsa key");
        l.add(null, "6  .            <name:dsa>           name of key");
        l.add(null, "5  6          ecdsa                  ecdsa key");
        l.add(null, "6  .            <name:ecd>           name of key");
        l.add(null, "1  2  xconnect                       define one protocol cross connection");
        l.add(null, "2  .    <name:xcn>                   name of connection");
        l.add(null, "1  2  connect                        define one interface cross connection");
        l.add(null, "2  .    <name:cnn>                   name of connection");
        l.add(null, "1  2  menu                           define one menu");
        l.add(null, "2  .    <name:mnu>                   name of menu");
        l.add(null, "1  2  server                         create new or update existing server process");
        l.add(null, "2  3    echo                         configure an echo server");
        l.add(cfgAll.dmnEcho.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    discard                      configure a discard server");
        l.add(cfgAll.dmnDiscard.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    chargen                      configure a chargen server");
        l.add(cfgAll.dmnCharGen.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    netflow                      configure a netflow server");
        l.add(cfgAll.dmnNetflow.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    udpfwd                       configure an udp forwarder server");
        l.add(cfgAll.dmnUdpFwd.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    upnpfwd                      configure an upnp forwarder server");
        l.add(cfgAll.dmnUpnpFwd.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    upnphub                      configure an upnp hub server");
        l.add(cfgAll.dmnUpnpHub.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    openflow                     configure an openflow server");
        l.add(cfgAll.dmnOpenflow.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    pktmux                       configure a p4lang emulator server");
        l.add(cfgAll.dmnPktmux.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    p4lang                       configure a p4lang server");
        l.add(cfgAll.dmnP4lang.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    quote                        configure a quote server");
        l.add(cfgAll.dmnQuote.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    forwarder                    configure a forwarder server");
        l.add(cfgAll.dmnForwarder.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    syslog                       configure a syslog server");
        l.add(cfgAll.dmnSyslog.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    loadbalancer                 configure a loadbalancer server");
        l.add(cfgAll.dmnLoadBalancer.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    multiplexer                  configure a multiplexer server");
        l.add(cfgAll.dmnMultiplexer.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    telnet                       configure a telnet server");
        l.add(cfgAll.dmnTelnet.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    rfb                          configure a rfb server");
        l.add(cfgAll.dmnRfb.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    udptn                        configure an udptn server");
        l.add(cfgAll.dmnUdptn.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    http                         configure a http server");
        l.add(cfgAll.dmnHttp.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    dhcp4                        configure a dhcp4 server");
        l.add(cfgAll.dmnDhcp4.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    dhcp6                        configure a dhcp6 server");
        l.add(cfgAll.dmnDhcp6.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    dns                          configure a dns server");
        l.add(cfgAll.dmnDns.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    lpd                          configure a lpd server");
        l.add(cfgAll.dmnLpd.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    honeypot                     configure a honeypot server");
        l.add(cfgAll.dmnHoney.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    pop3                         configure a pop3 server");
        l.add(cfgAll.dmnPop3.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    smtp                         configure a smtp server");
        l.add(cfgAll.dmnSmtp.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    modem                        configure a modem server");
        l.add(cfgAll.dmnModem.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    voice                        configure a voice server");
        l.add(cfgAll.dmnVoice.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    sip                          configure a sip server");
        l.add(cfgAll.dmnSip.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    ftp                          configure a ftp server");
        l.add(cfgAll.dmnFtp.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    tftp                         configure a tftp server");
        l.add(cfgAll.dmnTftp.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    gopher                       configure a gopher server");
        l.add(cfgAll.dmnGopher.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    iscsi                        configure an iscsi server");
        l.add(cfgAll.dmnIscsi.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    bmp2mrt                      configure a bmp to mrt server");
        l.add(cfgAll.dmnBmp.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    irc                          configure an irc server");
        l.add(cfgAll.dmnIrc.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    dcp                          configure a dcp server");
        l.add(cfgAll.dmnDcp.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    sdwan                        configure a sdwan server");
        l.add(cfgAll.dmnSdwan.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    pcep                         configure a pcep server");
        l.add(cfgAll.dmnPcep.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    ntp                          configure a ntp server");
        l.add(cfgAll.dmnNtp.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    daytime                      configure a daytime server");
        l.add(cfgAll.dmnNtp.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    time                         configure a time server");
        l.add(cfgAll.dmnTime.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    snmp                         configure a snmp server");
        l.add(cfgAll.dmnSnmp.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    socks                        configure a socks server");
        l.add(cfgAll.dmnSocks.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    rpki                         configure a rpki server");
        l.add(cfgAll.dmnRpki.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    nrpe                         configure a nrpe server");
        l.add(cfgAll.dmnNrpe.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    prometheus                   configure a prometheus server");
        l.add(cfgAll.dmnPrometheus.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    streamingmdt                 configure a streaming telemetry server");
        l.add(cfgAll.dmnStreamingMdt.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    bstun                        configure a bstun server");
        l.add(cfgAll.dmnBStun.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    stun                         configure a stun server");
        l.add(cfgAll.dmnStun.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    vxlan                        configure a vxlan server");
        l.add(cfgAll.dmnVxlan.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    geneve                       configure a geneve server");
        l.add(cfgAll.dmnGeneve.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    l2f                          configure a l2f server");
        l.add(cfgAll.dmnL2f.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    l2tp2                        configure a l2tp v2 server");
        l.add(cfgAll.dmnL2tp2.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    l2tp3                        configure a l2tp v3 server");
        l.add(cfgAll.dmnL2tp3.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    etherip                      configure a etherip server");
        l.add(cfgAll.dmnEtherIp.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    gre                          configure a gre server");
        l.add(cfgAll.dmnGre.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    mplsip                       configure a mplsip server");
        l.add(cfgAll.dmnMplsIp.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    mplsudp                      configure a mplsudp server");
        l.add(cfgAll.dmnMplsUdp.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    mplsoam                      configure a mplsoam server");
        l.add(cfgAll.dmnMplsOam.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    twamp                        configure a twamp server");
        l.add(cfgAll.dmnTwamp.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    amt                          configure an amt server");
        l.add(cfgAll.dmnAmt.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    uni2multi                    configure an uni2multi server");
        l.add(cfgAll.dmnUni2mul.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    pckoudp                      configure a pckoudp server");
        l.add(cfgAll.dmnPckOudp.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    pckodtls                     configure a pckodtls server");
        l.add(cfgAll.dmnPckOdtls.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    pckotcp                      configure a pckotcp server");
        l.add(cfgAll.dmnPckOtcp.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    pckotxt                      configure a pckotxt server");
        l.add(cfgAll.dmnPckOtxt.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    gtp                          configure a gtp server");
        l.add(cfgAll.dmnGtp.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    pptp                         configure a pptp server");
        l.add(cfgAll.dmnPptp.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    radius                       configure a radius server");
        l.add(cfgAll.dmnRadius.listServers(), "3  .      <name:loc>                 name of server");
        l.add(null, "2  3    tacacs                       configure a tacacs server");
        l.add(cfgAll.dmnTacacs.listServers(), "3  .      <name:loc>                 name of server");
    }

    private byte[] cmdGetRem() {
        return bits.byteConcat(cmd.getRemaining().getBytes(),
                pipeSide.getEnding(pipeSide.modTyp.modeCRLF));
    }

    private void doGlobal() {
        String a = cmd.word();
        if (a.equals("hostname")) {
            cfgAll.hostName = cmd.word();
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
            verCore.release = false;
            return;
        }
        if (a.equals("banner")) {
            a = cmd.word();
            if (a.equals("set")) {
                cfgAll.banner = cmdGetRem();
                return;
            }
            if (a.equals("add")) {
                cfgAll.banner = bits.byteConcat(cfgAll.banner, cmdGetRem());
                return;
            }
            if (a.equals("encoded")) {
                cfgAll.banner = cryBase64.decodeBytes(cmd.getRemaining());
                if (cfgAll.banner == null) {
                    cfgAll.banner = new byte[0];
                }
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("vdc")) {
            a = cmd.word();
            if (a.equals("definition")) {
                a = cmd.word();
                modeDconfig = cfgAll.vdcFind(a, true);
                if (modeDconfig == null) {
                    cmd.error("bad vdc name");
                    return;
                }
                modeV = modes.config;
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("process")) {
            a = cmd.word();
            if (a.equals("definition")) {
                a = cmd.word();
                modeDconfig = cfgAll.prcFind(a, true);
                if (modeDconfig == null) {
                    cmd.error("bad process name");
                    return;
                }
                modeV = modes.config;
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("vrf")) {
            a = cmd.word();
            if (a.equals("definition")) {
                a = cmd.word();
                modeDconfig = cfgAll.vrfFind(a, true);
                if (modeDconfig == null) {
                    cmd.error("bad vrf name");
                    return;
                }
                modeV = modes.config;
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("interface")) {
            modeDconfig = cfgAll.ifcFind(cmd.word(), true);
            if (modeDconfig == null) {
                cmd.error("no such interface");
                return;
            }
            modeV = modes.config;
            return;
        }
        if (a.equals("bridge")) {
            modeDconfig = cfgAll.brdgFind(cmd.word(), true);
            if (modeDconfig == null) {
                cmd.error("invalid bridge number");
                return;
            }
            modeV = modes.config;
            return;
        }
        if (a.equals("bundle")) {
            modeDconfig = cfgAll.bndlFind(cmd.word(), true);
            if (modeDconfig == null) {
                cmd.error("invalid bundle number");
                return;
            }
            modeV = modes.config;
            return;
        }
        if (a.equals("hairpin")) {
            modeDconfig = cfgAll.hrpnFind(cmd.word(), true);
            if (modeDconfig == null) {
                cmd.error("invalid hairpin number");
                return;
            }
            modeV = modes.config;
            return;
        }
        if (a.equals("session")) {
            modeDconfig = cfgAll.sessnFind(cmd.word(), true);
            if (modeDconfig == null) {
                cmd.error("invalid session name");
                return;
            }
            modeV = modes.config;
            return;
        }
        if (a.equals("check")) {
            modeDconfig = cfgAll.checkFind(cmd.word(), true);
            if (modeDconfig == null) {
                cmd.error("invalid check name");
                return;
            }
            modeV = modes.config;
            return;
        }
        if (a.equals("sensor")) {
            modeDconfig = cfgAll.sensorFind(cmd.word(), true);
            if (modeDconfig == null) {
                cmd.error("invalid sensor name");
                return;
            }
            modeV = modes.config;
            return;
        }
        if (a.equals("dial-peer")) {
            modeDconfig = cfgAll.dialFind(cmd.word(), true);
            if (modeDconfig == null) {
                cmd.error("invalid dial peer number");
                return;
            }
            modeV = modes.config;
            return;
        }
        if (a.equals("translation-rule")) {
            modeDconfig = cfgAll.trnsltnFind(cmd.word(), true);
            if (modeDconfig == null) {
                cmd.error("invalid translation rule number");
                return;
            }
            modeV = modes.config;
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
        if (a.equals("router")) {
            tabRouteAttr.routeType o = cfgRtr.name2num(cmd.word());
            if (o == null) {
                cmd.error("invalid routing protocol");
                return;
            }
            modeDconfig = cfgAll.rtrFind(o, bits.str2num(cmd.word()), true);
            if (modeDconfig == null) {
                cmd.error("bad process number");
                return;
            }
            modeV = modes.config;
            return;
        }
        if (a.equals("scheduler")) {
            modeDconfig = cfgAll.schedFind(cmd.word(), true);
            if (modeDconfig == null) {
                return;
            }
            modeV = modes.config;
            return;
        }
        if (a.equals("script")) {
            modeDconfig = cfgAll.scrptFind(cmd.word(), true);
            if (modeDconfig == null) {
                return;
            }
            modeV = modes.config;
            return;
        }
        if (a.equals("tracker")) {
            modeDconfig = cfgAll.trackFind(cmd.word(), true);
            if (modeDconfig == null) {
                return;
            }
            modeV = modes.config;
            return;
        }
        if (a.equals("mtracker")) {
            modeDconfig = cfgAll.mtrackFind(cmd.word(), true);
            if (modeDconfig == null) {
                return;
            }
            modeV = modes.config;
            return;
        }
        if (a.equals("alias")) {
            a = cmd.word();
            cfgAlias ntry = cfgAll.aliasFind(cmd.word(), cfgAlias.string2type(a), true);
            ntry.doCfgStr(cmd);
            return;
        }
        if (a.equals("ipx")) {
            doCmdIpx();
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
        if (a.equals("logging")) {
            doCmdLogging();
            return;
        }
        if (a.equals("crypto")) {
            doCmdCrypto();
            return;
        }
        if (a.equals("chat-script")) {
            modeDconfig = cfgAll.chatFind(cmd.word(), true);
            if (modeDconfig == null) {
                cmd.error("bad script name");
                return;
            }
            modeV = modes.config;
            return;
        }
        if (a.equals("object-group")) {
            a = cmd.word();
            if (a.equals("network")) {
                modeDconfig = cfgAll.objnetFind(cmd.word(), true);
                if (modeDconfig == null) {
                    cmd.error("bad object group name");
                    return;
                }
                modeV = modes.config;
                return;
            }
            if (a.equals("port")) {
                modeDconfig = cfgAll.objprtFind(cmd.word(), true);
                if (modeDconfig == null) {
                    cmd.error("bad object group name");
                    return;
                }
                modeV = modes.config;
                return;
            }
            return;
        }
        if (a.equals("access-list")) {
            modeDconfig = cfgAll.aclsFind(cmd.word(), true);
            if (modeDconfig == null) {
                cmd.error("bad access list name");
                return;
            }
            modeV = modes.config;
            return;
        }
        if (a.equals("telemetry")) {
            modeDconfig = cfgAll.tlmdsFind(cmd.word(), true);
            if (modeDconfig == null) {
                cmd.error("bad destination name");
                return;
            }
            modeV = modes.config;
            return;
        }
        if (a.equals("event-manager")) {
            modeDconfig = cfgAll.eemFind(cmd.word(), true);
            if (modeDconfig == null) {
                cmd.error("bad event manager name");
                return;
            }
            modeV = modes.config;
            return;
        }
        if (a.equals("xconnect")) {
            modeDconfig = cfgAll.xconFind(cmd.word(), true);
            if (modeDconfig == null) {
                cmd.error("bad connect name");
                return;
            }
            modeV = modes.config;
            return;
        }
        if (a.equals("connect")) {
            modeDconfig = cfgAll.iconFind(cmd.word(), true);
            if (modeDconfig == null) {
                cmd.error("bad connect name");
                return;
            }
            modeV = modes.config;
            return;
        }
        if (a.equals("vpdn")) {
            modeDconfig = cfgAll.vpdnFind(cmd.word(), true);
            if (modeDconfig == null) {
                cmd.error("bad vpdn name");
                return;
            }
            modeV = modes.config;
            return;
        }
        if (a.equals("proxy-profile")) {
            modeDconfig = cfgAll.proxyFind(cmd.word(), true);
            if (modeDconfig == null) {
                cmd.error("bad proxy name");
                return;
            }
            modeV = modes.config;
            return;
        }
        if (a.equals("time-map")) {
            modeDconfig = cfgAll.timeFind(cmd.word(), true);
            if (modeDconfig == null) {
                cmd.error("bad time name");
                return;
            }
            modeV = modes.config;
            return;
        }
        if (a.equals("prefix-list")) {
            modeDconfig = cfgAll.prfxFind(cmd.word(), true);
            if (modeDconfig == null) {
                cmd.error("bad prefix list name");
                return;
            }
            modeV = modes.config;
            return;
        }
        if (a.equals("route-map")) {
            modeDconfig = cfgAll.rtmpFind(cmd.word(), true);
            if (modeDconfig == null) {
                cmd.error("bad route map name");
                return;
            }
            modeV = modes.config;
            return;
        }
        if (a.equals("route-policy")) {
            modeDconfig = cfgAll.rtplFind(cmd.word(), true);
            if (modeDconfig == null) {
                cmd.error("bad route policy name");
                return;
            }
            modeV = modes.config;
            return;
        }
        if (a.equals("policy-map")) {
            modeDconfig = cfgAll.plmpFind(cmd.word(), true);
            if (modeDconfig == null) {
                cmd.error("bad policy map name");
                return;
            }
            modeV = modes.config;
            return;
        }
        if (a.equals("line")) {
            modeDconfig = cfgAll.linFind(cmd.word());
            if (modeDconfig == null) {
                cmd.error("invalid line name");
                return;
            }
            modeV = modes.config;
            return;
        }
        if (a.equals("menu")) {
            modeDconfig = cfgAll.menuFind(cmd.word(), true);
            if (modeDconfig == null) {
                cmd.error("invalid menu name");
                return;
            }
            modeV = modes.config;
            return;
        }
        if (a.equals("aaa")) {
            cfgAuther.methodType i = cfgAuther.string2auther(cmd.word());
            if (i == null) {
                cmd.badCmd();
                return;
            }
            modeDconfig = cfgAll.autherFind(cmd.word(), i);
            if (modeDconfig == null) {
                cmd.error("invalid authenticator name");
                return;
            }
            modeV = modes.config;
            return;
        }
        if (a.equals("server")) {
            a = cmd.word();
            if (a.equals("echo")) {
                daemonMake(new servEchoS(), cfgAll.dmnEcho);
                return;
            }
            if (a.equals("discard")) {
                daemonMake(new servDiscard(), cfgAll.dmnDiscard);
                return;
            }
            if (a.equals("quote")) {
                daemonMake(new servQuote(), cfgAll.dmnQuote);
                return;
            }
            if (a.equals("chargen")) {
                daemonMake(new servCharGen(), cfgAll.dmnCharGen);
                return;
            }
            if (a.equals("netflow")) {
                daemonMake(new servNetflow(), cfgAll.dmnNetflow);
                return;
            }
            if (a.equals("udpfwd")) {
                daemonMake(new servUdpFwd(), cfgAll.dmnUdpFwd);
                return;
            }
            if (a.equals("upnpfwd")) {
                daemonMake(new servUpnpFwd(), cfgAll.dmnUpnpFwd);
                return;
            }
            if (a.equals("upnphub")) {
                daemonMake(new servUpnpHub(), cfgAll.dmnUpnpHub);
                return;
            }
            if (a.equals("openflow")) {
                daemonMake(new servOpenflow(), cfgAll.dmnOpenflow);
                return;
            }
            if (a.equals("pktmux")) {
                daemonMake(new servPktmux(), cfgAll.dmnPktmux);
                return;
            }
            if (a.equals("p4lang")) {
                daemonMake(new servP4lang(), cfgAll.dmnP4lang);
                return;
            }
            if (a.equals("forwarder")) {
                daemonMake(new servForwarder(), cfgAll.dmnForwarder);
                return;
            }
            if (a.equals("syslog")) {
                daemonMake(new servSyslog(), cfgAll.dmnSyslog);
                return;
            }
            if (a.equals("loadbalancer")) {
                daemonMake(new servLoadBalancer(), cfgAll.dmnLoadBalancer);
                return;
            }
            if (a.equals("multiplexer")) {
                daemonMake(new servMultiplexer(), cfgAll.dmnMultiplexer);
                return;
            }
            if (a.equals("telnet")) {
                daemonMake(new servTelnet(), cfgAll.dmnTelnet);
                return;
            }
            if (a.equals("rfb")) {
                daemonMake(new servRfb(), cfgAll.dmnRfb);
                return;
            }
            if (a.equals("udptn")) {
                daemonMake(new servUdptn(), cfgAll.dmnUdptn);
                return;
            }
            if (a.equals("http")) {
                daemonMake(new servHttp(), cfgAll.dmnHttp);
                return;
            }
            if (a.equals("lpd")) {
                daemonMake(new servLpd(), cfgAll.dmnLpd);
                return;
            }
            if (a.equals("honeypot")) {
                daemonMake(new servHoneyPot(), cfgAll.dmnHoney);
                return;
            }
            if (a.equals("dhcp4")) {
                daemonMake(new servDhcp4(), cfgAll.dmnDhcp4);
                return;
            }
            if (a.equals("dhcp6")) {
                daemonMake(new servDhcp6(), cfgAll.dmnDhcp6);
                return;
            }
            if (a.equals("dns")) {
                daemonMake(new servDns(), cfgAll.dmnDns);
                return;
            }
            if (a.equals("pop3")) {
                daemonMake(new servPop3(), cfgAll.dmnPop3);
                return;
            }
            if (a.equals("smtp")) {
                daemonMake(new servSmtp(), cfgAll.dmnSmtp);
                return;
            }
            if (a.equals("modem")) {
                daemonMake(new servModem(), cfgAll.dmnModem);
                return;
            }
            if (a.equals("voice")) {
                daemonMake(new servVoice(), cfgAll.dmnVoice);
                return;
            }
            if (a.equals("sip")) {
                daemonMake(new servSip(), cfgAll.dmnSip);
                return;
            }
            if (a.equals("socks")) {
                daemonMake(new servSocks(), cfgAll.dmnSocks);
                return;
            }
            if (a.equals("ftp")) {
                daemonMake(new servFtp(), cfgAll.dmnFtp);
                return;
            }
            if (a.equals("tftp")) {
                daemonMake(new servTftp(), cfgAll.dmnTftp);
                return;
            }
            if (a.equals("gopher")) {
                daemonMake(new servGopher(), cfgAll.dmnGopher);
                return;
            }
            if (a.equals("iscsi")) {
                daemonMake(new servIscsi(), cfgAll.dmnIscsi);
                return;
            }
            if (a.equals("bmp2mrt")) {
                daemonMake(new servBmp2mrt(), cfgAll.dmnBmp);
                return;
            }
            if (a.equals("irc")) {
                daemonMake(new servIrc(), cfgAll.dmnIrc);
                return;
            }
            if (a.equals("dcp")) {
                daemonMake(new servDcp(), cfgAll.dmnDcp);
                return;
            }
            if (a.equals("sdwan")) {
                daemonMake(new servSdwan(), cfgAll.dmnSdwan);
                return;
            }
            if (a.equals("pcep")) {
                daemonMake(new servPcep(), cfgAll.dmnPcep);
                return;
            }
            if (a.equals("ntp")) {
                daemonMake(new servNtp(), cfgAll.dmnNtp);
                return;
            }
            if (a.equals("daytime")) {
                daemonMake(new servDaytime(), cfgAll.dmnDaytime);
                return;
            }
            if (a.equals("time")) {
                daemonMake(new servTime(), cfgAll.dmnTime);
                return;
            }
            if (a.equals("snmp")) {
                daemonMake(new servSnmp(), cfgAll.dmnSnmp);
                return;
            }
            if (a.equals("rpki")) {
                daemonMake(new servRpki(), cfgAll.dmnRpki);
                return;
            }
            if (a.equals("nrpe")) {
                daemonMake(new servNrpe(), cfgAll.dmnNrpe);
                return;
            }
            if (a.equals("prometheus")) {
                daemonMake(new servPrometheus(), cfgAll.dmnPrometheus);
                return;
            }
            if (a.equals("streamingmdt")) {
                daemonMake(new servStreamingMdt(), cfgAll.dmnStreamingMdt);
                return;
            }
            if (a.equals("bstun")) {
                daemonMake(new servBstun(), cfgAll.dmnBStun);
                return;
            }
            if (a.equals("stun")) {
                daemonMake(new servStun(), cfgAll.dmnStun);
                return;
            }
            if (a.equals("pckoudp")) {
                daemonMake(new servPckOudp(), cfgAll.dmnPckOudp);
                return;
            }
            if (a.equals("pckodtls")) {
                daemonMake(new servPckOdtls(), cfgAll.dmnPckOdtls);
                return;
            }
            if (a.equals("pckotcp")) {
                daemonMake(new servPckOtcp(), cfgAll.dmnPckOtcp);
                return;
            }
            if (a.equals("pckotxt")) {
                daemonMake(new servPckOtxt(), cfgAll.dmnPckOtxt);
                return;
            }
            if (a.equals("vxlan")) {
                daemonMake(new servVxlan(), cfgAll.dmnVxlan);
                return;
            }
            if (a.equals("geneve")) {
                daemonMake(new servGeneve(), cfgAll.dmnGeneve);
                return;
            }
            if (a.equals("l2f")) {
                daemonMake(new servL2f(), cfgAll.dmnL2f);
                return;
            }
            if (a.equals("l2tp2")) {
                daemonMake(new servL2tp2(), cfgAll.dmnL2tp2);
                return;
            }
            if (a.equals("l2tp3")) {
                daemonMake(new servL2tp3(), cfgAll.dmnL2tp3);
                return;
            }
            if (a.equals("etherip")) {
                daemonMake(new servEtherIp(), cfgAll.dmnEtherIp);
                return;
            }
            if (a.equals("gre")) {
                daemonMake(new servGre(), cfgAll.dmnGre);
                return;
            }
            if (a.equals("mplsip")) {
                daemonMake(new servMplsIp(), cfgAll.dmnMplsIp);
                return;
            }
            if (a.equals("mplsudp")) {
                daemonMake(new servMplsUdp(), cfgAll.dmnMplsUdp);
                return;
            }
            if (a.equals("mplsoam")) {
                daemonMake(new servMplsOam(), cfgAll.dmnMplsOam);
                return;
            }
            if (a.equals("twamp")) {
                daemonMake(new servTwamp(), cfgAll.dmnTwamp);
                return;
            }
            if (a.equals("amt")) {
                daemonMake(new servAmt(), cfgAll.dmnAmt);
                return;
            }
            if (a.equals("uni2multi")) {
                daemonMake(new servUni2multi(), cfgAll.dmnUni2mul);
                return;
            }
            if (a.equals("gtp")) {
                daemonMake(new servGtp(), cfgAll.dmnGtp);
                return;
            }
            if (a.equals("pptp")) {
                daemonMake(new servPptp(), cfgAll.dmnPptp);
                return;
            }
            if (a.equals("radius")) {
                daemonMake(new servRadius(), cfgAll.dmnRadius);
                return;
            }
            if (a.equals("tacacs")) {
                daemonMake(new servTacacs(), cfgAll.dmnTacacs);
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("client")) {
            a = cmd.word();
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
            if (a.equals("cpuhog")) {
                cfgAll.cpuhogCheck = bits.str2num(cmd.word());
                return;
            }
            if (a.equals("ifacestall")) {
                cfgAll.ifaceStallCheck = bits.str2num(cmd.word());
                return;
            }
            if (a.equals("redundancy")) {
                cfgAll.redundancyKeep = bits.str2num(cmd.word());
                cfgAll.redundancyHold = bits.str2num(cmd.word());
                cfgAll.redundancyInit = bits.str2num(cmd.word());
                return;
            }
            if (a.equals("whois-server")) {
                cfgAll.whoisServer = cmd.getRemaining();
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
            if (a.equals("password-stars")) {
                cfgAll.passwdStars = true;
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
            if (a.equals("tcp-timestamp")) {
                cfgAll.tcpTimStmp = true;
                return;
            }
            if (a.equals("tcp-segments")) {
                cfgAll.tcpMaxSegment = bits.str2num(cmd.word());
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
            if (a.equals("tls-version")) {
                cfgAll.tlsVerMin = bits.str2num(cmd.word());
                cfgAll.tlsVerMax = bits.str2num(cmd.word());
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
                cfgAll.configExclusive = 1;
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
            return;
        }
        if (!a.equals("no")) {
            cmd.badCmd();
            return;
        }
        a = cmd.word();
        if (a.equals("enable")) {
            cfgAll.enaPass = null;
            return;
        }
        if (a.equals("password-encrypt")) {
            cfgAll.passEnc = null;
            return;
        }
        if (a.equals("buggy")) {
            verCore.release = true;
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
            cfgAll.banner = new byte[0];
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
            cmd = new cmds("c", "no " + cmd.getRemaining());
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
            doCmdNoCrypto();
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
        if (a.equals("connect")) {
            cfgIconn prf = cfgAll.iconDel(cmd.word());
            if (prf == null) {
                cmd.error("no such connect");
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
            cfgMenu prf = cfgAll.menuDel(cmd.word());
            if (prf == null) {
                cmd.error("no such menu");
                return;
            }
            return;
        }
        if (a.equals("server")) {
            a = cmd.word();
            if (a.equals("echo")) {
                daemonErase(new servEchoS(), cfgAll.dmnEcho);
                return;
            }
            if (a.equals("discard")) {
                daemonErase(new servDiscard(), cfgAll.dmnDiscard);
                return;
            }
            if (a.equals("quote")) {
                daemonErase(new servQuote(), cfgAll.dmnQuote);
                return;
            }
            if (a.equals("chargen")) {
                daemonErase(new servCharGen(), cfgAll.dmnCharGen);
                return;
            }
            if (a.equals("netflow")) {
                daemonErase(new servNetflow(), cfgAll.dmnNetflow);
                return;
            }
            if (a.equals("udpfwd")) {
                daemonErase(new servUdpFwd(), cfgAll.dmnUdpFwd);
                return;
            }
            if (a.equals("upnpfwd")) {
                daemonErase(new servUpnpFwd(), cfgAll.dmnUpnpFwd);
                return;
            }
            if (a.equals("upnphub")) {
                daemonErase(new servUpnpHub(), cfgAll.dmnUpnpHub);
                return;
            }
            if (a.equals("openflow")) {
                daemonErase(new servOpenflow(), cfgAll.dmnOpenflow);
                return;
            }
            if (a.equals("pktmux")) {
                daemonErase(new servPktmux(), cfgAll.dmnPktmux);
                return;
            }
            if (a.equals("p4lang")) {
                daemonErase(new servP4lang(), cfgAll.dmnP4lang);
                return;
            }
            if (a.equals("forwarder")) {
                daemonErase(new servForwarder(), cfgAll.dmnForwarder);
                return;
            }
            if (a.equals("syslog")) {
                daemonErase(new servSyslog(), cfgAll.dmnSyslog);
                return;
            }
            if (a.equals("loadbalancer")) {
                daemonErase(new servLoadBalancer(), cfgAll.dmnLoadBalancer);
                return;
            }
            if (a.equals("multiplexer")) {
                daemonErase(new servMultiplexer(), cfgAll.dmnMultiplexer);
                return;
            }
            if (a.equals("telnet")) {
                daemonErase(new servTelnet(), cfgAll.dmnTelnet);
                return;
            }
            if (a.equals("rfb")) {
                daemonErase(new servRfb(), cfgAll.dmnRfb);
                return;
            }
            if (a.equals("udptn")) {
                daemonErase(new servUdptn(), cfgAll.dmnUdptn);
                return;
            }
            if (a.equals("http")) {
                daemonErase(new servHttp(), cfgAll.dmnHttp);
                return;
            }
            if (a.equals("lpd")) {
                daemonErase(new servLpd(), cfgAll.dmnLpd);
                return;
            }
            if (a.equals("honeypot")) {
                daemonErase(new servHoneyPot(), cfgAll.dmnHoney);
                return;
            }
            if (a.equals("dhcp4")) {
                daemonErase(new servDhcp4(), cfgAll.dmnDhcp4);
                return;
            }
            if (a.equals("dhcp6")) {
                daemonErase(new servDhcp6(), cfgAll.dmnDhcp6);
                return;
            }
            if (a.equals("dns")) {
                daemonErase(new servDns(), cfgAll.dmnDns);
                return;
            }
            if (a.equals("pop3")) {
                daemonErase(new servPop3(), cfgAll.dmnPop3);
                return;
            }
            if (a.equals("smtp")) {
                daemonErase(new servSmtp(), cfgAll.dmnSmtp);
                return;
            }
            if (a.equals("modem")) {
                daemonErase(new servModem(), cfgAll.dmnModem);
                return;
            }
            if (a.equals("voice")) {
                daemonErase(new servVoice(), cfgAll.dmnVoice);
                return;
            }
            if (a.equals("sip")) {
                daemonErase(new servSip(), cfgAll.dmnSip);
                return;
            }
            if (a.equals("socks")) {
                daemonErase(new servSocks(), cfgAll.dmnSocks);
                return;
            }
            if (a.equals("ftp")) {
                daemonErase(new servFtp(), cfgAll.dmnFtp);
                return;
            }
            if (a.equals("tftp")) {
                daemonErase(new servTftp(), cfgAll.dmnTftp);
                return;
            }
            if (a.equals("gopher")) {
                daemonErase(new servGopher(), cfgAll.dmnGopher);
                return;
            }
            if (a.equals("iscsi")) {
                daemonErase(new servIscsi(), cfgAll.dmnIscsi);
                return;
            }
            if (a.equals("bmp2mrt")) {
                daemonErase(new servBmp2mrt(), cfgAll.dmnBmp);
                return;
            }
            if (a.equals("irc")) {
                daemonErase(new servIrc(), cfgAll.dmnIrc);
                return;
            }
            if (a.equals("dcp")) {
                daemonErase(new servDcp(), cfgAll.dmnDcp);
                return;
            }
            if (a.equals("sdwan")) {
                daemonErase(new servSdwan(), cfgAll.dmnSdwan);
                return;
            }
            if (a.equals("pcep")) {
                daemonErase(new servPcep(), cfgAll.dmnPcep);
                return;
            }
            if (a.equals("ntp")) {
                daemonErase(new servNtp(), cfgAll.dmnNtp);
                return;
            }
            if (a.equals("daytime")) {
                daemonErase(new servDaytime(), cfgAll.dmnDaytime);
                return;
            }
            if (a.equals("time")) {
                daemonErase(new servTime(), cfgAll.dmnTime);
                return;
            }
            if (a.equals("snmp")) {
                daemonErase(new servSnmp(), cfgAll.dmnSnmp);
                return;
            }
            if (a.equals("rpki")) {
                daemonErase(new servRpki(), cfgAll.dmnRpki);
                return;
            }
            if (a.equals("nrpe")) {
                daemonErase(new servNrpe(), cfgAll.dmnNrpe);
                return;
            }
            if (a.equals("prometheus")) {
                daemonErase(new servPrometheus(), cfgAll.dmnPrometheus);
                return;
            }
            if (a.equals("streamingmdt")) {
                daemonErase(new servStreamingMdt(), cfgAll.dmnStreamingMdt);
                return;
            }
            if (a.equals("bstun")) {
                daemonErase(new servBstun(), cfgAll.dmnBStun);
                return;
            }
            if (a.equals("stun")) {
                daemonErase(new servStun(), cfgAll.dmnStun);
                return;
            }
            if (a.equals("pckoudp")) {
                daemonErase(new servPckOudp(), cfgAll.dmnPckOudp);
                return;
            }
            if (a.equals("pckodtls")) {
                daemonErase(new servPckOdtls(), cfgAll.dmnPckOdtls);
                return;
            }
            if (a.equals("pckotcp")) {
                daemonErase(new servPckOtcp(), cfgAll.dmnPckOtcp);
                return;
            }
            if (a.equals("pckotxt")) {
                daemonErase(new servPckOtxt(), cfgAll.dmnPckOtxt);
                return;
            }
            if (a.equals("vxlan")) {
                daemonErase(new servVxlan(), cfgAll.dmnVxlan);
                return;
            }
            if (a.equals("geneve")) {
                daemonErase(new servGeneve(), cfgAll.dmnGeneve);
                return;
            }
            if (a.equals("l2f")) {
                daemonErase(new servL2f(), cfgAll.dmnL2f);
                return;
            }
            if (a.equals("l2tp2")) {
                daemonErase(new servL2tp2(), cfgAll.dmnL2tp2);
                return;
            }
            if (a.equals("l2tp3")) {
                daemonErase(new servL2tp3(), cfgAll.dmnL2tp3);
                return;
            }
            if (a.equals("etherip")) {
                daemonErase(new servEtherIp(), cfgAll.dmnEtherIp);
                return;
            }
            if (a.equals("gre")) {
                daemonErase(new servGre(), cfgAll.dmnGre);
                return;
            }
            if (a.equals("mplsip")) {
                daemonErase(new servMplsIp(), cfgAll.dmnMplsIp);
                return;
            }
            if (a.equals("mplsudp")) {
                daemonErase(new servMplsUdp(), cfgAll.dmnMplsUdp);
                return;
            }
            if (a.equals("mplsoam")) {
                daemonErase(new servMplsOam(), cfgAll.dmnMplsOam);
                return;
            }
            if (a.equals("twamp")) {
                daemonErase(new servTwamp(), cfgAll.dmnTwamp);
                return;
            }
            if (a.equals("amt")) {
                daemonErase(new servAmt(), cfgAll.dmnAmt);
                return;
            }
            if (a.equals("uni2multi")) {
                daemonErase(new servUni2multi(), cfgAll.dmnUni2mul);
                return;
            }
            if (a.equals("gtp")) {
                daemonErase(new servGtp(), cfgAll.dmnGtp);
                return;
            }
            if (a.equals("pptp")) {
                daemonErase(new servPptp(), cfgAll.dmnPptp);
                return;
            }
            if (a.equals("radius")) {
                daemonErase(new servRadius(), cfgAll.dmnRadius);
                return;
            }
            if (a.equals("tacacs")) {
                daemonErase(new servTacacs(), cfgAll.dmnTacacs);
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("client")) {
            a = cmd.word();
            if (a.equals("bullying")) {
                cfgAll.clientShamer = null;
                return;
            }
            if (a.equals("whois-server")) {
                cfgAll.whoisServer = null;
                return;
            }
            if (a.equals("whois-proxy")) {
                cfgAll.whoisProxy = null;
                return;
            }
            if (a.equals("end-format")) {
                cfgAll.endForm = 0;
                return;
            }
            if (a.equals("graceful-reload")) {
                cfgAll.graceReload = false;
                return;
            }
            if (a.equals("password-stars")) {
                cfgAll.passwdStars = false;
                return;
            }
            if (a.equals("tcp-timestamp")) {
                cfgAll.tcpTimStmp = false;
                return;
            }
            if (a.equals("prefer-ipv6")) {
                cfgAll.preferIpv6 = false;
                return;
            }
            if (a.equals("prefer-ipv4")) {
                cfgAll.preferIpv6 = true;
                return;
            }
            if (a.equals("ftp-passive")) {
                cfgAll.ftpPassive = false;
                return;
            }
            if (a.equals("ftp-active")) {
                cfgAll.ftpPassive = true;
                return;
            }
            if (a.equals("ftp-proxy")) {
                cfgAll.ftpProxy = null;
                return;
            }
            if (a.equals("proxy")) {
                cfgAll.clientProxy = null;
                return;
            }
            if (a.equals("domain-name")) {
                cfgAll.domainName = null;
                return;
            }
            if (a.equals("name-proxy")) {
                cfgAll.nameServerProxy = null;
                return;
            }
            if (a.equals("name-server")) {
                cfgAll.nameServerAddr = new ArrayList<addrIP>();
                return;
            }
            if (a.equals("upgrade-config")) {
                cfgAll.upgradeConfig = false;
                return;
            }
            if (a.equals("upgrade-revert")) {
                cfgAll.upgradeRevert = 0;
                return;
            }
            if (a.equals("upgrade-backup")) {
                cfgAll.upgradeBackup = false;
                return;
            }
            if (a.equals("upgrade-ownkey")) {
                cfgAll.upgradeOwnKey = false;
                return;
            }
            if (a.equals("upgrade-server")) {
                cfgAll.upgradeServer = verCore.homeUrl;
                return;
            }
            if (a.equals("upgrade-script")) {
                cfgAll.upgradeScript = null;
                return;
            }
            if (a.equals("upgrade-pubkey")) {
                cfgAll.upgradePubKey = null;
                return;
            }
            if (a.equals("config-server")) {
                cfgAll.configServer = null;
                return;
            }
            if (a.equals("config-username")) {
                cfgAll.configUser = null;
                return;
            }
            if (a.equals("config-password")) {
                cfgAll.configPass = null;
                return;
            }
            if (a.equals("config-save")) {
                cfgAll.configAsave = false;
                return;
            }
            if (a.equals("config-archive")) {
                cfgAll.configAbackup = false;
                return;
            }
            if (a.equals("config-backup")) {
                cfgAll.configBackup = null;
                return;
            }
            if (a.equals("config-exclusive")) {
                cfgAll.configExclusive = 0;
                return;
            }
            if (a.equals("time-server")) {
                if (cfgAll.timeServerName != null) {
                    cfgAll.timeServerName.stopWork();
                }
                cfgAll.timeServerName = null;
                return;
            }
            if (a.equals("time-zone")) {
                cfgAll.timeZoneName = "Z";
                return;
            }
            if (a.equals("time-proxy")) {
                cfgAll.timeProxy = null;
                return;
            }
            if (a.equals("tftp-proxy")) {
                cfgAll.tftpProxy = null;
                return;
            }
            if (a.equals("http-proxy")) {
                cfgAll.httpProxy = null;
                return;
            }
            if (a.equals("mail-proxy")) {
                cfgAll.mailProxy = null;
                return;
            }
            if (a.equals("mail-server")) {
                cfgAll.mailServerName = null;
                return;
            }
            if (a.equals("mail-username")) {
                cfgAll.mailServerUser = null;
                return;
            }
            if (a.equals("mail-password")) {
                cfgAll.mailServerPass = null;
                return;
            }
            cmd.badCmd();
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
        ntry.sequence = fwd.pbrCfg.nextseq();
        if (ntry.fromString(p, cmd.getRemaining())) {
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
        ntry.sequence = fwd.natCfg.nextseq();
        int res = ntry.fromString(s, neg);
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
        old.fromString(s, neg);
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

    private void getHelpIpX(userHelping l) {
        l.add(null, "2  3    multicast                          configure multicast parameters");
        l.add(null, "3  4      <name:vrf>                       name of routing table");
        l.add(null, "4  5        join-group                     unconditionally process multicast traffic");
        l.add(null, "5  6          <addr>                       group address");
        l.add(null, "6  .            <addr>                     source address");
        l.add(null, "2  3    flow                               configure netflow parameters");
        l.add(null, "3  4      <name:vrf>                       name of routing table");
        l.add(null, "4  .        collect                        just collect");
        l.add(null, "4  5        export                         collect and export");
        l.add(null, "5  6          <name:prx>                   proxy profile");
        l.add(null, "6  7            <addr>                     target address");
        l.add(null, "7  .              <num>                    port number");
        l.add(null, "2  3    pbr                                configure policy based routing");
        l.add(null, "3  4,6    <name:vrf>                       name of routing table");
        l.add(null, "4  5        sequence                       sequence number");
        l.add(null, "5  6          <num>                        number");
        l.add(null, "6  7            <name:acl>                 access list name");
        l.add(null, "7  8,.            <name:vrf>               target vrf");
        l.add(null, "8  9                interface              set target interface");
        l.add(null, "9  8,.                <name:ifc>           interface name");
        l.add(null, "8  9                nexthop                set target address");
        l.add(null, "9  8,.                <addr>               target address");
        l.add(null, "8  9                nsh                    set target service");
        l.add(null, "9  10                 <num>                service path");
        l.add(null, "10 8,.                  <num>              service index");
        l.add(null, "2  3    nat                                configure network address translation");
        l.add(null, "3  4,6    <name:vrf>                       name of routing table");
        l.add(null, "4  5        sequence                       sequence number");
        l.add(null, "5  6          <num>                        number");
        l.add(null, "6  .            log-translations           turn on logging");
        l.add(null, "6  7            timeout                    specify timeout");
        l.add(null, "7  .              <num>                    time in ms");
        l.add(null, "6  7            sessions                   specify translation limit");
        l.add(null, "7  .              <num>                    number of translations");
        l.add(null, "6  7            rate                       specify translation rate");
        l.add(null, "7  .              <nam:pm>                 name of policy map");
        l.add(null, "6  7            randomize                  randomize source port");
        l.add(null, "7  8              <num>                    lowest port number");
        l.add(null, "8  .                <num>                  highest port number");
        l.add(null, "6  7            srclist                    source address translation");
        l.add(null, "7  8              <name:acl>               access list name");
        l.add(null, "8  9                interface              translated interface");
        l.add(null, "9  .                  <name:ifc>           translated interface");
        l.add(null, "8  .                <new>                  translated address");
        l.add(null, "6  7            source                     source address translation");
        l.add(null, "7  8              <orig>                   original address");
        l.add(null, "8  9                interface              translated interface");
        l.add(null, "9  .                  <name:ifc>           translated interface");
        l.add(null, "8  .                <new>                  translated address");
        l.add(null, "7  8              interface                original interface");
        l.add(null, "8  9                <name:ifc>             original interface");
        l.add(null, "9  10                 interface            translated interface");
        l.add(null, "10 .                    <name:ifc>         translated interface");
        l.add(null, "9  .                  <new>                translated address");
        l.add(null, "6  7            target                     target address translation");
        l.add(null, "7  8              <orig>                   original address");
        l.add(null, "8  9                interface              translated interface");
        l.add(null, "9  .                  <name:ifc>           translated interface");
        l.add(null, "8  .                <new>                  translated address");
        l.add(null, "7  8              interface                original interface");
        l.add(null, "8  9                <name:ifc>             original interface");
        l.add(null, "9  10                 interface            translated interface");
        l.add(null, "10 .                    <name:ifc>         translated interface");
        l.add(null, "9  .                  <new>                translated address");
        l.add(null, "6  7            srcport                    source address translation");
        l.add(null, "7  8              <proto>                  protocol number");
        l.add(null, "8  9                <orig>                 original address");
        l.add(null, "9  10                 <orig>               original port");
        l.add(null, "10 11                   interface          translated interface");
        l.add(null, "11 12                     <name:ifc>       translated interface");
        l.add(null, "12 .                        <new>          translated port");
        l.add(null, "10 11                   <new>              translated address");
        l.add(null, "11 .                      <new>            translated port");
        l.add(null, "8  9                interface              original interface");
        l.add(null, "9  10                 <name:ifc>           original interface");
        l.add(null, "10 11                   <orig>             original port");
        l.add(null, "11 12                     interface        translated interface");
        l.add(null, "12 13                       <name:ifc>     translated interface");
        l.add(null, "13 .                          <new>        translated port");
        l.add(null, "11 12                     <new>            translated address");
        l.add(null, "12 .                        <new>          translated port");
        l.add(null, "6  7            trgport                    target address translation");
        l.add(null, "7  8              <proto>                  protocol number");
        l.add(null, "8  9                <orig>                 original address");
        l.add(null, "9  10                 <orig>               original port");
        l.add(null, "10 11                   interface          translated interface");
        l.add(null, "11 12                     <name:ifc>       translated interface");
        l.add(null, "12 .                        <new>          translated port");
        l.add(null, "10 11                   <new>              translated address");
        l.add(null, "11 .                      <new>            translated port");
        l.add(null, "8  9                interface              original interface");
        l.add(null, "9  10                 <name:ifc>           original interface");
        l.add(null, "10 11                   <orig>             original port");
        l.add(null, "11 12                     interface        translated interface");
        l.add(null, "12 13                       <name:ifc>     translated interface");
        l.add(null, "13 .                          <new>        translated port");
        l.add(null, "11 12                     <new>            translated address");
        l.add(null, "12 .                        <new>          translated port");
        l.add(null, "6  7            srcpref                    source address translation");
        l.add(null, "7  8              <orig>                   original address");
        l.add(null, "8  9                <new>                  translated address");
        l.add(null, "9  .                  <mask>               address mask");
        l.add(null, "6  7            trgpref                    target address translation");
        l.add(null, "7  8              <orig>                   original address");
        l.add(null, "8  9                <new>                  translated address");
        l.add(null, "9  .                  <mask>               address mask");
        l.add(null, "2  3    route                              configure static unicast routes");
        l.add(null, "3  4        <name:vrf>                     name of routing table");
        l.add(null, "4  5          <network>                    destination network");
        l.add(null, "5  6            <mask>                     destination mask");
        l.add(null, "6  7,.            <nexthop>                forwarding router's address");
        l.add(null, "7  7,.              recurigp               use recursive nexthop");
        l.add(null, "7  7,.              recurbgp               use recursive nexthop");
        l.add(null, "7  7,.              recurvpn               use recursive nexthop");
        l.add(null, "7  7,.              mplsimp                use mpls implicit null");
        l.add(null, "7  7,.              mplsexp                use mpls explicit null");
        l.add(null, "7  8                distance               set distance metric");
        l.add(null, "8  7,.                <dist>               distance value");
        l.add(null, "7  8                metric                 set metric value");
        l.add(null, "8  7,.                <met>                metric value");
        l.add(null, "7  8                tag                    set tag value");
        l.add(null, "8  7,.                <tag>                tag value");
        l.add(null, "7  8                id                     set id value");
        l.add(null, "8  7,.                <id>                 id value");
        l.add(null, "7  8                tracker                set tracker to check");
        l.add(null, "8  7,.                <name:trk>           tracker name");
        l.add(null, "7  8                interface              force to interface");
        l.add(null, "8  7,.                <name:ifc>           interface name");
        l.add(null, "7  8                route-map              set parameters from route map");
        l.add(null, "8  7,.                <name:rm>            name of route map");
        l.add(null, "7  8                route-policy           set parameters from route policy");
        l.add(null, "8  7,.                <name:rpl>           name of route policy");
        l.add(null, "2  3    mroute                             configure static multicast routes");
        l.add(null, "3  4        <name:vrf>                     name of routing table");
        l.add(null, "4  5          <network>                    destination network");
        l.add(null, "5  6            <mask>                     destination mask");
        l.add(null, "6  7,.            <nexthop>                forwarding router's address");
        l.add(null, "7  7,.              recurigp               use recursive nexthop");
        l.add(null, "7  7,.              recurbgp               use recursive nexthop");
        l.add(null, "7  7,.              recurvpn               use recursive nexthop");
        l.add(null, "7  8                distance               set distance metric");
        l.add(null, "8  7,.                <dist>               distance value");
        l.add(null, "7  8                metric                 set metric value");
        l.add(null, "8  7,.                <met>                metric value");
        l.add(null, "7  8                tag                    set tag value");
        l.add(null, "8  7,.                <tag>                tag value");
        l.add(null, "7  8                id                     set id value");
        l.add(null, "8  7,.                <id>                 id value");
        l.add(null, "7  8                tracker                set tracker to check");
        l.add(null, "8  7,.                <name:trk>           tracker name");
        l.add(null, "7  8                interface              force to interface");
        l.add(null, "8  7,.                <name:ifc>           interface name");
        l.add(null, "7  8                route-map              set parameters from route map");
        l.add(null, "8  7,.                <name:rm>            name of route map");
        l.add(null, "7  8                route-policy           set parameters from route policy");
        l.add(null, "8  7,.                <name:rpl>           name of route policy");
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

    private <T extends cryKeyGeneric> void cryptoDoKey(tabGen<cfgKey<T>> lst, T key) {
        String nam = cmd.word();
        String a = cmd.word();
        if (a.equals("zeroize")) {
            cfgAll.keyDel(lst, nam);
            return;
        }
        if (a.equals("editor")) {
            List<String> txt = new ArrayList<String>();
            userEditor e = new userEditor(new userScreen(cmd.pipe), txt, "key", false);
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
                if (key.keyMake(i)) {
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
        if (k == null) {
            cmd.error("key not found");
        }
        return k;
    }

    private void doCmdCrypto() {
        String a = cmd.word();
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
                userEditor e = new userEditor(new userScreen(cmd.pipe), txt, "cert", false);
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
        if (a.equals("ipsec")) {
            modeDconfig = cfgAll.ipsecFind(cmd.word(), true);
            if (modeDconfig == null) {
                cmd.error("bad profile name");
                return;
            }
            modeV = modes.config;
            return;
        }
        cmd.badCmd();
    }

    private void doCmdNoCrypto() {
        String a = cmd.word();
        if (a.equals("ipsec")) {
            cfgAll.ipsecDel(cmd.word());
            return;
        }
        cmd.badCmd();
    }

    private <T extends servGeneric> void daemonMake(T srv, servGenList<T> lst) {
        srv.srvRename(cmd.word());
        modeDserver = lst.find(srv, true);
        if (modeDserver == null) {
            cmd.error("invalid server name");
            return;
        }
        modeV = modes.server;
        return;
    }

    private <T extends servGeneric> void daemonErase(T srv, servGenList<T> lst) {
        srv.srvRename(cmd.word());
        if (lst.del(srv) == null) {
            cmd.error("invalid server name");
            return;
        }
    }

}
