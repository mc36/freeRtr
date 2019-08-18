package user;

import cfg.cfgMenu;
import auth.authGeneric;
import auth.authResult;
import addr.addrIP;
import addr.addrIPv4;
import addr.addrIPv6;
import addr.addrIpx;
import auth.authLocal;
import cfg.cfgAceslst;
import cfg.cfgAlias;
import cfg.cfgAll;
import cfg.cfgAuther;
import cfg.cfgBndl;
import cfg.cfgBrdg;
import cfg.cfgCert;
import cfg.cfgChat;
import cfg.cfgDial;
import cfg.cfgEvntmgr;
import cfg.cfgTime;
import cfg.cfgXconn;
import cfg.cfgGeneric;
import cfg.cfgHrpn;
import cfg.cfgIconn;
import cfg.cfgKey;
import cfg.cfgMtrack;
import cfg.cfgObjnet;
import cfg.cfgObjprt;
import cfg.cfgPlymp;
import cfg.cfgPool;
import cfg.cfgPrfxlst;
import cfg.cfgProxy;
import cfg.cfgRoump;
import cfg.cfgRouplc;
import cfg.cfgRtr;
import cfg.cfgSched;
import cfg.cfgScrpt;
import cfg.cfgTrack;
import cfg.cfgVdc;
import cfg.cfgPrcss;
import cfg.cfgTrnsltn;
import cfg.cfgVpdn;
import cfg.cfgVrf;
import clnt.clntIrc;
import clnt.clntNetflow;
import clnt.clntNtp;
import clnt.clntSyslog;
import cry.cryBase64;
import cry.cryCertificate;
import cry.cryKeyDSA;
import cry.cryKeyECDSA;
import cry.cryKeyGeneric;
import cry.cryKeyRSA;
import ip.ipFwd;
import ip.ipFwdRoute;
import ipx.ipxFwd;
import java.util.ArrayList;
import pipe.pipeSide;
import serv.servBmp2mrt;
import serv.servBstun;
import serv.servCharGen;
import serv.servDaytime;
import serv.servDcp;
import serv.servDhcp4;
import serv.servDhcp6;
import serv.servDiscard;
import serv.servDns;
import serv.servEchoS;
import serv.servEtherIp;
import serv.servForwarder;
import serv.servFtp;
import serv.servGenList;
import serv.servGeneric;
import serv.servGopher;
import serv.servGtp;
import serv.servHttp;
import serv.servIrc;
import serv.servIscsi;
import serv.servL2f;
import serv.servL2tp2;
import serv.servL2tp3;
import serv.servLoadBalancer;
import serv.servLpd;
import serv.servNtp;
import serv.servPckOdtls;
import serv.servPckOtcp;
import serv.servPckOtxt;
import serv.servPckOudp;
import serv.servPop3;
import serv.servPptp;
import serv.servQuote;
import serv.servRadius;
import serv.servRfb;
import serv.servModem;
import serv.servSip;
import serv.servSmtp;
import serv.servSnmp;
import serv.servSocks;
import serv.servStun;
import serv.servSyslog;
import serv.servTacacs;
import serv.servTelnet;
import serv.servTftp;
import serv.servTime;
import serv.servUdptn;
import serv.servHoneyPot;
import serv.servRpki;
import serv.servVxlan;
import serv.servGeneve;
import serv.servGre;
import serv.servMplsIp;
import serv.servMplsUdp;
import serv.servNetflow;
import serv.servNrpe;
import serv.servOpenflow;
import serv.servP4lang;
import serv.servUpnpFwd;
import serv.servUpnpHub;
import serv.servVoice;
import tab.tabGen;
import tab.tabNatCfgN;
import tab.tabNshNtry;
import tab.tabPbrN;
import tab.tabRouteEntry;
import util.bits;
import util.cmds;
import util.debugger;
import util.logger;
import util.verCore;

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
     * @return helping instance
     */
    public userHelping getHelping() {
        switch (modeV) {
            case global:
                return getHelpGlobal();
            case server:
                return modeDserver.getHelp();
            case config:
                return modeDconfig.getHelp();
            default:
                resetMode();
                return userHelping.getGenCfg();
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

    /**
     * execute one command
     *
     * @param a the command to execute
     * @return status of operation null - continue processing "x" - exec command
     * requested "" - exec prompt requested
     */
    public String executeCommand(String a) {
        if (a == null) {
            a = "";
        }
        cmd = new cmds("config", a);
        cmd.pipe = pipe;
        if (debugger.userConfigEvnt) {
            logger.debug(cmd.getOriginal());
        }
        a = cmd.word();
        if (a.length() < 1) {
            return null;
        }
        if (a.equals("exit")) {
            if (modeV == modes.global) {
                return "";
            }
            resetMode();
            return null;
        }
        if (a.equals("end")) {
            resetMode();
            return "";
        }
        if (a.equals("do")) {
            return cmd.getRemaining();
        }
        cmd = cmd.copyBytes(true);
        switch (modeV) {
            case global:
                doGlobal();
                return null;
            case server:
                modeDserver.doCfgStr(cmd);
                return null;
            case config:
                modeDconfig.doCfgStr(cmd);
                return null;
            default:
                cmd.badCmd();
                resetMode();
                return null;
        }
    }

    /**
     * execute some commands
     *
     * @return status of operation, see at one command
     */
    public String doCommands() {
        for (;;) {
            reader.setContext(getHelping(), cfgAll.hostName + getPrompt() + "#");
            String s = reader.readLine(reader.deactive);
            if (s == null) {
                return "";
            }
            if (authorization != null) {
                authResult ntry = authorization.authUserCommand(username, s);
                if (ntry.result != authResult.authSuccessful) {
                    pipe.linePut("% not authorized to do that");
                    continue;
                }
            }
            s = executeCommand(s);
            if (s != null) {
                return s;
            }
        }
    }

    private userHelping getHelpGlobal() {
        userHelping l = userHelping.getGenCfg();
        l.add("1  2  hostname                       set name of system");
        l.add("2  .    <name>                       name of system");
        l.add("1  .  buggy                          enable dangerous things");
        l.add("1  2  enable                         set enable password");
        l.add("2  .    <name>                       enable password");
        l.add("1  2  password-encrypt               set password encryption key");
        l.add("2  .    <name>                       enable password");
        l.add("1  2  banner                         banner of system");
        l.add("2  3    set                          set banner");
        l.add("3  3,.    <cmd>                      first line of banner");
        l.add("2  3    add                          add banner");
        l.add("3  3,.    <cmd>                      add line to banner");
        l.add("2  3    encoded                      set banner");
        l.add("3  3,.    <cmd>                      encoded banner");
        l.add("1  2  logging                        set logging parameters");
        l.add("2  3    buffered                     buffered logging");
        l.add("3  4      debug                      debugging messages");
        l.add("3  4      informational              informational messages");
        l.add("3  4      warning                    warning messages");
        l.add("3  4      error                      error messages");
        l.add("3  4      exception                  exception messages");
        l.add("4  .        <num>                    number of lines");
        l.add("2  3    file                         file logging");
        l.add("3  4,.    debug                      debugging messages");
        l.add("3  4,.    informational              informational messages");
        l.add("3  4,.    warning                    warning messages");
        l.add("3  4,.    error                      error messages");
        l.add("3  4,.    exception                  exception messages");
        l.add("4  .        <name>                   name of file");
        l.add("2  3    rotate                       log file rotation");
        l.add("3  4      <num>                      maximum file size");
        l.add("4  .        <name>                   name of second file");
        l.add("2  3    syslog                       syslog logging");
        l.add("3  4      debug                      debugging messages");
        l.add("3  4      informational              informational messages");
        l.add("3  4      warning                    warning messages");
        l.add("3  4      error                      error messages");
        l.add("3  4      exception                  exception messages");
        l.add("4  5        kernel                   facility");
        l.add("4  5        user                     facility");
        l.add("4  5        mail                     facility");
        l.add("4  5        system                   facility");
        l.add("4  5        security1                facility");
        l.add("4  5        syslogd                  facility");
        l.add("4  5        lpd                      facility");
        l.add("4  5        news                     facility");
        l.add("4  5        uucp                     facility");
        l.add("4  5        clock1                   facility");
        l.add("4  5        security2                facility");
        l.add("4  5        ftp                      facility");
        l.add("4  5        ntp                      facility");
        l.add("4  5        logaudit                 facility");
        l.add("4  5        logalert                 facility");
        l.add("4  5        clock2                   facility");
        l.add("4  5        local0                   facility");
        l.add("4  5        local1                   facility");
        l.add("4  5        local2                   facility");
        l.add("4  5        local3                   facility");
        l.add("4  5        local4                   facility");
        l.add("4  5        local5                   facility");
        l.add("4  5        local6                   facility");
        l.add("4  5        local7                   facility");
        l.add("5  5,.        <addr>                 address of host");
        l.add("2  3    irc                          irc logging");
        l.add("3  4,.    debug                      debugging messages");
        l.add("3  4,.    informational              informational messages");
        l.add("3  4,.    warning                    warning messages");
        l.add("3  4,.    error                      error messages");
        l.add("3  4,.    exception                  exception messages");
        l.add("4  5        <name>                   name of host");
        l.add("5  .          <name>                 name of channel");
        l.add("2  3    monitor                      terminal logging");
        l.add("3  .      debug                      debugging messages");
        l.add("3  .      informational              informational messages");
        l.add("3  .      warning                    warning messages");
        l.add("3  .      error                      error messages");
        l.add("3  .      exception                  exception messages");
        l.add("2  3    format                       logging format");
        l.add("3  .      none                       not log source at all");
        l.add("3  .      brief                      log only class name");
        l.add("3  .      normal                     log class, file, line number");
        l.add("3  .      full                       log full stack trace");
        l.add("1  2  vrf                            configure a virtual routing forwarding");
        l.add("2  3    definition                   create new or update existing vrf");
        l.add("3  .      <name>                     name of vrf");
        l.add("1  2  vdc                            configure a virtual device context");
        l.add("2  3    definition                   create new or update existing vdc");
        l.add("3  .      <name>                     name of vdc");
        l.add("1  2  process                        configure a external process");
        l.add("2  3    definition                   create new or update existing process");
        l.add("3  .      <name>                     name of process");
        l.add("1  2  interface                      select an interface to configure");
        l.add("2  .    <name>                       name of interface");
        l.add("1  2  line                           select a line to configure");
        l.add("2  .    <name>                       name of line");
        l.add("1  2  bridge                         transparent bridging parameters");
        l.add("2  .    <num>                        number of bridge group");
        l.add("1  2  bundle                         interface bundle parameters");
        l.add("2  .    <num>                        number of bundle group");
        l.add("1  2  hairpin                        interface hairpin parameters");
        l.add("2  .    <num>                        number of hairpin group");
        l.add("1  2  dial-peer                      dial peer parameters");
        l.add("2  .    <num>                        number of peer");
        l.add("1  2  translation-rule               translation rule parameters");
        l.add("2  .    <num>                        number of peer");
        l.add("1  2  nsh                            specify service chaining");
        l.add("2  3    <num>                        service path");
        l.add("3  4      <num>                      service index");
        l.add("4  4,.      drop                     drop packets");
        l.add("4  4,.      rawpack                  output as raw packet, witout nsh header");
        l.add("4  4,.      keephdr                  keep original layer2 addresses");
        l.add("4  5        interface                forward as nsh");
        l.add("5  6          <name>                 target interface");
        l.add("6  4,.          <addr>               target mac address");
        l.add("4  5        route                    route normally");
        l.add("5  4,.        <name>                 target vrf");
        l.add("4  5        switch                   switch service");
        l.add("5  6          <num>                  new service path");
        l.add("6  4,.          <num>                new service index");
        l.add("1  2  client                         specify address of name server");
        l.add("2  3    proxy                        specify proxy profile");
        l.add("3  .      <name>                     name of profile");
        l.add("2  3    name-proxy                   specify proxy profile");
        l.add("3  .      <name>                     name of profile");
        l.add("2  3    name-server                  specify address of name server");
        l.add("3  3,.    <addr>                     address of server");
        l.add("2  .    upgrade-config               automatically save configuration on upgrade");
        l.add("2  .    upgrade-backup               automatically backup image on upgrade");
        l.add("2  .    upgrade-ownkey               use just the configured key");
        l.add("2  3    upgrade-server               specify url of upgrade server");
        l.add("3  .      <name>                     url of server");
        l.add("2  3    upgrade-pubkey               specify key of upgrade");
        l.add("3  .      <text>                     public key");
        l.add("2  3    config-server                specify url of config server");
        l.add("3  .      <name>                     url of server");
        l.add("2  3    config-username              specify username on config server");
        l.add("3  .      <text>                     set username");
        l.add("2  3    config-password              specify password on config server");
        l.add("3  .      <text>                     set password");
        l.add("2  3    config-backup                specify backup config file");
        l.add("3  .      <text>                     file to use");
        l.add("2  .    config-save                  automatically save configuration");
        l.add("2  .    config-archive               automatically archive configuration");
        l.add("2  .    config-exclusive             allow only one user in configuration mode");
        l.add("2  .    graceful-reload              close sessions before reload");
        l.add("2  3    whois-server                 set whois server");
        l.add("3  .      <nam>                      server name");
        l.add("2  .    password-stars               type stars in passwords");
        l.add("2  .    prefer-ipv6                  prefer ipv6 for domains");
        l.add("2  .    prefer-ipv4                  prefer ipv4 for domains");
        l.add("2  3    ipv4-checksum                set ipv4 checksum mode");
        l.add("2  3    icmp4-checksum               set icmp4 checksum mode");
        l.add("2  3    icmp6-checksum               set icmp6 checksum mode");
        l.add("2  3    udp-checksum                 set udp checksum mode");
        l.add("2  3    tcp-checksum                 set tcp checksum mode");
        l.add("2  3    ludp-checksum                set ludp checksum mode");
        l.add("2  3    dccp-checksum                set dccp checksum mode");
        l.add("2  3    sctp-checksum                set sctp checksum mode");
        l.add("3  .      both                       both generate and check");
        l.add(".3 .      receive                    only check, not generate");
        l.add(".3 .      transmit                   only generate, not check");
        l.add(".3 .      none                       not generate nor check");
        l.add(".2 .    punish-pmtud                 send back mtu exceeded if needed");
        l.add("2  3    unreach-interval             rate limit icmp generation");
        l.add("3  .      <num>                      millisecs between them");
        l.add("2  .    ftp-passive                  use passive mode ftp");
        l.add("2  .    ftp-active                   use active mode ftp");
        l.add("2  3    time-server                  specify name of time server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    time-zone                    specify time zone");
        l.add("3  .      <name>                     name of time zone");
        l.add("2  3    mail-server                  specify name of mail server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    mail-username                specify username on mail server");
        l.add("3  .      <name>                     set username");
        l.add("2  3    mail-password                specify password on mail server");
        l.add("3  .      <text>                     set password");
        l.add("1  2  ipx                            ipx config commands");
        l.add("2  3    route                        configure static routes");
        l.add("3  4      <vrf>                      name of routing table");
        l.add("4  5        <network>                destination network");
        l.add("5  6          <mask>                 destination mask");
        l.add("6  .            <nexthop>            forwarding router's address");
        l.add("1  2  ipv4                           internet protocol config commands");
        getHelpIpX(l);
        l.add("1  2  ipv6                           internet protocol config commands");
        getHelpIpX(l);
        l.add("1  2  scheduler                      configure a scheduler");
        l.add("2  .    <name>                       name of scheduler");
        l.add("1  2  script                         configure a script");
        l.add("2  .    <name>                       name of scheduler");
        l.add("1  2  tracker                        configure a tracker");
        l.add("2  .    <name>                       name of tracker");
        l.add("1  2  mtracker                       configure a mtracker");
        l.add("2  .    <name>                       name of mtracker");
        l.add("1  2  alias                          configure a command alias");
        l.add("2  3    exec                         exec alias");
        l.add("2  3    show                         show alias");
        l.add("2  3    clear                        clear alias");
        l.add("2  3    test                         test alias");
        l.add("3  4      <name>                     name of new command");
        l.add("4  5        command                  specify command to execute");
        l.add("5  5,.        <cmd>                  command");
        l.add("4  5        description              specify help description");
        l.add("5  5,.        <text>                 help text");
        l.add("4  5        parameter                specify parameter existence");
        l.add("5  .          forbidden              no parameters");
        l.add("5  .          required               need parameters");
        l.add("5  .          optional               parameters allowed");
        l.add("1  2  router                         enable a routing protocol");
        cfgRtr.getRouterList(l, 0, "");
        l.add("3  .      <num>                      process id");
        l.add("1  2  chat-script                    build a chat script");
        l.add("2  .    <name>                       name of script");
        l.add("1  2  object-group                   build an object group");
        l.add("2  3    network                      network entries");
        l.add("3  .      <name>                     name of object group");
        l.add("2  3    port                         port entries");
        l.add("3  .      <name>                     name of object group");
        l.add("1  2  access-list                    build an access list");
        l.add("2  .    <name>                       name of access list");
        l.add("1  2  event-manager                  build an event manager");
        l.add("2  .    <name>                       name of access list");
        l.add("1  2  prefix-list                    build a prefix list");
        l.add("2  .    <name>                       name of prefix list");
        l.add("1  2  route-map                      build a route map");
        l.add("2  .    <name>                       name of route map");
        l.add("1  2  route-policy                   build a route policy");
        l.add("2  .    <name>                       name of route policy");
        l.add("1  2  policy-map                     build a policy map");
        l.add("2  .    <name>                       name of policy map");
        l.add("1  2  aaa                            authentication configuration");
        l.add("2  3    userlist                     build a user list");
        l.add("3  .      <name>                     name of authenticator");
        l.add("2  3    radius                       set up a radius client");
        l.add("3  .      <name>                     name of authenticator");
        l.add("2  3    tacacs                       set up a tacacs client");
        l.add("3  .      <name>                     name of authenticator");
        l.add("2  3    list                         set up an aaa list");
        l.add("3  .      <name>                     name of authenticator");
        l.add("1  2  vpdn                           vpdn client parameters");
        l.add("2  .    <name>                       name of client");
        l.add("1  2  proxy-profile                  proxy profile parameters");
        l.add("2  .    <name>                       name of profile");
        l.add("1  2  time-map                       time map parameters");
        l.add("2  .    <name>                       name of profile");
        l.add("1  2  crypto                         cryptographic configuration");
        l.add("2  3    ipsec                        ipsec profile");
        l.add("3  .      <name>                     name of profile");
        l.add("2  3    rsakey                       rsa key");
        l.add("3  4      <name>                     name of key");
        l.add("4  5        import                   import key");
        l.add("5  .          <text>                 base64 encoded private key");
        l.add("4  5,.      generate                 generate new key");
        l.add("5  .          [size]                 key size in bits");
        l.add("4  .        zeroize                  delete the key");
        l.add("2  3    dsakey                       dsa key");
        l.add("3  4      <name>                     name of key");
        l.add("4  5        import                   import key");
        l.add("5  .          <text>                 base64 encoded private key");
        l.add("4  5,.      generate                 generate new key");
        l.add("5  .          [size]                 key size in bits");
        l.add("4  .        zeroize                  delete the key");
        l.add("2  3    ecdsakey                     ecdsa key");
        l.add("3  4      <name>                     name of key");
        l.add("4  5        import                   import key");
        l.add("5  .          <text>                 base64 encoded private key");
        l.add("4  5,.      generate                 generate new key");
        l.add("5  .          [size]                 key size in bits");
        l.add("4  .        zeroize                  delete the key");
        l.add("2  3    certificate                  certificate");
        l.add("3  4      <name>                     name of certificate");
        l.add("4  5        import                   import certificate");
        l.add("5  6          rsa                    rsa key");
        l.add("5  6          dsa                    dsa key");
        l.add("5  6          ecdsa                  ecdsa key");
        l.add("6  7,.          <key>                name of key");
        l.add("7  .              <text>             base64 encoded certificate");
        l.add("4  5        generate                 generate new certificate");
        l.add("5  6          rsa                    rsa key");
        l.add("5  6          dsa                    dsa key");
        l.add("5  6          ecdsa                  ecdsa key");
        l.add("6  7,.          <key>                name of key");
        l.add("7  8,.            <text>             identifier to give");
        l.add("8  .                <num>            validity in days");
        l.add("4  .        zeroize                  delete the certificate");
        l.add("1  2  xconnect                       define one protocol cross connection");
        l.add("2  .    <name>                       name of connection");
        l.add("1  2  connect                        define one interface cross connection");
        l.add("2  .    <name>                       name of connection");
        l.add("1  2  menu                           define one menu");
        l.add("2  .    <name>                       name of menu");
        l.add("1  2  server                         create new or update existing server process");
        l.add("2  3    echo                         configure an echo server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    discard                      configure a discard server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    chargen                      configure a chargen server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    netflow                      configure an netflow server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    upnpfwd                      configure an upnp forwarder server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    upnphub                      configure an upnp hub server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    openflow                     configure an openflow server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    p4lang                       configure an p4lang server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    quote                        configure a quote server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    forwarder                    configure a forwarder server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    syslog                       configure a syslog server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    loadbalancer                 configure a loadbalancer server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    telnet                       configure a telnet server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    rfb                          configure a rfb server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    udptn                        configure an udptn server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    http                         configure a http server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    dhcp4                        configure a dhcp4 server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    dhcp6                        configure a dhcp6 server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    dns                          configure a dns server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    lpd                          configure a lpd server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    honeypot                     configure a honeypot server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    pop3                         configure a pop3 server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    smtp                         configure a smtp server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    modem                        configure a modem server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    voice                        configure a voice server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    sip                          configure a sip server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    ftp                          configure a ftp server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    tftp                         configure a tftp server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    gopher                       configure a gopher server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    iscsi                        configure an iscsi server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    bmp2mrt                      configure an bmp to mrt server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    irc                          configure an irc server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    dcp                          configure a dcp server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    ntp                          configure a ntp server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    daytime                      configure a daytime server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    time                         configure a time server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    snmp                         configure a snmp server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    socks                        configure a socks server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    rpki                         configure a rpki server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    nrpe                         configure a nrpe server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    bstun                        configure a bstun server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    stun                         configure a stun server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    vxlan                        configure a vxlan server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    geneve                       configure a geneve server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    l2f                          configure a l2f server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    l2tp2                        configure a l2tp v2 server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    l2tp3                        configure a l2tp v3 server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    etherip                      configure a etherip server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    gre                          configure a gre server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    mplsip                       configure a mplsip server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    mplsudp                      configure a mplsudp server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    pckoudp                      configure a pckoudp server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    pckodtls                     configure a pckodtls server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    pckotcp                      configure a pckotcp server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    pckotxt                      configure a pckotxt server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    gtp                          configure a gtp server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    pptp                         configure a pptp server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    radius                       configure a radius server");
        l.add("3  .      <name>                     name of server");
        l.add("2  3    tacacs                       configure a tacacs server");
        l.add("3  .      <name>                     name of server");
        return l;
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
            tabNshNtry ntry = new tabNshNtry(p, i);
            ntry.doCfgStr(cmd);
            tabNshNtry.services.put(ntry);
            return;
        }
        if (a.equals("router")) {
            tabRouteEntry.routeType o = cfgRtr.name2num(cmd.word());
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
            cfgAlias ntry = cfgAll.aliasFind(cmd.word(),
                    cfgAlias.string2type(a), true);
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
            if (a.equals("whois-server")) {
                cfgAll.whoisServer = cmd.getRemaining();
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
            if (a.equals("tcp-checksum")) {
                int i = parseUpRxtx();
                cfgAll.tcpChecksumRx = (i & 1) != 0;
                cfgAll.tcpChecksumTx = (i & 2) != 0;
                return;
            }
            if (a.equals("ludp-checksum")) {
                int i = parseUpRxtx();
                cfgAll.ludpChecksumRx = (i & 1) != 0;
                cfgAll.ludpChecksumTx = (i & 2) != 0;
                return;
            }
            if (a.equals("dccp-checksum")) {
                int i = parseUpRxtx();
                cfgAll.dccpChecksumRx = (i & 1) != 0;
                cfgAll.dccpChecksumTx = (i & 2) != 0;
                return;
            }
            if (a.equals("sctp-checksum")) {
                int i = parseUpRxtx();
                cfgAll.sctpChecksumRx = (i & 1) != 0;
                cfgAll.sctpChecksumTx = (i & 2) != 0;
                return;
            }
            if (a.equals("unreach-interval")) {
                cfgAll.unreachInt = bits.str2num(cmd.word());
                return;
            }
            if (a.equals("punish-pmtud")) {
                cfgAll.ruinPmtuD = true;
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
            if (a.equals("proxy")) {
                cfgAll.clientProxy = cfgAll.proxyFind(cmd.word(), false);
                if (cfgAll.clientProxy == null) {
                    cmd.error("no such profile");
                    return;
                }
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
            if (a.equals("upgrade-backup")) {
                cfgAll.upgradeBackup = true;
                return;
            }
            if (a.equals("upgrade-ownkey")) {
                cfgAll.upgradeOwnKey = true;
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
                cfgAll.configBackup = cmd.word();
                return;
            }
            if (a.equals("config-exclusive")) {
                cfgAll.configExclusive = 1;
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
                cmd.error("invalid bundle number");
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
            tabNshNtry ntry = new tabNshNtry(p, i);
            if (tabNshNtry.services.del(ntry) == null) {
                cmd.error("invalid nsh number");
                return;
            }
            return;
        }
        if (a.equals("router")) {
            tabRouteEntry.routeType o = cfgRtr.name2num(cmd.word());
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
            cfgAll.aliasDel(cmd.word(), cfgAlias.string2type(a));
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
            if (a.equals("whois-server")) {
                cfgAll.whoisServer = null;
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
            if (a.equals("prefer-ipv6")) {
                cfgAll.preferIpv6 = false;
                return;
            }
            if (a.equals("prefer-ipv4")) {
                cfgAll.preferIpv6 = true;
                return;
            }
            if (a.equals("unreach-interval")) {
                cfgAll.unreachInt = 0;
                return;
            }
            if (a.equals("punish-pmtud")) {
                cfgAll.ruinPmtuD = false;
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
            if (a.equals("proxy")) {
                cfgAll.clientProxy = null;
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

    private cfgVrf parseUpNat(int p, tabNatCfgN ntry) {
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
        if (a.equals("timeout")) {
            fwd.natTimeout = bits.str2num(cmd.word());
            return null;
        }
        ntry.sequence = fwd.natCfg.nextseq();
        if (ntry.fromString(s)) {
            return null;
        }
        if (ntry.origSrcList != null) {
            ntry.origSrcList.copyCores(fwd.natCfg);
        }
        return vrf;
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
        l.add("2  3    multicast                    configure multicast parameters");
        l.add("3  4      <vrf>                      name of routing table");
        l.add("4  5        join-group               unconditionally process multicast traffic");
        l.add("5  6          <addr>                 group address");
        l.add("6  .            <addr>               source address");
        l.add("2  3    flow                         configure netflow parameters");
        l.add("3  4      <vrf>                      name of routing table");
        l.add("4  .        collect                  just collect");
        l.add("4  5        export                   collect and export");
        l.add("5  6          <name>                 proxy profile");
        l.add("6  7            <addr>               target address");
        l.add("7  .              <num>              port number");
        l.add("2  3    pbr                          configure policy based routing");
        l.add("3  4,6    <vrf>                      name of routing table");
        l.add("4  5        sequence                 sequence number");
        l.add("5  6          <num>                  number");
        l.add("6  7            <name>               access list name");
        l.add("7  8,.            <vrf>              target vrf");
        l.add("8  9                interface        set target interface");
        l.add("9  8,.                <name>         interface name");
        l.add("8  9                nexthop          set target address");
        l.add("9  8,.                <addr>         target address");
        l.add("8  9                nsh              set target service");
        l.add("9  10                 <num>          service path");
        l.add("10 8,.                  <num>        service index");
        l.add("2  3    nat                          configure network address translation");
        l.add("3  4,6    <vrf>                      name of routing table");
        l.add("4  5        timeout                  specify timeout");
        l.add("5  .          <num>                  time in ms");
        l.add("4  5        sequence                 sequence number");
        l.add("5  6          <num>                  number");
        l.add("6  7            srclist              source address translation");
        l.add("7  8              <name>             access list name");
        l.add("8  9                interface        translated interface");
        l.add("9  .                  <name>         translated interface");
        l.add("8  .                <new>            translated address");
        l.add("6  7            source               source address translation");
        l.add("7  8              <orig>             original address");
        l.add("8  9                interface        translated interface");
        l.add("9  .                  <name>         translated interface");
        l.add("8  .                <new>            translated address");
        l.add("6  7            target               target address translation");
        l.add("7  8              <orig>             original address");
        l.add("8  9                interface        translated interface");
        l.add("9  .                  <name>         translated interface");
        l.add("8  .                <new>            translated address");
        l.add("6  7            srcport              source address translation");
        l.add("7  8              <proto>            protocol number");
        l.add("8  9                <orig>           original address");
        l.add("9  10                 <orig>         original port");
        l.add("10 11                   interface    translated interface");
        l.add("11 12                     <name>     translated interface");
        l.add("12 .                        <new>    translated port");
        l.add("10 11                   <new>        translated address");
        l.add("11 .                      <new>      translated port");
        l.add("6  7            trgport              target address translation");
        l.add("7  8              <proto>            protocol number");
        l.add("8  9                <orig>           original address");
        l.add("9  10                 <orig>         original port");
        l.add("10 11                   interface    translated interface");
        l.add("11 12                     <name>     translated interface");
        l.add("12 .                        <new>    translated port");
        l.add("10 11                   <new>        translated address");
        l.add("11 .                      <new>      translated port");
        l.add("6  7            srcpref              source address translation");
        l.add("7  8              <orig>             original address");
        l.add("8  9                <new>            translated address");
        l.add("9  .                  <mask>         address mask");
        l.add("6  7            trgpref              target address translation");
        l.add("7  8              <orig>             original address");
        l.add("8  9                <new>            translated address");
        l.add("9  .                  <mask>         address mask");
        l.add("2  3    route                        configure static unicast routes");
        l.add("3  4        <vrf>                    name of routing table");
        l.add("4  5          <network>              destination network");
        l.add("5  6            <mask>               destination mask");
        l.add("6  7,.            <nexthop>          forwarding router's address");
        l.add("7  7,.              mplsimp          use mpls implicit null");
        l.add("7  7,.              mplsexp          use mpls explicit null");
        l.add("7  8                distance         set distance metric");
        l.add("8  7,.                <dist>         distance value");
        l.add("7  8                tag              set tag value");
        l.add("8  7,.                <tag>          tag value");
        l.add("7  8                tracker          set tracker to check");
        l.add("8  7,.                <name>         tracker name");
        l.add("7  8                interface        force to interface");
        l.add("8  7,.                <name>         interface name");
        l.add("2  3    mroute                       configure static multicast routes");
        l.add("3  4        <vrf>                    name of routing table");
        l.add("4  5          <network>              destination network");
        l.add("5  6            <mask>               destination mask");
        l.add("6  7,.            <nexthop>          forwarding router's address");
        l.add("7  8                distance         set distance metric");
        l.add("8  7,.                <dist>         distance value");
        l.add("7  8                tag              set tag value");
        l.add("8  7,.                <tag>          tag value");
        l.add("7  8                tracker          set tracker to check");
        l.add("8  7,.                <name>         tracker name");
        l.add("7  8                interface        force to interface");
        l.add("8  7,.                <name>         interface name");
        l.add("2  3    pool                         configure address pool");
        l.add("3  4      <name>                     name of address pool");
        l.add("4  5        <first>                  first address in pool");
        l.add("5  6          <inc>                  increment in address format");
        l.add("6  .            <num>                number of addresses");
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
            cfgVrf vrf = parseUpNat(4, red);
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
            cfgVrf vrf = parseUpNat(4, red);
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
            cfgVrf vrf = parseUpNat(6, red);
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
            cfgVrf vrf = parseUpNat(6, red);
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
            logger.fileStart("");
            return;
        }
        if (s.equals("rotate")) {
            logger.logRotLim = 0;
            logger.logRotNam = "";
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
            logger.fileStart(cmd.word());
            return;
        }
        if (s.equals("rotate")) {
            logger.logRotLim = bits.str2num(cmd.word());
            logger.logRotNam = cmd.word();
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

    private <T extends cryKeyGeneric> void cryptoDoKey(tabGen<cfgKey<T>> lst,
            T key) {
        String nam = cmd.word();
        String a = cmd.word();
        if (a.equals("zeroize")) {
            cfgAll.keyDel(lst, nam);
            return;
        }
        if (a.equals("import")) {
            if (key.pemReadStr(cmd.word(), false)) {
                cmd.error("error decoding");
                return;
            }
            key.keyName = nam;
            cfgKey<T> cfg = cfgAll.keyFind(lst, nam, true);
            cfg.key = key;
            return;
        }
        if (a.equals("generate")) {
            int i = bits.str2num(cmd.word());
            if (i < 1) {
                i = 512;
            }
            cmd.error("generating " + i + " bit long key");
            key.keyMake(i);
            if (key.keyVerify()) {
                cmd.error("bad key generated");
                return;
            }
            key.keyName = nam;
            cfgKey<T> cfg = cfgAll.keyFind(lst, nam, true);
            cfg.key = key;
            cmd.error("done");
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
            if (a.equals("import")) {
                cryKeyGeneric k = findKey();
                if (k == null) {
                    return;
                }
                cryCertificate c = new cryCertificate();
                c.crtName = nam;
                a = cmd.word();
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
            if (a.equals("generate")) {
                cryKeyGeneric k = findKey();
                if (k == null) {
                    return;
                }
                a = cmd.word();
                if (a.length() < 1) {
                    a = cfgAll.hostName;
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
        srv.rename(cmd.word());
        modeDserver = lst.find(srv, true);
        if (modeDserver == null) {
            cmd.error("invalid server name");
            return;
        }
        modeV = modes.server;
        return;
    }

    private <T extends servGeneric> void daemonErase(T srv, servGenList<T> lst) {
        srv.rename(cmd.word());
        if (lst.del(srv) == null) {
            cmd.error("invalid server name");
            return;
        }
    }

}
