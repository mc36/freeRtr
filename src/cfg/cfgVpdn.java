package cfg;

import auth.authLocal;
import clnt.clntAnyconn;
import clnt.clntBstun;
import clnt.clntErspan;
import clnt.clntEtherIp;
import clnt.clntUti;
import clnt.clntNvGre;
import clnt.clntGrePpp;
import clnt.clntAx25;
import clnt.clntDlsw;
import clnt.clntGtp;
import clnt.clntL2f;
import clnt.clntL2tp2;
import clnt.clntL2tp3;
import clnt.clntMplsPwe;
import clnt.clntPckOdtls;
import clnt.clntPckOtcp;
import clnt.clntPckOtxt;
import clnt.clntPckOudp;
import clnt.clntPptp;
import clnt.clntProxy;
import clnt.clntSstp;
import clnt.clntStun;
import clnt.clntTelnet;
import clnt.clntVxlan;
import clnt.clntGeneve;
import ifc.ifcBridgeIfc;
import ifc.ifcNull;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import pack.packLdpPwe;
import tab.tabGen;
import user.userFilter;
import user.userHelping;
import util.bits;
import util.cmds;

/**
 * one virtual private dialup network configuration
 *
 * @author matecsaba
 */
public class cfgVpdn implements Comparator<cfgVpdn>, cfgGeneric {

    /**
     * name of this vpdn
     */
    public String name = "";

    /**
     * description of this vpdn
     */
    public String description = null;

    /**
     * upper level interface
     */
    public cfgIfc ifaceDialer = null;

    /**
     * upper level interface
     */
    public cfgBrdg ifaceBridge = null;

    /**
     * proxy profile
     */
    public clntProxy proxy;

    /**
     * target of tunnel
     */
    public String target = null;

    /**
     * called telephone number
     */
    public String called;

    /**
     * calling telephone number
     */
    public String calling;

    /**
     * username
     */
    public String username;

    /**
     * password
     */
    public String password;

    /**
     * direction, true=outgoing, false=incoming
     */
    public boolean direction = true;

    /**
     * chat script
     */
    public cfgChat script = null;

    /**
     * vc id
     */
    public int vcid = 0;

    /**
     * pw encapsulation
     */
    public int pwtype = 0;

    /**
     * pw mtu
     */
    public int pwmtu = 0;

    /**
     * control word
     */
    public boolean ctrlWrd = false;

    /**
     * target protocol
     */
    public protocolType proto = null;

    /**
     * protocol type
     */
    public enum protocolType {

        /**
         * l2f
         */
        prL2f,
        /**
         * l2tp v2
         */
        prL2tp2,
        /**
         * l2tp v3
         */
        prL2tp3,
        /**
         * stun
         */
        prStun,
        /**
         * telnet
         */
        prTelnet,
        /**
         * tls
         */
        prTls,
        /**
         * bstun
         */
        prBstun,
        /**
         * gtp
         */
        prGtp,
        /**
         * pptp
         */
        prPptp,
        /**
         * pckoudp
         */
        prPou,
        /**
         * pckodtls
         */
        prPod,
        /**
         * pckotcp
         */
        prPot,
        /**
         * pckotxt
         */
        prPox,
        /**
         * pppogre
         */
        prPog,
        /**
         * ax25
         */
        prAx25,
        /**
         * pweompls
         */
        prPwom,
        /**
         * erspan
         */
        prErspan,
        /**
         * dlsw
         */
        prDlsw,
        /**
         * etherip
         */
        prEtherip,
        /**
         * uti
         */
        prUti,
        /**
         * nvgre
         */
        prNvgre,
        /**
         * sstp
         */
        prSstp,
        /**
         * anyconnect
         */
        prAnycon,
        /**
         * vxlan
         */
        prVxlan,
        /**
         * geneve
         */
        prGeneve

    }

    private clntL2f l2f;

    private clntL2tp2 l2tp2;

    private clntL2tp3 l2tp3;

    private clntSstp sstp;

    private clntAnyconn anycon;

    private clntStun stun;

    private clntBstun bstun;

    private clntTelnet telnet;

    private clntGtp gtp;

    private clntPptp pptp;

    private clntPckOudp pou;

    private clntPckOdtls pod;

    private clntPckOtcp pot;

    private clntPckOtxt pox;

    private clntGrePpp pog;

    private clntAx25 ax25;

    private clntVxlan vxl;

    private clntGeneve gnv;

    private clntMplsPwe pwom;

    private clntErspan erspan;

    private clntDlsw dlsw;

    private clntEtherIp etherip;

    private clntUti uti;

    private clntNvGre nvgre;

    private ifcBridgeIfc brdgIfc;

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "vpdn .*! no description",
        "vpdn .*! no interface",
        "vpdn .*! no bridge-group",
        "vpdn .*! no proxy",
        "vpdn .*! no script",
        "vpdn .*! no target",
        "vpdn .*! no called",
        "vpdn .*! no calling",
        "vpdn .*! no username",
        "vpdn .*! no password",
        "vpdn .*! no mtu",
        "vpdn .*! no vcid",
        "vpdn .*! direction outgoing",
        "vpdn .*! no control-word",
        "vpdn .*! no pwtype",
        "vpdn .*! no protocol"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    /**
     * convert type to string
     *
     * @param i type
     * @return string
     */
    public static String type2str(protocolType i) {
        if (i == null) {
            return null;
        }
        switch (i) {
            case prL2f:
                return "l2f";
            case prL2tp2:
                return "l2tp2";
            case prL2tp3:
                return "l2tp3";
            case prSstp:
                return "sstp";
            case prAnycon:
                return "anyconn";
            case prStun:
                return "stun";
            case prTelnet:
                return "telnet";
            case prTls:
                return "tls";
            case prBstun:
                return "bstun";
            case prGtp:
                return "gtp";
            case prPptp:
                return "pptp";
            case prPou:
                return "pckoudp";
            case prPod:
                return "pckodtls";
            case prPot:
                return "pckotcp";
            case prPox:
                return "pckotxt";
            case prPog:
                return "greppp";
            case prAx25:
                return "ax25";
            case prPwom:
                return "pweompls";
            case prErspan:
                return "erspan";
            case prDlsw:
                return "dlsw";
            case prEtherip:
                return "etherip";
            case prUti:
                return "uti";
            case prNvgre:
                return "nvgre";
            case prVxlan:
                return "vxlan";
            case prGeneve:
                return "geneve";
            default:
                return null;
        }
    }

    /**
     * convert string to type
     *
     * @param s string
     * @return type
     */
    public static protocolType str2type(String s) {
        if (s.equals("l2f")) {
            return protocolType.prL2f;
        }
        if (s.equals("l2tp2")) {
            return protocolType.prL2tp2;
        }
        if (s.equals("l2tp3")) {
            return protocolType.prL2tp3;
        }
        if (s.equals("sstp")) {
            return protocolType.prSstp;
        }
        if (s.equals("anyconn")) {
            return protocolType.prAnycon;
        }
        if (s.equals("stun")) {
            return protocolType.prStun;
        }
        if (s.equals("bstun")) {
            return protocolType.prBstun;
        }
        if (s.equals("telnet")) {
            return protocolType.prTelnet;
        }
        if (s.equals("tls")) {
            return protocolType.prTls;
        }
        if (s.equals("gtp")) {
            return protocolType.prGtp;
        }
        if (s.equals("pptp")) {
            return protocolType.prPptp;
        }
        if (s.equals("pckoudp")) {
            return protocolType.prPou;
        }
        if (s.equals("pckodtls")) {
            return protocolType.prPod;
        }
        if (s.equals("pckotcp")) {
            return protocolType.prPot;
        }
        if (s.equals("pckotxt")) {
            return protocolType.prPox;
        }
        if (s.equals("greppp")) {
            return protocolType.prPog;
        }
        if (s.equals("ax25")) {
            return protocolType.prAx25;
        }
        if (s.equals("pweompls")) {
            return protocolType.prPwom;
        }
        if (s.equals("erspan")) {
            return protocolType.prErspan;
        }
        if (s.equals("dlsw")) {
            return protocolType.prDlsw;
        }
        if (s.equals("etherip")) {
            return protocolType.prEtherip;
        }
        if (s.equals("uti")) {
            return protocolType.prUti;
        }
        if (s.equals("nvgre")) {
            return protocolType.prNvgre;
        }
        if (s.equals("vxlan")) {
            return protocolType.prVxlan;
        }
        if (s.equals("geneve")) {
            return protocolType.prGeneve;
        }
        return null;
    }

    public int compare(cfgVpdn o1, cfgVpdn o2) {
        return o1.name.toLowerCase().compareTo(o2.name.toLowerCase());
    }

    public String toString() {
        return "vpdn " + name;
    }

    public List<String> getShRun(boolean filter) {
        List<String> l = new ArrayList<String>();
        l.add("vpdn " + name);
        cmds.cfgLine(l, description == null, cmds.tabulator, "description", description);
        if (ifaceDialer == null) {
            l.add(cmds.tabulator + "no interface");
        } else {
            l.add(cmds.tabulator + "interface " + ifaceDialer.name);
        }
        if (ifaceBridge == null) {
            l.add(cmds.tabulator + "no bridge-group");
        } else {
            l.add(cmds.tabulator + "bridge-group " + ifaceBridge.name);
        }
        if (proxy == null) {
            l.add(cmds.tabulator + "no proxy");
        } else {
            l.add(cmds.tabulator + "proxy " + proxy.name);
        }
        if (script == null) {
            l.add(cmds.tabulator + "no script");
        } else {
            l.add(cmds.tabulator + "script " + script.name);
        }
        cmds.cfgLine(l, target == null, cmds.tabulator, "target", target);
        cmds.cfgLine(l, username == null, cmds.tabulator, "username", username);
        cmds.cfgLine(l, password == null, cmds.tabulator, "password", authLocal.passwdEncode(password));
        cmds.cfgLine(l, called == null, cmds.tabulator, "called", called);
        cmds.cfgLine(l, calling == null, cmds.tabulator, "calling", calling);
        cmds.cfgLine(l, pwmtu == 0, cmds.tabulator, "mtu", "" + pwmtu);
        cmds.cfgLine(l, vcid == 0, cmds.tabulator, "vcid", "" + vcid);
        String s;
        if (direction) {
            s = "outgoing";
        } else {
            s = "incoming";
        }
        l.add(cmds.tabulator + "direction " + s);
        cmds.cfgLine(l, !ctrlWrd, cmds.tabulator, "control-word", "");
        cmds.cfgLine(l, pwtype < 1, cmds.tabulator, "pwtype", packLdpPwe.type2string(pwtype));
        cmds.cfgLine(l, proto == null, cmds.tabulator, "protocol", type2str(proto));
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        if (!filter) {
            return l;
        }
        return userFilter.filterText(l, defaultF);
    }

    public userHelping getHelp() {
        userHelping l = userHelping.getGenCfg();
        l.add("1 2  protocol                       specify protocol to use");
        l.add("2 .    l2f                          select l2f");
        l.add("2 .    l2tp2                        select l2tp v2");
        l.add("2 .    l2tp3                        select l2tp v3");
        l.add("2 .    sstp                         select sstp");
        l.add("2 .    anyconn                      select anyconnect");
        l.add("2 .    stun                         select stun");
        l.add("2 .    bstun                        select bstun");
        l.add("2 .    telnet                       select telnet");
        l.add("2 .    tls                          select tls");
        l.add("2 .    gtp                          select gtp");
        l.add("2 .    greppp                       select ppp over gre");
        l.add("2 .    ax25                         select ax25");
        l.add("2 .    pptp                         select pptp");
        l.add("2 .    pckoudp                      select packet over udp");
        l.add("2 .    pckodtls                     select packet over dtls");
        l.add("2 .    pckotcp                      select packet over tcp");
        l.add("2 .    pckOtxt                      select packet over txt");
        l.add("2 .    pweompls                     select pwe over mpls");
        l.add("2 .    erspan                       select erspan");
        l.add("2 .    dlsw                         select dlsw");
        l.add("2 .    etherip                      select etherip");
        l.add("2 .    uti                          select uti");
        l.add("2 .    nvgre                        select nvgre");
        l.add("2 .    vxlan                        select vxlan");
        l.add("2 .    geneve                       select geneve");
        l.add("1 2  direction                      specify direction of connection");
        l.add("2 .    incoming                     act as incoming call");
        l.add("2 .    outgoing                     act as outgoing call");
        l.add("1 2  proxy                          proxy profile to use");
        l.add("2 .    <name>                       proxy name");
        l.add("1 2  script                         name of chat script to use");
        l.add("2 .    <name>                       script name");
        l.add("1 2  target                         specify target of tunnel");
        l.add("2 .    <name>                       name or address of target");
        l.add("1 2  description                    specify description");
        l.add("2 2,.  <str>                        description");
        l.add("1 2  interface                      name of interface to serve");
        l.add("2 .    <name>                       interface name");
        l.add("1 2  bridge-group                   name of bridge group to serve");
        l.add("2 .    <name>                       bridge group number");
        l.add("1 2  called                         specify called number");
        l.add("2 .    <name>                       called number");
        l.add("1 2  calling                        specify calling number");
        l.add("2 .    <name>                       calling number");
        l.add("1 2  username                       specify username");
        l.add("2 .    <name>                       username");
        l.add("1 2  password                       specify password");
        l.add("2 .    <name>                       password");
        l.add("1 2  vcid                           specify vc id");
        l.add("2 .    <num>                        vc id");
        l.add("1 .  control-word                   enable/disable control word");
        l.add("1 2  mtu                            specify vc mtu");
        l.add("2 .    <num>                        mtu");
        l.add("1 2  pwtype                         type of pseudowire");
        l.add("2 .    ethernet                     ethernet mode");
        l.add("2 .    vlan                         vlan mode");
        l.add("2 .    hdlc                         hdlc mode");
        l.add("2 .    ppp                          ppp mode");
        l.add("2 .    ip                           ip mode");
        l.add("2 .    fr-dlci                      fr dlci mode");
        l.add("2 .    atm-aal5                     atm aal5 mode");
        l.add("2 .    atm-vcc                      atm vcc mode");
        l.add("2 .    atm-vpc                      atm vpc mode");
        l.add("2 .    atm-port                     atm port mode");
        return l;
    }

    /**
     * flap session
     *
     * @param tim time of flap
     */
    public synchronized void doFlap(int tim) {
        stop2run();
        bits.sleep(tim);
        setup2run();
    }

    public synchronized void doCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("interface")) {
            stop2run();
            ifaceDialer = cfgAll.ifcFind(cmd.word(), false);
            if (ifaceDialer == null) {
                cmd.error("no such interface");
                return;
            }
            if (ifaceDialer.type != cfgIfc.ifaceType.dialer) {
                cmd.error("not a dialer");
                ifaceDialer = null;
                return;
            }
            setup2run();
            return;
        }
        if (s.equals("bridge-group")) {
            stop2run();
            ifaceBridge = cfgAll.brdgFind(cmd.word(), false);
            if (ifaceBridge == null) {
                cmd.error("no such bridge group");
                return;
            }
            setup2run();
            return;
        }
        if (s.equals("description")) {
            description = cmd.getRemaining();
            return;
        }
        if (s.equals("proxy")) {
            cfgProxy prx = cfgAll.proxyFind(cmd.word(), false);
            if (prx == null) {
                cmd.error("no such proxy");
                return;
            }
            proxy = prx.proxy;
            return;
        }
        if (s.equals("script")) {
            script = cfgAll.chatFind(cmd.word(), false);
            if (script == null) {
                cmd.error("no such script");
                return;
            }
            return;
        }
        if (s.equals("target")) {
            stop2run();
            target = cmd.word();
            setup2run();
            return;
        }
        if (s.equals("pwtype")) {
            pwtype = packLdpPwe.string2type(cmd.word());
            return;
        }
        if (s.equals("protocol")) {
            stop2run();
            s = cmd.word();
            proto = str2type(s);
            setup2run();
            return;
        }
        if (s.equals("direction")) {
            s = cmd.word();
            if (s.equals("incoming")) {
                direction = false;
            }
            if (s.equals("outgoing")) {
                direction = true;
            }
            return;
        }
        if (s.equals("called")) {
            called = cmd.word();
            return;
        }
        if (s.equals("calling")) {
            calling = cmd.word();
            return;
        }
        if (s.equals("mtu")) {
            pwmtu = bits.str2num(cmd.word());
            return;
        }
        if (s.equals("control-word")) {
            ctrlWrd = true;
            return;
        }
        if (s.equals("vcid")) {
            vcid = bits.str2num(cmd.word());
            return;
        }
        if (s.equals("username")) {
            username = cmd.word();
            return;
        }
        if (s.equals("password")) {
            password = authLocal.passwdDecode(cmd.getRemaining());
            return;
        }
        if (!s.equals("no")) {
            cmd.badCmd();
            return;
        }
        s = cmd.word();
        if (s.equals("interface")) {
            stop2run();
            ifaceDialer = null;
            return;
        }
        if (s.equals("bridge-group")) {
            stop2run();
            ifaceBridge = null;
            return;
        }
        if (s.equals("description")) {
            description = null;
            return;
        }
        if (s.equals("proxy")) {
            stop2run();
            proxy = null;
            return;
        }
        if (s.equals("script")) {
            stop2run();
            script = null;
            return;
        }
        if (s.equals("target")) {
            stop2run();
            target = null;
            return;
        }
        if (s.equals("pwtype")) {
            stop2run();
            pwtype = 0;
            return;
        }
        if (s.equals("protocol")) {
            stop2run();
            proto = null;
            return;
        }
        if (s.equals("called")) {
            called = null;
            return;
        }
        if (s.equals("calling")) {
            calling = null;
            return;
        }
        if (s.equals("mtu")) {
            pwmtu = 0;
            return;
        }
        if (s.equals("control-word")) {
            ctrlWrd = false;
            return;
        }
        if (s.equals("vcid")) {
            vcid = 0;
            return;
        }
        if (s.equals("username")) {
            username = null;
            return;
        }
        if (s.equals("password")) {
            password = null;
            return;
        }
        cmd.badCmd();
    }

    public String getPrompt() {
        return "vpdn";
    }

    /**
     * stop process
     */
    public void stop2run() {
        if (ifaceDialer != null) {
            ifaceDialer.setLowerHandler(new ifcNull());
        }
        if (brdgIfc != null) {
            brdgIfc.closeUp();
            brdgIfc = null;
        }
        if (l2f != null) {
            l2f.workStop();
            l2f = null;
        }
        if (l2tp2 != null) {
            l2tp2.workStop();
            l2tp2 = null;
        }
        if (l2tp3 != null) {
            l2tp3.workStop();
            l2tp3 = null;
        }
        if (sstp != null) {
            sstp.workStop();
            sstp = null;
        }
        if (anycon != null) {
            anycon.workStop();
            anycon = null;
        }
        if (stun != null) {
            stun.workStop();
            stun = null;
        }
        if (bstun != null) {
            bstun.workStop();
            bstun = null;
        }
        if (telnet != null) {
            telnet.workStop();
            telnet = null;
        }
        if (gtp != null) {
            gtp.workStop();
            gtp = null;
        }
        if (pptp != null) {
            pptp.workStop();
            pptp = null;
        }
        if (pou != null) {
            pou.workStop();
            pou = null;
        }
        if (pod != null) {
            pod.workStop();
            pod = null;
        }
        if (pot != null) {
            pot.workStop();
            pot = null;
        }
        if (pox != null) {
            pox.workStop();
            pox = null;
        }
        if (pog != null) {
            pog.workStop();
            pog = null;
        }
        if (ax25 != null) {
            ax25.workStop();
            ax25 = null;
        }
        if (pwom != null) {
            pwom.workStop();
            pwom = null;
        }
        if (erspan != null) {
            erspan.workStop();
            erspan = null;
        }
        if (dlsw != null) {
            dlsw.workStop();
            dlsw = null;
        }
        if (etherip != null) {
            etherip.workStop();
            etherip = null;
        }
        if (uti != null) {
            uti.workStop();
            uti = null;
        }
        if (nvgre != null) {
            nvgre.workStop();
            nvgre = null;
        }
        if (vxl != null) {
            vxl.workStop();
            vxl = null;
        }
        if (gnv != null) {
            gnv.workStop();
            gnv = null;
        }
    }

    /**
     * restart process
     */
    public void setup2run() {
        if (target == null) {
            return;
        }
        if (proxy == null) {
            return;
        }
        if (proto == null) {
            return;
        }
        if (proto == null) {
            return;
        }
        switch (proto) {
            case prL2f:
                if (ifaceDialer == null) {
                    return;
                }
                l2f = new clntL2f();
                l2f.target = target;
                l2f.vrf = proxy.vrf;
                l2f.srcIfc = proxy.srcIfc;
                l2f.hostname = username;
                l2f.password = password;
                l2f.setUpper(ifaceDialer.getEncapProto());
                l2f.workStart();
                break;
            case prL2tp2:
                if (ifaceDialer == null) {
                    return;
                }
                l2tp2 = new clntL2tp2();
                l2tp2.target = target;
                l2tp2.vrf = proxy.vrf;
                l2tp2.srcIfc = proxy.srcIfc;
                l2tp2.direction = direction;
                l2tp2.called = called;
                l2tp2.calling = calling;
                l2tp2.hostname = username;
                l2tp2.password = password;
                l2tp2.setUpper(ifaceDialer.getEncapProto());
                l2tp2.workStart();
                break;
            case prL2tp3:
                if ((ifaceDialer == null) && (ifaceBridge == null)) {
                    return;
                }
                l2tp3 = new clntL2tp3();
                l2tp3.pwType = pwtype;
                l2tp3.target = target;
                l2tp3.vrf = proxy.vrf;
                l2tp3.srcIfc = proxy.srcIfc;
                l2tp3.vcid = "" + vcid;
                l2tp3.direction = direction;
                l2tp3.hostname = username;
                l2tp3.password = password;
                if (ifaceDialer != null) {
                    l2tp3.setUpper(ifaceDialer.getEncapProto());
                } else {
                    brdgIfc = ifaceBridge.bridgeHed.newIface(false, true, false);
                    l2tp3.setUpper(brdgIfc);
                }
                l2tp3.workStart();
                break;
            case prSstp:
                if (ifaceDialer == null) {
                    return;
                }
                sstp = new clntSstp();
                sstp.target = target;
                sstp.proxy = proxy;
                sstp.unique = name;
                sstp.username = username;
                sstp.password = password;
                sstp.setUpper(ifaceDialer.getEncapProto());
                sstp.workStart();
                break;
            case prAnycon:
                if (ifaceDialer == null) {
                    return;
                }
                anycon = new clntAnyconn();
                anycon.cfger = ifaceDialer;
                anycon.target = target;
                anycon.proxy = proxy;
                anycon.username = username;
                anycon.password = password;
                anycon.setUpper(ifaceDialer.getEncapProto());
                anycon.workStart();
                break;
            case prStun:
                if (ifaceDialer == null) {
                    return;
                }
                stun = new clntStun();
                stun.target = target;
                stun.proxy = proxy;
                stun.group = vcid;
                stun.setUpper(ifaceDialer.getEncapProto());
                stun.workStart();
                break;
            case prBstun:
                if (ifaceDialer == null) {
                    return;
                }
                bstun = new clntBstun();
                bstun.target = target;
                bstun.proxy = proxy;
                bstun.group = vcid;
                bstun.script = script.script;
                bstun.setUpper(ifaceDialer.getEncapProto());
                bstun.workStart();
                break;
            case prTelnet:
                if (ifaceDialer == null) {
                    return;
                }
                telnet = new clntTelnet();
                telnet.target = target;
                telnet.proxy = proxy;
                telnet.port = vcid;
                telnet.tls = false;
                telnet.script = script.script;
                telnet.setUpper(ifaceDialer.getEncapProto());
                telnet.workStart();
                break;
            case prTls:
                if (ifaceDialer == null) {
                    return;
                }
                telnet = new clntTelnet();
                telnet.target = target;
                telnet.proxy = proxy;
                telnet.port = vcid;
                telnet.tls = true;
                telnet.script = script.script;
                telnet.setUpper(ifaceDialer.getEncapProto());
                telnet.workStart();
                break;
            case prGtp:
                if (ifaceDialer == null) {
                    return;
                }
                gtp = new clntGtp();
                gtp.target = target;
                gtp.vrf = proxy.vrf;
                gtp.srcIfc = proxy.srcIfc;
                gtp.apn = called;
                gtp.isdn = calling;
                gtp.imsi = calling;
                gtp.imei = calling;
                gtp.setUpper(ifaceDialer.getEncapProto());
                gtp.workStart();
                break;
            case prPptp:
                if (ifaceDialer == null) {
                    return;
                }
                pptp = new clntPptp();
                pptp.target = target;
                pptp.vrf = proxy.vrf;
                pptp.srcIfc = proxy.srcIfc;
                pptp.direction = direction;
                pptp.called = called;
                pptp.setUpper(ifaceDialer.getEncapProto());
                pptp.workStart();
                break;
            case prPou:
                if ((ifaceDialer == null) && (ifaceBridge == null)) {
                    return;
                }
                pou = new clntPckOudp();
                pou.target = target;
                pou.vrf = proxy.vrf;
                pou.srcIfc = proxy.srcIfc;
                pou.prtR = vcid;
                if (!ctrlWrd) {
                    pou.prtL = vcid;
                }
                if (ifaceDialer != null) {
                    pou.setUpper(ifaceDialer.getEncapProto());
                } else {
                    brdgIfc = ifaceBridge.bridgeHed.newIface(false, true, false);
                    pou.setUpper(brdgIfc);
                }
                pou.workStart();
                break;
            case prPod:
                if (ifaceDialer == null) {
                    return;
                }
                pod = new clntPckOdtls();
                pod.target = target;
                pod.proxy = proxy;
                pod.prtR = vcid;
                if (!ctrlWrd) {
                    pod.prtL = vcid;
                }
                pod.setUpper(ifaceDialer.getEncapProto());
                pod.workStart();
                break;
            case prPot:
                if (ifaceDialer == null) {
                    return;
                }
                pot = new clntPckOtcp();
                pot.target = target;
                pot.proxy = proxy;
                pot.prtR = vcid;
                if (!ctrlWrd) {
                    pot.prtL = vcid;
                }
                pot.setUpper(ifaceDialer.getEncapProto());
                pot.workStart();
                break;
            case prPox:
                if (ifaceDialer == null) {
                    return;
                }
                pox = new clntPckOtxt();
                pox.target = target;
                pox.proxy = proxy;
                pox.prtR = vcid;
                if (!ctrlWrd) {
                    pox.prtL = vcid;
                }
                pox.setUpper(ifaceDialer.getEncapProto());
                pox.workStart();
                break;
            case prPog:
                if (ifaceDialer == null) {
                    return;
                }
                pog = new clntGrePpp();
                pog.target = target;
                pog.vrf = proxy.vrf;
                pog.srcIfc = proxy.srcIfc;
                pog.vcid = vcid;
                pog.setUpper(ifaceDialer.getEncapProto());
                pog.workStart();
                break;
            case prAx25:
                if (ifaceDialer == null) {
                    return;
                }
                ax25 = new clntAx25();
                ax25.target = target;
                ax25.vrf = proxy.vrf;
                ax25.srcIfc = proxy.srcIfc;
                ax25.setUpper(ifaceDialer.getEncapProto());
                ax25.workStart();
                break;
            case prPwom:
                if ((ifaceDialer == null) && (ifaceBridge == null)) {
                    return;
                }
                pwom = new clntMplsPwe();
                pwom.pwType = pwtype;
                pwom.pwMtu = pwmtu;
                pwom.target = target;
                pwom.vrf = proxy.vrf;
                pwom.srcIfc = proxy.srcIfc;
                pwom.vcid = vcid;
                pwom.ctrlWrd = ctrlWrd;
                pwom.descr = name;
                if (ifaceDialer != null) {
                    pwom.setUpper(ifaceDialer.getEncapProto());
                } else {
                    brdgIfc = ifaceBridge.bridgeHed.newIface(false, true, false);
                    pwom.setUpper(brdgIfc);
                }
                pwom.workStart();
                break;
            case prErspan:
                if (ifaceBridge == null) {
                    return;
                }
                erspan = new clntErspan();
                erspan.target = target;
                erspan.vrf = proxy.vrf;
                erspan.srcIfc = proxy.srcIfc;
                erspan.spnid = vcid;
                erspan.vlnid = vcid;
                brdgIfc = ifaceBridge.bridgeHed.newIface(false, true, false);
                erspan.setUpper(brdgIfc);
                erspan.workStart();
                break;
            case prDlsw:
                if (ifaceBridge == null) {
                    return;
                }
                dlsw = new clntDlsw();
                dlsw.target = target;
                dlsw.vrf = proxy.vrf;
                dlsw.srcIfc = proxy.srcIfc;
                brdgIfc = ifaceBridge.bridgeHed.newIface(false, true, false);
                dlsw.setUpper(brdgIfc);
                dlsw.workStart();
                break;
            case prEtherip:
                if (ifaceBridge == null) {
                    return;
                }
                etherip = new clntEtherIp();
                etherip.target = target;
                etherip.vrf = proxy.vrf;
                etherip.srcIfc = proxy.srcIfc;
                brdgIfc = ifaceBridge.bridgeHed.newIface(false, true, false);
                etherip.setUpper(brdgIfc);
                etherip.workStart();
                break;
            case prUti:
                if (ifaceBridge == null) {
                    return;
                }
                uti = new clntUti();
                uti.target = target;
                uti.vrf = proxy.vrf;
                uti.srcIfc = proxy.srcIfc;
                uti.tunKey = vcid;
                brdgIfc = ifaceBridge.bridgeHed.newIface(false, true, false);
                uti.setUpper(brdgIfc);
                uti.workStart();
                break;
            case prNvgre:
                if (ifaceBridge == null) {
                    return;
                }
                nvgre = new clntNvGre();
                nvgre.target = target;
                nvgre.vrf = proxy.vrf;
                nvgre.srcIfc = proxy.srcIfc;
                nvgre.vsid = vcid;
                brdgIfc = ifaceBridge.bridgeHed.newIface(false, true, false);
                nvgre.setUpper(brdgIfc);
                nvgre.workStart();
                break;
            case prVxlan:
                if (ifaceBridge == null) {
                    return;
                }
                vxl = new clntVxlan();
                vxl.target = target;
                vxl.vrf = proxy.vrf;
                vxl.srcIfc = proxy.srcIfc;
                vxl.inst = vcid;
                vxl.prot = pwtype;
                brdgIfc = ifaceBridge.bridgeHed.newIface(false, true, false);
                vxl.setUpper(brdgIfc);
                vxl.workStart();
                break;
            case prGeneve:
                if (ifaceBridge == null) {
                    return;
                }
                gnv = new clntGeneve();
                gnv.target = target;
                gnv.vrf = proxy.vrf;
                gnv.srcIfc = proxy.srcIfc;
                gnv.vni = vcid;
                brdgIfc = ifaceBridge.bridgeHed.newIface(false, true, false);
                gnv.setUpper(brdgIfc);
                gnv.workStart();
                break;
        }
    }

}
