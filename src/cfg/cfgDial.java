package cfg;

import auth.authLocal;
import clnt.clntSip;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import pack.packRtp;
import pack.packSip;
import tab.tabGen;
import user.userFilter;
import user.userHelping;
import util.bits;
import util.cmds;
import util.logger;
import util.uniResLoc;

/**
 * one dial peer configuration
 *
 * @author matecsaba
 */
public class cfgDial implements Comparator<cfgDial>, cfgGeneric {

    /**
     * name of this dialpeer
     */
    public final String name;

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "dial-peer .*! codec alaw",
        "dial-peer .*! no vrf",
        "dial-peer .*! no source",
        "dial-peer .*! no target",
        "dial-peer .*! no username",
        "dial-peer .*! no password",
        "dial-peer .*! no myname",
        "dial-peer .*! no log",
        "dial-peer .*! keepalive 0",
        "dial-peer .*! register 0",
        "dial-peer .*! subscribe 0",
        "dial-peer .*! port-local 0",
        "dial-peer .*! port-remote " + packSip.port,
        "dial-peer .*! direction none",};

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    /**
     * direction: 0=none, 1=in, 2=out, 3=both
     */
    public int direction;

    /**
     * local port
     */
    public int portLoc = 0;

    /**
     * remote port
     */
    public int portRem = packSip.port;

    /**
     * codec, true=alaw, false=ulaw
     */
    public boolean aLaw = true;

    /**
     * keepalive interval
     */
    public int keepalive = 0;

    /**
     * register interval
     */
    public int register = 0;

    /**
     * subscribe interval
     */
    public int subscribe = 0;

    /**
     * log calls
     */
    public boolean log;

    /**
     * vrf of target
     */
    public cfgVrf vrf = null;

    /**
     * source interface
     */
    public cfgIfc ifc = null;

    /**
     * endpoint
     */
    public String endpt;

    /**
     * target
     */
    public String trg;

    /**
     * username
     */
    public String usr;

    /**
     * password
     */
    public String pwd;

    /**
     * src pattern
     */
    public List<String> matSrc = new ArrayList<String>();

    /**
     * dst pattern
     */
    public List<String> matDst = new ArrayList<String>();

    /**
     * translate in src
     */
    public List<cfgTrnsltn> trnsInSrc = new ArrayList<cfgTrnsltn>();

    /**
     * translate in dst
     */
    public List<cfgTrnsltn> trnsInDst = new ArrayList<cfgTrnsltn>();

    /**
     * translate out src
     */
    public List<cfgTrnsltn> trnsOutSrc = new ArrayList<cfgTrnsltn>();

    /**
     * translate out dst
     */
    public List<cfgTrnsltn> trnsOutDst = new ArrayList<cfgTrnsltn>();

    private clntSip sip;

    public int compare(cfgDial o1, cfgDial o2) {
        return o1.name.toLowerCase().compareTo(o2.name.toLowerCase());
    }

    public String toString() {
        return "dial " + name;
    }

    /**
     * create new dail peer
     *
     * @param nam name of peer
     */
    public cfgDial(String nam) {
        name = "" + bits.str2num(nam);
    }

    private String stripAddr(String a) {
        int i = a.indexOf(";");
        if (i >= 0) {
            a = a.substring(0, i);
        }
        return uniResLoc.fromEmail(a);
    }

    /**
     * check if matches the call
     *
     * @param calling calling number
     * @param called called number
     * @return false if not, true if yes
     */
    public boolean matches(String calling, String called) {
        if (sip == null) {
            return false;
        }
        if (sip.isCalling()) {
            return false;
        }
        if ((direction & 2) == 0) {
            return false;
        }
        calling = stripAddr(calling);
        boolean ok = false;
        for (int i = 0; i < matSrc.size(); i++) {
            if (!calling.matches(matSrc.get(i))) {
                continue;
            }
            ok = true;
            break;
        }
        if (!ok) {
            return false;
        }
        called = stripAddr(called);
        ok = false;
        for (int i = 0; i < matDst.size(); i++) {
            if (!called.matches(matDst.get(i))) {
                continue;
            }
            ok = true;
            break;
        }
        return ok;
    }

    /**
     * got the call
     *
     * @param calling calling number
     * @param called called number
     * @return peer to use, null if none
     */
    public cfgDial incomeCall(String calling, String called) {
        if (sip == null) {
            return null;
        }
        if (sip.isCalling()) {
            return null;
        }
        if ((direction & 1) == 0) {
            return null;
        }
        calling = stripAddr(calling);
        called = stripAddr(called);
        if (log) {
            logger.info("incoming call " + called + " from " + calling);
        }
        return cfgAll.dialFind(calling, called, this);
    }

    /**
     * translate incoming src address
     *
     * @param adr address
     * @return translated
     */
    public String incomeSrc(String adr) {
        return cfgTrnsltn.doTranslate(trnsInSrc, stripAddr(adr));
    }

    /**
     * translate incoming trg address
     *
     * @param adr address
     * @return translated
     */
    public String incomeTrg(String adr) {
        return cfgTrnsltn.doTranslate(trnsInDst, stripAddr(adr));
    }

    /**
     * make the call
     *
     * @param calling calling number
     * @param called called number
     * @return false if ok, true if error
     */
    public boolean makeCall(String calling, String called) {
        if (sip == null) {
            return true;
        }
        calling = cfgTrnsltn.doTranslate(trnsOutSrc, stripAddr(calling));
        called = cfgTrnsltn.doTranslate(trnsOutDst, stripAddr(called));
        if (log) {
            logger.info("outgoing call " + called + " from " + calling);
        }
        return sip.makeCall(calling, called);
    }

    /**
     * stop the call
     */
    public void stopCall() {
        if (sip == null) {
            return;
        }
        sip.stopCall();
    }

    /**
     * get call
     *
     * @return rtp
     */
    public packRtp getCall() {
        if (sip == null) {
            return null;
        }
        return sip.getCall();
    }

    /**
     * get codec
     *
     * @return codec
     */
    public boolean getCodec() {
        return aLaw;
    }

    public List<String> getShRun(boolean filter) {
        List<String> l = new ArrayList<String>();
        l.add("dial-peer " + name);
        for (int i = 0; i < matSrc.size(); i++) {
            l.add(cmds.tabulator + "match-calling " + matSrc.get(i));
        }
        for (int i = 0; i < matDst.size(); i++) {
            l.add(cmds.tabulator + "match-called " + matDst.get(i));
        }
        for (int i = 0; i < trnsInSrc.size(); i++) {
            l.add(cmds.tabulator + "translate-in-calling " + trnsInSrc.get(i).name);
        }
        for (int i = 0; i < trnsInDst.size(); i++) {
            l.add(cmds.tabulator + "translate-in-called " + trnsInDst.get(i).name);
        }
        for (int i = 0; i < trnsOutSrc.size(); i++) {
            l.add(cmds.tabulator + "translate-out-calling " + trnsOutSrc.get(i).name);
        }
        for (int i = 0; i < trnsOutDst.size(); i++) {
            l.add(cmds.tabulator + "translate-out-called " + trnsOutDst.get(i).name);
        }
        String a;
        if (aLaw) {
            a = "alaw";
        } else {
            a = "ulaw";
        }
        l.add(cmds.tabulator + "codec " + a);
        l.add(cmds.tabulator + "port-local " + portLoc);
        l.add(cmds.tabulator + "port-remote " + portRem);
        l.add(cmds.tabulator + "keepalive " + keepalive);
        l.add(cmds.tabulator + "register " + register);
        l.add(cmds.tabulator + "subscribe " + subscribe);
        cmds.cfgLine(l, !log, cmds.tabulator, "log", "");
        if (vrf != null) {
            l.add(cmds.tabulator + "vrf " + vrf.name);
        } else {
            l.add(cmds.tabulator + "no vrf");
        }
        if (ifc != null) {
            l.add(cmds.tabulator + "source " + ifc.name);
        } else {
            l.add(cmds.tabulator + "no source");
        }
        cmds.cfgLine(l, endpt == null, cmds.tabulator, "myname", endpt);
        cmds.cfgLine(l, trg == null, cmds.tabulator, "target", trg);
        cmds.cfgLine(l, usr == null, cmds.tabulator, "username", usr);
        cmds.cfgLine(l, pwd == null, cmds.tabulator, "password", authLocal.passwdEncode(pwd));
        switch (direction) {
            case 0:
                a = "none";
                break;
            case 1:
                a = "in";
                break;
            case 2:
                a = "out";
                break;
            case 3:
                a = "both";
                break;
            default:
                a = "unknown=" + direction;
                break;
        }
        l.add(cmds.tabulator + "direction " + a);
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        if (!filter) {
            return l;
        }
        return userFilter.filterText(l, defaultF);
    }

    public userHelping getHelp() {
        userHelping l = userHelping.getGenCfg();
        l.add("1 2    match-calling           match calling string");
        l.add("2 2,.    <str>                 regular expression");
        l.add("1 2    match-called            match called string");
        l.add("2 2,.    <str>                 regular expression");
        l.add("1 2    translate-in-calling    translate incoming calling string");
        l.add("2 .      <name>                rule name");
        l.add("1 2    translate-in-called     translate incoming called string");
        l.add("2 .      <name>                rule name");
        l.add("1 2    translate-out-calling   translate outgoing calling string");
        l.add("2 .      <name>                rule name");
        l.add("1 2    translate-out-called    translate outgoing called string");
        l.add("2 .      <name>                rule name");
        l.add("1 2    vrf                     vrf to use");
        l.add("2 .      <name>                vrf name");
        l.add("1 2    source                  interface to use");
        l.add("2 .      <name>                interface name");
        l.add("1 2    target                  set peer name");
        l.add("2 2,.    <name>                domain name");
        l.add("1 2    myname                  set endpoint");
        l.add("2 2,.    <name>                username");
        l.add("1 2    username                set username");
        l.add("2 2,.    <name>                username");
        l.add("1 2    password                set password");
        l.add("2 2,.    <name>                password");
        l.add("1 2    direction               set peer direction");
        l.add("2 .      in                    inbound");
        l.add("2 .      out                   outbound");
        l.add("2 .      both                  in and out");
        l.add("2 .      none                  disabled");
        l.add("1 .    log                     log calls");
        l.add("1 2    keepalive               keepalive to peer");
        l.add("2 .      <num>                 time in ms");
        l.add("1 2    register                register to peer");
        l.add("2 .      <num>                 time in ms");
        l.add("1 2    subscribe               subscribe to peer");
        l.add("2 .      <num>                 time in ms");
        l.add("1 2    port-local              local port");
        l.add("2 .      <num>                 port number");
        l.add("1 2    port-remote             remote port");
        l.add("2 .      <num>                 port number");
        l.add("1 2    codec                   set codec to use");
        l.add("2 .      alaw                  g711 a law");
        l.add("2 .      ulaw                  g711 u law");
        return l;
    }

    public void doCfgStr(cmds cmd) {
        String a = cmd.word();
        boolean negated = a.equals("no");
        if (negated) {
            a = cmd.word();
        }
        if (a.equals("direction")) {
            a = cmd.word();
            if (negated) {
                direction = 0;
                doShutdown();
                return;
            }
            if (a.equals("in")) {
                direction = 1;
                doStartup();
                return;
            }
            if (a.equals("out")) {
                direction = 2;
                doStartup();
                return;
            }
            if (a.equals("both")) {
                direction = 3;
                doStartup();
                return;
            }
            if (a.equals("none")) {
                direction = 0;
                doShutdown();
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("match-calling")) {
            a = cmd.getRemaining();
            if (negated) {
                matSrc.remove(a);
            } else {
                matSrc.add(a);
            }
            return;
        }
        if (a.equals("match-called")) {
            a = cmd.getRemaining();
            if (negated) {
                matDst.remove(a);
            } else {
                matDst.add(a);
            }
            return;
        }
        if (a.equals("translate-in-calling")) {
            cfgTrnsltn rule = cfgAll.trnsltnFind(cmd.word(), false);
            if (rule == null) {
                cmd.error("no such rule");
                return;
            }
            if (negated) {
                trnsInSrc.remove(rule);
            } else {
                trnsInSrc.add(rule);
            }
            return;
        }
        if (a.equals("translate-in-called")) {
            cfgTrnsltn rule = cfgAll.trnsltnFind(cmd.word(), false);
            if (rule == null) {
                cmd.error("no such rule");
                return;
            }
            if (negated) {
                trnsInDst.remove(rule);
            } else {
                trnsInDst.add(rule);
            }
            return;
        }
        if (a.equals("translate-out-calling")) {
            cfgTrnsltn rule = cfgAll.trnsltnFind(cmd.word(), false);
            if (rule == null) {
                cmd.error("no such rule");
                return;
            }
            if (negated) {
                trnsOutSrc.remove(rule);
            } else {
                trnsOutSrc.add(rule);
            }
            return;
        }
        if (a.equals("translate-out-called")) {
            cfgTrnsltn rule = cfgAll.trnsltnFind(cmd.word(), false);
            if (rule == null) {
                cmd.error("no such rule");
                return;
            }
            if (negated) {
                trnsOutDst.remove(rule);
            } else {
                trnsOutDst.add(rule);
            }
            return;
        }
        if (a.equals("port-local")) {
            doShutdown();
            portLoc = bits.str2num(cmd.word());
            if (negated) {
                portLoc = 0;
            }
            doStartup();
            return;
        }
        if (a.equals("port-remote")) {
            doShutdown();
            portRem = bits.str2num(cmd.word());
            if (negated) {
                portRem = packSip.port;
            }
            doStartup();
            return;
        }
        if (a.equals("keepalive")) {
            doShutdown();
            keepalive = bits.str2num(cmd.word());
            if (negated) {
                keepalive = 0;
            }
            doStartup();
            return;
        }
        if (a.equals("register")) {
            doShutdown();
            register = bits.str2num(cmd.word());
            if (negated) {
                register = 0;
            }
            doStartup();
            return;
        }
        if (a.equals("subscribe")) {
            doShutdown();
            subscribe = bits.str2num(cmd.word());
            if (negated) {
                subscribe = 0;
            }
            doStartup();
            return;
        }
        if (a.equals("vrf")) {
            doShutdown();
            vrf = cfgAll.vrfFind(cmd.word(), false);
            if (negated) {
                vrf = null;
            }
            doStartup();
            return;
        }
        if (a.equals("source")) {
            doShutdown();
            ifc = cfgAll.ifcFind(cmd.word(), false);
            if (negated) {
                ifc = null;
            }
            doStartup();
            return;
        }
        if (a.equals("target")) {
            doShutdown();
            trg = cmd.getRemaining();
            if (negated) {
                trg = null;
            }
            doStartup();
            return;
        }
        if (a.equals("username")) {
            doShutdown();
            usr = cmd.getRemaining();
            if (negated) {
                usr = null;
            }
            doStartup();
            return;
        }
        if (a.equals("myname")) {
            doShutdown();
            endpt = cmd.getRemaining();
            if (negated) {
                endpt = null;
            }
            doStartup();
            return;
        }
        if (a.equals("password")) {
            doShutdown();
            pwd = authLocal.passwdDecode(cmd.getRemaining());
            if (negated) {
                pwd = null;
            }
            doStartup();
            return;
        }
        if (a.equals("log")) {
            log = !negated;
            return;
        }
        if (a.equals("codec")) {
            doShutdown();
            a = cmd.word();
            if (a.equals("alaw")) {
                aLaw = true;
            }
            if (a.equals("ulaw")) {
                aLaw = false;
            }
            if (negated) {
                aLaw = !aLaw;
            }
            doStartup();
            return;
        }
        cmd.badCmd();
        return;
    }

    public String getPrompt() {
        return "dial";
    }

    /**
     * stop work
     */
    public synchronized void doShutdown() {
        if (sip == null) {
            return;
        }
        sip.stopWork();
        sip = null;
    }

    /**
     * start work
     */
    public synchronized void doStartup() {
        doShutdown();
        if (direction < 1) {
            return;
        }
        if (vrf == null) {
            return;
        }
        if (trg == null) {
            return;
        }
        if (endpt == null) {
            return;
        }
        sip = new clntSip();
        sip.upper = this;
        sip.endpt = endpt;
        sip.portLoc = portLoc;
        sip.portRem = portRem;
        sip.aLaw = aLaw;
        sip.keepalive = keepalive;
        sip.register = register;
        sip.subscribe = subscribe;
        sip.vrf = vrf;
        sip.srcIfc = ifc;
        sip.trgDom = trg;
        if ((usr != null) && (pwd != null)) {
            sip.usr = usr;
            sip.pwd = pwd;
        }
        sip.startWork();
    }

}
