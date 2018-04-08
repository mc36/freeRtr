package cfg;

import auth.authLocal;
import clnt.clntSip;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import pack.packRtp;
import pack.packSip;
import snd.sndCodec;
import snd.sndCodecG711aLaw;
import snd.sndCodecG711uLaw;
import tab.tabGen;
import user.userFilter;
import user.userFormat;
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
     * description of this dialpeer
     */
    public String description = null;

    /**
     * skip these dialpeers inbound
     */
    public String skipPeersIn = null;

    /**
     * skip these dialpeers inbound
     */
    public String skipPeersOut = null;

    /**
     * allow these dialpeers inbound
     */
    public String allowPeersIn = null;

    /**
     * allow these dialpeers inbound
     */
    public String allowPeersOut = null;

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "dial-peer .*! no description",
        "dial-peer .*! no skip-peers-in",
        "dial-peer .*! no skip-peers-out",
        "dial-peer .*! no allow-peers-in",
        "dial-peer .*! no allow-peers-out",
        "dial-peer .*! codec alaw",
        "dial-peer .*! no vrf",
        "dial-peer .*! no source",
        "dial-peer .*! no target",
        "dial-peer .*! no username",
        "dial-peer .*! no password",
        "dial-peer .*! no myname",
        "dial-peer .*! no log",
        "dial-peer .*! history 100",
        "dial-peer .*! keepalive 0",
        "dial-peer .*! max-calls-in 1",
        "dial-peer .*! max-calls-out 1",
        "dial-peer .*! register 0",
        "dial-peer .*! subscribe 0",
        "dial-peer .*! options 0",
        "dial-peer .*! port-local 0",
        "dial-peer .*! port-remote " + packSip.port,
        "dial-peer .*! protocol sip-udp",
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
     * protocol: 1=sip-udp, 2=sip-listen, 3=sip-connect
     */
    public int protocol = 1;

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
     * options interval
     */
    public int options = 0;

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

    /**
     * prematch translate src
     */
    public List<cfgTrnsltn> prmtSrc = new ArrayList<cfgTrnsltn>();

    /**
     * prematch translate dst
     */
    public List<cfgTrnsltn> prmtDst = new ArrayList<cfgTrnsltn>();

    /**
     * maximum in calls
     */
    public int maxCallsIn = 1;

    /**
     * maximum out calls
     */
    public int maxCallsOut = 1;

    /**
     * seen in calls
     */
    public int seenIn;

    /**
     * failed in calls
     */
    public int failIn;

    /**
     * seen out calls
     */
    public int seenOut;

    /**
     * failed out calls
     */
    public int failOut;

    /**
     * seen in msgs
     */
    public int seenMsgIn;

    /**
     * failed in msgs
     */
    public int failMsgIn;

    /**
     * seen out msgs
     */
    public int seenMsgOut;

    /**
     * failed out msgs
     */
    public int failMsgOut;

    /**
     * history data
     */
    public List<String> histDat = new ArrayList<String>();

    /**
     * history size
     */
    public int histMax = 100;

    /**
     * total time
     */
    public long seenTime;

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

    private void doLog(String s) {
        if (log) {
            logger.info(s);
        }
        histDat.add(bits.time2str(cfgAll.timeZoneName, bits.getTime() + cfgAll.timeServerOffset, 3) + " " + s);
        for (; histDat.size() > histMax;) {
            histDat.remove(0);
        }
    }

    /**
     * get direction
     *
     * @return direction
     */
    public String getDir() {
        switch (direction) {
            case 0:
                return "none";
            case 1:
                return "in";
            case 2:
                return "out";
            case 3:
                return "both";
            default:
                return "unknown=" + direction;
        }
    }

    /**
     * get statistics
     *
     * @param call true=calls, false=messages
     * @return statistics
     */
    public String getStats(boolean call) {
        String a = name + "|";
        if (call) {
            a += seenIn + "|" + seenOut + "|" + bits.timeDump(seenTime / 1000) + "|" + failIn + "|" + failOut + "|";
        } else {
            a += seenMsgIn + "|" + seenMsgOut + "|n/a|" + failMsgIn + "|" + failMsgOut + "|";
        }
        if (sip == null) {
            return a + "n/a|n/a";
        }
        if (call) {
            return a + sip.numCallsIn() + "|" + sip.numCallsOut();
        } else {
            return a + "n/a|" + sip.numMsgsOut();
        }
    }

    /**
     * get call list
     *
     * @param dir direction, true=in, false=out
     * @return list
     */
    public userFormat getCalls(boolean dir) {
        userFormat l = new userFormat("|", "id|calling|called|time");
        if (sip == null) {
            return l;
        }
        l.add(sip.listCalls(dir));
        return l;
    }

    /**
     * get call history
     *
     * @return list
     */
    public List<String> getHist() {
        return histDat;
    }

    private String stripAddr(String a) {
        a = uniResLoc.fromEmail(a);
        int i = a.indexOf(";");
        if (i >= 0) {
            return a.substring(0, i).trim();
        } else {
            return a.trim();
        }
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
        if ((direction & 2) == 0) {
            return false;
        }
        if (!sip.isReady()) {
            return false;
        }
        if (sip.numCallsOut() >= maxCallsOut) {
            return false;
        }
        calling = stripAddr(calling);
        called = stripAddr(called);
        calling = cfgTrnsltn.doTranslate(prmtSrc, calling);
        called = cfgTrnsltn.doTranslate(prmtDst, called);
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
     * got the msg
     *
     * @param calling calling number
     * @param called called number
     * @return peer to use, null if none
     */
    public cfgDial incomeMsg(String calling, String called) {
        seenMsgIn++;
        if (sip == null) {
            failMsgIn++;
            return null;
        }
        if ((direction & 1) == 0) {
            failMsgIn++;
            return null;
        }
        calling = stripAddr(calling);
        called = stripAddr(called);
        doLog("incoming msg " + called + " from " + calling + " started");
        cfgDial res = cfgAll.dialFind(calling, called, this);
        if (res == null) {
            failMsgIn++;
            return null;
        }
        return res;
    }

    /**
     * got the call
     *
     * @param calling calling number
     * @param called called number
     * @return peer to use, null if none
     */
    public cfgDial incomeCall(String calling, String called) {
        seenIn++;
        if (sip == null) {
            failIn++;
            return null;
        }
        if ((direction & 1) == 0) {
            failIn++;
            return null;
        }
        if (sip.numCallsIn() > maxCallsIn) {
            failIn++;
            return null;
        }
        calling = stripAddr(calling);
        called = stripAddr(called);
        doLog("incoming call " + called + " from " + calling + " started");
        cfgDial res = cfgAll.dialFind(calling, called, this);
        if (res == null) {
            failIn++;
            return null;
        }
        return res;
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
     * send msg
     *
     * @param calling calling number
     * @param called called number
     * @param msg message
     * @return false on success, true on error
     */
    public boolean sendMsg(String calling, String called, List<String> msg) {
        seenMsgOut++;
        if (sip == null) {
            failMsgOut++;
            return true;
        }
        calling = stripAddr(calling);
        called = stripAddr(called);
        calling = cfgTrnsltn.doTranslate(prmtSrc, calling);
        called = cfgTrnsltn.doTranslate(prmtDst, called);
        calling = cfgTrnsltn.doTranslate(trnsOutSrc, calling);
        called = cfgTrnsltn.doTranslate(trnsOutDst, called);
        doLog("outgoing msg " + called + " from " + calling + " started");
        boolean res = sip.sendMsg(calling, called, msg);
        if (res) {
            failMsgOut++;
            return true;
        }
        return res;
    }

    /**
     * make the call
     *
     * @param calling calling number
     * @param called called number
     * @return call id, null if error
     */
    public String makeCall(String calling, String called) {
        seenOut++;
        if (sip == null) {
            failOut++;
            return null;
        }
        calling = stripAddr(calling);
        called = stripAddr(called);
        calling = cfgTrnsltn.doTranslate(prmtSrc, calling);
        called = cfgTrnsltn.doTranslate(prmtDst, called);
        calling = cfgTrnsltn.doTranslate(trnsOutSrc, calling);
        called = cfgTrnsltn.doTranslate(trnsOutDst, called);
        doLog("outgoing call " + called + " from " + calling + " started");
        String res = sip.makeCall(calling, called);
        if (res == null) {
            failOut++;
            return null;
        }
        return res;
    }

    /**
     * noticed call end
     *
     * @param dir direction, true=out, false=in
     * @param calling calling number
     * @param called called number
     * @param time start time
     */
    public void stoppedCall(boolean dir, String calling, String called, long time) {
        doLog((dir ? "outgoing" : "incoming") + " call " + called + " from " + calling + " ended after " + bits.timePast(time));
        seenTime += bits.getTime() - time;
    }

    /**
     * stop the call
     *
     * @param cid call id
     */
    public void stopCall(String cid) {
        if (sip == null) {
            return;
        }
        sip.stopCall(cid);
    }

    /**
     * get call
     *
     * @param cid call id
     * @return rtp
     */
    public packRtp getCall(String cid) {
        if (sip == null) {
            return null;
        }
        return sip.getCall(cid);
    }

    /**
     * get codec
     *
     * @return codec
     */
    public sndCodec getCodec() {
        if (aLaw) {
            return new sndCodecG711aLaw();
        } else {
            return new sndCodecG711uLaw();
        }
    }

    public List<String> getShRun(boolean filter) {
        List<String> l = new ArrayList<String>();
        l.add("dial-peer " + name);
        cmds.cfgLine(l, description == null, cmds.tabulator, "description", description);
        cmds.cfgLine(l, skipPeersIn == null, cmds.tabulator, "skip-peers-in", skipPeersIn);
        cmds.cfgLine(l, skipPeersOut == null, cmds.tabulator, "skip-peers-out", skipPeersOut);
        cmds.cfgLine(l, allowPeersIn == null, cmds.tabulator, "allow-peers-in", allowPeersIn);
        cmds.cfgLine(l, allowPeersOut == null, cmds.tabulator, "allow-peers-out", allowPeersOut);
        for (int i = 0; i < prmtSrc.size(); i++) {
            l.add(cmds.tabulator + "prematch-calling " + prmtSrc.get(i).name);
        }
        for (int i = 0; i < prmtDst.size(); i++) {
            l.add(cmds.tabulator + "prematch-called " + prmtDst.get(i).name);
        }
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
        l.add(cmds.tabulator + "options " + options);
        cmds.cfgLine(l, !log, cmds.tabulator, "log", "");
        l.add(cmds.tabulator + "history " + histMax);
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
        cmds.cfgLine(l, usr == null, cmds.tabulator, "username", usr);
        cmds.cfgLine(l, pwd == null, cmds.tabulator, "password", authLocal.passwdEncode(pwd));
        cmds.cfgLine(l, trg == null, cmds.tabulator, "target", trg);
        l.add(cmds.tabulator + "max-calls-in " + maxCallsIn);
        l.add(cmds.tabulator + "max-calls-out " + maxCallsOut);
        switch (protocol) {
            case 1:
                a = "sip-udp";
                break;
            case 2:
                a = "sip-listen";
                break;
            case 3:
                a = "sip-connect";
                break;
            default:
                a = "unknown=" + protocol;
                break;
        }
        l.add(cmds.tabulator + "protocol " + a);
        l.add(cmds.tabulator + "direction " + getDir());
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        if (!filter) {
            return l;
        }
        return userFilter.filterText(l, defaultF);
    }

    public userHelping getHelp() {
        userHelping l = userHelping.getGenCfg();
        l.add("1 2    description             specify description");
        l.add("2 2,.    <str>                 description");
        l.add("1 2    skip-peers-in           skip dial peers incoming");
        l.add("2 .      <str>                 comma separated peers");
        l.add("1 2    skip-peers-out          skip dial peers outgoing");
        l.add("2 .      <str>                 comma separated peers");
        l.add("1 2    allow-peers-in          allow dial peers incoming");
        l.add("2 .      <str>                 comma separated peers");
        l.add("1 2    allow-peers-out         allow dial peers outgoing");
        l.add("2 .      <str>                 comma separated peers");
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
        l.add("1 2    prematch-calling        prematch translate outgoing calling string");
        l.add("2 .      <name>                rule name");
        l.add("1 2    prematch-called         prematch translate outgoing called string");
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
        l.add("1 2    protocol                set peer protocol");
        l.add("2 .      sip-udp               sip over udp");
        l.add("2 .      sip-listen            sip over tcp as server");
        l.add("2 .      sip-connect           sip over tcp as client");
        l.add("1 2    direction               set peer direction");
        l.add("2 .      in                    inbound");
        l.add("2 .      out                   outbound");
        l.add("2 .      both                  in and out");
        l.add("2 .      none                  disabled");
        l.add("1 .    log                     log calls");
        l.add("1 2    max-calls-in            maximum in calls allowed");
        l.add("2 .      <num>                 limit");
        l.add("1 2    max-calls-out           maximum out calls allowed");
        l.add("2 .      <num>                 limit");
        l.add("1 2    history                 history size");
        l.add("2 .      <num>                 limit");
        l.add("1 2    keepalive               keepalive to peer");
        l.add("2 .      <num>                 time in ms");
        l.add("1 2    register                register to peer");
        l.add("2 .      <num>                 time in ms");
        l.add("1 2    subscribe               subscribe to peer");
        l.add("2 .      <num>                 time in ms");
        l.add("1 2    options                 options to peer");
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
        if (a.equals("description")) {
            description = cmd.getRemaining();
            if (negated) {
                description = null;
            }
            return;
        }
        if (a.equals("skip-peers-in")) {
            skipPeersIn = cmd.getRemaining();
            if (negated) {
                skipPeersIn = null;
            }
            return;
        }
        if (a.equals("skip-peers-out")) {
            skipPeersOut = cmd.getRemaining();
            if (negated) {
                skipPeersOut = null;
            }
            return;
        }
        if (a.equals("allow-peers-in")) {
            allowPeersIn = cmd.getRemaining();
            if (negated) {
                allowPeersIn = null;
            }
            return;
        }
        if (a.equals("allow-peers-out")) {
            allowPeersOut = cmd.getRemaining();
            if (negated) {
                allowPeersOut = null;
            }
            return;
        }
        if (a.equals("protocol")) {
            a = cmd.word();
            if (negated) {
                protocol = 1;
                doStartup();
                return;
            }
            if (a.equals("sip-udp")) {
                protocol = 1;
                doStartup();
                return;
            }
            if (a.equals("sip-listen")) {
                protocol = 2;
                doStartup();
                return;
            }
            if (a.equals("sip-connect")) {
                protocol = 3;
                doStartup();
                return;
            }
            cmd.badCmd();
            return;
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
        if (a.equals("prematch-calling")) {
            cfgTrnsltn rule = cfgAll.trnsltnFind(cmd.word(), false);
            if (rule == null) {
                cmd.error("no such rule");
                return;
            }
            if (negated) {
                prmtSrc.remove(rule);
            } else {
                prmtSrc.add(rule);
            }
            return;
        }
        if (a.equals("prematch-called")) {
            cfgTrnsltn rule = cfgAll.trnsltnFind(cmd.word(), false);
            if (rule == null) {
                cmd.error("no such rule");
                return;
            }
            if (negated) {
                prmtDst.remove(rule);
            } else {
                prmtDst.add(rule);
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
        if (a.equals("max-calls-in")) {
            maxCallsIn = bits.str2num(cmd.word());
            if (negated) {
                maxCallsIn = 1;
            }
            return;
        }
        if (a.equals("max-calls-out")) {
            maxCallsOut = bits.str2num(cmd.word());
            if (negated) {
                maxCallsOut = 1;
            }
            return;
        }
        if (a.equals("history")) {
            histMax = bits.str2num(cmd.word());
            if (negated) {
                histMax = 100;
            }
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
        if (a.equals("options")) {
            doShutdown();
            options = bits.str2num(cmd.word());
            if (negated) {
                options = 0;
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
        sip.protocol = protocol;
        sip.upper = this;
        sip.endpt = endpt;
        sip.portLoc = portLoc;
        sip.portRem = portRem;
        sip.aLaw = aLaw;
        sip.keepalive = keepalive;
        sip.register = register;
        sip.subscribe = subscribe;
        sip.options = options;
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
