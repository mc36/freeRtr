package net.freertr.serv;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.auth.authGeneric;
import net.freertr.auth.authResult;
import net.freertr.cfg.cfgAceslst;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgAuther;
import net.freertr.clnt.clntDns;
import net.freertr.clnt.clntSmtp;
import net.freertr.cry.cryBase64;
import net.freertr.pack.packDnsRec;
import net.freertr.pack.packText;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServS;
import net.freertr.tab.tabAceslstN;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabListing;
import net.freertr.user.userFilter;
import net.freertr.user.userFormat;
import net.freertr.user.userHelping;
import net.freertr.user.userTerminal;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.debugger;
import net.freertr.util.logger;
import net.freertr.util.notifier;
import net.freertr.util.uniResLoc;
import net.freertr.util.version;

/**
 * simple mail transfer protocol (rfc821) server
 *
 * @author matecsaba
 */
public class servSmtp extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servSmtp() {
    }

    /**
     * port number
     */
    public static final int port = 25;

    /**
     * mail folders
     */
    public String mailFolders = "/";

    /**
     * recursion available
     */
    public boolean recursEna = false;

    /**
     * notification available
     */
    public boolean dsnEna = false;

    /**
     * access list to use
     */
    protected tabListing<tabAceslstN<addrIP>, addrIP> recursAcl;

    /**
     * authentication list
     */
    protected authGeneric recursAut;

    /**
     * bcc user
     */
    protected String bccUser = null;

    /**
     * list of local email addresses
     */
    public tabGen<servSmtpLoc> locals = new tabGen<servSmtpLoc>();

    /**
     * list of forward email addresses
     */
    public tabGen<servSmtpFwd> forwards = new tabGen<servSmtpFwd>();

    /**
     * list of config list email addresses
     */
    public tabGen<servSmtpClst> cfgLists = new tabGen<servSmtpClst>();

    /**
     * list of external list email addresses
     */
    public tabGen<servSmtpElst> extLists = new tabGen<servSmtpElst>();

    /**
     * list of rbl servers
     */
    public tabGen<servSmtpRbl> rbls = new tabGen<servSmtpRbl>();

    /**
     * number of rbl servers
     */
    public int rblMin;

    /**
     * rbl servers timeout
     */
    public int rblTim = 5000;

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        "server smtp .*! port " + port,
        "server smtp .*! protocol " + proto2string(protoAllStrm),
        "server smtp .*! no recursion access-class",
        "server smtp .*! no recursion authentication",
        "server smtp .*! no recursion enable",
        "server smtp .*! no dsn",
        "server smtp .*! no bcc",
        "server smtp .*! rbl-threshold 0",
        "server smtp .*! rbl-timeout 5000"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.setTime(120000);
        pipe.lineRx = pipeSide.modTyp.modeCRtryLF;
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        new servSmtpDoer(this, pipe, id);
        return false;
    }

    public void srvShRun(String beg, List<String> lst, int filter) {
        lst.add(beg + "rbl-threshold " + rblMin);
        lst.add(beg + "rbl-timeout " + rblTim);
        for (int i = 0; i < rbls.size(); i++) {
            lst.add(beg + "rbl-server " + rbls.get(i));
        }
        if (recursAcl != null) {
            lst.add(beg + "recursion access-class " + recursAcl.listName);
        } else {
            lst.add(beg + "no recursion access-class");
        }
        if (recursAut == null) {
            lst.add(beg + "no recursion authentication");
        } else {
            lst.add(beg + "recursion authentication " + recursAut.autName);
        }
        cmds.cfgLine(lst, !dsnEna, beg, "dsn", "");
        cmds.cfgLine(lst, !recursEna, beg, "recursion enable", "");
        cmds.cfgLine(lst, bccUser == null, beg, "bcc", "" + bccUser);
        lst.add(beg + "path " + mailFolders);
        doGetCfg(locals, beg + "local ", lst);
        doGetCfg(forwards, beg + "forward ", lst);
        doGetCfg(cfgLists, beg + "clist ", lst);
        doGetCfg(extLists, beg + "elist ", lst);
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("dsn")) {
            dsnEna = true;
            return false;
        }
        if (s.equals("recursion")) {
            s = cmd.word();
            if (s.equals("enable")) {
                recursEna = true;
                return false;
            }
            if (s.equals("access-class")) {
                cfgAceslst ntry = cfgAll.aclsFind(cmd.word(), false);
                if (ntry == null) {
                    cmd.error("no such access list");
                    return false;
                }
                recursAcl = ntry.aceslst;
                return false;
            }
            if (s.equals("authentication")) {
                cfgAuther lst = cfgAll.autherFind(cmd.word(), null);
                if (lst == null) {
                    cmd.error("no such auth list");
                    return false;
                }
                recursAut = lst.getAuther();
                return false;
            }
            cmd.badCmd();
            return false;
        }
        if (s.equals("bcc")) {
            bccUser = cmd.word();
            return false;
        }
        if (s.equals("local")) {
            return doAddCfg(locals, cmd, new servSmtpLoc());
        }
        if (s.equals("forward")) {
            return doAddCfg(forwards, cmd, new servSmtpFwd());
        }
        if (s.equals("clist")) {
            return doAddCfg(cfgLists, cmd, new servSmtpClst());
        }
        if (s.equals("elist")) {
            return doAddCfg(extLists, cmd, new servSmtpElst());
        }
        if (s.equals("rbl-server")) {
            servSmtpRbl ntry = new servSmtpRbl();
            if (ntry.fromString(cmd)) {
                return true;
            }
            rbls.put(ntry);
            return false;
        }
        if (s.equals("rbl-threshold")) {
            rblMin = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("rbl-timeout")) {
            rblTim = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("path")) {
            mailFolders = "/" + uniResLoc.normalizePath(cmd.word() + "/");
            return false;
        }
        if (!s.equals("no")) {
            return true;
        }
        s = cmd.word();
        if (s.equals("dsn")) {
            dsnEna = false;
            return false;
        }
        if (s.equals("recursion")) {
            s = cmd.word();
            if (s.equals("enable")) {
                recursEna = false;
                return false;
            }
            if (s.equals("access-class")) {
                recursAcl = null;
                return false;
            }
            if (s.equals("authentication")) {
                recursAut = null;
                return false;
            }
            cmd.badCmd();
            return false;
        }
        if (s.equals("bcc")) {
            bccUser = null;
            return false;
        }
        if (s.equals("local")) {
            return doDelCfg(locals, cmd, new servSmtpLoc());
        }
        if (s.equals("forward")) {
            return doDelCfg(forwards, cmd, new servSmtpFwd());
        }
        if (s.equals("clist")) {
            return doDelCfg(cfgLists, cmd, new servSmtpClst());
        }
        if (s.equals("elist")) {
            return doDelCfg(extLists, cmd, new servSmtpElst());
        }
        if (s.equals("rbl-server")) {
            servSmtpRbl ntry = new servSmtpRbl();
            if (ntry.fromString(cmd)) {
                return true;
            }
            return rbls.del(ntry) == null;
        }
        if (s.equals("rbl-threshold")) {
            rblMin = 0;
            return false;
        }
        if (s.equals("path")) {
            mailFolders = "/";
            return false;
        }
        return true;
    }

    public void srvHelp(userHelping l) {
        l.add(null, "1 .  dsn                          allow delivery notification");
        l.add(null, "1 2  recursion                    recursive parameters");
        l.add(null, "2 .    enable                     allow recursion");
        l.add(null, "2 3    access-class               set access list");
        l.add(null, "3 .      <name:acl>               port number to use");
        l.add(null, "2 3    authentication             set authentication");
        l.add(null, "3 .      <name:aaa>               name of authentication list");
        l.add(null, "1 2  bcc                          set bcc user");
        l.add(null, "2 .    <user>                     name of user");
        l.add(null, "1 2  local                        set local email address");
        l.add(null, "2 3    <user>                     name of local user");
        l.add(null, "3 4,.    <addr>                   local email address");
        l.add(null, "4 .        <user>                 name of bcc user");
        l.add(null, "1 2  forward                      set forward email address");
        l.add(null, "2 3    <user>                     remote email address");
        l.add(null, "3 4,.    <addr>                   local email address");
        l.add(null, "4 .        <user>                 name of bcc user");
        l.add(null, "1 2  clist                        set config list email address");
        l.add(null, "2 3    <addr>                     local email address");
        l.add(null, "3 3,.    <addr>                   remote email address");
        l.add(null, "1 2  elist                        set external list email address");
        l.add(null, "2 3    <addr>                     local email address");
        l.add(null, "3 .      <path>                   file name to read");
        l.add(null, "1 2  path                         set root folder");
        l.add(null, "2 .    <path>                     name of root folder");
        l.add(null, "1 2  rbl-server                   set rbl server");
        l.add(null, "2 .    <path>                     name of server");
        l.add(null, "1 2  rbl-threshold                set rbl threshold");
        l.add(null, "2 .    <num>                      number of servers");
        l.add(null, "1 2  rbl-timeout                  set rbl timeout");
        l.add(null, "2 .    <num>                      number of servers");
    }

    public String srvName() {
        return "smtp";
    }

    public int srvPort() {
        return 25;
    }

    public int srvProto() {
        return protoAllStrm;
    }

    public boolean srvInit() {
        return genStrmStart(this, new pipeLine(32768, false), 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

    /**
     * find in one list
     *
     * @param <T> type
     * @param l list
     * @param n entry
     * @param e email
     * @return result
     */
    protected static <T extends servSmtpTrg> T doFinder(tabGen<T> l, T n, String e) {
        n.email = uniResLoc.fromEmail(e).toLowerCase();
        n = l.find(n);
        if (n == null) {
            return null;
        }
        n.askNum++;
        n.askTim = bits.getTime();
        return n;
    }

    /**
     * config for one list
     *
     * @param <T> type
     * @param l list
     * @param p prepend
     * @param r result
     */
    protected static <T extends servSmtpTrg> void doGetCfg(tabGen<T> l, String p, List<String> r) {
        for (int i = 0; i < l.size(); i++) {
            r.add(p + l.get(i));
        }
    }

    /**
     * config for one list
     *
     * @param <T> type
     * @param l list
     * @param c cmds
     * @param n entry
     * @return true on error, false on success
     */
    protected static <T extends servSmtpTrg> boolean doAddCfg(tabGen<T> l, cmds c, T n) {
        if (n.fromString(c)) {
            return true;
        }
        l.put(n);
        return false;
    }

    /**
     * config for one list
     *
     * @param <T> type
     * @param l list
     * @param c cmds
     * @param n entry
     * @return true on error, false on success
     */
    protected static <T extends servSmtpTrg> boolean doDelCfg(tabGen<T> l, cmds c, T n) {
        if (n.fromString(c)) {
            return true;
        }
        return l.del(n) == null;
    }

    /**
     * show for one list
     *
     * @param <T> type
     * @param l list
     * @param r result
     */
    protected static <T extends servSmtpTrg> void doGetShw(tabGen<T> l, userFormat r) {
        for (int i = 0; i < l.size(); i++) {
            T n = l.get(i);
            r.add(n.email + "|" + n.askNum + "|" + bits.timePast(n.askTim));
        }
    }

    /**
     * get show
     *
     * @return result
     */
    public userFormat getShow() {
        userFormat res = new userFormat("|", "email|hit|last");
        doGetShw(locals, res);
        doGetShw(forwards, res);
        doGetShw(cfgLists, res);
        doGetShw(extLists, res);
        return res;
    }

}

abstract class servSmtpTrg implements Comparator<servSmtpTrg> {

    public String email;

    public int askNum;

    public long askTim;

    public int compare(servSmtpTrg o1, servSmtpTrg o2) {
        return o1.email.toLowerCase().compareTo(o2.email.toLowerCase());
    }

    abstract public boolean fromString(cmds cmd);

}

class servSmtpElst extends servSmtpTrg {

    public String extFil;

    public String toString() {
        return email + " " + extFil;
    }

    public boolean fromString(cmds cmd) {
        email = cmd.word();
        extFil = cmd.word();
        if (extFil.length() < 1) {
            return true;
        }
        if (email.length() < 1) {
            return true;
        }
        return false;
    }

}

class servSmtpClst extends servSmtpTrg {

    public List<String> remotes;

    public String toString() {
        String a = "";
        for (int i = 0; i < remotes.size(); i++) {
            a += " " + remotes.get(i);
        }
        return email + a;
    }

    public boolean fromString(cmds cmd) {
        remotes = new ArrayList<String>();
        for (;;) {
            String a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            remotes.add(a);
        }
        if (remotes.size() < 2) {
            return true;
        }
        email = remotes.remove(0);
        return false;
    }

}

class servSmtpFwd extends servSmtpTrg {

    public String remote;

    public String bcc;

    public String toString() {
        return (remote + " " + email + " " + bcc).trim();
    }

    public boolean fromString(cmds cmd) {
        remote = cmd.word();
        email = cmd.word();
        bcc = cmd.word();
        if (remote.length() < 1) {
            return true;
        }
        if (email.length() < 1) {
            return true;
        }
        return false;
    }

}

class servSmtpLoc extends servSmtpTrg {

    public String user;

    public String bcc;

    public String toString() {
        return (user + " " + email + " " + bcc).trim();
    }

    public boolean fromString(cmds cmd) {
        user = cmd.word();
        email = cmd.word();
        bcc = cmd.word();
        if (user.length() < 1) {
            return true;
        }
        if (email.length() < 1) {
            return true;
        }
        return false;
    }

}

class servSmtpRbl implements Comparator<servSmtpRbl> {

    public String server;

    public String toString() {
        return server;
    }

    public boolean fromString(cmds cmd) {
        server = cmd.word();
        if (server.length() < 1) {
            return true;
        }
        return false;
    }

    public int compare(servSmtpRbl o1, servSmtpRbl o2) {
        return o1.server.toLowerCase().compareTo(o2.server.toLowerCase());
    }

}

class servSmtpRbler implements Runnable {

    private int[] result;

    private int number;

    private notifier notif;

    private servSmtpRbl serv;

    private addrIP addr;

    public servSmtpRbler(int[] res, int num, notifier noti, servSmtpRbl srv, addrIP adr) {
        result = res;
        number = num;
        notif = noti;
        serv = srv;
        addr = adr;
        new Thread(this).start();
    }

    private int doRound() {
        clntDns dns = new clntDns();
        if (dns.doResolvList(cfgAll.nameServerAddr, serv.server, true, packDnsRec.typeNS) != 0) {
            return 2;
        }
        String srvN = dns.getNS();
        if (srvN == null) {
            return 2;
        }
        addrIP srvA = userTerminal.justResolv(srvN, 0);
        if (srvA == null) {
            return 2;
        }
        dns = new clntDns();
        List<addrIP> srvL = new ArrayList<addrIP>();
        srvL.add(srvA);
        if (dns.doResolvList(srvL, packDnsRec.generateReverse(addr, serv.server), false, packDnsRec.typeA) == 1) {
            return 2;
        }
        if (dns.getAddr(4) != null) {
            return 1;
        } else {
            return 3;
        }
    }

    public void run() {
        int res = 2;
        try {
            res = doRound();
        } catch (Exception e) {
            logger.traceback(e);
        }
        result[number] = res;
        notif.wakeup();
    }

}

class servSmtpDoer implements Runnable {

    private servSmtp lower;

    private pipeSide pipe;

    private prtGenConn conn;

    private String helo = "";

    private String src = "";

    private boolean dsn = false;

    private String env = "";

    private tabGen<servSmtpLoc> trgL = new tabGen<servSmtpLoc>();

    private List<String> trgR = new ArrayList<String>();

    private List<String> hdrA = new ArrayList<String>();

    private List<String> hdrD = new ArrayList<String>();

    private String trgS = "";

    private boolean recurAva;

    private int rblRes = 0;

    public servSmtpDoer(servSmtp parent, pipeSide stream, prtGenConn id) {
        lower = parent;
        pipe = stream;
        conn = id;
        recurAva = lower.recursEna;
        if (lower.recursAcl != null) {
            recurAva &= lower.recursAcl.matches(conn);
        }
        new Thread(this).start();
    }

    public void doLine(String s) {
        if (debugger.servSmtpTraf) {
            logger.debug("tx: " + s);
        }
        pipe.linePut(s);
    }

    public void doClear() {
        src = "";
        dsn = false;
        env = "";
        trgS = "";
        trgL.clear();
        trgR.clear();
        hdrA.clear();
        hdrD.clear();
    }

    public void doLists(String trg) {
        hdrA.add("Reply-To: " + trg + "," + src);
        hdrD.add("reply-to");
        hdrA.add("Sender: " + trg);
        hdrD.add("sender");
        hdrA.add("List-Id: " + trg);
    }

    public boolean doOne() {
        String s = pipe.lineGet(1).trim();
        if (debugger.servSmtpTraf) {
            logger.debug("rx: " + s);
        }
        cmds cmd = new cmds("", s);
        String a = cmd.word().toLowerCase();
        if (a.length() < 1) {
            return pipe.isClosed() != 0;
        }
        if (a.equals("help")) {
            doLine("214-recursion=" + recurAva + ", rblscore=" + rblRes + ", supported commands:");
            doLine("214 helo ehlo noop rcpt_to data rset mail_from quit help vrfy auth_plain");
            return false;
        }
        if (a.equals("auth")) {
            if (lower.recursAut == null) {
                doLine("500 not allowed");
                return false;
            }
            a = cmd.word().toLowerCase();
            if (!a.equals("plain")) {
                doLine("500 bad method");
                return false;
            }
            doLine("334 send authentication data");
            s = pipe.lineGet(1).trim();
            if (debugger.servSmtpTraf) {
                logger.debug("auth: " + s);
            }
            byte[] buf1 = cryBase64.decodeBytes(s);
            int o = 0;
            for (int i = 1; i < buf1.length; i++) {
                if (buf1[i] == 0) {
                    o = i;
                }
            }
            byte[] bug2 = new byte[o - 1];
            bits.byteCopy(buf1, 1, bug2, 0, bug2.length);
            s = new String(bug2);
            bug2 = new byte[buf1.length - o - 1];
            bits.byteCopy(buf1, o + 1, bug2, 0, bug2.length);
            authResult res = lower.recursAut.authUserPass(s, new String(bug2));
            if (res == null) {
                doLine("500 error");
                return false;
            }
            recurAva = (res.result == authResult.authSuccessful) & lower.recursEna;
            if (recurAva) {
                s = "successful";
            } else {
                s = "failed";
            }
            doLine("235 authentication " + s);
            return false;
        }
        if (a.equals("quit")) {
            doLine("221 goodbye");
            return true;
        }
        if (a.equals("helo")) {
            helo = cmd.word();
            doLine("250 " + cfgAll.getFqdn() + " hello [" + conn.peerAddr + " " + conn.portRem + "]");
            return false;
        }
        if (a.equals("starttls")) {
            if (lower.noneSecKeys()) {
                doLine("454 not allowed");
                return false;
            }
            doLine("220 do it");
            pipeSide res = lower.negoSecSess(pipe, servGeneric.protoTls, new pipeLine(65536, false), null);
            if (res == null) {
                return true;
            }
            res.lineRx = pipeSide.modTyp.modeCRtryLF;
            res.lineTx = pipeSide.modTyp.modeCRLF;
            pipe = res;
            return false;
        }
        if (a.equals("ehlo")) {
            helo = cmd.word();
            doLine("250-" + cfgAll.getFqdn() + " hello [" + conn.peerAddr + " " + conn.portRem + "]");
            doLine("250-SIZE 10240000");
            if (lower.recursAut != null) {
                doLine("250-AUTH PLAIN");
            }
            if (!lower.noneSecKeys()) {
                doLine("250-STARTTLS");
            }
            if (lower.dsnEna) {
                doLine("250-DSN");
            }
            doLine("250 PIPELINING");
            return false;
        }
        if (a.equals("noop")) {
            doLine("250 no operation done");
            return false;
        }
        if (rblRes > lower.rblMin) {
            doLine("550 you are blacklisted");
            return false;
        }
        if (a.equals("mail")) {
            for (;;) {
                a = cmd.word();
                if (a.length() < 1) {
                    break;
                }
                int i = a.indexOf(":");
                int o = a.indexOf("=");
                if (i < 0) {
                    i = o;
                }
                if (o < 0) {
                    o = i;
                }
                if (o < i) {
                    i = o;
                }
                s = "";
                if (i >= 0) {
                    s = a.substring(i + 1, a.length()).trim();
                    a = a.substring(0, i).trim();
                }
                a = a.toLowerCase();
                if (a.equals("from")) {
                    src = s;
                    continue;
                }
                if (a.equals("envid")) {
                    env = s;
                    continue;
                }
            }
            src = uniResLoc.fromEmail(src);
            doLine("250 " + src + " sender ok");
            return false;
        }
        if (a.equals("rcpt")) {
            String last = "";
            for (;;) {
                a = cmd.word();
                if (a.length() < 1) {
                    break;
                }
                int i = a.indexOf(":");
                int o = a.indexOf("=");
                if (i < 0) {
                    i = o;
                }
                if (o < 0) {
                    o = i;
                }
                if (o < i) {
                    i = o;
                }
                s = "";
                if (i >= 0) {
                    s = a.substring(i + 1, a.length()).trim();
                    a = a.substring(0, i).trim();
                }
                a = a.toLowerCase();
                if (a.equals("to")) {
                    last = s;
                    continue;
                }
                if (a.equals("notify")) {
                    dsn |= !s.toLowerCase().equals("never");
                    continue;
                }
            }
            last = uniResLoc.fromEmail(last);
            servSmtpLoc loc = servSmtp.doFinder(lower.locals, new servSmtpLoc(), last);
            if (loc != null) {
                trgS += last + " ";
                if (trgL.add(loc) != null) {
                    doLine("250 " + loc.email + " already added");
                    return false;
                }
                doLine("250 " + loc.email + " now added");
                return false;
            }
            servSmtpFwd fwd = servSmtp.doFinder(lower.forwards, new servSmtpFwd(), last);
            if (fwd != null) {
                trgS += last + " ";
                trgR.add(fwd.remote);
                doLine("250 " + fwd.email + " now added");
                if (fwd.bcc.length() < 1) {
                    return false;
                }
                loc = new servSmtpLoc();
                loc.email = fwd.email;
                loc.user = fwd.bcc;
                loc.bcc = "";
                trgL.add(loc);
                return false;
            }
            servSmtpClst clst = servSmtp.doFinder(lower.cfgLists, new servSmtpClst(), last);
            if (clst != null) {
                trgS += last + " ";
                trgR.addAll(clst.remotes);
                doLists(last);
                doLine("250 " + clst.email + " now added");
                return false;
            }
            servSmtpElst elst = servSmtp.doFinder(lower.extLists, new servSmtpElst(), last);
            if (elst != null) {
                List<String> res = bits.txt2buf(elst.extFil);
                if (res == null) {
                    doLine("550 " + last + " was not found");
                    return false;
                }
                trgS += last + " ";
                trgR.addAll(res);
                doLists(last);
                doLine("250 " + elst.email + " now added");
                return false;
            }
            if (recurAva) {
                trgS += last + " ";
                trgR.add(last);
                doLine("250 " + last + " will handled out");
                return false;
            }
            doLine("550 " + last + " no such user");
            return false;
        }
        if (a.equals("rset")) {
            doClear();
            doLine("250 target list cleared");
            return false;
        }
        if (a.equals("data")) {
            dsn &= lower.dsnEna;
            if ((trgL.size() + trgR.size()) < 1) {
                doLine("503 target not specified");
                return false;
            }
            doLine("354 start mail input");
            packText pt = new packText(pipe);
            List<String> txt = pt.dottedRecvAll();
            clntSmtp.deleteHead(txt, hdrD);
            clntSmtp.prependHead(txt, hdrA);
            hdrA.clear();
            long tim = bits.getTime();
            hdrA.add("Received: from " + conn.peerAddr + " (helo " + helo + ")");
            hdrA.add("    by " + conn.iface.addr + " (helo " + cfgAll.getFqdn() + ")");
            hdrA.add("    (envelope-from " + src + ") with smtp (" + version.namVer + ")");
            hdrA.add("    for " + trgS + "; " + bits.time2str(cfgAll.timeZoneName, tim + cfgAll.timeServerOffset, 4));
            clntSmtp.prependHead(txt, hdrA);
            int o = 0;
            for (int i = 0; i < trgL.size(); i++) {
                servSmtpLoc usr = trgL.get(i);
                if (!bits.buf2txt(true, txt, lower.mailFolders + usr.user + "/" + tim + ".msg")) {
                    o++;
                }
                if (usr.bcc.length() < 1) {
                    continue;
                }
                bits.buf2txt(true, txt, lower.mailFolders + usr.bcc + "/" + tim + ".msg");
            }
            if (lower.bccUser != null) {
                bits.buf2txt(true, txt, lower.mailFolders + lower.bccUser + "/" + tim + ".msg");
            }
            for (int i = 0; i < trgR.size(); i++) {
                clntSmtp sm = new clntSmtp(null);
                sm.rcpt = trgR.get(i);
                sm.from = src;
                sm.notify = dsn;
                sm.envid = env;
                sm.putBody(txt);
                sm.startSend();
            }
            doLine("250 mail saved in " + o + " local and " + trgR.size() + " remote mailboxes");
            if (!dsn) {
                doClear();
                return false;
            }
            clntSmtp sm = new clntSmtp(null);
            sm.from = src;
            sm.envid = env;
            sm.putBody(txt);
            if (!sm.conv2rep()) {
                sm.startSend();
            }
            doClear();
            return false;
        }
        if (a.equals("vrfy")) {
            String last = uniResLoc.fromEmail(cmd.word());
            servSmtpTrg trg = servSmtp.doFinder(lower.locals, new servSmtpLoc(), last);
            if (trg != null) {
                doLine("250 <" + trg.email + ">");
                return false;
            }
            trg = servSmtp.doFinder(lower.forwards, new servSmtpFwd(), last);
            if (trg != null) {
                doLine("250 <" + trg.email + ">");
                return false;
            }
            trg = servSmtp.doFinder(lower.cfgLists, new servSmtpClst(), last);
            if (trg != null) {
                doLine("250 <" + trg.email + ">");
                return false;
            }
            trg = servSmtp.doFinder(lower.extLists, new servSmtpElst(), last);
            if (trg != null) {
                doLine("250 <" + trg.email + ">");
                return false;
            }
            doLine("550 no such user");
            return false;
        }
        if (a.equals("expn")) {
            doLine("550 access denied");
            return false;
        }
        doLine("500 bad command");
        return false;
    }

    public void doRbls() {
        rblRes = 0;
        if (lower.rbls.size() < 1) {
            return;
        }
        int[] res = new int[lower.rbls.size()];
        notifier notif = new notifier();
        for (int i = 0; i < res.length; i++) {
            new servSmtpRbler(res, i, notif, lower.rbls.get(i), conn.peerAddr);
        }
        long tim = bits.getTime();
        for (;;) {
            notif.sleep(1000);
            if ((bits.getTime() - tim) > lower.rblTim) {
                break;
            }
            int o = 0;
            for (int i = 0; i < res.length; i++) {
                if (res[i] != 0) {
                    o++;
                }
            }
            if (o >= res.length) {
                break;
            }
        }
        for (int i = 0; i < res.length; i++) {
            switch (res[i]) {
                case 1:
                    rblRes++;
                    break;
                case 0:
                case 2:
                    logger.error("rbl " + lower.rbls.get(i) + " not responding");
                    break;
            }
        }
    }

    public void run() {
        try {
            doLine("220 server ready");
            doRbls();
            doClear();
            for (;;) {
                if (doOne()) {
                    break;
                }
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        pipe.setClose();
    }

}
