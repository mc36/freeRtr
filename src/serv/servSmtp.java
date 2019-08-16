package serv;

import addr.addrIP;
import auth.authGeneric;
import auth.authResult;
import cfg.cfgAceslst;
import cfg.cfgAll;
import cfg.cfgAuther;
import clnt.clntDns;
import clnt.clntSmtp;
import cry.cryBase64;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import pack.packDnsRec;
import pack.packText;
import pipe.pipeLine;
import pipe.pipeSide;
import prt.prtGenConn;
import prt.prtServS;
import tab.tabAceslstN;
import tab.tabGen;
import tab.tabListing;
import user.userFilter;
import user.userHelping;
import user.userTerminal;
import util.bits;
import util.cmds;
import util.debugger;
import util.logger;
import util.notifier;
import util.uniResLoc;

/**
 * simple mail transfer protocol (rfc821) server
 *
 * @author matecsaba
 */
public class servSmtp extends servGeneric implements prtServS {

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
     * access list to use
     */
    protected tabListing<tabAceslstN<addrIP>, addrIP> recursAcl;

    /**
     * authentication list
     */
    protected authGeneric recursAut;

    /**
     * list of email addresses
     */
    public tabGen<servSmtpAddr> emails = new tabGen<servSmtpAddr>();

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
    public final static String defaultL[] = {
        "server smtp .*! port " + port,
        "server smtp .*! protocol " + proto2string(protoAllStrm),
        "server smtp .*! no recursion access-class",
        "server smtp .*! no recursion authentication",
        "server smtp .*! no recursion enable",
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
        pipe.timeout = 120000;
        pipe.lineRx = pipeSide.modTyp.modeCRtryLF;
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        new servSmtpDoer(this, pipe, id);
        return false;
    }

    public void srvShRun(String beg, List<String> lst) {
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
        cmds.cfgLine(lst, !recursEna, beg, "recursion enable", "");
        lst.add(beg + "path " + mailFolders);
        for (int i = 0; i < emails.size(); i++) {
            lst.add(beg + "email " + emails.get(i));
        }
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
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
        if (s.equals("email")) {
            servSmtpAddr ntry = new servSmtpAddr();
            if (ntry.fromString(cmd)) {
                return true;
            }
            emails.put(ntry);
            return false;
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
        if (s.equals("email")) {
            servSmtpAddr ntry = new servSmtpAddr();
            if (ntry.fromString(cmd)) {
                return true;
            }
            return emails.del(ntry) == null;
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
        l.add("1 2  recursion                    recursive parameters");
        l.add("2 .    enable                     allow recursion");
        l.add("2 3    access-class               set access list");
        l.add("3 .      <name>                   port number to use");
        l.add("2 3    authentication             set authentication");
        l.add("3 .      <name>                   name of authentication list");
        l.add("1 2  email                        set email address");
        l.add("2 3    <user>                     name of user");
        l.add("3 .      <addr>                   email address");
        l.add("1 2  path                         set root folder");
        l.add("2 .    <path>                     name of root folder");
        l.add("1 2  rbl-server                   set rbl server");
        l.add("2 .    <path>                     name of server");
        l.add("1 2  rbl-threshold                set rbl threshold");
        l.add("2 .    <num>                      number of servers");
        l.add("1 2  rbl-timeout                  set rbl timeout");
        l.add("2 .    <num>                      number of servers");
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

}

class servSmtpAddr implements Comparator<servSmtpAddr> {

    public String email;

    public String user;

    public String toString() {
        return user + " " + email;
    }

    public boolean fromString(cmds cmd) {
        user = cmd.word();
        email = cmd.word();
        if (user.length() < 1) {
            return true;
        }
        if (email.length() < 1) {
            return true;
        }
        return false;
    }

    public int compare(servSmtpAddr o1, servSmtpAddr o2) {
        return o1.email.toLowerCase().compareTo(o2.email.toLowerCase());
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
        if (dns.doResolvList(cfgAll.nameServerAddr, serv.server, packDnsRec.typeNS)) {
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
        if (dns.doResolvOne(srvA, packDnsRec.generateReverse(addr, serv.server), packDnsRec.typeA)) {
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

    private tabGen<servSmtpAddr> trgL = new tabGen<servSmtpAddr>();

    private List<String> trgR = new ArrayList<String>();

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

    public servSmtpAddr findEmail(String s) {
        servSmtpAddr ntry = new servSmtpAddr();
        ntry.email = uniResLoc.fromEmail(s).toLowerCase();
        return lower.emails.find(ntry);
    }

    public void doLine(String s) {
        if (debugger.servSmtpTraf) {
            logger.debug("tx: " + s);
        }
        pipe.linePut(s);
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
            doLine("250 " + cfgAll.hostName + " hello [" + conn.peerAddr + " " + conn.portRem + "]");
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
            doLine("250-" + cfgAll.hostName + " hello [" + conn.peerAddr + " " + conn.portRem + "]");
            doLine("250-SIZE 10240000");
            doLine("250-PIPELINING");
            if (!lower.noneSecKeys()) {
                doLine("250-STARTTLS");
            }
            doLine("250 ok");
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
                s = "";
                if (i >= 0) {
                    s = a.substring(i + 1, a.length()).trim();
                    a = a.substring(0, i).trim();
                }
                a = a.toLowerCase();
                if (a.equals("from")) {
                    src = s;
                }
            }
            src = uniResLoc.fromEmail(src);
            doLine("250 " + src + " sender ok");
            return false;
        }
        if (a.equals("rcpt")) {
            servSmtpAddr ntry = null;
            String last = null;
            for (;;) {
                a = cmd.word();
                if (a.length() < 1) {
                    break;
                }
                int i = a.indexOf(":");
                s = "";
                if (i >= 0) {
                    s = a.substring(i + 1, a.length()).trim();
                    a = a.substring(0, i).trim();
                }
                a = a.toLowerCase();
                if (a.equals("to")) {
                    ntry = findEmail(s);
                    last = s;
                }
            }
            if (ntry == null) {
                last = uniResLoc.fromEmail(last);
                if (recurAva) {
                    trgR.add(last);
                    doLine("250 " + last + " will handled out");
                    return false;
                }
                doLine("550 " + last + " no such user");
                return false;
            }
            if (trgL.add(ntry) != null) {
                doLine("250 " + ntry.email + " already added");
                return false;
            }
            trgL.add(ntry);
            doLine("250 " + ntry.email + " now added");
            return false;
        }
        if (a.equals("rset")) {
            src = "";
            trgL.clear();
            trgR.clear();
            doLine("250 target list cleared");
            return false;
        }
        if (a.equals("data")) {
            if ((trgL.size() + trgR.size()) < 1) {
                doLine("503 target not specified");
                return false;
            }
            doLine("354 start mail input");
            packText pt = new packText(pipe);
            List<String> txt = pt.dottedRecvAll();
            txt.add(0, "Received: from " + conn.peerAddr + " (helo " + helo + ") by " + conn.iface.addr + " (helo " + cfgAll.hostName + ") (envelope-from " + src + ") with smtp at " + bits.time2str(cfgAll.timeZoneName, bits.getTime() + cfgAll.timeServerOffset, 3));
            int o = 0;
            for (int i = 0; i < trgL.size(); i++) {
                a = lower.mailFolders + trgL.get(i).user + "/" + bits.getTime() + ".msg";
                if (!bits.buf2txt(true, txt, a)) {
                    o++;
                }
            }
            int i = trgR.size();
            if (i > 0) {
                clntSmtp sm = new clntSmtp(null);
                sm.rcpt.addAll(trgR);
                sm.from = src;
                sm.putBody(txt);
                sm.startSend();
            }
            src = "";
            trgL.clear();
            trgR.clear();
            doLine("250 mail saved in " + o + " (local) + " + i + " (remote) mailbox(es)");
            return false;
        }
        if (a.equals("vrfy")) {
            servSmtpAddr ntry = findEmail(cmd.word());
            if (ntry == null) {
                doLine("550 no such user");
            } else {
                doLine("250 " + ntry.user + " <" + ntry.email + ">");
            }
            return false;
        }
        if (a.equals("expn")) {
            doLine("550 access denied");
            return false;
        }
        doLine("500 bad command");
        return false;
    }

    public void run() {
        try {
            doLine("220 server ready");
            if (lower.rbls.size() > 0) {
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
                rblRes = 0;
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
