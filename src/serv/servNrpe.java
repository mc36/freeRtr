package serv;

import addr.addrIP;
import cfg.cfgAll;
import clnt.clntDns;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import pack.packDnsRec;
import pack.packNrpe;
import pipe.pipeLine;
import pipe.pipeSide;
import prt.prtGenConn;
import prt.prtServS;
import tab.tabGen;
import user.userExec;
import user.userFilter;
import user.userFormat;
import user.userHelping;
import user.userReader;
import util.bits;
import util.cmds;
import util.debugger;
import util.logger;

/**
 * nagios remote plugin server
 *
 * @author matecsaba
 */
public class servNrpe extends servGeneric implements prtServS {

    /**
     * list of checks
     */
    public tabGen<servNrpeCheck> chks = new tabGen<servNrpeCheck>();

    /**
     * list of resolvers
     */
    public tabGen<servNrpeRes> ress = new tabGen<servNrpeRes>();

    /**
     * list of replacers
     */
    public tabGen<servNrpeRep> reps = new tabGen<servNrpeRep>();

    /**
     * send out command after error text
     */
    public boolean sendCmds = false;

    /**
     * send out my name after error text
     */
    public boolean sendMyId = false;

    /**
     * don't send out error/ok
     */
    public boolean noState = false;

    /**
     * truncate first line
     */
    public int truncState = 0;

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "server nrpe .*! port " + packNrpe.portNum,
        "server nrpe .*! protocol " + proto2string(protoAllStrm),
        "server nrpe .*! error-truncate 0",
        "server nrpe .*! no error-states",
        "server nrpe .*! no error-commands",
        "server nrpe .*! no error-hostname",
        "server nrpe .*! no check .* description",
        "server nrpe .*! no check .* error",
        "server nrpe .*! no check .* alternate",};

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public String srvName() {
        return "nrpe";
    }

    public int srvPort() {
        return packNrpe.portNum;
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

    public void srvShRun(String beg, List<String> lst) {
        cmds.cfgLine(lst, !sendCmds, beg, "error-commands", "");
        cmds.cfgLine(lst, !sendMyId, beg, "error-hostname", "");
        cmds.cfgLine(lst, !noState, beg, "error-states", "");
        lst.add(beg + "error-truncate " + truncState);
        for (int i = 0; i < ress.size(); i++) {
            lst.add(beg + "resolve " + ress.get(i));
        }
        for (int i = 0; i < reps.size(); i++) {
            lst.add(beg + "replace " + reps.get(i));
        }
        for (int o = 0; o < chks.size(); o++) {
            servNrpeCheck ntry = chks.get(o);
            if (ntry == null) {
                continue;
            }
            String cn = "check " + ntry.nam;
            lst.add(beg + cn + " command " + ntry.cmd);
            cmds.cfgLine(lst, ntry.dsc == null, beg, cn + " description", ntry.dsc);
            cmds.cfgLine(lst, ntry.err == null, beg, cn + " error", ntry.err);
            cmds.cfgLine(lst, !ntry.alternate, beg, cn + " alternate", "");
            for (int i = 0; i < ntry.ignT.size(); i++) {
                lst.add(beg + cn + " ign-txt " + ntry.ignT.get(i));
            }
            for (int i = 0; i < ntry.ignR.size(); i++) {
                lst.add(beg + cn + " ign-reg " + ntry.ignR.get(i));
            }
            for (int i = 0; i < ntry.reqT.size(); i++) {
                lst.add(beg + cn + " req-txt " + ntry.reqT.get(i));
            }
            for (int i = 0; i < ntry.reqR.size(); i++) {
                lst.add(beg + cn + " req-reg " + ntry.reqR.get(i));
            }
        }
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        boolean negated = s.equals("no");
        if (negated) {
            s = cmd.word();
        }
        if (s.equals("error-commands")) {
            sendCmds = !negated;
            return false;
        }
        if (s.equals("error-hostname")) {
            sendMyId = !negated;
            return false;
        }
        if (s.equals("error-states")) {
            noState = !negated;
            return false;
        }
        if (s.equals("error-truncate")) {
            truncState = bits.str2num(cmd.word());
            if (negated) {
                truncState = 0;
            }
            return false;
        }
        if (s.equals("resolve")) {
            String a = cmd.word();
            s = cmd.getRemaining();
            if (negated) {
                ress.del(new servNrpeRes(a, s));
            } else {
                ress.add(new servNrpeRes(a, s));
            }
            return false;
        }
        if (s.equals("replace")) {
            String a = cmd.word();
            String b = cmd.word();
            s = cmd.getRemaining();
            if (negated) {
                reps.del(new servNrpeRep(a, b, s));
            } else {
                reps.add(new servNrpeRep(a, b, s));
            }
            return false;
        }
        if (!s.equals("check")) {
            return true;
        }
        servNrpeCheck ntry = new servNrpeCheck(this, cmd.word());
        servNrpeCheck old = chks.add(ntry);
        if (old != null) {
            ntry = old;
        }
        s = cmd.word();
        if (s.equals("command")) {
            if (negated) {
                chks.del(ntry);
                return false;
            }
            ntry.cmd = cmd.getRemaining();
            return false;
        }
        if (ntry.cmd == null) {
            chks.del(ntry);
            cmd.error("no such check");
            return false;
        }
        if (s.equals("description")) {
            ntry.dsc = cmd.getRemaining();
            if (negated) {
                ntry.dsc = null;
            }
            return false;
        }
        if (s.equals("error")) {
            ntry.err = cmd.getRemaining();
            if (negated) {
                ntry.err = null;
            }
            return false;
        }
        if (s.equals("alternate")) {
            ntry.alternate = !negated;
            return false;
        }
        if (s.equals("req-reg")) {
            s = cmd.getRemaining();
            if (negated) {
                ntry.reqR.remove(s);
            } else {
                ntry.reqR.add(s);
            }
            return false;
        }
        if (s.equals("ign-reg")) {
            s = cmd.getRemaining();
            if (negated) {
                ntry.ignR.remove(s);
            } else {
                ntry.ignR.add(s);
            }
            return false;
        }
        if (s.equals("req-txt")) {
            s = cmd.getRemaining();
            if (negated) {
                ntry.reqT.remove(s);
            } else {
                ntry.reqT.add(s);
            }
            return false;
        }
        if (s.equals("ign-txt")) {
            s = cmd.getRemaining();
            if (negated) {
                ntry.ignT.remove(s);
            } else {
                ntry.ignT.add(s);
            }
            return false;
        }
        if (s.equals("train")) {
            s = cmd.getRemaining();
            if (s.length() < 1) {
                ntry.doTrain();
            } else {
                ntry.doTrain(s);
            }
            return false;
        }
        return true;
    }

    public void srvHelp(userHelping l) {
        l.add("1 2  error-truncate               truncate first line");
        l.add("2 .    <num>                      upper limit in characters");
        l.add("1 .  error-states                 remove state of messages");
        l.add("1 .  error-commands               include commands in states");
        l.add("1 .  error-hostname               include local hostname in states");
        l.add("1 2  resolve                      resolve the regexp group a to hostname");
        l.add("2 3    <name>                     regexp of checks");
        l.add("3 3,.    <str>                    text to resolve");
        l.add("1 2  replace                      replace from one string to another");
        l.add("2 3    <name>                     regexp of checks");
        l.add("3 4      <str>                    source string");
        l.add("4 4,.      <str>                  target string");
        l.add("1 2  check                        configure one check");
        l.add("2 3    <name>                     name of check");
        l.add("3 4,.    train                    train command to current result");
        l.add("4 4,.      <str>                  text");
        l.add("3 .      alternate                alternate reported state on diff change");
        l.add("3 4      command                  specify command to execute");
        l.add("4 4,.      <str>                  command");
        l.add("3 4      description              specify description");
        l.add("4 4,.      <str>                  description");
        l.add("3 4      error                    specify error text");
        l.add("4 4,.      <str>                  description");
        l.add("3 4      req-reg                  require one regexp line");
        l.add("4 4,.      <str>                  text");
        l.add("3 4      ign-reg                  ignore one regexp line");
        l.add("4 4,.      <str>                  text");
        l.add("3 4      req-txt                  require one text line");
        l.add("4 4,.      <str>                  text");
        l.add("3 4      ign-txt                  ignore one text line");
        l.add("4 4,.      <str>                  text");
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.timeout = 120000;
        new servNrpeConn(this, pipe);
        return false;
    }

}

class servNrpeConn implements Runnable {

    private servNrpe lower;

    private pipeSide conn;

    public servNrpeConn(servNrpe parent, pipeSide pipe) {
        lower = parent;
        conn = pipe;
        new Thread(this).start();
    }

    public void run() {
        try {
            for (;;) {
                packNrpe pck = new packNrpe();
                if (pck.recvPack(conn)) {
                    break;
                }
                if (debugger.servNrpeTraf) {
                    logger.debug("rx " + pck.dump());
                }
                if (pck.typ != packNrpe.tyReq) {
                    pck.typ = packNrpe.tyRep;
                    pck.cod = packNrpe.coUnk;
                    pck.str = "UNKNOWN invalid packet type";
                    pck.sendPack(conn);
                    if (debugger.servNrpeTraf) {
                        logger.debug("tx " + pck.dump());
                    }
                    continue;
                }
                pck.typ = packNrpe.tyRep;
                servNrpeCheck ntry = new servNrpeCheck(lower, pck.str.replaceAll("!", "-"));
                ntry = lower.chks.find(ntry);
                if (ntry == null) {
                    pck.cod = packNrpe.coUnk;
                    pck.str = "UNKNOWN no such check";
                    pck.sendPack(conn);
                    if (debugger.servNrpeTraf) {
                        logger.debug("tx " + pck.dump());
                    }
                    continue;
                }
                List<String> lst = ntry.doCheck();
                if (lst.size() < 1) {
                    pck.cod = packNrpe.coOk;
                    pck.str = "OK";
                    if (ntry.dsc != null) {
                        pck.str += " " + ntry.dsc;
                    }
                    pck.sendPack(conn);
                    if (debugger.servNrpeTraf) {
                        logger.debug("tx " + pck.dump());
                    }
                    continue;
                }
                pck.cod = packNrpe.coCri;
                pck.str = "";
                if (!lower.noState) {
                    pck.str += "ERROR ";
                }
                pck.str += lst.size() + " ";
                if (ntry.err != null) {
                    pck.str += ntry.err + " ";
                } else {
                    pck.str += "lines(s) changed ";
                }
                if (lower.sendMyId) {
                    pck.str += " - " + cfgAll.hostName + "#";
                }
                if (lower.sendCmds) {
                    pck.str += ntry.cmd + " ";
                }
                pck.str = pck.str.trim();
                if (lower.truncState > 0) {
                    int i = pck.str.length();
                    if (i > lower.truncState) {
                        i = lower.truncState;
                    }
                    pck.str = pck.str.substring(0, i);
                }
                String a = new String(pck.sep);
                for (int i = 0; i < lst.size(); i++) {
                    pck.str += a + lst.get(i);
                }
                if (ntry.alternate) {
                    int i = pck.str.hashCode();
                    if (i == ntry.lastHash) {
                        pck.cod = ntry.lastStat;
                    } else {
                        ntry.lastHash = i;
                        if (ntry.lastStat == packNrpe.coCri) {
                            i = packNrpe.coWar;
                        } else {
                            i = packNrpe.coCri;
                        }
                        ntry.lastStat = i;
                        pck.cod = i;
                    }
                }
                pck.sendPack(conn);
                if (debugger.servNrpeTraf) {
                    logger.debug("tx " + pck.dump());
                }
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        conn.setClose();

    }

}

class servNrpeRep implements Comparator<servNrpeRep> {

    public final String chk;

    public final String src;

    public final String trg;

    public servNrpeRep(String c, String s, String t) {
        chk = c;
        src = s;
        trg = t;
    }

    public String toString() {
        return chk + " " + src + " " + trg;
    }

    public int compare(servNrpeRep o1, servNrpeRep o2) {
        int i = o1.chk.toLowerCase().compareTo(o2.chk.toLowerCase());
        if (i != 0) {
            return i;
        }
        return o1.src.toLowerCase().compareTo(o2.src.toLowerCase());
    }

}

class servNrpeRes implements Comparator<servNrpeRes> {

    public final String chk;

    public final String nam;

    public servNrpeRes(String c, String s) {
        chk = c;
        nam = s;
    }

    public String toString() {
        return chk + " " + nam;
    }

    public int compare(servNrpeRes o1, servNrpeRes o2) {
        int i = o1.chk.toLowerCase().compareTo(o2.chk.toLowerCase());
        if (i != 0) {
            return i;
        }
        return o1.nam.toLowerCase().compareTo(o2.nam.toLowerCase());
    }

    public String doWork(String l) {
        String as;
        try {
            Pattern pat = Pattern.compile(nam);
            Matcher mat = pat.matcher(l);
            if (!mat.find()) {
                return l;
            }
            as = mat.group("a");
        } catch (Exception e) {
            as = null;
        }
        if (as == null) {
            return l;
        }
        addrIP ad = new addrIP();
        if (ad.fromString(as)) {
            return l;
        }
        clntDns clnt = new clntDns();
        clnt.doResolvList(cfgAll.nameServerAddr, packDnsRec.generateReverse(ad), packDnsRec.typePTR);
        String dn = clnt.getPTR();
        if (dn == null) {
            return l;
        }
        return l.replaceAll(as, dn);
    }

}

class servNrpeCheck implements Comparator<servNrpeCheck> {

    private final servNrpe lower;

    public final String nam;

    public String cmd;

    public String dsc;

    public String err;

    public final List<String> ignR;

    public final List<String> reqR;

    public final List<String> ignT;

    public final List<String> reqT;

    public boolean alternate;

    public int lastHash = 1;

    public int lastStat = packNrpe.coWar;

    public servNrpeCheck(servNrpe p, String n) {
        lower = p;
        nam = n;
        ignR = new ArrayList<String>();
        reqR = new ArrayList<String>();
        ignT = new ArrayList<String>();
        reqT = new ArrayList<String>();
    }

    public int compare(servNrpeCheck o1, servNrpeCheck o2) {
        return o1.nam.toLowerCase().compareTo(o2.nam.toLowerCase());
    }

    public List<String> getResult() {
        pipeLine pl = new pipeLine(1024 * 1024, false);
        pipeSide pip = pl.getSide();
        pip.lineTx = pipeSide.modTyp.modeCRLF;
        pip.lineRx = pipeSide.modTyp.modeCRorLF;
        userReader rdr = new userReader(pip, 1023);
        rdr.tabMod = userFormat.tableMode.raw;
        rdr.height = 0;
        userExec exe = new userExec(pip, rdr);
        exe.privileged = true;
        pip.timeout = 120000;
        String a = exe.repairCommand(cmd);
        exe.executeCommand(a);
        pip = pl.getSide();
        pl.setClose();
        pip.lineTx = pipeSide.modTyp.modeCRLF;
        pip.lineRx = pipeSide.modTyp.modeCRtryLF;
        List<String> lst = new ArrayList<String>();
        for (;;) {
            if (pip.ready2rx() < 1) {
                break;
            }
            a = pip.lineGet(1);
            if (a.length() < 1) {
                continue;
            }
            lst.add(a);
        }
        return lst;
    }

    public void delIgn(List<String> lst) {
        for (int o = 0; o < ignT.size(); o++) {
            String s = ignT.get(o);
            for (int i = 0; i < lst.size(); i++) {
                if (!lst.get(i).equals(s)) {
                    continue;
                }
                lst.remove(i);
                break;
            }
        }
        for (int o = 0; o < ignR.size(); o++) {
            String s = ignR.get(o);
            for (int i = 0; i < lst.size(); i++) {
                if (!lst.get(i).matches(s)) {
                    continue;
                }
                lst.remove(i);
                break;
            }
        }
    }

    public void doTrain(String ned) {
        List<String> lst = getResult();
        delIgn(lst);
        for (int i = reqT.size() - 1; i >= 0; i--) {
            if (reqT.get(i).indexOf(ned) >= 0) {
                reqT.remove(i);
            }
        }
        for (int i = 0; i < lst.size(); i++) {
            String a = lst.get(i);
            if (a.indexOf(ned) < 0) {
                continue;
            }
            reqT.add(a);
        }
    }

    public void doTrain() {
        List<String> lst = getResult();
        delIgn(lst);
        reqT.clear();
        for (int i = 0; i < lst.size(); i++) {
            reqT.add(lst.get(i));
        }
    }

    public String makeFancy(String l) {
        for (int i = 0; i < lower.ress.size(); i++) {
            servNrpeRes r = lower.ress.get(i);
            if (r == null) {
                continue;
            }
            if (!nam.matches(r.chk)) {
                continue;
            }
            l = r.doWork(l);
        }
        for (int i = 0; i < lower.reps.size(); i++) {
            servNrpeRep r = lower.reps.get(i);
            if (r == null) {
                continue;
            }
            if (!nam.matches(r.chk)) {
                continue;
            }
            l = l.replaceAll(r.src, r.trg);
        }
        return l;
    }

    public List<String> doCheck() {
        List<String> lst = getResult();
        delIgn(lst);
        List<String> res = new ArrayList<String>();
        for (int o = 0; o < reqT.size(); o++) {
            String s = reqT.get(o);
            boolean ok = false;
            for (int i = 0; i < lst.size(); i++) {
                if (!lst.get(i).equals(s)) {
                    continue;
                }
                lst.remove(i);
                ok = true;
                break;
            }
            if (ok) {
                continue;
            }
            res.add("- " + makeFancy(s));
        }
        for (int o = 0; o < reqR.size(); o++) {
            String s = reqR.get(o);
            boolean ok = false;
            for (int i = 0; i < lst.size(); i++) {
                if (!lst.get(i).matches(s)) {
                    continue;
                }
                lst.remove(i);
                ok = true;
                break;
            }
            if (ok) {
                continue;
            }
            res.add("- " + makeFancy(s));
        }
        for (int i = 0; i < lst.size(); i++) {
            res.add("+ " + makeFancy(lst.get(i)));
        }
        return res;
    }

}
