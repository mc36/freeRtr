package serv;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.regex.Pattern;
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
     * defaults text
     */
    public final static String defaultL[] = {
        "server nrpe .*! port " + packNrpe.portNum,
        "server nrpe .*! protocol " + proto2string(protoAllStrm),
        "server nrpe .*! no check .* description",
        "server nrpe .*! no check .* error"
    };

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
        for (int o = 0; o < chks.size(); o++) {
            servNrpeCheck ntry = chks.get(o);
            if (ntry == null) {
                continue;
            }
            String cn = "check " + ntry.nam;
            lst.add(beg + cn + " command " + ntry.cmd);
            cmds.cfgLine(lst, ntry.dsc == null, beg, cn + " description", ntry.dsc);
            cmds.cfgLine(lst, ntry.err == null, beg, cn + " error", ntry.err);
            for (int i = 0; i < ntry.ign.size(); i++) {
                lst.add(beg + cn + " ignore " + ntry.ign.get(i));
            }
            for (int i = 0; i < ntry.req.size(); i++) {
                lst.add(beg + cn + " require " + ntry.req.get(i));
            }
        }
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        boolean negated = s.equals("no");
        if (negated) {
            s = cmd.word();
        }
        if (!s.equals("check")) {
            return true;
        }
        servNrpeCheck ntry = new servNrpeCheck(cmd.word());
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
        if (s.equals("require")) {
            s = cmd.getRemaining();
            if (negated) {
                ntry.req.remove(s);
            } else {
                ntry.req.add(s);
            }
            return false;
        }
        if (s.equals("ignore")) {
            s = cmd.getRemaining();
            if (negated) {
                ntry.ign.remove(s);
            } else {
                ntry.ign.add(s);
            }
            return false;
        }
        if (s.equals("train")) {
            ntry.doTrain();
            return false;
        }
        return true;
    }

    public void srvHelp(userHelping l) {
        l.add("1 2  check                        configure one check");
        l.add("2 3    <name>                     name of check");
        l.add("3 .      train                    train command to current result");
        l.add("3 4      command                  specify command to execute");
        l.add("4 4,.      <str>                  command");
        l.add("3 4      description              specify description");
        l.add("4 4,.      <str>                  description");
        l.add("3 4      error                    specify error text");
        l.add("4 4,.      <str>                  description");
        l.add("3 4      require                  require one line");
        l.add("4 4,.      <str>                  regular expression");
        l.add("3 4      ignore                   ignore one line");
        l.add("4 4,.      <str>                  regular expression");
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
                    pck.str = pck.str + " UNKNOWN invalid packet type";
                    pck.sendPack(conn);
                    if (debugger.servNrpeTraf) {
                        logger.debug("tx " + pck.dump());
                    }
                    continue;
                }
                pck.typ = packNrpe.tyRep;
                servNrpeCheck ntry = new servNrpeCheck(pck.str);
                ntry = lower.chks.find(ntry);
                if (ntry == null) {
                    pck.cod = packNrpe.coUnk;
                    pck.str = pck.str + " UNKNOWN no such check";
                    pck.sendPack(conn);
                    if (debugger.servNrpeTraf) {
                        logger.debug("tx " + pck.dump());
                    }
                    continue;
                }
                List<String> lst = ntry.doCheck();
                if (lst.size() < 1) {
                    pck.cod = packNrpe.coOk;
                    pck.str = ntry.nam + " OK";
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
                pck.str = ntry.nam + " CRITICAL " + lst.size();
                if (ntry.err != null) {
                    pck.str += " " + ntry.err;
                }
                String a = new String(pck.sep);
                for (int i = 0; i < lst.size(); i++) {
                    pck.str += a + lst.get(i);
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

class servNrpeCheck implements Comparator<servNrpeCheck> {

    public final String nam;

    public String cmd;

    public String dsc;

    public String err;

    public final List<String> ign;

    public final List<String> req;

    public servNrpeCheck(String n) {
        nam = n;
        ign = new ArrayList<String>();
        req = new ArrayList<String>();
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
        for (int o = 0; o < ign.size(); o++) {
            String s = ign.get(o);
            for (int i = 0; i < lst.size(); i++) {
                if (!lst.get(i).matches(s)) {
                    continue;
                }
                lst.remove(i);
                break;
            }
        }
    }

    public void doTrain() {
        List<String> lst = getResult();
        delIgn(lst);
        req.clear();
        for (int i = 0; i < lst.size(); i++) {
            req.add(Pattern.quote(lst.get(i)));
        }
    }

    public List<String> doCheck() {
        List<String> lst = getResult();
        delIgn(lst);
        List<String> res = new ArrayList<String>();
        for (int o = 0; o < req.size(); o++) {
            String s = req.get(o);
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
            res.add("- " + s);
        }
        for (int i = 0; i < lst.size(); i++) {
            res.add("+ " + lst.get(i));
        }
        return res;
    }

}
