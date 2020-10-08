package serv;

import cfg.cfgAll;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
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
import util.uniResLoc;

/**
 * prometheus server
 *
 * @author matecsaba
 */
public class servPrometheus extends servGeneric implements prtServS {

    /**
     * default port
     */
    public final static int port = 9001;

    /**
     * list of metrics
     */
    public tabGen<servPrometheusMet> mets = new tabGen<servPrometheusMet>();

    /**
     * all the metrics
     */
    public String allMets = "metrics";

    /**
     * number of queries
     */
    public int allNum;

    /**
     * time to respond
     */
    public int allTim;

    /**
     * last response
     */
    public long allLast;

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "server prometheus .*! port " + port,
        "server prometheus .*! protocol " + proto2string(protoAllStrm),
        "server prometheus .*! all-metrics metrics",
        "server prometheus .*! metric .* name 0",
        "server prometheus .*! metric .* addname -1 null",
        "server prometheus .*! metric .* skip 1",
        "server prometheus .*! no metric .* labeled",
        "server prometheus .*! no metric .* excluded",
        "server prometheus .*! metric .* column .* type gauge",
        "server prometheus .*! metric .* column .* help null",};

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public String srvName() {
        return "prometheus";
    }

    public int srvPort() {
        return port;
    }

    public int srvProto() {
        return protoAllStrm;
    }

    public boolean srvInit() {
        return genStrmStart(this, new pipeLine(65536, false), 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

    public void srvShRun(String beg, List<String> lst) {
        for (int p = 0; p < mets.size(); p++) {
            servPrometheusMet met = mets.get(p);
            if (met == null) {
                continue;
            }
            lst.add(beg + "all-metrics " + allMets);
            String mn = beg + "metric " + met.nam;
            String nn = beg + "no metric " + met.nam;
            lst.add(mn + " command " + met.cmd);
            lst.add(mn + " prepend " + met.prep);
            lst.add(mn + " name " + met.col);
            lst.add(mn + " addname " + met.acol + " " + met.asep);
            lst.add(mn + " skip " + met.skp);
            if (met.exc) {
                lst.add(mn + " excluded");
            } else {
                lst.add(nn + " excluded");
            }
            if (met.lab) {
                lst.add(mn + " labeled");
            } else {
                lst.add(nn + " labeled");
            }
            for (int i = 0; i < met.reps.size(); i++) {
                servPrometheusRep rep = met.reps.get(i);
                lst.add(mn + " replace " + rep.src + " " + rep.trg);
            }
            for (int o = 0; o < met.cols.size(); o++) {
                servPrometheusCol col = met.cols.get(o);
                String cn = mn + " column " + col.num;
                lst.add(cn + " name " + col.nam);
                lst.add(cn + " type " + col.typ);
                lst.add(cn + " help " + col.hlp);
                for (int i = 0; i < col.reps.size(); i++) {
                    servPrometheusRep rep = col.reps.get(i);
                    lst.add(cn + " replace " + rep.src + " " + rep.trg);
                }
            }
        }
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        boolean negated = s.equals("no");
        if (negated) {
            s = cmd.word();
        }
        if (s.equals("all-metrics")) {
            allMets = cmd.getRemaining();
            return false;
        }
        if (!s.equals("metric")) {
            return true;
        }
        servPrometheusMet met = new servPrometheusMet(cmd.word());
        servPrometheusMet oldm = mets.add(met);
        if (oldm != null) {
            met = oldm;
        }
        s = cmd.word();
        if (s.equals("command")) {
            if (negated) {
                mets.del(met);
                return false;
            }
            met.cmd = cmd.getRemaining();
            return false;
        }
        if (met.cmd == null) {
            mets.del(met);
            cmd.error("no such metric");
            return false;
        }
        if (s.equals("prepend")) {
            met.prep = cmd.getRemaining();
            return false;
        }
        if (s.equals("name")) {
            met.col = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("addname")) {
            if (negated) {
                met.acol = -1;
                met.asep = null;
            } else {
                met.acol = bits.str2num(cmd.word());
                met.asep = cmd.getRemaining();
            }
            return false;
        }
        if (s.equals("skip")) {
            met.skp = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("excluded")) {
            met.exc = !negated;
            return false;
        }
        if (s.equals("labeled")) {
            met.lab = !negated;
            return false;
        }
        if (s.equals("replace")) {
            servPrometheusRep rep = new servPrometheusRep(cmd.word());
            rep.trg = cmd.word();
            if (negated) {
                met.reps.del(rep);
            } else {
                met.reps.add(rep);
            }
            return false;
        }
        if (!s.equals("column")) {
            return true;
        }
        servPrometheusCol col = new servPrometheusCol(bits.str2num(cmd.word()));
        servPrometheusCol oldc = met.cols.add(col);
        if (oldc != null) {
            col = oldc;
        }
        s = cmd.word();
        if (s.equals("name")) {
            if (negated) {
                met.cols.del(col);
                return false;
            }
            col.nam = cmd.getRemaining();
            return false;
        }
        if (s.equals("type")) {
            if (negated) {
                col.typ = "gauge";
            } else {
                col.typ = cmd.getRemaining();
            }
            return false;
        }
        if (s.equals("help")) {
            if (negated) {
                col.hlp = null;
            } else {
                col.hlp = cmd.getRemaining();
            }
            return false;
        }
        if (s.equals("replace")) {
            servPrometheusRep rep = new servPrometheusRep(cmd.word());
            rep.trg = cmd.word();
            if (negated) {
                col.reps.del(rep);
            } else {
                col.reps.add(rep);
            }
            return false;
        }
        return true;
    }

    public void srvHelp(userHelping l) {
        l.add("1 2  all-metrics                  configure whole exporter");
        l.add("2 .    <name>                     name to use");
        l.add("1 2  metric                       configure one metric");
        l.add("2 3    <name>                     name of metric");
        l.add("3 4      command                  specify command to execute");
        l.add("4 4,.      <str>                  command");
        l.add("3 4      prepend                  specify metric name to prepend");
        l.add("4 .        <str>                  name");
        l.add("3 4      name                     name column number");
        l.add("4 .        <num>                  column number");
        l.add("3 4      addname                  add name column number");
        l.add("4 5        <num>                  column number");
        l.add("5 .          <str>                separator");
        l.add("3 .      labeled                  expose labeled columns");
        l.add("3 .      excluded                 exclude from whole reporting");
        l.add("3 4      skip                     rows to skip");
        l.add("4 .        <num>                  lines to skip");
        l.add("3 4      replace                  define replaces in name");
        l.add("4 5        <str>                  string to replace");
        l.add("5 .          <str>                replacement string");
        l.add("3 4      column                   define column to export");
        l.add("4 5        <num>                  number");
        l.add("5 6,.        name                 set metric name");
        l.add("6 .            <str>              metric name");
        l.add("5 6          type                 set metric type");
        l.add("6 .            <str>              metric type");
        l.add("5 6          help                 set metric help");
        l.add("6 6,.          <str>              metric help");
        l.add("5 6          replace              define replaces in value");
        l.add("6 7            <str>              string to replace");
        l.add("7 .              <str>            replacement string");
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.timeout = 120000;
        new servPrometheusConn(this, pipe);
        return false;
    }

    /**
     * get show
     *
     * @return result
     */
    public userFormat getShow() {
        userFormat res = new userFormat("|", "name|asked|reply|last");
        res.add(allMets + "|" + allNum + "|" + allTim + "|" + bits.timePast(allLast));
        for (int i = 0; i < mets.size(); i++) {
            servPrometheusMet ntry = mets.get(i);
            res.add(ntry.nam + "|" + ntry.askNum + "|" + ntry.tim + "|" + bits.timePast(ntry.askLast));
        }
        return res;
    }

    /**
     * get show
     *
     * @param nam name of metric
     * @return result
     */
    public List<String> getShow(String nam) {
        servPrometheusMet ntry = new servPrometheusMet(nam);
        ntry = mets.find(ntry);
        if (ntry == null) {
            return null;
        }
        List<String> res = new ArrayList<String>();
        res.add("name=" + ntry.nam);
        res.add("command=" + ntry.cmd);
        res.add("asked=" + ntry.askNum + " times");
        res.add("reply=" + ntry.tim + " ms");
        res.add("output:");
        res.addAll(ntry.getResult());
        res.add("result:");
        res.addAll(ntry.doMetric());
        return res;
    }

}

class servPrometheusMet implements Comparator<servPrometheusMet> {

    public final String nam;

    public int tim;

    public int askNum;

    public long askLast;

    public String cmd;

    public String prep;

    public int col = 0;

    public int acol = -1;

    public String asep;

    public int skp = 1;

    public boolean exc;

    public boolean lab;

    public tabGen<servPrometheusCol> cols = new tabGen<servPrometheusCol>();

    public tabGen<servPrometheusRep> reps = new tabGen<servPrometheusRep>();

    public servPrometheusMet(String n) {
        nam = n;
    }

    public int compare(servPrometheusMet o1, servPrometheusMet o2) {
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

    public List<String> doMetric() {
        List<String> lst = new ArrayList<String>();
        List<String> res = getResult();
        for (int i = 0; i < skp; i++) {
            if (res.size() < 1) {
                break;
            }
            res.remove(0);
        }
        for (int p = 0; p < res.size(); p++) {
            cmds cm = new cmds("prom", res.get(p));
            List<String> cl = new ArrayList<String>();
            for (;;) {
                String a = cm.word(";");
                if (a.length() < 1) {
                    break;
                }
                cl.add(a);
            }
            int cls = cl.size();
            if (col >= cls) {
                continue;
            }
            String na = prep + cl.get(col);
            if ((acol >= 0) && (acol < cls)) {
                na += asep + cl.get(acol);
            }
            for (int i = 0; i < reps.size(); i++) {
                servPrometheusRep rep = reps.get(i);
                na = na.replaceAll(rep.src, rep.trg);
            }
            for (int o = 0; o < cols.size(); o++) {
                servPrometheusCol col = cols.get(o);
                if (cl.size() <= col.num) {
                    continue;
                }
                String nb = na + col.nam;
                String h;
                if (col.hlp == null) {
                    h = " column " + col.num + " of " + cmd;
                } else {
                    h = " " + col.hlp;
                }
                if (lab) {
                    lst.add("# HELP " + na + h);
                    lst.add("# TYPE " + na + " " + col.typ);
                } else {
                    lst.add("# HELP " + nb + h);
                    lst.add("# TYPE " + nb + " " + col.typ);
                }
                String a = cl.get(col.num);
                for (int i = 0; i < col.reps.size(); i++) {
                    servPrometheusRep rep = col.reps.get(i);
                    a = a.replaceAll(rep.src, rep.trg);
                }
                lst.add(nb + " " + a);
            }
        }
        return lst;
    }

}

class servPrometheusRep implements Comparator<servPrometheusRep> {

    public final String src;

    public String trg;

    public servPrometheusRep(String n) {
        src = n;
    }

    public int compare(servPrometheusRep o1, servPrometheusRep o2) {
        return o1.src.compareTo(o2.src);
    }

}

class servPrometheusCol implements Comparator<servPrometheusCol> {

    public final int num;

    public String nam;

    public String typ = "gauge";

    public String hlp = null;

    public tabGen<servPrometheusRep> reps = new tabGen<servPrometheusRep>();

    public servPrometheusCol(int n) {
        num = n;
    }

    public int compare(servPrometheusCol o1, servPrometheusCol o2) {
        if (o1.num < o2.num) {
            return -1;
        }
        if (o1.num > o2.num) {
            return +1;
        }
        return 0;
    }

}

class servPrometheusConn implements Runnable {

    private servPrometheus lower;

    private pipeSide conn;

    public servPrometheusConn(servPrometheus parent, pipeSide pipe) {
        lower = parent;
        conn = pipe;
        new Thread(this).start();
    }

    private void sendReply(String hdr, List<String> res) {
        conn.lineTx = pipeSide.modTyp.modeCRLF;
        conn.linePut("HTTP/1.1 " + hdr);
        conn.linePut("Content-Type: text/plain");
        conn.linePut("Date: " + bits.time2str(cfgAll.timeZoneName, bits.getTime(), 4));
        int len = res.size();
        for (int i = 0; i < res.size(); i++) {
            len += res.get(i).length();
        }
        conn.linePut("Content-Length: " + len);
        conn.linePut("");
        conn.lineTx = pipeSide.modTyp.modeLF;
        for (int i = 0; i < res.size(); i++) {
            conn.linePut(res.get(i));
        }
        if (debugger.servPrometheusTraf) {
            logger.debug("tx " + hdr + " and " + res.size() + " lines");
        }
    }

    private boolean doWork() {
        conn.lineRx = pipeSide.modTyp.modeCRtryLF;
        conn.lineTx = pipeSide.modTyp.modeCRLF;
        String gotCmd = conn.lineGet(1);
        if (debugger.servPrometheusTraf) {
            logger.debug("rx " + gotCmd);
        }
        if (gotCmd.length() < 1) {
            return true;
        }
        for (;;) {
            String a = conn.lineGet(1);
            if (a.length() < 1) {
                break;
            }
        }
        uniResLoc gotUrl = new uniResLoc();
        int i = gotCmd.toLowerCase().lastIndexOf(" http/");
        if (i > 0) {
            gotCmd = gotCmd.substring(0, i);
        }
        i = gotCmd.indexOf(" ");
        if (i < 0) {
            sendReply("501 bad request", bits.str2lst("bad request"));
            return true;
        }
        String s = gotCmd.substring(i + 1, gotCmd.length());
        gotCmd = gotCmd.substring(0, i);
        gotUrl.fromString(s);
        servPrometheusMet ntry = new servPrometheusMet(gotUrl.filName);
        ntry = lower.mets.find(ntry);
        if (ntry != null) {
            long tim = bits.getTime();
            List<String> res = ntry.doMetric();
            ntry.tim = (int) (bits.getTime() - tim);
            ntry.askLast = tim;
            ntry.askNum++;
            sendReply("200 ok", res);
            return false;
        }
        if (!gotUrl.filName.equals(lower.allMets)) {
            sendReply("404 not found", bits.str2lst("no such metric"));
            return false;
        }
        long tim = bits.getTime();
        List<String> res = new ArrayList<String>();
        for (i = 0; i < lower.mets.size(); i++) {
            ntry = lower.mets.get(i);
            if (ntry.exc) {
                continue;
            }
            ntry.askLast = bits.getTime();
            res.addAll(ntry.doMetric());
            ntry.tim = (int) (bits.getTime() - ntry.askLast);
            ntry.askNum++;
        }
        lower.allTim = (int) (bits.getTime() - tim);
        lower.allLast = tim;
        lower.allNum++;
        sendReply("200 ok", res);
        return false;
    }

    public void run() {
        try {
            for (;;) {
                if (doWork()) {
                    break;
                }
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        conn.setClose();

    }

}
