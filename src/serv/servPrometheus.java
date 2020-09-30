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
    public tabGen<servPrometheusMetric> mets = new tabGen<servPrometheusMetric>();

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "server prometheus .*! port " + port,
        "server prometheus .*! protocol " + proto2string(protoAllStrm),
        "server prometheus .*! metric .* column 1",
        "server prometheus .*! metric .* skip 1",};

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
        for (int o = 0; o < mets.size(); o++) {
            servPrometheusMetric ntry = mets.get(o);
            if (ntry == null) {
                continue;
            }
            String cn = "metric " + ntry.nam;
            lst.add(beg + cn + " command " + ntry.cmd);
            lst.add(beg + cn + " prepend " + ntry.prep);
            lst.add(beg + cn + " name " + ntry.col);
            lst.add(beg + cn + " skip " + ntry.skp);
            for (int i = 0; i < ntry.cols.size(); i++) {
                servPrometheusColumn col = ntry.cols.get(i);
                lst.add(beg + cn + " column " + col.num + " " + col.nam + " " + col.typ + " " + col.hlp);
            }
            for (int i = 0; i < ntry.reps.size(); i++) {
                servPrometheusReplace rep = ntry.reps.get(i);
                lst.add(beg + cn + " replace " + rep.src + " " + rep.trg);
            }
        }
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        boolean negated = s.equals("no");
        if (negated) {
            s = cmd.word();
        }
        if (!s.equals("metric")) {
            return true;
        }
        servPrometheusMetric ntry = new servPrometheusMetric(cmd.word());
        servPrometheusMetric old = mets.add(ntry);
        if (old != null) {
            ntry = old;
        }
        s = cmd.word();
        if (s.equals("command")) {
            if (negated) {
                mets.del(ntry);
                return false;
            }
            ntry.cmd = cmd.getRemaining();
            return false;
        }
        if (ntry.cmd == null) {
            mets.del(ntry);
            cmd.error("no such metric");
            return false;
        }
        if (s.equals("prepend")) {
            ntry.prep = cmd.getRemaining();
            if (negated) {
                ntry.prep = null;
            }
            return false;
        }
        if (s.equals("name")) {
            ntry.col = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("skip")) {
            ntry.skp = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("replace")) {
            servPrometheusReplace rep = new servPrometheusReplace(cmd.word());
            rep.trg = cmd.word();
            if (negated) {
                ntry.reps.del(rep);
            } else {
                ntry.reps.add(rep);
            }
            return false;
        }
        if (s.equals("column")) {
            servPrometheusColumn col = new servPrometheusColumn(bits.str2num(cmd.word()));
            col.nam = cmd.word();
            col.typ = cmd.word();
            col.hlp = cmd.getRemaining();
            if (negated) {
                ntry.cols.del(col);
            } else {
                ntry.cols.add(col);
            }
            return false;
        }
        return true;
    }

    public void srvHelp(userHelping l) {
        l.add("1 2  metric                       configure one metric");
        l.add("2 3    <name>                     name of metric");
        l.add("3 4      command                  specify command to execute");
        l.add("4 4,.      <str>                  command");
        l.add("3 4      prepend                  specify metric name to prepend");
        l.add("4 .        <str>                  name");
        l.add("3 4      name                     name column number");
        l.add("4 .        <num>                  column number of metric name");
        l.add("3 4      skip                     rows to skip");
        l.add("4 .        <num>                  lines to skip");
        l.add("3 4      replace                  define replaces in name");
        l.add("4 5        <str>                  string to replace");
        l.add("5 .          <str>                replacement string");
        l.add("3 4      column                   define column to export");
        l.add("4 5        <num>                  number");
        l.add("5 6          <str>                metric name");
        l.add("6 7            <str>              metric type");
        l.add("7 7,.            <str>            description");
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
        userFormat res = new userFormat("|", "name|asked|reply|times|last");
        for (int i = 0; i < mets.size(); i++) {
            servPrometheusMetric ntry = mets.get(i);
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
        servPrometheusMetric ntry = new servPrometheusMetric(nam);
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

class servPrometheusMetric implements Comparator<servPrometheusMetric> {

    public final String nam;

    public int tim;

    public int askNum;

    public long askLast;

    public String cmd;

    public String prep;

    public int col = 1;

    public int skp = 1;

    public tabGen<servPrometheusColumn> cols = new tabGen<servPrometheusColumn>();

    public tabGen<servPrometheusReplace> reps = new tabGen<servPrometheusReplace>();

    public servPrometheusMetric(String n) {
        nam = n;
    }

    public int compare(servPrometheusMetric o1, servPrometheusMetric o2) {
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
            res.remove(0);
        }
        for (int o = 0; o < res.size(); o++) {
            cmds cmd = new cmds("prom", res.get(o));
            List<String> cl = new ArrayList<String>();
            for (;;) {
                String a = cmd.word(";");
                if (a.length() < 1) {
                    break;
                }
                cl.add(a);
            }
            if (cl.size() <= col) {
                continue;
            }
            String na = prep + cl.get(col);
            for (int i = 0; i < reps.size(); i++) {
                servPrometheusReplace rep = reps.get(i);
                na = na.replaceAll(rep.src, rep.trg);
            }
            for (int i = 0; i < cols.size(); i++) {
                servPrometheusColumn col = cols.get(i);
                if (cl.size() < col.num) {
                    continue;
                }
                String nb = na + col.nam;
                lst.add("# HELP " + nb + " " + col.hlp);
                lst.add("# TYPE " + nb + " " + col.typ);
                lst.add(nb + " " + cl.get(col.num));
            }
        }
        return lst;
    }

}

class servPrometheusReplace implements Comparator<servPrometheusReplace> {

    public final String src;

    public String trg;

    public servPrometheusReplace(String n) {
        src = n;
    }

    public int compare(servPrometheusReplace o1, servPrometheusReplace o2) {
        return o1.src.compareTo(o2.src);
    }

}

class servPrometheusColumn implements Comparator<servPrometheusColumn> {

    public final int num;

    public String nam;

    public String typ;

    public String hlp;

    public servPrometheusColumn(int n) {
        num = n;
    }

    public int compare(servPrometheusColumn o1, servPrometheusColumn o2) {
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

    private void doWork() {
        conn.lineRx = pipeSide.modTyp.modeCRtryLF;
        conn.lineTx = pipeSide.modTyp.modeLF;
        String gotCmd = conn.lineGet(1);
        if (debugger.servPrometheusTraf) {
            logger.debug("rx " + gotCmd);
        }
        if (gotCmd.length() < 1) {
            return;
        }
        for (;;) {
            String a = conn.lineGet(1);
            if (a.length() < 1) {
                break;
            }
        }
        uniResLoc gotUrl = new uniResLoc();
        gotUrl.clear();
        int i = gotCmd.toLowerCase().lastIndexOf(" http/");
        if (i > 0) {
            String s = gotCmd.substring(i + 6, gotCmd.length());
            gotCmd = gotCmd.substring(0, i);
            i = s.indexOf(".");
        }
        i = gotCmd.indexOf(" ");
        if (i > 0) {
            String s = gotCmd.substring(i + 1, gotCmd.length());
            gotCmd = gotCmd.substring(0, i);
            gotUrl.fromString(s);
        }
        servPrometheusMetric ntry = new servPrometheusMetric(gotUrl.filName);
        ntry = lower.mets.find(ntry);
        long tim = bits.getTime();
        if (ntry == null) {
            conn.linePut("HTTP/1.1 404 not found");
            conn.linePut("Date: " + bits.time2str(cfgAll.timeZoneName, tim, 4));
            conn.linePut("");
            conn.linePut("metric not found");
            return;
        }
        conn.linePut("HTTP/1.1 200 ok");
        conn.linePut("Content-Type: text/plain");
        conn.linePut("Date: " + bits.time2str(cfgAll.timeZoneName, tim, 4));
        conn.linePut("");
        List<String> res = ntry.doMetric();
        for (i = 0; i < res.size(); i++) {
            conn.linePut(res.get(i));
        }
        if (debugger.servPrometheusTraf) {
            logger.debug("tx " + res.size() + " lines");
        }
    }

    public void run() {
        try {
            doWork();
        } catch (Exception e) {
            logger.traceback(e);
        }
        conn.setClose();

    }

}
