package serv;

import cfg.cfgAll;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.zip.Deflater;
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
        "server prometheus .*! no metric .* labels",
        "server prometheus .*! metric .* addname -1 null",
        "server prometheus .*! metric .* skip 1",
        "server prometheus .*! no metric .* excluded",
        "server prometheus .*! metric .* column .* type gauge",
        "server prometheus .*! metric .* column .* split null null null",
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
            String a = "";
            if (met.lab != null) {
                a = " " + met.lab;
            }
            lst.add(mn + " name " + met.col + a);
            if (met.slab != null) {
                lst.add(mn + " labels " + met.slab);
            } else {
                lst.add(nn + " labels");
            }
            a = "";
            if (met.alab != null) {
                a = " " + met.alab;
            }
            lst.add(mn + " addname " + met.acol + " " + met.asep + a);
            lst.add(mn + " skip " + met.skp);
            if (met.exc) {
                lst.add(mn + " excluded");
            } else {
                lst.add(nn + " excluded");
            }
            for (int i = 0; i < met.reps.size(); i++) {
                servPrometheusRep rep = met.reps.get(i);
                lst.add(mn + " replace " + rep.src + " " + rep.trg);
            }
            for (int o = 0; o < met.cols.size(); o++) {
                servPrometheusCol col = met.cols.get(o);
                String cn = mn + " column " + col.num;
                a = "";
                if (col.lab != null) {
                    a = " " + col.lab;
                }
                lst.add(cn + " name " + col.nam + a);
                lst.add(cn + " type " + col.typ);
                lst.add(cn + " help " + col.hlp);
                lst.add(cn + " split " + col.splS + " " + col.splL + " " + col.splR);
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
            allMets = cmd.word();
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
            met.prep = cmd.word();
            return false;
        }
        if (s.equals("labels")) {
            if (negated) {
                met.slab = null;
            } else {
                met.slab = cmd.word();
            }
            return false;
        }
        if (s.equals("name")) {
            met.col = bits.str2num(cmd.word());
            if (cmd.size() < 1) {
                met.lab = null;
            } else {
                met.lab = cmd.word();
            }
            return false;
        }
        if (s.equals("addname")) {
            if (negated) {
                met.acol = -1;
                met.asep = null;
                met.alab = null;
                return false;
            }
            met.acol = bits.str2num(cmd.word());
            met.asep = cmd.word();
            if (cmd.size() < 1) {
                met.alab = null;
            } else {
                met.alab = cmd.word();
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
            col.nam = cmd.word();
            if (cmd.size() < 1) {
                col.lab = null;
            } else {
                col.lab = cmd.word();
            }
            return false;
        }
        if (s.equals("type")) {
            if (negated) {
                col.typ = "gauge";
            } else {
                col.typ = cmd.word();
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
        if (s.equals("split")) {
            if (negated) {
                col.splS = null;
                col.splL = null;
                col.splR = null;
            } else {
                col.splS = cmd.word();
                col.splL = cmd.word();
                col.splR = cmd.word();
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
        l.add("3 4      labels                   static labels");
        l.add("4 .        <str>                  name");
        l.add("3 4      name                     name column number");
        l.add("4 5,.      <num>                  column number");
        l.add("5 .          <str>                label");
        l.add("3 4      addname                  add name column number");
        l.add("4 5        <num>                  column number");
        l.add("5 6,.        <str>                separator, * means empty");
        l.add("6 .            <str>              label");
        l.add("3 .      excluded                 exclude from whole reporting");
        l.add("3 4      skip                     rows to skip");
        l.add("4 .        <num>                  lines to skip");
        l.add("3 4      replace                  define replaces in name");
        l.add("4 5        <str>                  string to replace");
        l.add("5 .          <str>                replacement string");
        l.add("3 4      column                   define column to export");
        l.add("4 5        <num>                  number");
        l.add("5 6,.        name                 set metric name");
        l.add("6 7,.          <str>              metric name, * means empty");
        l.add("7 .              <str>            label");
        l.add("5 6          type                 set metric type");
        l.add("6 .            gauge              gauge");
        l.add("6 .            counter            counter");
        l.add("5 6          help                 set metric help");
        l.add("6 6,.          <str>              metric help");
        l.add("5 6          replace              define replaces in value");
        l.add("6 7            <str>              string to replace");
        l.add("7 .              <str>            replacement string");
        l.add("5 6          split                define split of value");
        l.add("6 7            <str>              delimiter");
        l.add("7 8              <str>            first label");
        l.add("8 .                <str>          second label");
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.setTime(120000);
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

    public int col;

    public String slab;

    public String lab;

    public int acol = -1;

    public String asep;

    public String alab;

    public int skp = 1;

    public boolean exc;

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
        pip.setTime(120000);
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

    private void doMetric(List<String> lst, String nb, String labs, String val) {
        if (labs.length() > 0) {
            labs = "{" + labs.substring(1, labs.length()) + "}";
        }
        lst.add(nb + labs + " " + val);
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
        List<String> smt = new ArrayList<String>();
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
            String na = prep;
            String nc = cl.get(col);
            String nd = "";
            if ((acol >= 0) && (acol < cls)) {
                String a = asep;
                if (asep.equals("*")) {
                    a = "";
                }
                nd = a + cl.get(acol);
            }
            for (int i = 0; i < reps.size(); i++) {
                servPrometheusRep rep = reps.get(i);
                na = na.replaceAll(rep.src, rep.trg);
                nc = nc.replaceAll(rep.src, rep.trg);
                nd = nd.replaceAll(rep.src, rep.trg);
            }
            if (lab == null) {
                na += nc;
                na += nd;
            }
            for (int o = 0; o < cols.size(); o++) {
                servPrometheusCol cc = cols.get(o);
                if (cl.size() <= cc.num) {
                    continue;
                }
                String nb = na;
                if (!cc.nam.equals("*")) {
                    nb += cc.nam;
                }
                String labs = "";
                if (slab != null) {
                    labs += "," + slab;
                }
                if (lab != null) {
                    labs += "," + lab + "\"" + nc + "\"";
                }
                if (alab != null) {
                    labs += "," + alab + "\"" + nd + "\"";
                }
                if (cc.lab != null) {
                    labs += "," + cc.lab;
                }
                if (smt.indexOf(nb) < 0) {
                    String h;
                    if (cc.hlp == null) {
                        h = " column " + cc.num + " of " + cmd;
                    } else {
                        h = " " + cc.hlp;
                    }
                    lst.add("# HELP " + nb + h);
                    lst.add("# TYPE " + nb + " " + cc.typ);
                    smt.add(nb);
                }
                String a = cl.get(cc.num);
                for (int i = 0; i < cc.reps.size(); i++) {
                    servPrometheusRep rep = cc.reps.get(i);
                    a = a.replaceAll(rep.src, rep.trg);
                }
                if (cc.splS == null) {
                    doMetric(lst, nb, labs, a);
                    continue;
                }
                int i = a.indexOf(cc.splS);
                if (i < 0) {
                    doMetric(lst, nb, labs, a);
                    continue;
                }
                doMetric(lst, nb, labs + "," + cc.splL, a.substring(0, i));
                doMetric(lst, nb, labs + "," + cc.splR, a.substring(i + cc.splS.length(), a.length()));
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

    public String lab;

    public String typ = "gauge";

    public String hlp;

    public String splS;

    public String splL;

    public String splR;

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

    private boolean gotCompr;

    public servPrometheusConn(servPrometheus parent, pipeSide pipe) {
        lower = parent;
        conn = pipe;
        new Thread(this).start();
    }

    private void sendReply(String hdr, List<String> res) {
        if (debugger.servPrometheusTraf) {
            logger.debug("tx " + hdr);
        }
        conn.linePut("HTTP/1.1 " + hdr);
        conn.linePut("Content-Type: text/plain");
        conn.linePut("Date: " + bits.time2str(cfgAll.timeZoneName, bits.getTime(), 4));
        if (res == null) {
            conn.linePut("Content-Length: 0");
            conn.linePut("");
            return;
        }
        byte[] buf1 = new byte[0];
        byte[] buf3 = new byte[1];
        buf3[0] = 10;
        for (int i = 0; i < res.size(); i++) {
            buf1 = bits.byteConcat(buf1, res.get(i).getBytes());
            buf1 = bits.byteConcat(buf1, buf3);
        }
        if (!gotCompr) {
            conn.linePut("Content-Length: " + buf1.length);
            conn.linePut("");
            conn.morePut(buf1, 0, buf1.length);
            return;
        }
        byte[] buf2 = new byte[buf1.length];
        Deflater cmp = new Deflater(Deflater.DEFAULT_COMPRESSION, true);
        cmp.setInput(buf1);
        cmp.finish();
        int i = cmp.deflate(buf2);
        if (i >= buf2.length) {
            conn.linePut("Content-Length: " + buf1.length);
            conn.linePut("");
            conn.morePut(buf1, 0, buf1.length);
            return;
        }
        buf3 = servHttp.getGzipHdr();
        byte[] buf4 = servHttp.getGzipTrl(buf1);
        conn.linePut("Content-Encoding: gzip");
        conn.linePut("Content-Length: " + (buf3.length + i + buf4.length));
        conn.linePut("");
        conn.morePut(buf3, 0, buf3.length);
        conn.morePut(buf2, 0, i);
        conn.morePut(buf4, 0, buf4.length);
    }

    private boolean doWork() {
        conn.lineRx = pipeSide.modTyp.modeCRtryLF;
        conn.lineTx = pipeSide.modTyp.modeCRLF;
        String gotCmd = conn.lineGet(1);
        gotCompr = false;
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
            a = a.toLowerCase();
            if (a.startsWith("accept-encoding")) {
                if (a.indexOf("gzip") >= 0) {
                    gotCompr = true;
                }
                continue;
            }
        }
        uniResLoc gotUrl = new uniResLoc();
        int i = gotCmd.toLowerCase().lastIndexOf(" http/");
        if (i > 0) {
            gotCmd = gotCmd.substring(0, i);
        }
        i = gotCmd.indexOf(" ");
        if (i < 0) {
            sendReply("501 bad request", null);
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
            sendReply("404 no such metric", null);
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
