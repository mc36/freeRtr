package user;

import addr.addrIP;
import addr.addrMac;
import cfg.cfgAll;
import cfg.cfgInit;
import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import pipe.pipeLine;
import pipe.pipeProgress;
import pipe.pipeShell;
import pipe.pipeSide;
import serv.servHttp;
import util.bits;
import util.cmds;
import util.version;

/**
 * process image tester
 *
 * @author matecsaba
 */
public class userTester {

    private pipeProgress rdr;

    private String path = "../cfg/";

    private String url = path;

    private String remoteF = null;

    private List<String> remoteD = null;

    private addrIP remoteA;

    private addrIP remoteL;

    private int remoteP;

    private String remoteS;

    private String otherI = null;

    private String otherN = null;

    private int otherM = 0;

    private List<userTesterCap> capture = new ArrayList<userTesterCap>();

    private boolean debug = false;

    private boolean summary = false;

    private boolean window = false;

    private boolean config = false;

    private boolean randord = false;

    private String release = "unknown";

    private int maxTry = 1;

    private int reapply = 0;

    /**
     * do the work
     *
     * @param cmd command to do
     */
    public void doer(cmds cmd) {
        rdr = new pipeProgress(cmd.pipe);
        int mem = 256;
        String beg = cmd.word();
        String jvn = "java";
        String jvp = " XmxZZZm -jar " + version.getFileName();
        for (;;) {
            String s = cmd.word();
            if (s.length() < 1) {
                break;
            }
            if (s.equals("debug")) {
                debug = true;
            }
            if (s.equals("nodebug")) {
                debug = false;
            }
            if (s.equals("summary")) {
                summary = true;
            }
            if (s.equals("nosummary")) {
                summary = false;
            }
            if (s.equals("window")) {
                window = true;
            }
            if (s.equals("nowindow")) {
                window = false;
            }
            if (s.equals("reapply")) {
                reapply = 1;
            }
            if (s.equals("noreapply")) {
                reapply = 0;
            }
            if (s.equals("config")) {
                config = true;
            }
            if (s.equals("noconfig")) {
                config = false;
            }
            if (s.equals("randord")) {
                randord = true;
            }
            if (s.equals("norandord")) {
                randord = false;
            }
            if (s.equals("retry")) {
                maxTry = 16;
            }
            if (s.equals("noretry")) {
                maxTry = 1;
            }
            if (s.equals("mem")) {
                mem = bits.str2num(cmd.word());
            }
            if (s.equals("path")) {
                path = cmd.word();
            }
            if (s.equals("url")) {
                url = cmd.word();
            }
            if (s.equals("remote")) {
                remoteF = cmd.word();
            }
            if (s.equals("noremote")) {
                remoteF = null;
            }
            if (s.equals("other")) {
                otherI = cmd.word();
                otherN = cmd.word();
                otherM = bits.str2num(cmd.word());
            }
            if (s.equals("noother")) {
                otherI = null;
                otherN = null;
                otherM = 0;
            }
            if (s.equals("capture")) {
                userTesterCap cap = new userTesterCap();
                cap.rtr = cmd.word();
                cap.ifc = cmd.word();
                capture.add(cap);
            }
            if (s.equals("nocapture")) {
                capture.clear();
            }
            if (s.equals("openjdk6")) {
                jvn = "/usr/lib/jvm/java-6-openjdk-amd64/bin/java";
            }
            if (s.equals("openjdk7")) {
                jvn = "/usr/lib/jvm/java-7-openjdk-amd64/bin/java";
            }
            if (s.equals("openjdk8")) {
                jvn = "/usr/lib/jvm/java-8-openjdk-amd64/bin/java";
            }
            if (s.equals("openjdk9")) {
                jvn = "/usr/lib/jvm/java-9-openjdk-amd64/bin/java";
            }
            if (s.equals("openjdk10")) {
                jvn = "/usr/lib/jvm/java-10-openjdk-amd64/bin/java";
            }
            if (s.equals("openjdk11")) {
                jvn = "/usr/lib/jvm/java-11-openjdk-amd64/bin/java";
            }
            if (s.equals("openjdk12")) {
                jvn = "/usr/lib/jvm/java-12-openjdk-amd64/bin/java";
            }
            if (s.equals("openjdk13")) {
                jvn = "/usr/lib/jvm/java-13-openjdk-amd64/bin/java";
            }
            if (s.equals("ikvm")) {
                jvn = "/usr/bin/ikvm";
                mem = 0;
            }
            if (s.equals("gcj")) {
                jvn = "/usr/lib/jvm/java-gcj/bin/java";
                mem = 0;
            }
            if (s.equals("binary")) {
                jvn = "./rtr.bin";
                jvp = "";
                mem = 0;
            }
        }
        String s = "-Xmx" + mem + "m";
        if (mem < 1) {
            s = "";
        }
        jvp = jvp.replaceAll("XmxZZZm", s);
        userTesterPrc prc = new userTesterPrc(rdr, "rtr", jvn + jvp + " show version");
        release = prc.getLine();
        prc.stopNow();
        if (beg.length() < 2) {
            beg = "";
        }
        List<userTesterFtr> ned = new ArrayList<userTesterFtr>();
        try {
            File fils[] = new File(path).listFiles();
            if (fils == null) {
                return;
            }
            for (int i = 0; i < fils.length; i++) {
                if (fils[i].isDirectory()) {
                    continue;
                }
                s = fils[i].getName();
                if (!s.endsWith(".tst")) {
                    continue;
                }
                if (!s.startsWith(beg)) {
                    continue;
                }
                userTesterFtr ftr = new userTesterFtr();
                ftr.fil = s;
                ftr.ret = maxTry;
                ned.add(ftr);
            }
        } catch (Exception e) {
            return;
        }
        Collections.sort(ned, new userTesterFtr());
        if (config) {
            maxTry = 1;
            window = false;
        }
        if (remoteF != null) {
            remoteD = bits.txt2buf(path + remoteF);
            remoteA = new addrIP();
            remoteA.fromString(remoteD.remove(0));
            remoteP = bits.str2num(remoteD.remove(0));
            remoteL = new addrIP();
            remoteL.fromString(remoteD.remove(0));
            remoteS = remoteD.remove(0);
        } else {
            remoteD = new ArrayList<String>();
        }
        rdr.debugStat("jvm=" + jvn + jvp);
        rdr.debugStat("release=" + release);
        rdr.debugStat("url=" + url);
        rdr.debugStat("path=" + path);
        rdr.debugStat("debug=" + debug);
        rdr.debugStat("summary=" + summary);
        rdr.debugStat("window=" + window);
        rdr.debugStat("config=" + config);
        rdr.debugStat("reapply=" + reapply);
        rdr.debugStat("randord=" + randord);
        rdr.debugStat("retry=" + maxTry);
        rdr.debugStat("other=" + otherI + " " + otherN + " " + otherM);
        rdr.debugStat("remote=" + remoteL + " " + remoteP + " " + remoteA);
        rdr.debugStat("capture=" + capture.size());
        rdr.debugStat("files=" + ned.size());
        List<userTesterFtr> don = new ArrayList<userTesterFtr>();
        int err = 0;
        int ret = 0;
        int trc = 0;
        long tim = bits.getTime();
        for (; ned.size() > 0;) {
            int cur = 0;
            if (randord) {
                cur = bits.random(0, ned.size());
            }
            userTesterFtr ftr = ned.get(cur);
            s = ftr.fil;
            ftr.ran++;
            final String sep = " ---------- ";
            rdr.debugRes(sep + "err=" + err + " trc=" + trc + " ret=" + ret + " don=" + don.size() + " ned=" + ned.size() + " tot=" + (don.size() + ned.size()) + " tim=" + bits.timePast(tim) + sep + s + sep);
            userTesterOne lt = new userTesterOne();
            bits.sleep(1000);
            lt = new userTesterOne();
            lt.debug = debug;
            lt.config = config;
            lt.reapply = reapply;
            lt.jvm = jvn + jvp;
            lt.otherI = otherI;
            lt.otherN = otherN;
            lt.otherM = otherM;
            lt.remoteD = remoteD;
            lt.remoteA = remoteA;
            lt.remoteL = remoteL;
            lt.remoteP = remoteP;
            lt.remoteS = remoteS;
            lt.capture = capture;
            if (window) {
                lt.window += "w";
            }
            lt.rdr = rdr;
            lt.doTest(path, s);
            lt.stopAll();
            trc += lt.traces;
            rdr.debugRes(lt.getCsv());
            boolean del = lt.getSucc();
            if (!del) {
                ftr.ret--;
                del |= ftr.ret < 1;
                ret++;
            }
            if (!del) {
                continue;
            }
            ned.remove(cur);
            if (!lt.getSucc()) {
                err++;
            }
            ftr.res = lt.getSucc();
            ftr.htm = lt.getHtm(url);
            ftr.csv = lt.getCsv();
            ftr.ftr = lt.getFet();
            don.add(ftr);
        }
        Collections.sort(don, new userTesterFtr());
        for (int i = 0; i < don.size(); i++) {
            userTesterFtr ftr = don.get(i);
            if (!ftr.res) {
                rdr.debugStat("failed: " + ftr.csv);
                continue;
            }
            if (ftr.ran > 1) {
                rdr.debugStat("retried " + ftr.ran + "x: " + ftr.csv);
                continue;
            }
        }
        String a = bits.time2str(cfgAll.timeZoneName, bits.getTime() + cfgAll.timeServerOffset, 3) + ", took " + bits.timePast(tim) + " on " + don.size() + " cases, " + err + " failed, " + trc + " traces, " + ret + " retries";
        rdr.debugStat("summary: " + a);
        if (!summary) {
            return;
        }
        rdr.debugStat("writing summary");
        List<String> txt = bits.txt2buf("../todo.txt");
        if (txt == null) {
            txt = new ArrayList<String>();
        }
        for (int i = 0; i < txt.size(); i++) {
            userTesterFtr ftr = new userTesterFtr();
            ftr.ftr = "todo: " + txt.get(i);
            don.add(ftr);
        }
        txt = new ArrayList<String>();
        txt.add(servHttp.html401tr);
        txt.add("<html><head><title>tester</title></head><body bgcolor=black text=white link=white vlink=white alink=white>");
        txt.add("release: " + release + "<br/>");
        txt.add("tested: " + a + "<br/>");
        txt.add("jvm: " + jvn + jvp + "<br/>");
        txt.add("<br/>");
        txt.add("<table border=1><tr><td><b>file</b></td><td><b>code</b></td><td><b>test</b></td><td><b>stage</b></td><td><b>command</b></td></tr>");
        txt.addAll(features2list(don, 3));
        txt.add("</table></body></html>");
        bits.buf2txt(true, txt, "rtr" + beg + ".html");
        txt = new ArrayList<String>();
        txt.add("file;code;test;stage;command");
        txt.add("-;-;" + release + ";-;-");
        txt.add("-;-;" + a + ";-;-");
        txt.add("-;-;" + jvn + jvp + ";-;-");
        txt.addAll(features2list(don, 4));
        bits.buf2txt(true, txt, "rtr" + beg + ".csv");
        a = "rtr" + beg + ".ftr";
        txt = bits.txt2buf(a);
        if (txt == null) {
            txt = new ArrayList<String>();
        }
        bits.buf2txt(true, features2list(don, 2), a);
        txt = userFilter.getDiffs(txt, features2list(don, 2));
        if (txt.size() < 1) {
            return;
        }
        txt.add(0, "---------------------------------- " + bits.time2str(cfgAll.timeZoneName, tim + cfgAll.timeServerOffset, 3));
        bits.buf2txt(false, txt, "../changelog.txt");
    }

    private static List<String> features2list(List<userTesterFtr> ftr, int mod) {
        List<String> res = new ArrayList<String>();
        for (int i = 0; i < ftr.size(); i++) {
            String a = ftr.get(i).getter(mod);
            if (a == null) {
                continue;
            }
            res.add(a);
        }
        return res;
    }

}

class userTesterFtr implements Comparator<userTesterFtr> {

    public String fil;

    public int ret;

    public int ran;

    public boolean res;

    public String ftr;

    public String htm;

    public String csv;

    public int compare(userTesterFtr o1, userTesterFtr o2) {
        return o1.fil.compareTo(o2.fil);
    }

    public String getter(int mod) {
        switch (mod) {
            case 1: // filename
                return fil;
            case 2: // feature
                return ftr;
            case 3: // html
                return htm;
            case 4: // csv
                return csv;
            case 5: // result
                return "" + res;
            case 6: // retries
                return "" + ret;
            default:
                return null;
        }
    }

}

class userTesterPrc {

    public String name;

    public Process proc;

    public pipeShell shell;

    public pipeSide pipe;

    public pipeProgress rdr;

    public boolean debug;

    public String syncr = "!!!hello there!!!";

    public userTesterPrc(pipeProgress reader, String nam, String command) {
        name = nam;
        rdr = reader;
        pipeLine pl = new pipeLine(32768, false);
        final int tim = 600 * 1000;
        pipe = pl.getSide();
        pipe.timeout = tim;
        shell = pipeShell.exec(pipe, command, null, true, false);
        pipe = pl.getSide();
        pipe.timeout = tim;
        pipe.lineRx = pipeSide.modTyp.modeCRorLF;
        pipe.lineTx = pipeSide.modTyp.modeCR;
        rdr.debugStat(nam + ": starting process");
    }

    public void stopNow() {
        rdr.debugStat(name + ": stopping process");
        shell.kill(0);
        shell.waitFor();
        pipe.setClose();
    }

    public void putChar(int i) {
        byte[] buf = new byte[1];
        buf[0] = (byte) i;
        pipe.blockingPut(buf, 0, buf.length);
    }

    public String getLine() {
        String s = pipe.lineGet(0x11);
        if (s == null) {
            return null;
        }
        if (s.length() < 1) {
            if (pipe.isClosed() != 0) {
                return null;
            }
            return "";
        }
        if (debug) {
            rdr.debugRx(name + ": " + s);
        }
        return s;
    }

    public void putLine(String s) {
        if (debug) {
            rdr.debugTx(name + ": " + s);
        }
        pipe.linePut(s);
    }

    public int doPing(String s) {
        putLine("ping " + s);
        for (;;) {
            s = getLine();
            if (s == null) {
                return -1;
            }
            s = s.toLowerCase();
            if (s.startsWith("result=")) {
                break;
            }
        }
        int i = s.indexOf("=");
        if (i < 0) {
            return -1;
        }
        s = s.substring(i + 1, s.length());
        i = s.indexOf("%");
        s = s.substring(0, i);
        return bits.str2num(s);
    }

    public boolean morePings(String s, int need, int round) {
        rdr.debugStat(name + ": pinging " + s + ".");
        rdr.setMax(round);
        int i = -1;
        for (int rnd = 0; rnd < round; rnd++) {
            rdr.setCurr(rnd);
            i = doPing(s);
            if (i < 0) {
                return true;
            }
            if (i == need) {
                return false;
            }
            bits.sleep(1000);
        }
        rdr.debugStat(name + ": test failed: got " + i + ", expected " + need);
        return true;
    }

    public List<String> getOutput(String s) {
        String beg = "!begin-command-" + s;
        String end = "!end-command-" + s;
        rdr.debugStat(name + ": output " + s + ".");
        putLine(beg);
        putLine(s);
        putLine(end);
        List<String> res = new ArrayList<String>();
        for (;;) {
            String a = getLine();
            if (a == null) {
                break;
            }
            a = bits.trimE(a);
            if (a.length() < 1) {
                continue;
            }
            if (a.endsWith(beg)) {
                res.clear();
                continue;
            }
            if (a.endsWith(end)) {
                break;
            }
            res.add(a);
        }
        return res;
    }

    public void doSync() {
        putLine(syncr);
        for (;;) {
            String a = getLine();
            if (a == null) {
                return;
            }
            a = bits.trimE(a);
            if (a.indexOf(syncr) >= 0) {
                break;
            }
        }
    }

    public void applyCfg(List<String> cfg) {
        doSync();
        rdr.setMax(cfg.size());
        for (int i = 0; i < cfg.size(); i++) {
            if ((i % 5) == 0) {
                doSync();
            }
            putLine(cfg.get(i));
            getLine();
            rdr.setCurr(i);
        }
        doSync();
    }

}

class userTesterOne {

    public static final String path = "../binTmp/zzz-";

    public String fileName;

    public String testName = "unnamed";

    public int testRes = 1;

    public boolean debug;

    public boolean config;

    public int reapply;

    public String jvm;

    public List<userTesterCap> capture;

    public List<String> remoteD = null;

    public addrIP remoteA;

    public addrIP remoteL;

    public int remoteP;

    public String remoteS;

    public String otherI;

    public String otherN;

    public int otherM;

    public String window = "c";

    public pipeProgress rdr;

    public List<List<String>> shows = new ArrayList<List<String>>();

    public int traces;

    private cmds cmd = new cmds("", "");

    private String stage = "init";

    private List<userTesterPrc> procs = new ArrayList<userTesterPrc>();

    private List<String> lineD;

    private int lineN = -1;

    public userTesterOne() {
    }

    public boolean getSucc() {
        return testRes == 0;
    }

    public String getFet() {
        String qc;
        if (testRes != 0) {
            qc = cmds.negated + "work: ";
        } else {
            qc = "qc pass: ";
        }
        return qc + testName;
    }

    public String getCsv() {
        return fileName + ";" + testRes + ";" + testName + ";" + stage + ";" + cmd.getOriginal();
    }

    public String getHtm(String url) {
        return "<tr><td><a href=\"" + url + fileName + "\">" + fileName + "</a></td><td>" + testRes + "</td><td>" + testName + "</td><td>" + stage + "</td><td>" + cmd.getOriginal() + "</td></tr>";
    }

    public String getLin() {
        if (lineD == null) {
            return null;
        }
        lineN++;
        if (lineN >= lineD.size()) {
            return null;
        }
        return lineD.get(lineN);
    }

    public void stopAll() {
        for (int i = 0; i < procs.size(); i++) {
            userTesterPrc prc = procs.get(i);
            prc.stopNow();
            List<String> log = bits.txt2buf(getLogName(prc.name, 1));
            bits.buf2txt(false, log, getLogName(prc.name, 2));
            userFlash.delete(getLogName(prc.name, 1));
            if (checkLogs(log)) {
                continue;
            }
            bits.buf2txt(false, log, getLogName(prc.name, 3));
            traces++;
        }
    }

    public userTesterPrc getPrc(String s) {
        for (int i = 0; i < procs.size(); i++) {
            userTesterPrc p = procs.get(i);
            if (s.equals(p.name)) {
                return p;
            }
        }
        testRes = 4;
        return null;
    }

    private void success() {
        testRes = 0;
        stage = "successfully";
        cmd = new cmds("", "finished");
    }

    public void doTest(String pt, String fn) {
        testRes = 2;
        fileName = fn;
        lineD = bits.txt2buf(pt + fn);
        stage = "run";
        for (;;) {
            if (testRes != 2) {
                break;
            }
            fn = getLin();
            if (fn == null) {
                success();
                break;
            }
            cmd = new cmds("", fn);
            if (debug) {
                rdr.debugStat("cmd> " + fn);
            }
            doLine();
        }
    }

    private String repairHwCfg(String s) {
        for (;;) {
            int i = s.indexOf("$");
            if (i < 0) {
                break;
            }
            String a = s.substring(i + 1, s.length());
            s = s.substring(0, i);
            i = a.indexOf("$");
            String b = a.substring(i + 1, a.length());
            a = a.substring(0, i);
            if (a.startsWith("rem")) {
                i = bits.str2num(a.substring(3, a.length())) + remoteP;
                s = s + remoteL + " " + i + " " + remoteA + " " + i + b;
                continue;
            }
            i = bits.str2num(a.substring(0, a.length() - 1)) * 4;
            i += 24000;
            if (a.substring(a.length() - 1, a.length()).equals("b")) {
                i += 1;
            }
            s = s + "127.0.0.1 " + i + b;
        }
        return s;
    }

    public boolean checkLogs(List<String> l) {
        if (l == null) {
            return true;
        }
        for (int i = 0; i < l.size(); i++) {
            String a = l.get(i).toLowerCase();
            if (a.indexOf("executeswcommand") >= 0) {
                return false;
            }
            if (a.indexOf("exception") >= 0) {
                return false;
            }
            if (a.indexOf("traceback") >= 0) {
                return false;
            }
        }
        return true;
    }

    public String getLogName(String rn, int rtr) {
        String s = "log";
        switch (rtr) {
            case 1:
                s = "run";
                break;
            case 2:
                s = "txt";
                break;
            case 3:
                s = "err";
                break;
        }
        return path + "log-" + rn + "." + s;
    }

    public void doLine() {
        String s = cmd.word();
        if (s.length() < 1) {
            return;
        }
        if (s.startsWith("!")) {
            return;
        }
        if (s.equals("description")) {
            testName = cmd.getRemaining();
            rdr.debugRes("test: " + testName);
            return;
        }
        if (s.equals("exit")) {
            success();
            return;
        }
        if (s.equals("output")) {
            String beg = "<!>show:";
            List<String> cfg = new ArrayList<String>();
            for (;;) {
                s = getLin();
                if (s.equals("!")) {
                    break;
                }
                if (!s.startsWith(beg)) {
                    cfg.add(s);
                    continue;
                }
                s = s.substring(beg.length(), s.length());
                cfg.addAll(shows.get(bits.str2num(s)));
            }
            bits.buf2txt(true, cfg, cmd.getRemaining());
            return;
        }
        if (s.equals("addremote")) {
            if (remoteA == null) {
                success();
                return;
            }
            String rn = cmd.word();
            List<String> cfg = new ArrayList<String>();
            for (;;) {
                s = getLin();
                if (s.equals("!")) {
                    break;
                }
                s = repairHwCfg(s);
                cfg.add(s);
            }
            s = "telnet " + remoteA + " " + remoteP;
            cfg.add("!" + s);
            bits.buf2txt(true, cfg, path + rn + "-" + cfgInit.hwCfgEnd);
            userTesterPrc p = new userTesterPrc(rdr, rn, s);
            p.debug = debug;
            p.syncr = remoteS;
            procs.add(p);
            cfg = new ArrayList<String>();
            cfg.add("hostname " + rn);
            for (;;) {
                s = getLin();
                if (s.equals("!")) {
                    break;
                }
                cfg.add(s);
            }
            bits.buf2txt(true, cfg, path + rn + "-" + cfgInit.swCfgEnd);
            p.applyCfg(remoteD);
            p.applyCfg(cfg);
            return;
        }
        if (s.equals("addother")) {
            if (otherI == null) {
                success();
                return;
            }
            String rn = cmd.word();
            List<String> cfg = new ArrayList<String>();
            for (;;) {
                s = getLin();
                if (s.equals("!")) {
                    break;
                }
                s = repairHwCfg(s);
                cfg.add(s);
            }
            s = "qemu-system-x86_64 -monitor none -serial stdio -nographic -no-reboot -enable-kvm -smp cores=2,threads=1,sockets=1 -hda " + otherI + " -m " + otherM;
            for (int i = 0; i < cfg.size(); i++) {
                String a = cfg.get(i);
                cmd = new cmds("hw", a);
                a = cmd.word();
                if (!a.equals("int")) {
                    continue;
                }
                cmd.word();
                a = cmd.word();
                if (!a.equals("eth")) {
                    continue;
                }
                addrMac mac = new addrMac();
                mac.fromString(cmd.word());
                cmd.word();
                int lp = bits.str2num(cmd.word());
                cmd.word();
                int rp = bits.str2num(cmd.word());
                int vl = i + 1;
                s += " -netdev socket,id=n" + vl + ",udp=:" + rp + ",localaddr=:" + lp + " -device " + otherN + ",netdev=n" + vl + ",mac=" + mac.toEmuStr();
            }
            cfg.add("!" + s);
            bits.buf2txt(true, cfg, path + rn + "-" + cfgInit.hwCfgEnd);
            userTesterPrc p = new userTesterPrc(rdr, rn, s);
            p.debug = debug;
            procs.add(p);
            cfg = new ArrayList<String>();
            cfg.add("hostname " + rn);
            for (;;) {
                s = getLin();
                if (s.equals("!")) {
                    break;
                }
                cfg.add(s);
            }
            bits.buf2txt(true, cfg, path + rn + "-" + cfgInit.swCfgEnd);
            int round = 500;
            rdr.setMax(round);
            for (int rnd = 0; rnd < round; rnd++) {
                if ((rnd % 10) == 0) {
                    p.putChar(13);
                }
                String a = p.getLine();
                rdr.setCurr(rnd);
                if (a == null) {
                    return;
                }
                a = a.toLowerCase().trim();
                if (a.equals("router>")) {
                    break;
                }
            }
            p.putLine("enable");
            p.putLine("configure terminal");
            p.putLine("no logging console");
            p.applyCfg(cfg);
            return;
        }
        if (s.equals("addrouter")) {
            String rn = cmd.word();
            List<String> cfg = new ArrayList<String>();
            for (;;) {
                s = getLin();
                if (s.equals("!")) {
                    break;
                }
                s = repairHwCfg(s);
                cfg.add(s);
            }
            bits.buf2txt(true, cfg, path + rn + "-" + cfgInit.hwCfgEnd);
            cfg = new ArrayList<String>();
            cfg.add("");
            cfg.add("");
            cfg.add(fileName + " - " + rn + " - " + testName + ":");
            bits.buf2txt(true, cfg, getLogName(rn, 1));
            cfg = new ArrayList<String>();
            cfg.add("hostname " + rn);
            cfg.add("logging file debug " + getLogName(rn, 1));
            for (;;) {
                s = getLin();
                if (s.equals("!")) {
                    break;
                }
                cfg.add(s);
            }
            bits.buf2txt(true, cfg, path + rn + "-" + cfgInit.swCfgEnd);
            userTesterPrc p = new userTesterPrc(rdr, rn, jvm + " router" + window + " " + path + rn + "-");
            p.debug = debug;
            procs.add(p);
            for (int i = 0; i < capture.size(); i++) {
                userTesterCap cap = capture.get(i);
                if (!rn.equals(cap.rtr)) {
                    continue;
                }
                p.putLine("packet capture " + cap.ifc + " " + path + "log-" + rn + "-" + cap.ifc + ".pcap");
            }
            p.putLine("terminal length 0");
            p.putLine("terminal table fancy");
            p.putLine("write");
            p.putLine("reload in 10");
            p.putLine("y");
            for (int i = 0; i < reapply; i++) {
                p.putLine("configure reapply");
            }
            if (config) {
                p.putLine("reload force");
            }
            return;
        }
        if (s.equals("stage")) {
            stage = cmd.getRemaining();
            return;
        }
        if (s.equals("sleep")) {
            int i = bits.str2num(cmd.word());
            if (i < 1) {
                i = 1;
            }
            bits.sleep(i * 1000);
            return;
        }
        userTesterPrc p = getPrc(s);
        if (p == null) {
            return;
        }
        s = cmd.word();
        if (s.equals("sync")) {
            p.doSync();
            return;
        }
        if (s.equals("send")) {
            p.putLine(cmd.getRemaining());
            return;
        }
        if (s.equals("char")) {
            p.putChar(bits.str2num(cmd.word()));
            return;
        }
        if (s.equals("read")) {
            for (;;) {
                s = p.getLine();
                if (s == null) {
                    return;
                }
                if (s.indexOf(cmd.getRemaining()) >= 0) {
                    break;
                }
            }
            return;
        }
        if (s.equals("ping")) {
            cmd = new cmds("", "100 1 " + cmd.getRemaining());
            pingTest(p, false);
            return;
        }
        if (s.equals("mping")) {
            pingTest(p, false);
            return;
        }
        if (s.equals("tping")) {
            pingTest(p, true);
            return;
        }
        if (s.equals("output")) {
            List<String> buf = p.getOutput(cmd.getRemaining());
            shows.add(buf);
            return;
        }
        testRes = 5;
    }

    private void pingTest(userTesterPrc p, boolean chk) {
        int ned = bits.str2num(cmd.word());
        int rnd = bits.str2num(cmd.word());
        if (!chk) {
            p.morePings(cmd.getRemaining(), ned, rnd);
            return;
        }
        if (!p.morePings(cmd.getRemaining(), ned, rnd)) {
            return;
        }
        testRes = 6;
    }

}

class userTesterCap {

    public String rtr;

    public String ifc;

    public userTesterCap() {

    }

}
