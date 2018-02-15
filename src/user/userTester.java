package user;

import addr.addrMac;
import cfg.cfgAll;
import cfg.cfgInit;
import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
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

    private String otherI = null;

    private int otherM = 0;

    private List<userTesterCap> capture = new ArrayList<userTesterCap>();

    private boolean debug = false;

    private boolean summary = false;

    private boolean window = false;

    private boolean config = false;

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
            if (s.equals("other")) {
                otherI = cmd.word();
                otherM = bits.str2num(cmd.word());
            }
            if (s.equals("noother")) {
                otherI = null;
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
        List<String> lf = new ArrayList<String>(); // list of files
        try {
            File fl[];
            fl = new File(path).listFiles();
            if (fl == null) {
                return;
            }
            for (int i = 0; i < fl.length; i++) {
                if (fl[i].isDirectory()) {
                    continue;
                }
                s = fl[i].getName();
                if (!s.endsWith(".tst")) {
                    continue;
                }
                if (!s.startsWith(beg)) {
                    continue;
                }
                lf.add(s);
            }
        } catch (Exception e) {
            return;
        }
        Collections.sort(lf);
        if (config) {
            maxTry = 1;
            window = false;
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
        rdr.debugStat("retry=" + maxTry);
        rdr.debugStat("other=" + otherI + " " + otherM);
        rdr.debugStat("capture=" + capture.size());
        rdr.debugStat("files=" + lf.size());
        long tim1 = bits.getTime();
        List<String> lh = new ArrayList<String>(); // html result
        lh.add(servHttp.html401tr);
        lh.add("<html><head><title>tester</title></head><body bgcolor=black text=white link=white vlink=white alink=white>");
        lh.add("release: " + release + "<br/>");
        lh.add("tested: bug");
        lh.add("jvm: " + jvn + jvp + "<br/>");
        lh.add("<br/>");
        lh.add("<table border=1><tr><td><b>file</b></td><td><b>code</b></td><td><b>test</b></td><td><b>stage</b></td><td><b>command</b></td></tr>");
        List<String> lc = new ArrayList<String>(); // csv result
        lc.add("file;code;test;stage;command");
        lc.add("-;-;" + release + ";-;-");
        lc.add("-;-;bug;-;-");
        List<String> lq = new ArrayList<String>(); // feature result
        int err = 0;
        for (int i = 0; i < lf.size(); i++) {
            s = lf.get(i);
            final String sep = " ---------- ";
            rdr.debugRes(sep + i + "/" + lf.size() + sep + s + sep);
            userTesterOne lt = new userTesterOne(); // list of tests
            for (int retry = 0; retry < maxTry; retry++) {
                bits.sleep(1000);
                lt = new userTesterOne();
                lt.debug = debug;
                lt.config = config;
                lt.reapply = reapply;
                lt.jvm = jvn + jvp;
                lt.otherI = otherI;
                lt.otherM = otherM;
                lt.capture = capture;
                if (window) {
                    lt.window += "w";
                }
                lt.rdr = rdr;
                lt.doTest(path, s);
                lt.stopAll();
                if (lt.getSucc()) {
                    break;
                }
                rdr.debugRes(sep + "retry " + retry + "/" + maxTry + sep);
            }
            if (!lt.getSucc()) {
                err++;
            }
            rdr.debugRes(lt.getCsv());
            lh.add(lt.getHtm(url));
            lc.add(lt.getCsv());
            lq.add(lt.getFet());
        }
        long tim2 = bits.getTime();
        rdr.debugStat("took " + bits.timePast(tim1) + ", " + err + " failed");
        String a = bits.time2str(cfgAll.timeZoneName, tim2 + cfgAll.timeServerOffset, 3) + ", took " + bits.timePast(tim1) + " on " + lf.size() + " cases";
        lh.set(3, "tested: " + a + "<br/>");
        lc.set(2, "-;-;" + a + ";-;-");
        if (!summary) {
            return;
        }
        rdr.debugStat("writing summary");
        lh.add("</table></body></html>");
        lq.addAll(bits.lst2pre(bits.txt2buf("../todo.txt"), "todo: ", false));
        bits.buf2txt(true, lh, "rtr" + beg + ".html");
        bits.buf2txt(true, lc, "rtr" + beg + ".csv");
        a = "rtr" + beg + ".ftr";
        lc = bits.txt2buf(a);
        if (lc == null) {
            lc = new ArrayList<String>();
        }
        bits.buf2txt(true, lq, a);
        lc = userFilter.getDiffs(lc, lq);
        if (lc.size() < 1) {
            return;
        }
        lc.add(0, "---------------------------------- " + bits.time2str(cfgAll.timeZoneName, tim1 + cfgAll.timeServerOffset, 3));
        bits.buf2txt(false, lc, "../changelog.txt");
    }

}

class userTesterPrc {

    public String name;

    public Process proc;

    public pipeShell shell;

    public pipeSide pipe;

    public pipeProgress rdr;

    public boolean debug;

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

    public void doSync(String str) {
        if (str == null) {
            str = "!!!hello there!!!";
        }
        putLine(str);
        for (;;) {
            String a = getLine();
            if (a == null) {
                return;
            }
            a = bits.trimE(a);
            if (a.indexOf(str) >= 0) {
                break;
            }
        }
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

    public String otherI;

    public int otherM;

    public String window = "c";

    public pipeProgress rdr;

    public List<List<String>> shows = new ArrayList<List<String>>();

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
            s = "qemu-system-x86_64 -monitor none -serial stdio -nographic -no-reboot -enable-kvm -hda " + otherI + " -m " + otherM;
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
                s += " -net nic,vlan=" + vl + ",macaddr=" + mac.toEmuStr() + " -net socket,vlan=" + vl + ",udp=:" + rp + ",localaddr=:" + lp;
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
            rdr.setMax(cfg.size());
            for (int i = 0; i < cfg.size(); i++) {
                if ((i % 5) == 0) {
                    p.doSync(null);
                }
                p.putLine(cfg.get(i));
                p.getLine();
                rdr.setCurr(i);
            }
            p.putLine("end");
            p.doSync(null);
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
            p.doSync(null);
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
