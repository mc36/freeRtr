package user;

import addr.addrIP;
import addr.addrMac;
import cfg.cfgAll;
import cfg.cfgInit;
import java.io.File;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import pipe.pipeLine;
import pipe.pipeProgress;
import pipe.pipeShell;
import pipe.pipeSide;
import serv.servHttp;
import tab.tabGen;
import util.bits;
import util.cmds;
import util.logger;
import util.syncInt;
import util.version;

/**
 * process image tester
 *
 * @author matecsaba
 */
public class userTester {

    /**
     * port base
     */
    protected final static int portBase = 34000;

    /**
     * slot increment
     */
    protected final static int portSlot = 200;

    private pipeProgress rdr;

    private cmds cmd;

    private String path = "../cfg/";

    private String discard = "^$";

    private String url = path;

    private int slot = 0;

    private int paralell = 0;

    private String persistF = null;

    private int persistP = 0;

    private List<String> persistD = null;

    private userTesterPrc persistC = null;

    private String remoteF = null;

    private List<String> remoteD = null;

    private addrIP remoteA = null;

    private addrIP remoteL = null;

    private int remoteP = 0;

    private String remoteS = null;

    private String otherF = null;

    private String otherI = null;

    private String otherN = null;

    private String otherW = null;

    private String otherS = null;

    private List<String> otherD = null;

    private int otherM = 0;

    private List<userTesterCap> capture = new ArrayList<userTesterCap>();

    private boolean summary = false;

    private boolean window = false;

    private boolean config = false;

    private boolean mdfile = false;

    private boolean randord = false;

    private String release = "unknown";

    private int maxTry = 1;

    private int reapply = 0;

    private String jvn = "java";

    private String jvp = " XmxZZZm -jar " + version.getFileName();

    private String beg = "-";

    private static final String sep = " ---------- ";

    private tabGen<userTesterFtr> needed = new tabGen<userTesterFtr>();

    private tabGen<userTesterFtr> finished = new tabGen<userTesterFtr>();

    private long started;

    private syncInt errored = new syncInt(0);

    private syncInt retries = new syncInt(0);

    private syncInt traces = new syncInt(0);

    private userTesterOne workers[];

    /**
     * do the work
     *
     * @param c command to do
     */
    public void doer(cmds c) {
        cmd = c;
        rdr = new pipeProgress(cmd.pipe);
        int mem = 256;
        beg = cmd.word();
        for (;;) {
            String s = cmd.word();
            if (s.length() < 1) {
                break;
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
                reapply = bits.str2num(cmd.word());
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
            if (s.equals("mdfile")) {
                mdfile = true;
            }
            if (s.equals("nomdfile")) {
                mdfile = false;
            }
            if (s.equals("randord")) {
                randord = true;
            }
            if (s.equals("norandord")) {
                randord = false;
            }
            if (s.equals("retry")) {
                maxTry = bits.str2num(cmd.word());
            }
            if (s.equals("noretry")) {
                maxTry = 1;
            }
            if (s.equals("paralell")) {
                paralell = bits.str2num(cmd.word());
            }
            if (s.equals("noparalell")) {
                paralell = 0;
            }
            if (s.equals("slot")) {
                slot = bits.str2num(cmd.word());
            }
            if (s.equals("noslot")) {
                slot = 0;
            }
            if (s.equals("mem")) {
                mem = bits.str2num(cmd.word());
            }
            if (s.equals("discard")) {
                discard = cmd.word();
            }
            if (s.equals("nodiscard")) {
                discard = "^$";
            }
            if (s.equals("path")) {
                path = cmd.word();
            }
            if (s.equals("url")) {
                url = cmd.word();
            }
            if (s.equals("persist")) {
                persistF = cmd.word();
            }
            if (s.equals("nopersist")) {
                persistF = null;
            }
            if (s.equals("remote")) {
                remoteF = cmd.word();
            }
            if (s.equals("noremote")) {
                remoteF = null;
            }
            if (s.equals("other")) {
                otherF = cmd.word();
            }
            if (s.equals("noother")) {
                otherF = null;
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
            if (s.equals("openjdk14")) {
                jvn = "/usr/lib/jvm/java-14-openjdk-amd64/bin/java";
            }
            if (s.equals("openjdk15")) {
                jvn = "/usr/lib/jvm/java-15-openjdk-amd64/bin/java";
            }
            if (s.equals("ikvm")) {
                jvn = "/usr/bin/ikvm";
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
        userTesterPrc prc = new userTesterPrc(rdr, slot, "version", jvn + jvp + " show version");
        release = prc.getLine();
        prc.stopNow();
        if (beg.length() < 2) {
            beg = "";
        }
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
                if (s.matches(discard)) {
                    continue;
                }
                userTesterFtr ftr = new userTesterFtr(s);
                ftr.ret = maxTry;
                needed.add(ftr);
            }
        } catch (Exception e) {
            return;
        }
        if (config) {
            maxTry = 1;
            window = false;
        }
        rdr.debugStat("slot=" + slot);
        rdr.debugStat("paralell=" + paralell);
        rdr.debugStat("jvm=" + jvn + jvp);
        rdr.debugStat("release=" + release);
        rdr.debugStat("url=" + url);
        rdr.debugStat("path=" + path);
        rdr.debugStat("discard=" + discard);
        rdr.debugStat("mdfile=" + mdfile);
        rdr.debugStat("summary=" + summary);
        rdr.debugStat("window=" + window);
        rdr.debugStat("config=" + config);
        rdr.debugStat("reapply=" + reapply);
        rdr.debugStat("randord=" + randord);
        rdr.debugStat("retry=" + maxTry);
        rdr.debugStat("other=" + otherF);
        rdr.debugStat("remote=" + remoteF);
        rdr.debugStat("persist=" + persistF);
        rdr.debugStat("capture=" + capture.size());
        rdr.debugStat("files=" + needed.size());
        if (otherF != null) {
            paralell = 0;
            otherD = bits.txt2buf(path + otherF);
            otherI = otherD.remove(0);
            otherM = bits.str2num(otherD.remove(0));
            otherN = otherD.remove(0);
            otherW = otherD.remove(0);
            otherS = otherD.remove(0);
        }
        if (remoteF != null) {
            paralell = 0;
            remoteD = bits.txt2buf(path + remoteF);
            remoteA = new addrIP();
            remoteA.fromString(remoteD.remove(0));
            remoteP = bits.str2num(remoteD.remove(0));
            remoteL = new addrIP();
            remoteL.fromString(remoteD.remove(0));
            remoteS = remoteD.remove(0);
        }
        if (persistF != null) {
            paralell = 0;
            persistD = bits.txt2buf(path + persistF);
            persistP = portBase + (portSlot / 2) + (slot * portSlot);
            String a = persistD.remove(0);
            int i = bits.str2num(persistD.remove(0));
            s = "qemu-system-x86_64 -monitor none -serial stdio -nographic -no-reboot -enable-kvm -cpu host -smp cores=4,threads=1,sockets=1 -hda " + a + " -m " + i;
            a = persistD.remove(0);
            for (i = 0; i < 8; i++) {
                int rp = persistP + ((i + 1) * 4);
                int lp = rp + 1;
                s += " -netdev socket,id=n" + i + ",udp=127.0.0.1:" + rp + ",localaddr=:" + lp + " -device " + a + ",netdev=n" + i + ",mac=00:00:00:00:11:" + bits.toHexB(i);
            }
            persistP += (4 * bits.str2num(persistD.remove(0)));
            persistC = new userTesterPrc(rdr, slot, "persist", s);
            persistC.persistent = true;
            bits.buf2txt(true, bits.str2lst(""), persistC.getLogName(4));
            s = persistD.remove(0);
            int round = 5000;
            rdr.setMax(round);
            for (int rnd = 0; rnd < round; rnd++) {
                a = persistC.getLine();
                rdr.setCurr(rnd);
                if (a == null) {
                    return;
                }
                a = a.trim();
                if (a.matches(s)) {
                    break;
                }
            }
            bits.sleep(1000);
            persistC.putChar(13);
            bits.sleep(1000);
            persistC.putLine(persistD.remove(0));
            bits.sleep(1000);
            persistC.putLine(persistD.remove(0));
            bits.sleep(1000);
            persistC.putLine(persistD.remove(0));
            persistC.syncr = persistD.remove(0);
            persistC.doSync();
            persistC.applyCfg(persistD);
        }
        started = bits.getTime();
        if (paralell > 1) {
            randord = true;
        } else {
            paralell = 1;
        }
        workers = new userTesterOne[paralell];
        for (int i = 0; i < paralell; i++) {
            workers[i] = getTester(i);
            new userTesterWrk(this, i);
            bits.sleep(200);
        }
        for (; needed.size() > 0;) {
            bits.sleep(1000);
        }
        if (persistC != null) {
            persistC.applyCfg(persistD);
            persistC.doSync();
            persistC.persistent = false;
            persistC.stopNow();
        }
        for (int i = 0; i < paralell; i++) {
            workers[i].stopAll();
        }
        for (int i = 0; i < finished.size(); i++) {
            userTesterFtr ftr = finished.get(i);
            if (!ftr.res) {
                rdr.debugStat("failed: " + ftr.csv);
                continue;
            }
            if (ftr.fld > 0) {
                rdr.debugStat("retried " + ftr.fld + "x: " + ftr.csv);
                continue;
            }
        }
        String a = bits.time2str(cfgAll.timeZoneName, bits.getTime() + cfgAll.timeServerOffset, 3) + ", took " + bits.timePast(started) + ", with " + paralell + " workers, on " + finished.size() + " cases, " + errored + " failed, " + traces + " traces, " + retries + " retries";
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
            userTesterFtr ftr = new userTesterFtr("zzz" + bits.padBeg("" + i, 4, "0"));
            ftr.ftr = "todo: " + txt.get(i);
            finished.add(ftr);
        }
        txt = new ArrayList<String>();
        txt.add(servHttp.html401tr);
        txt.add("<html><head><title>tester</title></head><body bgcolor=black text=white link=white vlink=white alink=white>");
        txt.add("release: " + release + "<br/>");
        txt.add("tested: " + a + "<br/>");
        txt.add("jvm: " + jvn + jvp + "<br/>");
        txt.add("<br/>");
        txt.add("<table border=1><tr><td><b>file</b></td><td><b>code</b></td><td><b>try</b></td><td><b>test</b></td><td><b>stage</b></td><td><b>command</b></td></tr>");
        txt.addAll(features2list(finished, 3));
        txt.add("</table></body></html>");
        bits.buf2txt(true, txt, "rtr" + beg + ".html");
        txt = new ArrayList<String>();
        txt.add("file;code;try;test;stage;command");
        txt.add("-;-;" + release + ";-;-");
        txt.add("-;-;" + a + ";-;-");
        txt.add("-;-;" + jvn + jvp + ";-;-");
        txt.addAll(features2list(finished, 4));
        bits.buf2txt(true, txt, "rtr" + beg + ".csv");
        a = "rtr" + beg + ".ftr";
        txt = bits.txt2buf(a);
        if (txt == null) {
            txt = new ArrayList<String>();
        }
        bits.buf2txt(true, features2list(finished, 2), a);
        txt = userFilter.getDiffs(txt, features2list(finished, 2));
        if (txt.size() < 1) {
            return;
        }
        txt.add(0, "---------------------------------- " + bits.time2str(cfgAll.timeZoneName, started + cfgAll.timeServerOffset, 3));
        bits.buf2txt(false, txt, "../changelog" + beg + ".txt");
    }

    /**
     * do worker round
     *
     * @param slt worker
     */
    protected void doWorker(int slt) {
        for (; needed.size() > 0;) {
            doOneTest(slt);
        }
    }

    private void doOneTest(int slt) {
        int cur = 0;
        if (randord) {
            cur = bits.random(0, needed.size());
        }
        userTesterFtr ftr = needed.get(cur);
        if (ftr.lck.add(1) > 1) {
            bits.sleep(200);
            return;
        }
        rdr.debugRes(sep + "err=" + errored + " trc=" + traces + " ret=" + retries + " don=" + finished.size() + " ned=" + needed.size() + " tot=" + (finished.size() + needed.size()) + " tim=" + bits.timePast(started) + sep + ftr.fil + sep);
        userTesterOne lt = getTester(slt);
        workers[slt] = lt;
        lt.doTest(path, ftr.fil);
        lt.stopAll();
        workers[slt] = getTester(slt);
        traces.add(lt.traces);
        boolean del = lt.getSucc();
        if (!del) {
            ftr.fld++;
            ftr.ret--;
            del |= ftr.ret < 1;
            retries.add(1);
        }
        rdr.debugRes(lt.getCsv(ftr.fld));
        if (!del) {
            ftr.lck.set(0);
            return;
        }
        needed.del(ftr);
        if (!lt.getSucc()) {
            errored.add(1);
        }
        ftr.res = lt.getSucc();
        ftr.htm = lt.getHtm(url, ftr.fld);
        ftr.csv = lt.getCsv(ftr.fld);
        ftr.ftr = lt.getFet();
        finished.add(ftr);
        lt.saveMd();
    }

    private userTesterOne getTester(int slt) {
        userTesterOne lt = new userTesterOne();
        lt.slot = slot + slt;
        lt.mdfile = mdfile;
        lt.config = config;
        lt.reapply = reapply;
        lt.jvm = jvn + jvp;
        lt.otherD = otherD;
        lt.otherI = otherI;
        lt.otherN = otherN;
        lt.otherM = otherM;
        lt.otherW = otherW;
        lt.otherS = otherS;
        lt.remoteD = remoteD;
        lt.remoteA = remoteA;
        lt.remoteL = remoteL;
        lt.remoteP = remoteP;
        lt.remoteS = remoteS;
        lt.persistP = persistP;
        lt.persistD = persistD;
        lt.persistC = persistC;
        lt.capture = capture;
        if (window) {
            lt.window += "w";
        }
        lt.rdr = new pipeProgress(cmd.pipe);
        return lt;
    }

    private static List<String> features2list(tabGen<userTesterFtr> ftr, int mod) {
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

class userTesterWrk implements Runnable {

    public int slot;

    public userTester lower;

    public userTesterWrk(userTester parent, int slt) {
        lower = parent;
        slot = slt;
        new Thread(this).start();
    }

    public void run() {
        try {
            lower.doWorker(slot);
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}

class userTesterFtr implements Comparator<userTesterFtr> {

    public final String fil;

    public syncInt lck = new syncInt(0);

    public int ret;

    public int fld;

    public boolean res;

    public String ftr;

    public String htm;

    public String csv;

    public userTesterFtr(String fn) {
        fil = fn;
    }

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

    public int slot = 0;

    public String name;

    public Process proc;

    public pipeShell shell;

    public pipeSide pipe;

    public pipeProgress rdr;

    public boolean persistent;

    public String syncr = "!!!hello there!!!";

    public userTesterPrc(pipeProgress reader, int slt, String nam, String command) {
        slot = slt;
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
        rdr.debugStat(slot + "/" + name + ": starting process");
    }

    public void stopNow() {
        if (persistent) {
            return;
        }
        rdr.debugStat(slot + "/" + name + ": stopping process");
        shell.kill();
        shell.waitFor();
        pipe.setClose();
    }

    public static String getLogName(int slt, String nam, int mod) {
        String s;
        switch (mod) {
            case 1:
                s = "run";
                break;
            case 2:
                s = "all";
                break;
            case 3:
                s = "err";
                break;
            case 4:
                s = "con";
                break;
            default:
                s = "log";
                break;
        }
        return userTesterOne.path + slt + nam + "-log." + s;
    }

    public String getLogName(int mod) {
        return getLogName(slot, name, mod);
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
        bits.buf2txt(false, bits.str2lst("rx:" + s), getLogName(4));
        return s;
    }

    public void putLine(String s) {
        bits.buf2txt(false, bits.str2lst("tx:" + s), getLogName(4));
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
        rdr.debugStat(slot + "/" + name + ": pinging " + s + ".");
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
        rdr.debugStat(slot + "/" + name + ": test failed: got " + i + ", expected " + need);
        return true;
    }

    public List<String> getOutput(String s) {
        String beg = "!begin-command-" + s;
        String end = "!end-command-" + s;
        rdr.debugStat(slot + "/" + name + ": output " + s + ".");
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
        String s = syncr + bits.randomD();
        putLine(s);
        for (;;) {
            String a = getLine();
            if (a == null) {
                return;
            }
            a = bits.trimE(a);
            if (a.indexOf(s) >= 0) {
                break;
            }
        }
    }

    public void applyCfg(List<String> cfg) {
        doSync();
        rdr.setMax(cfg.size());
        for (int i = 0; i < cfg.size(); i++) {
            if ((i % 5) == 4) {
                doSync();
            }
            putLine(cfg.get(i));
            bits.sleep(100);
            rdr.setCurr(i);
        }
        doSync();
    }

}

class userTesterOne {

    public int slot = 0;

    public static final String path = "../binTmp/zzz";

    public String fileName;

    public String testName = "unnamed";

    public int testRes = 1;

    public boolean config;

    public boolean mdfile;

    public int reapply;

    public String jvm;

    public List<userTesterCap> capture;

    public int persistP;

    public List<String> persistD;

    public userTesterPrc persistC;

    public List<String> remoteD;

    public addrIP remoteA;

    public addrIP remoteL;

    public int remoteP;

    public String remoteS;

    public String otherI;

    public String otherN;

    public String otherW;

    public String otherS;

    public List<String> otherD;

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

    public String getCsv(int ret) {
        return fileName + ";" + testRes + ";" + ret + ";" + testName + ";" + stage + ";" + cmd.getOriginal();
    }

    public String getHtm(String url, int ret) {
        return "<tr><td><a href=\"" + url + fileName + "\">" + fileName + "</a></td><td>" + testRes + "</td><td>" + ret + "</td><td>" + testName + "</td><td>" + stage + "</td><td>" + cmd.getOriginal() + "</td></tr>";
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
            List<String> log = bits.txt2buf(prc.getLogName(1));
            bits.buf2txt(false, log, prc.getLogName(2));
            userFlash.delete(prc.getLogName(1));
            if (checkLogs(log)) {
                continue;
            }
            bits.buf2txt(false, log, prc.getLogName(3));
            traces++;
        }
    }

    public void saveMd() {
        if (!mdfile) {
            return;
        }
        List<String> l = new ArrayList<String>();
        l.add("# Example: " + testName);
        l.add("");
        l.add("## **Topology diagram**");
        l.add("");
        l.add("![topology](/img/" + fileName + ".png)");
        l.add("");
        l.add("## **Configuration**");
        for (int i = 0; i < procs.size(); i++) {
            userTesterPrc p = procs.get(i);
            l.add("");
            l.add("**" + p.name + ":**");
            l.add("```");
            l.addAll(bits.txt2buf(path + slot + p.name + "-" + cfgInit.swCfgEnd));
            l.add("```");
        }
        if (shows.size() > 0) {
            l.add("");
            l.add("## **Verification**");
        }
        for (int i = 0; i < shows.size(); i++) {
            l.add("");
            l.add("```");
            l.addAll(shows.get(i));
            l.add("```");
        }
        bits.buf2txt(true, l, "../binTmp/" + fileName + ".md");
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
                i = bits.str2num(a.substring(3, a.length()));
                if (remoteD == null) {
                    i = userTester.portBase + (slot * userTester.portSlot) + (i * 4);
                    s = s + "127.0.0.1 " + (i + 3) + " 127.0.0.1 " + (i + 2) + b;
                    continue;
                }
                i = i + remoteP;
                s = s + remoteL + " " + i + " " + remoteA + " " + i + b;
                continue;
            }
            if (a.startsWith("per")) {
                i = bits.str2num(a.substring(3, a.length()));
                if (persistD == null) {
                    i = userTester.portBase + (slot * userTester.portSlot) + (i * 4);
                    s = s + "127.0.0.1 " + (i + 3) + " 127.0.0.1 " + (i + 2) + b;
                    continue;
                }
                i = (i * 4) + persistP;
                s = s + "127.0.0.1 " + i + " 127.0.0.1 " + (i + 1) + b;
                continue;
            }
            i = bits.str2num(a.substring(0, a.length() - 1)) * 4;
            i += userTester.portBase + (slot * userTester.portSlot);
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
            String beg1 = "<!>show:";
            String beg2 = "<!>config:";
            List<String> cfg = new ArrayList<String>();
            for (;;) {
                s = getLin();
                if (s.equals("!")) {
                    break;
                }
                if (s.startsWith(beg1)) {
                    s = s.substring(beg1.length(), s.length());
                    cfg.addAll(shows.get(bits.str2num(s)));
                    continue;
                }
                if (s.startsWith(beg2)) {
                    s = s.substring(beg2.length(), s.length());
                    cfg.addAll(bits.txt2buf(path + slot + s + "-" + cfgInit.swCfgEnd));
                    continue;
                }
                cfg.add(s);
            }
            bits.buf2txt(true, cfg, cmd.getRemaining());
            return;
        }
        if (s.equals("addpersist")) {
            if (persistD == null) {
                success();
                return;
            }
            String rn = cmd.word();
            List<String> cfg = new ArrayList<String>();
            rdr.debugStat(slot + "/" + rn + ": configuring process");
            for (;;) {
                s = getLin();
                if (s.equals("!")) {
                    break;
                }
                s = repairHwCfg(s);
                cfg.add(s);
            }
            bits.buf2txt(true, cfg, path + slot + rn + "-" + cfgInit.hwCfgEnd);
            persistC.name = rn;
            procs.add(persistC);
            bits.buf2txt(true, bits.str2lst(""), persistC.getLogName(4));
            cfg = new ArrayList<String>();
            for (;;) {
                s = getLin();
                if (s.equals("!")) {
                    break;
                }
                cfg.add(s);
            }
            bits.buf2txt(true, cfg, path + slot + rn + "-" + cfgInit.swCfgEnd);
            persistC.applyCfg(persistD);
            persistC.applyCfg(cfg);
            return;
        }
        if (s.equals("addremote")) {
            if (remoteD == null) {
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
            bits.buf2txt(true, cfg, path + slot + rn + "-" + cfgInit.hwCfgEnd);
            userTesterPrc p = new userTesterPrc(rdr, slot, rn, s);
            p.syncr = remoteS;
            procs.add(p);
            bits.buf2txt(true, bits.str2lst(""), p.getLogName(4));
            cfg = new ArrayList<String>();
            for (;;) {
                s = getLin();
                if (s.equals("!")) {
                    break;
                }
                cfg.add(s);
            }
            bits.buf2txt(true, cfg, path + slot + rn + "-" + cfgInit.swCfgEnd);
            p.applyCfg(remoteD);
            p.applyCfg(cfg);
            return;
        }
        if (s.equals("addother")) {
            if (otherD == null) {
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
            s = "qemu-system-x86_64 -monitor none -serial stdio -nographic -no-reboot -enable-kvm -cpu host -smp cores=2,threads=1,sockets=1 -hda " + otherI + " -m " + otherM;
            for (int i = 0; i < cfg.size(); i++) {
                String a = cfg.get(i);
                cmd = new cmds("hw", a);
                a = cmd.word();
                a = cmd.word();
                a = cmd.word();
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
            bits.buf2txt(true, cfg, path + slot + rn + "-" + cfgInit.hwCfgEnd);
            userTesterPrc p = new userTesterPrc(rdr, slot, rn, s);
            p.syncr = otherS;
            procs.add(p);
            bits.buf2txt(true, bits.str2lst(""), p.getLogName(4));
            cfg = new ArrayList<String>();
            for (;;) {
                s = getLin();
                if (s.equals("!")) {
                    break;
                }
                cfg.add(s);
            }
            bits.buf2txt(true, cfg, path + slot + rn + "-" + cfgInit.swCfgEnd);
            int round = 500;
            rdr.setMax(round);
            for (int rnd = 0; rnd < round; rnd++) {
                String a = p.getLine();
                rdr.setCurr(rnd);
                if (a == null) {
                    return;
                }
                a = a.trim();
                if (a.matches(otherW)) {
                    break;
                }
            }
            p.putChar(13);
            p.applyCfg(otherD);
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
            cfg.add("hwid tester-slot" + slot);
            cfg.add("tcp2vrf " + (2001 + (slot * userTester.portSlot) + procs.size()) + " tester 23");
            bits.buf2txt(true, cfg, path + slot + rn + "-" + cfgInit.hwCfgEnd);
            cfg = new ArrayList<String>();
            cfg.add("");
            cfg.add("");
            cfg.add("");
            cfg.add(fileName + " - " + rn + " - " + testName + ":");
            bits.buf2txt(true, cfg, userTesterPrc.getLogName(slot, rn, 1));
            cfg = new ArrayList<String>();
            cfg.add("hostname " + rn);
            cfg.add("logging file debug " + userTesterPrc.getLogName(slot, rn, 1));
            cfg.add("vrf definition tester");
            cfg.add(" exit");
            cfg.add("server telnet tester");
            cfg.add(" security protocol telnet");
            cfg.add(" vrf tester");
            cfg.add(" exit");
            for (;;) {
                s = getLin();
                if (s.equals("!")) {
                    break;
                }
                cfg.add(s);
            }
            bits.buf2txt(true, cfg, path + slot + rn + "-" + cfgInit.swCfgEnd);
            userTesterPrc p = new userTesterPrc(rdr, slot, rn, jvm + " router" + window + " " + path + slot + rn + "-");
            procs.add(p);
            bits.buf2txt(true, bits.str2lst(""), p.getLogName(4));
            p.putLine("terminal no monitor");
            p.putLine("terminal length 0");
            p.putLine("terminal table fancy");
            p.putLine("write");
            p.putLine("reload in 10");
            p.putLine("y");
            for (int i = 0; i < capture.size(); i++) {
                userTesterCap cap = capture.get(i);
                if (!rn.equals(cap.rtr)) {
                    continue;
                }
                p.putLine("packet capture " + cap.ifc + " " + path + slot + "log-" + rn + "-" + cap.ifc + ".pcap");
            }
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
