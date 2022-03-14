package net.freertr.user;

import java.io.File;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrMac;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgInit;
import net.freertr.pipe.pipeDiscard;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeProgress;
import net.freertr.pipe.pipeShell;
import net.freertr.pipe.pipeSide;
import net.freertr.serv.servHttp;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabIntMatcher;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.logger;
import net.freertr.util.shrtPthFrst;
import net.freertr.util.syncInt;
import net.freertr.util.version;

/**
 * process image tester
 *
 * @author matecsaba
 */
public class userTester {

    /**
     * create instance
     */
    public userTester() {
    }

    /**
     * port base
     */
    protected final static int portBase = 34000;

    /**
     * slot increment
     */
    protected final static int portSlot = 100;

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

    private List<userTesterImg> others = new ArrayList<userTesterImg>();

    private String other0;

    private List<userTesterCap> capture = new ArrayList<userTesterCap>();

    private boolean summary = false;

    private String cfgarch = "./";

    private boolean window = false;

    private boolean wait = false;

    private boolean config = false;

    private boolean mdfile = false;

    private boolean randord = false;

    private tabIntMatcher chatty;

    private String release = "unknown";

    private int maxTry = 1;

    private int reapply = 0;

    private int restart = 0;

    private int predelay = 0;

    private int postdelay = 0;

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

    private userTesterOne[] workers;

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
                continue;
            }
            if (s.equals("nosummary")) {
                summary = false;
                continue;
            }
            if (s.equals("cfgarch")) {
                cfgarch = cmd.word();
                continue;
            }
            if (s.equals("nocfgarch")) {
                cfgarch = "./";
                continue;
            }
            if (s.equals("window")) {
                window = true;
                continue;
            }
            if (s.equals("nowindow")) {
                window = false;
                continue;
            }
            if (s.equals("wait")) {
                wait = true;
                continue;
            }
            if (s.equals("nowait")) {
                wait = false;
                continue;
            }
            if (s.equals("reapply")) {
                reapply = bits.str2num(cmd.word());
                continue;
            }
            if (s.equals("noreapply")) {
                reapply = 0;
                continue;
            }
            if (s.equals("predelay")) {
                predelay = bits.str2num(cmd.word());
                continue;
            }
            if (s.equals("nopredelay")) {
                predelay = 0;
                continue;
            }
            if (s.equals("postdelay")) {
                postdelay = bits.str2num(cmd.word());
                continue;
            }
            if (s.equals("nopostdelay")) {
                postdelay = 0;
                continue;
            }
            if (s.equals("restart")) {
                restart = bits.str2num(cmd.word());
                continue;
            }
            if (s.equals("norestart")) {
                restart = 0;
                continue;
            }
            if (s.equals("config")) {
                config = true;
                continue;
            }
            if (s.equals("noconfig")) {
                config = false;
                continue;
            }
            if (s.equals("mdfile")) {
                mdfile = true;
                continue;
            }
            if (s.equals("nomdfile")) {
                mdfile = false;
                continue;
            }
            if (s.equals("randord")) {
                randord = true;
                continue;
            }
            if (s.equals("norandord")) {
                randord = false;
                continue;
            }
            if (s.equals("chatty")) {
                chatty = new tabIntMatcher();
                chatty.fromString(cmd.word());
                continue;
            }
            if (s.equals("nochatty")) {
                chatty = null;
                continue;
            }
            if (s.equals("retry")) {
                maxTry = bits.str2num(cmd.word());
                continue;
            }
            if (s.equals("noretry")) {
                maxTry = 1;
                continue;
            }
            if (s.equals("paralell")) {
                paralell = bits.str2num(cmd.word());
                continue;
            }
            if (s.equals("noparalell")) {
                paralell = 0;
                continue;
            }
            if (s.equals("slot")) {
                slot = bits.str2num(cmd.word());
                continue;
            }
            if (s.equals("noslot")) {
                slot = 0;
                continue;
            }
            if (s.equals("mem")) {
                mem = bits.str2num(cmd.word());
                continue;
            }
            if (s.equals("discard")) {
                discard = cmd.word();
                continue;
            }
            if (s.equals("nodiscard")) {
                discard = "^$";
                continue;
            }
            if (s.equals("path")) {
                path = cmd.word();
                continue;
            }
            if (s.equals("url")) {
                url = cmd.word();
                continue;
            }
            if (s.equals("persist")) {
                persistF = cmd.word();
                continue;
            }
            if (s.equals("nopersist")) {
                persistF = null;
                continue;
            }
            if (s.equals("remote")) {
                remoteF = cmd.word();
                continue;
            }
            if (s.equals("noremote")) {
                remoteF = null;
                continue;
            }
            if (s.equals("other")) {
                userTesterImg img = new userTesterImg();
                img.otherF = cmd.word();
                others.add(img);
                continue;
            }
            if (s.equals("noother")) {
                others.clear();
                continue;
            }
            if (s.equals("capture")) {
                userTesterCap cap = new userTesterCap();
                cap.rtr = cmd.word();
                cap.ifc = cmd.word();
                capture.add(cap);
                continue;
            }
            if (s.equals("nocapture")) {
                capture.clear();
                continue;
            }
            if (s.startsWith("openjdk")) {
                jvn = "/usr/lib/jvm/java-" + s.substring(7, s.length()) + "-openjdk-amd64/bin/java";
                continue;
            }
            if (s.startsWith("java")) {
                jvn = cmd.word();
                continue;
            }
            if (s.equals("ikvm")) {
                jvn = "/usr/bin/ikvm";
                mem = 0;
                continue;
            }
            if (s.equals("binary")) {
                jvn = "./rtr.bin";
                jvp = "";
                mem = 0;
                continue;
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
            File[] fils = new File(path).listFiles();
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
        for (int i = 0; i < others.size(); i++) {
            userTesterImg img = others.get(i);
            img.otherD = bits.txt2buf(path + img.otherF);
            img.otherP = " " + img.otherD.remove(0) + " ";
            img.otherI = img.otherD.remove(0);
            img.otherM = bits.str2num(img.otherD.remove(0));
            img.otherC = bits.str2num(img.otherD.remove(0));
            img.otherN = img.otherD.remove(0);
            img.otherW = img.otherD.remove(0);
            img.otherS = img.otherD.remove(0);
        }
        if (others.size() > 0) {
            cmds cmd = new cmds("ftr", others.get(0).otherP.trim());
            other0 = cmd.word();
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
        rdr.debugStat("slot=" + slot);
        rdr.debugStat("paralell=" + paralell);
        rdr.debugStat("jvm=" + jvn + jvp);
        rdr.debugStat("release=" + release);
        rdr.debugStat("url=" + url);
        rdr.debugStat("path=" + path);
        rdr.debugStat("discard=" + discard);
        rdr.debugStat("mdfile=" + mdfile);
        rdr.debugStat("summary=" + summary);
        rdr.debugStat("cfgarch=" + cfgarch);
        rdr.debugStat("window=" + window);
        rdr.debugStat("wait=" + wait);
        rdr.debugStat("config=" + config);
        rdr.debugStat("reapply=" + reapply);
        rdr.debugStat("restart=" + restart);
        rdr.debugStat("chatty=" + chatty);
        rdr.debugStat("predelay=" + predelay);
        rdr.debugStat("postdelay=" + postdelay);
        rdr.debugStat("randord=" + randord);
        rdr.debugStat("retry=" + maxTry);
        rdr.debugStat("other=" + others.size() + " " + other0);
        rdr.debugStat("remote=" + remoteF);
        rdr.debugStat("persist=" + persistF);
        rdr.debugStat("capture=" + capture.size());
        rdr.debugStat("files=" + needed.size());
        if (persistF != null) {
            paralell = 0;
            persistD = bits.txt2buf(path + persistF);
            persistP = portBase + (portSlot / 2) + (slot * portSlot);
            String a = persistD.remove(0);
            int i = bits.str2num(persistD.remove(0));
            int o = bits.str2num(persistD.remove(0));
            s = "qemu-system-x86_64 -monitor none -serial stdio -nographic -no-reboot -enable-kvm -cpu host -smp cores=" + o + ",threads=1,sockets=1 -drive file=" + a + " -m " + i;
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
            for (int rnd = 0; rnd <= round; rnd++) {
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
        if (paralell > needed.size()) {
            paralell = needed.size();
        }
        if (paralell > 1) {
            randord = true;
            wait = false;
        } else {
            paralell = 1;
        }
        workers = new userTesterOne[paralell];
        rdr.debugRes(sep + "starting " + paralell + " workers" + sep);
        for (int i = 0; i < paralell; i++) {
            workers[i] = getTester(i);
            new userTesterWrk(this, i);
            bits.sleep(200);
        }
        for (; needed.size() > 0;) {
            bits.sleep(1000);
            if (paralell > 1) {
                rdr.debugRes(sep + "err=" + errored + " trc=" + traces + " ret=" + retries + " don=" + finished.size() + " ned=" + needed.size() + " tot=" + (finished.size() + needed.size()) + " tim=" + bits.timePast(started) + sep);
            }
            if (cmd.pipe.ready2rx() < 1) {
                continue;
            }
            cmd.pipe.lineGet(1);
            listFails(finished, true, 1);
            listFails(needed, false, 0);
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
        listFails(finished, true, 1);
        listFails(needed, false, 0);
        String a = logger.getTimestamp() + ", took " + bits.timePast(started) + ", with " + paralell + " workers, on " + finished.size() + " cases, " + errored + " failed, " + traces + " traces, " + retries + " retries";
        rdr.debugStat("summary: " + a);
        if (!summary) {
            return;
        }
        rdr.debugStat("writing summary");
        if (other0 != null) {
            beg += other0 + "-";
        }
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
        txt.add(servHttp.htmlHead + "<style>");
        txt.add(" body { background-color: black; color: white; }");
        txt.add(" table, th, td { border: 1px solid }");
        txt.add(" :link { color: white }");
        txt.add(" :visited { color: white }");
        txt.add(" :active { color: white }");
        txt.add("</style>");
        txt.add("<title>tester</title></head><body>");
        txt.add("release: " + release + "<br/>");
        txt.add("tested: " + a + "<br/>");
        txt.add("jvm: " + jvn + jvp + "<br/>");
        txt.add("<br/>");
        txt.add("<table><thead><tr><td><b>file</b></td><td><b>result</b></td><td><b>test</b></td></tr></thead><tbody>");
        txt.addAll(features2list(finished, 3));
        txt.add("</tbody></table></body></html>");
        bits.buf2txt(true, txt, "rtr" + beg + ".html");
        txt = new ArrayList<String>();
        txt.add("url;file;result;test");
        txt.add("-;-;-;" + release);
        txt.add("-;-;-;" + a);
        txt.add("-;-;-;" + jvn + jvp);
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

    private void listFails(tabGen<userTesterFtr> lst, boolean nonres, int ranlim) {
        for (int i = 0; i < lst.size(); i++) {
            userTesterFtr ftr = lst.get(i);
            if (ftr == null) {
                continue;
            }
            if (nonres && !ftr.res) {
                rdr.debugStat("failed: " + ftr.fil + " " + ftr.csv);
                continue;
            }
            if (ftr.ran > ranlim) {
                rdr.debugStat("ran " + ftr.ran + "x: " + ftr.fil + " " + ftr.csv);
                continue;
            }
        }
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
        workers[slt].stopAll();
        userTesterOne lt = getTester(slt);
        workers[slt] = lt;
        lt.rdr.debugRes(sep + "err=" + errored + " trc=" + traces + " ret=" + retries + " don=" + finished.size() + " ned=" + needed.size() + " tot=" + (finished.size() + needed.size()) + " tim=" + bits.timePast(started) + sep + ftr.fil + sep);
        lt.doTest(path, ftr.fil);
        if (wait) {
            cmd.pipe.strChr("press q to quit test:", "qQ");
        }
        lt.stopAll();
        workers[slt] = getTester(slt);
        traces.add(lt.traces);
        boolean del = lt.getSucc();
        ftr.ran++;
        ftr.ret--;
        del |= ftr.ret < 1;
        lt.rdr.debugRes(lt.getCsv(url));
        if (!del) {
            ftr.lck.set(0);
            retries.add(1);
            return;
        }
        needed.del(ftr);
        if (!lt.getSucc()) {
            errored.add(1);
        }
        if (mdfile) {
            lt.saveMd();
        }
        ftr.res = lt.getSucc();
        ftr.htm = lt.getHtm(url);
        ftr.csv = lt.getCsv(url);
        ftr.ftr = lt.getFet();
        finished.add(ftr);
    }

    private userTesterOne getTester(int slt) {
        userTesterOne lt = new userTesterOne();
        lt.slot = slot + slt;
        lt.config = config;
        lt.wait = wait;
        lt.reapply = reapply;
        lt.restart = restart;
        lt.cfgarch = cfgarch;
        lt.chatty = chatty;
        lt.predelay = predelay;
        lt.postdelay = postdelay;
        lt.jvm = jvn + jvp;
        lt.others = others;
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
        pipeSide pip = cmd.pipe;
        if (paralell > 1) {
            pip = pipeDiscard.needAny(null);
            lt.pipe = pip;
        }
        lt.rdr = new pipeProgress(pip);
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

class userTesterImg {

    public String otherF = null;

    public String otherP = null;

    public String otherI = null;

    public String otherN = null;

    public String otherW = null;

    public String otherS = null;

    public int otherM = 0;

    public int otherC = 0;

    public List<String> otherD = null;

}

class userTesterFtr implements Comparator<userTesterFtr> {

    public final String fil;

    public syncInt lck = new syncInt(0);

    public int ret;

    public int ran;

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

class userTesterCon implements Comparator<userTesterCon> {

    public int locP;

    public int remP;

    public userTesterPrc perP;

    public userTesterCon perC;

    public String ifc;

    public int compare(userTesterCon o1, userTesterCon o2) {
        if (o1.locP < o2.locP) {
            return -1;
        }
        if (o1.locP > o2.locP) {
            return +1;
        }
        return 0;
    }

}

class userTesterCap {

    public String rtr;

    public String ifc;

    public userTesterCap() {

    }

}

class userTesterPrc {

    public int slot = 0;

    public String name;

    public pipeShell shell;

    public pipeSide pipe;

    public pipeProgress rdr;

    public boolean persistent;

    public String syncr = "!!!hello there!!!";

    public tabGen<userTesterCon> conns;

    public userTesterPrc(pipeProgress reader, int slt, String nam, String command) {
        slot = slt;
        name = nam;
        rdr = reader;
        pipeLine pl = new pipeLine(32768, false);
        final int tim = 600 * 1000;
        pipe = pl.getSide();
        pipe.setTime(tim);
        shell = pipeShell.exec(pipe, command, null, true, false);
        pipe = pl.getSide();
        pipe.setTime(tim);
        pipe.lineRx = pipeSide.modTyp.modeCRorLF;
        pipe.lineTx = pipeSide.modTyp.modeCR;
        rdr.debugStat(slot + "/" + name + ": starting process");
    }

    public void waitFor() {
        rdr.debugStat(slot + "/" + name + ": stopping process");
        shell.waitFor();
        pipe.setClose();
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
        return userTesterOne.prefix + slt + nam + "-log." + s;
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

    public boolean morePings(String s, tabIntMatcher ned, int round) {
        rdr.debugStat(slot + "/" + name + ": pinging " + s + ".");
        rdr.setMax(round);
        int i = -1;
        for (int rnd = 0; rnd <= round; rnd++) {
            rdr.setCurr(rnd);
            i = doPing(s);
            if (i < 0) {
                return true;
            }
            bits.buf2txt(false, bits.str2lst("res:" + i + " percent"), getLogName(4));
            if (ned.matches(i)) {
                return false;
            }
            bits.sleep(1000);
        }
        rdr.debugStat(slot + "/" + name + ": test failed: got " + i + ", expected " + ned);
        return true;
    }

    public List<String> getOutput(String s) {
        String beg = "!begin-command-" + s;
        String end = "!end-command-" + s;
        putLine(beg);
        putLine(s);
        putLine(end);
        List<String> res = new ArrayList<String>();
        for (;;) {
            String a = getLine();
            if (a == null) {
                return null;
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
                return res;
            }
            res.add(a);
        }
    }

    public int getSummary(String inc, String exc) {
        List<String> buf = getOutput("show interface swsummary");
        if (buf == null) {
            return -1;
        }
        int tot = 0;
        int usd = 0;
        for (int i = 0; i < buf.size(); i++) {
            String s = buf.get(i);
            if (s.indexOf(inc) < 0) {
                continue;
            }
            if (s.indexOf(exc) >= 0) {
                continue;
            }
            bits.buf2txt(false, bits.str2lst("hit:" + s), getLogName(4));
            cmds cmd = new cmds("res", s);
            int col = 0;
            int sum = 0;
            for (;;) {
                s = cmd.word(";");
                if (s.length() < 1) {
                    break;
                }
                col++;
                sum += bits.str2num(s);
            }
            if (col < 3) {
                continue;
            }
            tot += sum;
            usd++;
        }
        bits.buf2txt(false, bits.str2lst("res:" + tot + " bytes"), getLogName(4));
        if (usd < 1) {
            return -1;
        }
        return tot;
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

    public void readConns() {
        conns = new tabGen<userTesterCon>();
        List<String> l = bits.txt2buf(userTesterOne.prefix + slot + name + "-" + cfgInit.hwCfgEnd);
        if (l == null) {
            return;
        }
        for (int i = 0; i < l.size(); i++) {
            String a = l.get(i);
            if (!a.startsWith("int ")) {
                continue;
            }
            userTesterCon c = new userTesterCon();
            a = a.substring(4, a.length());
            c.ifc = a.substring(0, a.indexOf(" "));
            int p = a.indexOf("127.0.0.1");
            if (p < 0) {
                continue;
            }
            c.locP = bits.str2num(a.substring(p + 10, p + 15));
            p = a.lastIndexOf("127.0.0.1");
            if (p < 0) {
                continue;
            }
            c.remP = bits.str2num(a.substring(p + 10, p + 15));
            conns.add(c);
        }
    }

}

class userTesterRep {

    public final String src;

    public String trg;

    public userTesterRep(String s) {
        src = s;
    }

}

class userTesterOne {

    public int slot = 0;

    public static final String path = "../binTmp/";

    public static final String prefix = path + "zzz";

    public String fileName;

    public String testName = "unnamed";

    public int testRes = 1;

    public boolean config;

    public boolean wait;

    public int reapply;

    public int restart;

    public String cfgarch;

    public tabIntMatcher chatty;

    public int predelay;

    public int postdelay;

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

    public List<userTesterImg> others = new ArrayList<userTesterImg>();

    public String window = "c";

    public pipeProgress rdr;

    public pipeSide pipe;

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
            qc = "failed: ";
        } else {
            qc = "qc pass: ";
        }
        return qc + testName;
    }

    public String getRes() {
        if (testRes == 0) {
            return stage;
        } else {
            return "#" + testRes + "-" + stage + "-" + cmd.getOriginal();
        }
    }

    public String getCsv(String url) {
        return url + fileName + ";" + fileName + ";" + getRes() + ";" + testName;
    }

    public String getHtm(String url) {
        return "<tr><td><a href=\"" + url + fileName + "\">" + fileName + "</a></td><td>" + getRes() + "</td><td>" + testName + "</td></tr>";
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
        if (pipe != null) {
            pipe.setClose();
        }
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
        rdr.debugRes("generating mdfile");
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
            l.addAll(bits.txt2buf(prefix + slot + p.name + "-" + cfgInit.swCfgEnd));
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
        bits.buf2txt(true, l, path + fileName + ".md");
        l = new ArrayList<String>();
        for (int i = 0; i < procs.size(); i++) {
            procs.get(i).readConns();
        }
        for (int o = 0; o < procs.size(); o++) {
            userTesterPrc p = procs.get(o);
            for (int i = 0; i < p.conns.size(); i++) {
                userTesterCon c = p.conns.get(i);
                userTesterCon c1 = new userTesterCon();
                userTesterCon c2 = new userTesterCon();
                c1.locP = c.remP;
                c2.locP = c.locP;
                for (int q = 0; q < procs.size(); q++) {
                    userTesterPrc pp = procs.get(q);
                    if (p == pp) {
                        continue;
                    }
                    userTesterCon cf = pp.conns.find(c1);
                    if (cf == null) {
                        cf = pp.conns.find(c2);
                    }
                    if (cf == null) {
                        continue;
                    }
                    pp.conns.del(cf);
                    c.perP = pp;
                    c.perC = cf;
                    break;
                }
            }
        }
        l.add(shrtPthFrst.graphBeg1);
        l.add(shrtPthFrst.graphBeg2);
        l.add(shrtPthFrst.graphBeg3);
        for (int o = 0; o < procs.size(); o++) {
            userTesterPrc p = procs.get(o);
            l.add("//" + p.name);
            for (int i = 0; i < p.conns.size(); i++) {
                userTesterCon c = p.conns.get(i);
                if (c.perP == null) {
                    continue;
                }
                l.add("  " + p.name + " -- " + c.perP.name + " [weight=10] [taillabel=" + c.ifc + "] [headlabel=" + c.perC.ifc + "]");
            }
        }
        l.add(shrtPthFrst.graphEnd1);
        l.add(shrtPthFrst.graphEnd2);
        bits.buf2txt(true, l, path + fileName + ".dot");
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
        stage = "success";
        cmd = new cmds("", "");
    }

    private void nothave() {
        testRes = 0;
        stage = "not applicable";
        cmd = new cmds("", "");
    }

    public void doTest(String pt, String fn) {
        testRes = 2;
        fileName = fn;
        lineD = bits.txt2buf(pt + fn);
        stage = "run";
        for (;;) {
            if (testRes != 2) {
                return;
            }
            fn = getLin();
            if (fn == null) {
                break;
            }
            cmd = new cmds("", fn);
            doLine();
        }
        if (chatty == null) {
            success();
            return;
        }
        stage = "chatty";
        for (int i = 0; i < procs.size(); i++) {
            if (doChatty(procs.get(i), chatty)) {
                return;
            }
        }
        success();
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

    public boolean doChatty(userTesterPrc p, tabIntMatcher rng) {
        p.putLine("terminal table raw");
        p.doSync();
        int o = p.getSummary(";", "<nonexistent>");
        if (chatty.matches(o)) {
            return false;
        }
        testRes = 10;
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
                    cfg.addAll(bits.txt2buf(prefix + slot + s + "-" + cfgInit.swCfgEnd));
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
            bits.buf2txt(true, cfg, prefix + slot + rn + "-" + cfgInit.hwCfgEnd);
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
            bits.buf2txt(true, cfg, prefix + slot + rn + "-" + cfgInit.swCfgEnd);
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
            bits.buf2txt(true, cfg, prefix + slot + rn + "-" + cfgInit.hwCfgEnd);
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
            bits.buf2txt(true, cfg, prefix + slot + rn + "-" + cfgInit.swCfgEnd);
            p.applyCfg(remoteD);
            p.applyCfg(cfg);
            return;
        }
        if (s.equals("addother")) {
            if (others.size() < 1) {
                success();
                return;
            }
            String rn = cmd.word();
            String ftr = "";
            for (;;) {
                s = cmd.word();
                if (s.length() < 1) {
                    break;
                }
                if (s.equals("feature")) {
                    ftr = cmd.getRemaining();
                    break;
                }
            }
            List<userTesterImg> imgs = new ArrayList<userTesterImg>();
            for (int i = 0; i < others.size(); i++) {
                userTesterImg cur = others.get(i);
                cmd = new cmds("ftr", ftr);
                boolean miss = false;
                for (;;) {
                    s = cmd.word();
                    if (s.length() < 1) {
                        break;
                    }
                    miss |= cur.otherP.indexOf(" " + s + " ") < 0;
                }
                if (miss) {
                    continue;
                }
                imgs.add(cur);
            }
            rdr.debugRes("found " + imgs.size() + " images for features " + ftr);
            if (imgs.size() < 1) {
                nothave();
                return;
            }
            userTesterImg img = imgs.get(bits.random(0, imgs.size()));
            List<String> cfg = new ArrayList<String>();
            for (;;) {
                s = getLin();
                if (s.equals("!")) {
                    break;
                }
                s = repairHwCfg(s);
                cfg.add(s);
            }
            s = prefix + slot + rn + ".qcow2";
            String f = "auto";
            if (img.otherI.endsWith(".img")) {
                f = "raw";
            }
            if (img.otherI.endsWith(".qcow2")) {
                f = "qcow2";
            }
            if (img.otherI.endsWith(".vmdk")) {
                f = "vmdk";
            }
            pipeShell.exec("qemu-img create -f qcow2 -o backing_file=" + img.otherI + ",backing_fmt=" + f + " " + s, null, true);
            s = "qemu-system-x86_64 -monitor none -serial stdio -nographic -no-reboot -enable-kvm -cpu host -smp cores=" + img.otherC + ",threads=1,sockets=1 -drive file=" + s + ",format=qcow2,cache=unsafe -m " + img.otherM;
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
                s += " -netdev socket,id=n" + vl + ",udp=:" + rp + ",localaddr=:" + lp + " -device " + img.otherN + ",netdev=n" + vl + ",mac=" + mac.toEmuStr();
            }
            cfg.add("!" + s);
            bits.buf2txt(true, cfg, prefix + slot + rn + "-" + cfgInit.hwCfgEnd);
            userTesterPrc p = new userTesterPrc(rdr, slot, rn, s);
            p.pipe.setTime(5 * 60000);
            p.syncr = img.otherS;
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
            bits.buf2txt(true, cfg, prefix + slot + rn + "-" + cfgInit.swCfgEnd);
            int round = 5000;
            rdr.setMax(round);
            for (int rnd = 0; rnd <= round; rnd++) {
                String a = p.getLine();
                rdr.setCurr(rnd);
                if (a == null) {
                    return;
                }
                a = a.trim();
                if (a.matches(img.otherW)) {
                    break;
                }
            }
            p.putChar(13);
            p.applyCfg(img.otherD);
            p.applyCfg(cfg);
            return;
        }
        if (s.equals("addrouter")) {
            if (predelay > 0) {
                bits.sleep(predelay);
            }
            String rn = cmd.word();
            boolean write = true;
            boolean telnet = true;
            boolean fancy = true;
            boolean extra = false;
            String source = null;
            List<userTesterRep> reps = new ArrayList<userTesterRep>();
            List<userTesterRep> dels = new ArrayList<userTesterRep>();
            for (;;) {
                s = cmd.word();
                if (s.length() < 1) {
                    if (!extra) {
                        break;
                    }
                    s = getLin();
                    if (s.equals("!")) {
                        break;
                    }
                    cmd = new cmds("ln", s);
                    continue;
                }
                if (s.equals("extra")) {
                    extra = true;
                    continue;
                }
                if (s.equals("noextra")) {
                    extra = false;
                    continue;
                }
                if (s.equals("write")) {
                    write = true;
                    continue;
                }
                if (s.equals("nowrite")) {
                    write = false;
                    continue;
                }
                if (s.equals("fancy")) {
                    fancy = true;
                    continue;
                }
                if (s.equals("nofancy")) {
                    fancy = false;
                    continue;
                }
                if (s.equals("telnet")) {
                    telnet = true;
                    continue;
                }
                if (s.equals("notelnet")) {
                    telnet = false;
                    continue;
                }
                if (s.equals("source")) {
                    source = cmd.word();
                    continue;
                }
                if (s.equals("nosource")) {
                    source = null;
                    continue;
                }
                if (s.equals("replace")) {
                    userTesterRep rep = new userTesterRep(cmd.word());
                    rep.trg = cmd.word();
                    reps.add(rep);
                    continue;
                }
                if (s.equals("noreplace")) {
                    reps.clear();
                    continue;
                }
                if (s.equals("delete")) {
                    if (extra) {
                        s = cmd.getRemaining();
                        cmd = new cmds("ln", "");
                    } else {
                        s = cmd.word();
                    }
                    userTesterRep rep = new userTesterRep(s);
                    dels.add(rep);
                    continue;
                }
                if (s.equals("nodelete")) {
                    dels.clear();
                    continue;
                }
            }
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
            cfg.add("tcp2vrf " + (20001 + (slot * userTester.portSlot) + procs.size()) + " tester 23");
            bits.buf2txt(true, cfg, prefix + slot + rn + "-" + cfgInit.hwCfgEnd);
            cfg = new ArrayList<String>();
            cfg.add("");
            cfg.add("");
            cfg.add("");
            cfg.add(fileName + " - " + rn + " - " + testName + ":");
            bits.buf2txt(true, cfg, userTesterPrc.getLogName(slot, rn, 1));
            cfg = new ArrayList<String>();
            cfg.add("hostname " + rn);
            cfg.add("logging file debug " + userTesterPrc.getLogName(slot, rn, 1));
            if (telnet) {
                cfg.add("vrf definition tester");
                cfg.add(" exit");
                cfg.add("server telnet tester");
                cfg.add(" security protocol telnet");
                cfg.add(" vrf tester");
                cfg.add(" exit");
            }
            for (;;) {
                s = getLin();
                if (s.equals("!")) {
                    break;
                }
                cfg.add(s);
            }
            if (source != null) {
                List<String> src = bits.txt2buf(cfgarch + source);
                if (src == null) {
                    testRes = 11;
                    return;
                }
                cfg.addAll(src);
            }
            for (int o = 0; o < reps.size(); o++) {
                userTesterRep rep = reps.get(o);
                for (int i = 0; i < cfg.size(); i++) {
                    s = cfg.get(i);
                    s = s.replaceAll(rep.src, rep.trg);
                    cfg.set(i, s);
                }
            }
            if (dels.size() > 0) {
                List<userFilter> secs = userFilter.text2section(cfg);
                for (int o = 0; o < dels.size(); o++) {
                    userTesterRep del = dels.get(o);
                    secs = userFilter.getSection(secs, del.src, true, true, true);
                }
                cfg = userFilter.section2text(secs, false);
            }
            bits.buf2txt(true, cfg, prefix + slot + rn + "-" + cfgInit.swCfgEnd);
            s = jvm + " router" + window + " " + prefix + slot + rn + "-";
            userTesterPrc p = new userTesterPrc(rdr, slot, rn, s);
            bits.buf2txt(true, bits.str2lst(""), p.getLogName(4));
            if (write) {
                for (int i = 0; i < restart; i++) {
                    p.putLine("write");
                    p.putLine("reload force");
                    p.waitFor();
                    p = new userTesterPrc(rdr, slot, rn, s);
                }
            }
            procs.add(p);
            p.putLine("terminal no monitor");
            p.putLine("terminal length 0");
            for (int i = 0; i < capture.size(); i++) {
                userTesterCap cap = capture.get(i);
                if (!rn.equals(cap.rtr)) {
                    continue;
                }
                p.putLine("packet capture " + cap.ifc + " " + prefix + slot + "log-" + rn + "-" + cap.ifc + ".pcap");
            }
            if (fancy) {
                p.putLine("terminal table fancy");
            }
            if (write) {
                p.putLine("write");
            }
            if (!wait) {
                p.putLine("reload in 10");
                p.putLine("y");
            }
            for (int i = 0; i < reapply; i++) {
                p.putLine("configure reapply");
            }
            if (config) {
                p.putLine("reload force");
            }
            if (postdelay > 0) {
                bits.sleep(postdelay);
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
            bits.sleep(i);
            return;
        }
        userTesterPrc p = getPrc(s);
        if (p == null) {
            return;
        }
        bits.buf2txt(false, bits.str2lst("cmd:" + cmd.getOriginal()), p.getLogName(4));
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
        if (s.equals("chatty")) {
            tabIntMatcher rng = new tabIntMatcher();
            rng.fromString(cmd.word());
            doChatty(p, rng);
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
        if (s.equals("dping")) {
            String inc = cmd.word();
            String exc = cmd.word();
            userTesterPrc op = getPrc(cmd.word());
            if (op == null) {
                return;
            }
            String org = cmd.getRemaining();
            String add = " /size 1111 /repeat 1111";
            addrIP adr = new addrIP();
            adr.fromString(cmd.word());
            if (adr.isMulticast()) {
                add += " /delay 11";
            }
            bits.buf2txt(false, bits.str2lst("cmd:" + org + add), op.getLogName(4));
            p.putLine("terminal table raw");
            p.doSync();
            int old = p.getSummary(inc, exc);
            for (int rnd = 0; rnd <= 5; rnd++) {
                tabIntMatcher ned = new tabIntMatcher();
                ned.fromString("90-100");
                if (op.morePings(org + add, ned, 1)) {
                    return;
                }
                p.doSync();
                int cur = p.getSummary(inc, exc);
                if (cur < 1000) {
                    testRes = 8;
                    return;
                }
                int tot = cur - old;
                old = cur;
                if (tot < 500000) {
                    return;
                }
                rdr.debugStat(slot + "/" + p.name + ": test failed: too much traffic " + tot);
            }
            testRes = 9;
            return;
        }
        if (s.equals("output")) {
            s = cmd.getRemaining();
            rdr.debugStat(slot + "/" + p.name + ": output " + s + ".");
            List<String> buf = p.getOutput(s);
            if (buf == null) {
                testRes = 7;
                return;
            }
            shows.add(buf);
            return;
        }
        testRes = 5;
    }

    private boolean pingTest(userTesterPrc p, boolean chk) {
        tabIntMatcher ned = new tabIntMatcher();
        ned.fromString(cmd.word());
        int rnd = bits.str2num(cmd.word());
        if (!chk) {
            p.morePings(cmd.getRemaining(), ned, rnd);
            return false;
        }
        if (!p.morePings(cmd.getRemaining(), ned, rnd)) {
            return false;
        }
        testRes = 6;
        return true;
    }

}
