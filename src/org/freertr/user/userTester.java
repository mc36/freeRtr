package org.freertr.user;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrMac;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgInit;
import org.freertr.enc.enc7bit;
import org.freertr.pipe.pipeDiscard;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeProgress;
import org.freertr.pipe.pipeShell;
import org.freertr.pipe.pipeSide;
import org.freertr.serv.servHttp;
import org.freertr.spf.spfCalc;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabIntMatcher;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.logger;
import org.freertr.util.syncInt;
import org.freertr.util.version;

/**
 * process image tester
 *
 * @author matecsaba
 */
public class userTester {

    /**
     * create instance
     */
    protected userTester() {
    }

    /**
     * port base
     */
    protected final static int portBase = 34000;

    /**
     * slot increment
     */
    protected final static int portSlot = 100;

    /**
     * changelog separator
     */
    protected final static String chgLogSep = "---------------------------------- ";

    /**
     * style to use
     */
    protected final static String htmlStyle = "<style>\n body { background-color: black; color: white; }\n table, th, td { border: 1px solid }\n :link { color: white }\n :visited { color: white }\n :active { color: white }\n.good { color: green }\n.bad { color: red }\n</style>";

    /**
     * temporary path
     */
    protected String temp = "../binTmp/";

    /**
     * commands to use
     */
    protected cmds cmd;

    /**
     * reader to use
     */
    protected pipeProgress rdr;

    /**
     * config path
     */
    protected String path = "../cfg/";

    /**
     * discarded tests
     */
    protected String discard = "^$";

    /**
     * test url
     */
    protected String url = path;

    /**
     * slot number
     */
    protected int slot = 0;

    /**
     * worker count
     */
    protected int parallel = 0;

    /**
     * worker delay
     */
    protected int paragap = 300;

    /**
     * persistent file
     */
    protected String persistF = null;

    /**
     * persistent port
     */
    protected int persistP = 0;

    /**
     * persistent config
     */
    protected List<String> persistD = null;

    /**
     * persistent process
     */
    protected userTesterPrc persistC = null;

    /**
     * remote image file
     */
    protected String remoteF = null;

    /**
     * remote image config
     */
    protected List<String> remoteD = null;

    /**
     * remote image address
     */
    protected addrIP remoteA = null;

    /**
     * remote image local
     */
    protected addrIP remoteL = null;

    /**
     * remote image port
     */
    protected int remoteP = 0;

    /**
     * remote image syncer
     */
    protected String remoteS = null;

    /**
     * list of other images
     */
    protected final List<userTesterImg> others = new ArrayList<userTesterImg>();

    /**
     * other image 0th parameter
     */
    protected String other0;

    /**
     * list of captures
     */
    protected final List<userTesterCap> capture = new ArrayList<userTesterCap>();

    /**
     * write summary
     */
    protected boolean summary = false;

    /**
     * config archive
     */
    protected String cfgarch = "./";

    /**
     * runner command
     */
    protected String runner = "";

    /**
     * open a window
     */
    protected boolean window = false;

    /**
     * wait after tests
     */
    protected boolean wait = false;

    /**
     * dont exit
     */
    protected boolean unexit = false;

    /**
     * save config
     */
    protected boolean config = false;

    /**
     * generate md files
     */
    protected boolean mdfile = false;

    /**
     * randomize test order
     */
    protected boolean randord = false;

    /**
     * chattyness matcher
     */
    protected tabIntMatcher chatty;

    /**
     * release number
     */
    protected String releaseN = "unknown";

    /**
     * release version
     */
    protected String releaseV = "unknown";

    /**
     * maximum retries
     */
    protected int maxTry = 1;

    /**
     * reapply counter
     */
    protected int reapply = 0;

    /**
     * restart counter
     */
    protected int restart = 0;

    /**
     * delay in ms
     */
    protected int predelay = 0;

    /**
     * delay in ms
     */
    protected int postdelay = 0;

    /**
     * oob base port
     */
    protected int oobase = 20001;

    /**
     * java natives
     */
    protected String jvn = "java";

    /**
     * java parameters
     */
    protected String jvp = " XmxZZZm -jar " + cfgInit.getFileName();

    /**
     * beginning to use
     */
    protected String beg = "-";

    /**
     * separator to use
     */
    protected final static String sep = " ---------- ";

    /**
     * needed tests
     */
    protected final tabGen<userTesterFtr> needed = new tabGen<userTesterFtr>();

    /**
     * finished tests
     */
    protected final tabGen<userTesterFtr> finished = new tabGen<userTesterFtr>();

    /**
     * startup time
     */
    protected long started;

    /**
     * failed tests
     */
    protected syncInt errored = new syncInt(0);

    /**
     * retries seen
     */
    protected syncInt retries = new syncInt(0);

    /**
     * tracebacks seend
     */
    protected syncInt traces = new syncInt(0);

    /**
     * list of workers
     */
    protected userTesterOne[] workers;

    /**
     * convert result to string
     *
     * @param s string
     * @return converted string
     */
    protected final static String result2string(String s) {
        if (s.equals("success")) {
            return "ok";
        } else {
            return "not ok";
        }
    }

    /**
     * do the copier work
     *
     * @param c command to do
     * @param m true to move, false to copy
     */
    protected final static void doMover(cmds c, boolean m) {
        String s = c.word();
        String t = c.word();
        c.error("moving results " + s + " to " + t);
        doMover(c, s, t, ".csv", m);
        doMover(c, s, t, ".ftr", m);
        doMover(c, s, t, ".html", m);
    }

    /**
     * do the copier work
     *
     * @param c command to do
     * @param s source
     * @param t target
     * @param e extension
     * @param m true to move, false to copy
     * @return true on error, false on success
     */
    protected final static boolean doMover(cmds c, String s, String t, String e, boolean m) {
        s += e;
        t += e;
        boolean b;
        if (m) {
            b = userFlash.rename(s, t, true, true);
        } else {
            b = userFlash.copy(s, t, true);
        }
        c.error("moved " + s + " to " + t + " error=" + b);
        return b;
    }

    /**
     * get one tester
     *
     * @param slt slot to use
     * @return tester created
     */
    protected userTesterOne getTester(int slt) {
        return new userTesterOne(this, slt);
    }

    /**
     * convert features to list
     *
     * @param ftr features
     * @param mod modifier
     * @return converted list
     */
    protected final static List<String> features2list(tabGen<userTesterFtr> ftr, int mod) {
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

    /**
     * add html style
     *
     * @param l text to update
     */
    protected final static void addHtmlStyle(List<String> l) {
        l.add(htmlStyle);
    }

    /**
     * do the summary work
     *
     * @param c command to do
     */
    protected void doSummary(cmds c) {
        cmd = c;
        String target = "rtrp4sum-";
        beg = target;
        path = "./";
        List<String> nams = new ArrayList<String>();
        List<String> fils = new ArrayList<String>();
        List<List<String>> lins = new ArrayList<List<String>>();
        for (;;) {
            String s = cmd.word();
            if (s.length() < 1) {
                break;
            }
            if (s.equals("path")) {
                path = cmd.word();
                continue;
            }
            if (s.equals("target")) {
                target = cmd.word();
                continue;
            }
            if (s.equals("begin")) {
                beg = cmd.word();
                continue;
            }
            if (s.equals("addone")) {
                s = cmd.word();
                nams.add(s);
                s = cmd.word();
                fils.add(s);
                continue;
            }
        }
        for (int i = 0; i < fils.size(); i++) {
            String s = fils.get(i);
            s = path + s + ".csv";
            cmd.error("reading " + s + " as " + nams.get(i));
            List<String> cur = bits.txt2buf(s);
            if (cur == null) {
                cmd.error("error reading");
                return;
            }
            cur.remove(0);
            cur.remove(0);
            cur.remove(0);
            cur.remove(0);
            lins.add(cur);
        }
        if (lins.size() < 2) {
            cmd.error("not enough summary found");
            return;
        }
        int o = lins.get(0).size();
        cmd.error("doing " + o + " features");
        for (int i = 1; i < lins.size(); i++) {
            if (lins.get(i).size() != o) {
                cmd.error("mismatching size in " + nams.get(i));
                return;
            }
        }
        releaseN = cfgInit.versionFull;
        List<String> url = new ArrayList<String>();
        List<String> fil = new ArrayList<String>();
        List<List<String>> res = new ArrayList<List<String>>();
        List<String> tst = new ArrayList<String>();
        for (;;) {
            if (lins.get(0).size() < 1) {
                break;
            }
            c = new cmds("lin", lins.get(0).remove(0));
            List<String> cur = new ArrayList<String>();
            url.add(c.word(";"));
            fil.add(c.word(";"));
            cur.add(result2string(c.word(";")));
            tst.add(c.getRemaining());
            for (int i = 1; i < lins.size(); i++) {
                c = new cmds("lin", lins.get(i).remove(0));
                c.word(";");
                c.word(";");
                cur.add(result2string(c.word(";")));
            }
            res.add(cur);
        }
        String a = "url;file";
        for (int i = 0; i < lins.size(); i++) {
            a += ";" + nams.get(i);
        }
        a += ";test";
        List<String> txt = new ArrayList<String>();
        txt.add(a);
        a = "-;-;";
        for (int i = 0; i < lins.size(); i++) {
            a += "-;";
        }
        a += releaseN;
        txt.add(a);
        for (o = 0; o < fil.size(); o++) {
            a = url.get(o) + ";" + fil.get(o);
            List<String> cur = res.get(o);
            for (int i = 0; i < lins.size(); i++) {
                a += ";" + cur.get(i);
            }
            a += ";" + tst.get(o);
            txt.add(a);
        }
        bits.buf2txt(true, txt, target + ".csv");
        txt = new ArrayList<String>();
        txt.add(servHttp.htmlHead);
        addHtmlStyle(txt);
        txt.add("<title>dataplanes</title></head><body>");
        txt.add("release: " + releaseN + "<br/>");
        txt.add("<br/>");
        a = "<table><thead><tr><td><b>file</b></td>";
        for (int i = 0; i < lins.size(); i++) {
            a += "<td><b>" + nams.get(i) + "</b></td>";
        }
        a += "<td><b>test</b></td></tr></thead><tbody>";
        txt.add(a);
        for (o = 0; o < fil.size(); o++) {
            a = "<tr><td><a href=\"" + url.get(o) + "\">" + fil.get(o) + "</a></td>";
            List<String> cur = res.get(o);
            for (int i = 0; i < lins.size(); i++) {
                String s = cur.get(i);
                String b;
                if (s.length() <= 2) {
                    b = "good";
                } else {
                    b = "bad";
                }
                a += "<td class=" + b + ">" + s + "</td>";
            }
            a += "<td>" + tst.get(o) + "</td></tr>";
            txt.add(a);
        }
        txt.add("</tbody></table></body></html>");
        bits.buf2txt(true, txt, target + ".html");
    }

    /**
     * do the changelog work
     *
     * @param c command to do
     */
    protected void doChanges(cmds c) {
        cmd = c;
        String source = "../changelog.txt";
        String target = "";
        String state = "";
        summary = false;
        releaseN = version.name;
        jvn = "sid";
        jvp = "medium";
        releaseV = version.author;
        String startS = "";
        boolean forward = true;
        int startI = 0;
        for (;;) {
            String s = cmd.word();
            if (s.length() < 1) {
                break;
            }
            if (s.equals("pure")) {
                summary = true;
                continue;
            }
            if (s.equals("header")) {
                summary = false;
                continue;
            }
            if (s.equals("forward")) {
                forward = true;
                continue;
            }
            if (s.equals("backward")) {
                forward = false;
                continue;
            }
            if (s.equals("source")) {
                source = cmd.word();
                continue;
            }
            if (s.equals("packge")) {
                releaseN = cmd.word();
                continue;
            }
            if (s.equals("mntner")) {
                releaseV = cmd.word().replaceAll("_", " ");
                continue;
            }
            if (s.equals("distro")) {
                jvn = cmd.word();
                continue;
            }
            if (s.equals("urgent")) {
                jvp = cmd.word();
                continue;
            }
            if (s.equals("target")) {
                target = cmd.word();
                continue;
            }
            if (s.equals("state")) {
                state = cmd.word();
                continue;
            }
            if (s.equals("since")) {
                startS = cmd.word();
                continue;
            }
        }
        List<String> lst = bits.txt2buf(state);
        if (lst != null) {
            startS = lst.get(0);
        }
        lst = bits.txt2buf(source);
        if (lst == null) {
            cmd.error("error reading source");
            return;
        }
        List<userTesterChg> res = new ArrayList<userTesterChg>();
        userTesterChg cur = new userTesterChg("");
        for (int i = 0; i < lst.size(); i++) {
            String a = lst.get(i);
            if (!a.startsWith(chgLogSep)) {
                cur.txt.add(a);
                continue;
            }
            a = a.substring(chgLogSep.length(), a.length());
            cmds cm = new cmds("hed", a);
            a = cm.word() + " ";
            a += cm.word();
            cur = new userTesterChg(a);
            a = cm.word();
            if (a.length() < 1) {
                a = bits.time2str(cfgAll.timeZoneName, cur.tim, 1);
                a = a.substring(2, a.length());
                a = a.replaceAll("-", ".");
            }
            cur.ver = a;
            if (startS.compareTo(cur.str) >= 0) {
                startI = res.size();
            }
            res.add(cur);
        }
        lst = new ArrayList<String>();
        startI += 1;
        if (forward) {
            for (int i = startI; i < res.size(); i++) {
                dumpOneChange(res.get(i), lst);
            }
        } else {
            for (int i = res.size() - 1; i >= startI; i--) {
                dumpOneChange(res.get(i), lst);
            }
        }
        for (int i = 0; i < lst.size(); i++) {
            cmd.pipe.linePut(lst.get(i));
        }
        cur = res.get(res.size() - 1);
        bits.buf2txt(true, lst, target);
        bits.buf2txt(true, bits.str2lst(cur.str), state);
    }

    private void dumpOneChange(userTesterChg cur, List<String> lst) {
        if (summary) {
            lst.add("");
            lst.addAll(cur.txt);
            return;
        }
        lst.add("");
        lst.add(releaseN + " (" + cur.ver + ") " + jvn + "; urgency=" + jvp);
        lst.add("");
        for (int i = 0; i < cur.txt.size(); i++) {
            lst.add("  * " + cur.txt.get(i));
        }
        lst.add("");
        lst.add(" -- " + releaseV + "  " + bits.time2str(cfgAll.timeZoneName, cur.tim, 4));
    }

    /**
     * do the testing work
     *
     * @param c command to do
     */
    protected void doTesting(cmds c) {
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
            if (s.equals("runner")) {
                runner = cmd.word() + " ";
                continue;
            }
            if (s.equals("norunner")) {
                runner = "";
                continue;
            }
            if (s.equals("temp")) {
                temp = cmd.word();
                continue;
            }
            if (s.equals("notemp")) {
                temp = "./";
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
            if (s.equals("oobase")) {
                oobase = bits.str2num(cmd.word());
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
            if (s.equals("unexit")) {
                unexit = true;
                continue;
            }
            if (s.equals("nounexit")) {
                unexit = false;
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
            if (s.equals("parallel")) {
                parallel = bits.str2num(cmd.word());
                continue;
            }
            if (s.equals("noparallel")) {
                parallel = 0;
                continue;
            }
            if (s.equals("paragap")) {
                paragap = bits.str2num(cmd.word());
                continue;
            }
            if (s.equals("noparagap")) {
                paragap = 1;
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
                s = cmd.word();
                userTesterImg img = new userTesterImg(s);
                others.add(img);
                continue;
            }
            if (s.equals("noother")) {
                others.clear();
                continue;
            }
            if (s.equals("capture")) {
                String a = cmd.word();
                s = cmd.word();
                userTesterCap cap = new userTesterCap(a, s);
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
            if (s.equals("param")) {
                jvp = " " + cmd.word() + " " + jvp;
                continue;
            }
            if (s.equals("noparam")) {
                jvp = "";
                continue;
            }
        }
        String s = "-Xmx" + mem + "m";
        if (mem < 1) {
            s = "";
        }
        jvp = jvp.replaceAll("XmxZZZm", s);
        userTesterPrc prc = new userTesterPrc(rdr, temp, slot, "release", jvn + jvp + " show version brief");
        releaseN = prc.getLine();
        prc.stopNow();
        prc = new userTesterPrc(rdr, temp, slot, "version", jvn + jvp + " show version number");
        releaseV = prc.getLine();
        prc.stopNow();
        if (beg.length() < 2) {
            beg = "";
        }
        File[] fils = userFlash.dirList(path);
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
        if (config) {
            maxTry = 1;
            window = false;
        }
        for (int i = 0; i < others.size(); i++) {
            userTesterImg img = others.get(i);
            img.otherD = bits.txt2buf(path + img.otherF);
            img.otherP = " " + img.otherD.remove(0) + " ";
            img.otherC1 = img.otherD.remove(0);
            img.otherC2 = img.otherD.remove(0);
            img.otherC3 = img.otherD.remove(0);
            img.otherNS = bits.str2num(img.otherD.remove(0));
            img.otherNC = img.otherD.remove(0);
            img.otherW = img.otherD.remove(0);
            img.otherS = img.otherD.remove(0);
        }
        if (others.size() > 0) {
            cmds cmd = new cmds("ftr", others.get(0).otherP.trim());
            other0 = cmd.word();
        }
        if (remoteF != null) {
            parallel = 0;
            remoteD = bits.txt2buf(path + remoteF);
            remoteA = new addrIP();
            remoteA.fromString(remoteD.remove(0));
            remoteP = bits.str2num(remoteD.remove(0));
            remoteL = new addrIP();
            remoteL.fromString(remoteD.remove(0));
            remoteS = remoteD.remove(0);
        }
        rdr.debugStat("oobase=" + oobase);
        rdr.debugStat("slot=" + slot);
        rdr.debugStat("parallel=" + parallel);
        rdr.debugStat("jvm=" + jvn + jvp);
        rdr.debugStat("release=" + releaseN);
        rdr.debugStat("version=" + releaseV);
        rdr.debugStat("url=" + url);
        rdr.debugStat("path=" + path);
        rdr.debugStat("temp=" + temp);
        rdr.debugStat("discard=" + discard);
        rdr.debugStat("mdfile=" + mdfile);
        rdr.debugStat("summary=" + summary);
        rdr.debugStat("cfgarch=" + cfgarch);
        rdr.debugStat("runner=" + runner);
        rdr.debugStat("window=" + window);
        rdr.debugStat("wait=" + wait);
        rdr.debugStat("config=" + config);
        rdr.debugStat("unexit=" + unexit);
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
            parallel = 0;
            persistD = bits.txt2buf(path + persistF);
            persistP = portBase + (portSlot / 2) + (slot * portSlot);
            String a = persistD.remove(0);
            String b = persistD.remove(0);
            int i = bits.str2num(persistD.remove(0));
            int o = bits.str2num(persistD.remove(0));
            s = "qemu-system-x86_64 " + b + " -monitor none -serial stdio -nographic -no-reboot -enable-kvm -cpu host -smp cores=" + o + ",threads=1,sockets=1 -drive file=" + a + ",format=raw,cache=unsafe -m " + i;
            a = persistD.remove(0);
            for (i = 0; i < 8; i++) {
                int rp = persistP + ((i + 1) * 4);
                int lp = rp + 1;
                s += " -netdev socket,id=n" + i + ",udp=127.0.0.1:" + rp + ",localaddr=:" + lp + " -device " + a + ",netdev=n" + i + ",mac=00:00:00:00:11:" + bits.toHexB(i);
            }
            persistP += (4 * bits.str2num(persistD.remove(0)));
            persistC = new userTesterPrc(rdr, temp, slot, "persist", runner + s);
            persistC.persistent = true;
            bits.buf2txt(true, bits.str2lst(""), persistC.getLogName(4));
            s = persistD.remove(0);
            int round = 5000;
            rdr.setMax(round);
            for (int rnd = 0; rnd <= round; rnd++) {
                a = persistC.getLine();
                rdr.setCurr(rnd);
                if (a == null) {
                    persistC.persistent = false;
                    persistC.stopNow();
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
        if (parallel > needed.size()) {
            parallel = needed.size();
        }
        if (parallel > 1) {
            randord = true;
            wait = false;
        } else {
            parallel = 1;
        }
        workers = new userTesterOne[parallel];
        rdr.debugRes(sep + "starting " + parallel + " workers" + sep);
        for (int i = 0; i < parallel; i++) {
            workers[i] = getTester(i);
            new userTesterWrk(this, i);
            bits.sleep(paragap);
        }
        for (; needed.size() > 0;) {
            bits.sleep(1000);
            if (parallel > 1) {
                rdr.debugRes(sep + "err=" + errored + " trc=" + traces + " ret=" + retries + " don=" + finished.size() + " ned=" + needed.size() + " tot=" + (finished.size() + needed.size()) + " tim=" + bits.timePast(started) + sep);
            }
            if (cmd.pipe.ready2rx() < 1) {
                continue;
            }
            cmd.pipe.lineGet(1);
            listFails(rdr, finished, true, 1);
            listFails(rdr, needed, false, 0);
        }
        if (persistC != null) {
            persistC.applyCfg(persistD);
            persistC.doSync();
            persistC.persistent = false;
            persistC.stopNow();
        }
        for (int i = 0; i < parallel; i++) {
            workers[i].stopAll();
        }
        listFails(rdr, finished, true, 1);
        listFails(rdr, needed, false, 0);
        String a = logger.getTimestamp() + ", took " + bits.timePast(started) + ", with " + parallel + " workers, on " + finished.size() + " cases, " + errored + " failed, " + traces + " traces, " + retries + " retries";
        rdr.debugStat("summary: " + a);
        if (!summary) {
            return;
        }
        if (other0 != null) {
            beg += other0 + "-";
        }
        rdr.debugStat("writing summary " + beg);
        final String todoStr = "todo";
        List<String> txt = bits.txt2buf("../" + todoStr + ".txt");
        if (txt == null) {
            txt = new ArrayList<String>();
        }
        for (int i = 0; i < txt.size(); i++) {
            String b = txt.get(i);
            b = b.trim();
            if (b.length() < 1) {
                continue;
            }
            userTesterFtr ftr = new userTesterFtr(todoStr + bits.padBeg("" + i, 4, "0"));
            b = todoStr + ": " + b;
            b = enc7bit.doOneString(b);
            if (b.length() < 1) {
                continue;
            }
            ftr.ftr = b;
            finished.add(ftr);
        }
        txt = new ArrayList<String>();
        txt.add(servHttp.htmlHead);
        txt.add(htmlStyle);
        txt.add("<title>tester</title></head><body>");
        txt.add("release: " + releaseN + "<br/>");
        txt.add("tested: " + a + "<br/>");
        txt.add("jvm: " + jvn + jvp + ", other: " + other0 + "<br/>");
        txt.add("<br/>");
        txt.add("<table><thead><tr><td><b>file</b></td><td><b>result</b></td><td><b>test</b></td></tr></thead><tbody>");
        txt.addAll(features2list(finished, 3));
        txt.add("</tbody></table></body></html>");
        bits.buf2txt(true, txt, "rtr" + beg + ".html");
        txt = new ArrayList<String>();
        txt.add("url;file;result;test");
        txt.add("-;-;-;" + releaseN);
        txt.add("-;-;-;" + a);
        txt.add("-;-;-;" + jvn + jvp + ", " + other0);
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
        txt.add(0, chgLogSep + bits.time2str(cfgAll.timeZoneName, started + cfgAll.timeServerOffset, 3) + " " + releaseV);
        bits.buf2txt(false, txt, "../changelog" + beg + ".txt");
    }

    /**
     * list failed tests
     *
     * @param rdr reader to use
     * @param lst tests to scan
     * @param nonres enforced retries
     * @param ranlim enforced run count
     */
    protected final static void listFails(pipeProgress rdr, tabGen<userTesterFtr> lst, boolean nonres, int ranlim) {
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
     * convert to udp endpoint
     *
     * @param cmd command to update
     * @param skp intrefaces to skip
     * @param ifc interface string
     * @param fn filename
     * @param sl sl slot
     * @param cp control port
     * @param lp local port
     * @param rp remote port
     * @param ad mac address
     * @return hwcfg string
     */
    protected final static String convert2udp(String cmd, int skp, String ifc, int sl, String fn, int cp, List<Integer> lp, List<Integer> rp, List<addrMac> ad) {
        String nc = "";
        for (int i = skp; i < lp.size(); i++) {
            String a = "" + ifc;
            a = a.replaceAll("\\$id\\$", "" + i);
            a = a.replaceAll("\\$lp\\$", "" + lp.get(i));
            a = a.replaceAll("\\$rp\\$", "" + rp.get(i));
            a = a.replaceAll("\\$ad\\$", "" + ad.get(i).toEmuStr());
            nc += a;
        }
        cmd = cmd.replaceAll("\\$nc\\$", nc);
        cmd = cmd.replaceAll("\\$fn\\$", fn);
        cmd = cmd.replaceAll("\\$cp\\$", "" + cp);
        cmd = cmd.replaceAll("\\$sl\\$", "" + sl);
        return cmd;
    }

    /**
     * get log name
     *
     * @param pfx prefix
     * @param slt slot
     * @param nam router name
     * @param mod log to get
     * @return log file name
     */
    protected final static String getLogName(String pfx, int slt, String nam, int mod) {
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
        return pfx + slt + nam + "-log." + s;
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

    /**
     * perform one test
     *
     * @param slt slot to use
     */
    protected void doOneTest(int slt) {
        int cur = 0;
        if (randord) {
            cur = bits.random(0, needed.size());
        }
        userTesterFtr ftr = needed.get(cur);
        if (ftr.lck.add(1) > 1) {
            bits.sleep(paragap);
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

}

class userTesterOne {

    /**
     * pipeline to use
     */
    protected final pipeSide pipe;

    /**
     * console handler
     */
    protected final pipeProgress rdr;

    /**
     * slot number
     */
    protected final int slot;

    /**
     * path to use
     */
    protected final String path;

    /**
     * prefix to use
     */
    protected final String prefix;

    /**
     * just save config
     */
    protected final boolean config;

    /**
     * dont exit
     */
    protected final boolean unexit;

    /**
     * dont reload
     */
    protected final boolean wait;

    /**
     * reapply counter
     */
    protected final int reapply;

    /**
     * restart counter
     */
    protected final int restart;

    /**
     * config archive
     */
    protected final String cfgarch;

    /**
     * runner command
     */
    protected final String runner;

    /**
     * chattyness matcher
     */
    protected final tabIntMatcher chatty;

    /**
     * wait before start
     */
    protected final int predelay;

    /**
     * wait before stop
     */
    protected final int postdelay;

    /**
     * middleware to test
     */
    protected final String jvm;

    /**
     * local console base
     */
    protected final int oobase;

    /**
     * interfaces to capture
     */
    protected final List<userTesterCap> capture;

    /**
     * persistent image port base
     */
    protected final int persistP;

    /**
     * persistent image config
     */
    protected final List<String> persistD;

    /**
     * persistent image process
     */
    protected final userTesterPrc persistC;

    /**
     * remote image config
     */
    protected final List<String> remoteD;

    /**
     * remote image address
     */
    protected final addrIP remoteA;

    /**
     * remote image local
     */
    protected final addrIP remoteL;

    /**
     * remote image port
     */
    protected final int remoteP;

    /**
     * remote image syncer
     */
    protected final String remoteS;

    /**
     * other images to test
     */
    protected final List<userTesterImg> others;

    /**
     * verification commands
     */
    protected final List<List<String>> shows = new ArrayList<List<String>>();

    /**
     * processes
     */
    private final tabGen<userTesterPrc> procs = new tabGen<userTesterPrc>();

    /**
     * filename to use
     */
    protected String fileName;

    /**
     * name of the test
     */
    protected String testName = "unnamed";

    /**
     * result of the test
     */
    protected int testRes = 1;
    /**
     * self parameters
     */
    protected String window = "c";

    /**
     * tracebacks seen
     */
    protected int traces;

    /**
     * commands to do
     */
    private cmds cmd = new cmds("", "");

    /**
     * current stage
     */
    private String stage = "init";

    /**
     * lines to do
     */
    private List<String> lineD;

    /**
     * lines done
     */
    private int lineN = -1;

    /**
     * create instance
     *
     * @param frm get data from
     * @param slt slot number
     */
    protected userTesterOne(userTester frm, int slt) {
        pipeSide pip = frm.cmd.pipe;
        if (frm.parallel > 1) {
            pip = pipeDiscard.needAny(null);
        }
        rdr = new pipeProgress(pip);
        pipe = pip;
        path = frm.temp;
        prefix = frm.temp;
        slot = frm.slot + slt;
        config = frm.config;
        unexit = frm.unexit;
        wait = frm.wait;
        reapply = frm.reapply;
        restart = frm.restart;
        cfgarch = frm.cfgarch;
        runner = frm.runner;
        chatty = frm.chatty;
        predelay = frm.predelay;
        postdelay = frm.postdelay;
        jvm = frm.jvn + frm.jvp;
        oobase = frm.oobase;
        others = frm.others;
        remoteD = frm.remoteD;
        remoteA = frm.remoteA;
        remoteL = frm.remoteL;
        remoteP = frm.remoteP;
        remoteS = frm.remoteS;
        persistP = frm.persistP;
        persistD = frm.persistD;
        persistC = frm.persistC;
        capture = frm.capture;
        if (frm.window) {
            window += "w";
        }
    }

    /**
     * get result
     *
     * @return true if successful false if not
     */
    protected boolean getSucc() {
        return testRes == 0;
    }

    /**
     * get feature line
     *
     * @return string
     */
    protected String getFet() {
        String qc;
        if (testRes != 0) {
            qc = "failed: ";
        } else {
            qc = "qc pass: ";
        }
        return qc + testName;
    }

    /**
     * get result line
     *
     * @return string
     */
    protected String getRes() {
        if (testRes == 0) {
            return stage;
        } else {
            return "#" + testRes + "-" + stage + "-" + cmd.getOriginal();
        }
    }

    /**
     * get csv line
     *
     * @param url url to use
     * @return string
     */
    protected String getCsv(String url) {
        return url + fileName + ";" + fileName + ";" + getRes() + ";" + testName;
    }

    /**
     * get html line
     *
     * @param url url to use
     * @return string
     */
    protected String getHtm(String url) {
        return "<tr><td><a href=\"" + url + fileName + "\">" + fileName + "</a></td><td>" + getRes() + "</td><td>" + testName + "</td></tr>";
    }

    /**
     * get next line
     *
     * @return string, null if no more
     */
    protected String getLin() {
        if (lineD == null) {
            return null;
        }
        lineN++;
        if (lineN >= lineD.size()) {
            return null;
        }
        return lineD.get(lineN);
    }

    /**
     * stop everything
     */
    protected void stopAll() {
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

    /**
     * generate md file
     */
    protected void saveMd() {
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
        l.add(spfCalc.graphBeg1);
        l.add(spfCalc.graphBeg2);
        l.add(spfCalc.graphBeg3);
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
        l.add(spfCalc.graphEnd1);
        l.add(spfCalc.graphEnd2);
        bits.buf2txt(true, l, path + fileName + ".dot");
    }

    /**
     * find one router
     *
     * @param s string to find
     * @return process, null if not found
     */
    protected userTesterPrc getPrc(String s) {
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

    /**
     * perform the test
     *
     * @param pt pathname
     * @param fn filename
     */
    protected void doTest(String pt, String fn) {
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
            a = a.substring(a.length() - 1, a.length());
            if (a.equals("b")) {
                i += 1;
            }
            if (a.equals("p")) {
                s = s + i + b;
            } else {
                s = s + "127.0.0.1 " + i + b;
            }
        }
        return s;
    }

    /**
     * check the logs
     *
     * @param l logs to check
     * @return false on error, true on success
     */
    protected boolean checkLogs(List<String> l) {
        if (l == null) {
            return true;
        }
        for (int i = 0; i < l.size(); i++) {
            String a = l.get(i).toLowerCase();
            if (a.indexOf("executeswcommand") >= 0) {
                return false;
            }
            if (a.indexOf("java.net.serversocket.bind") >= 0) {
                continue;
            }
            if (a.indexOf("java.net.bindexception") >= 0) {
                continue;
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

    /**
     * check chattyenss tests
     *
     * @param p process to check
     * @param rng range to use
     * @return false on success, true on error
     */
    protected boolean doChatty(userTesterPrc p, tabIntMatcher rng) {
        p.putLine("terminal table raw");
        p.doSync();
        int o = p.getSummary(";", "<nonexistent>");
        if (chatty.matches(o)) {
            return false;
        }
        testRes = 10;
        return true;
    }

    /**
     * do one line
     */
    protected void doLine() {
        String s = cmd.word();
        if (s.length() < 1) {
            return;
        }
        if (s.startsWith("!")) {
            return;
        }
        if (s.equals("comment")) {
            return;
        }
        if (s.equals("comments")) {
            for (;;) {
                s = getLin();
                if (s == null) {
                    break;
                }
                if (s.equals("!")) {
                    break;
                }
            }
            return;
        }
        if (s.equals("description")) {
            testName = cmd.getRemaining();
            rdr.debugRes("test: " + testName);
            return;
        }
        if (s.equals("exit")) {
            if (unexit) {
                return;
            }
            success();
            return;
        }
        if (s.equals("output")) {
            String beg1 = "<!>show:";
            String beg2 = "<!>config:";
            List<String> cfg = new ArrayList<String>();
            for (;;) {
                s = getLin();
                if (s == null) {
                    break;
                }
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
                if (s == null) {
                    break;
                }
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
                if (s == null) {
                    break;
                }
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
                if (s == null) {
                    break;
                }
                if (s.equals("!")) {
                    break;
                }
                s = repairHwCfg(s);
                cfg.add(s);
            }
            s = "telnet " + remoteA + " " + remoteP;
            cfg.add("!" + s);
            bits.buf2txt(true, cfg, prefix + slot + rn + "-" + cfgInit.hwCfgEnd);
            userTesterPrc p = new userTesterPrc(rdr, prefix, slot, rn, runner + s);
            p.syncr = remoteS;
            procs.add(p);
            bits.buf2txt(true, bits.str2lst(""), p.getLogName(4));
            cfg = new ArrayList<String>();
            for (;;) {
                s = getLin();
                if (s == null) {
                    break;
                }
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
            userTesterPrc ctP = null;
            String ctV = null;
            int ctL = 30001 + (slot * userTester.portSlot) + procs.size();
            String rn = cmd.word();
            String ftr = "";
            for (;;) {
                s = cmd.word();
                if (s.length() < 1) {
                    break;
                }
                if (s.equals("controller")) {
                    ctP = new userTesterPrc(rdr, prefix, slot, cmd.word(), null);
                    ctP.stopNow();
                    ctP = procs.find(ctP);
                    ctV = cmd.word();
                    ctV += " " + cmd.word();
                    ctV += " - " + cmd.word();
                    continue;
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
                if (s == null) {
                    break;
                }
                if (s.equals("!")) {
                    break;
                }
                s = repairHwCfg(s);
                cfg.add(s);
            }
            List<addrMac> mcs = new ArrayList<addrMac>();
            List<Integer> lps = new ArrayList<Integer>();
            List<Integer> rps = new ArrayList<Integer>();
            for (int i = 0; i < cfg.size(); i++) {
                String a = cfg.get(i);
                cmd = new cmds("hw", a);
                a = cmd.word();
                a = cmd.word();
                a = cmd.word();
                addrMac mac = new addrMac();
                mac.fromString(cmd.word());
                mcs.add(mac);
                cmd.word();
                lps.add(bits.str2num(cmd.word()));
                cmd.word();
                rps.add(bits.str2num(cmd.word()));
            }
            if (ctP != null) {
                ctP.putLine("test hwcfg tcp2vrf " + ctL + " " + ctV);
                ctP.doSync();
            }
            pipeShell.exec(runner + userTester.convert2udp(img.otherC1, img.otherNS, img.otherNC, slot, prefix + slot + rn, ctL, lps, rps, mcs), null, true, false, true);
            pipeShell.exec(runner + userTester.convert2udp(img.otherC2, img.otherNS, img.otherNC, slot, prefix + slot + rn, ctL, lps, rps, mcs), null, true, false, true);
            s = userTester.convert2udp(img.otherC3, img.otherNS, img.otherNC, slot, prefix + slot + rn, ctL, lps, rps, mcs);
            cfg.add("!" + s);
            bits.buf2txt(true, cfg, prefix + slot + rn + "-" + cfgInit.hwCfgEnd);
            userTesterPrc p = new userTesterPrc(rdr, prefix, slot, rn, runner + s);
            p.pipe.setTime(5 * 60000);
            p.syncr = img.otherS;
            procs.add(p);
            bits.buf2txt(true, bits.str2lst(""), p.getLogName(4));
            cfg = new ArrayList<String>();
            for (;;) {
                s = getLin();
                if (s == null) {
                    break;
                }
                if (s.equals("!")) {
                    break;
                }
                cfg.add(s);
            }
            bits.buf2txt(true, cfg, prefix + slot + rn + "-" + cfgInit.swCfgEnd);
            if (img.otherW == null) {
                return;
            }
            if (img.otherW.length() < 1) {
                return;
            }
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
            boolean tstamp = true;
            boolean extra = false;
            boolean swcfg = false;
            String extcfg = null;
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
                    if (s == null) {
                        break;
                    }
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
                if (s.equals("swcfg")) {
                    swcfg = true;
                    continue;
                }
                if (s.equals("noswcfg")) {
                    swcfg = false;
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
                if (s.equals("tstamp")) {
                    tstamp = true;
                    continue;
                }
                if (s.equals("notstamp")) {
                    tstamp = false;
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
                if (s.equals("extcfg")) {
                    extcfg = cmd.word();
                    continue;
                }
                if (s.equals("noextcfg")) {
                    extcfg = null;
                    continue;
                }
                if (s.equals("replace")) {
                    s = cmd.word();
                    userTesterRep rep = new userTesterRep(s, cmd.word());
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
                    userTesterRep rep = new userTesterRep(s, "");
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
                if (s == null) {
                    break;
                }
                if (s.equals("!")) {
                    break;
                }
                s = repairHwCfg(s);
                cfg.add(s);
            }
            cfg.add("hwid tester-slot" + slot);
            cfg.add("rwpath " + prefix);
            cfg.add("save " + prefix + slot + rn + "-state");
            cfg.add("tcp2vrf " + (oobase + (slot * userTester.portSlot) + procs.size()) + " tester 23");
            int i = userTester.portBase + (slot * userTester.portSlot);
            int o = userTester.portSlot / 2;
            i += procs.size() * o;
            cfg.add("port " + (i + o) + " " + (i + o + o));
            bits.buf2txt(true, cfg, prefix + slot + rn + "-" + cfgInit.hwCfgEnd);
            cfg = new ArrayList<String>();
            cfg.add("");
            cfg.add("");
            cfg.add("");
            cfg.add(fileName + " - " + rn + " - " + testName + ":");
            bits.buf2txt(true, cfg, userTester.getLogName(prefix, slot, rn, 1));
            cfg = new ArrayList<String>();
            cfg.add("hostname " + rn);
            cfg.add("logging milliseconds");
            cfg.add("logging file debug " + userTester.getLogName(prefix, slot, rn, 1));
            if (telnet) {
                cfg.add("vrf definition tester");
                cfg.add(" exit");
                cfg.add("server telnet tester");
                cfg.add(" security protocol telnet");
                cfg.add(" vrf tester");
                cfg.add(" exec colorize header");
                cfg.add(" exec monitor");
                cfg.add(" exit");
            }
            for (;;) {
                s = getLin();
                if (s == null) {
                    break;
                }
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
            for (o = 0; o < reps.size(); o++) {
                userTesterRep rep = reps.get(o);
                for (i = 0; i < cfg.size(); i++) {
                    s = cfg.get(i);
                    s = s.replaceAll(rep.src, rep.trg);
                    cfg.set(i, s);
                }
            }
            if (dels.size() > 0) {
                List<userFilter> secs = userFilter.text2section(cfg);
                for (o = 0; o < dels.size(); o++) {
                    userTesterRep del = dels.get(o);
                    secs = userFilter.getSection(secs, del.src, true, true, true);
                }
                cfg = userFilter.section2text(secs, false);
            }
            if (swcfg) {
                for (i = 0; i < cfg.size(); i++) {
                    s = cfg.get(i);
                    s = repairHwCfg(s);
                    cfg.set(i, s);
                }
            }
            s = jvm;
            s = s.replaceAll("%fn%", fileName);
            s = s.replaceAll("%rn%", rn);
            String a = "";
            if (extcfg != null) {
                a = "s";
            }
            s = s + " router" + window + a + " " + prefix + slot + rn + "-";
            if (extcfg != null) {
                s += cfgInit.hwCfgEnd + " " + extcfg;
            }
            cfg.add(0, "!" + s);
            if (extcfg == null) {
                bits.buf2txt(true, cfg, prefix + slot + rn + "-" + cfgInit.swCfgEnd);
            } else {
                if (!new File(extcfg).exists()) {
                    bits.buf2txt(true, cfg, extcfg);
                }
            }
            userTesterPrc p = new userTesterPrc(rdr, prefix, slot, rn, runner + s);
            bits.buf2txt(true, bits.str2lst(""), p.getLogName(4));
            if (write) {
                for (i = 0; i < restart; i++) {
                    p.putLine("write");
                    p.putLine("reload force");
                    p.waitFor();
                    p = new userTesterPrc(rdr, prefix, slot, rn, runner + s);
                }
            }
            procs.add(p);
            p.putLine("terminal no monitor");
            p.putLine("terminal length 0");
            for (i = 0; i < capture.size(); i++) {
                userTesterCap cap = capture.get(i);
                if (!rn.equals(cap.rtr)) {
                    continue;
                }
                p.putLine("packet capture " + cap.ifc + " " + prefix + slot + rn + "-" + cap.ifc + ".pcap");
            }
            if (fancy) {
                p.putLine("terminal table fancy");
            }
            if (tstamp) {
                p.putLine("terminal timestamps");
            }
            if (write) {
                p.putLine("write");
            }
            if (!wait) {
                p.putLine("reload in 10");
                p.putLine("y");
            }
            for (i = 0; i < reapply; i++) {
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
        if (s.equals("pcap")) {
            p.putLine("packet capture " + cmd.getRemaining());
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
            String add = " size 1111 repeat 1111";
            addrIP adr = new addrIP();
            adr.fromString(cmd.word());
            if (adr.isMulticast()) {
                add += " delay 11";
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

class userTesterPrc implements Comparable<userTesterPrc> {

    /**
     * reader to use
     */
    protected final pipeProgress rdr;

    /**
     * router name
     */
    protected String name;

    /**
     * slot to use
     */
    protected final int slot;

    /**
     * prefix to use
     */
    protected final String prefix;

    /**
     * shell to use
     */
    protected final pipeShell shell;

    /**
     * pipe to use
     */
    protected pipeSide pipe;

    /**
     * persistent process
     */
    protected boolean persistent;

    /**
     * syncer string
     */
    protected String syncr = "!!!hello there!!!";

    /**
     * connections
     */
    protected tabGen<userTesterCon> conns;

    /**
     * create instance
     *
     * @param reader reader to use
     * @param pfx test prefix
     * @param slt tester slot
     * @param nam test name
     * @param command command, null if none
     */
    protected userTesterPrc(pipeProgress reader, String pfx, int slt, String nam, String command) {
        slot = slt;
        name = nam;
        rdr = reader;
        prefix = pfx;
        if (command == null) {
            shell = null;
            return;
        }
        pipeLine pl = new pipeLine(32768, false);
        final int tim = 600 * 1000;
        pipe = pl.getSide();
        pipe.setTime(tim);
        shell = pipeShell.exec(pipe, command, null, true, false, false, true);
        pipe = pl.getSide();
        pipe.setTime(tim);
        pipe.lineRx = pipeSide.modTyp.modeCRorLF;
        pipe.lineTx = pipeSide.modTyp.modeCR;
        rdr.debugStat(slot + "/" + name + ": starting process");
    }

    public int compareTo(userTesterPrc o) {
        return name.compareTo(o.name);
    }

    /**
     * wait until terminates
     */
    protected void waitFor() {
        if (shell == null) {
            return;
        }
        rdr.debugStat(slot + "/" + name + ": stopping process");
        shell.waitFor();
        pipe.setClose();
    }

    /**
     * forced stop process
     */
    protected void stopNow() {
        if (shell == null) {
            return;
        }
        if (persistent) {
            return;
        }
        rdr.debugStat(slot + "/" + name + ": stopping process");
        shell.kill();
        shell.waitFor();
        pipe.setClose();
    }

    /**
     * get log name
     *
     * @param mod log to get
     * @return log file name
     */
    protected String getLogName(int mod) {
        return userTester.getLogName(prefix, slot, name, mod);
    }

    /**
     * write one char
     *
     * @param i character
     */
    protected void putChar(int i) {
        byte[] buf = new byte[1];
        buf[0] = (byte) i;
        pipe.blockingPut(buf, 0, buf.length);
    }

    /**
     * read one line
     *
     * @return line read, null if nothing
     */
    protected String getLine() {
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

    /**
     * write one line
     *
     * @param s string
     */
    protected void putLine(String s) {
        bits.buf2txt(false, bits.str2lst("tx:" + s), getLogName(4));
        pipe.linePut(s);
    }

    /**
     * perform ping test
     *
     * @param s command
     * @return success rate
     */
    protected int doPing(String s) {
        putLine("ping " + s);
        for (;;) {
            s = getLine();
            if (s == null) {
                return -100000000;
            }
            s = s.toLowerCase();
            if (s.startsWith("result=")) {
                break;
            }
        }
        int i = s.indexOf("=");
        if (i < 0) {
            return -100000000;
        }
        s = s.substring(i + 1, s.length());
        i = s.indexOf("%");
        s = s.substring(0, i);
        i = s.indexOf(".");
        if (i >= 0) {
            s = s.substring(0, i);
        }
        return bits.str2num(s);
    }

    /**
     * perform more pings
     *
     * @param s command
     * @param ned needed success rate
     * @param round retry maximum
     * @return false on success, true on error
     */
    protected boolean morePings(String s, tabIntMatcher ned, int round) {
        rdr.debugStat(slot + "/" + name + ": pinging " + s + ".");
        rdr.setMax(round);
        int i = -1;
        for (int rnd = 0; rnd <= round; rnd++) {
            rdr.setCurr(rnd);
            i = doPing(s);
            bits.buf2txt(false, bits.str2lst("res:" + i + " percent"), getLogName(4));
            if (ned.matches(i)) {
                return false;
            }
            bits.sleep(1000);
        }
        rdr.debugStat(slot + "/" + name + ": test failed: got " + i + ", expected " + ned);
        return true;
    }

    /**
     * get command output
     *
     * @param s command
     * @return returned output, null on error
     */
    protected List<String> getOutput(String s) {
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

    /**
     * get software summary
     *
     * @param inc include string
     * @param exc exclude string
     * @return counter
     */
    protected int getSummary(String inc, String exc) {
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

    /**
     * perform synchronization
     */
    protected void doSync() {
        if (syncr.length() < 1) {
            return;
        }
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

    /**
     * apply config
     *
     * @param cfg config
     */
    protected void applyCfg(List<String> cfg) {
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

    /**
     * read up connections
     */
    protected void readConns() {
        conns = new tabGen<userTesterCon>();
        List<String> l = bits.txt2buf(prefix + slot + name + "-" + cfgInit.hwCfgEnd);
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

class userTesterImg {

    /**
     * filename
     */
    protected final String otherF;

    /**
     * ports
     */
    protected String otherP = null;

    /**
     * command to execute
     */
    protected String otherC1 = null;

    /**
     * command to execute
     */
    protected String otherC2 = null;

    /**
     * command to execute
     */
    protected String otherC3 = null;

    /**
     * number of ports
     */
    protected int otherNS = 0;

    /**
     * ports string
     */
    protected String otherNC = null;

    /**
     * string to wait
     */
    protected String otherW = null;

    /**
     * syncer string
     */
    protected String otherS = null;

    /**
     * default config
     */
    protected List<String> otherD = null;

    /**
     * create instance
     *
     * @param s filename
     */
    protected userTesterImg(String s) {
        otherF = s;
    }

}

class userTesterFtr implements Comparable<userTesterFtr> {

    /**
     * name of file
     */
    protected final String fil;

    /**
     * locker
     */
    protected final syncInt lck = new syncInt(0);

    /**
     * retries
     */
    protected int ret;

    /**
     * already run
     */
    protected int ran;

    /**
     * result
     */
    protected boolean res;

    /**
     * feature name
     */
    protected String ftr;

    /**
     * html line
     */
    protected String htm;

    /**
     * csv line
     */
    protected String csv;

    /**
     * create instance
     *
     * @param fn name of file
     */
    protected userTesterFtr(String fn) {
        fil = fn;
    }

    public int compareTo(userTesterFtr o) {
        return fil.compareTo(o.fil);
    }

    /**
     * get one line
     *
     * @param mod mode
     * @return line
     */
    protected String getter(int mod) {
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

class userTesterWrk implements Runnable {

    /**
     * parent
     */
    protected final userTester lower;

    /**
     * slot to use
     */
    protected final int slot;

    /**
     * create instance
     *
     * @param parent parent
     * @param slt slot to use
     */
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

class userTesterCon implements Comparable<userTesterCon> {

    /**
     * local port
     */
    protected int locP;

    /**
     * remote port
     */
    protected int remP;

    /**
     * persistent process
     */
    protected userTesterPrc perP;

    /**
     * console of process
     */
    protected userTesterCon perC;

    /**
     * interface to use
     */
    protected String ifc;

    /**
     * create instance
     */
    protected userTesterCon() {
    }

    public int compareTo(userTesterCon o) {
        if (locP < o.locP) {
            return -1;
        }
        if (locP > o.locP) {
            return +1;
        }
        return 0;
    }

}

class userTesterChg {

    /**
     * time in text format
     */
    protected final String str;

    /**
     * time in binary format
     */
    protected final long tim;

    /**
     * changes detected
     */
    protected final List<String> txt = new ArrayList<String>();

    /**
     * version information
     */
    protected String ver;

    /**
     * create instance
     *
     * @param a time
     */
    public userTesterChg(String a) {
        a = a.replaceAll(" ", "_");
        str = a;
        tim = bits.str2time(cfgAll.timeZoneName, a);
    }

}

class userTesterCap {

    /**
     * router
     */
    protected final String rtr;

    /**
     * interface
     */
    protected final String ifc;

    /**
     * create instance
     *
     * @param r router
     * @param i interface
     */
    public userTesterCap(String r, String i) {
        rtr = r;
        ifc = i;
    }

}

class userTesterRep {

    /**
     * source
     */
    protected final String src;

    /**
     * target
     */
    protected final String trg;

    /**
     * create instance
     *
     * @param s source
     * @param t target
     */
    public userTesterRep(String s, String t) {
        src = s;
        trg = t;
    }

}
