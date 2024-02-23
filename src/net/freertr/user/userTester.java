package net.freertr.user;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.cfg.cfgAll;
import net.freertr.enc.enc7bit;
import net.freertr.pipe.pipeProgress;
import net.freertr.serv.servHttp;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabIntMatcher;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.logger;
import net.freertr.util.syncInt;
import net.freertr.util.verCore;
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
    protected String jvp = " XmxZZZm -jar " + version.getFileName();

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
        releaseN = version.headLine;
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
        releaseN = verCore.name;
        jvn = "sid";
        jvp = "medium";
        releaseV = verCore.author;
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
            s = "qemu-system-x86_64 -M " + b + " -monitor none -serial stdio -nographic -no-reboot -enable-kvm -cpu host -smp cores=" + o + ",threads=1,sockets=1 -drive file=" + a + ",format=raw,cache=unsafe -m " + i;
            a = persistD.remove(0);
            for (i = 0; i < 8; i++) {
                int rp = persistP + ((i + 1) * 4);
                int lp = rp + 1;
                s += " -netdev socket,id=n" + i + ",udp=127.0.0.1:" + rp + ",localaddr=:" + lp + " -device " + a + ",netdev=n" + i + ",mac=00:00:00:00:11:" + bits.toHexB(i);
            }
            persistP += (4 * bits.str2num(persistD.remove(0)));
            persistC = new userTesterPrc(rdr, temp, slot, "persist", s);
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
        txt.add("jvm: " + jvn + jvp + "<br/>");
        txt.add("<br/>");
        txt.add("<table><thead><tr><td><b>file</b></td><td><b>result</b></td><td><b>test</b></td></tr></thead><tbody>");
        txt.addAll(features2list(finished, 3));
        txt.add("</tbody></table></body></html>");
        bits.buf2txt(true, txt, "rtr" + beg + ".html");
        txt = new ArrayList<String>();
        txt.add("url;file;result;test");
        txt.add("-;-;-;" + releaseN);
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
