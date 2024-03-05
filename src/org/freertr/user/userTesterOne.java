package org.freertr.user;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrMac;
import org.freertr.cfg.cfgInit;
import org.freertr.pipe.pipeDiscard;
import org.freertr.pipe.pipeProgress;
import org.freertr.pipe.pipeShell;
import org.freertr.pipe.pipeSide;
import org.freertr.spf.spfCalc;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabIntMatcher;
import org.freertr.util.bits;
import org.freertr.util.cmds;

/**
 * one tester runner
 *
 * @author matecsaba
 */
public class userTesterOne {

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
        prefix = frm.temp + "slot";
        slot = frm.slot + slt;
        config = frm.config;
        unexit = frm.unexit;
        wait = frm.wait;
        reapply = frm.reapply;
        restart = frm.restart;
        cfgarch = frm.cfgarch;
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
            userTesterPrc p = new userTesterPrc(rdr, prefix, slot, rn, s);
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
            pipeShell.exec(img.convert2udp(img.otherC1, prefix + slot + rn, ctL, lps, rps, mcs), null, true, false, true);
            pipeShell.exec(img.convert2udp(img.otherC2, prefix + slot + rn, ctL, lps, rps, mcs), null, true, false, true);
            s = img.convert2udp(img.otherC3, prefix + slot + rn, ctL, lps, rps, mcs);
            cfg.add("!" + s);
            bits.buf2txt(true, cfg, prefix + slot + rn + "-" + cfgInit.hwCfgEnd);
            userTesterPrc p = new userTesterPrc(rdr, prefix, slot, rn, s);
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
            bits.buf2txt(true, cfg, userTesterPrc.getLogName(prefix, slot, rn, 1));
            cfg = new ArrayList<String>();
            cfg.add("hostname " + rn);
            cfg.add("logging milliseconds");
            cfg.add("logging file debug " + userTesterPrc.getLogName(prefix, slot, rn, 1));
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
            userTesterPrc p = new userTesterPrc(rdr, prefix, slot, rn, s);
            bits.buf2txt(true, bits.str2lst(""), p.getLogName(4));
            if (write) {
                for (i = 0; i < restart; i++) {
                    p.putLine("write");
                    p.putLine("reload force");
                    p.waitFor();
                    p = new userTesterPrc(rdr, prefix, slot, rn, s);
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
                p.putLine("packet capture " + cap.ifc + " " + prefix + slot + "log-" + rn + "-" + cap.ifc + ".pcap");
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
