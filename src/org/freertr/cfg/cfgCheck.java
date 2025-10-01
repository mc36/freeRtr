package org.freertr.cfg;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.freertr.addr.addrIP;
import org.freertr.clnt.clntDns;
import org.freertr.cry.cryHashCrc32;
import org.freertr.pack.packDnsRec;
import org.freertr.pack.packNrpe;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSetting;
import org.freertr.pipe.pipeSide;
import org.freertr.tab.tabGen;
import org.freertr.user.userExec;
import org.freertr.user.userFilter;
import org.freertr.user.userFormat;
import org.freertr.user.userHelp;
import org.freertr.user.userRead;
import org.freertr.util.bits;
import org.freertr.util.cmds;

/**
 * check exporter
 *
 * @author matecsaba
 */
public class cfgCheck implements Comparable<cfgCheck>, cfgGeneric {

    /**
     * name of check
     */
    public String name;

    /**
     * time to answer
     */
    public int time;

    /**
     * ok answers
     */
    public int okNum;

    /**
     * error answers
     */
    public int errNum;

    /**
     * last ok
     */
    public long okTim;

    /**
     * last error
     */
    public long errTim;

    /**
     * last error
     */
    public List<String> errLst;

    /**
     * command to execute
     */
    public String command;

    /**
     * send out command after error text
     */
    public boolean sendCmds;

    /**
     * send out my name after error text
     */
    public boolean sendMyId;

    /**
     * don't send out error/ok
     */
    public boolean noState;

    /**
     * template parameters
     */
    public cfgCheck template;

    /**
     * description
     */
    public String dsc;

    /**
     * error string
     */
    public String err;

    /**
     * ignore all regexp
     */
    public final List<String> allR;

    /**
     * ignore one regexp
     */
    public final List<String> ignR;

    /**
     * require one regexp
     */
    public final List<String> reqR;

    /**
     * ignore all text
     */
    public final List<String> allT;

    /**
     * ignore one text
     */
    public final List<String> ignT;

    /**
     * require one text
     */
    public final List<String> reqT;

    /**
     * list of resolvers
     */
    public tabGen<cfgCheckRes> ress = new tabGen<cfgCheckRes>();

    /**
     * list of replacers
     */
    public tabGen<cfgCheckRep> reps = new tabGen<cfgCheckRep>();

    /**
     * alternate result
     */
    public boolean alternate;

    /**
     * severity
     */
    public int severity = packNrpe.coCri;

    /**
     * last hash
     */
    public int lastHash = 1;

    /**
     * last status
     */
    public int lastStat = packNrpe.coWar;

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("check .*", cmds.tabulator + cmds.negated + cmds.tabulator + "command", null),
        new userFilter("check .*", cmds.tabulator + cmds.negated + cmds.tabulator + "description", null),
        new userFilter("check .*", cmds.tabulator + cmds.negated + cmds.tabulator + "template", null),
        new userFilter("check .*", cmds.tabulator + cmds.negated + cmds.tabulator + "error-text", null),
        new userFilter("check .*", cmds.tabulator + cmds.negated + cmds.tabulator + "alternate", null),
        new userFilter("check .*", cmds.tabulator + "severity critical", null),
        new userFilter("check .*", cmds.tabulator + cmds.negated + cmds.tabulator + "error-states", null),
        new userFilter("check .*", cmds.tabulator + cmds.negated + cmds.tabulator + "error-commands", null),
        new userFilter("check .*", cmds.tabulator + cmds.negated + cmds.tabulator + "error-hostname", null)
    };

    /**
     * create new check
     *
     * @param n name
     */
    public cfgCheck(String n) {
        name = n;
        allR = new ArrayList<String>();
        ignR = new ArrayList<String>();
        reqR = new ArrayList<String>();
        allT = new ArrayList<String>();
        ignT = new ArrayList<String>();
        reqT = new ArrayList<String>();
    }

    public String toString() {
        return name;
    }

    public int compareTo(cfgCheck o) {
        return name.toLowerCase().compareTo(o.name.toLowerCase());
    }

    public String getPrompt() {
        return "check";
    }

    public void getHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "rename", "rename this check");
        l.add(null, false, 2, new int[]{-1}, "<str>", "set new name");
        l.add(null, false, 1, new int[]{2}, "resolve", "resolve the regexp group a to hostname");
        l.add(null, false, 2, new int[]{2, -1}, "<str>", "text to resolv");
        l.add(null, false, 1, new int[]{2}, "replace", "replace from one string to another");
        l.add(null, false, 2, new int[]{3}, "<str>", "source string");
        l.add(null, false, 3, new int[]{3, -1}, "<str>", "target string");
        l.add(null, false, 1, new int[]{2, -1}, "train", "train command to current result");
        l.add(null, false, 2, new int[]{2, -1}, "<str>", "text");
        l.add(null, false, 1, new int[]{-1}, "alternate", "alternate reported state on diff change");
        l.add(null, false, 1, new int[]{2}, "severity", "severity level");
        l.add(null, false, 2, new int[]{-1}, "critical", "critical");
        l.add(null, false, 2, new int[]{-1}, "warning", "warning");
        l.add(null, false, 2, new int[]{-1}, "unknown", "unknown");
        l.add(null, false, 2, new int[]{-1}, "ok", "ok");
        l.add(null, false, 1, new int[]{2}, "template", "template arameters");
        l.add(null, false, 2, new int[]{-1}, "<name:chk>", "name of check");
        l.add(null, false, 1, new int[]{2}, "command", "specify command to execute");
        l.add(null, false, 2, new int[]{2, -1}, "<str>", "command");
        l.add(null, false, 1, new int[]{2}, "description", "specify description");
        l.add(null, false, 2, new int[]{2, -1}, "<str>", "description");
        l.add(null, false, 1, new int[]{2}, "error-text", "specify error text");
        l.add(null, false, 2, new int[]{2, -1}, "<str>", "text");
        l.add(null, false, 1, new int[]{-1}, "error-states", "remove state of messages");
        l.add(null, false, 1, new int[]{-1}, "error-commands", "include commands in states");
        l.add(null, false, 1, new int[]{-1}, "error-hostname", "include local hostname in states");
        l.add(null, false, 1, new int[]{2}, "require-regexp", "require one regexp line");
        l.add(null, false, 2, new int[]{2, -1}, "<str>", "text");
        l.add(null, false, 1, new int[]{2}, "ignore-regexp", "ignore one regexp line");
        l.add(null, false, 2, new int[]{2, -1}, "<str>", "text");
        l.add(null, false, 1, new int[]{2}, "ignorall-regexp", "ignore all regexp line");
        l.add(null, false, 2, new int[]{2, -1}, "<str>", "text");
        l.add(null, false, 1, new int[]{2}, "require-text", "require one text line");
        l.add(null, false, 2, new int[]{2, -1}, "<str>", "text");
        l.add(null, false, 1, new int[]{2}, "ignore-text", "ignore one text line");
        l.add(null, false, 2, new int[]{2, -1}, "<str>", "text");
        l.add(null, false, 1, new int[]{2}, "ignorall-text", "ignore all text line");
        l.add(null, false, 2, new int[]{2, -1}, "<str>", "text");
    }

    public List<String> getShRun(int filter) {
        List<String> l = new ArrayList<String>();
        l.add("check " + name);
        cmds.cfgLine(l, command == null, cmds.tabulator, "command", "" + command);
        cmds.cfgLine(l, template == null, cmds.tabulator, "template", "" + template);
        for (int i = 0; i < ress.size(); i++) {
            l.add(cmds.tabulator + "resolve " + ress.get(i));
        }
        for (int i = 0; i < reps.size(); i++) {
            l.add(cmds.tabulator + "replace " + reps.get(i));
        }
        cmds.cfgLine(l, dsc == null, cmds.tabulator, "description", dsc);
        cmds.cfgLine(l, err == null, cmds.tabulator, "error-text", err);
        cmds.cfgLine(l, !alternate, cmds.tabulator, "alternate", "");
        l.add(cmds.tabulator + "severity " + packNrpe.code2string(severity));
        cmds.cfgLine(l, !sendCmds, cmds.tabulator, "error-commands", "");
        cmds.cfgLine(l, !sendMyId, cmds.tabulator, "error-hostname", "");
        cmds.cfgLine(l, !noState, cmds.tabulator, "error-states", "");
        for (int i = 0; i < ignT.size(); i++) {
            l.add(cmds.tabulator + "ignore-text " + ignT.get(i));
        }
        for (int i = 0; i < allT.size(); i++) {
            l.add(cmds.tabulator + "ignorall-text " + allT.get(i));
        }
        for (int i = 0; i < ignR.size(); i++) {
            l.add(cmds.tabulator + "ignore-regexp " + ignR.get(i));
        }
        for (int i = 0; i < allR.size(); i++) {
            l.add(cmds.tabulator + "ignorall-regexp " + allR.get(i));
        }
        for (int i = 0; i < reqT.size(); i++) {
            l.add(cmds.tabulator + "require-text " + reqT.get(i));
        }
        for (int i = 0; i < reqR.size(); i++) {
            l.add(cmds.tabulator + "require-regexp " + reqR.get(i));
        }
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        if ((filter & 1) == 0) {
            return l;
        }
        return userFilter.filterText(l, defaultF);
    }

    public void doCfgStr(cmds cmd) {
        String s = cmd.word();
        boolean negated = s.equals(cmds.negated);
        if (negated) {
            s = cmd.word();
        }
        if (s.equals("rename")) {
            s = cmd.word();
            cfgCheck v = cfgAll.checkFind(s, false);
            if (v != null) {
                cmd.error("already exists");
                return;
            }
            name = s;
            return;
        }
        if (s.equals("resolve")) {
            s = cmd.getRemaining();
            if (negated) {
                ress.del(new cfgCheckRes(s));
            } else {
                ress.add(new cfgCheckRes(s));
            }
            return;
        }
        if (s.equals("replace")) {
            String a = cmd.word();
            s = cmd.getRemaining();
            if (negated) {
                reps.del(new cfgCheckRep(a, s));
            } else {
                reps.add(new cfgCheckRep(a, s));
            }
            return;
        }
        if (s.equals("command")) {
            command = cmd.getRemaining();
            if (negated) {
                command = null;
            }
            return;
        }
        if (s.equals("description")) {
            dsc = cmd.getRemaining();
            if (negated) {
                dsc = null;
            }
            return;
        }
        if (s.equals("error-text")) {
            err = cmd.getRemaining();
            if (negated) {
                err = null;
            }
            return;
        }
        if (s.equals("error-commands")) {
            sendCmds = !negated;
            return;
        }
        if (s.equals("error-hostname")) {
            sendMyId = !negated;
            return;
        }
        if (s.equals("error-states")) {
            noState = !negated;
            return;
        }
        if (s.equals("alternate")) {
            alternate = !negated;
            return;
        }
        if (s.equals("severity")) {
            severity = packNrpe.string2code(cmd.word());
            return;
        }
        if (s.equals("require-regexp")) {
            s = cmd.getRemaining();
            if (negated) {
                reqR.remove(s);
            } else {
                reqR.add(s);
            }
            return;
        }
        if (s.equals("ignore-regexp")) {
            s = cmd.getRemaining();
            if (negated) {
                ignR.remove(s);
            } else {
                ignR.add(s);
            }
            return;
        }
        if (s.equals("ignorall-regexp")) {
            s = cmd.getRemaining();
            if (negated) {
                allR.remove(s);
            } else {
                allR.add(s);
            }
            return;
        }
        if (s.equals("require-text")) {
            s = cmd.getRemaining();
            if (negated) {
                reqT.remove(s);
            } else {
                reqT.add(s);
            }
            return;
        }
        if (s.equals("ignore-text")) {
            s = cmd.getRemaining();
            if (negated) {
                ignT.remove(s);
            } else {
                ignT.add(s);
            }
            return;
        }
        if (s.equals("ignorall-text")) {
            s = cmd.getRemaining();
            if (negated) {
                allT.remove(s);
            } else {
                allT.add(s);
            }
            return;
        }
        if (s.equals("template")) {
            template = cfgAll.checkFind(cmd.word(), false);
            if (template == null) {
                cmd.error("no such check");
                return;
            }
            if (negated) {
                template = null;
            }
            return;
        }
        if (s.equals("train")) {
            s = cmd.getRemaining();
            if (s.length() < 1) {
                doTrain();
            } else {
                doTrain(s);
            }
            return;
        }
        cmd.badCmd();
    }

    /**
     * get result
     *
     * @return result
     */
    public List<String> getResult() {
        if (command == null) {
            return new ArrayList<String>();
        }
        pipeLine pl = new pipeLine(1024 * 1024, false);
        pipeSide pip = pl.getSide();
        pip.lineTx = pipeSide.modTyp.modeCRLF;
        pip.lineRx = pipeSide.modTyp.modeCRorLF;
        userRead rdr = new userRead(pip, null);
        pip.settingsPut(pipeSetting.tabMod, userFormat.tableMode.raw);
        pip.settingsPut(pipeSetting.height, 0);
        userExec exe = new userExec(pip, rdr);
        exe.privileged = true;
        pip.setTime(120000);
        String a = exe.repairCommand(command);
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

    private void delReg(List<String> lst) {
        for (int o = 0; o < reqR.size(); o++) {
            String s = reqR.get(o);
            for (int i = 0; i < lst.size(); i++) {
                if (!lst.get(i).matches(s)) {
                    continue;
                }
                lst.remove(i);
                break;
            }
        }
    }

    private void delIgn(List<String> lst) {
        for (int o = 0; o < ignT.size(); o++) {
            String s = ignT.get(o);
            for (int i = 0; i < lst.size(); i++) {
                if (!lst.get(i).equals(s)) {
                    continue;
                }
                lst.remove(i);
                break;
            }
        }
        for (int o = 0; o < allT.size(); o++) {
            String s = allT.get(o);
            for (int i = lst.size() - 1; i >= 0; i--) {
                if (!lst.get(i).equals(s)) {
                    continue;
                }
                lst.remove(i);
            }
        }
        for (int o = 0; o < ignR.size(); o++) {
            String s = ignR.get(o);
            for (int i = 0; i < lst.size(); i++) {
                if (!lst.get(i).matches(s)) {
                    continue;
                }
                lst.remove(i);
                break;
            }
        }
        for (int o = 0; o < allR.size(); o++) {
            String s = allR.get(o);
            for (int i = lst.size() - 1; i >= 0; i--) {
                if (!lst.get(i).matches(s)) {
                    continue;
                }
                lst.remove(i);
            }
        }
    }

    private String makeFancy(String l) {
        for (int i = 0; i < ress.size(); i++) {
            cfgCheckRes r = ress.get(i);
            if (r == null) {
                continue;
            }
            l = r.doWork(l);
        }
        for (int i = 0; i < reps.size(); i++) {
            cfgCheckRep r = reps.get(i);
            if (r == null) {
                continue;
            }
            l = l.replaceAll(r.src, r.trg);
        }
        if (template == null) {
            return l;
        }
        return template.makeFancy(l);
    }

    private void doCheckMiss(List<String> lst, List<String> res) {
        for (int o = 0; o < reqT.size(); o++) {
            String s = reqT.get(o);
            boolean ok = false;
            for (int i = 0; i < lst.size(); i++) {
                if (!lst.get(i).equals(s)) {
                    continue;
                }
                lst.remove(i);
                ok = true;
                break;
            }
            if (ok) {
                continue;
            }
            res.add("- " + makeFancy(s));
        }
        for (int o = 0; o < reqR.size(); o++) {
            String s = reqR.get(o);
            boolean ok = false;
            for (int i = 0; i < lst.size(); i++) {
                if (!lst.get(i).matches(s)) {
                    continue;
                }
                lst.remove(i);
                ok = true;
                break;
            }
            if (ok) {
                continue;
            }
            res.add("- " + makeFancy(s));
        }
    }

    private void doCheckExtra(List<String> lst, List<String> res) {
        for (int i = 0; i < lst.size(); i++) {
            res.add("+ " + makeFancy(lst.get(i)));
        }
    }

    private String getHeadLine(List<String> lst) {
        String s = "";
        boolean b = noState;
        if (template != null) {
            b |= template.noState;
        }
        if (!b) {
            s += "ERROR ";
        }
        s += lst.size() + " ";
        String a = err;
        if ((a == null) && (template != null)) {
            a = template.err;
        }
        if (a == null) {
            a = "line(s) in diff";
        }
        s += a + " ";
        b = sendMyId;
        if (template != null) {
            b |= template.sendMyId;
        }
        if (b) {
            s += " - " + cfgAll.hostName + "#";
        }
        b = sendCmds;
        if (template != null) {
            b |= template.sendCmds;
        }
        if (b) {
            s += command + " ";
        }
        return s;
    }

    /**
     * do the checking
     *
     * @return true if up, false if down
     */
    public boolean getStatus() {
        return doCheckText().size() < 1;
    }

    /**
     * get summary line
     *
     * @return string
     */
    public String getShSum() {
        return name + "|" + cmds.upDown(getStatus()) + "|" + (okNum + errNum) + "|" + time + "|" + okNum + "|" + bits.timePast(okTim) + "|" + errNum + "|" + bits.timePast(errTim);
    }

    /**
     * do the checking
     *
     * @return result
     */
    public List<String> doCheckText() {
        long tim = bits.getTime();
        List<String> lst = getResult();
        List<String> res = new ArrayList<String>();
        delIgn(lst);
        if (template != null) {
            template.delIgn(lst);
            template.doCheckMiss(lst, res);
        }
        doCheckMiss(lst, res);
        doCheckExtra(lst, res);
        time = (int) (bits.getTime() - tim);
        if (res.size() < 1) {
            okNum++;
            okTim = tim;
        } else {
            errNum++;
            errTim = tim;
            errLst = lst;
        }
        return res;
    }

    /**
     * get nrpe report
     *
     * @param pck packet to update
     */
    public void getReportNrpe(packNrpe pck) {
        pck.typ = packNrpe.tyRep;
        List<String> lst = doCheckText();
        if (lst.size() < 1) {
            pck.cod = packNrpe.coOk;
            pck.str = "OK";
            if (dsc != null) {
                pck.str += " " + dsc;
            } else if (template != null) {
                if (template.dsc != null) {
                    pck.str += " " + template.dsc;
                }
            }
            return;
        }
        pck.cod = severity;
        pck.str = getHeadLine(lst).trim();
        for (int i = 0; i < lst.size(); i++) {
            pck.str += new String(pck.sep) + lst.get(i).trim();
        }
        if (!alternate) {
            if (template == null) {
                return;
            }
            if (!template.alternate) {
                return;
            }
        }
        cryHashCrc32 hsh = new cryHashCrc32(cryHashCrc32.polyCrc32i);
        hsh.init();
        hsh.update(pck.str.getBytes());
        int i = hsh.getCrc();
        if (i == lastHash) {
            pck.cod = lastStat;
        } else {
            lastHash = i;
            if (lastStat == packNrpe.coCri) {
                i = packNrpe.coWar;
            } else {
                i = packNrpe.coCri;
            }
            lastStat = i;
            pck.cod = i;
        }
    }

    /**
     * train the check
     *
     * @param ned line to train
     */
    public void doTrain(String ned) {
        List<String> lst = getResult();
        delIgn(lst);
        delReg(lst);
        if (template != null) {
            template.delIgn(lst);
            template.delReg(lst);
            template.doCheckMiss(lst, new ArrayList<String>());
        }
        for (int i = reqT.size() - 1; i >= 0; i--) {
            if (reqT.get(i).indexOf(ned) >= 0) {
                reqT.remove(i);
            }
        }
        for (int i = 0; i < lst.size(); i++) {
            String a = lst.get(i);
            if (a.indexOf(ned) < 0) {
                continue;
            }
            reqT.add(a);
        }
    }

    /**
     * train the check
     */
    public void doTrain() {
        List<String> lst = getResult();
        delIgn(lst);
        delReg(lst);
        if (template != null) {
            template.delIgn(lst);
            template.delReg(lst);
            template.doCheckMiss(lst, new ArrayList<String>());
        }
        reqT.clear();
        for (int i = 0; i < lst.size(); i++) {
            reqT.add(lst.get(i));
        }
    }

    /**
     * get show
     *
     * @return result
     */
    public userFormat getShowDetail() {
        userFormat res = new userFormat("|", "category|value");
        res.add("name|" + name);
        packNrpe nrp = new packNrpe();
        getReportNrpe(nrp);
        res.add("nrpe|" + packNrpe.code2string(nrp.cod));
        res.add("template|" + template);
        res.add("description|" + dsc);
        res.add("command|" + command);
        res.add("error|" + err);
        res.add("alternate|" + alternate);
        res.add("severity|" + packNrpe.code2string(severity));
        res.add("asked|" + (okNum + errNum) + " times");
        res.add("reply|" + time + " ms");
        res.add("passed|" + okNum + " times");
        res.add("last|" + bits.time2str(cfgAll.timeZoneName, okTim + cfgAll.timeServerOffset, 3));
        res.add("ago|" + bits.timePast(okTim));
        res.add("failed|" + errNum + " times");
        res.add("last|" + bits.time2str(cfgAll.timeZoneName, errTim + cfgAll.timeServerOffset, 3));
        res.add("ago|" + bits.timePast(errTim));
        return res;
    }

    /**
     * get show
     *
     * @return result
     */
    public List<String> getShowError() {
        List<String> res = new ArrayList<String>();
        if (errLst != null) {
            res.addAll(errLst);
        }
        return res;
    }

}

class cfgCheckRep implements Comparable<cfgCheckRep> {

    public final String src;

    public final String trg;

    public cfgCheckRep(String s, String t) {
        src = s;
        trg = t;
    }

    public String toString() {
        return src + " " + trg;
    }

    public int compareTo(cfgCheckRep o) {
        return src.toLowerCase().compareTo(o.src.toLowerCase());
    }

}

class cfgCheckRes implements Comparable<cfgCheckRes> {

    public final String nam;

    public cfgCheckRes(String s) {
        nam = s;
    }

    public String toString() {
        return nam;
    }

    public int compareTo(cfgCheckRes o) {
        return nam.toLowerCase().compareTo(o.nam.toLowerCase());
    }

    public String doWork(String l) {
        String as;
        try {
            Pattern pat = Pattern.compile(nam);
            Matcher mat = pat.matcher(l);
            if (!mat.find()) {
                return l;
            }
            as = mat.group("a");
        } catch (Exception e) {
            as = null;
        }
        if (as == null) {
            return l;
        }
        addrIP ad = new addrIP();
        if (ad.fromString(as)) {
            return l;
        }
        clntDns clnt = new clntDns();
        clnt.doResolvList(cfgAll.nameServerAddr, packDnsRec.generateReverse(ad), false, packDnsRec.typePTR);
        String dn = clnt.getPTR();
        if (dn == null) {
            return l;
        }
        return l.replaceAll(as, dn);
    }

}
