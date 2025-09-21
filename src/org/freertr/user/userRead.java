package org.freertr.user;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgInit;
import org.freertr.clnt.clntHttp;
import org.freertr.enc.enc7bit;
import org.freertr.enc.encUrl;
import org.freertr.enc.encXml;
import org.freertr.pipe.pipeConnect;
import org.freertr.pipe.pipeReader;
import org.freertr.pipe.pipeSetting;
import org.freertr.pipe.pipeSide;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.debugger;
import org.freertr.util.logger;

/**
 * read one line from the user
 *
 * @author matecsaba
 */
public class userRead implements Comparator<String> {

    private pipeSide pipe; // pipe to use

    private String prompt; // current prompt

    private userHelp help; // help context

    private String[] histD; // history data

    private int histN; // history number

    private String curr; // current line

    private String clip; // clipboard

    private int pos; // position of next typed character goes

    private int len; // length of current line

    private int beg; // first character displayed

    private boolean clear; // clearing of display required

    private String filterS; // filter string

    private mode filterM; // filter mode

    private String filterN; // next filters

    private String filterO; // original command

    private int columnL; // column line

    private int columnN; // column number

    private String columnS; // column separator

    private int columnB; // beginning of column

    private int columnE; // ending of column

    /**
     * filter modes
     */
    private enum mode {

        /**
         * no filtering
         */
        raw,
        /**
         * begin
         */
        begin,
        /**
         * begin with headers
         */
        hbegin,
        /**
         * end
         */
        end,
        /**
         * include
         */
        include,
        /**
         * include with headers
         */
        hinclude,
        /**
         * exclude
         */
        exclude,
        /**
         * remove
         */
        remove,
        /**
         * section names
         */
        headers,
        /**
         * first lines
         */
        first,
        /**
         * last lines
         */
        last,
        /**
         * last lines with header
         */
        hlast,
        /**
         * count entities
         */
        count,
        /**
         * sort entities
         */
        sort,
        /**
         * padded sort entities
         */
        padsort,
        /**
         * reverse sort entities
         */
        revsort,
        /**
         * reverse padded sort entities
         */
        repsort,
        /**
         * unique entities
         */
        uniq,
        /**
         * hide columns
         */
        hide,
        /**
         * redirection
         */
        redirect,
        /**
         * pastebin
         */
        pastebin,
        /**
         * text viewer
         */
        viewer,
        /**
         * level hierarchy
         */
        level,
        /**
         * csv list
         */
        csv,
        /**
         * htmlized
         */
        html,
        /**
         * xmlized
         */
        xml,
        /**
         * prepend line numbers
         */
        linenum,
        /**
         * hacker writing
         */
        hacked,
        /**
         * specified section
         */
        section,
        /**
         * set/delete mode
         */
        setdel,
        /**
         * c code mode
         */
        ccode,
        /**
         * summary
         */
        summary

    }

    /**
     * line defaults text
     */
    public final static userFilter[] linedefF = {
        new userFilter(".*", cmds.tabulator + cmds.negated + cmds.tabulator + "exec interface", null),
        new userFilter(".*", cmds.tabulator + "exec timeout 300000", null),
        new userFilter(".*", cmds.tabulator + "exec width 79", null),
        new userFilter(".*", cmds.tabulator + "exec height 24", null),
        new userFilter(".*", cmds.tabulator + "exec history 64", null),
        new userFilter(".*", cmds.tabulator + "exec riblines 8192", null),
        new userFilter(".*", cmds.tabulator + cmds.negated + cmds.tabulator + "exec timestamp", null),
        new userFilter(".*", cmds.tabulator + "exec colorize normal", null),
        new userFilter(".*", cmds.tabulator + "exec boxer normal", null),
        new userFilter(".*", cmds.tabulator + "exec background black", null),
        new userFilter(".*", cmds.tabulator + "exec foreground white", null),
        new userFilter(".*", cmds.tabulator + "exec prompt bright-green", null),
        new userFilter(".*", cmds.tabulator + "exec header bright-yellow", null),
        new userFilter(".*", cmds.tabulator + "exec ansimode normal", null),
        new userFilter(".*", cmds.tabulator + cmds.negated + cmds.tabulator + "exec spacetab", null),
        new userFilter(".*", cmds.tabulator + cmds.negated + cmds.tabulator + "exec capslock", null),
        new userFilter(".*", cmds.tabulator + cmds.negated + cmds.tabulator + "exec bells", null),
        new userFilter(".*", cmds.tabulator + "exec tablemode normal", null),
        new userFilter(".*", cmds.tabulator + "exec welcome welcome", null),
        new userFilter(".*", cmds.tabulator + "exec before before:", null),
        new userFilter(".*", cmds.tabulator + "exec ready line ready", null),
        new userFilter(".*", cmds.tabulator + "exec bye see you later", null),
        new userFilter(".*", cmds.tabulator + cmds.negated + cmds.tabulator + "exec logging", null),
        new userFilter(".*", cmds.tabulator + "exec privilege 15", null),
        new userFilter(".*", cmds.tabulator + "exec autocommand ", null),
        new userFilter(".*", cmds.tabulator + "exec banner", null),
        new userFilter(".*", cmds.tabulator + "exec title", null),
        new userFilter(".*", cmds.tabulator + cmds.negated + cmds.tabulator + "exec detect", null),
        new userFilter(".*", cmds.tabulator + cmds.negated + cmds.tabulator + "exec expirity", null),
        new userFilter(".*", cmds.tabulator + cmds.negated + cmds.tabulator + "exec monitor", null),
        new userFilter(".*", cmds.tabulator + cmds.negated + cmds.tabulator + "exec autohangup", null),
        new userFilter(".*", cmds.tabulator + "login timeout 60000", null),
        new userFilter(".*", cmds.tabulator + "login retry 3", null),
        new userFilter(".*", cmds.tabulator + "login delay 3000", null),
        new userFilter(".*", cmds.tabulator + "login user username:", null),
        new userFilter(".*", cmds.tabulator + "login pass password:", null),
        new userFilter(".*", cmds.tabulator + "login fail authentication failed", null),
        new userFilter(".*", cmds.tabulator + "login activate 13", null),
        new userFilter(".*", cmds.tabulator + "login deactivate 65536", null),
        new userFilter(".*", cmds.tabulator + "login escape 3", null),
        new userFilter(".*", cmds.tabulator + cmds.negated + cmds.tabulator + "login stars", null),
        new userFilter(".*", cmds.tabulator + cmds.negated + cmds.tabulator + "login logging", null),
        new userFilter(".*", cmds.tabulator + "login last none", null)
    };

    /**
     * constructs new reader for a pipeline
     *
     * @param pip pipeline to use as input
     * @param parent line to use
     */
    public userRead(pipeSide pip, userLine parent) {
        pipe = pip;
        clip = "";
        filterS = "";
        if (parent == null) {
            setHistory(64);
            pipe.settingsAdd(pipeSetting.spacTab, false);
            pipe.settingsAdd(pipeSetting.capsLock, false);
            pipe.settingsAdd(pipeSetting.termBells, false);
            pipe.settingsAdd(pipeSetting.ansiMode, userScreen.ansiMode.normal);
            pipe.settingsAdd(pipeSetting.logging, false);
            pipe.settingsAdd(pipeSetting.times, false);
            pipe.settingsAdd(pipeSetting.passStar, false);
            pipe.settingsAdd(pipeSetting.colors, userFormat.colorMode.normal);
            pipe.settingsAdd(pipeSetting.boxer, userFormat.boxerMode.normal);
            pipe.settingsAdd(pipeSetting.colNormal, userScreen.colWhite);
            pipe.settingsAdd(pipeSetting.colPrompt, userScreen.colBrGreen);
            pipe.settingsAdd(pipeSetting.colHeader, userScreen.colBrYellow);
            pipe.settingsAdd(pipeSetting.riblines, 8192);
            pipe.settingsAdd(pipeSetting.width, 79);
            pipe.settingsAdd(pipeSetting.height, 24);
            pipe.settingsAdd(pipeSetting.tabMod, userFormat.tableMode.normal);
            pipe.settingsAdd(pipeSetting.deactive, 65536);
            pipe.settingsAdd(pipeSetting.escape, 65536);
            return;
        }
        setHistory(parent.execHistory);
        pipe.settingsAdd(pipeSetting.spacTab, parent.execSpace);
        pipe.settingsAdd(pipeSetting.capsLock, parent.execCaps);
        pipe.settingsAdd(pipeSetting.ansiMode, parent.ansiMode);
        pipe.settingsAdd(pipeSetting.termBells, parent.execBells);
        pipe.settingsAdd(pipeSetting.logging, parent.execLogging);
        pipe.settingsAdd(pipeSetting.times, parent.execTimes);
        pipe.settingsAdd(pipeSetting.passStar, parent.passStars);
        pipe.settingsAdd(pipeSetting.colors, parent.execColor);
        pipe.settingsAdd(pipeSetting.boxer, parent.execBoxer);
        pipe.settingsAdd(pipeSetting.colNormal, parent.execColNrm);
        pipe.settingsAdd(pipeSetting.colPrompt, parent.execColPrm);
        pipe.settingsAdd(pipeSetting.colHeader, parent.execColHdr);
        pipe.settingsAdd(pipeSetting.riblines, parent.execRibLines);
        pipe.settingsAdd(pipeSetting.width, parent.execWidth);
        pipe.settingsAdd(pipeSetting.height, parent.execHeight);
        pipe.settingsAdd(pipeSetting.tabMod, parent.execTables);
        pipe.settingsAdd(pipeSetting.deactive, parent.promptDeActive);
        pipe.settingsAdd(pipeSetting.escape, parent.promptEscape);
    }

    /**
     * set width of screen
     *
     * @param pip pipe to set
     * @param siz size
     */
    public static void setTermWdt(pipeSide pip, int siz) {
        if (siz < 1) {
            return;
        }
        if (siz < 20) {
            siz = 20;
        }
        pip.settingsPut(pipeSetting.width, siz);
    }

    /**
     * set length of route listing
     *
     * @param pip pipe to set
     * @param siz size
     */
    public static void setRibLin(pipeSide pip, int siz) {
        if (siz < 5) {
            siz = 5;
        }
        pip.settingsPut(pipeSetting.riblines, siz);
    }

    /**
     * set height of screen
     *
     * @param pip pipe to set
     * @param siz size
     */
    public static void setTermLen(pipeSide pip, int siz) {
        if (siz < 0) {
            siz = 0;
        }
        pip.settingsPut(pipeSetting.height, siz);
    }

    /**
     * set context in which input will be processed
     *
     * @param hlp helper instance
     * @param prmt promtp to show
     */
    public void setContext(userHelp hlp, String prmt) {
        help = hlp;
        prompt = prmt;
    }

    /**
     * get command history
     *
     * @return history
     */
    public List<String> getHistory() {
        List<String> lst = new ArrayList<String>();
        for (int i = histD.length - 1; i >= 0; i--) {
            String a = histD[i];
            if (a == null) {
                continue;
            }
            a = a.trim();
            if (a.length() < 1) {
                continue;
            }
            lst.add(a);
        }
        return lst;
    }

    /**
     * set size of history buffer
     *
     * @param num size
     */
    public void setHistory(int num) {
        if (num < 1) {
            num = 1;
        }
        String[] d = new String[num];
        for (int i = 0; i < num; i++) {
            d[i] = "";
        }
        if (histD != null) {
            if (num > histD.length) {
                num = histD.length;
            }
            for (int i = 0; i < num; i++) {
                d[i] = histD[i];
            }
        }
        histD = d;
    }

    private int doMorePrompt() {
        for (;;) {
            pipe.strPut("\r  -=[more]=-\r");
            byte[] buf = new byte[1];
            if (pipe.blockingGet(buf, 0, buf.length) != buf.length) {
                return 3;
            }
            pipe.strPut("\r                   \r");
            switch (buf[0]) {
                case 13: // enter
                    return 2;
                case 32: // space
                    return 1;
                case 27: // escape
                case 3: // ctrl+c
                case 113: // q
                case 81: // Q
                    buf = new byte[32];
                    pipe.nonBlockGet(buf, 0, buf.length);
                    return 3;
            }
        }
    }

    private void findColumn(List<String> lst) {
        String s = lst.get(columnL);
        columnN = 0;
        columnS = " ";
        columnB = s.indexOf(filterS);
        columnE = columnB + filterS.length();
        if (columnB < 0) {
            return;
        }
        for (; columnE < s.length(); columnE++) {
            String a = s.substring(columnB, columnE).trim();
            if (!a.equals(filterS)) {
                columnE--;
                break;
            }
        }
        if (columnB > 0) {
            columnS = s.substring(columnB - 1, columnB);
            String a = s.substring(0, columnB);
            columnN = a.length() - a.replaceAll(columnS, "").length();
        }
        if (columnE < s.length()) {
            return;
        }
        for (int i = 0; i < lst.size(); i++) {
            int o = lst.get(i).length();
            if (o > columnE) {
                columnE = o;
            }
        }
    }

    private String getColText(String o1) {
        int i = columnE;
        int p = columnB;
        int o = o1.length();
        if (i > o) {
            i = o;
        }
        if (p > o) {
            p = o;
        }
        return o1.substring(p, i);
    }

    public int compare(String o1, String o2) {
        return getColText(o1).compareTo(getColText(o2));
    }

    private cmds doSummary(userFormat.tableMode tabMod, String a) {
        switch (tabMod) {
            case raw:
            case csv:
                a = a.replaceAll(";", " ").trim();
                break;
            case fancy:
            case table:
                a = a.replaceAll("\\|", " ").trim();
                break;
            case html:
            case normal:
                break;
        }
        return new cmds("row", a);
    }

    private void doPadCol(List<String> lst) {
        for (int i = 0; i < lst.size(); i++) {
            String a = lst.get(i);
            int o = a.length();
            if (o < columnB) {
                continue;
            }
            String b = a.substring(0, columnB);
            String c = "";
            if (o > columnE) {
                c = a.substring(columnE, o);
                a = a.substring(columnB, columnE).trim();
            } else {
                a = a.substring(columnB, o);
            }
            a = bits.padBeg(a, columnE - columnB, " ");
            lst.set(i, b + a + " " + c);
        }
    }

    private void doBegin(List<String> lst, int num) {
        if (num <= 0) {
            lst.clear();
            return;
        }
        num--;
        for (int i = num; i >= 0; i--) {
            lst.remove(i);
        }
    }

    private List<String> doInclude(List<String> lst) {
        List<String> res = new ArrayList<String>();
        for (int i = 0; i < lst.size(); i++) {
            String s = lst.get(i);
            boolean b = false;
            try {
                b = s.matches(filterS);
            } catch (Exception e) {
            }
            if (!b) {
                continue;
            }
            res.add(s);
        }
        return res;
    }

    private void doLast(List<String> lst, int num) {
        for (int i = lst.size() - num; i >= 0; i--) {
            lst.remove(i);
        }
    }

    private void doEnd(List<String> lst, int num) {
        if (num < 0) {
            return;
        }
        num++;
        for (int i = lst.size() - 1; i >= num; i--) {
            lst.remove(i);
        }
    }

    private void doFirst(List<String> lst, int num) {
        for (int i = lst.size() - 1; i >= num; i--) {
            lst.remove(i);
        }
    }

    private List<String> doUniq(List<String> lst) {
        List<String> res = new ArrayList<String>();
        List<String> tab = new ArrayList<String>();
        for (int i = 0; i < lst.size(); i++) {
            String a = lst.get(i);
            String b = getColText(a);
            if (tab.contains(b)) {
                continue;
            }
            tab.add(b);
            res.add(a);
        }
        return res;
    }

    private List<String> doHide(List<String> lst) {
        List<String> res = new ArrayList<String>();
        if (columnS.equals(" ")) {
            for (int i = 0; i < lst.size(); i++) {
                String a = lst.get(i);
                if (a.length() > columnB) {
                    a = a.substring(0, columnB);
                }
                res.add(a);
            }
        } else {
            for (int i = 0; i < lst.size(); i++) {
                String a = lst.get(i);
                String b = "";
                for (int o = 0; o < columnN; o++) {
                    int p = a.indexOf(columnS);
                    if (p < 0) {
                        break;
                    }
                    b += a.substring(0, p + 1);
                    a = a.substring(p + 1, a.length());
                }
                res.add(b);
            }
        }
        return res;
    }

    private List<String> doRemove(List<String> lst) {
        List<String> res = new ArrayList<String>();
        for (int i = 0; i < lst.size(); i++) {
            String s = lst.get(i);
            s = s.replaceAll(filterS, "");
            res.add(s);
        }
        return res;
    }

    private List<String> doExclude(List<String> lst) {
        List<String> res = new ArrayList<String>();
        for (int i = 0; i < lst.size(); i++) {
            String s = lst.get(i);
            boolean b = false;
            try {
                b = s.matches(filterS);
            } catch (Exception e) {
            }
            if (b) {
                continue;
            }
            res.add(s);
        }
        return res;
    }

    private List<String> doFilterList(List<String> lst) {
        if (lst == null) {
            return null;
        }
        if (lst.size() < 1) {
            return null;
        }
        switch (filterM) {
            case include:
                return doInclude(lst);
            case hinclude:
                String a = lst.remove(columnL);
                lst = doInclude(lst);
                lst.add(0, a);
                return lst;
            case exclude:
                return doExclude(lst);
            case remove:
                return doRemove(lst);
            case sort:
                findColumn(lst);
                if (columnB < 0) {
                    return bits.str2lst("no such column");
                }
                a = lst.remove(columnL);
                Collections.sort(lst, this);
                lst.add(0, a);
                return lst;
            case revsort:
                findColumn(lst);
                if (columnB < 0) {
                    return bits.str2lst("no such column");
                }
                a = lst.remove(columnL);
                Collections.sort(lst, this);
                Collections.reverse(lst);
                lst.add(0, a);
                return lst;
            case repsort:
                findColumn(lst);
                if (columnB < 0) {
                    return bits.str2lst("no such column");
                }
                doPadCol(lst);
                a = lst.remove(columnL);
                Collections.sort(lst, this);
                Collections.reverse(lst);
                lst.add(0, a);
                return lst;
            case padsort:
                findColumn(lst);
                if (columnB < 0) {
                    return bits.str2lst("no such column");
                }
                doPadCol(lst);
                a = lst.remove(columnL);
                Collections.sort(lst, this);
                lst.add(0, a);
                return lst;
            case hide:
                findColumn(lst);
                if (columnB < 0) {
                    return bits.str2lst("no such column");
                }
                return doHide(lst);
            case uniq:
                findColumn(lst);
                if (columnB < 0) {
                    return bits.str2lst("no such column");
                }
                return doUniq(lst);
            case pastebin:
                if (cfgAll.pasteBin == null) {
                    return lst;
                }
                encUrl url = new encUrl();
                a = cfgAll.hostName + "#" + filterO + "\r\n" + bits.time2str(cfgAll.timeZoneName, bits.getTime() + cfgAll.timeServerOffset, 3) + "\r\n";
                for (int i = 0; i < lst.size(); i++) {
                    a += lst.get(i) + "\r\n";
                }
                a = cfgAll.pasteBin + encUrl.percentEncode(a);
                url.fromString(a);
                clntHttp http = new clntHttp(null, cfgAll.getClntPrx(cfgAll.httpProxy), null, false);
                if (http.doConnect(url)) {
                    return lst;
                }
                http.sendLine("GET " + url.toURL(true, true, true, true) + " HTTP/1.1");
                http.sendLine("User-Agent: " + cfgInit.versionAgent);
                http.sendLine("Host: " + url.server);
                http.sendLine("Connection: Close");
                http.sendLine("");
                pipeReader rdr = new pipeReader();
                rdr.setLineMode(pipeSide.modTyp.modeCRorLF);
                pipeConnect.connect(http.pipe, rdr.getPipe(), true);
                rdr.waitFor();
                lst.addAll(rdr.getResult());
                return lst;
            case redirect:
                bits.buf2txt(true, lst, filterS);
                return lst;
            case first:
                int num = bits.str2num(filterS);
                doFirst(lst, num);
                return lst;
            case last:
                num = bits.str2num(filterS);
                doLast(lst, num);
                return lst;
            case hlast:
                num = bits.str2num(filterS);
                a = lst.remove(columnL);
                doLast(lst, num);
                lst.add(0, a);
                return lst;
            case begin:
                num = bits.lstFnd(lst, filterS);
                doBegin(lst, num);
                return lst;
            case hbegin:
                num = bits.lstFnd(lst, filterS);
                a = lst.remove(columnL);
                doBegin(lst, num);
                lst.add(0, a);
                return lst;
            case end:
                num = bits.lstFnd(lst, filterS);
                doEnd(lst, num);
                return lst;
            case count:
                int wrd = 0;
                int chr = 0;
                for (int i = 0; i < lst.size(); i++) {
                    a = lst.get(i).trim();
                    for (;;) {
                        String s = a.replaceAll("  ", " ");
                        if (s.equals(a)) {
                            break;
                        }
                        a = s;
                    }
                    int o = a.length();
                    a = a.replaceAll(" ", "");
                    int p = a.length();
                    wrd += o - p;
                    if (p > 0) {
                        wrd++;
                    }
                    chr += o;
                }
                userFormat.tableMode tabMod = pipe.settingsGet(pipeSetting.tabMod, userFormat.tableMode.normal);
                if (tabMod == userFormat.tableMode.normal) {
                    return bits.str2lst(lst.size() + " lines, " + wrd + " words, " + chr + " characters");
                }
                userFormat res = new userFormat("|", "category|value");
                res.add("lines|" + lst.size());
                res.add("words|" + wrd);
                res.add("chars|" + chr);
                return res.formatAll(tabMod);
            case summary:
                List<Long> sum = new ArrayList<Long>();
                List<Long> min = new ArrayList<Long>();
                List<Long> max = new ArrayList<Long>();
                tabMod = pipe.settingsGet(pipeSetting.tabMod, userFormat.tableMode.normal);
                for (int i = 0; i < lst.size(); i++) {
                    cmds cmd = doSummary(tabMod, lst.get(i));
                    for (int p = 0;; p++) {
                        a = cmd.word();
                        if (a.length() < 1) {
                            break;
                        }
                        if (p >= sum.size()) {
                            sum.add((long) 0);
                            min.add(Long.MAX_VALUE);
                            max.add(Long.MIN_VALUE);
                        }
                        long v = bits.str2long(a);
                        if (!a.equals("" + v)) {
                            continue;
                        }
                        sum.set(p, sum.get(p) + v);
                        if (v < min.get(p)) {
                            min.set(p, v);
                        }
                        if (v > max.get(p)) {
                            max.set(p, v);
                        }
                    }
                }
                res = new userFormat("|", "col|name|summary|average|minimum|maximum");
                long div = lst.size() - 1;
                if (div < 1) {
                    div = 1;
                }
                cmds cmd = doSummary(tabMod, lst.get(columnL));
                for (int i = 0; i < sum.size(); i++) {
                    long val = sum.get(i);
                    res.add(i + "|" + cmd.word() + "|" + val + "|" + (val / div) + "|" + min.get(i) + "|" + max.get(i));
                }
                return res.formatAll(tabMod);
            case headers:
                return userFilter.getSecList(userFilter.text2section(lst), null, null);
            case viewer:
                userEditor edtr = new userEditor(new userScreen(pipe), lst, "result", false);
                edtr.doView();
                return new ArrayList<String>();
            case xml:
                encXml xml = new encXml();
                userFilter.section2xml(xml, "/config", userFilter.text2section(lst));
                return xml.toXMLlst();
            case linenum:
                return bits.lst2lin(lst, true);
            case hacked:
                return enc7bit.toHackedLst(lst);
            case section:
                return userFilter.getSection(lst, filterS);
            case level:
                List<userFilter> sec = userFilter.text2section(lst);
                lst = new ArrayList<String>();
                for (int i = 0; i < sec.size(); i++) {
                    userFilter ntry = sec.get(i);
                    lst.add(ntry.section + "|" + ntry.command + "|");
                }
                return lst;
            case csv:
                sec = userFilter.text2section(lst);
                lst = new ArrayList<String>();
                for (int i = 0; i < sec.size(); i++) {
                    userFilter ntry = sec.get(i);
                    lst.add(ntry.section + ";" + ntry.command);
                }
                return lst;
            case html:
                sec = userFilter.text2section(lst);
                lst = new ArrayList<String>();
                for (int i = 0; i < sec.size(); i++) {
                    userFilter ntry = sec.get(i);
                    lst.add("<tr><td>" + ntry.section + "</td><td>" + ntry.command + "</td></tr>");
                }
                return lst;
            case setdel:
                sec = userFilter.text2section(lst);
                lst = new ArrayList<String>();
                for (int i = 0; i < sec.size(); i++) {
                    userFilter ntry = sec.get(i);
                    a = ntry.command.trim();
                    if (a.equals(cmds.finish)) {
                        continue;
                    }
                    if (a.equals(cmds.comment)) {
                        lst.add(cmds.comment);
                        continue;
                    }
                    String s = "set";
                    if (a.startsWith(cmds.negated + cmds.tabulator)) {
                        s = "delete";
                        a = a.substring(3, a.length());
                    }
                    lst.add(s + " " + (ntry.section + " " + a).trim());
                }
                return lst;
            case ccode:
                sec = userFilter.text2section(lst);
                lst = new ArrayList<String>();
                boolean prv = false;
                for (int i = 0; i < sec.size(); i++) {
                    userFilter ntry = sec.get(i);
                    boolean mpty = ntry.section.length() < 1;
                    if (prv && !mpty) {
                        lst.set(i - 1, lst.get(i - 1) + " {");
                    }
                    prv = mpty;
                    a = ntry.command.trim();
                    if (a.equals(cmds.finish)) {
                        lst.add("}");
                        continue;
                    }
                    if (a.equals(cmds.comment)) {
                        lst.add("");
                        continue;
                    }
                    lst.add(ntry.command);
                }
                return lst;
            default:
                return lst;
        }
    }

    private boolean doPutArr(List<String> lst, userFormat.colorMode color, boolean boxed) {
        lst = doFilterList(lst);
        for (; filterN.length() > 0;) {
            setNfilter();
            lst = doFilterList(lst);
        }
        if (lst == null) {
            pipe.linePut("");
            return true;
        }
        userFormat.applyBoxing(lst, pipe.settingsGet(pipeSetting.boxer, userFormat.boxerMode.normal), !boxed);
        final int height = pipe.settingsGet(pipeSetting.height, 25);
        int p = 2;
        if (pipe.settingsGet(pipeSetting.times, false)) {
            p++;
        }
        int o = p;
        int[] rainc = {userScreen.colBrWhite, userScreen.colBrYellow, userScreen.colBrCyan, userScreen.colBrGreen, userScreen.colBrRed, userScreen.colBrMagenta};
        for (int i = 0; i < lst.size(); i++) {
            String a = lst.get(i);
            switch (color) {
                case normal:
                case prompt:
                    pipe.linePut(a);
                    break;
                case header:
                    userScreen.sendAnsCol(pipe, pipe.settingsGet(pipeSetting.colHeader, userScreen.colBrYellow));
                    pipe.linePut(a);
                    userScreen.sendAnsCol(pipe, pipe.settingsGet(pipeSetting.colNormal, userScreen.colWhite));
                    if (i >= columnL) {
                        color = userFormat.colorMode.normal;
                    }
                    break;
                case rainbow:
                    int d = pipe.settingsGet(pipeSetting.colNormal, userScreen.colWhite);
                    int r = a.length();
                    for (int q = 0;; q++) {
                        int s = q * rainc.length;
                        if (s >= r) {
                            break;
                        }
                        int t = s + rainc.length;
                        if (t > r) {
                            t = r;
                        }
                        userScreen.sendAnsCol(pipe, userScreen.setForeground(d, rainc[(i + q) % rainc.length]));
                        pipe.strPut(a.substring(s, t));
                    }
                    userScreen.sendAnsCol(pipe, d);
                    pipe.linePut("");
                    break;
                case zeroes:
                    d = pipe.settingsGet(pipeSetting.colNormal, userScreen.colWhite);
                    r = d;
                    byte[] b = a.getBytes();
                    for (int q = 0; q < b.length; q++) {
                        int ch = b[q];
                        int s = userFormat.zeroesColor(ch, d, rainc);
                        if (s != r) {
                            userScreen.sendAnsCol(pipe, userScreen.setForeground(d, s));
                            r = s;
                        }
                        pipe.strPut("" + (char) ch);
                    }
                    userScreen.sendAnsCol(pipe, d);
                    pipe.linePut("");
                    break;
                default:
                    break;
            }
            if (height < 1) {
                continue;
            }
            o++;
            if (o < height) {
                continue;
            }
            o = p;
            switch (doMorePrompt()) {
                case 1: // normal listing
                    break;
                case 2: // next line
                    o = height - 1;
                    break;
                case 3: // end listing
                    return true;
            }
        }
        pipe.linePut("");
        return false;
    }

    /**
     * display one text to user
     *
     * @param lst string array to display
     * @return true on quit, false on continue
     */
    public boolean putStrArr(List<String> lst) {
        if (lst == null) {
            pipe.linePut("");
            return true;
        }
        userFormat.colorMode color = pipe.settingsGet(pipeSetting.colors, userFormat.colorMode.normal);
        if (color == userFormat.colorMode.header) {
            color = userFormat.colorMode.normal;
        }
        columnL = 0;
        return doPutArr(lst, color, false);
    }

    /**
     * display one text to table
     *
     * @param lst string array to display
     * @return true on quit, false on continue
     */
    public boolean putStrTab(userFormat lst) {
        if (lst == null) {
            pipe.linePut("");
            return true;
        }
        columnL = lst.headerIdx();
        userFormat.tableMode mod = pipe.settingsGet(pipeSetting.tabMod, userFormat.tableMode.normal);
        if (mod == userFormat.tableMode.fancy) {
            columnL++;
        }
        return doPutArr(lst.formatAll(mod), pipe.settingsGet(pipeSetting.colors, userFormat.colorMode.normal), mod == userFormat.tableMode.fancy);
    }

    /**
     * put current line
     *
     * @param clr clear before draw
     */
    public synchronized void putCurrLine(boolean clr) {
        final String trncd = "$";
        final boolean color = pipe.settingsGet(pipeSetting.colors, userFormat.colorMode.normal) != userFormat.colorMode.normal;
        final int width = pipe.settingsGet(pipeSetting.width, 80) - 1;
        clr |= rangeCheck();
        pipe.blockingPut(pipeSide.getEnding(pipeSide.modTyp.modeCR), 0, 1);
        String s = curr.substring(beg, curr.length());
        int crsr = pos - beg + prompt.length();
        if (beg > 0) {
            s = trncd + s;
            crsr += trncd.length();
        }
        int left = width - prompt.length();
        if (s.length() > left) {
            final int i = left - trncd.length();
            if (i < 0) {
                s = trncd;
            } else {
                s = s.substring(0, i) + trncd;
            }
        }
        s = prompt + s;
        pipe.blockingPut(pipeSide.getEnding(pipeSide.modTyp.modeCR), 0, 1);
        if (clr) {
            pipe.strPut(bits.padEnd(s, width, " "));
        } else {
            pipe.strPut(s);
        }
        pipe.blockingPut(pipeSide.getEnding(pipeSide.modTyp.modeCR), 0, 1);
        if (color) {
            userScreen.sendAnsCol(pipe, pipe.settingsGet(pipeSetting.colPrompt, userScreen.colBrGreen));
        }
        if (crsr > s.length()) {
            crsr = s.length();
        }
        pipe.strPut(s.substring(0, crsr));
        if (color) {
            userScreen.sendAnsCol(pipe, pipe.settingsGet(pipeSetting.colNormal, userScreen.colWhite));
        }
    }

    private boolean rangeCheck() {
        len = curr.length();
        int old = beg;
        final int width = pipe.settingsGet(pipeSetting.width, 80) - 1;
        final int mov = width / 10;
        if (pos < 0) {
            pos = 0;
        }
        if (pos > len) {
            pos = len;
        }
        int i = pos - width + prompt.length() + mov;
        if (beg < i) {
            beg = i + mov;
        }
        i = pos - mov;
        if (beg > i) {
            beg = i - mov;
        }
        if (beg < 0) {
            beg = 0;
        }
        return (old != beg);
    }

    private String part(int beg, int end) {
        len = curr.length();
        if (beg < 0) {
            beg = 0;
        }
        if (end > len) {
            end = len;
        }
        if (beg >= end) {
            return "";
        }
        return curr.substring(beg, end);
    }

    private boolean moreChars() {
        return pipe.ready2rx() > 0;
    }

    /**
     * flush keyboard buffer
     */
    public void keyFlush() {
        for (;;) {
            byte[] buf = new byte[1];
            if (pipe.nonBlockGet(buf, 0, buf.length) != buf.length) {
                break;
            }
        }
    }

    private void beginWriting() {
        curr = "";
        pos = 0;
        beg = 0;
        histN = -1;
    }

    private void cmdLeft(boolean bells) {
        if (pos < 1) {
            if (!bells) {
                return;
            }
            userScreen.sendBeep(pipe);
            return;
        }
        pos--;
    }

    private void cmdRight(boolean bells) {
        if (pos >= len) {
            if (!bells) {
                return;
            }
            userScreen.sendBeep(pipe);
            return;
        }
        pos++;
    }

    private void cmdHome(boolean bells) {
        if (pos < 1) {
            if (!bells) {
                return;
            }
            userScreen.sendBeep(pipe);
            return;
        }
        pos = 0;
    }

    private void cmdEnd(boolean bells) {
        if (pos >= len) {
            if (!bells) {
                return;
            }
            userScreen.sendBeep(pipe);
            return;
        }
        pos = len;
    }

    private void cmdClear() {
        clip = curr;
        curr = "";
        clear = true;
    }

    private void cmdDelChr() {
        curr = part(0, pos) + part(pos + 1, len);
        clear = true;
    }

    private void cmdInsStr(String st) {
        if (pipe.settingsGet(pipeSetting.capsLock, false)) {
            st = st.toUpperCase();
        }
        curr = part(0, pos) + st + part(pos, len);
        pos += st.length();
        curr = part(0, 65536);
    }

    private boolean isTextChar(int ch) {
        if ((ch & 0xff) != ch) {
            return true;
        }
        if (ch < 32) {
            return true;
        }
        if (ch > 127) {
            return true;
        }
        return false;
    }

    private void cmdInsChr(int ch) {
        if (isTextChar(ch)) {
            return;
        }
        cmdInsStr("" + ((char) ch));
    }

    private void cmdBackspace(boolean bells) {
        if (pos < 1) {
            if (!bells) {
                return;
            }
            userScreen.sendBeep(pipe);
            return;
        }
        curr = part(0, pos - 1) + part(pos, len);
        clear = true;
        pos--;
    }

    private void cmdTabulator(boolean bells) {
        String s = help.guessLine(curr);
        if (s == null) {
            if (!bells) {
                return;
            }
            userScreen.sendBeep(pipe);
            return;
        }
        if (s.equals(curr)) {
            userScreen.sendBeep(pipe);
            return;
        }
        curr = s;
        pos = curr.length();
        clear = true;
    }

    private void cmdErase2end(boolean bells) {
        if (pos >= len) {
            if (!bells) {
                return;
            }
            userScreen.sendBeep(pipe);
            return;
        }
        clip = part(pos, len);
        curr = part(0, pos);
        clear = true;
    }

    private void cmdErase2beg(boolean bells) {
        if (pos < 1) {
            if (!bells) {
                return;
            }
            userScreen.sendBeep(pipe);
            return;
        }
        clip = part(0, pos);
        curr = part(pos, len);
        pos = 0;
        clear = true;
    }

    private String cmdEnter() {
        putCurrLine(true);
        String a = curr;
        beginWriting();
        pipe.linePut("");
        clear = true;
        int i = a.indexOf(cmds.comment);
        if (i >= 0) {
            a = a.substring(0, i);
        }
        if (a.trim().length() < 1) {
            putCurrLine(true);
            return null;
        }
        if (!a.equals("" + histD[0])) {
            for (i = histD.length - 2; i >= 0; i--) {
                histD[i + 1] = histD[i];
            }
            histD[0] = a;
        }
        String b = help.repairLine(a);
        if (!help.endOfCmd(b)) {
            if (debugger.userReaderEvnt) {
                logger.debug("got " + prompt + b);
            }
            if (pipe.settingsGet(pipeSetting.logging, false)) {
                logger.info("command " + prompt + b + " from " + pipe.settingsGet(pipeSetting.origin, "?"));
            }
            return b;
        }
        putStrArr(help.getHelp(a, true));
        putCurrLine(true);
        return null;
    }

    private void cmdRefreshLine(boolean bells) {
        pipe.linePut("");
        putCurrLine(true);
        if (!bells) {
            return;
        }
        userScreen.sendBeep(pipe);
    }

    private void cmdHistNext() {
        if (histN < 0) {
            curr = "";
            histN = 0;
        } else {
            curr = histD[histN];
        }
        histN--;
        pos = curr.length();
        clear = true;
    }

    private void cmdHistPrev() {
        histN++;
        if (histN >= histD.length) {
            histN = histD.length - 1;
        }
        curr = histD[histN];
        if (curr.length() < 1) {
            histN--;
        }
        pos = curr.length();
        clear = true;
    }

    private void doHistFind(boolean sam, int dir, String str) {
        if (str.length() < 1) {
            if (sam) {
                histN = -1;
            }
            return;
        }
        int old = histN;
        if (sam) {
            histN -= dir;
        }
        if (histN < 0) {
            histN = -1;
        }
        for (;;) {
            histN += dir;
            if (histN < 0) {
                if (sam) {
                    histN = -1;
                } else {
                    histN = old;
                }
                return;
            }
            if (histN >= histD.length) {
                if (sam) {
                    histN = -1;
                } else {
                    histN = old;
                }
                return;
            }
            if (histD[histN].indexOf(str) >= 0) {
                return;
            }
        }
    }

    private boolean cmdHistFind() {
        final int width = pipe.settingsGet(pipeSetting.width, 80) - 10;
        final boolean bells = pipe.settingsGet(pipeSetting.termBells, false);
        String oldPrm = prompt;
        String oldCur = curr;
        String text = curr;
        histN = -1;
        doHistFind(true, +1, text);
        for (;;) {
            prompt = "find:" + text + ":";
            if (histN < 0) {
                curr = "";
            } else {
                curr = histD[histN];
            }
            pos = curr.length();
            beg = 0;
            rangeCheck();
            putCurrLine(true);
            int ch = userScreen.getKey(pipe);
            switch (ch) {
                case -1:
                    if (debugger.userReaderEvnt) {
                        logger.debug("closed");
                    }
                    return true;
                case 0x8002: // tabulator
                case 0x8004: // enter
                    prompt = oldPrm;
                    return false;
                case 0x8003: // backspace
                    if (text.length() < 1) {
                        if (!bells) {
                            break;
                        }
                        userScreen.sendBeep(pipe);
                        break;
                    }
                    text = text.substring(0, text.length() - 1);
                    doHistFind(true, +1, text);
                    break;
                case 0x800c: // up
                    doHistFind(false, +1, text);
                    break;
                case 0x800d: // down
                    doHistFind(false, -1, text);
                    break;
                case 0x8016: // f3
                    doHistFind(false, +1, text);
                    break;
                case 0x8017: // f4
                    doHistFind(false, -1, text);
                    break;
                case 0x0277: // ctrl + w
                    text = "";
                    doHistFind(true, +1, text);
                    break;
                case 0x026e: // ctrl + n
                    doHistFind(false, -1, text);
                    break;
                case 0x0270: // ctrl + p
                    doHistFind(false, +1, text);
                    break;
                case 0x0272: // ctrl + r
                    cmdRefreshLine(bells);
                    break;
                case 0x026c: // ctrl + l
                    cmdRefreshLine(bells);
                    break;
                case 0x8005: // escape
                case 0x801d: // f10
                case 0x0273: // ctrl + s
                case 0x0263: // ctrl + c
                case 0x0278: // ctrl + x
                    prompt = oldPrm;
                    curr = oldCur;
                    pos = curr.length();
                    return false;
                default:
                    if (text.length() > width) {
                        break;
                    }
                    if (isTextChar(ch)) {
                        break;
                    }
                    text += (char) ch;
                    doHistFind(true, +1, text);
                    break;
            }
        }
    }

    private void cmdSwapLetters() {
        curr = part(0, pos - 1) + part(pos, pos + 1) + part(pos - 1, pos)
                + part(pos + 1, len);
        pos++;
    }

    private void cmdSpecChr() {
        int i = userScreen.getKey(pipe);
        if ((i & 0x8000) != 0) { // special
            return;
        }
        if ((i & 0x200) != 0) { // ctrl
            i &= 0x1f;
        }
        cmdInsChr(i & 0xff);
    }

    private void cmdEraseBack(boolean bells) {
        if (pos < 1) {
            if (!bells) {
                return;
            }
            userScreen.sendBeep(pipe);
            return;
        }
        int i = cmds.wordBound(curr, pos - 1, -1);
        if (i != 0) {
            i++;
        }
        clip = part(i, pos);
        curr = part(0, i) + part(pos, len);
        pos = i;
        clear = true;
    }

    private void cmdEraseFwrd(boolean bells) {
        if (pos >= len) {
            if (!bells) {
                return;
            }
            userScreen.sendBeep(pipe);
            return;
        }
        int i = cmds.wordBound(curr, pos, +1);
        clip = part(pos, i);
        curr = part(0, pos) + part(i, len);
        clear = true;
    }

    private void cmdShowHelp() {
        putCurrLine(true);
        pipe.linePut("?");
        List<String> l = help.getHelp(curr, false);
        Collections.sort(l);
        putStrArr(l);
        putCurrLine(true);
    }

    private void cmdBackward(boolean bells) {
        if (pos < 1) {
            if (!bells) {
                return;
            }
            userScreen.sendBeep(pipe);
            return;
        }
        pos = cmds.wordBound(curr, pos - 1, -1);
        if (pos != 0) {
            pos++;
        }
    }

    private void cmdForward(boolean bells) {
        if (pos >= len) {
            if (!bells) {
                return;
            }
            userScreen.sendBeep(pipe);
            return;
        }
        pos = cmds.wordBound(curr, pos, +1);
    }

    private void cmdCapitalize() {
        int i = cmds.wordBound(curr, pos, +1);
        String s = part(pos, i);
        if (s.length() < 1) {
            return;
        }
        s = s.substring(0, 1).toUpperCase()
                + s.substring(1, s.length()).toLowerCase();
        curr = part(0, pos) + s + part(i, len);
        pos = i;
    }

    private void cmdLowercase() {
        int i = cmds.wordBound(curr, pos, +1);
        curr = part(0, pos) + part(pos, i).toLowerCase() + part(i, len);
        pos = i;
    }

    private void cmdUppercase() {
        int i = cmds.wordBound(curr, pos, +1);
        curr = part(0, pos) + part(pos, i).toUpperCase() + part(i, len);
        pos = i;
    }

    /**
     * read up one line
     *
     * @param exit exit command to return
     * @return string readed, null if error happened
     */
    public String readLine(String exit) {
        final int deactivate = pipe.settingsGet(pipeSetting.deactive, 65536);
        final boolean spacetab = pipe.settingsGet(pipeSetting.spacTab, false);
        final boolean bells = pipe.settingsGet(pipeSetting.termBells, false);
        setFilter(null);
        if (debugger.userReaderEvnt) {
            logger.debug("reading");
        }
        beginWriting();
        putCurrLine(true);
        for (;;) {
            clear = false;
            String oldS = "" + curr;
            int oldP = pos;
            for (;;) {
                len = curr.length();
                int ch = userScreen.getKey(pipe);
                if (ch == deactivate) {
                    return null;
                }
                switch (ch) {
                    case -1:
                        if (debugger.userReaderEvnt) {
                            logger.debug("closed");
                        }
                        return null;
                    case 0x8003: // backspace
                        cmdBackspace(bells);
                        break;
                    case 0x8002: // tabulator
                        cmdTabulator(bells);
                        break;
                    case 0x8004: // enter
                        if (spacetab) {
                            cmdTabulator(false);
                        }
                        String s = cmdEnter();
                        if (s == null) {
                            break;
                        }
                        return s;
                    case 0x800e: // left
                        cmdLeft(bells);
                        break;
                    case 0x800f: // right
                        cmdRight(bells);
                        break;
                    case 0x820e: // ctrl+left
                        cmdBackward(bells);
                        break;
                    case 0x820f: // ctrl+right
                        cmdForward(bells);
                        break;
                    case 0x800c: // up
                        cmdHistPrev();
                        break;
                    case 0x800d: // down
                        cmdHistNext();
                        break;
                    case 0x8008: // home
                        cmdHome(bells);
                        break;
                    case 0x8009: // end
                        cmdEnd(bells);
                        break;
                    case 0x8007: // delete
                        cmdDelChr();
                        break;
                    case 0x0261: // ctrl + a
                        cmdHome(bells);
                        break;
                    case 0x0262: // ctrl + b
                        cmdLeft(bells);
                        break;
                    case 0x0263: // ctrl + c
                        cmdClear();
                        break;
                    case 0x0264: // ctrl + d
                        cmdDelChr();
                        break;
                    case 0x0265: // ctrl + e
                        cmdEnd(bells);
                        break;
                    case 0x0266: // ctrl + f
                        cmdRight(bells);
                        break;
                    case 0x026b: // ctrl + k
                        cmdErase2end(bells);
                        break;
                    case 0x026c: // ctrl + l
                        cmdRefreshLine(bells);
                        break;
                    case 0x026e: // ctrl + n
                        cmdHistNext();
                        break;
                    case 0x0270: // ctrl + p
                        cmdHistPrev();
                        break;
                    case 0x0271: // ctrl + q
                        cmdSpecChr();
                        break;
                    case 0x0272: // ctrl + r
                        cmdRefreshLine(bells);
                        break;
                    case 0x0273: // ctrl + s
                        if (cmdHistFind()) {
                            return null;
                        }
                        clear = true;
                        break;
                    case 0x0274: // ctrl + t
                        cmdSwapLetters();
                        break;
                    case 0x0275: // ctrl + u
                        cmdErase2beg(bells);
                        break;
                    case 0x0276: // ctrl + v
                        cmdSpecChr();
                        break;
                    case 0x0277: // ctrl + w
                        cmdEraseBack(bells);
                        break;
                    case 0x0278: // ctrl + x
                        cmdErase2beg(bells);
                        break;
                    case 0x0279: // ctrl + y
                        cmdInsStr(clip);
                        break;
                    case 0x027a: // ctrl + z
                        cmdRefreshLine(false);
                        cmdClear();
                        if (exit == null) {
                            break;
                        }
                        return exit;
                    case 0x0462: // alt + b
                        cmdBackward(bells);
                        break;
                    case 0x0463: // alt + c
                        cmdCapitalize();
                        break;
                    case 0x0464: // alt + d
                        cmdEraseFwrd(bells);
                        break;
                    case 0x0466: // alt + f
                        cmdForward(bells);
                        break;
                    case 0x046c: // alt + l
                        cmdLowercase();
                        break;
                    case 0x0471: // alt + q
                        cmdSpecChr();
                        break;
                    case 0x0475: // alt + u
                        cmdUppercase();
                        break;
                    case 32: // space
                        if (spacetab && (pos >= len)) {
                            cmdTabulator(bells);
                        } else {
                            cmdInsChr(ch);
                        }
                        break;
                    case 63: // ? question mark
                        cmdShowHelp();
                        break;
                    default:
                        cmdInsChr(ch);
                        break;
                }
                if (!moreChars()) {
                    break;
                }
                rangeCheck();
            }
            if (clear) {
                putCurrLine(true);
                continue;
            }
            if ((!oldS.equals(curr)) || (pos != oldP)) {
                putCurrLine(clear);
            }
        }
    }

    /**
     * get section name
     *
     * @param lst list to read
     * @return name of section
     */
    public static String section2filter(List<String> lst) {
        for (int i = 0; i < lst.size(); i++) {
            String a = lst.get(i);
            int o = a.indexOf(cmds.comment);
            if (o >= 0) {
                a = a.substring(0, o);
            }
            if (a.startsWith(cmds.tabulator)) {
                continue;
            }
            return a;
        }
        return null;
    }

    /**
     * convert user filter to regexp
     *
     * @param flt user filter
     * @return regular expression
     */
    public static String filter2reg(String flt) {
        flt = ".*" + flt + ".*";
        flt = flt.replaceAll(" ", ".*");
        flt = flt.replaceAll("\\|", ".*|.*");
        return flt;
    }

    /**
     * set filter string
     *
     * @param cmd filter string, null if nothing
     * @return updated command
     */
    public cmds setFilter(cmds cmd) {
        if (debugger.userReaderEvnt) {
            logger.debug("set filter");
        }
        filterS = "";
        filterM = mode.raw;
        filterN = "";
        if (cmd == null) {
            return null;
        }
        String a = cmd.getRemaining();
        filterO = cmd.getOriginal();
        int i = a.indexOf(" | ");
        if (i < 0) {
            return cmd;
        }
        pipeSide pip = cmd.pipe;
        cmd = new cmds("exec", a.substring(0, i).trim());
        cmd.pipe = pip;
        filterN = a.substring(i + 3, a.length()).trim();
        setNfilter();
        return cmd;
    }

    private void setNfilter() {
        int i = filterN.indexOf(" | ");
        String a;
        if (i > 0) {
            a = filterN.substring(0, i);
            filterN = filterN.substring(i + 3, filterN.length()).trim();
        } else {
            a = filterN;
            filterN = "";
        }
        filterS = "";
        filterM = mode.raw;
        i = a.indexOf(" ");
        if (i >= 0) {
            filterS = a.substring(i, a.length()).trim();
            a = a.substring(0, i).trim();
        }
        if (a.equals("pastebin")) {
            filterM = mode.pastebin;
            return;
        }
        if (a.equals("headers")) {
            filterM = mode.headers;
            return;
        }
        if (a.equals("count")) {
            filterM = mode.count;
            return;
        }
        if (a.equals("summary")) {
            filterM = mode.summary;
            return;
        }
        if (a.equals("viewer")) {
            filterM = mode.viewer;
            return;
        }
        if (a.equals("csv")) {
            filterM = mode.csv;
            return;
        }
        if (a.equals("html")) {
            filterM = mode.html;
            return;
        }
        if (a.equals("xml")) {
            filterM = mode.xml;
            return;
        }
        if (a.equals("setdel")) {
            filterM = mode.setdel;
            return;
        }
        if (a.equals("ccode")) {
            filterM = mode.ccode;
            return;
        }
        if (a.equals("level")) {
            filterM = mode.level;
            return;
        }
        if (a.equals("linenumbers")) {
            filterM = mode.linenum;
            return;
        }
        if (a.equals("hacked")) {
            filterM = mode.hacked;
            return;
        }
        if (a.equals("include")) {
            filterS = filter2reg(filterS);
            filterM = mode.include;
            return;
        }
        if (a.equals("hinclude")) {
            filterS = filter2reg(filterS);
            filterM = mode.hinclude;
            return;
        }
        if (a.equals("exclude")) {
            filterS = filter2reg(filterS);
            filterM = mode.exclude;
            return;
        }
        if (a.equals("remove")) {
            filterM = mode.remove;
            return;
        }
        if (a.equals("begin")) {
            filterS = filter2reg(filterS);
            filterM = mode.begin;
            return;
        }
        if (a.equals("hbegin")) {
            filterS = filter2reg(filterS);
            filterM = mode.hbegin;
            return;
        }
        if (a.equals("end")) {
            filterS = filter2reg(filterS);
            filterM = mode.end;
            return;
        }
        if (a.equals("redirect")) {
            filterM = mode.redirect;
            return;
        }
        if (a.equals("sort")) {
            filterM = mode.sort;
            return;
        }
        if (a.equals("padsort")) {
            filterM = mode.padsort;
            return;
        }
        if (a.equals("revsort")) {
            filterM = mode.revsort;
            return;
        }
        if (a.equals("repsort")) {
            filterM = mode.repsort;
            return;
        }
        if (a.equals("uniq")) {
            filterM = mode.uniq;
            return;
        }
        if (a.equals("hide")) {
            filterM = mode.hide;
            return;
        }
        if (a.equals("section")) {
            filterS = filter2reg(filterS);
            filterM = mode.section;
            return;
        }
        if (a.equals("first")) {
            filterM = mode.first;
            return;
        }
        if (a.equals("last")) {
            filterM = mode.last;
            return;
        }
        if (a.equals("hlast")) {
            filterM = mode.hlast;
            return;
        }
        if (a.equals("reginc")) {
            filterM = mode.include;
            return;
        }
        if (a.equals("hreginc")) {
            filterM = mode.hinclude;
            return;
        }
        if (a.equals("regexc")) {
            filterM = mode.exclude;
            return;
        }
        if (a.equals("regbeg")) {
            filterM = mode.begin;
            return;
        }
        if (a.equals("hregbeg")) {
            filterM = mode.hbegin;
            return;
        }
        if (a.equals("regend")) {
            filterM = mode.end;
            return;
        }
        if (a.equals("regsec")) {
            filterM = mode.section;
            return;
        }
    }

}
