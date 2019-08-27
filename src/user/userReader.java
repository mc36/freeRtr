package user;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import pipe.pipeSide;
import util.bits;
import util.cmds;
import util.debugger;
import util.logger;

/**
 * reading one line from the user
 *
 * @author matecsaba
 */
public class userReader implements Comparator<String> {

    /**
     * user origin
     */
    public String from;

    /**
     * user name
     */
    public String user;

    /**
     * width of terminal screen
     */
    public int width;

    /**
     * height of terminal screen
     */
    public int height;

    /**
     * show commands
     */
    public boolean logging;

    /**
     * show timestamps
     */
    public boolean timeStamp;

    /**
     * colorize
     */
    public boolean colorize;

    /**
     * deactivation character
     */
    public int deactive = 65535;

    /**
     * table mode
     */
    public userFormat.tableMode tabMod = userFormat.tableMode.normal;

    private pipeSide pipe; // pipe to use

    private String prompt; // current prompt

    private userHelping help; // help context

    private String histD[]; // history data

    private int histN; // history number

    private String curr; // current line

    private String clip; // clipboard

    private int pos; // position of next typed character goes

    private int len; // length of current line

    private int beg; // first character displayed

    private boolean clear; // clearing of display required

    private String filterS; // filter string

    private mode filterM; // filter mode

    private int columnB; // beginning of column

    private int columnE; // ending of column

    /**
     * operation modes
     */
    public enum mode {

        /**
         * no filtering
         */
        raw,
        /**
         * begin
         */
        begin,
        /**
         * end
         */
        end,
        /**
         * include
         */
        include,
        /**
         * exclude
         */
        exclude,
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
         * count entities
         */
        count,
        /**
         * sort entities
         */
        sort,
        /**
         * unique entities
         */
        uniq,
        /**
         * redirection
         */
        redirect,
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
         * prepend line numbers
         */
        linenum,
        /**
         * specified section
         */
        section

    }

    /**
     * deactivation character
     */
    public int deactivate = 255;

    /**
     * constructs new reader for a pipeline
     *
     * @param pip pipeline to use as input
     * @param deact deactivation character
     */
    public userReader(pipeSide pip, int deact) {
        pipe = pip;
        setHistory(64);
        width = 79;
        height = 24;
        clip = "";
        filterS = "";
    }

    /**
     * set context in which input will be processed
     *
     * @param hlp helper instance
     * @param prmt promtp to show
     */
    public void setContext(userHelping hlp, String prmt) {
        help = hlp;
        prompt = prmt;
    }

    /**
     * set size of history buffer
     *
     * @param num
     */
    public void setHistory(int num) {
        String d[] = new String[num];
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
                    return 3;
            }
        }
    }

    private void findColumn(String s) {
        columnB = s.indexOf(filterS);
        columnE = columnB + filterS.length();
        if (columnB < 0) {
            return;
        }
        for (; columnE < s.length(); columnE++) {
            String a = s.substring(columnB, columnE).trim();
            if (!a.equals(filterS)) {
                break;
            }
        }
        columnE--;
    }

    public int compare(String o1, String o2) {
        return o1.substring(columnB, columnE).compareTo(o2.substring(columnB, columnE));
    }

    /**
     * filter the list
     *
     * @param lst list to filter
     * @return filtered list
     */
    public List<String> doFilterList(List<String> lst) {
        if (lst == null) {
            return null;
        }
        switch (filterM) {
            case include:
                List<String> res = new ArrayList<String>();
                for (int i = 0; i < lst.size(); i++) {
                    String s = lst.get(i);
                    boolean b = s.matches(filterS);
                    if (!b) {
                        continue;
                    }
                    res.add(s);
                }
                return res;
            case exclude:
                res = new ArrayList<String>();
                for (int i = 0; i < lst.size(); i++) {
                    String s = lst.get(i);
                    boolean b = s.matches(filterS);
                    if (b) {
                        continue;
                    }
                    res.add(s);
                }
                return res;
            case sort:
                findColumn(lst.get(0));
                if (columnB < 0) {
                    return bits.str2lst("no such column");
                }
                Collections.sort(lst, this);
                return lst;
            case uniq:
                findColumn(lst.get(0));
                if (columnB < 0) {
                    return bits.str2lst("no such column");
                }
                res = new ArrayList<String>();
                List<String> tab = new ArrayList<String>();
                for (int i = 0; i < lst.size(); i++) {
                    String a = lst.get(i);
                    if (tab.contains(a.substring(columnB, columnE))) {
                        continue;
                    }
                    tab.add(a.substring(columnB, columnE));
                    res.add(a);
                }
                return res;
            case redirect:
                bits.buf2txt(true, lst, filterS);
                return lst;
            case first:
                int num = bits.str2num(filterS);
                for (int i = lst.size() - 1; i >= num; i--) {
                    lst.remove(i);
                }
                return lst;
            case last:
                num = bits.str2num(filterS);
                for (int i = lst.size() - num; i >= 0; i--) {
                    lst.remove(i);
                }
                return lst;
            case begin:
                num = bits.lstFnd(lst, filterS);
                if (num < 0) {
                    return new ArrayList<String>();
                }
                num--;
                for (int i = num; i >= 0; i--) {
                    lst.remove(i);
                }
                return lst;
            case end:
                num = bits.lstFnd(lst, filterS);
                if (num < 0) {
                    return lst;
                }
                num++;
                for (int i = lst.size() - 1; i >= num; i--) {
                    lst.remove(i);
                }
                return lst;
            case count:
                int wrd = 0;
                int chr = 0;
                for (int i = 0; i < lst.size(); i++) {
                    String a = lst.get(i).trim();
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
                return bits.str2lst(lst.size() + " lines, " + wrd + " words, " + chr + " characters");
            case headers:
                return userFilter.getSecList(userFilter.text2section(lst), null, null);
            case viewer:
                userEditor edtr = new userEditor(new userScreen(pipe, width, height), lst, "result", false);
                edtr.doView();
                return new ArrayList<String>();
            case level:
                return userFilter.sectionDump(userFilter.text2section(lst), userFormat.tableMode.normal);
            case csv:
                return userFilter.sectionDump(userFilter.text2section(lst), userFormat.tableMode.csv);
            case html:
                return userFilter.sectionDump(userFilter.text2section(lst), userFormat.tableMode.html);
            case linenum:
                return bits.lst2lin(lst, true);
            case section:
                return userFilter.getSection(lst, filterS);
            default:
                return lst;
        }
    }

    private void doPutArr(List<String> lst, boolean need2color) {
        lst = doFilterList(lst);
        if (lst == null) {
            pipe.linePut("");
            return;
        }
        int o = 0;
        for (int i = 0; i < lst.size(); i++) {
            if ((i == 0) && colorize && need2color) {
                userScreen.sendCol(pipe, userScreen.colBrYellow);
            }
            pipe.linePut(lst.get(i));
            if ((i == 0) && colorize && need2color) {
                userScreen.sendCol(pipe, userScreen.colWhite);
            }
            o++;
            if (o < height) {
                continue;
            }
            o = 0;
            if (height < 1) {
                continue;
            }
            switch (doMorePrompt()) {
                case 1: // normal listing
                    break;
                case 2: // next line
                    o = height - 1;
                    break;
                case 3: // end listing
                    return;
            }
        }
        pipe.linePut("");
    }

    /**
     * display one text to user
     *
     * @param lst string array to display
     */
    public void putStrArr(List<String> lst) {
        doPutArr(lst, false);
    }

    /**
     * display one text to table
     *
     * @param lst string array to display
     */
    public void putStrTab(userFormat lst) {
        if (lst == null) {
            pipe.linePut("");
            return;
        }
        doPutArr(lst.formatAll(tabMod), true);
    }

    /**
     * put current line
     *
     * @param clr clear before draw
     */
    public synchronized void putCurrLine(boolean clr) {
        final String trncd = "..";
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
            s = s.substring(0, left - trncd.length()) + trncd;
        }
        s = prompt + s;
        pipe.blockingPut(pipeSide.getEnding(pipeSide.modTyp.modeCR), 0, 1);
        if (clr) {
            pipe.strPut(bits.padEnd(s, width, " "));
        } else {
            pipe.strPut(s);
        }
        pipe.blockingPut(pipeSide.getEnding(pipeSide.modTyp.modeCR), 0, 1);
        if (colorize) {
            userScreen.sendCol(pipe, userScreen.colBrGreen);
        }
        pipe.strPut(s.substring(0, crsr));
        if (colorize) {
            userScreen.sendCol(pipe, userScreen.colWhite);
        }
    }

    private boolean rangeCheck() {
        len = curr.length();
        int old = beg;
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

    private int readChar() {
        byte buf[] = new byte[1];
        int i = pipe.blockingGet(buf, 0, buf.length);
        if (i != buf.length) {
            return -1;
        }
        return buf[0] & 0xff;
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

    private void cmdLeft() {
        pos--;
    }

    private void cmdRight() {
        pos++;
    }

    private void cmdHome() {
        pos = 0;
    }

    private void cmdEnd() {
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
        curr = part(0, pos) + st + part(pos, len);
        pos += st.length();
        curr = part(0, 65536);
    }

    private void cmdInsChr(int ch) {
        if (ch < 32) {
            return;
        }
        if (ch > 127) {
            return;
        }
        cmdInsStr("" + ((char) ch));
    }

    private void cmdBackspace() {
        curr = part(0, pos - 1) + part(pos, len);
        clear = true;
        pos--;
    }

    private void cmdTabulator() {
        String s = help.guessLine(curr);
        if (s == null) {
            return;
        }
        curr = s;
        pos = curr.length();
        clear = true;
    }

    private void cmdErase2end() {
        clip = part(pos, len);
        curr = part(0, pos);
        clear = true;
    }

    private void cmdErase2beg() {
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
            if (logging) {
                logger.info("command " + prompt + b + " from " + from);
            }
            return b;
        }
        putStrArr(help.getHelp(a, true));
        putCurrLine(true);
        return null;
    }

    private void cmdRefreshLine() {
        pipe.linePut("");
        putCurrLine(true);
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

    private void cmdSwapLetters() {
        curr = part(0, pos - 1) + part(pos, pos + 1) + part(pos - 1, pos)
                + part(pos + 1, len);
        pos++;
    }

    private void cmdSpecChr() {
        int i = readChar();
        if (i < 0) {
            return;
        }
        cmdInsChr(i);
    }

    private void cmdEraseBack() {
        int i = cmds.wordBound(curr, pos - 1, -1);
        if (i != 0) {
            i++;
        }
        clip = part(i, pos);
        curr = part(0, i) + part(pos, len);
        pos = i;
        clear = true;
    }

    private void cmdEraseFwrd() {
        int i = cmds.wordBound(curr, pos, +1);
        clip = part(pos, i);
        curr = part(0, pos) + part(i, len);
        clear = true;
    }

    private void cmdShowHelp() {
        putCurrLine(true);
        pipe.linePut("");
        List<String> l = help.getHelp(curr, false);
        Collections.sort(l);
        putStrArr(l);
        putCurrLine(true);
    }

    private void cmdBackward() {
        pos = cmds.wordBound(curr, pos - 1, -1);
        if (pos != 0) {
            pos++;
        }
    }

    private void cmdForward() {
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

    private void cmdEscape() {
        String s = "";
        for (;;) {
            int ch = readChar();
            if ((ch < 32) || (ch > 127)) {
                break;
            }
            s += (char) ch;
            if (ch == 91) {
                continue;
            }
            if ((ch > 48) && (ch < 57)) {
                continue;
            }
            break;
        }
        if (debugger.userReaderEvnt) {
            logger.debug("escaped " + s);
        }
        if (s.equals("[D")) { // left
            cmdLeft();
            return;
        }
        if (s.equals("[C")) { // right
            cmdRight();
            return;
        }
        if (s.equals("[A")) { // up
            cmdHistPrev();
            return;
        }
        if (s.equals("[B")) { // down
            cmdHistNext();
            return;
        }
        if (s.equals("b")) {
            cmdBackward();
            return;
        }
        if (s.equals("c")) {
            cmdCapitalize();
            return;
        }
        if (s.equals("d")) {
            cmdEraseFwrd();
            return;
        }
        if (s.equals("f")) {
            cmdForward();
            return;
        }
        if (s.equals("l")) {
            cmdLowercase();
            return;
        }
        if (s.equals("q")) {
            cmdSpecChr();
            return;
        }
        if (s.equals("u")) {
            cmdUppercase();
            return;
        }
    }

    private String cmdOneChar(int ch) {
        len = curr.length();
        switch (ch) {
            case 1: // ctrl + a
                cmdHome();
                break;
            case 2: // ctrl + b
                cmdLeft();
                break;
            case 3: // ctrl + c
                cmdClear();
                break;
            case 4: // ctrl + d
                cmdDelChr();
                break;
            case 5: // ctrl + e
                cmdEnd();
                break;
            case 6: // ctrl + f
                cmdRight();
                break;
            case 8: // ctrl + h - backspace
                cmdBackspace();
                break;
            case 9: // ctrl + i - tabulator
                cmdTabulator();
                break;
            case 11: // ctrl + k
                cmdErase2end();
                break;
            case 12: // ctrl + l
                cmdRefreshLine();
                break;
            case 10: // ctrl + j - ctrl+enter
            case 13: // ctrl + m - enter
                String s = cmdEnter();
                if (s == null) {
                    break;
                }
                return s;
            case 14: // ctrl + n
                cmdHistNext();
                break;
            case 16: // ctrl + p
                cmdHistPrev();
                break;
            case 18: // ctrl + r
                cmdRefreshLine();
                break;
            case 20: // ctrl + t
                cmdSwapLetters();
                break;
            case 21: // ctrl + u
                cmdErase2beg();
                break;
            case 22: // ctrl + v
                cmdSpecChr();
                break;
            case 23: // ctrl + w
                cmdEraseBack();
                break;
            case 24: // ctrl + x
                cmdErase2beg();
                break;
            case 25: // ctrl + y
                cmdInsStr(clip);
                break;
            case 27:
                cmdEscape();
                break;
            case 63: // ? question mark
                cmdShowHelp();
                break;
            case 7: // ctrl + g
            case 15: // ctrl + o
            case 17: // ctrl + q
            case 19: // ctrl + s
            case 26: // ctrl + z
                break;
            case 127: // delete character
                cmdBackspace();
                break;
            default:
                cmdInsChr(ch);
                break;
        }
        return null;
    }

    /**
     * read up one line
     *
     * @param deactivate deactivation character
     * @return string readed, null if error happened
     */
    public String readLine(int deactivate) {
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
                int ch = readChar();
                if (ch == deactivate) {
                    return null;
                }
                if (ch < 0) {
                    if (debugger.userReaderEvnt) {
                        logger.debug("closed");
                    }
                    return null;
                }
                String s = cmdOneChar(ch);
                if (s != null) {
                    return s;
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
     * convert user filter to regexp
     *
     * @param flt user filter
     * @return regular expression
     */
    public static String filter2reg(String flt) {
        return (" " + flt + " ").replaceAll(" ", ".*");
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
        if (cmd == null) {
            return null;
        }
        String a = cmd.getRemaining();
        int i = a.indexOf("|");
        if (i < 0) {
            return cmd;
        }
        cmd = new cmds("exec", a.substring(0, i - 1).trim());
        a = a.substring(i + 1, a.length()).trim();
        i = a.indexOf(" ");
        if (i < 0) {
            if (a.equals("headers")) {
                filterM = mode.headers;
                return cmd;
            }
            if (a.equals("count")) {
                filterM = mode.count;
                return cmd;
            }
            if (a.equals("viewer")) {
                filterM = mode.viewer;
                return cmd;
            }
            if (a.equals("csv")) {
                filterM = mode.csv;
                return cmd;
            }
            if (a.equals("html")) {
                filterM = mode.html;
                return cmd;
            }
            if (a.equals("level")) {
                filterM = mode.level;
                return cmd;
            }
            if (a.equals("linenumbers")) {
                filterM = mode.linenum;
                return cmd;
            }
            return cmd;
        }
        filterS = a.substring(i, a.length()).trim();
        a = a.substring(0, i).trim();
        if (a.equals("include")) {
            filterS = filter2reg(filterS);
            filterM = mode.include;
            return cmd;
        }
        if (a.equals("exclude")) {
            filterS = filter2reg(filterS);
            filterM = mode.exclude;
            return cmd;
        }
        if (a.equals("begin")) {
            filterS = filter2reg(filterS);
            filterM = mode.begin;
            return cmd;
        }
        if (a.equals("end")) {
            filterS = filter2reg(filterS);
            filterM = mode.end;
            return cmd;
        }
        if (a.equals("redirect")) {
            filterM = mode.redirect;
            return cmd;
        }
        if (a.equals("sort")) {
            filterM = mode.sort;
            return cmd;
        }
        if (a.equals("uniq")) {
            filterM = mode.uniq;
            return cmd;
        }
        if (a.equals("section")) {
            filterS = filter2reg(filterS);
            filterM = mode.section;
            return cmd;
        }
        if (a.equals("first")) {
            filterM = mode.first;
            return cmd;
        }
        if (a.equals("last")) {
            filterM = mode.last;
            return cmd;
        }
        if (a.equals("reginc")) {
            filterM = mode.include;
            return cmd;
        }
        if (a.equals("regexc")) {
            filterM = mode.exclude;
            return cmd;
        }
        if (a.equals("regbeg")) {
            filterM = mode.begin;
            return cmd;
        }
        if (a.equals("regend")) {
            filterM = mode.end;
            return cmd;
        }
        if (a.equals("regsec")) {
            filterM = mode.section;
            return cmd;
        }
        return cmd;
    }

}
