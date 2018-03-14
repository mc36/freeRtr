package user;

import java.util.Comparator;
import java.util.List;
import pipe.pipeDiscard;
import pipe.pipeLine;
import pipe.pipeSide;
import tab.tabGen;
import util.bits;
import util.logger;

/**
 * tool command language script
 *
 * @author matecsaba
 */
public class userScript {

    /**
     * pipeline to use
     */
    public pipeSide pipe;

    /**
     * allow exec commands
     */
    public boolean allowExec = false;

    /**
     * allow config commands
     */
    public boolean allowConfig = false;

    /**
     * working directory
     */
    public String currDir = "";

    private userScriptList prcs;

    private userScriptList vars;

    private String cmd;

    private boolean returned = false;

    private boolean breaked = false;

    private boolean continued = false;

    private int unChr = -1;

    private final static byte[] newlines = {10, 13, 59};

    private final static byte[] whiteSpace = {0, 9, 10, 13, 32, 59, (byte) 255};

    private final static byte[] nameChars = {97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90};

    private final static byte[] numChars = {48, 49, 50, 51, 52, 53, 54, 55, 56, 57};

    private final static byte[] hexChars = {48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 97, 98, 99, 100, 101, 102, 65, 66, 67, 68, 69, 70, 120};

    private static int user2str(String s) {
        if (s.startsWith("0x")) {
            return bits.fromHex(s.substring(2, s.length()));
        }
        return bits.str2num(s);
    }

    /**
     * do command
     *
     * @param pip pipeline to use
     * @param str string to execute
     * @return computed value
     */
    public static String doCmd(pipeSide pip, String str) {
        userScript s = new userScript(pip, str);
        return s.cmdAll();
    }

    /**
     * create new instance
     *
     * @param pip pipeline to use
     * @param str string to execute
     */
    public userScript(pipeSide pip, String str) {
        cmd = str;
        pipe = pip;
        prcs = new userScriptList();
        vars = new userScriptList();
    }

    private userScript(userScript old, String str, boolean nw) {
        cmd = str;
        pipe = old.pipe;
        allowExec = old.allowExec;
        allowConfig = old.allowConfig;
        currDir = old.currDir;
        if (!nw) {
            prcs = old.prcs;
            vars = old.vars;
            return;
        }
        prcs = new userScriptList();
        vars = new userScriptList();
        prcs.prnt = old.prcs;
        vars.prnt = old.vars;
    }

    /**
     * add one line
     *
     * @param s line to add
     */
    public void addLine(String s) {
        cmd += ";" + s;
    }

    /**
     * add lines to process
     *
     * @param l lines to add
     */
    public void addLines(List<String> l) {
        for (int i = 0; i < l.size(); i++) {
            addLine(l.get(i));
        }
    }

    /**
     * check if ready to run
     *
     * @return false if no, true if yes
     */
    public boolean ready2run() {
        String old = cmd;
        int o = 0;
        for (;;) {
            if (cmd.length() < 1) {
                break;
            }
            int i = getChar();
            switch (i) {
                case 123: // {
                    o++;
                    break;
                case 125: // }
                    o--;
                    break;
            }
        }
        cmd = old;
        return o < 1;
    }

    private boolean isOnList(byte[] lst, int chr) {
        for (int i = 0; i < lst.length; i++) {
            if (lst[i] == chr) {
                return true;
            }
        }
        return false;
    }

    private int getChar() {
        if (unChr >= 0) {
            int i = unChr;
            unChr = -1;
            return i;
        }
        byte[] buf = cmd.getBytes();
        if (buf.length < 1) {
            return -1;
        }
        cmd = cmd.substring(1, cmd.length());
        int i = buf[0] & 0xff;
        if (i != 92) {
            return i;
        }
        cmd = cmd.substring(1, cmd.length());
        i = buf[1] & 0xff;
        switch (i) {
            case 97: // a - bell
                i = 7;
                break;
            case 98: // b - bell
                i = 8;
                break;
            case 99: // c - ctrl+char
                cmd = cmd.substring(1, cmd.length());
                i = buf[2] & 0x1f;
                break;
            case 101: // e - escape
                i = 27;
                break;
            case 102: // f - form feed
                i = 12;
                break;
            case 110: // n - new line
                i = 10;
                break;
            case 114: // r - carriage return
                i = 13;
                break;
            case 116: // t - horizontal tab
                i = 9;
                break;
            case 118: // v - vertical tab
                i = 11;
                break;
        }
        return i | 0x100;
    }

    private String chr2str(int i) {
        byte[] buf = new byte[1];
        buf[0] = (byte) (i & 0xff);
        return new String(buf);
    }

    private String doParse(boolean allowed, int chr) {
        if (!allowed) {
            return null;
        }
        if (chr == 36) { // $
            String s = getThese(nameChars, false);
            String a = vars.get(s);
            if (a == null) {
                return "%noVar:" + s + "%";
            }
            return a;
        }
        if (chr == 91) { // []
            String s = getUntil(new byte[]{93}, false, 91, 93);
            userScript e = new userScript(this, s, false);
            s = e.cmdAll();
            returned |= e.returned;
            continued |= e.continued;
            breaked |= e.breaked;
            return s;
        }
        return null;
    }

    private String getThese(byte[] allow, boolean parse) {
        String s = "";
        for (;;) {
            int i = getChar();
            if (i < 0) {
                return s;
            }
            String a = doParse(parse, i);
            if (a != null) {
                s += a;
                continue;
            }
            if (!isOnList(allow, i)) {
                unChr = i;
                return s;
            }
            s += chr2str(i);
        }
    }

    private String getUntil(byte[] stop, boolean parse, int incr, int decr) {
        String s = "";
        int lev = 0;
        for (;;) {
            int i = getChar();
            if (i < 0) {
                return s;
            }
            String a = doParse(parse, i);
            if (a != null) {
                s += a;
                continue;
            }
            if (lev == 0) {
                if (isOnList(stop, i)) {
                    return s;
                }
            }
            if (i == incr) {
                lev++;
            }
            if (i == decr) {
                lev--;
            }
            s += chr2str(i);
        }
    }

    private int skipSpace() {
        int i;
        for (;;) {
            i = getChar();
            if (i < 0) {
                return i;
            }
            if (!isOnList(whiteSpace, i)) {
                return i;
            }
        }
    }

    private String getWord(boolean parse) {
        int i = skipSpace();
        if (i == 34) { // ""
            return getUntil(new byte[]{34}, parse, -2, -2);
        }
        if (i == 123) { // {}
            return getUntil(new byte[]{125}, parse, 123, 125);
        }
        unChr = i;
        return getUntil(whiteSpace, parse, -2, -2);
    }

    private boolean isNext(String s) {
        String a = getWord(false);
        if (a.equals(s)) {
            return true;
        }
        cmd = a + ";" + cmd;
        return false;
    }

    private int getNum() {
        boolean neg = false;
        boolean not = false;
        boolean bnt = false;
        int i = 0;
        for (;;) {
            i = skipSpace();
            if (i == 48) {
                unChr = i;
                i = user2str(getThese(hexChars, false));
                break;
            }
            if (isOnList(numChars, i)) {
                unChr = i;
                i = user2str(getThese(numChars, false));
                break;
            }
            if (i == 43) {
                continue;
            }
            if (i == 45) {
                neg = !neg;
                continue;
            }
            if (i == 21) {
                not = !not;
                continue;
            }
            if (i == 126) {
                bnt = !bnt;
                continue;
            }
            if (i == 40) { // ()
                String s = getUntil(new byte[]{41}, true, 40, 41);
                String b = cmd;
                cmd = s;
                s = calcOne();
                cmd = b;
                i = user2str(s);
                break;
            }
            unChr = i;
            i = user2str(getWord(true));
            break;
        }
        if (neg) {
            i = -i;
        }
        if (bnt) {
            i = ~i;
        }
        if (not) {
            if (i == 0) {
                i = 1;
            } else {
                i = 0;
            }
        }
        return i;
    }

    /**
     * calculate value
     *
     * @return calculated value
     */
    public String calcOne() {
        int p = getNum();
        for (;;) {
            int i = skipSpace();
            if (i < 0) {
                break;
            }
            int o = -1;
            switch (i) {
                case 61: // ==
                    o = 61;
                    break;
                case 60: // <=
                    o = 61;
                    break;
                case 62: // <=
                    o = 61;
                    break;
                case 38: // &&
                    o = 38;
                    break;
                case 124: // ||
                    o = 124;
                    break;
            }
            if (o > 0) {
                int q = skipSpace();
                if (q == o) {
                    i = -i;
                } else {
                    unChr = q;
                }
            }
            o = getNum();
            switch (i) {
                case 38: // &
                    p &= o;
                    break;
                case -38: // &&
                    if ((p != 0) && (o != 0)) {
                        p = 1;
                    } else {
                        p = 0;
                    }
                    break;
                case 124: // |
                    p |= o;
                    break;
                case -124: // ||
                    if ((p == 0) && (o == 0)) {
                        p = 0;
                    } else {
                        p = 1;
                    }
                    break;
                case 94: // ^
                    p ^= o;
                    break;
                case 43: // +
                    p += o;
                    break;
                case 45: // -
                    p -= o;
                    break;
                case 42: // *
                    p *= o;
                    break;
                case 47: // /
                    if (o == 0) {
                        return "%div0%";
                    }
                    p /= o;
                    break;
                case 37: // %
                    if (o == 0) {
                        return "%div0%";
                    }
                    p %= o;
                    break;
                case -61: // ==
                case 61: // =
                    if (p == o) {
                        p = 1;
                    } else {
                        p = 0;
                    }
                    break;
                case 60: // <
                    if (p < o) {
                        p = 1;
                    } else {
                        p = 0;
                    }
                    break;
                case 62: // >
                    if (p > o) {
                        p = 1;
                    } else {
                        p = 0;
                    }
                    break;
                case -60: // <=
                    if (p <= o) {
                        p = 1;
                    } else {
                        p = 0;
                    }
                    break;
                case -62: // >=
                    if (p >= o) {
                        p = 1;
                    } else {
                        p = 0;
                    }
                    break;
                default:
                    return "%badExpr%";
            }
        }
        return "" + p;
    }

    private String cmdOne() {
        String a = getWord(true);
        if (a.length() < 1) {
            return "";
        }
        if (a.equals("set")) {
            String b = getWord(true);
            a = getWord(true);
            vars.set(b, a);
            return a;
        }
        if (a.equals("unset")) {
            a = getWord(true);
            vars.del(a);
            return "";
        }
        if (a.equals("global")) {
            a = getWord(true);
            vars.global(a);
            return "";
        }
        if (a.equals("incr")) {
            String b = getWord(true);
            a = vars.get(b);
            a = "" + (user2str(a) + 1);
            vars.set(b, a);
            return a;
        }
        if (a.equals("execbg")) {
            if (!allowExec) {
                return "%forbidden%";
            }
            if (!allowConfig) {
                return "%forbidden%";
            }
            pipeSide pip = pipeDiscard.needAny(null);
            pip.lineTx = pipeSide.modTyp.modeCRLF;
            pip.lineRx = pipeSide.modTyp.modeCRorLF;
            userReader rdr = new userReader(pip, 1023);
            rdr.height = 0;
            userExec exe = new userExec(pip, rdr);
            exe.privileged = allowConfig;
            pip.timeout = 60000;
            a = getWord(true);
            if (a.length() < 1) {
                return a;
            }
            a = exe.repairCommand(a);
            new userScriptExec(exe, pip, a);
            return a;
        }
        if (a.equals("exec")) {
            if (!allowExec) {
                return "%forbidden%";
            }
            a = getUntil(newlines, false, -2, -2);
            userScript e = new userScript(this, a, false);
            pipeLine pl = new pipeLine(1024 * 1024, false);
            pipeSide pip = pl.getSide();
            pip.lineTx = pipeSide.modTyp.modeCRLF;
            pip.lineRx = pipeSide.modTyp.modeCRorLF;
            userReader rdr = new userReader(pip, 1023);
            rdr.height = 0;
            userExec exe = new userExec(pip, rdr);
            exe.privileged = allowConfig;
            pip.timeout = 60000;
            for (;;) {
                a = e.getWord(true);
                if (a.length() < 1) {
                    break;
                }
                a = exe.repairCommand(a);
                exe.executeCommand(a);
            }
            pip = pl.getSide();
            pl.setClose();
            a = pip.strGet(65536);
            return a;
        }
        if (a.equals("config")) {
            if (!allowConfig) {
                return "%forbidden%";
            }
            a = getUntil(newlines, false, -2, -2);
            userScript e = new userScript(this, a, false);
            pipeLine pl = new pipeLine(32768, false);
            pipeSide pip = pl.getSide();
            pip.lineTx = pipeSide.modTyp.modeCRLF;
            pip.lineRx = pipeSide.modTyp.modeCRorLF;
            userReader rdr = new userReader(pip, 1023);
            rdr.height = 0;
            userConfig cfg = new userConfig(pip, rdr);
            pip.timeout = 60000;
            for (;;) {
                a = e.getWord(true);
                if (a.length() < 1) {
                    break;
                }
                userHelping hlp = cfg.getHelping();
                rdr.setContext(hlp, "");
                String b = hlp.repairLine(a);
                if (b.length() < 1) {
                    pip.linePut("bad: " + a);
                    continue;
                }
                cfg.executeCommand(b);
            }
            pip = pl.getSide();
            pl.setClose();
            a = pip.strGet(65536);
            return a;
        }
        if (a.equals("split")) {
            a = getWord(true);
            String[] res = a.split(getWord(true));
            a = "";
            for (int i = 0; i < res.length; i++) {
                a += " " + res[i];
            }
            if (res.length > 0) {
                a = a.substring(1, a.length());
            }
            return a;
        }
        if (a.equals("string")) {
            a = getWord(true);
            if (a.equals("bytelength")) {
                a = getWord(true);
                return "" + a.length();
            }
            if (a.equals("compare")) {
                a = getWord(true);
                String b = getWord(true);
                return "" + a.compareTo(b);
            }
            if (a.equals("equal")) {
                a = getWord(true);
                String b = getWord(true);
                if (a.equals(b)) {
                    return "1";
                } else {
                    return "0";
                }
            }
            if (a.equals("first")) {
                a = getWord(true);
                String b = getWord(true);
                return "" + b.indexOf(a);
            }
            if (a.equals("index")) {
                a = getWord(true);
                int i = user2str(getWord(true));
                if (i < 0) {
                    return "";
                }
                if (i >= a.length()) {
                    return "";
                }
                return a.substring(i, i + 1);
            }
            if (a.equals("last")) {
                a = getWord(true);
                String b = getWord(true);
                return "" + b.lastIndexOf(a);
            }
            if (a.equals("match")) {
                a = getWord(true);
                String b = getWord(true);
                if (b.matches(a)) {
                    return "1";
                } else {
                    return "0";
                }
            }
            if (a.equals("length")) {
                a = getWord(true);
                return "" + a.length();
            }
            if (a.equals("range")) {
                a = getWord(true);
                int i = user2str(getWord(true));
                int o = user2str(getWord(true)) + 1;
                if (i < 0) {
                    i = 0;
                }
                if (o > a.length()) {
                    o = a.length();
                }
                if (i >= o) {
                    return "";
                }
                return a.substring(i, o);
            }
            if (a.equals("repeat")) {
                a = getWord(true);
                int o = user2str(getWord(true));
                String s = "";
                for (int i = 0; i < o; i++) {
                    s += a;
                }
                return s;
            }
            if (a.equals("reverse")) {
                a = getWord(true);
                String s = "";
                for (int i = 0; i < a.length(); i++) {
                    s = a.substring(i, i + 1) + s;
                }
                return s;
            }
            if (a.equals("tolower")) {
                a = getWord(true);
                return a.toLowerCase();
            }
            if (a.equals("toupper")) {
                a = getWord(true);
                return a.toUpperCase();
            }
            if (a.equals("trim")) {
                a = getWord(true);
                return a.trim();
            }
            if (a.equals("wordend")) {
                a = getWord(true);
                int i = user2str(getWord(true));
                i = a.indexOf(" ", i);
                return "" + i;
            }
            if (a.equals("wordstart")) {
                a = getWord(true);
                int i = user2str(getWord(true));
                i = a.lastIndexOf(" ", i);
                return "" + i;
            }
            return "%nostr%";
        }
        if (a.equals("after")) {
            a = getWord(true);
            bits.sleep(user2str(a));
            return "";
        }
        if (a.equals("expr")) {
            a = getUntil(newlines, true, -2, -2);
            userScript e = new userScript(this, a, false);
            return e.calcOne();
        }
        if (a.equals("proc")) {
            a = getWord(true);
            String b = getWord(true);
            String c = getWord(false);
            prcs.set(a, b + " } " + c);
            return "";
        }
        if (a.equals("sleep")) {
            a = getWord(true);
            int o = bits.str2num(a);
            for (int i = 0; i < o; i++) {
                if (pipe.isClosed() != 0) {
                    break;
                }
                bits.sleep(1000);
            }
            return a;
        }
        if (a.equals("puts")) {
            a = getWord(true);
            pipe.linePut(a);
            return a;
        }
        if (a.equals("gets")) {
            a = getWord(true);
            a = pipe.lineGet(user2str(a));
            return a;
        }
        if (a.equals("flush")) {
            a = getWord(true);
            pipe.moreSkip(pipe.ready2rx());
            return a;
        }
        if (a.equals("return")) {
            returned = true;
            return getWord(true);
        }
        if (a.equals("break")) {
            breaked = true;
            return "";
        }
        if (a.equals("continue")) {
            continued = true;
            return "";
        }
        if (a.equals("eval")) {
            a = getWord(true);
            cmd = a + ";" + cmd;
            return "";
        }
        if (a.equals("if")) {
            a = getWord(true);
            String b = getWord(false);
            String c = "";
            if (isNext("else")) {
                c = getWord(false);
            }
            userScript e = new userScript(this, a, false);
            a = e.calcOne();
            if (a.equals("0")) {
                b = c;
            }
            cmd = b + ";" + cmd;
            return "";
        }
        if (a.equals("script")) {
            a = getWord(true);
            List<String> l = bits.txt2buf(currDir + a);
            if (l == null) {
                return "%nofile%";
            }
            userScript e = new userScript(this, "", false);
            e.addLines(l);
            return e.cmdAll();
        }
        if (a.equals("foreach")) {
            a = getWord(true);
            String b = getWord(true);
            String c = getWord(false);
            userScript e = new userScript(this, b, false);
            e.cmd = b;
            for (;;) {
                if (pipe.isClosed() != 0) {
                    break;
                }
                b = e.getWord(true);
                if (b.length() < 1) {
                    break;
                }
                userScript f = new userScript(this, b, true);
                f.vars.set(a, b);
                f.cmd = c;
                f.cmdAll();
                returned |= f.returned;
                if (f.breaked) {
                    break;
                }
                if (returned) {
                    break;
                }
            }
            return "";
        }
        if (a.equals("while")) {
            a = getWord(false);
            String b = getWord(false);
            for (;;) {
                if (pipe.isClosed() != 0) {
                    break;
                }
                userScript e = new userScript(this, a, false);
                String c = e.calcOne();
                if (!c.equals("1")) {
                    break;
                }
                e = new userScript(this, b, false);
                e.cmdAll();
                returned |= e.returned;
                if (e.breaked) {
                    break;
                }
                if (returned) {
                    break;
                }
            }
            return "";
        }
        if (a.equals("for")) {
            a = getWord(false);
            String b = getWord(false);
            String c = getWord(false);
            String d = getWord(false);
            userScript e = new userScript(this, a, false);
            e.cmdAll();
            for (;;) {
                if (pipe.isClosed() != 0) {
                    break;
                }
                e = new userScript(this, b, false);
                a = e.calcOne();
                if (!a.equals("1")) {
                    break;
                }
                e = new userScript(this, d, false);
                e.cmdAll();
                returned |= e.returned;
                if (e.breaked) {
                    break;
                }
                if (returned) {
                    break;
                }
                e = new userScript(this, c, false);
                e.cmdAll();
            }
            return "";
        }
        String b = prcs.get(a);
        if (b == null) {
            return "%noproc:" + a + "%";
        }
        userScript e = new userScript(this, b, true);
        for (;;) {
            b = e.getWord(false);
            if (b.equals("}")) {
                break;
            }
            if (b.length() < 1) {
                return "internalError";
            }
            a = getWord(true);
            e.vars.set(b, a);
        }
        return e.cmdAll();
    }

    /**
     * do commands
     *
     * @return calculated value
     */
    public String cmdAll() {
        String s = "";
        for (;;) {
            if (cmd.length() < 1) {
                break;
            }
            if (pipe.isClosed() != 0) {
                break;
            }
            s = cmdOne();
            if (s == null) {
                s = "";
            }
            if (breaked) {
                break;
            }
            if (continued) {
                break;
            }
            if (returned) {
                break;
            }
        }
        return s;
    }

}

class userScriptList {

    public final tabGen<userScriptEntry> lst;

    public userScriptList prnt;

    public userScriptList() {
        lst = new tabGen<userScriptEntry>();
    }

    public userScriptEntry find(userScriptEntry ntry) {
        userScriptEntry old = lst.find(ntry);
        if (old != null) {
            return old;
        }
        if (prnt == null) {
            return null;
        }
        return prnt.find(ntry);
    }

    public void set(String nam, String val) {
        userScriptEntry ntry = new userScriptEntry(nam);
        userScriptEntry old = lst.add(ntry);
        if (old != null) {
            old.value = val;
        } else {
            ntry.value = val;
        }
    }

    public void del(String nam) {
        lst.del(new userScriptEntry(nam));
    }

    public String get(String nam) {
        userScriptEntry ntry = new userScriptEntry(nam);
        ntry = lst.find(ntry);
        if (ntry == null) {
            return null;
        }
        return ntry.value;
    }

    public void global(String nam) {
        userScriptEntry ntry = new userScriptEntry(nam);
        lst.del(ntry);
        ntry = find(ntry);
        if (ntry == null) {
            return;
        }
        lst.add(ntry);
    }

}

class userScriptEntry implements Comparator<userScriptEntry> {

    public final String name;

    public String value;

    public userScriptEntry(String n) {
        name = n;
    }

    public int compare(userScriptEntry o1, userScriptEntry o2) {
        return o1.name.compareTo(o2.name);
    }

}

class userScriptExec implements Runnable {

    private final userExec exe;

    private final pipeSide pip;

    private final String cmd;

    public userScriptExec(userExec e, pipeSide p, String c) {
        exe = e;
        pip = p;
        cmd = c;
        new Thread(this).start();
    }

    public void run() {
        try {
            exe.executeCommand(cmd);
        } catch (Exception e) {
            logger.traceback(e);
        }
        pip.setClose();
    }

}
