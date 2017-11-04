package util;

import java.util.List;
import pipe.pipeSide;

/**
 * command word related functions
 *
 * @author matecsaba
 */
public class cmds {

    /**
     * comment beginning
     */
    public final static String comment = "!";

    /**
     * tabulator
     */
    public final static String tabulator = " ";

    /**
     * section end
     */
    public final static String finish = "exit";

    /**
     * negate a command
     */
    public final static String negated = "no" + tabulator;

    /**
     * pipe side to use
     */
    public pipeSide pipe;

    private String orig;

    private String curr;

    private String mode;

    /**
     * create new command parser
     *
     * @param mod current mode
     * @param str command to parse
     */
    public cmds(String mod, String str) {
        curr = str;
        orig = str;
        mode = mod;
    }

    /**
     * find word boundary
     *
     * @param str string to find in
     * @param pos position where to start
     * @param dir direction to go
     * @return new position
     */
    public static int wordBound(String str, int pos, int dir) {
        final int space = 32;
        int len = str.length();
        int cur = space;
        if (pos == len) {
            pos--;
        }
        for (;; pos += dir) {
            if (pos < 0) {
                pos = 0;
                break;
            }
            if (pos >= len) {
                pos = len;
                break;
            }
            int lst = cur;
            cur = str.codePointAt(pos);
            if (lst == space) {
                continue;
            }
            if (cur == space) {
                break;
            }
        }
        return pos;
    }

    /**
     * append line command line
     *
     * @param l list to append
     * @param no parameter to negate command
     * @param beg beginning
     * @param cmd command to append
     * @param par parameter on not negaint
     */
    public static void cfgLine(List<String> l, boolean no, String beg, String cmd, String par) {
        if (par == null) {
            par = "";
        }
        if (par.length() > 0) {
            par = " " + par;
        }
        if (no) {
            beg += "no ";
            par = "";
        }
        l.add(beg + cmd + par);
    }

    /**
     * clone bytes from current reader
     *
     * @param restart
     * @return new reader
     */
    public cmds copyBytes(boolean restart) {
        cmds c = new cmds(mode, orig);
        if (restart) {
            c.curr = orig;
        } else {
            c.curr = curr;
        }
        c.pipe = pipe;
        return c;
    }

    /**
     * get original string
     *
     * @return original string
     */
    public String getOriginal() {
        return orig;
    }

    /**
     * get remaining string
     *
     * @return remaining string
     */
    public String getRemaining() {
        return curr;
    }

    /**
     * get one word
     *
     * @param sep separator
     * @return next word
     */
    public String word(String sep) {
        int i = curr.indexOf(sep);
        String s;
        if (i < 0) {
            s = curr;
            curr = "";
        } else {
            s = curr.substring(0, i).trim();
            curr = curr.substring(i + 1, curr.length()).trim();
        }
        return s;
    }

    /**
     * get one word
     *
     * @return next word
     */
    public String word() {
        return word(" ");
    }

    /**
     * returns number of remaining bytes
     *
     * @return number of remaining bytes
     */
    public int size() {
        return curr.length();
    }

    /**
     * show one error line
     *
     * @param s text to display
     */
    public void error(String s) {
        if (pipe == null) {
            return;
        }
        pipe.linePut("% " + s);
    }

    /**
     * bad command
     */
    public void badCmd() {
        error("unknown " + mode + " command entered");
        logger.error("bad " + mode + " command '" + orig + "'");
    }

}
