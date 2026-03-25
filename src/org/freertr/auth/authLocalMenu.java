package org.freertr.auth;

import java.util.ArrayList;
import java.util.List;
import org.freertr.cfg.cfgInit;
import org.freertr.pipe.pipeScreen;
import org.freertr.pipe.pipeSide;
import org.freertr.user.userEditor;
import org.freertr.util.bits;

/**
 * run tui based menu
 *
 * @author matecsaba
 */
public class authLocalMenu {

    private final authLocal database;

    private final pipeScreen console;

    private boolean changed;

    private int beg;

    private int cur;

    private String flt;

    private List<authLocalEntry> buf;

    private int usr;

    private int max;

    /**
     * create instance
     *
     * @param loc database to use
     * @param pipe pipe to use
     */
    public authLocalMenu(authLocal loc, pipeSide pipe) {
        database = loc;
        console = new pipeScreen(pipe);
    }

    /**
     * do menu
     *
     * @param prv privileged
     * @return true to save config, false if no changes made
     */
    public boolean doMenu(boolean prv) {
        if (!database.menuEna) {
            return false;
        }
        if (!prv && !database.menuGst) {
            return false;
        }
        usr = 0;
        for (int i = 0; i < database.users.size(); i++) {
            authLocalEntry ent = database.users.get(i);
            String a = ent.remark + "";
            int o = a.length();
            if (o > usr) {
                usr = o;
            }
        }
        changed = false;
        console.putCls();
        doReset();
        doFilter();
        for (;;) {
            doRange();
            doDraw(false);
            if (doKey()) {
                break;
            }
        }
        doClear();
        return changed & database.menuAsv;
    }

    private boolean doKey() {
        int i = pipeScreen.getKey(console.pipe);
        switch (i) {
            case -1: // end
                return true;
            case 0x0261: // ctrl+a
                doKeyUp();
                return false;
            case 0x027a: // ctrl+z
                doKeyDn();
                return false;
            case 0x0268: // ctrl+h
                doKeyBs();
                return false;
            case 0x026c: // ctrl+l
                doDraw(true);
                return false;
            case 0x0273: // ctrl+s
                doKeyF1();
                return false;
            case 0x0271: // ctrl+q
                return true;
            case 0x0278: // ctrl+x
                return true;
            case 0x0263: // ctrl+c
                return true;
            case 0x0276: // ctrl+v
                doKeyF3();
                return false;
            case 0x0265: // ctrl+e
                doKeyF4();
                return false;
            case 0x0264: // ctrl+d
                doKeyF5();
                return false;
            case 0x026e: // ctrl+n
                doKeyF7();
                return false;
            case 0x0272: // ctrl+r
                doKeyF8();
                return false;
            case 0x0277: // ctrl+w
                doKeyClr();
                return false;
            case 0x026f: // ctrl+o
                doKeyOtp();
                return false;
            case 0x0270: // ctrl+p
                doKeyPwd();
                return false;
            case 0x0267: // ctrl+g
                doKeyGen();
                return false;
            case 0x8003: // backspace
                doKeyBs();
                return false;
            case 0x8008: // home
                doKeyHom();
                return false;
            case 0x8009: // end
                doKeyEnd();
                return false;
            case 0x800a: // pgup
                doKeyPgUp();
                return false;
            case 0x800b: // pgdn
                doKeyPgDn();
                return false;
            case 0x800c: // up
                doKeyUp();
                return false;
            case 0x800d: // down
                doKeyDn();
                return false;
            case 0x800e: // left
                doKeyLft();
                return false;
            case 0x800f: // right
                doKeyRgt();
                return false;
            case 0x8014: // f1
                doKeyF1();
                return false;
            case 0x8016: // f3
                doKeyF3();
                return false;
            case 0x8017: // f4
                doKeyF4();
                return false;
            case 0x8018: // f5
                doKeyF5();
                return false;
            case 0x801a: // f7
                doKeyF7();
                return false;
            case 0x801b: // f8
                doKeyF8();
                return false;
            case 0x801d: // f10
                return true;
            default:
                doKeyChr(i);
                return false;
        }
    }

    private void doKeyUp() {
        cur--;
    }

    private void doKeyDn() {
        cur++;
    }

    private void doKeyLft() {
        usr--;
    }

    private void doKeyRgt() {
        usr++;
    }

    private void doKeyHom() {
        cur = 0;
    }

    private void doKeyEnd() {
        cur = buf.size();
    }

    private void doKeyPgDn() {
        cur += console.sizY / 4;
    }

    private void doKeyPgUp() {
        cur -= console.sizY / 4;
    }

    private void doKeyBs() {
        int i = flt.length();
        if (i < 1) {
            return;
        }
        flt = flt.substring(0, i - 1);
        doFilter();
    }

    private void doKeyClr() {
        flt = "";
        doFilter();
    }

    private void doKeyChr(int k) {
        if (k < 0x20) {
            return;
        }
        if (k > 0x7f) {
            return;
        }
        flt += (char) k;
        doFilter();
    }

    private void doKeyF1() {
        List<String> l = new ArrayList<String>();
        l.add("f1 - help");
        l.add("f3 - view entry");
        l.add("f4 - edit entry");
        l.add("f5 - duplicate entry");
        l.add("f7 - create entry");
        l.add("f8 - remove entry");
        l.add("f10 - exit");
        l.add("type - search");
        l.add("ctrl+s - help");
        l.add("ctrl+v - view entry");
        l.add("ctrl+e - edit entry");
        l.add("ctrl+d - duplicate entry");
        l.add("ctrl+r - remove entry");
        l.add("ctrl+n - create entry");
        l.add("ctrl+o - copy otp");
        l.add("ctrl+p - copy pwd");
        l.add("ctrl+g - generate password");
        l.add("ctrl+a - move up");
        l.add("ctrl+z - move down");
        l.add("ctrl+w - erase filter");
        l.add("ctrl+l - redraw screen");
        l.add("ctrl+q - exit");
        l.add("ctrl+x - exit");
        l.add("ctrl+c - exit");
        console.helpWin(pipeScreen.colBlue, pipeScreen.colWhite, pipeScreen.colBrWhite, -1, -1, -1, -1, l);
    }

    private void doKeyF3() {
        if (cur >= buf.size()) {
            return;
        }
        authLocalEntry ent = buf.get(cur);
        List<String> l = ent.toMenu(true);
        console.helpWin(pipeScreen.colBlue, pipeScreen.colWhite, pipeScreen.colBrWhite, -1, -1, -1, -1, l);
    }

    private void doKeyF4() {
        if (!database.menuWrt) {
            return;
        }
        if (cur >= buf.size()) {
            return;
        }
        authLocalEntry ent = buf.get(cur);
        List<String> l = ent.toMenu(false);
        userEditor e = new userEditor(console, l, "entry", false);
        if (e.doEdit()) {
            return;
        }
        ent.fromMenu(l);
        changed = true;
        doFilter();
    }

    private void doKeyF5() {
        if (!database.menuWrt) {
            return;
        }
        if (cur >= buf.size()) {
            return;
        }
        authLocalEntry ent = buf.get(cur);
        List<String> l = ent.toMenu(false);
        ent = new authLocalEntry();
        ent.fromMenu(l);
        ent.description += " - dup";
        ent.username = "" + bits.getTime();
        database.users.add(ent);
        changed = true;
        doFilter();
    }

    private void doKeyF7() {
        if (!database.menuWrt) {
            return;
        }
        authLocalEntry ent = new authLocalEntry();
        ent.fromMenu(new ArrayList<String>());
        ent.description = "new";
        ent.group = "new";
        ent.username = "" + bits.getTime();
        database.users.add(ent);
        changed = true;
        doFilter();
    }

    private void doKeyF8() {
        if (!database.menuWrt) {
            return;
        }
        if (cur >= buf.size()) {
            return;
        }
        authLocalEntry ent = buf.get(cur);
        String a = console.askUser("delete entry? (y/n)", pipeScreen.colRed, pipeScreen.colWhite, pipeScreen.colBrYellow, pipeScreen.colBrWhite, -1, -1, -1, "n");
        if (!a.trim().toLowerCase().equals("y")) {
            return;
        }
        database.users.del(ent);
        changed = true;
        doFilter();
    }

    private void doKeyPwd() {
        if (cur >= buf.size()) {
            return;
        }
        authLocalEntry ent = buf.get(cur);
        pipeScreen.sendClp(console.pipe, "" + ent.password);
    }

    private void doKeyOtp() {
        if (cur >= buf.size()) {
            return;
        }
        authLocalEntry ent = buf.get(cur);
        pipeScreen.sendClp(console.pipe, "" + ent.getOtpPass(false));
    }

    private void doKeyGen() {
        int len = 16;
        boolean low = true;
        boolean upp = true;
        boolean num = true;
        boolean spc = false;
        for (;;) {
            String s = authLocal.passwdRand(len, low, upp, num, spc);
            String a = console.askUser("0=ok 1=az 2=AZ 3=09 4=@# X=len " + s, pipeScreen.colRed, pipeScreen.colWhite, pipeScreen.colBrYellow, pipeScreen.colBrWhite, -1, -1, -1, "0");
            int i = bits.str2num(a);
            switch (i) {
                case 0:
                    pipeScreen.sendClp(console.pipe, "" + s);
                    return;
                case 1:
                    low ^= true;
                    break;
                case 2:
                    upp ^= true;
                    break;
                case 3:
                    num ^= true;
                    break;
                case 4:
                    spc ^= true;
                    break;
                default:
                    len = i;
                    break;
            }
        }
    }

    private void doReset() {
        beg = 0;
        cur = 0;
        flt = "";
    }

    private void doClear() {
        console.putCls();
        console.putCur(0, 0);
        console.refresh();
    }

    private void doFilter() {
        buf = new ArrayList<authLocalEntry>();
        max = 0;
        for (int i = 0; i < database.users.size(); i++) {
            authLocalEntry ent = database.users.get(i);
            String a = "" + ent.group;
            if ((a.indexOf(flt) < 0) && (ent.description.indexOf(flt) < 0)) {
                continue;
            }
            int o = a.length();
            if (max < o) {
                max = o;
            }
            buf.add(ent);
        }
    }

    private void doRange() {
        int bs = buf.size();
        if (cur >= bs) {
            cur = bs - 1;
        }
        if (cur < 0) {
            cur = 0;
        }
        int i = cur - console.sizY + 3;
        if (beg < i) {
            beg = i;
        }
        i = bs - console.sizY + 2;
        if (beg > i) {
            beg = i;
        }
        if (beg > cur) {
            beg = cur;
        }
        if (beg < 0) {
            beg = 0;
        }
        if (beg > bs) {
            beg = bs;
        }
        if (usr < 0) {
            usr = 0;
        }
        i = console.sizX - 5;
        if (usr > i) {
            usr = i;
        }
    }

    private void doDraw(boolean clr) {
        if (clr) {
            console.putCls();
            console.refresh();
            for (int i = 0; i < console.sizY; i++) {
                putFill(i, pipeScreen.colWhite, pipeScreen.colBlack, 32);
            }
            console.refresh();
        }
        putHeader();
        putFooter();
        for (int i = 0; i < console.sizY - 2; i++) {
            putLine(i);
        }
        console.putCur(8 + flt.length(), console.sizY - 1);
        console.refresh();
    }

    private void putHeader() {
        putFill(0, pipeScreen.colGreen, pipeScreen.colWhite, 32);
        console.putStr(0, 0, pipeScreen.colGreen, pipeScreen.colBrYellow, false, cfgInit.versionName);
    }

    private void putFooter() {
        putFill(console.sizY - 1, pipeScreen.colBlue, pipeScreen.colWhite, 32);
        console.putStr(0, console.sizY - 1, pipeScreen.colBlue, pipeScreen.colBrWhite, false, (cur + 1) + "/" + buf.size());
        console.putStr(8, console.sizY - 1, pipeScreen.colBlue, pipeScreen.colBrWhite, false, flt);
        console.putStr(console.sizX - 8, console.sizY - 1, pipeScreen.colBlue, pipeScreen.colWhite, false, "f1=help");
    }

    private void putLine(int ln) {
        int lin = ln + beg;
        int bg;
        int fg;
        if (lin == cur) {
            bg = pipeScreen.colMagenta;
            fg = pipeScreen.colBrYellow;
        } else {
            bg = pipeScreen.colBlack;
            fg = pipeScreen.colWhite;
        }
        putFill(ln + 1, bg, fg, 32);
        if (lin < 0) {
            return;
        }
        if (lin >= buf.size()) {
            return;
        }
        authLocalEntry ent = buf.get(lin);
        console.putStr(0, ln + 1, bg, fg, false, "" + ent.group);
        console.putStr(max + 1, ln + 1, bg, fg, false, "" + ent.remark);
        console.putStr(usr + max + 1, ln + 1, bg, fg, false, " " + ent.description);
    }

    private void putFill(int ln, int bg, int fg, int ch) {
        for (int i = 0; i < console.sizX; i++) {
            console.putInt(i, ln, bg, fg, false, ch);
        }
    }

}
