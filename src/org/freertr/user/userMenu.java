package org.freertr.user;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.freertr.cfg.cfgInit;
import org.freertr.pipe.pipeScreen;
import org.freertr.pipe.pipeSetting;
import org.freertr.util.logger;

/**
 * run tui based menu
 *
 * @author matecsaba
 */
public class userMenu {

    private final String hst;

    private final int prm;

    private final userExec exe;

    private final userHelp hlp;

    private final pipeScreen console;

    private final String frc;

    private final boolean pau;

    private int beg;

    private int cur;

    private String cmd;

    private List<String> buf;

    /**
     * create new instance
     *
     * @param e exec to use
     * @param h hostname to use
     * @param p pause after
     * @param f force beginning
     */
    public userMenu(userExec e, String h, boolean p, String f) {
        hst = h;
        prm = h.length();
        console = new pipeScreen(e.pipe);
        hlp = e.getHelping();
        exe = e;
        pau = p;
        frc = "" + f;
        cmd = "" + f;
    }

    /**
     * execute commands
     */
    public void doWork() {
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
            case 0x0270: // ctrl+p
                doKeyPgUp();
                return false;
            case 0x026e: // ctrl+n
                doKeyPgDn();
                return false;
            case 0x0272: // ctrl+r
                doDraw(true);
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
            case 0x0277: // ctrl+w
                doKeyClr();
                return false;
            case 0x8003: // backspace
                doKeyBs();
                return false;
            case 0x8002: // tabulator
                doKeyTab();
                return false;
            case 0x8004: // enter
                doKeyEnter();
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
            case 0x8014: // f1
                doKeyF1();
                return false;
            case 0x801d: // f10
                return true;
            default:
                doKeyChr(i);
                return false;
        }
    }

    private void doKeyF1() {
        List<String> l = new ArrayList<String>();
        l.add("f1 - help");
        l.add("f10 - exit");
        l.add("type - input");
        l.add("tab - autocomplete");
        l.add("enter - execute");
        l.add("ctrl+s - help");
        l.add("ctrl+a - move up");
        l.add("ctrl+z - move down");
        l.add("ctrl+w - erase word");
        l.add("ctrl+p - move page up");
        l.add("ctrl+n - move page down");
        l.add("ctrl+r - redraw screen");
        l.add("ctrl+l - redraw screen");
        l.add("ctrl+q - exit");
        l.add("ctrl+x - exit");
        l.add("ctrl+c - exit");
        console.helpWin(pipeScreen.colBlue, pipeScreen.colWhite, pipeScreen.colBrWhite, -1, -1, -1, -1, l);
    }

    private void doKeyEnter() {
        if (hlp.endOfCmd(cmd)) {
            return;
        }
        doClear();
        cmd = exe.repairCommand(cmd);
        if (console.pipe.settingsGet(pipeSetting.logging, false)) {
            logger.info("command exec:" + cmd + " from " + console.pipe.settingsGet(pipeSetting.origin, "?"));
        }
        console.pipe.linePut(hst + cmd);
        exe.executeCommand(cmd);
        if (pau) {
            console.pipe.strPut("press any key");
            pipeScreen.getKey(console.pipe);
        }
        doReset();
        doFilter();
        doClear();
        doDraw(true);
    }

    private void doKeyUp() {
        cur--;
    }

    private void doKeyDn() {
        cur++;
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
        int i = cmd.length();
        if (i < 1) {
            return;
        }
        cmd = cmd.substring(0, i - 1);
        doFilter();
    }

    private void doKeyClr() {
        cmd = cmd.trim();
        int i = cmd.lastIndexOf(" ");
        if (i < 0) {
            i = 0;
        }
        cmd = cmd.substring(0, i);
        doFilter();
    }

    private void doKeyTab() {
        if (cur >= buf.size()) {
            doComp();
            return;
        }
        String s = buf.get(cur);
        int i = s.indexOf(" - ");
        if (i < 0) {
            doComp();
            return;
        }
        s = s.substring(0, i);
        s = s.trim();
        if (s.startsWith("<") || s.startsWith("[")) {
            doComp();
            return;
        }
        i = cmd.lastIndexOf(" ");
        if (i < 0) {
            i = 0;
        }
        cmd = cmd.substring(0, i) + " " + s;
        doComp();
    }

    private void doKeyChr(int k) {
        if (k < 0x20) {
            return;
        }
        if (k > 0x7f) {
            return;
        }
        cmd += (char) k;
        doFilter();
    }

    private void doComp() {
        String s = hlp.guessLine(cmd);
        if (s == null) {
            return;
        }
        cmd = s;
        beg = 0;
        cur = 0;
        doFilter();
    }

    private void doReset() {
        beg = 0;
        cur = 0;
        cmd = "" + frc;
    }

    private void doClear() {
        console.putCls();
        console.putCur(0, 0);
        console.refresh();
    }

    private void doFilter() {
        if (!cmd.startsWith(frc)) {
            cmd = "" + frc;
        }
        buf = hlp.getHelp(cmd, false);
        for (int i = 0; i < buf.size(); i++) {
            buf.set(i, buf.get(i).trim());
        }
        Collections.sort(buf);
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
    }

    private void doDraw(boolean clr) {
        if (clr) {
            console.putCls();
            console.refresh();
            for (int i = 0; i < console.sizY; i++) {
                console.fillLine(i, pipeScreen.colWhite, pipeScreen.colBlack, 32);
            }
            console.refresh();
        }
        putHeader();
        putFooter();
        for (int i = 0; i < console.sizY - 2; i++) {
            putLine(i);
        }
        console.putCur(prm + cmd.length(), console.sizY - 1);
        console.refresh();
    }

    private void putHeader() {
        console.fillLine(0, pipeScreen.colGreen, pipeScreen.colWhite, 32);
        console.putStr(0, 0, pipeScreen.colGreen, pipeScreen.colBrYellow, false, cfgInit.versionName);
    }

    private void putFooter() {
        console.fillLine(console.sizY - 1, pipeScreen.colBlue, pipeScreen.colWhite, 32);
        console.putStr(0, console.sizY - 1, pipeScreen.colBlue, pipeScreen.colBrCyan, false, hst);
        console.putStr(prm, console.sizY - 1, pipeScreen.colBlue, pipeScreen.colBrGreen, false, cmd);
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
        console.fillLine(ln + 1, bg, fg, 32);
        if (lin < 0) {
            return;
        }
        if (lin >= buf.size()) {
            return;
        }
        console.putStr(0, ln + 1, bg, fg, false, buf.get(lin));
    }

}
