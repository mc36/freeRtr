package user;

import cfg.cfgAll;
import java.util.ArrayList;
import java.util.List;
import util.bits;
import util.version;

/**
 * a simple editor
 *
 * @author matecsaba
 */
public class userEditor {

    private final userScreen console;

    private final List<String> buffer;

    private final String title;

    private final boolean clock;

    private boolean readOnly;

    private boolean insert;

    private boolean changed;

    private String lastFind;

    private int begX;

    private int begY;

    private int curX;

    private int curY;

    /**
     * create editor
     *
     * @param pip console
     * @param buf buffer
     * @param tit title
     * @param clk clock
     */
    public userEditor(userScreen pip, List<String> buf, String tit, boolean clk) {
        console = pip;
        title = tit;
        clock = clk;
        if (buf == null) {
            buffer = new ArrayList<String>();
        } else {
            buffer = buf;
        }
    }

    /**
     * edit the file
     *
     * @return false to save, true to skip
     */
    public boolean doEdit() {
        readOnly = false;
        return doWork();
    }

    /**
     * get offset
     *
     * @return value
     */
    public int getOfs() {
        return begX;
    }

    /**
     * view the file
     */
    public void doView() {
        readOnly = true;
        doWork();
    }

    /**
     * do reset to defaults
     */
    public void doReset() {
        changed = false;
        insert = true;
        begX = 0;
        begY = 0;
        curX = 0;
        curY = 0;
        lastFind = "";
    }

    /**
     * clear console after
     */
    public void doClear() {
        console.putCls();
        console.putCur(0, 0);
        console.refresh();
    }

    /**
     * do timed work
     *
     * @param tim time in milliseconds
     * @param oneKey just one key
     * @return true to exit, false to continue after redraw
     */
    public boolean doTimed(int tim, boolean oneKey) {
        readOnly = true;
        long beg = bits.getTime();
        for (;;) {
            if ((bits.getTime() - beg) > tim) {
                return false;
            }
            doRange();
            doDraw(false);
            if (console.pipe.ready2rx() < 1) {
                bits.sleep(100);
                continue;
            }
            if (doKey()) {
                return true;
            }
            doRange();
            if (oneKey) {
                return false;
            }
        }
    }

    private boolean doWork() {
        doReset();
        for (;;) {
            doRange();
            doDraw(false);
            if (doKey()) {
                break;
            }
        }
        boolean b = readOnly;
        if (changed) {
            String a = userScreenTest.askUser(console, "save changes? (y/n)", userScreen.colRed, userScreen.colWhite, userScreen.colBrYellow, userScreen.colBrWhite, -1, -1, -1, "n");
            a = a.trim().toLowerCase();
            b = !a.equals("y");
        }
        doClear();
        return b;
    }

    private boolean doKey() {
        int i = userVM.getKey(console.pipe);
        switch (i) {
            case -1: // end
                return true;
            case 0x0261: // ctrl+a
                doKeyUp();
                return false;
            case 0x027a: // ctrl+z
                doKeyDn();
                return false;
            case 0x0277: // ctrl+w
                doKeyLft();
                return false;
            case 0x0265: // ctrl+e
                doKeyRgt();
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
            case 0x0279: // ctrl+y
                doKeyDeLn();
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
            case 0x8003: // backspace
                doKeyBs();
                return false;
            case 0x8004: // enter
                doKeyEnter();
                return false;
            case 0x8006: // insert
                doKeyIns();
                return false;
            case 0x8007: // delete
                doKeyDel();
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
            case 0x801a: // f7
                doKeyF7();
                return false;
            case 0x801d: // f10
                return true;
            default:
                doKeyChr(i);
                return false;
        }
    }

    private void doRange() {
        for (;;) {
            int i = buffer.size() - 1;
            if (i < 0) {
                break;
            }
            if (buffer.get(i).length() > 0) {
                break;
            }
            buffer.remove(i);
        }
        if (curY < 0) {
            curY = 0;
        }
        if (curY > buffer.size()) {
            curY = buffer.size();
        }
        if (curX < 0) {
            curX = 0;
        }
        int i = curX - console.sizX + 1;
        if (begX < i) {
            begX = i;
        }
        if (begX > curX) {
            begX = curX;
        }
        i = curY - console.sizY + 3;
        if (begY < i) {
            begY = i;
        }
        if (begY > curY) {
            begY = curY;
        }
        if (begY < 0) {
            begY = 0;
        }
        if (begY > buffer.size()) {
            begY = buffer.size();
        }
        if (begX < 0) {
            begX = 0;
        }
        for (; buffer.size() <= curY;) {
            buffer.add("");
        }
    }

    private void doKeyChr(int k) {
        if (readOnly) {
            return;
        }
        if (k < 0x20) {
            return;
        }
        if (k > 0x7f) {
            return;
        }
        String s = buffer.get(curY);
        s = bits.padEnd(s, curX + 1, " ");
        if (insert) {
            s = s.substring(0, curX) + (char) k + s.substring(curX, s.length());
        } else {
            s = s.substring(0, curX) + (char) k + s.substring(curX + 1, s.length());
        }
        buffer.set(curY, bits.trimE(s));
        curX++;
        changed = true;
    }

    private void doKeyEnter() {
        if (!insert) {
            curX = 0;
            curY++;
            return;
        }
        if (readOnly) {
            return;
        }
        String s = buffer.get(curY);
        if (curX >= s.length()) {
            buffer.add(curY + 1, "");
            curY++;
            curX = 0;
            changed = true;
            return;
        }
        buffer.set(curY, bits.trimE(s.substring(0, curX)));
        buffer.add(curY + 1, bits.trimE(s.substring(curX, s.length())));
        curY++;
        curX = 0;
        changed = true;
    }

    private void doKeyBs() {
        if (readOnly) {
            return;
        }
        curX--;
        if (curX < 0) {
            curY--;
            if (curY < 0) {
                return;
            }
            doKeyEnd();
        }
        doRange();
        doKeyDel();
    }

    private void doKeyDeLn() {
        if (readOnly) {
            return;
        }
        buffer.remove(curY);
        changed = true;
    }

    private void doKeyDel() {
        if (readOnly) {
            return;
        }
        String s = buffer.get(curY);
        if (curX < s.length()) {
            s = bits.padEnd(s, curX + 1, " ");
            s = s.substring(0, curX) + s.substring(curX + 1, s.length());
            buffer.set(curY, bits.trimE(s));
            changed = true;
            return;
        }
        if (curY >= (buffer.size() - 1)) {
            return;
        }
        s = bits.padEnd(s, curX + 1, " ");
        s = s.substring(0, curX) + buffer.get(curY + 1);
        buffer.set(curY, bits.trimE(s));
        buffer.remove(curY + 1);
        changed = true;
    }

    private void doKeyIns() {
        insert = !insert;
    }

    private void doKeyHom() {
        curX = 0;
    }

    private void doKeyEnd() {
        curX = buffer.get(curY).length();
    }

    private void doKeyDn() {
        curY++;
        if (readOnly) {
            begY++;
        }
    }

    private void doKeyUp() {
        curY--;
        if (readOnly) {
            begY--;
        }
    }

    private void doKeyPgDn() {
        begY += 5;
        curY += 5;
    }

    private void doKeyPgUp() {
        begY -= 5;
        curY -= 5;
    }

    private void doKeyLft() {
        curX--;
        if (readOnly) {
            begX--;
        }
    }

    private void doKeyRgt() {
        curX++;
        if (readOnly) {
            begX++;
        }
    }

    private void doKeyF1() {
        List<String> l = new ArrayList<String>();
        l.add("f1 - help");
        l.add("f7 - find text");
        l.add("f10 - exit");
        l.add("ctrl+s - help");
        l.add("ctrl+a - move up");
        l.add("ctrl+z - move down");
        l.add("ctrl+w - move left");
        l.add("ctrl+e - move right");
        l.add("ctrl+p - move page up");
        l.add("ctrl+n - move page down");
        l.add("ctrl+r - redraw screen");
        l.add("ctrl+l - redraw screen");
        l.add("ctrl+y - delete line");
        l.add("ctrl+q - exit");
        l.add("ctrl+x - exit");
        userScreenTest.helpWin(console, userScreen.colBlue, userScreen.colWhite, userScreen.colBrWhite, -1, -1, -1, -1, l);
    }

    private void doKeyF7() {
        String a = userScreenTest.askUser(console, "text to find:", userScreen.colBlue, userScreen.colBrGreen, userScreen.colBrGreen, userScreen.colBrGreen, -1, -1, -1, lastFind);
        if (a.length() < 1) {
            return;
        }
        lastFind = a;
        for (int i = curY + 1; i < buffer.size(); i++) {
            int o = buffer.get(i).indexOf(a);
            if (o < 0) {
                continue;
            }
            curX = o;
            curY = i;
            return;
        }
    }

    private void doDraw(boolean clr) {
        if (clr) {
            console.refresh();
            for (int i = 0; i < console.sizY; i++) {
                putFill(i, userScreen.colWhite, userScreen.colBlack, 32);
            }
            console.refresh();
        }
        console.putCls();
        putHeader();
        putFooter();
        for (int i = 0; i < console.sizY - 2; i++) {
            putLine(i);
        }
        console.refresh();
        console.putCur(curX - begX, curY - begY + 1);
        console.refresh();
    }

    private void putHeader() {
        putFill(0, userScreen.colGreen, userScreen.colWhite, 32);
        console.putStr(0, 0, userScreen.colGreen, userScreen.colBrYellow, false, version.namVer);
        if (!clock) {
            return;
        }
        String a = bits.time2str(cfgAll.timeZoneName, bits.getTime() + cfgAll.timeServerOffset, 3);
        console.putStr(console.sizX - a.length() - 1, 0, userScreen.colGreen, userScreen.colBrYellow, false, a);
    }

    private void putFooter() {
        putFill(console.sizY - 1, userScreen.colBlue, userScreen.colWhite, 32);
        console.putStr(0, console.sizY - 1, userScreen.colBlue, userScreen.colBrWhite, false, curY + ":" + curX + " " + (insert ? "ins" : "ovr") + " (" + begY + ":" + begX + ")");
        String s = "";
        if (changed) {
            s = "* ";
        }
        console.putStr(20, console.sizY - 1, userScreen.colBlue, userScreen.colBrCyan, false, s + title);
    }

    private void putLine(int ln) {
        putFill(ln + 1, userScreen.colBlack, userScreen.colWhite, 32);
        int lin = ln + begY;
        if (lin < 0) {
            return;
        }
        if (lin >= buffer.size()) {
            return;
        }
        String s = buffer.get(lin);
        if (begX >= s.length()) {
            return;
        }
        s = s.substring(begX, s.length());
        console.putStr(0, ln + 1, userScreen.colBlack, userScreen.colWhite, false, s);
    }

    private void putFill(int ln, int bg, int fg, int ch) {
        for (int i = 0; i < console.sizX; i++) {
            console.putInt(i, ln, bg, fg, false, ch);
        }
    }

}
