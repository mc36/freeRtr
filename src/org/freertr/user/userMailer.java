package org.freertr.user;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import org.freertr.cfg.cfgInit;
import org.freertr.pipe.pipeProgress;
import org.freertr.util.bits;

/**
 * email reader
 *
 * @author matecsaba
 */
public class userMailer {

    private final userScreen console;

    private String path;

    private List<userMailerNtry> mails;

    private int beg;

    private int cur;

    /**
     * create mailer
     *
     * @param pip console
     * @param pt path
     */
    public userMailer(userScreen pip, String pt) {
        console = pip;
        if (!pt.endsWith("/")) {
            pt += "/";
        }
        path = pt;
    }

    /**
     * do work
     */
    public void doWork() {
        console.putCls();
        doRead();
        doClear();
        for (;;) {
            doRange();
            doDraw();
            console.refresh();
            if (doKey()) {
                break;
            }
        }
        doClear();
    }

    private void doRead() {
        console.pipe.linePut("scanning directory " + path);
        File[] fl = userFlash.dirList(path);
        mails = new ArrayList<userMailerNtry>();
        pipeProgress pr = new pipeProgress(console.pipe);
        pr.setMax(fl.length);
        for (int i = 0; i < fl.length; i++) {
            pr.setCurr(i);
            userMailerNtry msg = new userMailerNtry();
            msg.file = fl[i].getName();
            List<String> txt = bits.txt2buf(path + msg.file);
            if (txt == null) {
                continue;
            }
            msg.fromTxt(txt);
            mails.add(msg);
        }
        cur = 0;
        beg = 0;
    }

    private boolean doKey() {
        int i = userScreen.getKey(console.pipe);
        switch (i) {
            case -1: // end
                return true;
            case 0x0261: // ctrl+a
                doKeyUp();
                return false;
            case 0x027a: // ctrl+z
                doKeyDn();
                return false;
            case 0x0270: // ctrl+p
                doKeyPgUp();
                return false;
            case 0x026e: // ctrl+n
                doKeyPgDn();
                return false;
            case 0x0272: // ctrl+r
                doClear();
                doDraw();
                return false;
            case 0x026c: // ctrl+l
                doClear();
                doDraw();
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
            case 0x8004: // enter
                doKeyF3();
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
            case 0x8016: // f3
                doKeyF3();
                return false;
            case 0x801d: // f10
                return true;
            default:
                return false;
        }
    }

    private void doKeyUp() {
        cur--;
    }

    private void doKeyDn() {
        cur++;
    }

    private void doKeyPgDn() {
        cur += console.sizY / 4;
    }

    private void doKeyPgUp() {
        cur -= console.sizY / 4;
    }

    private void doKeyHom() {
        cur = 0;
        beg = 0;
    }

    private void doKeyEnd() {
        cur = console.sizY - 2;
        beg = mails.size() - cur;
    }

    private void doRange() {
        int i = console.sizY - 3;
        if (cur > i) {
            beg += cur - i;
            cur = i;
        }
        if (cur < 0) {
            beg += cur;
            cur = 0;
        }
        i = mails.size() - console.sizY + 2;
        if (i < 1) {
            i = 0;
        }
        if (beg > i) {
            beg = i;
        }
        if (beg < 0) {
            beg = 0;
        }
    }

    private void doKeyF1() {
        List<String> l = new ArrayList<String>();
        l.add("f1 - help");
        l.add("f3 - view email");
        l.add("f10 - exit");
        l.add("ctrl+s - help");
        l.add("ctrl+v - view command");
        l.add("ctrl+a - move up");
        l.add("ctrl+z - move down");
        l.add("ctrl+p - move page up");
        l.add("ctrl+n - move page down");
        l.add("ctrl+r - redraw screen");
        l.add("ctrl+l - redraw screen");
        l.add("ctrl+q - exit");
        l.add("ctrl+x - exit");
        l.add("ctrl+c - exit");
        console.helpWin(userScreen.colBlue, userScreen.colWhite, userScreen.colBrWhite, -1, -1, -1, -1, l);
    }

    private void doKeyF3() {
        int i = beg + cur;
        if (i < 0) {
            return;
        }
        if (i >= mails.size()) {
            return;
        }
        userMailerNtry m = mails.get(i);
        List<String> b = bits.txt2buf(path + m.file);
        userEditor v = new userEditor(console, b, path + m.file, false);
        v.doView();
        doClear();
    }

    private void doClear() {
        console.putCls();
        console.putCur(0, 0);
        console.refresh();
    }

    private void doDraw() {
        String a = bits.padEnd(cfgInit.versionName, console.sizX, " ").substring(0, console.sizX);
        console.putStr(0, 0, userScreen.colGreen, userScreen.colBrYellow, false, a);
        a = bits.padEnd((beg + cur + 1) + "/" + mails.size(), console.sizX, " ").substring(0, console.sizX);
        console.putStr(0, console.sizY - 1, userScreen.colBlue, userScreen.colBrCyan, false, a);
        console.putStr(15, console.sizY - 1, userScreen.colBlue, userScreen.colBrCyan, false, path);
        console.putStr(console.sizX - 8, console.sizY - 1, userScreen.colBlue, userScreen.colWhite, false, "f1=help");
        int siz = (console.sizX - 22) / 2;
        for (int o = 0; o < console.sizY - 2; o++) {
            int bg;
            int fg;
            if (o == cur) {
                bg = userScreen.colWhite;
                fg = userScreen.colBlack;
            } else {
                bg = userScreen.colBlack;
                fg = userScreen.colWhite;
            }
            console.putStr(siz, o + 1, bg, fg, false, bits.padEnd("", console.sizX, " "));
            if ((beg + o) >= mails.size()) {
                continue;
            }
            userMailerNtry msg = mails.get(beg + o);
            a = msg.date + " ";
            int i = a.indexOf(",");
            if (i >= 0) {
                a = a.substring(i + 1, a.length()).trim();
            }
            a = bits.padEnd(a, 20, " ").substring(0, 20);
            console.putStr(console.sizX - 20, o + 1, bg, fg, false, a);
            a = bits.padEnd(msg.subj + " ", siz, " ").substring(0, siz);
            console.putStr(0, o + 1, bg, fg, false, a);
            a = bits.padEnd(msg.from + " ", siz, " ").substring(0, siz);
            console.putStr(siz + 1, o + 1, bg, fg, false, a);
        }
        console.putCur(siz, cur + 1);
        console.refresh();
    }

}

class userMailerNtry {

    public String file;

    public String from;

    public String subj;

    public String date;

    public void fromTxt(List<String> t) {
        for (int o = 0; o < t.size(); o++) {
            String a = t.get(o);
            if (a.length() < 1) {
                break;
            }
            int i = a.indexOf(":");
            if (i < 0) {
                continue;
            }
            String b = a.substring(i + 1, a.length()).trim();
            a = a.substring(0, i).trim().toLowerCase();
            if (a.equals("date")) {
                date = b;
                continue;
            }
            if (a.equals("from")) {
                from = b;
                continue;
            }
            if (a.equals("subject")) {
                subj = b;
                continue;
            }
        }
    }

}
