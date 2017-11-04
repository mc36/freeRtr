package user;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import util.bits;
import util.extMrkLng;
import util.extMrkLngEntry;
import util.uniResLoc;
import util.verCore;

/**
 * web browser
 *
 * @author matecsaba
 */
public class userBrowser {

    private final String tempFile;

    private final userScreen console;

    private String url;

    private String oldurl;

    private extMrkLng xml;

    private List<String> txt;

    private List<List<Integer>> lnk;

    private int beg;

    private int curX;

    private int curY;

    /**
     * create browser
     *
     * @param pip console
     * @param u url
     */
    public userBrowser(userScreen pip, String u) {
        console = pip;
        tempFile = "web" + this.hashCode() + ".tmp";
        if (u.length() < 1) {
            url = verCore.homeUrl;
        } else {
            url = u;
        }
        oldurl = url;
    }

    /**
     * do work
     */
    public void doWork() {
        doKeyRead();
        for (;;) {
            doRange();
            doDraw();
            console.refresh();
            if (doKey()) {
                break;
            }
        }
        userFlash.delete(tempFile);
        doClear();
    }

    /**
     * clear console after
     */
    public void doClear() {
        console.refresh();
        console.fillLines(0, console.sizY, userScreen.colBlack, 32);
        console.refresh();
        console.putCls();
        console.putCur(0, 0);
        console.refresh();
    }

    private boolean doKey() {
        int i = userVM.getKey(console.pipe);
        switch (i) {
            case -1: // end
                return true;
            case 0x0261: // ctrl+a
                doKeyUp();
                return false;
            case 0x0264: // ctrl+d
                doKeyF3();
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
            case 0x0266: // ctrl+f
                doKeyF7();
                return false;
            case 0x0267: // ctrl+g
                doKeyF5();
                return false;
            case 0x0268: // ctrl+h
                doKeyLft();
                return false;
            case 0x0269: // ctrl+i
                doKeyRgt();
                return false;
            case 0x0272: // ctrl+r
                doKeyRead();
                return false;
            case 0x0273: // ctrl+s
                doKeyF2();
                return false;
            case 0x0262: // ctrl+b
                doKeyBck();
                return false;
            case 0x026c: // ctrl+l
                doClear();
                return false;
            case 0x0270: // ctrl+p
                doKeyF1();
                return false;
            case 0x0271: // ctrl+q
                return true;
            case 0x0278: // ctrl+x
                return true;
            case 0x8004: // enter
                doKeyEnter();
                return false;
            case 0x8003: // backspace
                doKeyLft();
                return false;
            case 0x8002: // tabulator
                doKeyRgt();
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
            case 0x8014: // f1
                doKeyF1();
                return false;
            case 0x8015: // f2
                doKeyF2();
                return false;
            case 0x8016: // f3
                doKeyF3();
                return false;
            case 0x8018: // f5
                doKeyF5();
                return false;
            case 0x801a: // f7
                doKeyF7();
                return false;
            case 0x801d: // f10
                return true;
        }
        return false;
    }

    private void doChg2txt() {
        console.putCur(console.sizX, console.sizY);
        console.refresh();
        console.pipe.linePut("");
    }

    private String doRead() {
        doChg2txt();
        xml = new extMrkLng();
        xml.setup2html();
        txt = new ArrayList<String>();
        lnk = new ArrayList<List<Integer>>();
        beg = 0;
        curX = 0;
        curY = 0;
        userFlash.delete(tempFile);
        if (userFlash.doReceive(console.pipe, uniResLoc.parseOne(url), new File(tempFile))) {
            return "error downloading";
        }
        console.pipe.linePut("reading");
        List<String> lst = bits.txt2buf(tempFile);
        if (lst == null) {
            return "error reading";
        }
        console.pipe.linePut("parsing");
        if (xml.fromString(lst)) {
            return "error parsing";
        }
        console.pipe.linePut("formatting");
        if (xml.formatHtml(txt, lnk, console.sizX)) {
            return "error formatting";
        }
        console.pipe.linePut("done");
        return null;
    }

    private void doRange() {
        int i = txt.size() - console.sizY + 2;
        if (i < 1) {
            i = 0;
        }
        if (beg > i) {
            beg = i;
            curX = 0;
            curY = 0;
        }
        if (beg < 0) {
            beg = 0;
            curX = 0;
            curY = 0;
        }
        if (curY >= (console.sizY - 2)) {
            curX = 0;
            curY = 0;
        }
        if (curY < 0) {
            curX = 0;
            curY = 0;
        }
        if (curX < 0) {
            curX = 0;
            curY = 0;
        }
    }

    private String getTxt(int ln) {
        if ((ln < 0) || (ln >= txt.size())) {
            return "";
        }
        return txt.get(ln);
    }

    private List<Integer> getLnk(int ln) {
        if ((ln < 0) || (ln >= lnk.size())) {
            return new ArrayList<Integer>();
        }
        return lnk.get(ln);
    }

    private void doDraw() {
        String a = bits.padEnd(url, console.sizX, " ").substring(0, console.sizX);
        console.putStr(0, 0, userScreen.colGreen, userScreen.colBrYellow, false, a);
        console.putCur(curX, curY + 1);
        for (int lin = 0; lin < (console.sizY - 2); lin++) {
            List<Integer> col = getLnk(beg + lin);
            a = getTxt(beg + lin);
            a = bits.padEnd(a, console.sizX, " ").substring(0, console.sizX);
            console.putStr(0, lin + 1, userScreen.colBlack, userScreen.colWhite, false, a);
            for (int i = 0; i < col.size(); i++) {
                if (i >= console.sizX) {
                    break;
                }
                if (col.get(i) > 0) {
                    console.putCol(i, lin + 1, userScreen.colBlack, userScreen.colBrGreen);
                }
            }
        }
        a = getLink();
        if (a == null) {
            a = "";
        }
        a = bits.padEnd(beg + "  " + a, console.sizX, " ").substring(0, console.sizX);
        console.putStr(0, console.sizY - 1, userScreen.colBlue, userScreen.colBrCyan, false, a);
        console.refresh();
    }

    private void doKeyF1() {
        List<String> l = new ArrayList<String>();
        l.add("f1 - help");
        l.add("f2 - save page");
        l.add("f3 - download link");
        l.add("f5 - enter url");
        l.add("f7 - find text");
        l.add("f10 - exit");
        l.add("bs - previous link");
        l.add("tab - next link");
        l.add("enter - follow link");
        l.add("ctrl+p - help");
        l.add("ctrl+b - previous page");
        l.add("ctrl+i - next link");
        l.add("ctrl+r - reread page");
        l.add("ctrl+l - redraw screen");
        l.add("ctrl+f - find text");
        l.add("ctrl+g - enter url");
        l.add("ctrl+a - move up");
        l.add("ctrl+z - move down");
        l.add("ctrl+w - move left");
        l.add("ctrl+e - move right");
        l.add("ctrl+s - save page");
        l.add("ctrl+d - download link");
        l.add("ctrl+q - exit");
        l.add("ctrl+x - exit");
        userScreenTest.helpWin(console, userScreen.colBlue, userScreen.colWhite, userScreen.colBrWhite, -1, -1, -1, -1, l);
    }

    private void doKeyRead() {
        String a = doRead();
        if (a == null) {
            doClear();
            return;
        }
        console.pipe.linePut(a);
        userVM.getKey(console.pipe);
        doClear();
    }

    private void doKeyUp() {
        beg--;
        curY++;
    }

    private void doKeyDn() {
        beg++;
        curY--;
    }

    private void doKeyHom() {
        beg = 0;
        curX = 0;
        curY = 0;
    }

    private void doKeyEnd() {
        beg = txt.size();
        curX = 0;
        curY = 0;
    }

    private void doKeyPgUp() {
        beg -= 5;
        curY += 5;
    }

    private void doKeyPgDn() {
        beg += 5;
        curY -= 5;
    }

    private int getEntryNum() {
        List<Integer> col = getLnk(beg + curY);
        if (curX >= col.size()) {
            return -1;
        }
        int i = col.get(curX);
        if (i == 0) {
            return -1;
        }
        return i;
    }

    private extMrkLngEntry getEntryDat() {
        int i = getEntryNum();
        if (i < 0) {
            return null;
        }
        return xml.data.get(i);
    }

    private uniResLoc doUrl(String s) {
        uniResLoc n = uniResLoc.parseOne(s);
        if (!s.startsWith("/") && (n.proto.length() > 0)) {
            return n;
        }
        uniResLoc o = uniResLoc.parseOne(url);
        if (s.startsWith("/")) {
            n = uniResLoc.parseOne("url://a.b" + s);
            o.filPath = n.filPath;
        } else {
            o.filPath = o.filPath + n.filPath;
        }
        o.filName = n.filName;
        o.filExt = n.filExt;
        o.param = n.param;
        o.normalizePath();
        return o;
    }

    private String getLink() {
        extMrkLngEntry ntry = getEntryDat();
        if (ntry == null) {
            return null;
        }
        String a = ntry.getTag().trim().toLowerCase();
        String b = null;
        if (a.equals("img")) {
            b = "|src|";
        }
        if (a.equals("frame")) {
            b = "|src|";
        }
        if (a.equals("iframe")) {
            b = "|src|";
        }
        if (a.equals("script")) {
            b = "|src|";
        }
        if (a.equals("a")) {
            b = "|href|";
        }
        if (a.equals("link")) {
            b = "|href|";
        }
        if (a.equals("object")) {
            b = "|data|classid|";
        }
        if (a.equals("applet")) {
            b = "|code|archive|";
        }
        if (a.equals("meta")) {
            b = "|content|";
        }
        if (b == null) {
            return null;
        }
        List<extMrkLngEntry> lst = extMrkLng.decodeParams(ntry.param);
        int i = extMrkLng.findParam(lst, b);
        if (i < 0) {
            return null;
        }
        ntry = lst.get(i);
        b = ntry.value;
        if (a.equals("meta")) {
            i = b.indexOf("=");
            if (i > 0) {
                b = b.substring(i + 1, b.length());
            }
        }
        return doUrl(b).toURL(true, true);
    }

    private void doKeyLft() {
        boolean nxt = false;
        List<Integer> col = getLnk(beg + curY);
        if (curX < col.size()) {
            nxt = col.get(curX) != 0;
        }
        for (;;) {
            if (curY < 0) {
                break;
            }
            col = getLnk(beg + curY);
            if (curX > col.size()) {
                curX = col.size();
            }
            curX--;
            if (curX < 0) {
                curY--;
                curX = console.sizX;
                nxt = false;
                continue;
            }
            int i = col.get(curX);
            if (i == 0) {
                nxt = false;
                continue;
            }
            if (nxt) {
                continue;
            }
            break;
        }
    }

    private void doKeyRgt() {
        boolean nxt = false;
        List<Integer> col = getLnk(beg + curY);
        if (curX < col.size()) {
            nxt = col.get(curX) != 0;
        }
        for (;;) {
            if (curY >= console.sizY) {
                break;
            }
            col = getLnk(beg + curY);
            curX++;
            if (curX >= col.size()) {
                curY++;
                curX = -1;
                nxt = false;
                continue;
            }
            int i = col.get(curX);
            if (i == 0) {
                nxt = false;
                continue;
            }
            if (nxt) {
                continue;
            }
            break;
        }
    }

    private void doKeyF2() {
        String b = userScreenTest.askUser(console, "enter name of file to save:", userScreen.colRed, userScreen.colWhite, userScreen.colBrYellow, userScreen.colBrWhite, -1, -1, -1, "");
        if (b.length() < 1) {
            return;
        }
        doChg2txt();
        userFlash f = new userFlash(console.pipe);
        f.copy(tempFile, b);
        doClear();
    }

    private void doKeyF3() {
        String s = getLink();
        if (s == null) {
            return;
        }
        String b = userScreenTest.askUser(console, "enter name of file to download:", userScreen.colRed, userScreen.colWhite, userScreen.colBrYellow, userScreen.colBrWhite, -1, -1, -1, "");
        if (b.length() < 1) {
            return;
        }
        doChg2txt();
        if (userFlash.doReceive(console.pipe, uniResLoc.parseOne(s), new File(b))) {
            console.pipe.linePut("error downloading");
            userVM.getKey(console.pipe);
        }
        doClear();
    }

    private void doKeyF7() {
        String b = userScreenTest.askUser(console, "enter text to find:", userScreen.colRed, userScreen.colWhite, userScreen.colBrYellow, userScreen.colBrWhite, -1, -1, -1, "");
        if (b.length() < 1) {
            return;
        }
        b = userReader.filter2reg(b);
        for (int i = beg; i < txt.size(); i++) {
            String res = getTxt(i);
            if (!res.matches(b)) {
                continue;
            }
            beg = i;
            curX = 0;
            curY = 0;
            return;
        }
    }

    private void doKeyF5() {
        String b = userScreenTest.askUser(console, "enter url to go to:", userScreen.colRed, userScreen.colWhite, userScreen.colBrYellow, userScreen.colBrWhite, -1, -1, -1, url);
        if (b.length() < 1) {
            return;
        }
        oldurl = url;
        url = b;
        doKeyRead();
    }

    private void doKeyBck() {
        String s = url;
        url = oldurl;
        oldurl = s;
        doKeyRead();
    }

    private void doKeyEnter() {
        String s = getLink();
        if (s != null) {
            oldurl = url;
            url = s;
            doKeyRead();
            return;
        }
        int pos = getEntryNum();
        if (pos < 0) {
            return;
        }
        extMrkLngEntry ntry = xml.data.get(pos);
        String a = ntry.getTag().trim().toLowerCase();
        if (a.equals("textarea") || a.equals("select")) {
            List<extMrkLngEntry> lst = extMrkLng.decodeParams(ntry.param);
            int i = extMrkLng.findParam(lst, "|name|");
            if (i < 0) {
                a = "";
            } else {
                a = lst.get(i).value;
            }
            String b = userScreenTest.askUser(console, "enter value of " + a + ":", userScreen.colRed, userScreen.colWhite, userScreen.colBrYellow, userScreen.colBrWhite, -1, -1, -1, ntry.value);
            if (b.length() < 1) {
                return;
            }
            ntry.value = b;
            return;
        }
        if (!(a.equals("input") || a.equals("button"))) {
            return;
        }
        List<extMrkLngEntry> lst = extMrkLng.decodeParams(ntry.param);
        int i = extMrkLng.findParam(lst, "|type|");
        if (i < 0) {
            a = "unknown";
        } else {
            a = lst.get(i).value.trim().toLowerCase();
        }
        if (!a.equals("submit")) {
            i = extMrkLng.findParam(lst, "|name|");
            if (i < 0) {
                a = "";
            } else {
                a = lst.get(i).value;
            }
            i = extMrkLng.findParam(lst, "|value|");
            String b;
            extMrkLngEntry rec;
            if (i < 0) {
                b = "";
                rec = new extMrkLngEntry("value", "", "");
                lst.add(rec);
            } else {
                rec = lst.get(i);
            }
            b = userScreenTest.askUser(console, "enter value of " + a + ":", userScreen.colRed, userScreen.colWhite, userScreen.colBrYellow, userScreen.colBrWhite, -1, -1, -1, rec.value);
            if (b.length() < 1) {
                return;
            }
            rec.value = b;
            ntry.param = extMrkLng.encodeParams(lst);
            return;
        }
        a = ntry.name;
        i = a.lastIndexOf("/form/");
        if (i < 0) {
            return;
        }
        a = a.substring(0, i + 5);
        int bg = pos;
        for (; bg > 0; bg--) {
            ntry = xml.data.get(bg);
            if (!ntry.name.startsWith(a)) {
                break;
            }
        }
        bg++;
        int ed = pos;
        for (; ed < xml.data.size(); ed++) {
            ntry = xml.data.get(ed);
            if (!ntry.name.startsWith(a)) {
                break;
            }
        }
        ntry = xml.data.get(bg);
        lst = extMrkLng.decodeParams(ntry.param);
        i = extMrkLng.findParam(lst, "|action|");
        if (i < 0) {
            a = url;
        } else {
            a = lst.get(i).value;
        }
        uniResLoc res = doUrl(a);
        res.param.clear();
        for (int ps = bg; ps < ed; ps++) {
            ntry = xml.data.get(ps);
            a = ntry.getTag().trim().toLowerCase();
            if (a.equals("textarea") || a.equals("select")) {
                lst = extMrkLng.decodeParams(ntry.param);
                i = extMrkLng.findParam(lst, "|name|");
                if (i < 0) {
                    continue;
                }
                res.addParam(lst.get(i).value, ntry.value);
                continue;
            }
            if (!(a.equals("input") || a.equals("button"))) {
                continue;
            }
            lst = extMrkLng.decodeParams(ntry.param);
            i = extMrkLng.findParam(lst, "|type|");
            if (i < 0) {
                a = "unknown";
            } else {
                a = lst.get(i).value.trim().toLowerCase();
            }
            if (a.equals("submit") && (pos != ps)) {
                continue;
            }
            i = extMrkLng.findParam(lst, "|name|");
            if (i < 0) {
                continue;
            }
            a = lst.get(i).value;
            i = extMrkLng.findParam(lst, "|value|");
            if (i < 0) {
                continue;
            }
            res.addParam(a, lst.get(i).value);
        }
        oldurl = url;
        url = res.toURL(true, true);
        doKeyRead();
    }

}
