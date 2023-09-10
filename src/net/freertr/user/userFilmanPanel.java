package net.freertr.user;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import net.freertr.util.bits;

/**
 * file manager panel
 *
 * @author matecsaba
 */
public class userFilmanPanel {

    public userScreen con;

    public int scrX;

    public int scrY;

    public int sizX;

    public int sizY;

    public int begL;

    public int curL;

    public String path;

    public List<String> fil;

    public void readUp() {
        fil = new ArrayList<String>();
        if (path.length() > 1) {
            fil.add(path + "../");
        }
        File[] res = userFlash.dirList(path);
        if (res == null) {
            return;
        }
        for (int i = 0; i < res.length; i++) {
            String a = path + res[i].getName();
            if (res[i].isDirectory()) {
                a += "/";
            }
            fil.add(a);
        }
        doRange();
    }

    public void doRange() {
        if (curL < 0) {
            curL = 0;
        }
        if (curL >= fil.size()) {
            curL = fil.size() - 1;
        }
        if (begL > curL) {
            begL = curL - 5;
        }
        if (begL <= (curL - sizY)) {
            begL = curL - sizY + 5;
        }
        if (begL > (fil.size() - sizY)) {
            begL = fil.size() - sizY;
        }
        if (begL < 0) {
            begL = 0;
        }
    }

    public String getFn() {
        if (curL >= fil.size()) {
            return null;
        }
        return fil.get(curL);
    }

    public void doEnter() {
        String a = getFn();
        begL = 0;
        curL = 0;
        if (!a.endsWith("/")) {
            return;
        }
        if (a.endsWith("/./")) {
            return;
        }
        if (!a.endsWith("/../")) {
            path = a;
            readUp();
            doRange();
            return;
        }
        a = path.substring(0, path.length() - 1);
        int i = a.lastIndexOf("/");
        if (i < 0) {
            return;
        }
        path = a.substring(0, i + 1);
        a += "/";
        readUp();
        for (i = 0; i < fil.size(); i++) {
            if (a.equals(fil.get(i))) {
                curL = i;
            }
        }
        doRange();
    }

    public void doCurs() {
        int ln = scrY + curL - begL + 1;
        con.putCols(scrX, ln, userScreen.colWhite, userScreen.colBlack, sizX);
    }

    public void doDraw() {
        String a = bits.padEnd(path, sizX, " ").substring(0, sizX);
        con.putStr(scrX, scrY, userScreen.colGreen, userScreen.colBrYellow, false, a);
        for (int i = 0; i < sizY; i++) {
            if ((begL + i) >= fil.size()) {
                a = "";
            } else {
                a = fil.get(begL + i);
            }
            if (a.startsWith(path)) {
                a = a.substring(path.length(), a.length());
            }
            a = bits.padEnd(a, sizX, " ").substring(0, sizX);
            con.putStr(scrX, scrY + i + 1, userScreen.colBlack, userScreen.colWhite, false, a);
        }
        a = bits.padEnd((curL + 1) + "/" + fil.size(), sizX, " ");
        con.putStr(scrX, scrY + sizY + 1, userScreen.colBlue, userScreen.colBrCyan, false, a);
        con.putStr(con.sizX - 8, con.sizY - 1, userScreen.colBlue, userScreen.colWhite, false, "f1=help");
    }

}
