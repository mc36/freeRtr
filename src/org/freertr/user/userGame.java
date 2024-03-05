package org.freertr.user;

import java.util.ArrayList;
import java.util.List;
import org.freertr.auth.authResult;
import org.freertr.cfg.cfgAll;
import org.freertr.pipe.pipeSetting;
import org.freertr.serv.servQuote;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.version;

/**
 * screen games
 *
 * @author matecsaba
 */
public class userGame {

    private final userScreen console;

    private final userReader reader;

    /**
     * screen tester
     *
     * @param conn connection to use
     * @param rdr reader to use
     */
    public userGame(userScreen conn, userReader rdr) {
        console = conn;
        if (rdr == null) {
            rdr = new userReader(conn.pipe, null);
        }
        reader = rdr;
    }

    /**
     * start screen
     */
    public void doStart() {
        console.putCls();
    }

    /**
     * finish screen
     */
    public void doFinish() {
        console.putCls();
        console.refresh();
    }

    /**
     * send a broadcast message
     *
     * @param cmd command parser
     * @return messages sent
     */
    public String doSend(cmds cmd) {
        List<String> txt = new ArrayList<String>();
        String a = cmd.getRemaining().trim();
        if (a.length() > 0) {
            txt.add(a);
        } else {
            reader.keyFlush();
            doStart();
            userEditor e = new userEditor(console, txt, "message", false);
            boolean r = e.doEdit();
            doFinish();
            reader.keyFlush();
            if (r) {
                return "send cancelled";
            }
        }
        a = cmd.pipe.settingsGet(pipeSetting.authed, new authResult()).user + " from " + cmd.pipe.settingsGet(pipeSetting.origin, "?");
        return "sent to " + userLine.sendBcastMsg(a, txt) + " terminals";
    }

    private void colorDrawer(int[] god, List<String> sec) {
        int gods = god.length;
        console.putCls();
        for (int o = 0; o < sec.size(); o++) {
            String s = sec.get(o);
            byte[] b = s.getBytes();
            for (int i = 0; i < b.length; i++) {
                int ch = b[i];
                int cl = userScreen.colBrGreen;
                char chr = (char) ch;
                switch (chr) {
                    case 'o':
                    case '0':
                    case '@':
                    case 'O':
                    case '3':
                        cl = god[bits.random(0, gods)];
                        break;
                    default:
                        break;
                }
                console.putInt(i, o, false, cl, ch);
            }
        }
        console.refresh();
        for (;;) {
            if (console.keyPress()) {
                break;
            }
            bits.sleep(1000);
        }
    }

    /**
     * palette test
     */
    public void doPalette() {
        console.putCls();
        for (int i = 0; i < 16; i++) {
            int o = 15 - i;
            String a = bits.padEnd("  bg=" + o, 10, " ");
            String b = bits.padEnd("  fg=" + i, 10, " ");
            console.putStr(10, i + 1, o, i, false, a + b);
            console.putStr(40, i + 1, 0, i, false, b);
            console.putStr(60, i + 1, o, 0, false, a);
        }
        console.putCur(0, 0);
        console.refresh();
        for (;;) {
            if (console.keyPress()) {
                break;
            }
            bits.sleep(1000);
        }
    }

    /**
     * ascii table
     */
    public void doAscTab() {
        console.putCls();
        for (int o = 0; o < 16; o++) {
            console.putStr(7, 2 + o, userScreen.colBlack, userScreen.colGreen, true, "" + o);
            console.putStr(10 + (o * 3), 1, userScreen.colBlack, userScreen.colGreen, true, "" + o);
            for (int i = 0; i < 16; i++) {
                console.putInt(10 + (i * 3), 2 + o, userScreen.colBlack, userScreen.colWhite, true, (o * 16) + i);
            }
        }
        console.putCur(0, 0);
        console.refresh();
        for (;;) {
            if (console.keyPress()) {
                break;
            }
            bits.sleep(1000);
        }
    }

    /**
     * keyboard codes
     */
    public void doKeys() {
        console.putCls();
        console.putCur(0, 0);
        console.refresh();
        for (;;) {
            int i = userScreen.getKey(console.pipe);
            console.putStr(console.sizX / 2, console.sizY / 2, userScreen.colBlack, userScreen.colWhite, false, bits.toHexW(i) + " " + (i & 0xff) + "       ");
            console.putCur(0, 0);
            console.refresh();
            switch (i) {
                case -1: // end
                    return;
                case 0x0271: // ctrl+q
                    return;
                case 0x0278: // ctrl+x
                    return;
                case 0x801d: // f10
                    return;
                default:
                    break;
            }
        }
    }

    /**
     * flying text
     *
     * @param s text to use
     */
    public void doText(List<String> s) {
        int maxY = console.sizY - s.size();
        if (maxY < 1) {
            return;
        }
        int maxX = 0;
        for (int i = 0; i < s.size(); i++) {
            int o = s.get(i).length();
            if (maxX < o) {
                maxX = o;
            }
        }
        maxX = console.sizX - maxX;
        if (maxX < 1) {
            return;
        }
        for (;;) {
            if (console.keyPress()) {
                break;
            }
            console.putCls();
            console.putMaps(bits.random(0, maxX), bits.random(0, maxY), userScreen.colBlack, bits.random(1, 15), false, s);
            console.refresh();
            bits.sleep(5000);
        }
    }

    /**
     * flying clock
     *
     * @param font font to use
     */
    public void doClock(byte[][][] font) {
        int maxX = console.sizX - (font[0][0].length * 5);
        int maxY = console.sizY - font[0].length;
        for (;;) {
            if (console.keyPress()) {
                break;
            }
            String s = bits.time2str(cfgAll.timeZoneName, bits.getTime() + cfgAll.timeServerOffset, 2);
            s = s.substring(0, 5);
            console.putCls();
            console.putMaps(bits.random(0, maxX), bits.random(0, maxY), userScreen.colBlack, bits.random(1, 15), false, userScreen.fontText(s, " ", userFonts.fontFiller, font));
            console.refresh();
            bits.sleep(5000);
        }
    }

    private byte[] getMatrixStr() {
        byte[] res = new byte[bits.random(4, console.curY * 3)];
        for (int i = 0; i < res.length; i++) {
            res[i] = (byte) bits.random(32, 127);
        }
        return res;
    }

    private boolean doMatrix(int x, int pos, byte[] str) {
        int len = str.length;
        for (int o = 0; o < console.sizY; o++) {
            int i = o + pos;
            if (i < 0) {
                continue;
            }
            if (i >= len) {
                continue;
            }
            int c = userScreen.colGreen;
            if (i >= (len - 2)) {
                c = userScreen.colBrGreen;
            }
            if (i >= (len - 1)) {
                c = userScreen.colBrWhite;
            }
            console.putInt(x, o, userScreen.colBlack, c, false, str[i]);
        }
        return pos > -len;
    }

    /**
     * falling columns
     */
    public void doMatrix() {
        int[] poss = new int[console.sizX / 2];
        byte[][] strs = new byte[poss.length][];
        for (int i = 0; i < strs.length; i++) {
            strs[i] = getMatrixStr();
            int len = strs[i].length;
            poss[i] = bits.random(-len, +len);
        }
        for (;;) {
            if (console.keyPress()) {
                break;
            }
            console.putCls();
            for (int i = 0; i < poss.length; i++) {
                poss[i]--;
                if (doMatrix(i * 2, poss[i], strs[i])) {
                    continue;
                }
                strs[i] = getMatrixStr();
                int len = strs[i].length;
                poss[i] = bits.random(-len, +len);
            }
            console.refresh();
            bits.sleep(500);
        }
    }

    /**
     * moving snake
     */
    public void doSnake() {
        final int[] chars = {48, 48, 48, 48, 79, 79, 79, 79, 111, 111, 111, 111, 99, 99, 99, 99};
        int[] posX = new int[chars.length];
        int[] posY = new int[chars.length];
        int movX = -1;
        int movY = -1;
        int movC = 1;
        for (;;) {
            if (console.keyPress()) {
                break;
            }
            for (int i = chars.length - 1; i > 0; i--) {
                posX[i] = posX[i - 1];
                posY[i] = posY[i - 1];
            }
            posX[0] += movX;
            posY[0] += movY;
            if ((posX[0] / 10) >= console.sizX) {
                movX = -bits.random(1, 10);
                movC = bits.random(1, 16);
            }
            if ((posY[0] / 10) >= console.sizY) {
                movY = -bits.random(1, 10);
                movC = bits.random(1, 16);
            }
            if (posX[0] <= 0) {
                movX = bits.random(1, 10);
                movC = bits.random(1, 16);
            }
            if (posY[0] <= 0) {
                movY = bits.random(1, 10);
                movC = bits.random(1, 16);
            }
            console.putCls();
            for (int i = 0; i < chars.length; i++) {
                int x = posX[i] / 10;
                int y = posY[i] / 10;
                int c = chars[i];
                console.putInt(x + 0, y, 0, movC, false, c);
                console.putInt(x + 1, y, 0, movC, false, c);
            }
            console.refresh();
            bits.sleep(500);
        }
    }

    /**
     * burning fire
     */
    public void doFire() {
        int[][] buf = new int[console.sizY + 10][console.sizX + 10];
        for (;;) {
            if (console.keyPress()) {
                break;
            }
            int[][] old = buf;
            buf = new int[console.sizY + 10][console.sizX + 10];
            for (int y = 0; y < console.sizY; y++) {
                for (int x = 0; x < console.sizX; x++) {
                    int i = old[y + 4][x + 4] + old[y + 4][x + 5] + old[y + 4][x + 3] + old[y + 5][x + 4];
                    buf[y + 3][x + 4] = (i * 4) / 18;
                }
            }
            for (int i = 0; i < console.sizX; i++) {
                buf[console.sizY + 3][i + 4] = bits.random(0, 2) * 82;
            }
            console.putCls();
            for (int y = 0; y < console.sizY; y++) {
                for (int x = 0; x < console.sizX; x++) {
                    int i = buf[y + 4][x + 4];
                    int o = userScreen.colBlack;
                    if (i > 3) {
                        o = userScreen.colBlue;
                    }
                    if (i > 11) {
                        o = userScreen.colRed;
                    }
                    if (i > 24) {
                        o = userScreen.colBrRed;
                    }
                    if (i > 45) {
                        o = userScreen.colBrYellow;
                    }
                    if (i > 70) {
                        o = userScreen.colBrWhite;
                    }
                    console.putInt(x, y, userScreen.colBlack, o, false, 88);
                }
            }
            console.refresh();
            bits.sleep(500);
        }
    }

    /**
     * life game if has 2 or 3 neighbors, then it stays alive, otherwise dies if
     * has 3 neighbors, then a new one borns
     */
    public void doLife() {
        int[][] buf = new int[console.sizY + 10][console.sizX + 10];
        for (int y = 0; y < console.sizY; y++) {
            for (int x = 0; x < console.sizX; x++) {
                int i = 0;
                if (bits.randomB() < 80) {
                    i = 1;
                }
                buf[y + 4][x + 4] = i;
            }
        }
        for (;;) {
            if (console.keyPress()) {
                break;
            }
            int[][] old = buf;
            buf = new int[console.sizY + 10][console.sizX + 10];
            for (int y = 0; y < console.sizY; y++) {
                for (int x = 0; x < console.sizX; x++) {
                    int p = old[y + 3][x + 3] + old[y + 3][x + 4] + old[y + 3][x + 5] + old[y + 4][x + 3] + old[y + 4][x + 5] + old[y + 5][x + 3] + old[y + 5][x + 4] + old[y + 5][x + 5];
                    int o = old[y + 4][x + 4];
                    int i = 0;
                    if ((o == 1) && (p == 2)) {
                        i = 1;
                    }
                    if (p == 3) {
                        i = 1;
                    }
                    buf[y + 4][x + 4] = i;
                }
            }
            console.putCls();
            for (int y = 0; y < console.sizY; y++) {
                for (int x = 0; x < console.sizX; x++) {
                    int i = buf[y + 4][x + 4];
                    if (i == 0) {
                        i = 32;
                    } else {
                        i = 88;
                    }
                    console.putInt(x, y, userScreen.colBlack, userScreen.colWhite, false, i);
                }
            }
            console.refresh();
            bits.sleep(500);
        }
    }

    private void doAntBall() {
        console.putCls();
        for (;;) {
            if (console.keyPress()) {
                return;
            }
            for (int o = 0; o < console.sizY; o++) {
                for (int i = 0; i < console.sizX; i++) {
                    int p;
                    if ((bits.randomB() & 1) == 0) {
                        p = userScreen.colBlack;
                    } else {
                        p = userScreen.colWhite;
                    }
                    console.putStr(i, o, p, 0, false, " ");
                }
            }
            console.refresh();
            bits.sleep(500);
        }
    }

    /**
     * random quotes from zen master
     *
     * @param cmd command line to use
     */
    public void doZenmaster(cmds cmd) {
        servQuote ntry = new servQuote();
        ntry.srvName = cmd.word();
        ntry = cfgAll.dmnQuote.find(ntry, false);
        if (ntry == null) {
            cmd.error("no such server");
            return;
        }
        for (;;) {
            cmd.pipe.strPut("you: ");
            String a = cmd.pipe.lineGet(0x12);
            if (a.length() < 1) {
                if (cmd.pipe.isClosed() != 0) {
                    break;
                }
                continue;
            }
            cmd.pipe.linePut("");
            String b = a.trim().toLowerCase();
            if (b.equals("quit") || b.equals("exit")) {
                break;
            }
            cmd.pipe.linePut("zen: " + ntry.getOneLine());
        }
    }

    /**
     * do one command
     *
     * @param cmd parameters
     */
    public void doCommand(cmds cmd) {
        String a = cmd.word();
        if (a.equals("gomoku")) {
            userGameGomoku t = new userGameGomoku(console);
            t.doStart();
            t.doGame();
            t.doFinish();
            return;
        }
        if (a.equals("zenmaster")) {
            doZenmaster(cmd);
            return;
        }
        if (a.equals("tetris")) {
            userGameTetris t = new userGameTetris(console);
            t.doStart();
            t.doGame();
            t.doFinish();
            return;
        }
        if (a.equals("minesweep")) {
            userGameMines t = new userGameMines(console);
            t.doStart();
            t.doGame();
            t.doFinish();
            return;
        }
        if (a.equals("hanoi")) {
            userGameHanoi t = new userGameHanoi(console);
            t.doStart();
            t.doGame();
            t.doFinish();
            return;
        }
        if (a.equals("clear")) {
            userScreen.sendTit(console.pipe, cfgAll.hostName);
            return;
        }
        int i = version.findSecret(a);
        if (i >= 0) {
            List<String> sec = version.shSecret(i);
            int[] god = new int[6];
            god[0] = userScreen.colBrCyan;
            god[1] = userScreen.colBrWhite;
            god[2] = userScreen.colBrYellow;
            god[3] = userScreen.colBrGreen;
            god[4] = userScreen.colBrBlue;
            god[5] = userScreen.colBrRed;
            colorDrawer(god, sec);
            return;
        }
        if (a.equals("chat")) {
            userChat c = new userChat(cmd.pipe, reader);
            c.doChat();
            return;
        }
        if (a.equals("send")) {
            a = doSend(cmd);
            cmd.error(a);
            return;
        }
        if (a.equals("ansi")) {
            userFlash.ansiArt(cmd.getRemaining(), console);
            for (;;) {
                if (console.keyPress()) {
                    break;
                }
                bits.sleep(1000);
            }
            return;
        }
        if (a.equals("image")) {
            a = cmd.getRemaining();
            doText(userFlash.asciiArt(a, console.sizX, console.sizY));
            return;
        }
        if (a.equals("color")) {
            doPalette();
            return;
        }
        if (a.equals("ascii")) {
            doAscTab();
            return;
        }
        if (a.equals("vmkeys")) {
            doKeys();
            return;
        }
        if (a.equals("title")) {
            a = cmd.getRemaining();
            if (a.length() < 1) {
                a = cfgAll.hostName;
            }
            userScreen.sendTit(console.pipe, a);
            return;
        }
        if (a.equals("text")) {
            a = cmd.getRemaining();
            if (a.length() < 1) {
                a = version.namVer;
            }
            doText(bits.str2lst(a));
            return;
        }
        if (a.equals("logo")) {
            a = cmd.getRemaining();
            List<String> txt;
            if (a.length() < 1) {
                txt = version.shLogo(0x08);
            } else {
                txt = userScreen.fontText(a, " ", userFonts.fontFiller, userFonts.font8x16());
            }
            doText(txt);
            return;
        }
        if (a.equals("clock")) {
            doClock(userFonts.font8x16());
            return;
        }
        if (a.equals("snake")) {
            doSnake();
            return;
        }
        if (a.equals("matrix")) {
            doMatrix();
            return;
        }
        if (a.equals("fire")) {
            doFire();
            return;
        }
        if (a.equals("life")) {
            doLife();
            return;
        }
        if (a.equals("antball")) {
            doAntBall();
            return;
        }
        cmd.badCmd();
    }

}
