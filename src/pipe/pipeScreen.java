package pipe;

import java.util.ArrayList;
import java.util.List;
import user.userScreen;
import util.bits;
import util.cmds;

/**
 * display pipeline to screen
 *
 * @author matecsaba
 */
public class pipeScreen {

    /**
     * set true if changed
     */
    public boolean changed;

    /**
     * screen to read
     */
    public final userScreen scr;

    private final pipeSide pipe;

    private int savX;

    private int savY;

    private List<Integer> escBuf = null;

    /**
     * create new screen
     *
     * @param pip connection to use
     * @param x x size
     * @param y y size
     */
    public pipeScreen(pipeSide pip, int x, int y) {
        pipe = pip;
        scr = new userScreen(pipeDiscard.needAny(null), x, y);
    }

    private void putChar(int ch) {
        scr.putInt(scr.curX, scr.curY, true, scr.col, ch);
    }

    private static int getParam(cmds cmd) {
        int i = bits.str2num(cmd.word(";"));
        if (i < 1) {
            i = 1;
        }
        return i;
    }

    private void doCtrl(int ch) {
        byte[] buf = new byte[escBuf.size()];
        for (int i = 0; i < buf.length; i++) {
            int o = escBuf.get(i);
            buf[i] = (byte) o;
        }
        escBuf = null;
        String s = new String(buf);
        if (s.startsWith("[")) {
            s = s.substring(1, s.length());
        }
        cmds cmd = new cmds("", s);
        switch (ch) {
            case 65: // A
                scr.curY -= getParam(cmd);
                scr.curRange(scr.col);
                return;
            case 66: // B
                scr.curY += getParam(cmd);
                scr.curRange(scr.col);
                return;
            case 67: // C
                scr.curX += getParam(cmd);
                scr.curRange(scr.col);
                return;
            case 68: // D
                scr.curX -= getParam(cmd);
                scr.curRange(scr.col);
                return;
            case 69: // E
                scr.curY += getParam(cmd);
                scr.curX = 0;
                scr.curRange(scr.col);
                return;
            case 70: // F
                scr.curY -= getParam(cmd);
                scr.curX = 0;
                scr.curRange(scr.col);
                return;
            case 71: // G
                scr.curX = getParam(cmd) - 1;
                scr.curRange(scr.col);
                return;
            case 72: // H
                scr.curY = getParam(cmd) - 1;
                scr.curX = getParam(cmd) - 1;
                scr.curRange(scr.col);
                return;
            case 74: // J
                switch (bits.str2num(cmd.word(";"))) {
                    case 0:
                        scr.fillLine(scr.curY, scr.curX, scr.sizX, scr.col, 32);
                        scr.fillLines(scr.curY + 1, scr.sizY, scr.col, 32);
                        return;
                    case 1:
                        scr.fillLines(0, scr.curY - 1, scr.col, 32);
                        scr.fillLine(scr.curY, 0, scr.curX, scr.col, 32);
                        return;
                    case 2:
                        scr.fillLines(0, scr.sizY, scr.col, 32);
                        scr.curY = 0;
                        scr.curX = 0;
                        scr.curRange(scr.col);
                        return;
                }
                return;
            case 75: // K
                switch (bits.str2num(cmd.word(";"))) {
                    case 0:
                        scr.fillLine(scr.curY, scr.curX, scr.sizX, scr.col, 32);
                        return;
                    case 1:
                        scr.fillLine(scr.curY, 0, scr.curX, scr.col, 32);
                        return;
                    case 2:
                        scr.fillLine(scr.curY, 0, scr.sizX, scr.col, 32);
                        return;
                }
                return;
            case 83: // S
                int o = getParam(cmd);
                for (int i = 0; i < o; i++) {
                    scr.scrollUp(scr.col);
                }
                return;
            case 84: // T
                o = getParam(cmd);
                for (int i = 0; i < o; i++) {
                    scr.scrollDn(scr.col);
                }
                return;
            case 102: // f
                scr.curY = getParam(cmd) - 1;
                scr.curX = getParam(cmd) - 1;
                scr.curRange(scr.col);
                return;
            case 109: // m
                for (;;) {
                    s = cmd.word(";");
                    if (s.length() < 1) {
                        break;
                    }
                    int i = bits.str2num(s);
                    switch (i) {
                        case 0:
                            scr.col = userScreen.colWhite;
                            break;
                        case 1:
                            scr.col |= userScreen.colBright;
                            break;
                        case 5:
                            scr.col |= userScreen.colBright << 16;
                            break;
                        case 7:
                            scr.col = (scr.col >>> 16) | (scr.col << 16);
                            break;
                        case 21:
                            scr.col &= ~userScreen.colBright;
                            break;
                        case 25:
                            scr.col &= ~(userScreen.colBright << 16);
                            break;
                        case 27:
                            scr.col = (scr.col >>> 16) | (scr.col << 16);
                            break;
                        case 30:
                        case 31:
                        case 32:
                        case 33:
                        case 34:
                        case 35:
                        case 36:
                        case 37:
                            scr.col = (scr.col & 0xfffffff8) | (i % 10);
                            break;
                        case 40:
                        case 41:
                        case 42:
                        case 43:
                        case 44:
                        case 45:
                        case 46:
                        case 47:
                            scr.col = (scr.col & 0xfff8ffff) | ((i % 10) << 16);
                            break;
                    }
                }
                return;
            case 110: // n
                pipe.strPut("\033[" + (scr.curX + 1) + ";" + (scr.curY + 1) + "R");
                return;
            case 115: // s
                savX = scr.curX;
                savY = scr.curY;
                return;
            case 117: // u
                scr.curX = savX;
                scr.curY = savY;
                scr.curRange(scr.col);
                return;
            default:
                buf = new byte[1];
                buf[0] = (byte) ch;
                doChar("\033[" + cmd.getRemaining() + new String(buf));
                return;
        }
    }

    /**
     * put out character
     *
     * @param ch character to write
     */
    public void doChar(int ch) {
        changed = true;
        if (escBuf != null) {
            switch (ch) {
                case 0x30: // 0
                case 0x31: // 1
                case 0x32: // 2
                case 0x33: // 3
                case 0x34: // 4
                case 0x35: // 5
                case 0x36: // 6
                case 0x37: // 7
                case 0x38: // 8
                case 0x39: // 9
                case 0x3b: // ;
                case 0x5b: // [
                    escBuf.add(ch);
                    return;
            }
            doCtrl(ch);
            scr.curRange(scr.col);
            return;
        }
        scr.curRange(scr.col);
        switch (ch) {
            case 8: // backspace
                putChar(32);
                scr.curX -= 2;
                scr.curRange(scr.col);
                return;
            case 13: // cr
                scr.curX = 0;
                scr.curRange(scr.col);
                return;
            case 10: // lf
                scr.curY++;
                scr.curRange(scr.col);
                return;
            case 27: // esc
                escBuf = new ArrayList<Integer>();
                return;
        }
        putChar(ch);
        scr.curRange(scr.col);
    }

    /**
     * put out characters
     *
     * @param buf characters to put
     */
    public void doChar(byte[] buf) {
        for (int i = 0; i < buf.length; i++) {
            doChar(buf[i]);
            scr.curRange(scr.col);
        }
    }

    /**
     * put out characters
     *
     * @param str string to put
     */
    public void doChar(String str) {
        doChar(str.getBytes());
    }

}
