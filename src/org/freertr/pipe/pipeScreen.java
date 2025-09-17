package org.freertr.pipe;

import java.util.ArrayList;
import java.util.List;
import org.freertr.user.userRead;
import org.freertr.user.userScreen;
import org.freertr.util.bits;
import org.freertr.util.cmds;

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
        pipeSide ps = pipeDiscard.needAny(pip);
        userRead.setTermWdt(ps, x);
        userRead.setTermLen(ps, y);
        scr = new userScreen(ps);
        scr.putCls();
    }

    private void putChar(int ch, boolean cr) {
        scr.putInt(scr.curX, scr.curY, cr, scr.col, ch);
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
                scr.curRange(-1);
                return;
            case 66: // B
                scr.curY += getParam(cmd);
                scr.curRange(-1);
                return;
            case 67: // C
                scr.curX += getParam(cmd);
                scr.curRange(-1);
                return;
            case 68: // D
                scr.curX -= getParam(cmd);
                scr.curRange(-1);
                return;
            case 69: // E
                scr.curY += getParam(cmd);
                scr.curX = 0;
                scr.curRange(-1);
                return;
            case 70: // F
                scr.curY -= getParam(cmd);
                scr.curX = 0;
                scr.curRange(-1);
                return;
            case 71: // G
                scr.curX = getParam(cmd) - 1;
                scr.curRange(-1);
                return;
            case 72: // H
                scr.curY = getParam(cmd) - 1;
                scr.curX = getParam(cmd) - 1;
                scr.curRange(-1);
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
                        case 38:
                            cmd.word(";");
                            s = cmd.word(";");
                            i = bits.str2num(s);
                            scr.col = (scr.col & 0xffffff00) | (i % 255);
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
                        case 48:
                            cmd.word(";");
                            s = cmd.word(";");
                            i = bits.str2num(s);
                            scr.col = (scr.col & 0xff00ffff) | ((i % 255) << 16);
                            break;
                        case 90:
                        case 91:
                        case 92:
                        case 93:
                        case 94:
                        case 95:
                        case 96:
                        case 97:
                            scr.col = (scr.col & 0xfffffff8) | (i % 10) | userScreen.colBright;
                            break;
                        case 100:
                        case 101:
                        case 102:
                        case 103:
                        case 104:
                        case 105:
                        case 106:
                        case 107:
                            scr.col = (scr.col & 0xfff8ffff) | ((i % 10) << 16) | (userScreen.colBright << 16);
                            break;
                    }
                }
                return;
            case 110: // n
                pipe.strPut("\033[" + (scr.curY + 1) + ";" + (scr.curX + 1) + "R");
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
        switch (ch) {
            case 7: // bell
                return;
            case 8: // backspace
                scr.curX -= 1;
                scr.curRange(scr.col);
                putChar(32, false);
                return;
            case 9: // tab
                scr.curX = (scr.curX + 8) & 0xfff8;
                scr.curRange(scr.col);
                return;
            case 10: // lf
                scr.curY++;
                scr.curRange(scr.col);
                return;
            case 13: // cr
                scr.curX = 0;
                scr.curRange(scr.col);
                return;
            case 27: // esc
                escBuf = new ArrayList<Integer>();
                return;
        }
        scr.curRange(scr.col);
        putChar(ch, true);
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

    /**
     * do one round
     *
     * @param wait set true to wait for input, false to fail otherwise
     * @return true on error, false on success
     */
    public boolean doRound(boolean wait) {
        int i = pipe.ready2rx();
        if (i < 1) {
            if (wait) {
                i = 1;
            } else {
                return true;
            }
        }
        byte[] buf = new byte[i];
        if (pipe.moreGet(buf, 0, i) != i) {
            return true;
        }
        doChar(buf);
        return false;
    }

}
