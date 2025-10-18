package org.freertr.user;

import org.freertr.pipe.pipeScreen;
import java.util.ArrayList;
import java.util.List;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeTerm;
import org.freertr.pipe.pipeSide;
import org.freertr.util.bits;
import org.freertr.util.logger;

/**
 * terminal multiplexer
 *
 * @author matecsaba
 */
public class userTmux {

    private final pipeScreen cons;

    private final userExec orig;

    private int begX[];

    private int begY[];

    private int cur;

    private pipeTerm scr[];

    private userRead rdr[];

    private userExec exe[];

    private userConfig cfg[];

    private boolean running = true;

    /**
     * screen multiplexer
     *
     * @param scr connection to use
     * @param exe exec to use
     */
    public userTmux(pipeScreen scr, userExec exe) {
        cons = scr;
        orig = exe;
    }

    /**
     * split screen
     *
     * @param mode mode, 0=none, 1=x, 2=y, 3=both
     * @return true on error, false on success
     */
    public boolean doInit(int mode) {
        int sizX;
        int sizY;
        switch (mode) {
            case 1: // x
                begX = new int[2];
                begY = new int[2];
                sizX = (cons.sizX - 1) / 2;
                sizY = cons.sizY - 1;
                begX[1] = cons.sizX - sizX;
                break;
            case 2: // y
                begX = new int[2];
                begY = new int[2];
                sizX = cons.sizX - 1;
                sizY = (cons.sizY - 1) / 2;
                begY[1] = cons.sizY - sizY;
                break;
            case 3:
                begX = new int[4];
                begY = new int[4];
                sizX = (cons.sizX - 1) / 2;
                sizY = (cons.sizY - 1) / 2;
                begX[1] = cons.sizX - sizX;
                begY[2] = cons.sizY - sizY;
                begX[3] = begX[1];
                begY[3] = begY[2];
                break;
            default:
                begX = new int[1];
                begY = new int[1];
                sizX = cons.sizX - 1;
                sizY = cons.sizY - 1;
                break;
        }
        if (sizX < 20) {
            return true;
        }
        if (sizY < 5) {
            return true;
        }
        scr = new pipeTerm[begX.length];
        exe = new userExec[begX.length];
        cfg = new userConfig[begX.length];
        for (int i = 0; i < begX.length; i++) {
            pipeLine pl = new pipeLine(32768, false);
            pipeSide pip = pl.getSide();
            scr[i] = new pipeTerm(pip, sizX, sizY);
            scr[i].scr.putCls();
            pip.setTime(0);
            pip = pl.getSide();
            pip.lineTx = pipeSide.modTyp.modeCRLF;
            pip.lineRx = pipeSide.modTyp.modeCRorLF;
            rdr[i] = new userRead(pip, null);
            pip.settingsCopy(cons.pipe);
            userRead.setTermWdt(pip, sizX);
            userRead.setTermLen(pip, sizY);
            pip.setTime(0);
            exe[i] = new userExec(pip, rdr[i]);
            cfg[i] = new userConfig(pip, rdr[i]);
            orig.copy2exec(exe[i]);
            orig.copy2cfg(cfg[i]);
            new userTmuxWin(this, i);
        }
        cur = 0;
        cons.putCls();
        return false;
    }

    /**
     * do one window
     *
     * @param n number
     */
    protected void doWindow(int n) {
        for (;;) {
            if (!running) {
                break;
            }
            userLine.doCommands(rdr[n], exe[n], cfg[n]);
            exe[n].pipe.linePut("% not on this line");
        }
    }

    private void doKeys() {
        if (cons.pipe.isClosed() != 0) {
            running = false;
            return;
        }
        int i = cons.pipe.ready2rx();
        if (i < 1) {
            return;
        }
        byte[] buf = new byte[i];
        cons.pipe.blockingGet(buf, 0, i);
        if (buf[0] != 2) {
            scr[cur].scr.pipe.blockingPut(buf, 0, buf.length);
            return;
        }
        if (i > 1) {
            i = buf[1];
        } else {
            cons.pipe.blockingGet(buf, 0, 1);
            i = buf[0];
        }
        switch (i) {
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
                cur = (i - 1) % scr.length;
                break;
            case 0x62: // b
            case 0x42: // B
            case 0x02: // ctrl+b
                buf = new byte[1];
                buf[0] = 2;
                scr[cur].scr.pipe.blockingPut(buf, 0, buf.length);
                break;
            case 0x3f: // ?
            case 0x68: // h
            case 0x48: // H
            case 0x08: // ctrl+h
                List<String> l = new ArrayList<String>();
                l.add("ctrl+h - help");
                l.add("ctrl+l - redraw screen");
                l.add("ctrl+b - send ctrl+b to window");
                l.add("number - select window");
                l.add("ctrl+n - next window");
                l.add("ctrl+n - previous window");
                l.add("ctrl+q - exit");
                l.add("ctrl+x - exit");
                l.add("ctrl+c - exit");
                cons.helpWin(pipeScreen.colBlue, pipeScreen.colWhite, pipeScreen.colBrWhite, -1, -1, -1, -1, l);
                break;
            case 0x6c: // l
            case 0x4c: // L
            case 0x0c: // ctrl+l
                cons.putCls();
                break;
            case 0x6e: // n
            case 0x4e: // N
            case 0x0e: // ctrl+n
                cur = (cur + 1) % scr.length;
                break;
            case 0x70: // p
            case 0x50: // P
            case 0x10: // ctrl+p
                cur = (cur - 1 + scr.length) % scr.length;
                break;
            case 0x78: // x
            case 0x58: // X
            case 0x18: // ctrl+x
            case 0x71: // q
            case 0x51: // Q
            case 0x11: // ctrl+q
            case 0x63: // c
            case 0x43: // C
            case 0x03: // ctrl+c
                running = false;
                break;
        }
    }

    private void doRound() {
        for (int o = 0; o < cons.sizY; o++) {
            for (int i = 0; i < cons.sizX; i++) {
                cons.putInt(i, o, pipeScreen.colWhite, pipeScreen.colBlack, false, 32);
            }
        }
        for (int i = 0; i < scr.length; i++) {
            orig.updateLast(exe[i]);
            scr[i].doRound(false);
            cons.putScr(begX[i], begY[i], scr[i].scr, false);
        }
        cons.putScr(begX[cur], begY[cur], scr[cur].scr, true);
        cons.refresh();
    }

    /**
     * start screen
     */
    public void doWork() {
        cons.putCls();
        try {
            for (;;) {
                if (!running) {
                    break;
                }
                doKeys();
                doRound();
                bits.sleep(100);
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        running = false;
        for (int i = 0; i < scr.length; i++) {
            exe[i].pipe.setClose();
        }
        cons.putCls();
        cons.refresh();
    }

}

class userTmuxWin implements Runnable {

    public final int num;

    public final userTmux lower;

    public userTmuxWin(userTmux p, int n) {
        lower = p;
        num = n;
        new Thread(this).start();
    }

    public void run() {
        try {
            lower.doWindow(num);
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}
