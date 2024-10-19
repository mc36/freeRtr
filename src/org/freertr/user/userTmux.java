package org.freertr.user;

import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeScreen;
import org.freertr.pipe.pipeSide;
import org.freertr.util.bits;

/**
 * terminal multiplexer
 *
 * @author matecsaba
 */
public class userTmux {

    private final userScreen cons;

    private final userExec orig;

    private int begX[];

    private int begY[];

    private int sizX;

    private int sizY;

    private int cur;

    private pipeScreen scr[];

    private userExec exe[];

    /**
     * screen multiplexer
     *
     * @param scr connection to use
     * @param exe exec to use
     */
    public userTmux(userScreen scr, userExec exe) {
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
        orig.reader.keyFlush();
        cons.putCls();
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
        for (int i = 0; i < begX.length; i++) {
            pipeLine pl = new pipeLine(32768, false);
            pipeSide pip = pl.getSide();
            scr[i] = new pipeScreen(pip, sizX, sizY);
            pip = pl.getSide();
            pip.lineTx = pipeSide.modTyp.modeCRLF;
            pip.lineRx = pipeSide.modTyp.modeCRorLF;
            userReader rdr = new userReader(pip, null);
            pip.settingsCopy(cons.pipe);
            userReader.setTermWdt(pip, sizX);
            userReader.setTermLen(pip, sizY);
            pip.setTime(0);
            exe[i] = new userExec(pip, rdr);
            exe[i].needExpand = true;
            exe[i].privileged = orig.privileged;
            exe[i].authorization = orig.authorization;
        }
        cur = 0;
        return false;
    }

    private void doRound() {
        for (int i = 0; i < scr.length; i++) {
            if (scr[i].doRound(false)) {
                continue;
            }
            if (!scr[i].changed) {
                continue;
            }
            scr[i].changed = false;
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

        for (int i = 0; i < 10; i++) {
            doRound();
            bits.sleep(100);
        }

        cons.putCls();
        cons.refresh();
        orig.reader.keyFlush();
    }

}
