package org.freertr.user;

/**
 * terminal multiplexer
 *
 * @author matecsaba
 */
public class userTmux {

    private final userScreen console;

    private final userExec exec;

    private int begX[];

    private int begY[];

    private int sizX[];

    private int sizY[];

    /**
     * screen multiplexer
     *
     * @param conn connection to use
     * @param rdr reader to use
     */
    public userTmux(userScreen conn, userExec exe) {
        console = conn;
        exec = exe;
    }

    /**
     * split screen
     *
     * @param mode mode, 0=none, 1=x, 2=y, 3=both
     */
    public void doInit(int mode) {
        exec.reader.keyFlush();
        console.putCls();
        switch (mode) {
            case 1: // x
                begX = new int[2];
                begY = new int[2];
                sizX = new int[2];
                sizY = new int[2];
                sizX[0] = (console.sizX - 1) / 2;
                sizY[0] = console.sizY - 1;
                begX[1] = console.sizX - sizX[0];
                break;
            case 2: // y
                begX = new int[2];
                begY = new int[2];
                sizX = new int[2];
                sizY = new int[2];
                sizX[0] = console.sizX - 1;
                sizY[0] = (console.sizY - 1) / 2;
                begY[1] = console.sizY - sizY[0];
                break;
            case 3:
                begX = new int[4];
                begY = new int[4];
                sizX = new int[4];
                sizY = new int[4];
                sizX[0] = (console.sizX - 1) / 2;
                sizY[0] = (console.sizY - 1) / 2;
                begX[1] = console.sizX - sizX[0];
                begY[2] = console.sizY - sizY[0];
                begX[3] = begX[1];
                begY[3] = begY[2];
                break;
            default:
                begX = new int[1];
                begY = new int[1];
                sizX = new int[1];
                sizY = new int[1];
                sizX[0] = console.sizX - 1;
                sizY[0] = console.sizY - 1;
                break;
        }
        for (int i = 1; i < sizX.length; i++) {
            sizX[i] = sizX[0];
            sizY[i] = sizY[0];
        }
    }

    /**
     * start screen
     */
    public void doWork() {

        console.putCls();
        console.refresh();
        exec.reader.keyFlush();
    }

}
