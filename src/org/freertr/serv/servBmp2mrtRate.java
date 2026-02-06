package org.freertr.serv;

import org.freertr.util.bits;
import org.freertr.util.logger;

/**
 * bmp server rate
 *
 * @author matecsaba
 */
public class servBmp2mrtRate implements Runnable {

    private final servBmp2mrt lower;

    private boolean need2run;

    private int timSlp;

    /**
     * create instance
     *
     * @param prnt parent to use
     * @param tim time to sleep
     */
    public servBmp2mrtRate(servBmp2mrt prnt, int tim) {
        timSlp = tim;
        lower = prnt;
    }

    public void run() {
        for (;;) {
            if (!need2run) {
                break;
            }
            bits.sleep(timSlp);
            try {
                lower.doRates();
            } catch (Exception e) {
                logger.traceback(e);
            }
        }
    }

    /**
     * start work
     */
    public void startWork() {
        if (timSlp < 1) {
            return;
        }
        need2run = true;
        new Thread(this).start();
    }

    /**
     * stop work
     */
    public void stopWork() {
        need2run = false;
    }

}
