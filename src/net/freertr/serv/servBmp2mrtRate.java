package net.freertr.serv;

import java.util.TimerTask;

/**
 * bmp server rate
 *
 * @author matecsaba
 */
public class servBmp2mrtRate extends TimerTask {

    private final servBmp2mrt lower;

    public servBmp2mrtRate(servBmp2mrt prnt) {
        lower = prnt;
    }

    public void run() {
        lower.doRates();
    }

}
