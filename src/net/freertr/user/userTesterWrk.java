package net.freertr.user;

import net.freertr.util.logger;

/**
 * one tester work
 *
 * @author mate csaba
 */
public class userTesterWrk implements Runnable {

    public int slot;

    public userTester lower;

    public userTesterWrk(userTester parent, int slt) {
        lower = parent;
        slot = slt;
        new Thread(this).start();
    }

    public void run() {
        try {
            lower.doWorker(slot);
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}
