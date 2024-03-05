package org.freertr.user;

import org.freertr.util.logger;

/**
 * one tester work
 *
 * @author mate csaba
 */
public class userTesterWrk implements Runnable {

    /**
     * parent
     */
    protected final userTester lower;

    /**
     * slot to use
     */
    protected final int slot;

    /**
     * create instance
     *
     * @param parent parent
     * @param slt slot to use
     */
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
