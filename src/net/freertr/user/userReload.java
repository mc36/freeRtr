package net.freertr.user;

import java.util.List;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgInit;
import net.freertr.util.bits;
import net.freertr.util.logger;
import net.freertr.util.notifier;

/**
 * stores one reload request
 *
 * @author matecsaba
 */
public class userReload implements Runnable {

    /**
     * time when reload will happen
     */
    public final long when;

    /**
     * mode
     */
    public final int code;

    /**
     * interval between checks
     */
    public int interval = 60000;

    private boolean need2run = true;

    private final notifier notif;

    public String toString() {
        return bits.time2str(cfgAll.timeZoneName, when + cfgAll.timeServerOffset, 3);
    }

    /**
     * schedule reload
     *
     * @param mod mode: false=warm, true=cold
     * @param tim time
     */
    public userReload(boolean mod, long tim) {
        if (mod) {
            code = 4;
        } else {
            code = 5;
        }
        when = tim;
        notif = new notifier();
        new Thread(this).start();
    }

    /**
     * get reload time
     *
     * @param rel what to query
     * @return show lines
     */
    public static List<String> reloadShow(userReload rel) {
        if (rel == null) {
            return bits.str2lst("no scheduled reload");
        }
        return bits.str2lst(bits.timeLeft(rel.when) + " to reload, will happen at " + rel + " with code " + rel.code);
    }

    /**
     * stop working
     */
    public void stopWorking() {
        need2run = false;
        notif.wakeup();
    }

    public void run() {
        try {
            doer();
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

    private void doer() {
        logger.error("reload scheduled to " + this);
        for (;;) {
            notif.sleep(interval);
            if (!need2run) {
                break;
            }
            if (bits.getTime() > when) {
                cfgInit.stopRouter(true, 5, "timed request");
            }
            logger.warn(bits.timeLeft(when) + " to reload");
        }
        logger.error("reload canceled from " + this);
    }

}
