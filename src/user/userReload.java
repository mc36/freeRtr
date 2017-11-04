package user;

import cfg.cfgAll;
import cfg.cfgInit;
import java.util.List;
import util.bits;
import util.logger;
import util.notifier;

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
     * @param tim time
     */
    public userReload(long tim) {
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
        return bits.str2lst(bits.timeLeft(rel.when) + " to reload, will happen at " + rel);
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
