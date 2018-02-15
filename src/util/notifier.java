package util;

/**
 * notifier
 *
 * @author matecsaba
 */
public class notifier {

    private int waked = 0; // number of wakes received

    private int notified = 0; // number of notifications received

    private final Object lck = new Object(); // locker

    private notifier peer = this; // peer to notify

    /**
     * create new notifier
     */
    public notifier() {
    }

    /**
     * set peer that will be notified
     *
     * @param per peer notifier
     */
    public void setPeer(notifier per) {
        if (peer == null) {
            peer = this;
        } else {
            peer = per;
        }
    }

    /**
     * wait for timeout or notification whichever comes first
     *
     * @param msec time to wait, 0 means forever
     */
    public void sleep(int msec) {
        try {
            synchronized (lck) {
                if (msec < 1) {
                    lck.wait();
                } else {
                    lck.wait(msec);
                }
                waked = 0;
            }
        } catch (Exception E) {
            logger.info("failed to wait");
        }
    }

    /**
     * read and clear number of wakeups missed
     *
     * @return number of wakeups missed
     */
    public int missedWakes() {
        synchronized (lck) {
            int i = waked;
            waked = 0;
            return i;
        }
    }

    /**
     * read number of notifications received
     *
     * @return number of notifications received
     */
    public int totalNotifies() {
        return notified;
    }

    /**
     * wake up peer
     */
    public void wakeup() {
        try {
            synchronized (peer.lck) {
                peer.waked++;
                peer.notified++;
                peer.lck.notify();
            }
        } catch (Exception E) {
            logger.info("failed to notify");
        }
    }

    public String toString() {
        return "notifier";
    }

}
