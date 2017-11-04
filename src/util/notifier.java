package util;

/**
 * notifier
 *
 * @author matecsaba
 */
public class notifier {

    private int waked = 0; // number of wakes received

    private int notified = 0; // number of notifications received

    private final int valDat[]; // this will referenced

    private final Object valRef; // reference wait on

    private notifier peer; // peer to notify

    private static int nextNumber;

    /**
     * create new notifier
     */
    public notifier() {
        valDat = new int[2];
        valRef = valDat;
        peer = this;
        valDat[0] = (nextNumber++);
        valDat[1] = 0;
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
            synchronized (valRef) {
                if (msec < 1) {
                    valRef.wait();
                } else {
                    valRef.wait(msec);
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
        synchronized (valRef) {
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
            synchronized (peer.valRef) {
                peer.waked++;
                peer.notified++;
                peer.valRef.notify();
            }
        } catch (Exception E) {
            logger.info("failed to notify");
        }
    }

    public String toString() {
        return "notifier#" + valDat[0];
    }

}
