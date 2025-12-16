
/**
 * motion http handler
 *
 * @author matecsaba
 */
public class motionHttp implements Runnable {

    private final motion parent;

    /**
     * create instance
     *
     * @param lower parent
     */
    public motionHttp(motion lower) {
        parent = lower;
        new Thread(this).start();
    }

    public void run() {
        try {
            parent.httpAlert();
        } catch (Exception e) {
        }
    }

}
