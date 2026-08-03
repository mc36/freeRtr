
/**
 * motion mail handler
 *
 * @author matecsaba
 */
public class motionMail implements Runnable {

    private final motion parent;

    private final String myName;

    private final String myPath;

    /**
     * create instance
     *
     * @param lower parent
     * @param nam name
     * @param pat path
     */
    public motionMail(motion lower, String nam, String pat) {
        parent = lower;
        myName = nam;
        myPath = pat;
        new Thread(this).start();
    }

    public void run() {
        try {
            parent.mailAlert(myName, myPath);
        } catch (Exception e) {
        }
    }

}
