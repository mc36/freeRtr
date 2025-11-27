
import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.Socket;
import java.net.URI;
import java.net.URL;
import java.net.URLConnection;

/**
 * motion sensor handler
 *
 * @author matecsaba
 */
public class motionSens implements Runnable {

    /**
     * parent of me
     */
    protected final motion parent;

    /**
     * my number
     */
    protected int myNum;

    /**
     * name of sensor
     */
    protected String myName;

    /**
     * host of sensor
     */
    protected String myHost;

    /**
     * port of sensor
     */
    protected int myPort;

    /**
     * bit of sensor
     */
    protected int myBit;

    /**
     * need of sensor
     */
    protected int myNed;

    /**
     * reget of sensor
     */
    protected int reget;

    /**
     * alarm mode
     */
    protected int alarmMail;

    /**
     * alarm http
     */
    protected int alarmHttp;

    /**
     * events happened
     */
    protected int events;

    /**
     * last event time
     */
    protected long lastEvnt;

    /**
     * reads happened
     */
    protected int reads;

    /**
     * exceptions happened
     */
    protected int calls;

    /**
     * exceptions happened
     */
    protected int errors;

    /**
     * create instance
     *
     * @param lower parent
     */
    protected motionSens(motion lower) {
        parent = lower;
    }

    /**
     * clear statistics
     */
    protected void doClear() {
    }

    /**
     * get web line
     *
     * @param tim current time
     * @return string
     */
    protected String getMeas(long tim) {
        String a;
        if (lastEvnt < 1) {
            a = "never";
        } else {
            a = motionUtil.timePast(tim, lastEvnt);
        }
        return "<tr><td>" + myNum + "</td><td>" + myName + "</td><td>" + parent.needAlert(alarmMail) + "," + parent.needAlert(alarmHttp) + "</td><td>" + events + "</td><td>" + a + "</td><td>" + errors + "</td><td>" + reads + "</td><td>" + calls + "</td><td><a href=\"" + parent.url + "?cmd=img&nam=" + myNum + "\">pic</a> <a href=\"" + parent.url + "?cmd=liv&nam=" + myNum + "\">vid</a> <a href=\"" + parent.url + "?cmd=sel&nam=" + myNum + "\">sel</a></td><td>-</td><td>-</td><td>-</td><td>-</td></tr>";
    }

    /**
     * this is needed for cli startup
     *
     * @param args command line parameters
     */
    private void doRound() throws Exception {
        Socket sck = new Socket(myHost, myPort);
        sck.setSoTimeout(30000);
        InputStream in = sck.getInputStream();
        long lst = 0;
        boolean old = true;
        for (;;) {
            int i = in.read();
            reads++;
            if (i < 0) {
                errors++;
                break;
            }
            boolean cur = (i & myBit) == myNed;
            if (old != cur) {
                lastEvnt = motionUtil.getTime();
                events++;
            }
            old = cur;
            if (cur) {
                continue;
            }
            if ((motionUtil.getTime() - lst) < reget) {
                continue;
            }
            calls++;
            lst = motionUtil.getTime();
            if (parent.needAlert(alarmMail)) {
                new motionMail(parent, myName, "");
            }
            if (parent.needAlert(alarmHttp)) {
                new motionHttp(parent);
            }
        }
        in.close();
    }

    public void run() {
        for (;;) {
            try {
                doRound();
            } catch (Exception e) {
                errors++;
            }
        }
    }

}
