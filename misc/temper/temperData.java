
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLConnection;

/**
 * temperature reading
 *
 * @author matecsaba
 */
public class temperData {

    /**
     * my number
     */
    protected final int myNum;

    /**
     * my name
     */
    protected final String myNam;

    /**
     * my url
     */
    protected final String myUrl;

    /**
     * usable measurement
     */
    protected final boolean usable;

    /**
     * last measured temperature
     */
    protected float lastMeasure;

    /**
     * time measured temperature
     */
    protected long timeMeasure;

    /**
     * time window temperature
     */
    protected long timeWindow;

    /**
     * last window temperature
     */
    protected float lastWindow;

    /**
     * last works status
     */
    protected boolean isWorking;

    /**
     * last tilted status
     */
    protected boolean isWindow;

    /**
     * last calculated value
     */
    protected int lastCalc;

    /**
     * create instance
     *
     * @param num my number
     * @param nam my name
     */
    protected temperData(int num, String nam) {
        myNum = num;
        usable = temperUtil.str2num(nam.substring(0, 1)) == 1;
        int i = nam.indexOf(";");
        myNam = nam.substring(1, i);
        myUrl = nam.substring(i + 1, nam.length());
        lastMeasure = 20;
        lastWindow = lastMeasure;
    }

    /**
     * get current measurement
     */
    protected synchronized void getValue() {
        try {
            URL testUrl = new URL(myUrl);
            URLConnection testConn = testUrl.openConnection();
            testConn.setConnectTimeout(5000);
            testConn.setReadTimeout(5000);
            BufferedReader testReader = new BufferedReader(new InputStreamReader(testConn.getInputStream()));
            String testLine = testReader.readLine();
            lastMeasure = Float.parseFloat(testLine);
            timeMeasure = temperUtil.getTime();
        } catch (Exception e) {
        }
    }

    /**
     * update window
     */
    protected void setWindow() {
        lastWindow = lastMeasure;
    }

    /**
     * calculate
     *
     * @param cl parent
     */
    protected void doCalc(temper cl) {
        isWindow = false;
        if (!usable) {
            isWorking = false;
            return;
        }
        isWorking = true;
        long tim = temperUtil.getTime();
        lastCalc = cl.currValue & 1;
        int old = lastCalc;
        if (cl.lastNeeded > (lastMeasure + cl.tempTol)) {
            lastCalc = 1;
        }
        if (cl.lastNeeded < (lastMeasure - cl.tempTol)) {
            lastCalc = 0;
        }
        if ((tim - timeMeasure) > cl.measTime) {
            isWorking = false;
            lastCalc = 0;
            return;
        }
        if (timeWindow > 0) {
            isWindow = true;
            lastCalc = 0;
            if (lastMeasure < (lastWindow - cl.windowTol)) {
                lastWindow = lastMeasure;
                timeWindow = tim;
            }
            if ((tim - timeWindow) < cl.windowMin) {
                return;
            }
            if (lastMeasure > (lastWindow + cl.windowTol)) {
                timeWindow = 0;
            }
            if ((tim - timeWindow) > cl.windowMax) {
                timeWindow = 0;
            }
            return;
        }
        if ((old == 1) && (lastMeasure < (lastWindow - cl.windowTol))) {
            isWindow = true;
            lastCalc = 0;
            timeWindow = tim;
            return;
        }
    }

    /**
     * get log line
     *
     * @return string
     */
    public String getLog() {
        return timeMeasure + ";" + timeWindow + ";" + lastWindow + ";" + lastMeasure + ";" + isWorking + ";" + isWindow;
    }

    /**
     * get web line
     *
     * @return string
     */
    public String getMeas() {
        String s = "";
        if (!isWorking) {
            s += ", discard";
        }
        if (isWindow) {
            s += ", window (" + lastWindow + " celsius, " + temperUtil.timePast(temperUtil.getTime(), timeWindow) + " ago)";
        }
        return "measure #" + myNum + "-" + myNam + ": " + lastMeasure + " celsius, " + temperUtil.timePast(temperUtil.getTime(), timeMeasure) + " ago" + s;
    }

}
