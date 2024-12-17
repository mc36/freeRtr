
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.URI;
import java.net.URL;
import java.net.URLConnection;
import java.util.ArrayList;
import java.util.List;

/**
 * temperature reading
 *
 * @author matecsaba
 */
public class temperData {

    /**
     * result codes
     */
    public enum results {
        idle, cool, heat
    };

    /**
     * parent
     */
    protected final temper lower;

    /**
     * my number
     */
    protected final int myNum;

    /**
     * my priority
     */
    protected final int myPri;

    /**
     * my name
     */
    protected final String myNam;

    /**
     * my url
     */
    protected final String myUrl;

    /**
     * inside measurement
     */
    protected final boolean inside;

    /**
     * last measured temperature
     */
    protected float lastMeasure;

    /**
     * time measured temperature
     */
    protected long timeMeasure;

    /**
     * measurement history
     */
    protected List<temperHist> histDat;

    /**
     * history result
     */
    protected results histRes = results.idle;

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
     * exceptions happened
     */
    protected int errors;

    /**
     * images got
     */
    protected int fetches;

    /**
     * last calculated value
     */
    protected results lastCalc = results.idle;

    /**
     * create instance
     *
     * @param prnt parent
     * @param num my number
     * @param nam my name
     */
    protected temperData(temper prnt, int num, String nam) {
        lower = prnt;
        myNum = num;
        int i = nam.indexOf(";");
        inside = nam.substring(0, i).equals("in");
        nam = nam.substring(i + 1, nam.length());
        i = nam.indexOf(";");
        myPri = (int) temperUtil.str2num(nam.substring(0, i));
        nam = nam.substring(i + 1, nam.length());
        i = nam.indexOf(";");
        myNam = nam.substring(0, i);
        myUrl = nam.substring(i + 1, nam.length());
        lastMeasure = 20;
        lastWindow = lastMeasure;
        histDat = new ArrayList<temperHist>();
    }

    /**
     * get current measurement
     */
    protected synchronized void getValue() {
        try {
            URL testUrl = new URI(myUrl).toURL();
            URLConnection testConn = testUrl.openConnection();
            testConn.setConnectTimeout(lower.measIotm);
            testConn.setReadTimeout(lower.measIotm);
            BufferedReader testReader = new BufferedReader(new InputStreamReader(testConn.getInputStream()));
            String testLine = testReader.readLine();
            if (!testReader.readLine().equals("done")) {
                errors++;
                return;
            }
            lastMeasure = Float.parseFloat(testLine);
            timeMeasure = temperUtil.getTime();
            fetches++;
            temperHist ntry = new temperHist();
            ntry.placeMeas(timeMeasure, lastMeasure, lower.lastNeeded);
            histDat.add(ntry);
            for (; histDat.size() > lower.collHist;) {
                histDat.remove(0);
            }
        } catch (Exception e) {
            errors++;
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
     */
    protected void doCalc() {
        float tmpMin = Float.MAX_VALUE;
        float tmpMax = Float.MIN_VALUE;
        int posMin = 0;
        int posMax = 0;
        for (int i = 0; i < histDat.size(); i++) {
            Float v = histDat.get(i).meas[0];
            if (tmpMin > v) {
                tmpMin = v;
                posMin = i;
            }
            if (tmpMax < v) {
                tmpMax = v;
                posMax = i;
            }
        }
        if (posMin > posMax) {
            histRes = results.cool;
        } else {
            histRes = results.heat;
        }
        if ((tmpMax - tmpMin) < lower.collIlde) {
            histRes = results.idle;
        }
        long tim = temperUtil.getTime();
        isWorking = (tim - timeMeasure) < lower.measTime;
        if (!inside) {
            return;
        }
        if (!isWorking) {
            return;
        }
        if (myPri < 0) {
            return;
        }
        //lastCalc = results.idle;
        if (lower.lastNeeded > (lastMeasure + lower.heatTol)) {
            lastCalc = results.heat;
        }
        if (lower.lastNeeded < (lastMeasure - lower.coolTol)) {
            lastCalc = results.cool;
        }
        isWindow = false;
        if (lower.windowTol < 0) {
            return;
        }
        if (timeWindow > 0) {
            isWindow = true;
            lastCalc = results.idle;
            if ((tim - timeWindow) < lower.windowTim) {
                return;
            }
            timeWindow = 0;
            return;
        }
        switch (lastCalc) {
            case heat:
                isWindow = lastMeasure < (lastWindow - lower.windowTol);
                isWindow &= (lower.currValue & lower.heatPin) != 0;
                break;
            case cool:
                isWindow = lastMeasure > (lastWindow + lower.windowTol);
                isWindow &= (lower.currValue & lower.coolPin) != 0;
                break;
            default:
                break;
        }
        if (!isWindow) {
            return;
        }
        lastCalc = results.idle;
        timeWindow = tim;
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
        return "<tr><td>" + myNum + "</td><td>" + myNam + "</td><td>" + lastMeasure + "</td><td>" + temperUtil.timePast(temperUtil.getTime(), timeMeasure) + "</td><td>" + errors + "</td><td>" + fetches + "</td><td>" + isWorking + "</td><td>" + isWindow + "</td><td>" + lastCalc + "</td><td><a href=\"" + lower.url + "?temp=" + myNum + "&cmd=history\">" + histRes + "</a></td><td>" + lastWindow + "</td><td>" + temperUtil.timePast(temperUtil.getTime(), timeWindow) + "</td></tr>";
    }

}
