
import java.util.ArrayList;
import java.util.List;

/**
 * history
 *
 * @author matecsaba
 */
public class temperHist {

    /**
     * time
     */
    protected float time;

    /**
     * needed
     */
    protected float need;

    /**
     * measures
     */
    protected float[] meas;

    /**
     * current value
     */
    protected int curr;

    private String line;

    public String toString() {
        String a = time + ";" + need;
        for (int i = 0; i < meas.length; i++) {
            a += ";" + meas[i];
        }
        return a;
    }

    /**
     * get word
     *
     * @return word
     */
    protected String getWord() {
        int i = line.indexOf(";");
        if (i < 0) {
            String a = line;
            line = "";
            return a;
        }
        String a = line.substring(0, i);
        line = line.substring(i + 1, line.length());
        return a;
    }

    /**
     * parse line
     *
     * @param s line to read
     */
    protected void parseLine(String s) {
        line = s;
        time = temperUtil.str2num(getWord());
        getWord(); // who
        curr = (int) temperUtil.str2num(getWord());
        need = temperUtil.str2num(getWord());
        getWord(); // meas in
        getWord(); // meas out
        List<Float> res = new ArrayList<Float>();
        for (; line.length() > 0;) {
            getWord(); // time
            getWord(); // wintime
            getWord(); // win
            res.add(temperUtil.str2num(getWord()));
            getWord(); // work
            getWord(); // win
        }
        meas = new float[res.size()];
        for (int i = 0; i < meas.length; i++) {
            meas[i] = res.get(i);
        }
        line = null;
    }

    /**
     * place measurement
     *
     * @param t time
     * @param v value
     * @param n needed
     */
    protected void placeMeas(long t, float v, float n) {
        meas = new float[1];
        meas[0] = v;
        time = t;
        need = n;
        curr = 0;
    }

}
