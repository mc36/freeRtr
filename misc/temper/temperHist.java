
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
        getWord(); // curr
        need = temperUtil.str2num(getWord());
        getWord(); // meas
        List<Float> res = new ArrayList<Float>();
        for (;line.length()>0;) {
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

}
