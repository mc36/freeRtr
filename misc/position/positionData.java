
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.URI;
import java.net.URL;
import java.net.URLConnection;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * neighbor reading
 *
 * @author matecsaba
 */
public class positionData {

    /**
     * x coordinate
     */
    public final float myX;

    /**
     * y coordinate
     */
    public final float myY;

    /**
     * url coordinate
     */
    public final String myUrl;

    /**
     * last reading
     */
    public List<positionAddr> data = new ArrayList<positionAddr>();

    /**
     * create instance
     *
     * @param a config
     */
    public positionData(String a) {
        int i = a.indexOf(";");
        myX = positionUtil.str2num(a.substring(0, i));
        a = a.substring(i + 1, a.length());
        i = a.indexOf(";");
        myY = positionUtil.str2num(a.substring(0, i));
        myUrl = a.substring(i + 1, a.length());
    }

    /**
     * get current measurement
     */
    protected synchronized void getNeighs() {
        try {
            data = new ArrayList<positionAddr>();
            URL testUrl = new URI(myUrl).toURL();
            URLConnection testConn = testUrl.openConnection();
            testConn.setConnectTimeout(5000);
            testConn.setReadTimeout(5000);
            BufferedReader testReader = new BufferedReader(new InputStreamReader(testConn.getInputStream()));
            for (;;) {
                String a = testReader.readLine();
                if (a == null) {
                    break;
                }
                try {
                    positionAddr v = new positionAddr(a);
                    if (v.sign > -1) {
                        continue;
                    }
                    data.add(v);
                } catch (Exception e) {
                }
            }
            Collections.sort(data);
        } catch (Exception e) {
        }
    }

}
