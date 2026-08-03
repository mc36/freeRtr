
/**
 * utils
 *
 * @author matecsaba
 */
public class positionUtil {

    /**
     * convert string to number
     *
     * @param s string to convert
     * @return value of string, 0 if failed to convert
     */
    public static float str2num(String s) {
        float i = 0;
        s = s.trim();
        try {
            i = Float.parseFloat(s);
        } catch (Exception e) {
        }
        return i;
    }

    /**
     * convert to hex
     *
     * @param i number
     * @return converted
     */
    public static String toHex(int i) {
        String a = Integer.toString(i, 16);
        for (; a.length() < 4;) {
            a = "0" + a;
        }
        return a;
    }

    /**
     * signal to meters
     *
     * @param mhz frequency
     * @param dbm signal
     * @return meters
     */
    public static double signal2distance(float mhz, float dbm) {
        return Math.pow(10.0, (27.55 - (20.0 * Math.log10(mhz)) - dbm) / 20.0);
    }

    /**
     * trilaterate points
     *
     * @param xa x coordinate
     * @param ya y coordinate
     * @param xb x coordinate
     * @param yb y coordinate
     * @param xc x coordinate
     * @param yc y coordinate
     * @param ra distance
     * @param rb distance
     * @param rc distance
     * @return x,y coordinates
     */
    public static double[] trilateration(float xa, float ya, float xb, float yb, float xc, float yc, double ra, double rb, double rc) {
        double s = (Math.pow(xc, 2.0) - Math.pow(xb, 2.0) + Math.pow(yc, 2.0) - Math.pow(yb, 2.0) + Math.pow(rb, 2.0) - Math.pow(rc, 2.0)) / 2.0;
        double t = (Math.pow(xa, 2.0) - Math.pow(xb, 2.0) + Math.pow(ya, 2.0) - Math.pow(yb, 2.0) + Math.pow(rb, 2.0) - Math.pow(ra, 2.0)) / 2.0;
        double y = ((t * (xb - xc)) - (s * (xb - xa))) / (((ya - yb) * (xb - xc)) - ((yc - yb) * (xb - xa)));
        double x = ((y * (ya - yb)) - t) / (xb - xa);
        return new double[]{x, y};
    }

}
