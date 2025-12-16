
/**
 * neighbor entry
 *
 * @author matecsaba
 */
public class positionAddr implements Comparable<positionAddr> {

    /**
     * address
     */
    public final int addr1;

    /**
     * address
     */
    public final int addr2;

    /**
     * address
     */
    public final int addr3;

    /**
     * rate
     */
    public final float rate;

    /**
     * channel
     */
    public final float chan;

    /**
     * signal
     */
    public final float sign;

    /**
     * x coordinate
     */
    public double curX;

    /**
     * y coordinate
     */
    public double curY;
    
    /**
     * coordinates valid
     */
    public boolean good;

    /**
     * create instance
     *
     * @param a config
     */
    public positionAddr(String a) {
        a = a.replaceAll("  ", " ");
        a = a.replaceAll("  ", " ");
        a = a.trim();
        int i = a.indexOf(".");
        addr1 = Integer.parseInt(a.substring(0, i), 16);
        a = a.substring(i + 1, a.length());
        i = a.indexOf(".");
        addr2 = Integer.parseInt(a.substring(0, i), 16);
        a = a.substring(i + 1, a.length());
        i = a.indexOf(" ");
        addr3 = Integer.parseInt(a.substring(0, i), 16);
        a = a.substring(i + 1, a.length());
        i = a.indexOf(" ");
        rate = positionUtil.str2num(a.substring(0, i));
        a = a.substring(i + 1, a.length());
        i = a.indexOf(" ");
        chan = positionUtil.str2num(a.substring(0, i));
        a = a.substring(i + 1, a.length());
        i = a.indexOf(" ");
        sign = positionUtil.str2num(a.substring(0, i));
    }

    public int compareTo(positionAddr o) {
        if (addr1 < o.addr1) {
            return -1;
        }
        if (addr1 > o.addr1) {
            return +1;
        }
        if (addr2 < o.addr2) {
            return -1;
        }
        if (addr2 > o.addr2) {
            return +1;
        }
        if (addr3 < o.addr3) {
            return -1;
        }
        if (addr3 > o.addr3) {
            return +1;
        }
        return 0;
    }

    /**
     * get mac address
     *
     * @return string
     */
    public String getMac() {
        return positionUtil.toHex(addr1) + "." + positionUtil.toHex(addr2) + "." + positionUtil.toHex(addr3);
    }

}
