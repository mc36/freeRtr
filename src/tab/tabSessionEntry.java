package tab;

import addr.addrMac;
import addr.addrType;
import java.util.Comparator;
import util.bits;

/**
 * session entry
 *
 * @param <T> address type
 * @author matecsaba
 */
public class tabSessionEntry<T extends addrType> implements Comparator<tabSessionEntry<T>> {

    /**
     * source address
     */
    public T srcAdr;

    /**
     * target address
     */
    public T trgAdr;

    /**
     * ip protocol number
     */
    public int ipPrt;

    /**
     * source port
     */
    public int srcPrt;

    /**
     * target port
     */
    public int trgPrt;

    /**
     * received bytes
     */
    public long rxByte;

    /**
     * transmitted bytes
     */
    public long txByte;

    /**
     * received packets
     */
    public long rxPack;

    /**
     * transmitted packets
     */
    public long txPack;

    /**
     * session started
     */
    public long startTime;

    /**
     * session updated
     */
    public long lastTime;

    /**
     * session direction
     */
    public boolean dir;

    /**
     * log mac addresses
     */
    public boolean logMacs;

    /**
     * source mac
     */
    public addrMac srcMac;

    /**
     * target mac
     */
    public addrMac trgMac;

    /**
     * create new entry
     *
     * @param macs log mac addresses
     */
    public tabSessionEntry(boolean macs) {
        logMacs = macs;
    }

    public int compare(tabSessionEntry<T> o1, tabSessionEntry<T> o2) {
        if (o1.ipPrt < o2.ipPrt) {
            return -1;
        }
        if (o1.ipPrt > o2.ipPrt) {
            return +1;
        }
        if (o1.srcPrt < o2.srcPrt) {
            return -1;
        }
        if (o1.srcPrt > o2.srcPrt) {
            return +1;
        }
        if (o1.trgPrt < o2.trgPrt) {
            return -1;
        }
        if (o1.trgPrt > o2.trgPrt) {
            return +1;
        }
        int i = o1.srcAdr.compare(o1.srcAdr, o2.srcAdr);
        if (i != 0) {
            return i;
        }
        return o1.trgAdr.compare(o1.trgAdr, o2.trgAdr);
    }

    private String getSrc(String sep) {
        return trgAdr + sep + trgPrt;
    }

    private String getTrg(String sep) {
        return srcAdr + sep + srcPrt;
    }

    private String getDur() {
        return bits.timePast(startTime);
    }

    private String getDir() {
        return dir ? "tx" : "rx";
    }

    public String toString() {
        String s;
        s = getDir() + " " + ipPrt + " " + getSrc(" ") + " -> " + getTrg(" ") + " (" + rxByte + "/" + txByte + "/" + getDur() + ")";
        if (!logMacs) {
            return s;
        }
        s = s + " (" + srcMac + "->" + trgMac + ")";
        return s;
    }

    /**
     * dump one session
     *
     * @return session information
     */
    public String dump() {
        String s;
        s = getDir() + "|" + ipPrt + "|" + getSrc("|") + "|" + getTrg("|") + "|" + rxByte + "|" + txByte + "|" + getDur();
        if (!logMacs) {
            return s;
        }
        s = s + "|" + srcMac + "|" + trgMac;
        return s;
    }

}
