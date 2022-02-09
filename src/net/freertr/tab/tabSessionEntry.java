package net.freertr.tab;

import java.util.Comparator;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrMac;
import net.freertr.pack.packHolder;
import net.freertr.util.bits;

/**
 * session entry
 *
 * @author matecsaba
 */
public class tabSessionEntry implements Comparator<tabSessionEntry> {

    /**
     * source address
     */
    public addrIP srcAdr;

    /**
     * target address
     */
    public addrIP trgAdr;

    /**
     * ip protocol number
     */
    public int ipPrt;

    /**
     * ip tos number
     */
    public int ipTos;

    /**
     * ip flow number
     */
    public int ipFlw;

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
     * session direction, true=rx, false=tx
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

    public int compare(tabSessionEntry o1, tabSessionEntry o2) {
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

    /**
     * copy data
     *
     * @return copy
     */
    public tabSessionEntry copyBytes() {
        tabSessionEntry n = new tabSessionEntry(logMacs);
        n.srcAdr = srcAdr.copyBytes();
        n.trgAdr = trgAdr.copyBytes();
        n.ipPrt = ipPrt;
        n.ipTos = ipTos;
        n.ipFlw = ipFlw;
        n.srcPrt = srcPrt;
        n.trgPrt = trgPrt;
        n.rxByte = rxByte;
        n.txByte = txByte;
        n.rxPack = rxPack;
        n.txPack = txPack;
        n.startTime = startTime;
        n.lastTime = lastTime;
        n.dir = dir;
        n.logMacs = logMacs;
        if (srcMac != null) {
            n.srcMac = srcMac.copyBytes();
        }
        if (trgMac != null) {
            n.trgMac = trgMac.copyBytes();
        }
        return n;
    }

    /**
     * create entry from packet
     *
     * @param pck packet to use
     * @param macs log macs
     * @return entry
     */
    public static tabSessionEntry fromPack(packHolder pck, boolean macs) {
        tabSessionEntry ses = new tabSessionEntry(macs);
        ses.ipPrt = pck.IPprt;
        ses.ipTos = pck.IPtos;
        ses.ipFlw = pck.IPid;
        ses.srcPrt = pck.UDPsrc;
        ses.trgPrt = pck.UDPtrg;
        ses.srcAdr = pck.IPsrc.copyBytes();
        ses.trgAdr = pck.IPtrg.copyBytes();
        if (!macs) {
            return ses;
        }
        ses.srcMac = pck.ETHsrc.copyBytes();
        ses.trgMac = pck.ETHtrg.copyBytes();
        return ses;
    }

    /**
     * reverse this session direction
     *
     * @return reversed
     */
    public tabSessionEntry reverseDirection() {
        tabSessionEntry n = new tabSessionEntry(logMacs);
        n.dir = !dir;
        n.srcMac = trgMac;
        n.trgMac = srcMac;
        n.srcAdr = trgAdr;
        n.trgAdr = srcAdr;
        n.srcPrt = trgPrt;
        n.trgPrt = srcPrt;
        n.ipPrt = ipPrt;
        n.ipTos = ipTos;
        n.ipFlw = ipFlw;
        return n;
    }

    /**
     * reverse this session direction
     *
     * @return reversed
     */
    public tabSessionEntry reverseCounts() {
        tabSessionEntry n = new tabSessionEntry(logMacs);
        n.rxByte = txByte;
        n.txByte = rxByte;
        n.rxPack = txPack;
        n.txPack = rxPack;
        return n;
    }

    /**
     * clear counters
     */
    public void clearCounts() {
        rxByte = 0;
        txByte = 0;
        rxPack = 0;
        txPack = 0;
    }

    /**
     * add counters
     *
     * @param old where from
     */
    public void addCounts(tabSessionEntry old) {
        rxByte += old.rxByte;
        txByte += old.txByte;
        rxPack += old.rxPack;
        txPack += old.txPack;
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
        return dir ? "rx" : "tx";
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
        s = getDir() + "|" + ipPrt + "|" + ipTos + "|" + getSrc("|") + "|" + getTrg("|") + "|" + rxPack + "|" + txPack + "|" + rxByte + "|" + txByte + "|" + getDur();
        if (!logMacs) {
            return s;
        }
        s = s + "|" + srcMac + "|" + trgMac;
        return s;
    }

}
