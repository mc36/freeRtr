package org.freertr.tab;

import org.freertr.addr.addrIP;
import org.freertr.addr.addrMac;
import org.freertr.pack.packHolder;
import org.freertr.util.bits;
import org.freertr.util.counter;

/**
 * session entry
 *
 * @author matecsaba
 */
public class tabSessionEntry implements Comparable<tabSessionEntry> {

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
     * counter
     */
    public counter cntr;

    /**
     * hardware counter
     */
    public counter hwCntr;

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
     * still evaluating headers
     */
    public packHolder evaluating;

    /**
     * url matched
     */
    public String sawUrl;

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

    public int compareTo(tabSessionEntry o) {
        if (ipPrt < o.ipPrt) {
            return -1;
        }
        if (ipPrt > o.ipPrt) {
            return +1;
        }
        if (srcPrt < o.srcPrt) {
            return -1;
        }
        if (srcPrt > o.srcPrt) {
            return +1;
        }
        if (trgPrt < o.trgPrt) {
            return -1;
        }
        if (trgPrt > o.trgPrt) {
            return +1;
        }
        int i = srcAdr.compareTo(o.srcAdr);
        if (i != 0) {
            return i;
        }
        return trgAdr.compareTo(o.trgAdr);
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
        n.cntr = cntr.copyBytes();
        if (hwCntr != null) {
            n.hwCntr = hwCntr.copyBytes();
        }
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
        ses.cntr = new counter();
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
        n.cntr = cntr;
        n.hwCntr = hwCntr;
        return n;
    }

    /**
     * reverse this session direction
     *
     * @return reversed
     */
    public tabSessionEntry reverseCounts() {
        tabSessionEntry n = new tabSessionEntry(logMacs);
        n.cntr = cntr.reverse();
        if (hwCntr != null) {
            n.hwCntr = hwCntr.reverse();
        }
        return n;
    }

    /**
     * clear counters
     */
    public void clearCounts() {
        cntr = new counter();
        if (hwCntr != null) {
            hwCntr = new counter();
        }
    }

    /**
     * add counters
     *
     * @param old where from
     */
    public void addCounts(tabSessionEntry old) {
        cntr = cntr.plus(old.cntr);
        if (old.hwCntr != null) {
            if (hwCntr == null) {
                hwCntr = new counter();
            }
            hwCntr = hwCntr.plus(old.hwCntr);
        }
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
        String hr = "";
        String ht = "";
        if (hwCntr != null) {
            hr = "+" + hwCntr.byteRx;
            ht = "+" + hwCntr.byteTx;
        }
        s = getDir() + " " + ipPrt + " " + getSrc(" ") + " -> " + getTrg(" ") + " " + sawUrl + " " + cntr.byteRx + hr + "/" + cntr.byteTx + ht + "/" + getDur();
        if (!logMacs) {
            return s;
        }
        s = s + " " + srcMac + " -> " + trgMac;
        return s;
    }

    /**
     * dump one session
     *
     * @return session information
     */
    public String dump() {
        String s;
        String hpr = "";
        String hpt = "";
        String hbr = "";
        String hbt = "";
        if (hwCntr != null) {
            hpr = "+" + hwCntr.packRx;
            hpt = "+" + hwCntr.packTx;
            hbr = "+" + hwCntr.byteRx;
            hbt = "+" + hwCntr.byteTx;
        }
        s = getDir() + "|" + ipPrt + "|" + ipTos + "|" + getSrc("|") + "|" + getTrg("|") + "|" + sawUrl + "|" + cntr.packRx + hpr + "|" + cntr.packTx + hpt + "|" + cntr.byteRx + hbr + "|" + cntr.byteTx + hbt + "|" + getDur();
        if (!logMacs) {
            return s;
        }
        s = s + "|" + srcMac + "|" + trgMac;
        return s;
    }

}
