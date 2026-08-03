package org.freertr.util;

import java.util.ArrayList;
import java.util.List;
import org.freertr.cfg.cfgAll;
import org.freertr.pack.packHolder;

/**
 * interface counters
 *
 * @author matecsaba
 */
public class counter implements Comparable<counter> {

    /**
     * drop reason
     */
    public enum reasons {

        /**
         * bad ethernet type
         */
        badEthTyp,
        /**
         * no upper/lower interface
         */
        noIface,
        /**
         * line protocol no up
         */
        notUp,
        /**
         * bad vlan id
         */
        badVlan,
        /**
         * bad receive sequence number
         */
        badRxSeq,
        /**
         * bad transmit sequence number
         */
        badTxSeq,
        /**
         * bad command
         */
        badCmd,
        /**
         * bad type
         */
        badTyp,
        /**
         * bad time
         */
        badTim,
        /**
         * bad value
         */
        badVal,
        /**
         * bad code
         */
        badCod,
        /**
         * bad length
         */
        badLen,
        /**
         * bad address
         */
        badAddr,
        /**
         * bad source address
         */
        badSrcAddr,
        /**
         * bad target address
         */
        badTrgAddr,
        /**
         * bad source port
         */
        badSrcPort,
        /**
         * bad target port
         */
        badTrgPort,
        /**
         * bad network
         */
        badNet,
        /**
         * bad port
         */
        badPort,
        /**
         * bad checksum
         */
        badSum,
        /**
         * bad header
         */
        badHdr,
        /**
         * no route
         */
        noRoute,
        /**
         * not in table
         */
        notInTab,
        /**
         * fragmented/fragmentation needed
         */
        fragment,
        /**
         * reassembly exceeded
         */
        reassembly,
        /**
         * ttl exceeded
         */
        ttlExceed,
        /**
         * bad protocol id
         */
        badProto,
        /**
         * bad version number
         */
        badVer,
        /**
         * bad flag
         */
        badFlag,
        /**
         * bad key
         */
        badKey,
        /**
         * not allowed
         */
        denied,
        /**
         * no buffer
         */
        noBuffer,
        /**
         * bad option
         */
        badOpt,
        /**
         * bad size
         */
        badSiz,
        /**
         * too small
         */
        tooSmall,
        /**
         * too long
         */
        tooLong,
        /**
         * bad id
         */
        badID

    }

    /**
     * convert reason code to string
     *
     * @param reason reason code
     * @return string showing this code
     */
    public static String reason2string(reasons reason) {
        switch (reason) {
            case badEthTyp:
                return "bad ethtype";
            case noIface:
                return "no interface";
            case notUp:
                return "interface down";
            case badVlan:
                return "bad vlan id";
            case badRxSeq:
                return "bad rx seq";
            case badTxSeq:
                return "bad tx seq";
            case badCmd:
                return "bad command";
            case badTyp:
                return "bad type";
            case badTim:
                return "bad time";
            case badVal:
                return "bad value";
            case badLen:
                return "bad length";
            case badCod:
                return "bad code";
            case badAddr:
                return "bad address";
            case badSrcAddr:
                return "bad source address";
            case badTrgAddr:
                return "bad target address";
            case badSrcPort:
                return "bad source port ";
            case badTrgPort:
                return "bad target port";
            case badNet:
                return "bad network";
            case badPort:
                return "bad port";
            case badSum:
                return "bad checksum";
            case badHdr:
                return "bad header";
            case noRoute:
                return "no route";
            case notInTab:
                return "not in table";
            case fragment:
                return "fragmented";
            case ttlExceed:
                return "ttl exceed";
            case reassembly:
                return "reassembly";
            case badProto:
                return "bad protocol id";
            case badVer:
                return "bad version";
            case badFlag:
                return "bad flag";
            case badKey:
                return "bad key";
            case denied:
                return "not allowed";
            case noBuffer:
                return "no buffer";
            case badOpt:
                return "bad option";
            case badSiz:
                return "bad size";
            case tooSmall:
                return "too small";
            case tooLong:
                return "too long";
            case badID:
                return "bad id";
            default:
                return "unknown #" + reason;
        }
    }

    /**
     * packets received
     */
    public long packRx;

    /**
     * packets transmitted
     */
    public long packTx;

    /**
     * packets dropped
     */
    public long packDr;

    /**
     * bytes received
     */
    public long byteRx;

    /**
     * bytes transmitted
     */
    public long byteTx;

    /**
     * bytes dropped
     */
    public long byteDr;

    /**
     * line protocol transitioned
     */
    public int stateChg;

    /**
     * last time changed
     */
    public long lastChgd;

    /**
     * last rx
     */
    public long lastRx;

    /**
     * last tx
     */
    public long lastTx;

    /**
     * last drop
     */
    public long lastDr;

    /**
     * last seen state
     */
    public state.states lastState = state.states.close;

    /**
     * dropper to use
     */
    public counter dropper;

    /**
     * creates new counter
     */
    public counter() {
        clear();
    }

    public int compareTo(counter o) {
        long v1 = byteDr + byteRx + byteTx;
        long v2 = o.byteDr + o.byteRx + o.byteTx;
        int i = Long.compare(v1, v2);
        if (i != 0) {
            return i;
        }
        v1 = packDr + packRx + packTx;
        v2 = o.packDr + o.packRx + o.packTx;
        return Long.compare(v1, v2);
    }

    /**
     * update receive counters
     *
     * @param pck packet received
     */
    public void rx(packHolder pck) {
        packRx++;
        byteRx += pck.dataSize();
        lastRx = bits.getTime();
    }

    /**
     * update transmit counters
     *
     * @param pck packet is going to be sent
     */
    public void tx(packHolder pck) {
        packTx++;
        byteTx += pck.dataSize();
        byteTx += pck.headSize();
        lastTx = bits.getTime();
    }

    /**
     * update drop counters
     *
     * @param pck packet dropped
     * @param reason reason why dropped
     */
    public void drop(packHolder pck, reasons reason) {
        if (dropper != null) {
            dropper.packDr++;
            dropper.byteDr += pck.dataSize();
            dropper.byteDr += pck.headSize();
            dropper.lastDr = bits.getTime();
        }
        packDr++;
        byteDr += pck.dataSize();
        byteDr += pck.headSize();
        lastDr = bits.getTime();
        if (!debugger.counterTraf) {
            return;
        }
        logger.debug("dropping packet; reason=" + reason2string(reason) + "; packet=" + pck.dump());
    }

    /**
     * update interface state changes
     *
     * @param stat current state of line protocol
     */
    public void stateChange(state.states stat) {
        if (stat == lastState) {
            return;
        }
        lastState = stat;
        stateChg++;
        lastChgd = bits.getTime();
    }

    /**
     * percent values
     *
     * @param old percent this
     * @param pkt packets too
     * @param drp drops too
     * @return percent value
     */
    public int percent(counter old, boolean pkt, boolean drp) {
        int res = percent(old.byteRx, byteRx);
        res += percent(old.byteTx, byteTx);
        if (!pkt) {
            if (!drp) {
                return res / 2;
            }
            res += percent(old.byteDr, byteDr);
            return res / 3;
        }
        res += percent(old.packRx, packRx);
        res += percent(old.packTx, packTx);
        if (!drp) {
            return res / 4;
        }
        res += percent(old.packDr, packDr);
        res += percent(old.byteDr, byteDr);
        return res / 6;
    }

    private int percent(long prv, long cur) {
        if (prv < 1) {
            return 100;
        }
        if (cur < 1) {
            return 100;
        }
        if (cur > prv) {
            return (int) ((cur * 100) / prv);
        } else {
            return (int) ((prv * 100) / cur);
        }
    }

    /**
     * substract values
     *
     * @param old substract this
     * @return resulting values
     */
    public counter minus(counter old) {
        counter res = new counter();
        res.packRx = packRx - old.packRx;
        res.packTx = packTx - old.packTx;
        res.packDr = packDr - old.packDr;
        res.byteRx = byteRx - old.byteRx;
        res.byteTx = byteTx - old.byteTx;
        res.byteDr = byteDr - old.byteDr;
        res.stateChg = stateChg;
        res.lastChgd = lastChgd;
        res.lastRx = lastRx;
        res.lastTx = lastTx;
        res.lastDr = lastDr;
        return res;
    }

    /**
     * add values
     *
     * @param old add this
     * @return resulting values
     */
    public counter plus(counter old) {
        counter res = new counter();
        res.packRx = packRx + old.packRx;
        res.packTx = packTx + old.packTx;
        res.packDr = packDr + old.packDr;
        res.byteRx = byteRx + old.byteRx;
        res.byteTx = byteTx + old.byteTx;
        res.byteDr = byteDr + old.byteDr;
        res.stateChg = stateChg;
        res.lastChgd = lastChgd;
        res.lastRx = lastRx;
        res.lastTx = lastTx;
        res.lastDr = lastDr;
        return res;
    }

    /**
     * sum up rx and tx counters
     *
     * @param dr count drops too
     * @return summary
     */
    public counter sumUp(boolean dr) {
        counter res = new counter();
        res.packRx = packRx + packTx;
        res.byteRx = byteRx + byteTx;
        if (dr) {
            res.packRx += packDr;
            res.byteRx += byteDr;
        }
        res.stateChg = stateChg;
        res.lastChgd = lastChgd;
        res.lastRx = lastRx;
        res.lastTx = lastTx;
        res.lastDr = lastDr;
        return res;
    }

    /**
     * multiply by integer
     *
     * @param m integer
     * @return resulting values
     */
    public counter mul(int m) {
        counter res = new counter();
        res.packRx = packRx * m;
        res.packTx = packTx * m;
        res.packDr = packDr * m;
        res.byteRx = byteRx * m;
        res.byteTx = byteTx * m;
        res.byteDr = byteDr * m;
        res.stateChg = stateChg;
        res.lastChgd = lastChgd;
        res.lastRx = lastRx;
        res.lastTx = lastTx;
        res.lastDr = lastDr;
        return res;
    }

    /**
     * divide by integer
     *
     * @param d integer
     * @return resulting values
     */
    public counter div(int d) {
        counter res = new counter();
        res.packRx = packRx / d;
        res.packTx = packTx / d;
        res.packDr = packDr / d;
        res.byteRx = byteRx / d;
        res.byteTx = byteTx / d;
        res.byteDr = byteDr / d;
        res.stateChg = stateChg;
        res.lastChgd = lastChgd;
        res.lastRx = lastRx;
        res.lastTx = lastTx;
        res.lastDr = lastDr;
        return res;
    }

    private static long safeDiv(long a, long b) {
        if (b == 0) {
            return 0;
        }
        return a / b;
    }

    /**
     * divide bytes by packets
     *
     * @return resulting values
     */
    public counter bpRat() {
        counter res = new counter();
        res.byteTx = safeDiv(byteTx, packTx);
        res.byteRx = safeDiv(byteRx, packRx);
        res.byteDr = safeDiv(byteDr, packDr);
        res.stateChg = stateChg;
        res.lastChgd = lastChgd;
        res.lastRx = lastRx;
        res.lastTx = lastTx;
        res.lastDr = lastDr;
        return res;
    }

    /**
     * divide other by me
     *
     * @param oth other
     * @return resulting values
     */
    public counter othRat(counter oth) {
        if (oth == null) {
            return null;
        }
        counter res = new counter();
        res.byteTx = safeDiv(oth.byteTx, byteTx);
        res.byteRx = safeDiv(oth.byteRx, byteRx);
        res.byteDr = safeDiv(oth.byteDr, byteDr);
        res.packTx = safeDiv(oth.packTx, packTx);
        res.packRx = safeDiv(oth.packRx, packRx);
        res.packDr = safeDiv(oth.packDr, packDr);
        res.stateChg = stateChg;
        res.lastChgd = lastChgd;
        res.lastRx = lastRx;
        res.lastTx = lastTx;
        res.lastDr = lastDr;
        return res;
    }

    /**
     * clear all variables
     */
    public void clear() {
        packRx = 0;
        packTx = 0;
        byteRx = 0;
        byteTx = 0;
        packDr = 0;
        byteDr = 0;
        stateChg = 0;
        lastChgd = 0;
        lastRx = 0;
        lastTx = 0;
        lastDr = 0;
    }

    /**
     * copy values
     *
     * @return copied counter
     */
    public counter copyBytes() {
        counter res = new counter();
        res.packRx = packRx;
        res.packTx = packTx;
        res.byteRx = byteRx;
        res.byteTx = byteTx;
        res.packDr = packDr;
        res.byteDr = byteDr;
        res.stateChg = stateChg;
        res.lastChgd = lastChgd;
        res.lastRx = lastRx;
        res.lastTx = lastTx;
        res.lastDr = lastDr;
        return res;
    }

    /**
     * reverse values
     *
     * @return reversed counter
     */
    public counter reverse() {
        counter res = new counter();
        res.packRx = packTx;
        res.packTx = packRx;
        res.byteRx = byteTx;
        res.byteTx = byteRx;
        res.packDr = packDr;
        res.byteDr = byteDr;
        res.stateChg = stateChg;
        res.lastChgd = lastChgd;
        res.lastRx = lastRx;
        res.lastTx = lastTx;
        res.lastDr = lastDr;
        return res;
    }

    /**
     * convert counter to displayable text
     *
     * @param macsec macsec state
     * @param sgt sgt state
     * @return string list of user string
     */
    public List<String> getShFull(boolean macsec, boolean sgt) {
        List<String> l = new ArrayList<String>();
        l.add(cmds.tabulator + "received " + packRx + " packets (" + byteRx + " bytes) dropped " + packDr + " packets (" + byteDr + " bytes)");
        l.add(cmds.tabulator + "transmitted " + packTx + " packets (" + byteTx + " bytes) macsec=" + macsec + " sgt=" + sgt);
        return l;
    }

    /**
     * get header
     *
     * @return header for details
     */
    public String getShTrans() {
        return stateChg + " times, last at " + bits.time2str(cfgAll.timeZoneName, lastChgd + cfgAll.timeServerOffset, 3) + ", " + bits.timePast(lastChgd) + " ago";
    }

    /**
     * get header
     *
     * @return header for details
     */
    public String getShTraff() {
        return "input " + bits.timePast(lastRx) + " ago, output " + bits.timePast(lastTx) + " ago, drop " + bits.timePast(lastDr) + " ago";
    }

    /**
     * get statistics
     *
     * @return summary for table
     */
    public String getShStat() {
        return "tx=" + byteTx + "(" + packTx + ") rx=" + byteRx + "(" + packRx + ") drp=" + byteDr + "(" + packDr + ")";
    }

    /**
     * get statistics
     *
     * @param hwc hw counter
     * @param hws hw substract
     * @param wht what: 1=tx, 2=rx, 4=drop
     * @return summary for table
     */
    public String getShHwStat(counter hwc, counter hws, int wht) {
        String tb = "";
        String rb = "";
        String db = "";
        String tp = "";
        String rp = "";
        String dp = "";
        if (hwc != null) {
            if (hws != null) {
                hwc = hwc.minus(hws);
            }
            tb = "+" + hwc.byteTx;
            rb = "+" + hwc.byteRx;
            db = "+" + hwc.byteDr;
            tp = "+" + hwc.packTx;
            rp = "+" + hwc.packRx;
            dp = "+" + hwc.packDr;
        }
        String a = "";
        if ((wht & 1) != 0) {
            a += "|" + byteTx + tb + "|" + packTx + tp;
        }
        if ((wht & 2) != 0) {
            a += "|" + byteRx + rb + "|" + packRx + rp;
        }
        if ((wht & 4) != 0) {
            a += "|" + byteDr + db + "|" + packDr + dp;
        }
        return a;
    }

    /**
     * get statistics
     *
     * @param c counter to show
     * @return statistics
     */
    public static String getShStat(counter c) {
        if (c == null) {
            return null;
        }
        return c.getShStat();
    }

    /**
     * get byte summary
     *
     * @param c counter to show
     * @return summary for table
     */
    public static String getShBsum(counter c) {
        if (c == null) {
            return null;
        }
        return c.getShBsum();
    }

    /**
     * get byte summary
     *
     * @param c counter to show
     * @return summary for table
     */
    public static String getShPsum(counter c) {
        if (c == null) {
            return null;
        }
        return c.getShPsum();
    }

    /**
     * get byte summary
     *
     * @return summary for table
     */
    public String getShBsum() {
        return byteTx + "|" + byteRx + "|" + byteDr;
    }

    /**
     * get byte summary
     *
     * @param hw hw counter
     * @return summary for table
     */
    public String getShHwBsum(counter hw) {
        String t = "";
        String r = "";
        String d = "";
        if (hw != null) {
            t = "+" + hw.byteTx;
            r = "+" + hw.byteRx;
            d = "+" + hw.byteDr;
        }
        return byteTx + t + "|" + byteRx + r + "|" + byteDr + d;
    }

    /**
     * get packet summary
     *
     * @return summary for table
     */
    public String getShPsum() {
        return packTx + "|" + packRx + "|" + packDr;
    }

    /**
     * get packet summary
     *
     * @param hw hw counter
     * @return summary for table
     */
    public String getShHwPsum(counter hw) {
        String t = "";
        String r = "";
        String d = "";
        if (hw != null) {
            t = "+" + hw.packTx;
            r = "+" + hw.packRx;
            d = "+" + hw.packDr;
        }
        return packTx + t + "|" + packRx + r + "|" + packDr + d;
    }

    /**
     * calculate average
     *
     * @param lst list to scan
     * @return result
     */
    public static counter average(List<counter> lst) {
        counter cur = new counter();
        for (int i = lst.size() - 1; i >= 0; i--) {
            cur = cur.plus(lst.get(i));
        }
        return cur.div(lst.size());
    }

    /**
     * find minimum
     *
     * @param lst list to scan
     * @return result
     */
    public static counter minimum(List<counter> lst) {
        int i = lst.size() - 1;
        if (i < 0) {
            return new counter();
        }
        counter cur = lst.get(i).copyBytes();
        for (; i >= 0; i--) {
            counter ntry = lst.get(i);
            if (ntry.compareTo(cur) < 0) {
                cur = ntry.copyBytes();
            }
        }
        return cur;
    }

    /**
     * find maximum
     *
     * @param lst list to scan
     * @return result
     */
    public static counter maximum(List<counter> lst) {
        int i = lst.size() - 1;
        if (i < 0) {
            return new counter();
        }
        counter cur = lst.get(i).copyBytes();
        for (; i >= 0; i--) {
            counter ntry = lst.get(i);
            if (ntry.compareTo(cur) > 0) {
                cur = ntry.copyBytes();
            }
        }
        return cur;
    }

}
