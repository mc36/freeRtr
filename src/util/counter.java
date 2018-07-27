package util;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import pack.packHolder;

/**
 * interface counters
 *
 * @author matecsaba
 */
public class counter implements Comparator<counter> {

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
     * last seen state
     */
    public state.states lastState = state.states.close;

    /**
     * creates new counter
     */
    public counter() {
        clear();
    }

    public int compare(counter o1, counter o2) {
        long v1 = o1.byteDr + o1.byteRx + o1.byteTx;
        long v2 = o2.byteDr + o2.byteRx + o2.byteTx;
        int i = Long.compare(v1, v2);
        if (i != 0) {
            return i;
        }
        v1 = o1.packDr + o1.packRx + o1.packTx;
        v2 = o2.packDr + o2.packRx + o2.packTx;
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
    }

    /**
     * update drop counters
     *
     * @param pck packet dropped
     * @param reason reason why dropped
     */
    public void drop(packHolder pck, reasons reason) {
        packDr++;
        byteDr += pck.dataSize();
        byteDr += pck.headSize();
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
        return res;
    }

    /**
     * convert counter to displayable text
     *
     * @param promisc promiscous state
     * @param macsec macsec state
     * @return string list of user string
     */
    public List<String> getShFull(boolean promisc, boolean macsec) {
        List<String> l = new ArrayList<String>();
        l.add(cmds.tabulator + "received " + packRx + " packets (" + byteRx + " bytes) dropped " + packDr + " packets (" + byteDr + " bytes)");
        l.add(cmds.tabulator + "transmitted " + packTx + " packets (" + byteTx + " bytes) promisc=" + promisc + " macsec=" + macsec);
        return l;
    }

    /**
     * get header
     *
     * @return header for details
     */
    public String getShHead() {
        return " (since " + bits.timePast(lastChgd) + ", " + stateChg + " changes)";
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
     * @return summary for table
     */
    public String getShBsum() {
        return byteTx + "|" + byteRx + "|" + byteDr;
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
            if (ntry.compare(ntry, cur) < 0) {
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
            if (ntry.compare(ntry, cur) > 0) {
                cur = ntry.copyBytes();
            }
        }
        return cur;
    }

}
