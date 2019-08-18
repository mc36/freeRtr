package ip;

import addr.addrIP;
import pack.packHolder;
import util.counter;
import util.counter.reasons;
import util.debugger;
import util.logger;
import util.state.states;

/**
 * multicast host (igmp/mld) handler
 *
 * @author matecsaba
 */
public abstract class ipMhost implements ipPrt {

    /**
     * forwarder
     */
    protected ipFwd lower;

    private counter cntr = new counter();

    /**
     * set ip forwarder
     *
     * @param ifw forwarder to use
     * @param icc icmp to use
     */
    public void setForwarder(ipFwd ifw, ipIcmp icc) {
        lower = ifw;
        if (icc.getProtoNum() == getProtoNum()) {
            return;
        }
        ifw.protoAdd(this, null, null);
    }

    /**
     * time to byte
     *
     * @param tim timme
     * @return byte
     */
    public static int time2byte(int tim) {
        if (tim < 128) {
            return tim;
        }
        for (int i = 128; i < 256; i++) {
            if (byte2time(i) > tim) {
                return i;
            }
        }
        return 255;
    }

    /**
     * time to word
     *
     * @param tim time
     * @return word
     */
    public static int time2word(int tim) {
        if (tim < 32768) {
            return tim;
        }
        for (int i = 32768; i < 65536; i++) {
            if (word2time(i) > tim) {
                return i;
            }
        }
        return 65535;
    }

    /**
     * byte to time
     *
     * @param byt byte
     * @return time
     */
    public static int byte2time(int byt) {
        if (byt < 128) {
            return byt;
        }
        return ((byt & 0xf) | 0x10) << (((byt >>> 4) & 7) + 3);
    }

    /**
     * word to time
     *
     * @param wrd word
     * @return time
     */
    public static int word2time(int wrd) {
        if (wrd < 32768) {
            return wrd;
        }
        return ((wrd & 0xfff) | 0x1000) << (((wrd >>> 12) & 7) + 3);
    }

    /**
     * close interface
     *
     * @param iface interface
     */
    public void closeUp(ipFwdIface iface) {
    }

    /**
     * set state
     *
     * @param iface interface
     * @param stat state
     */
    public void setState(ipFwdIface iface, states stat) {
    }

    /**
     * received packet
     *
     * @param rxIfc interface
     * @param pck packet
     */
    public void recvPack(ipFwdIface rxIfc, packHolder pck) {
        cntr.rx(pck);
        if (rxIfc.mhostCfg == null) {
            cntr.drop(pck, reasons.denied);
            return;
        }
        if (parsePacket(rxIfc, pck)) {
            cntr.drop(pck, reasons.badHdr);
            return;
        }
    }

    /**
     * alert packet
     *
     * @param rxIfc interface
     * @param pck packet
     * @return false on success, true on error
     */
    public boolean alertPack(ipFwdIface rxIfc, packHolder pck) {
        return true;
    }

    /**
     * error packet
     *
     * @param err error code
     * @param rtr address
     * @param rxIfc interface
     * @param pck packet
     */
    public void errorPack(reasons err, addrIP rtr, ipFwdIface rxIfc, packHolder pck) {
        return;
    }

    public counter getCounter() {
        return cntr;
    }

    /**
     * parse packet
     *
     * @param rxIfc receiving interface
     * @param pck packet to parse
     * @return true on error, false on success
     */
    public abstract boolean parsePacket(ipFwdIface rxIfc, packHolder pck);

    /**
     * create packet
     *
     * @param rxIfc receiving interface
     * @param tim query interval
     * @param pck packet to use
     * @param grp group queryed, null if generic
     * @param src source, null if generic
     */
    public abstract void createQuery(ipFwdIface rxIfc, int tim, packHolder pck, addrIP grp, addrIP src);

    /**
     * create report
     *
     * @param rxIfc receiving interface
     * @param pck packet to use
     * @param grp group reported, null if generic
     * @param src source, null if generic
     * @param need true if report, false if leaved
     */
    public abstract void createReport(ipFwdIface rxIfc, packHolder pck, addrIP grp, addrIP src, boolean need);

    /**
     * process query message
     *
     * @param rxIfc receiving interface
     * @param grp group queryed, null if generic
     * @param src source, null if generic
     */
    protected void gotQuery(ipFwdIface rxIfc, addrIP grp, addrIP src) {
        if (debugger.ipMhostTraf) {
            logger.debug("rx query src=" + src + " grp=" + grp + " ifc=" + rxIfc);
        }
        if (src == null) {
            return;
        }
        if (grp == null) {
            return;
        }
    }

    /**
     * process report message
     *
     * @param rxIfc receiving interface
     * @param grp group reported, null if generic
     * @param src source, null if generic
     * @param need true if report, false if leaved
     */
    protected void gotReport(ipFwdIface rxIfc, addrIP grp, addrIP src, boolean need) {
        if (debugger.ipMhostTraf) {
            logger.debug("rx report need=" + need + " src=" + src + " grp=" + grp);
        }
        if (src == null) {
            return;
        }
        if (grp == null) {
            return;
        }
        if (need) {
            lower.mcastAddFloodIfc(grp, src, rxIfc, rxIfc.mhostCfg.queryInterval * 3);
        } else {
            lower.mcastDelFloodIfc(grp, src, rxIfc);
        }
    }

    /**
     * create packet
     *
     * @param rxIfc receiving interface
     * @param tim query interval
     * @param grp group queryed, null if generic
     * @param src source, null if generic
     */
    public void sendQuery(ipFwdIface rxIfc, int tim, addrIP grp, addrIP src) {
        if (debugger.ipMhostTraf) {
            logger.debug("tx query src=" + src + " grp=" + grp);
        }
        packHolder pck = new packHolder(true, true);
        createQuery(rxIfc, tim, pck, grp, src);
        lower.protoPack(rxIfc, pck);
    }

    /**
     * create report
     *
     * @param rxIfc receiving interface
     * @param grp group reported, null if generic
     * @param src source, null if generic
     * @param need true if report, false if leaved
     */
    public void sendReport(ipFwdIface rxIfc, addrIP grp, addrIP src, boolean need) {
        if (debugger.ipMhostTraf) {
            logger.debug("tx report need=" + need + " src=" + src + " grp=" + grp);
        }
        packHolder pck = new packHolder(true, true);
        createReport(rxIfc, pck, grp, src, need);
        lower.protoPack(rxIfc, pck);
    }

}
