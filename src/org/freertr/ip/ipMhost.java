package org.freertr.ip;

import org.freertr.addr.addrIP;
import org.freertr.pack.packHolder;
import org.freertr.util.counter;
import org.freertr.util.counter.reasons;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.state.states;

/**
 * multicast host (igmp/mld) handler
 *
 * @author matecsaba
 */
public abstract class ipMhost implements ipPrt, ipMhostHndl {

    /**
     * create instance
     */
    public ipMhost() {
    }

    /**
     * forwarder
     */
    protected ipFwd fwdCore;

    /**
     * icmp
     */
    protected ipIcmp icmpCore;

    /**
     * uses icmp
     */
    protected boolean usesIcmp;

    private counter cntr = new counter();

    /**
     * set ip forwarder
     *
     * @param ifw forwarder to use
     * @param icc icmp to use
     */
    public void setForwarder(ipFwd ifw, ipIcmp icc) {
        fwdCore = ifw;
        icmpCore = icc;
        usesIcmp = icc.getProtoNum() == getProtoNum();
        if (usesIcmp) {
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
     * create header
     *
     * @param rxIfc receiving interface
     * @param pck packet to parse
     * @param query whether query or report
     */
    public abstract void updateHeader(ipFwdIface rxIfc, packHolder pck, boolean query);

    /**
     * create query
     *
     * @param tim query interval
     * @param pck packet to use
     * @param grp group queryed, null if generic
     * @param src source, null if generic
     */
    public abstract void createQuery(int tim, packHolder pck, addrIP grp, addrIP src);

    /**
     * create report
     *
     * @param pck packet to use
     * @param grp group reported, null if generic
     * @param src source, null if generic
     * @param need true if report, false if leaved
     */
    public abstract void createReport(packHolder pck, addrIP grp, addrIP src, boolean need);

    public void mhostQuery(Object ifc, addrIP grp, addrIP src) {
        if (debugger.ipMhostTraf) {
            logger.debug("rx query src=" + src + " grp=" + grp + " ifc=" + ifc);
        }
        if (src == null) {
            return;
        }
        if (grp == null) {
            return;
        }
    }

    public void mhostReport(Object ifc, addrIP grp, addrIP src, boolean need) {
        if (debugger.ipMhostTraf) {
            logger.debug("rx report need=" + need + " src=" + src + " grp=" + grp + " ifc=" + ifc);
        }
        ipFwdIface rxIfc = (ipFwdIface) ifc;
        if (rxIfc.mcastSrcIn != null) {
            src = rxIfc.mcastSrcIn.copyBytes();
        }
        if (src == null) {
            return;
        }
        if (grp == null) {
            return;
        }
        if (need) {
            fwdCore.mcastAddFloodIfc(grp, src, rxIfc, rxIfc.mhostCfg.queryInterval * 3);
        } else {
            fwdCore.mcastDelFloodIfc(grp, src, rxIfc);
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
        createQuery(tim, pck, grp, src);
        updateHeader(rxIfc, pck, true);
        if (usesIcmp) {
            icmpCore.createICMPheader(pck);
        }
        fwdCore.protoPack(rxIfc, null, pck);
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
        if (rxIfc.mcastSrcOut != null) {
            src = rxIfc.mcastSrcOut.copyBytes();
        }
        packHolder pck = new packHolder(true, true);
        createReport(pck, grp, src, need);
        updateHeader(rxIfc, pck, false);
        if (usesIcmp) {
            icmpCore.createICMPheader(pck);
        }
        fwdCore.protoPack(rxIfc, null, pck);
    }

}
