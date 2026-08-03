package org.freertr.rtr;

import org.freertr.addr.addrIP;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipPrt;
import org.freertr.pack.packHolder;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.state;

/**
 * segment routing header (rfc8754) interface
 *
 * @author matecsaba
 */
public class rtrSrhIface implements ipPrt {

    /**
     * protocol number
     */
    public final static int protoNum = 43;

    /**
     * routing type
     */
    public final static int rouType = 4;

    private final ipFwd fwdCore;

    private final ipFwdIface iface;

    private counter cntr = new counter();

    /**
     * create new instance
     *
     * @param fwd forwarder
     * @param ifc interface
     */
    public rtrSrhIface(ipFwd fwd, ipFwdIface ifc) {
        fwdCore = fwd;
        iface = ifc;
    }

    /**
     * unregister from ip
     */
    public void unregister2ip() {
        if (debugger.rtrSrhTraf) {
            logger.debug("unregister " + iface);
        }
        fwdCore.protoDel(this, iface, null);
    }

    /**
     * register to ip
     */
    public void register2ip() {
        if (debugger.rtrSrhTraf) {
            logger.debug("register " + iface);
        }
        fwdCore.protoAdd(this, iface, null);
    }

    public String toString() {
        return "srh";
    }

    /**
     * get protocol number
     *
     * @return number
     */
    public int getProtoNum() {
        return protoNum;
    }

    /**
     * close interface
     *
     * @param iface interface
     */
    public void closeUp(ipFwdIface iface) {
    }

    /**
     * get counter
     *
     * @return counter
     */
    public counter getCounter() {
        return cntr;
    }

    /**
     * set state
     *
     * @param iface interface
     * @param stat state
     */
    public void setState(ipFwdIface iface, state.states stat) {
    }

    /**
     * alert packet
     *
     * @param rxIfc interface
     * @param pck packet
     * @return false if success, true if error
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
    public void errorPack(counter.reasons err, addrIP rtr, ipFwdIface rxIfc, packHolder pck) {
        pck.getSkip(-pck.IPsiz);
        int siz = skipHeader(pck);
        if (siz < 0) {
            cntr.drop(pck, counter.reasons.badHdr);
            return;
        }
        pck.unMergeBytes(pck.IPsiz);
        pck.putSkip(-pck.IPsiz);
        pck.getSkip(siz);
        pck.putSkip(pck.IPsiz);
        pck.mergeHeader(-1, pck.headSize() - pck.IPsiz);
        fwdCore.updateIPheader(pck, pck.IPsrc, pck.IPtrg, pck.IPprt, pck.IPttl, pck.IPtos, pck.IPid, pck.dataSize() - pck.IPsiz);
        pck.IPsrc.setAddr(rtr);
        fwdCore.errorReport(err, rxIfc, pck);
    }

    /**
     * received packet
     *
     * @param rxIfc interface
     * @param pck packet
     */
    public void recvPack(ipFwdIface rxIfc, packHolder pck) {
        pck.getSkip(-pck.IPsiz);
        int res = parseHeader(pck);
        switch (res) {
            case 2:
                fwdCore.ifacePack(rxIfc, pck);
                return;
            case 1:
                cntr.drop(pck, counter.reasons.badHdr);
                return;
            default:
                break;
        }
        if ((fwdCore.mplsPropTtl | rxIfc.mplsPropTtlAlways) & rxIfc.mplsPropTtlAllow) {
            pck.IPttl = pck.MPLSttl;
        }
        fwdCore.updateIPheader(pck, pck.IPsrc, pck.IPtrg, pck.IPprt, pck.IPttl, pck.IPtos, pck.IPid, pck.dataSize() - pck.IPsiz);
        fwdCore.ifacePack(rxIfc, pck);
    }

    /**
     * parse header
     *
     * @param pck packet to read
     * @return size of header, -1 on error
     */
    public static int skipHeader(packHolder pck) {
        int prt = pck.getByte(pck.IPsiz + 0);
        int siz = (pck.getByte(pck.IPsiz + 1) + 1) << 3;
        if (pck.getByte(pck.IPsiz + 2) != rouType) {
            return -1;
        }
        pck.getAddr(pck.IPtrg, pck.IPsiz + 8);
        if (debugger.rtrSrhTraf) {
            logger.debug("skip " + pck.IPsrc + " -> " + pck.IPtrg + " pr=" + prt + " siz=" + siz);
        }
        pck.IPprt = prt;
        return siz;
    }

    /**
     * parse header
     *
     * @param pck packet to read
     * @return 1=error, 2=local, 3=forward
     */
    public static int parseHeader(packHolder pck) {
        int prt = pck.getByte(pck.IPsiz + 0);
        int siz = (pck.getByte(pck.IPsiz + 1) + 1) << 3;
        if (pck.dataSize() < siz) {
            return 1;
        }
        if (pck.getByte(pck.IPsiz + 2) != rouType) {
            return 1;
        }
        int lft = pck.getByte(pck.IPsiz + 3);
        if (lft == 0) {
            if (debugger.rtrSrhTraf) {
                logger.debug("rx " + pck.IPsrc + " -> " + pck.IPtrg + " pr=" + prt + " siz=" + siz);
            }
            pck.IPsiz += siz;
            pck.IPprt = prt;
            return 2;
        }
        if (((addrIP.size * lft) + 8) > siz) {
            return -1;
        }
        lft--;
        pck.getAddr(pck.IPtrg, pck.IPsiz + 8 + (addrIP.size * lft));
        if (debugger.rtrSrhTraf) {
            logger.debug("fwd " + pck.IPsrc + " -> " + pck.IPtrg + " pr=" + prt + " lft=" + lft);
        }
        int skp = pck.IPsiz + siz;
        pck.unMergeBytes(skp);
        pck.putSkip(-skp);
        pck.putByte(pck.IPsiz + 3, lft);
        if (lft < 1) {
            skp = pck.IPsiz;
            pck.IPprt = prt;
        }
        pck.putSkip(skp);
        pck.mergeHeader(-1, pck.headSize() - skp);
        return 3;
    }

    /**
     * parse header
     *
     * @param pck packet to send
     * @param hops intermediate hops
     */
    public static void createHeader(packHolder pck, addrIP[] hops) {
        pck.unMergeBytes(pck.IPsiz);
        pck.putSkip(-pck.IPsiz);
        int siz = (hops.length + 1) * addrIP.size + 8;
        if (debugger.rtrSrhTraf) {
            logger.debug("tx " + pck.IPsrc + " -> " + pck.IPtrg + " prt=" + pck.IPprt + " lft=" + (hops.length + 1) + " siz=" + pck.IPsiz + "+" + siz);
        }
        pck.putByte(pck.IPsiz + 0, pck.IPprt); // next header
        pck.putByte(pck.IPsiz + 1, (siz >>> 3) - 1); // header size
        pck.putByte(pck.IPsiz + 2, rouType); // routing type
        pck.putByte(pck.IPsiz + 3, hops.length); // segments left
        pck.putByte(pck.IPsiz + 4, hops.length); // last entry
        pck.putByte(pck.IPsiz + 5, 0); // flags
        pck.msbPutW(pck.IPsiz + 6, 0); // tag
        pck.putAddr(pck.IPsiz + 8, pck.IPtrg); // segment#0
        for (int i = 0; i < hops.length; i++) {
            pck.putAddr(pck.IPsiz + 8 + ((hops.length - i) * addrIP.size), hops[i]); // segment#n
        }
        siz += pck.IPsiz;
        pck.putSkip(siz);
        pck.mergeHeader(-1, pck.headSize() - siz);
        pck.IPprt = protoNum;
        if (hops.length > 0) {
            pck.IPtrg.setAddr(hops[0]);
        }
    }

}
