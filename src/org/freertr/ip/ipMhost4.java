package org.freertr.ip;

import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.pack.packHolder;

/**
 * internet group management protocol (rfc3376) handler
 *
 * @author matecsaba
 */
public class ipMhost4 extends ipMhost {

    /**
     * create instance
     */
    public ipMhost4() {
    }

    /**
     * protocol number
     */
    public final static int protoNum = 2;

    /**
     * group membership query, v1, v2, v3
     */
    public final static int typQuery = 0x11;

    /**
     * group membership report v1
     */
    public final static int typReport1 = 0x12;

    /**
     * group membership report v2
     */
    public final static int typReport2 = 0x16;

    /**
     * group membership report v3
     */
    public final static int typReport3 = 0x22;

    /**
     * group membership leave v2, v3
     */
    public final static int typLeave = 0x17;

    /**
     * convert to string
     *
     * @return string
     */
    public String toString() {
        return "igmp on " + fwdCore;
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
     * parse header
     *
     * @param hnd handler to use
     * @param ifc interface
     * @param pck packet
     * @return false on success, true on error
     */
    public static boolean parsePacket(ipMhostHndl hnd, Object ifc, packHolder pck) {
        if (pck.getIPsum(0, pck.dataSize(), 0) != 0xffff) {
            return true;
        }
        addrIPv4 a4 = new addrIPv4();
        int typ = pck.getByte(0); // type
//  int tim = pck.getByte(1); // time
        pck.getSkip(4);
        pck.getAddr(a4, 0); // group
        pck.getSkip(addrIPv4.size);
        addrIP grp;
        if (a4.isEmpty()) {
            grp = null;
        } else {
            grp = new addrIP();
            grp.fromIPv4addr(a4);
        }
        switch (typ) {
            case typQuery:
                if (pck.dataSize() < 1) {
                    hnd.mhostQuery(ifc, grp, null);
                    return false;
                }
                int cnt = pck.msbGetW(2); // number of sources
                pck.getSkip(4);
                for (int i = 0; i < cnt; i++) {
                    pck.getAddr(a4, 0); // source
                    pck.getSkip(addrIPv4.size);
                    addrIP adr = new addrIP();
                    adr.fromIPv4addr(a4);
                    hnd.mhostQuery(ifc, grp, adr);
                }
                return false;
            case typReport1:
            case typReport2:
                hnd.mhostReport(ifc, grp, null, true);
                return false;
            case typLeave:
                hnd.mhostReport(ifc, grp, null, false);
                return false;
            case typReport3:
                cnt = pck.msbGetW(-2); // number of groups
                for (int i = 0; i < cnt; i++) {
                    typ = pck.getByte(0); // type
                    int aux = pck.getByte(1); // auxiliary data
                    int src = pck.msbGetW(2); // source count
                    pck.getSkip(4);
                    pck.getAddr(a4, 0); // group
                    grp.fromIPv4addr(a4);
                    pck.getSkip(addrIPv4.size);
                    if (src < 1) {
                        hnd.mhostReport(ifc, grp, null, (typ & 1) == 0);
                        pck.getSkip(aux);
                        continue;
                    }
                    for (int o = 0; o < src; o++) {
                        pck.getAddr(a4, 0); // source address
                        pck.getSkip(addrIPv4.size);
                        addrIP adr = new addrIP();
                        adr.fromIPv4addr(a4);
                        hnd.mhostReport(ifc, grp, adr, (typ & 1) != 0);
                    }
                    pck.getSkip(aux);
                }
                return false;
            default:
                return true;
        }
    }

    public boolean parsePacket(ipFwdIface ifc, packHolder pck) {
        return parsePacket(this, ifc, pck);
    }

    public void updateHeader(ipFwdIface rxIfc, packHolder pck, boolean query) {
        int siz = pck.headSize();
        pck.putSkip(-siz);
        pck.lsbPutW(2, 0xffff - pck.putIPsum(0, siz, 0)); // checksum
        pck.putSkip(siz);
        pck.merge2beg();
        pck.IPdf = false;
        pck.IPfrg = 0;
        pck.IPttl = 1;
        pck.IPtos = 0;
        pck.IPid = 0;
        pck.IPalrt = 0;
        pck.IPprt = getProtoNum();
        pck.IPsrc.setAddr(rxIfc.addr);
        if (query) {
            pck.IPtrg.fromString("224.0.0.1");
        } else {
            pck.IPtrg.fromString("224.0.0.22");
        }
    }

    public void createQuery(int tim, packHolder pck, addrIP grp, addrIP src) {
        if (grp == null) {
            grp = new addrIP();
        }
        pck.putByte(0, typQuery); // type
        pck.putByte(1, 10); // max time
        pck.msbPutW(2, 0); // checksum
        pck.putSkip(4);
        pck.putAddr(0, grp.toIPv4());
        pck.putSkip(addrIPv4.size);
        pck.putByte(0, 2); // robustness variable
        pck.putByte(1, time2byte(tim)); // querier interval
        pck.msbPutW(2, src == null ? 0 : 1); // source count
        pck.putSkip(4);
        if (src != null) {
            pck.putAddr(0, src.toIPv4());
            pck.putSkip(addrIPv4.size);
        }
    }

    public void createReport(packHolder pck, addrIP grp, addrIP source, boolean need) {
        addrIPv4 sa = null;
        if (source != null) {
            sa = source.toIPv4();
            if (sa.isEmpty()) {
                sa = null;
            }
        }
        pck.putByte(0, typReport3); // type
        pck.putByte(1, 0); // reserved
        pck.msbPutW(2, 0); // checksum
        pck.msbPutW(4, 0); // reserved
        pck.msbPutW(6, 1); // groups
        pck.putSkip(8);
        pck.putByte(0, need ^ (sa == null) ? 1 : 2); // type
        pck.putByte(1, 0); // aux size
        pck.msbPutW(2, sa == null ? 0 : 1); // source count
        pck.putSkip(4);
        pck.putAddr(0, grp.toIPv4());
        pck.putSkip(addrIPv4.size);
        if (sa != null) {
            pck.putAddr(0, sa);
            pck.putSkip(addrIPv4.size);
        }
    }

}
