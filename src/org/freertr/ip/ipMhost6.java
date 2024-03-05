package org.freertr.ip;

import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv6;
import org.freertr.pack.packHolder;

/**
 * multicast listener discovery protocol (rfc3810) handler
 *
 * @author matecsaba
 */
public class ipMhost6 extends ipMhost {

    /**
     * create instance
     */
    public ipMhost6() {
    }

    public String toString() {
        return "mld on " + fwdCore;
    }

    public int getProtoNum() {
        return ipIcmp6.protoNum;
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
        addrIPv6 a6 = new addrIPv6();
//  int tim = pck.msbGetW(-4); // time
        pck.getAddr(a6, 0); // group
        pck.getSkip(addrIPv6.size);
        addrIP grp;
        if (a6.isEmpty()) {
            grp = null;
        } else {
            grp = new addrIP();
            grp.fromIPv6addr(a6);
        }
        switch (pck.ICMPtc) {
            case ipIcmp6.icmpMcastQuery:
                if (pck.dataSize() < 1) {
                    hnd.mhostQuery(ifc, grp, null);
                    return false;
                }
                int cnt = pck.msbGetW(2); // number of sources
                pck.getSkip(4);
                for (int i = 0; i < cnt; i++) {
                    pck.getAddr(a6, 0); // source
                    pck.getSkip(addrIPv6.size);
                    addrIP adr = new addrIP();
                    adr.fromIPv6addr(a6);
                    hnd.mhostQuery(ifc, grp, adr);
                }
                return false;
            case ipIcmp6.icmpMcastDone:
                hnd.mhostReport(ifc, grp, null, false);
                return false;
            case ipIcmp6.icmpMcastRprt1:
                hnd.mhostReport(ifc, grp, null, true);
                return false;
            case ipIcmp6.icmpMcastRprt2:
                pck.getSkip(-addrIPv6.size);
                cnt = pck.msbGetW(-2); // number of groups
                for (int i = 0; i < cnt; i++) {
                    int typ = pck.getByte(0); // type
                    int aux = pck.getByte(1); // auxiliary data
                    int src = pck.msbGetW(2); // source count
                    pck.getSkip(4);
                    pck.getAddr(a6, 0); // group
                    pck.getSkip(addrIPv6.size);
                    grp.fromIPv6addr(a6);
                    if (src < 1) {
                        hnd.mhostReport(ifc, grp, null, (typ & 1) == 0);
                        pck.getSkip(aux);
                        continue;
                    }
                    for (int o = 0; o < src; o++) {
                        pck.getAddr(a6, 0); // source address
                        pck.getSkip(addrIPv6.size);
                        addrIP adr = new addrIP();
                        adr.fromIPv6addr(a6);
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
        pck.IPdf = false;
        pck.IPfrg = 0;
        pck.IPttl = 1;
        pck.IPtos = 0;
        pck.IPid = 0;
        pck.IPalrt = 0;
        pck.IPsrc.setAddr(rxIfc.lower.getLinkLocalAddr());
        if (query) {
            pck.IPtrg.fromString("ff02::1");
        } else {
            pck.IPtrg.fromString("ff02::16");
        }
    }

    public void createQuery(int tim, packHolder pck, addrIP grp, addrIP src) {
        if (grp == null) {
            grp = new addrIP();
        }
        pck.putAddr(0, grp.toIPv6()); // group
        pck.putSkip(addrIPv6.size);
        pck.putByte(0, 2); // robustness variable
        pck.putByte(1, time2byte(tim)); // querier interval
        pck.msbPutW(2, src == null ? 0 : 1); // source count
        pck.putSkip(4);
        if (src != null) {
            pck.putAddr(0, src.toIPv6());
            pck.putSkip(addrIPv6.size);
        }
        pck.merge2beg();
        pck.msbPutW(4, 1000); // max response code
        pck.msbPutW(6, 0); // reserved
        pck.ICMPtc = ipIcmp6.icmpMcastQuery;
    }

    public void createReport(packHolder pck, addrIP grp, addrIP source, boolean need) {
        addrIPv6 sa = null;
        if (source != null) {
            sa = source.toIPv6();
            if (sa.isEmpty()) {
                sa = null;
            }
        }
        pck.putByte(0, need ^ (sa == null) ? 1 : 2); // type
        pck.putByte(1, 0); // aux size
        pck.msbPutW(2, sa == null ? 0 : 1); // source count
        pck.putSkip(4);
        pck.putAddr(0, grp.toIPv6());
        pck.putSkip(addrIPv6.size);
        if (sa != null) {
            pck.putAddr(0, sa);
            pck.putSkip(addrIPv6.size);
        }
        pck.merge2beg();
        pck.msbPutW(4, 0); // reserved
        pck.msbPutW(6, 1); // groups
        pck.ICMPtc = ipIcmp6.icmpMcastRprt2;
    }

}
