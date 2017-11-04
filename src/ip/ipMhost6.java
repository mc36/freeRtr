package ip;

import addr.addrIP;
import addr.addrIPv6;
import pack.packHolder;

/**
 * multicast listener discovery protocol (rfc3810) handler
 *
 * @author matecsaba
 */
public class ipMhost6 extends ipMhost {

    public String toString() {
        return "mld on " + lower;
    }

    public int getProtoNum() {
        return ipIcmp6.protoNum;
    }

    public boolean parsePacket(ipFwdIface rxIfc, packHolder pck) {
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
                    gotQuery(rxIfc, grp, null);
                    return false;
                }
                int cnt = pck.msbGetW(2); // number of sources
                pck.getSkip(4);
                for (int i = 0; i < cnt; i++) {
                    pck.getAddr(a6, 0); // source
                    pck.getSkip(addrIPv6.size);
                    addrIP adr = new addrIP();
                    adr.fromIPv6addr(a6);
                    gotQuery(rxIfc, grp, adr);
                }
                return false;
            case ipIcmp6.icmpMcastDone:
                gotReport(rxIfc, grp, null, false);
                return false;
            case ipIcmp6.icmpMcastRprt1:
                gotReport(rxIfc, grp, null, true);
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
                    for (int o = 0; o < src; o++) {
                        pck.getAddr(a6, 0); // source address
                        pck.getSkip(addrIPv6.size);
                        addrIP adr = new addrIP();
                        adr.fromIPv6addr(a6);
                        gotReport(rxIfc, grp, adr, (typ & 1) != 0);
                    }
                    pck.getSkip(aux);
                }
                return false;
            default:
                return true;
        }
    }

    private void updateHeader(ipFwdIface rxIfc, packHolder pck) {
        pck.IPdf = false;
        pck.IPttl = 255;
        pck.IPtos = 0;
        pck.IPsrc.setAddr(rxIfc.addr);
        pck.IPtrg.fromString("ff02::16");
        lower.icmpCore.createICMPheader(pck);
    }

    public void createQuery(ipFwdIface rxIfc, int tim, packHolder pck, addrIP grp, addrIP src) {
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
        updateHeader(rxIfc, pck);
    }

    public void createReport(ipFwdIface rxIfc, packHolder pck, addrIP grp, addrIP src, boolean need) {
        pck.putByte(0, need ? 5 : 6); // type
        pck.putByte(1, 0); // aux size
        pck.msbPutW(2, src == null ? 0 : 1); // source count
        pck.putSkip(4);
        pck.putAddr(0, grp.toIPv6());
        pck.putSkip(addrIPv6.size);
        if (src != null) {
            pck.putAddr(0, src.toIPv6());
            pck.putSkip(addrIPv6.size);
        }
        pck.merge2beg();
        pck.msbPutW(4, 0); // reserved
        pck.msbPutW(6, 1); // groups
        pck.ICMPtc = ipIcmp6.icmpMcastRprt2;
        updateHeader(rxIfc, pck);
    }

}
