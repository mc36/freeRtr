package ip;

import addr.addrIP;
import addr.addrIPv4;
import pack.packHolder;

/**
 * internet group management protocol (rfc3376) handler
 *
 * @author matecsaba
 */
public class ipMhost4 extends ipMhost {

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
        return "igmp on " + lower;
    }

    /**
     * get protocol number
     *
     * @return number
     */
    public int getProtoNum() {
        return 2;
    }

    /**
     * parse header
     *
     * @param rxIfc interface
     * @param pck packet
     * @return false on success, true on error
     */
    public boolean parsePacket(ipFwdIface rxIfc, packHolder pck) {
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
                    gotQuery(rxIfc, grp, null);
                    return false;
                }
                int cnt = pck.msbGetW(2); // number of sources
                pck.getSkip(4);
                for (int i = 0; i < cnt; i++) {
                    pck.getAddr(a4, 0); // source
                    pck.getSkip(addrIPv4.size);
                    addrIP adr = new addrIP();
                    adr.fromIPv4addr(a4);
                    gotQuery(rxIfc, grp, adr);
                }
                return false;
            case typReport1:
            case typReport2:
                gotReport(rxIfc, grp, null, true);
                return false;
            case typLeave:
                gotReport(rxIfc, grp, null, false);
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
                    for (int o = 0; o < src; o++) {
                        pck.getAddr(a4, 0); // source address
                        pck.getSkip(addrIPv4.size);
                        addrIP adr = new addrIP();
                        adr.fromIPv4addr(a4);
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
        int siz = pck.headSize();
        pck.putSkip(-siz);
        pck.lsbPutW(2, 0xffff - pck.putIPsum(0, siz, 0)); // checksum
        pck.putSkip(siz);
        pck.merge2beg();
        pck.IPdf = false;
        pck.IPttl = 255;
        pck.IPtos = 0;
        pck.IPprt = getProtoNum();
        pck.IPsrc.setAddr(rxIfc.addr);
        pck.IPtrg.fromString("224.0.0.1");
    }

    /**
     * create query
     *
     * @param rxIfc interface
     * @param tim time
     * @param pck packet
     * @param grp group
     * @param src source
     */
    public void createQuery(ipFwdIface rxIfc, int tim, packHolder pck, addrIP grp, addrIP src) {
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
        updateHeader(rxIfc, pck);
    }

    /**
     * create report
     *
     * @param rxIfc interface
     * @param pck packet
     * @param grp group
     * @param src source
     * @param need needed
     */
    public void createReport(ipFwdIface rxIfc, packHolder pck, addrIP grp, addrIP src, boolean need) {
        pck.putByte(0, typReport3); // type
        pck.putByte(1, 0); // reserved
        pck.msbPutW(2, 0); // checksum
        pck.msbPutW(4, 0); // reserved
        pck.msbPutW(6, 1); // groups
        pck.putSkip(8);
        pck.putByte(0, need ? 5 : 6); // type
        pck.putByte(1, 0); // aux size
        pck.msbPutW(2, src == null ? 0 : 1); // source count
        pck.putSkip(4);
        pck.putAddr(0, grp.toIPv4());
        pck.putSkip(addrIPv4.size);
        if (src != null) {
            pck.putAddr(0, src.toIPv4());
            pck.putSkip(addrIPv4.size);
        }
        updateHeader(rxIfc, pck);
    }

}
