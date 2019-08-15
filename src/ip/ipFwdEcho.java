package ip;

import java.util.Comparator;

import util.notifier;
import addr.addrIP;
import pack.packHolder;

/**
 * stores one echo session
 *
 * @author matecsaba
 */
public class ipFwdEcho implements Comparator<ipFwdEcho> {

    /**
     * id number
     */
    public int echoNum;

    /**
     * notifier to use
     */
    public notifier notif;

    /**
     * source address
     */
    public addrIP src;

    /**
     * target address
     */
    public addrIP trg;

    /**
     * time when entry created
     */
    public long created;

    public int compare(ipFwdEcho o1, ipFwdEcho o2) {
        if (o1.echoNum < o2.echoNum) {
            return -1;
        }
        if (o1.echoNum > o2.echoNum) {
            return +1;
        }
        return 0;
    }

    /**
     * add mpls fields to reports
     *
     * @param pck packet to update
     */
    public static void addMplsFields(packHolder pck) {
        if (pck.MPLSlabel == 0) {
            return;
        }
        int i = 128 - pck.dataSize();
        if (i > 0) {
            pck.putFill(0, i, 0);
            pck.putSkip(i);
            pck.merge2end();
        }
        pck.msbPutD(0, 0x20000000); // version, chksum
        pck.msbPutD(4, 0x00080101); // length, class
        i = pck.MPLSlabel << 12;
        i |= pck.MPLSttl & 0xff;
        i |= (pck.MPLSexp & 7) << 9;
        i |= 0x100;
        pck.msbPutD(8, i);
        i = pck.getIPsum(0, pck.dataSize(), 0);
        i = pck.putIPsum(0, 8, i);
        pck.lsbPutW(2, 0xffff - i); // checksum
        pck.putSkip(12);
        pck.merge2end();
    }

}
