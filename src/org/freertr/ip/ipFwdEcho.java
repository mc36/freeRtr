package org.freertr.ip;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.pack.packHolder;
import org.freertr.util.notifier;

/**
 * stores one echo session
 *
 * @author matecsaba
 */
public class ipFwdEcho implements Comparable<ipFwdEcho> {

    /**
     * create instance
     */
    public ipFwdEcho() {
    }

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
     * accept multiple responses
     */
    public boolean multi;

    /**
     * time when entry created
     */
    public long created;

    /**
     * reports
     */
    public final List<ipFwdEchod> res = new ArrayList<ipFwdEchod>();

    public int compareTo(ipFwdEcho o) {
        if (echoNum < o.echoNum) {
            return -1;
        }
        if (echoNum > o.echoNum) {
            return +1;
        }
        return 0;
    }

    /**
     * get mpls fields to reports
     *
     * @param pck packet to parse
     * @return label, -1 on error
     */
    public static int getMplsExt(packHolder pck) {
        int i = getMplsExt(pck, 12);
        if (i >= 0) {
            return i;
        }
        i = getMplsExt(pck, 16);
        if (i >= 0) {
            return i;
        }
        i = getMplsExt(pck, 20);
        if (i >= 0) {
            return i;
        }
        return getMplsExt(pck, 24);
    }

    private static int getMplsExt(packHolder pck, int len) {
        int i = pck.dataSize() - len;
        if (i < 0) {
            return -1;
        }
        if (pck.msbGetW(i + 0) != 0x2000) { // version
            return -1;
        }
        if (pck.msbGetW(i + 4) != (len - 4)) { // length
            return -1;
        }
        if (pck.msbGetW(i + 6) != 0x0101) { // class
            return -1;
        }
        if (pck.getIPsum(i, len, 0) != 0xffff) {
            return -1;
        }
        int lab = pck.msbGetD(i + 8) >>> 12;
        pck.setDataSize(i);
        return lab;
    }

    /**
     * add mpls fields to reports
     *
     * @param pck packet to update
     */
    public static void addMplsExt(packHolder pck) {
        if (pck.MPLSlabel < ipMpls.labelExt) {
            return;
        }
        int i = 8 - (pck.dataSize() % 8);
        if (i > 0) {
            pck.putFill(0, i, 0);
            pck.putSkip(i);
            pck.merge2end();
        }
        i = 128 - pck.dataSize();
        if (i > 0) {
            pck.putFill(0, i, 0);
            pck.putSkip(i);
            pck.merge2end();
        }
        pck.msbPutW(0, 0x2000); // version
        pck.msbPutW(2, 0x0000); // chksum
        pck.msbPutW(4, 0x0008); // length
        pck.msbPutW(6, 0x0101); // class
        i = pck.MPLSlabel << 12;
        i |= pck.MPLSttl & 0xff;
        i |= (pck.MPLSexp & 7) << 9;
        if (pck.MPLSbottom) {
            i |= 0x100;
        }
        pck.msbPutD(8, i);
        i = pck.putIPsum(0, 12, 0);
        pck.lsbPutW(2, 0xffff - i); // checksum
        pck.putSkip(12);
        pck.merge2end();
    }

}
