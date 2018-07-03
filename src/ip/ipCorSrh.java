package ip;

import addr.addrIP;
import pack.packHolder;
import util.debugger;
import util.logger;

/**
 * segment routing header
 *
 * @author matecsaba
 */
public class ipCorSrh {

    /**
     * protocol number
     */
    public final static int protoNum = 43;

    /**
     * routing type
     */
    public final static int rouType = 4;

    /**
     * parse header
     *
     * @param pck packet to read
     */
    public static void skipHeader(packHolder pck) {
        int prt = pck.getByte(pck.IPsiz + 0);
        int siz = (pck.getByte(pck.IPsiz + 1) + 1) << 3;
        if (pck.getByte(pck.IPsiz + 2) != rouType) {
            return;
        }
        pck.getAddr(pck.IPtrg, pck.IPsiz + 8);
        if (debugger.ipCorSrhTraf) {
            logger.debug("skip " + pck.IPsrc + " -> " + pck.IPtrg + " pr=" + prt + " siz=" + siz);
        }
        pck.IPsiz += siz;
        pck.IPprt = prt;
    }

    /**
     * parse header
     *
     * @param pck packet to read
     * @return 1=error, 2=local, 3=forward forward
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
            if (debugger.ipCorSrhTraf) {
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
        if (debugger.ipCorSrhTraf) {
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
        if (debugger.ipCorSrhTraf) {
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
