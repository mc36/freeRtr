package org.freertr.addr;

import org.freertr.pack.packHolder;

/**
 * one bridge address
 *
 * @author matecsaba
 */
public class addrBridge implements Comparable<addrBridge> {

    /**
     * create instance
     */
    public addrBridge() {
    }

    /**
     * size of address
     */
    public final static int size = 8;

    /**
     * priority
     */
    public int pri;

    /**
     * address
     */
    public addrMac adr = new addrMac();

    public int compareTo(addrBridge o) {
        if (pri < o.pri) {
            return -1;
        }
        if (pri > o.pri) {
            return +1;
        }
        return adr.compareTo(o.adr);
    }

    /**
     * convert to string
     *
     * @return string
     */
    public String toString() {
        return pri + "/" + adr;
    }

    /**
     * clone this address
     *
     * @return new instance with the same content
     */
    public addrBridge copyBytes() {
        addrBridge n = new addrBridge();
        n.adr.setAddr(adr);
        n.pri = pri;
        return n;
    }

    /**
     * get from packet
     *
     * @param pck packet to read
     * @param ofs offset where from read
     */
    public void fromPack(packHolder pck, int ofs) {
        pri = pck.msbGetW(ofs + 0);
        pck.getAddr(adr, ofs + 2);
    }

    /**
     * put to packet
     *
     * @param pck packet to write
     * @param ofs offset where to write
     */
    public void toPack(packHolder pck, int ofs) {
        pck.msbPutW(ofs + 0, pri);
        pck.putAddr(ofs + 2, adr);
    }

    /**
     * convert bandwidth to cost
     *
     * @param bw bandwidth
     * @return cost
     */
    public static int bandwidth2cost(long bw) {
        if (bw > 9999999999L) {
            return 2;
        }
        if (bw > 1999999999L) {
            return 3;
        }
        if (bw > 999999999L) {
            return 4;
        }
        if (bw > 99999999L) {
            return 19;
        }
        if (bw > 15999999L) {
            return 62;
        }
        if (bw > 9999999L) {
            return 100;
        }
        if (bw > 3999999L) {
            return 250;
        }
        return 10000;
    }

}
