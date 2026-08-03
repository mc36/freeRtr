package org.freertr.rtr;

import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrType;
import org.freertr.util.bits;

/**
 * ospfv3 spf address
 *
 * @author matecsaba
 */
public class rtrOspf6areaSpf extends addrType {

    /**
     * size of address
     */
    public final static int size = addrIPv4.size + addrIPv4.size;

    /**
     * node address
     */
    private addrIPv4 adr;

    /**
     * link id
     */
    private int lnk;

    /**
     * create instance
     *
     * @param a node
     * @param l link
     */
    public rtrOspf6areaSpf(addrIPv4 a, int l) {
        adr = a;
        lnk = l;
        setUp();
    }

    private void setUp() {
        adr.toBuffer(addr, 0);
        bits.msbPutD(addr, addrIPv4.size, lnk);
    }

    public int getSize() {
        return size;
    }

    public rtrOspf6areaSpf copyBytes() {
        return new rtrOspf6areaSpf(adr, lnk);
    }

    public String toString() {
        return adr + "/" + bits.toHexD(lnk);
    }

    /**
     * convert from string
     *
     * @param s string to convert
     * @return true on error, false on success
     */
    public boolean fromString(String s) {
        int i = s.lastIndexOf("/");
        if (i < 0) {
            return true;
        }
        if (adr.fromString(s.substring(0, i))) {
            return true;
        }
        lnk = bits.fromHex(s.substring(i + 1, s.length()));
        setUp();
        return false;
    }

}
