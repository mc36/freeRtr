package org.freertr.rtr;

import org.freertr.addr.addrType;
import org.freertr.util.bits;

/**
 * rift spf address
 *
 * @author matecsaba
 */
public class rtrRiftTieSpf extends addrType {

    /**
     * create instance
     *
     * @param i originator
     */
    public rtrRiftTieSpf(long i) {
        addr = new byte[8];
        bits.msbPutQ(addr, 0, i);
    }

    public int getSize() {
        return 8;
    }

    public addrType copyBytes() {
        return new rtrRiftTieSpf(bits.msbGetQ(addr, 0));
    }

    public String toString() {
        return "" + bits.msbGetQ(addr, 0);
    }

    public boolean fromString(String s) {
        bits.msbPutQ(addr, 0, bits.str2long(s));
        return false;
    }

}
