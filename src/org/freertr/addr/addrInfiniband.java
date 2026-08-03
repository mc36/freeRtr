package org.freertr.addr;

import org.freertr.util.bits;

/**
 * stores one infiniband address
 *
 * @author matecsaba
 */
public class addrInfiniband extends addrType {

    /**
     * create instance
     */
    public addrInfiniband() {
    }

    /**
     * size of address
     */
    public final static int size = 2;

    public addrInfiniband copyBytes() {
        addrInfiniband a = new addrInfiniband();
        a.fromBuf(addr, 0);
        return a;
    }

    public boolean fromString(String s) {
        try {
            int i = (byte) Integer.parseInt(s, 10);
            bits.msbPutW(addr, 0, i);
        } catch (Exception e) {
            return true;
        }
        return false;
    }

    public int getSize() {
        return size;
    }

    public String toString() {
        return "" + bits.msbGetW(addr, 0);
    }

    /**
     * check if multicast address
     *
     * @return true if yess
     */
    public boolean isMulticast() {
        return (addr[0] & 0xff) >= 0xc0;
    }

    /**
     * generate random address
     *
     * @return bytes of address
     */
    public static addrInfiniband getRandom() {
        addrInfiniband a = new addrInfiniband();
        a.addr[0] = (byte) bits.random(0, 0xc0);
        a.addr[1] = (byte) bits.randomB();
        return a;
    }

}
