package net.freertr.addr;

import net.freertr.util.bits;

/**
 * stores one mac address
 *
 * @author matecsaba
 */
public class addrMac extends addrType {

    /**
     * create instance
     */
    public addrMac() {
    }

    /**
     * size of address
     */
    public final static int size = 6;

    /**
     * size of ethernet header (dstadr,srcadr)
     */
    public final static int sizeX2 = size * 2;

    /**
     * convert to string
     *
     * @return string
     */
    public String toString() {
        return bits.toHexB(addr[0]) + bits.toHexB(addr[1]) + "." + bits.toHexB(addr[2]) + bits.toHexB(addr[3]) + "." + bits.toHexB(addr[4]) + bits.toHexB(addr[5]);
    }

    /**
     * convert to string
     *
     * @return string
     */
    public String toEmuStr() {
        return bits.toHexB(addr[0]) + ":" + bits.toHexB(addr[1]) + ":" + bits.toHexB(addr[2]) + ":" + bits.toHexB(addr[3]) + ":" + bits.toHexB(addr[4]) + ":" + bits.toHexB(addr[5]);
    }

    /**
     * copy bytes
     *
     * @return copy
     */
    public addrMac copyBytes() {
        addrMac a = new addrMac();
        a.addr[0] = addr[0];
        a.addr[1] = addr[1];
        a.addr[2] = addr[2];
        a.addr[3] = addr[3];
        a.addr[4] = addr[4];
        a.addr[5] = addr[5];
        return a;
    }

    /**
     * compare two instances
     *
     * @param o1 first
     * @param o2 second
     * @return as usual
     */
    public int compare(addrType o1, addrType o2) {
        int v1 = bits.msbGetW(o1.addr, 0);
        int v2 = bits.msbGetW(o2.addr, 0);
        if (v1 < v2) {
            return -1;
        }
        if (v1 > v2) {
            return +1;
        }
        v1 = bits.msbGetW(o1.addr, 2);
        v2 = bits.msbGetW(o2.addr, 2);
        if (v1 < v2) {
            return -1;
        }
        if (v1 > v2) {
            return +1;
        }
        v1 = bits.msbGetW(o1.addr, 4);
        v2 = bits.msbGetW(o2.addr, 4);
        if (v1 < v2) {
            return -1;
        }
        if (v1 > v2) {
            return +1;
        }
        return 0;
    }

    /**
     * convert from string
     *
     * @param s string
     * @return false on success, true on error
     */
    public boolean fromString(String s) {
        s = s.replaceAll("\\.", "");
        s = s.replaceAll("\\:", "");
        s = s.replaceAll("\\-", "");
        if (s.length() != sizeX2) {
            return true;
        }
        for (int i = 0; i < size; i++) {
            try {
                addr[i] = (byte) Integer.parseInt(s.substring(i * 2, i * 2 + 2), 16);
            } catch (Exception e) {
                return true;
            }
        }
        return false;
    }

    /**
     * get size
     *
     * @return size
     */
    public int getSize() {
        return size;
    }

    /**
     * generate random address
     *
     * @return bytes of address
     */
    public static addrMac getRandom() {
        addrMac a = new addrMac();
        a.fillRandom();
        a.addr[0] = 0;
        return a;
    }

    /**
     * get broadcast address
     *
     * @return bytes of address
     */
    public static addrMac getBroadcast() {
        addrMac a = new addrMac();
        bits.byteFill(a.addr, 0, size, 0xff);
        return a;
    }

    /**
     * get multicast base address
     *
     * @return bytes of address
     */
    public static addrMac getMultiBase() {
        addrMac a = new addrMac();
        a.addr[0] = 1;
        return a;
    }

    /**
     * test if this address should be flooded to anybody
     *
     * @return true if yes
     */
    public boolean isFloodable() {
        return (addr[0] & 1) != 0;
    }

    /**
     * test if this is a broadcast address
     *
     * @return true if yes
     */
    public boolean isBroadcast() {
        return isFilled(255);
    }

    /**
     * test if this is a multicast address
     *
     * @return true if yes
     */
    public boolean isMulticast() {
        return isFloodable() && (!isBroadcast());
    }

    /**
     * test if this is a unicast address
     *
     * @return tris if yes
     */
    public boolean isUnicast() {
        return !isFloodable();
    }

}
