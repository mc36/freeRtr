package net.freertr.addr;

import net.freertr.util.bits;

/**
 * stores one IPv4 address
 *
 * @author matecsaba
 */
public class addrIPv4 extends addrType {

    /**
     * create instance
     */
    public addrIPv4() {
    }

    /**
     * copy bytes
     *
     * @return copy
     */
    public addrIPv4 copyBytes() {
        addrIPv4 a = new addrIPv4();
        a.addr[0] = addr[0];
        a.addr[1] = addr[1];
        a.addr[2] = addr[2];
        a.addr[3] = addr[3];
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
        return 0;
    }

    /**
     * size of address
     */
    public final static int size = 4;

    /**
     * get size
     *
     * @return size
     */
    public int getSize() {
        return size;
    }

    /**
     * convert to string
     *
     * @return string
     */
    public String toString() {
        return (addr[0] & 0xff) + "." + (addr[1] & 0xff) + "." + (addr[2] & 0xff) + "." + (addr[3] & 0xff);
    }

    /**
     * convert from string
     *
     * @param s string
     * @return false on success, true on error
     */
    public boolean fromString(String s) {
        s += ".";
        for (int i = 0; i < size; i++) {
            int o = s.indexOf(".");
            if (o < 0) {
                return true;
            }
            try {
                addr[i] = (byte) Integer.parseInt(s.substring(0, o), 10);
            } catch (Exception e) {
                return true;
            }
            s = s.substring(o + 1, s.length());
        }
        if (s.length() != 0) {
            return true;
        }
        return false;
    }

    /**
     * get class of this address (a..e)
     *
     * @return 1=a, 2=b, etc, 0 if unknown
     */
    public int getAddrClass() {
        int i = addr[0] & 0xff;
        if (i < 128) { // a - /8 nets
            return 1;
        }
        if (i < 192) { // b - /16 nets
            return 2;
        }
        if (i < 224) { // c - /24 nets
            return 3;
        }
        if (i < 240) { // d - multicast
            return 4;
        }
        if (i < 248) { // e - reserved
            return 5;
        }
        return 0;
    }

    /**
     * get broadcast address
     *
     * @return bytes of address
     */
    public static addrIPv4 getBroadcast() {
        addrIPv4 a = new addrIPv4();
        bits.byteFill(a.addr, 0, size, 0xff);
        return a;
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
     * test if this is a link local address
     *
     * @return true if yes
     */
    public boolean isLinkLocal() {
        return false;
        // return (((addr[0] & 0xff) == 169) && ((addr[1] & 0xff) == 254));
    }

    /**
     * get empty address
     *
     * @return bytes of address
     */
    public static addrIPv4 getEmpty() {
        addrIPv4 a = new addrIPv4();
        bits.byteFill(a.addr, 0, size, 0);
        return a;
    }

    /**
     * test if this is an empty address
     *
     * @return true if yes
     */
    public boolean isEmpty() {
        return isFilled(0);
    }

    /**
     * test if this is a multicast address
     *
     * @return true if yes
     */
    public boolean isMulticast() {
        int i = addr[0] & 0xff;
        return (i >= 224) && (i < 240);
    }

    /**
     * test if this is a routed multicast address
     *
     * @return true if yes
     */
    public boolean isRoutedMcast() {
        int i = bits.msbGetW(addr, 0);
        if (i == 0xefff) { // 239.255.x.x
            return false;
        }
        return i > 0xe000; // 224.0.x.x
    }

    /**
     * test if this is a unicast address
     *
     * @return tris if yes
     */
    public boolean isUnicast() {
        return (!isBroadcast()) && (!isMulticast());
    }

    /**
     * convert multicast address to multicast mac address
     *
     * @return multicast mac address
     */
    public addrMac conv2multiMac() {
        addrMac a = new addrMac();
        a.addr[0] = 0x01;
        a.addr[1] = 0x00;
        a.addr[2] = 0x5e;
        a.addr[3] = (byte) (addr[1] & 0x7f);
        a.addr[4] = addr[2];
        a.addr[5] = addr[3];
        return a;
    }

}
