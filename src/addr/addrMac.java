package addr;

import util.bits;

/**
 * stores one mac address
 *
 * @author matecsaba
 */
public class addrMac extends addrType {

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
        a.fromBuf(addr, 0);
        return a;
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
        addrMac a = getBroadcast();
        return compare(this, a) == 0;
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
