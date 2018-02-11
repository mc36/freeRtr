package addr;

import util.bits;

/**
 * stores one eui64 address
 *
 * @author matecsaba
 */
public class addrEui extends addrType {

    /**
     * size of address
     */
    public final static int size = 8;

    public String toString() {
        return bits.toHexB(addr[0]) + bits.toHexB(addr[1]) + "-" + bits.toHexB(addr[2]) + bits.toHexB(addr[3]) + "-"
                + bits.toHexB(addr[4]) + bits.toHexB(addr[5]) + "-" + bits.toHexB(addr[6]) + bits.toHexB(addr[7]);
    }

    public addrEui copyBytes() {
        addrEui a = new addrEui();
        a.fromBuf(addr, 0);
        return a;
    }

    public boolean fromString(String s) {
        if (s.length() != toString().length()) {
            return true;
        }
        s = s.replaceAll("\\.", "-");
        s = s.replaceAll("\\:", "-");
        if (!s.substring(4, 5).endsWith("-")) {
            return true;
        }
        if (!s.substring(9, 10).endsWith("-")) {
            return true;
        }
        s = s.replaceAll("\\-", "");
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
     * generate random address
     *
     * @return bytes of address
     */
    public static addrEui getRandom() {
        addrEui a = new addrEui();
        a.fillRandom();
        a.addr[0] = 0;
        return a;
    }

    public int getSize() {
        return size;
    }

    /**
     * convert to ipv6
     *
     * @return ipv6 address
     */
    public addrIPv6 toIPv6() {
        addrIPv6 a = new addrIPv6();
        a.addr[0] = (byte) 0xfe;
        a.addr[1] = (byte) 0x80;
        bits.byteCopy(addr, 0, a.addr, size, size);
        return a;
    }

    /**
     * convert from ipv6
     *
     * @param a address
     */
    public void fromIPv6(addrIPv6 a) {
        for (int i = 0; i < size; i++) {
            addr[i] = a.addr[size + i];
        }
    }

}
