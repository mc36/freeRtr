package addr;

import util.bits;

/**
 * stores one IPv6 address
 *
 * @author matecsaba
 */
public class addrIPv6 extends addrType {

    /**
     * size of address
     */
    public final static int size = 16;

    /**
     * get size
     *
     * @return size
     */
    public int getSize() {
        return size;
    }

    /**
     * copy bytes
     *
     * @return copy
     */
    public addrIPv6 copyBytes() {
        addrIPv6 a = new addrIPv6();
        a.fromBuf(addr, 0);
        return a;
    }

    /**
     * convert to string
     *
     * @return string
     */
    public String toString() {
        int bstPos = 0;
        int bstLen = 0;
        int curBeg = 0;
        for (int curPos = 0; curPos <= size; curPos += 2) {
            int curLen = curPos - curBeg - 1;
            if (checkPartZero(curPos)) {
                curBeg = curPos + 1;
            }
            if (curLen <= bstLen) {
                continue;
            }
            bstLen = curLen;
            bstPos = curPos;
        }
        if (bstLen < 1) {
            return toPartString(0, size / 2);
        }
        return toPartString(0, (bstPos - bstLen) / 2) + "::" + toPartString(bstPos / 2, size / 2);
    }

    private boolean checkPartZero(int curPos) {
        if (curPos >= size) {
            return false;
        }
        return (addr[curPos] != 0) || (addr[curPos + 1] != 0);
    }

    private String toPartString(int min, int max) {
        if (min >= max) {
            return "";
        }
        String s = "";
        for (int i = min; i < max; i++) {
            String a = bits.toHexB(addr[i * 2]) + bits.toHexB(addr[(i * 2) + 1]);
            for (;;) {
                if (a.length() < 1) {
                    a = "0";
                    break;
                }
                if (!a.substring(0, 1).equals("0")) {
                    break;
                }
                a = a.substring(1, a.length());
            }
            s += ":" + a;
        }
        return s.substring(1, s.length());
    }

    private int fromPartString(byte[] buf, String s) {
        if (s.length() == 0) {
            return 0;
        }
        int o = 0;
        s += ":";
        for (;;) {
            int i = s.indexOf(":");
            if (i < 0) {
                return o;
            }
            if (o >= size) {
                return -1;
            }
            try {
                bits.msbPutW(buf, o, Integer.parseInt(s.substring(0, i), 16));
            } catch (Exception e) {
                return -1;
            }
            o += 2;
            s = s.substring(i + 1, s.length());
        }
    }

    /**
     * convert from string
     *
     * @param s string
     * @return false on success, true on error
     */
    public boolean fromString(String s) {
        int o = s.indexOf("::");
        if (o < 0) {
            byte[] b = new byte[size];
            int i = fromPartString(b, s);
            if (i != size) {
                return true;
            }
            fromBuf(b, 0);
            return false;
        }
        byte[] b1 = new byte[size];
        byte[] b2 = new byte[size];
        bits.byteFill(b1, 0, size, 0);
        int s1 = fromPartString(b1, s.substring(0, o));
        if (s1 < 0) {
            return true;
        }
        int s2 = fromPartString(b2, s.substring(o + 2, s.length()));
        if (s2 < 0) {
            return true;
        }
        if (s1 + s2 > size) {
            return true;
        }
        bits.byteCopy(b2, 0, b1, size - s2, s2);
        fromBuf(b1, 0);
        return false;
    }

    /**
     * get empty address
     *
     * @return bytes of address
     */
    public static addrIPv6 getEmpty() {
        addrIPv6 a = new addrIPv6();
        bits.byteFill(a.addr, 0, size, 0);
        return a;
    }

    /**
     * test if this is an empty address
     *
     * @return true if yes
     */
    public boolean isEmpty() {
        addrIPv6 a = getEmpty();
        return compare(this, a) == 0;
    }

    /**
     * test if this is a broadcast address
     *
     * @return true if yes
     */
    public boolean isBroadcast() {
        return false;
    }

    /**
     * test if this is a link local address
     *
     * @return true if yes
     */
    public boolean isLinkLocal() {
        return (((addr[0] & 0xff) == 0xfe) && ((addr[1] & 0x80) == 0x80));
    }

    /**
     * test if this is a multicast address
     *
     * @return true if yes
     */
    public boolean isMulticast() {
        return ((addr[0] & 0xff) == 0xff);
    }

    /**
     * test if this is a routed multicast address
     *
     * @return true if yes
     */
    public boolean isRoutedMcast() {
        return (bits.msbGetW(addr, 0) & 0xff0f) > 0xff03;
    }

    /**
     * test if this is a unicast address
     *
     * @return tris if yes
     */
    public boolean isUnicast() {
        return !isMulticast();
    }

    /**
     * convert multicast address to multicast mac address
     *
     * @return multicast mac address
     */
    public addrMac conv2multiMac() {
        addrMac a = new addrMac();
        a.addr[0] = 0x33;
        a.addr[1] = 0x33;
        a.addr[2] = addr[12];
        a.addr[3] = addr[13];
        a.addr[4] = addr[14];
        a.addr[5] = addr[15];
        return a;
    }

    /**
     * convert to solicited node multicast address
     *
     * @return solicited node address
     */
    public addrIPv6 conv2solicited() {
        addrIPv6 adr = new addrIPv6();
        adr.addr[0] = (byte) 0xff;
        adr.addr[1] = (byte) 0x02;
        adr.addr[11] = (byte) 0x01;
        adr.addr[12] = (byte) 0xff;
        adr.addr[13] = addr[13];
        adr.addr[14] = addr[14];
        adr.addr[15] = addr[15];
        return adr;
    }

    /**
     * generate link local address
     *
     * @param mac mac address to convert
     * @return ipv6 link local address of mac
     */
    public static addrIPv6 genLinkLocal(addrMac mac) {
        addrIPv6 adr = new addrIPv6();
        adr.addr[0] = (byte) 0xfe;
        adr.addr[1] = (byte) 0x80;
        return genPublic(mac, adr);
    }

    /**
     * generate public address
     *
     * @param mac mac address to convert
     * @param ip ip address to convert
     * @return ipv6 link local address of mac
     */
    public static addrIPv6 genPublic(addrMac mac, addrIPv6 ip) {
        addrIPv6 adr = new addrIPv6();
        bits.byteCopy(ip.addr, 0, adr.addr, 0, size / 2);
        adr.addr[8] = (byte) (mac.addr[0] ^ 0x02);
        adr.addr[9] = mac.addr[1];
        adr.addr[10] = mac.addr[2];
        adr.addr[11] = (byte) 0xff;
        adr.addr[12] = (byte) 0xfe;
        adr.addr[13] = mac.addr[3];
        adr.addr[14] = mac.addr[4];
        adr.addr[15] = mac.addr[5];
        return adr;
    }

    /**
     * get all nodes multicast address
     *
     * @return the address
     */
    public static addrIPv6 getAllNodes() {
        addrIPv6 adr = new addrIPv6();
        adr.addr[0] = (byte) 0xff;
        adr.addr[1] = (byte) 0x02;
        adr.addr[15] = (byte) 0x01;
        return adr;
    }

    /**
     * get all routers multicast address
     *
     * @return the address
     */
    public static addrIPv6 getAllRouters() {
        addrIPv6 adr = new addrIPv6();
        adr.addr[0] = (byte) 0xff;
        adr.addr[1] = (byte) 0x02;
        adr.addr[15] = (byte) 0x02;
        return adr;
    }

}
