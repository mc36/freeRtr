package addr;

import util.bits;

/**
 * stores one clns address the first byte is the size byte
 *
 * @author matecsaba
 */
public class addrClns extends addrType {

    /**
     * copy bytes
     *
     * @return copy
     */
    public addrClns copyBytes() {
        addrClns a = new addrClns();
        a.fromBuf(addr, 0);
        return a;
    }

    /**
     * get size
     *
     * @return size
     */
    public int getSize() {
        return 256;
    }

    /**
     * fill unused bytes
     */
    public void fillUnunsed() {
        int i = (addr[0] & 0xff) + 1;
        bits.byteFill(addr, i, addr.length - i, 0);
    }

    /**
     * convert to string
     *
     * @return string
     */
    public String toString() {
        String s = bits.toHexB(addr[1]);
        for (int i = 0; i < (addr[0] - 2); i += 2) {
            s += "." + bits.toHexB(addr[i + 2]) + bits.toHexB(addr[i + 3]);
        }
        if ((addr[0] & 1) == 0) {
            s += "." + bits.toHexB(addr[addr[0]]);
        }
        return s;
    }

    /**
     * from string
     *
     * @param s string
     * @return false on success, true on error
     */
    public boolean fromString(String s) {
        bits.byteFill(addr, 0, addr.length, 0);
        s = s.replaceAll("\\.", "");
        s = s.replaceAll("\\:", "");
        s = s.replaceAll("\\-", "");
        if ((s.length() & 1) != 0) {
            return true;
        }
        addr[0] = (byte) (s.length() / 2);
        for (int i = 0; i < addr[0]; i++) {
            try {
                addr[i + 1] = (byte) Integer.parseInt(s.substring(i * 2, i * 2 + 2), 16);
            } catch (Exception e) {
                return true;
            }
        }
        fillUnunsed();
        return false;
    }

    /**
     * get bytes of address
     *
     * @param lenAlso include length in data
     * @return bytes
     */
    public byte[] getAddrDat(boolean lenAlso) {
        int len = addr[0] & 0xff;
        if (lenAlso) {
            byte[] buf = new byte[len + 1];
            bits.byteCopy(addr, 0, buf, 0, buf.length);
            return buf;
        }
        byte[] buf = new byte[len];
        bits.byteCopy(addr, 1, buf, 0, buf.length);
        return buf;
    }

    /**
     * get real length of address including size byte
     *
     * @return bytes
     */
    public int getAddrLen() {
        return (addr[0] & 0xff) + 1;
    }

    /**
     * get selector byte
     *
     * @return selector
     */
    public int getSel() {
        return addr[addr[0] & 0xff] & 0xff;
    }

    /**
     * get system id
     *
     * @return system id, null if too short
     */
    public addrIsis getNode() {
        if ((addr[0] & 0xff) < (addrIsis.size + 1)) {
            return null;
        }
        addrIsis a = new addrIsis();
        a.fromBuf(addr, (addr[0] & 0xff) - addrIsis.size);
        return a;
    }

    /**
     * get area id
     *
     * @return area id, null if too short
     */
    public addrClns getArea() {
        if ((addr[0] & 0xff) < (addrIsis.size + 1)) {
            return null;
        }
        addrClns a = new addrClns();
        a.addr[0] = (byte) (addr[0] - addrIsis.size - 1);
        bits.byteCopy(addr, 1, a.addr, 1, a.addr[0] & 0xff);
        return a;
    }

}
