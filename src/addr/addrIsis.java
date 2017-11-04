package addr;

import util.bits;

/**
 * stores one isis system id
 *
 * @author matecsaba
 */
public class addrIsis extends addrType {

    /**
     * size of address
     */
    public final static int size = 6;

    public addrIsis copyBytes() {
        addrIsis a = new addrIsis();
        a.fromBuf(addr, 0);
        return a;
    }

    public int getSize() {
        return size;
    }

    public String toString() {
        return bits.toHexB(addr[0]) + bits.toHexB(addr[1]) + "." + bits.toHexB(addr[2]) + bits.toHexB(addr[3]) + "."
                + bits.toHexB(addr[4]) + bits.toHexB(addr[5]);
    }

    public boolean fromString(String s) {
        s = s.replaceAll("\\.", "");
        s = s.replaceAll("\\:", "");
        s = s.replaceAll("\\-", "");
        if (s.length() != (size * 2)) {
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

}
