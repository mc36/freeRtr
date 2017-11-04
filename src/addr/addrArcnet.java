package addr;

import util.bits;

/**
 * stores one arcnet address
 *
 * @author matecsaba
 */
public class addrArcnet extends addrType {

    /**
     * size of address
     */
    public final static int size = 1;

    /**
     * size of ethernet header (dstadr,srcadr)
     */
    public final static int sizeX2 = size * 2;

    public String toString() {
        return bits.toHexB(addr[0]);
    }

    public addrArcnet copyBytes() {
        addrArcnet a = new addrArcnet();
        a.fromBuf(addr, 0);
        return a;
    }

    public boolean fromString(String s) {
        try {
            addr[0] = (byte) Integer.parseInt(s, 16);
        } catch (Exception e) {
            return true;
        }
        return false;
    }

    public int getSize() {
        return size;
    }

    /**
     * generate random address
     *
     * @return bytes of address
     */
    public static addrArcnet getRandom() {
        addrArcnet a = new addrArcnet();
        a.fillRandom();
        return a;
    }

    /**
     * get broadcast address
     *
     * @return bytes of address
     */
    public static addrArcnet getBroadcast() {
        addrArcnet a = new addrArcnet();
        bits.byteFill(a.addr, 0, size, 0x00);
        return a;
    }

}
