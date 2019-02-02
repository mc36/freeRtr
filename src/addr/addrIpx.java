package addr;

import util.bits;

/**
 * stores one ipx address
 *
 * @author matecsaba
 */
public class addrIpx extends addrType {

    /**
     * size of address
     */
    public final static int size = 10;

    /**
     * get size
     *
     * @return size
     */
    public int getSize() {
        return size;
    }

    /**
     * get copy
     *
     * @return copy
     */
    public addrIpx copyBytes() {
        addrIpx a = new addrIpx();
        a.fromBuf(addr, 0);
        return a;
    }

    /**
     * convert to string
     *
     * @return string
     */
    public String toString() {
        return bits.toHexB(addr[0]) + bits.toHexB(addr[1]) + bits.toHexB(addr[2]) + bits.toHexB(addr[3]) + "."
                + bits.toHexB(addr[4]) + bits.toHexB(addr[5]) + "." + bits.toHexB(addr[6]) + bits.toHexB(addr[7]) + "."
                + bits.toHexB(addr[8]) + bits.toHexB(addr[9]);
    }

    /**
     * from string
     *
     * @param s string
     * @return false on success, true on error
     */
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

    /**
     * get network part
     *
     * @return network
     */
    public int getNet() {
        return bits.msbGetD(addr, 0);
    }

    /**
     * get mac part
     *
     * @return mac
     */
    public addrMac getMac() {
        addrMac a = new addrMac();
        a.fromBuf(addr, 4);
        return a;
    }

    /**
     * put network
     *
     * @param i network
     */
    public void putNet(int i) {
        bits.msbPutD(addr, 0, i);
    }

    /**
     * put mac
     *
     * @param a mac
     */
    public void putMac(addrMac a) {
        a.toBuffer(addr, 4);
    }

}
