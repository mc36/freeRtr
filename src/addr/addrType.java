package addr;

import java.util.Comparator;

import util.bits;

/**
 * one address
 *
 * @author matecsaba
 */
public abstract class addrType implements Comparator<addrType> {

    /**
     * bytes representing this address
     */
    protected byte[] addr;

    /**
     * get size of this address in bytes
     *
     * @return bytes in the address
     */
    public abstract int getSize();

    /**
     * clone this address
     *
     * @return new instance with the same content
     */
    public abstract addrType copyBytes();

    /**
     * text representation of this address
     *
     * @return text representing of this address
     */
    public abstract String toString();

    /**
     * convert string to address
     *
     * @param s string to convert
     * @return true if error happened
     */
    public abstract boolean fromString(String s);

    /**
     * get size of this address in bits
     *
     * @return bits in the address
     */
    public int maxBits() {
        return addr.length * 8;
    }

    /**
     * set address from other address
     *
     * @param src
     */
    public void setAddr(addrType src) {
        bits.byteCopy(src.addr, 0, addr, 0, addr.length);
    }

    /**
     * exchange two addresses
     *
     * @param other other to exchange
     */
    public void exchangeAddr(addrType other) {
        byte[] ob = other.addr;
        other.addr = addr;
        addr = ob;
    }

    /**
     * convert prefix length to address
     *
     * @param len number of 1 bits
     */
    public void fromNetmask(int len) {
        if (len > addr.length * 8) {
            len = addr.length * 8;
        }
        if (len < 0) {
            len = 0;
        }
        int ofs = len / 8;
        len &= 7;
        bits.byteFill(addr, 0, ofs, 0xff);
        bits.byteFill(addr, ofs, addr.length - ofs, 0);
        if (len < 1) {
            return;
        }
        addr[ofs] = maskVals[len];
    }

    private final static byte[] maskVals = {
        (byte) 0x00,
        (byte) 0x80, (byte) 0xc0, (byte) 0xe0, (byte) 0xf0,
        (byte) 0xf8, (byte) 0xfc, (byte) 0xfe, (byte) 0xff
    };

    /**
     * convert address to prefix length
     *
     * @return number of 1 bits
     */
    public int toNetmask() {
        int len;
        for (len = 0; len < addr.length; len++) {
            if ((addr[len] & 0xff) != 0xff) {
                break;
            }
        }
        if (len >= addr.length) {
            return addr.length * 8;
        }
        int val = addr[len] & 0xff;
        int ofs;
        for (ofs = 7; ofs >= 0; ofs--) {
            if ((bits.bitVals[ofs] & val) == 0) {
                break;
            }
        }
        return len * 8 + 7 - ofs;
    }

    /**
     * bit values in u32
     */
    public final static int[] bitVals = {
        0x80, 0x40, 0x20, 0x10, 0x8, 0x4, 0x2, 0x1
    };

    /**
     * set one bit
     *
     * @param num bit number
     */
    public void bitSet(int num) {
        addr[num / 8] |= bitVals[num & 0x7];
    }

    /**
     * set one bit
     *
     * @param num bit number
     */
    public void bitClear(int num) {
        addr[num / 8] &= -bitVals[num & 0x7] - 1;
    }

    /**
     * set one bit
     *
     * @param num bit number
     * @return bit value
     */
    public boolean bitValue(int num) {
        return (addr[num / 8] & bitVals[num & 0x7]) != 0;
    }

    /**
     * create empty address
     */
    public addrType() {
        addr = new byte[getSize()];
    }

    /**
     * get bytes of address
     *
     * @return bytes represents the address
     */
    public byte[] getBytes() {
        return addr;
    }

    /**
     * set address bytes from buffer
     *
     * @param buf source buffer
     * @param ofs offset in buffer
     */
    public void fromBuf(byte[] buf, int ofs) {
        bits.byteCopy(buf, ofs, addr, 0, addr.length);
    }

    /**
     * copy address bytes to buffer
     *
     * @param buf target buffer
     * @param ofs offset in buffer
     */
    public void toBuffer(byte[] buf, int ofs) {
        bits.byteCopy(addr, 0, buf, ofs, addr.length);
    }

    /**
     * test if address matches to a network/mask
     *
     * @param net network to test for
     * @param msk mask to test for
     * @return true if matches, false if not matches
     */
    public boolean isMatches(addrType net, addrType msk) {
        for (int i = 0; i < addr.length; i++) {
            if ((addr[i] & msk.addr[i]) != (net.addr[i] & msk.addr[i])) {
                return false;
            }
        }
        return true;
    }

    /**
     * test if address filled with a value
     *
     * @param val value to check for
     * @return true if filled, false if not
     */
    public boolean isFilled(int val) {
        for (int i = 0; i < addr.length; i++) {
            if (addr[i] != (byte) val) {
                return false;
            }
        }
        return true;
    }

    /**
     * fill up buffer with value
     *
     * @param val value to set
     */
    public void fillBytes(int val) {
        for (int i = 0; i < addr.length; i++) {
            addr[i] = (byte) val;
        }
    }

    /**
     * fill up buffer with random
     */
    public void fillRandom() {
        for (int i = 0; i < addr.length; i++) {
            addr[i] = (byte) bits.randomB();
        }
    }

    /**
     * set from a1 and a2
     *
     * @param a1 address 1
     * @param a2 address 2
     */
    public void setAnd(addrType a1, addrType a2) {
        for (int i = 0; i < addr.length; i++) {
            addr[i] = (byte) ((a1.addr[i] & 0xff) & (a2.addr[i] & 0xff));
        }
    }

    /**
     * set from a1 or a2
     *
     * @param a1 address 1
     * @param a2 address 2
     */
    public void setOr(addrType a1, addrType a2) {
        for (int i = 0; i < addr.length; i++) {
            addr[i] = (byte) ((a1.addr[i] & 0xff) | (a2.addr[i] & 0xff));
        }
    }

    /**
     * set from a1 xor a2
     *
     * @param a1 address 1
     * @param a2 address 2
     */
    public void setXor(addrType a1, addrType a2) {
        for (int i = 0; i < addr.length; i++) {
            addr[i] = (byte) ((a1.addr[i] & 0xff) ^ (a2.addr[i] & 0xff));
        }
    }

    /**
     * set from not a1
     *
     * @param a1 address
     */
    public void setNot(addrType a1) {
        for (int i = 0; i < addr.length; i++) {
            addr[i] = (byte) (255 - (a1.addr[i] & 0xff));
        }
    }

    /**
     * set from a1 + a2
     *
     * @param a1 address 1
     * @param a2 address 2
     */
    public void setAdd(addrType a1, addrType a2) {
        for (int i = 0; i < addr.length; i++) {
            addr[i] = (byte) ((a1.addr[i] & 0xff) + (a2.addr[i] & 0xff));
        }
    }

    /**
     * set from a1 - a2
     *
     * @param a1 address 1
     * @param a2 address 2
     */
    public void setSub(addrType a1, addrType a2) {
        for (int i = 0; i < addr.length; i++) {
            addr[i] = (byte) ((a1.addr[i] & 0xff) - (a2.addr[i] & 0xff));
        }
    }

    /**
     * compare two instances
     *
     * @param o1 first
     * @param o2 second
     * @return as usual
     */
    public int compare(addrType o1, addrType o2) {
        return bits.byteComp(o1.addr, 0, o2.addr, 0, addr.length);
    }

    /**
     * get address hash
     *
     * @return xor value
     */
    public int getHash() {
        int o = 0;
        for (int i = 0; i < addr.length; i++) {
            o ^= addr[i] & 0xff;
        }
        return o;
    }

}
