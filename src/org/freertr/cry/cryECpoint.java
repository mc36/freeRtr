package org.freertr.cry;

import java.math.BigInteger;
import org.freertr.util.bits;

/**
 * one point on elliptic curve
 *
 * @author matecsaba
 */
public class cryECpoint {

    /**
     * curve
     */
    public final cryECcurve c;

    /**
     * x
     */
    public final BigInteger x;

    /**
     * y
     */
    public final BigInteger y;

    /**
     * 0
     */
    public final static BigInteger int0 = new BigInteger("0");

    /**
     * 1
     */
    public final static BigInteger int1 = new BigInteger("1");

    /**
     * 2
     */
    public final static BigInteger int2 = new BigInteger("2");

    /**
     * 3
     */
    public final static BigInteger int3 = new BigInteger("3");

    private cryECpoint mul2res;

    /**
     * create point
     *
     * @param mc curve
     * @param mx x
     * @param my y
     */
    public cryECpoint(cryECcurve mc, BigInteger mx, BigInteger my) {
        c = mc;
        x = mx;
        y = my;
    }

    public String toString() {
        return "(" + x + "," + y + ")";
    }

    /**
     * test if on curve
     *
     * @return true if yes, false if no
     */
    public boolean check() {
        BigInteger t1 = y.modPow(int2, c.p);
        BigInteger t2 = x.modPow(int3, c.p);
        BigInteger t3 = ((t2.add(c.a.multiply(x))).add(c.b)).mod(c.p);
        return t1.compareTo(t3) == 0;
    }

    /**
     * negate point
     *
     * @return result
     */
    public cryECpoint neg() {
        return new cryECpoint(c, x, c.p.subtract(y));
    }

    /**
     * add two points
     *
     * @param p point to add
     * @return result
     */
    public cryECpoint add(cryECpoint p) {
        if ((p == this) && (mul2res != null)) {
            return mul2res;
        }
        BigInteger t;
        if (p.x.compareTo(x) == 0) {
            t = ((x.modPow(int2, c.p)).multiply(int3)).add(c.a);
            t = (t.multiply((int2.multiply(y)).modInverse(c.p))).mod(c.p);
        } else {
            t = ((p.y.subtract(y)).multiply((p.x.subtract(x)).modInverse(c.p))).mod(c.p);
        }
        BigInteger x3 = (((t.modPow(int2, c.p)).subtract(p.x)).subtract(x)).mod(c.p);
        BigInteger y3 = ((t.multiply(x.subtract(x3))).subtract(y)).mod(c.p);
        cryECpoint res = new cryECpoint(c, x3, y3);
        if (p == this) {
            mul2res = res;
        }
        return res;
    }

    /**
     * multiply by number
     *
     * @param n number
     * @return result
     */
    public cryECpoint mul(BigInteger n) {
        cryECpoint res = null;
        cryECpoint val = this;
        int len = n.bitLength();
        for (int pos = 0; pos < len; pos++) {
            if (n.testBit(pos)) {
                if (res == null) {
                    res = val;
                } else {
                    res = res.add(val);
                }
            }
            val = val.add(val);
        }
        return res;
    }

    /**
     * get x coordinate
     *
     * @return x coordinate
     */
    public byte[] getBytesX() {
        int siz = (c.p.bitLength() + 7) / 8;
        byte[] b1 = cryKeyGeneric.bigInt2buffer(x, siz);
        return b1;
    }

    private byte[] getBytes() {
        int siz = (c.p.bitLength() + 7) / 8;
        byte[] b1 = cryKeyGeneric.bigInt2buffer(x, siz);
        byte[] b2 = cryKeyGeneric.bigInt2buffer(y, siz);
        return bits.byteConcat(b1, b2);
    }

    /**
     * get bytes
     *
     * @return bytes
     */
    public byte[] toBytes1() {
        byte[] b0 = new byte[1];
        b0[0] = 4;
        return bits.byteConcat(b0, getBytes());
    }

    /**
     * get bytes
     *
     * @return bytes
     */
    public byte[] toBytes2() {
        byte[] b0 = new byte[2];
        b0[1] = 4;
        return bits.byteConcat(b0, getBytes());
    }

    private static cryECpoint fromBytes(cryECcurve c, byte[] buf, int ofs) {
        int siz = (c.p.bitLength() + 7) / 8;
        if ((buf.length - ofs) < (siz * 2)) {
            return null;
        }
        BigInteger x = cryKeyGeneric.buffer2bigInt(buf, ofs, siz);
        BigInteger y = cryKeyGeneric.buffer2bigInt(buf, ofs + siz, siz);
        return new cryECpoint(c, x, y);
    }

    /**
     * convert from bytes
     *
     * @param c curve
     * @param buf buffer
     * @param ofs where to start
     * @return point
     */
    public static cryECpoint fromBytes1(cryECcurve c, byte[] buf, int ofs) {
        if (buf[ofs] != 4) {
            return null;
        }
        return fromBytes(c, buf, ofs + 1);
    }

    /**
     * convert from bytes
     *
     * @param c curve
     * @param buf buffer
     * @param ofs where to start
     * @return point
     */
    public static cryECpoint fromBytes2(cryECcurve c, byte[] buf, int ofs) {
        if (buf[ofs + 1] != 4) {
            return null;
        }
        return fromBytes(c, buf, ofs + 2);
    }

}
