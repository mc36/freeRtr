package net.freertr.cry;

import java.math.BigInteger;

/**
 * one polynomial
 *
 * @author matecsaba
 */
public class cryPoly {

    private final BigInteger coeff;

    /**
     * create instance
     *
     * @param c coefficient
     */
    public cryPoly(BigInteger c) {
        coeff = c;
    }

    /**
     * create instance
     *
     * @param c coefficient
     */
    public cryPoly(int c) {
        coeff = BigInteger.valueOf(c);
    }

    /**
     * create instance
     *
     * @param c coefficient
     */
    public cryPoly(String c) {
        coeff = new BigInteger(c, 2);
    }

    /**
     * get coefficient
     *
     * @return coefficient
     */
    public BigInteger getCoeff() {
        return coeff;
    }

    /**
     * get coefficient
     *
     * @return coefficient
     */
    public int intCoeff() {
        return coeff.intValue();
    }

    /**
     * get degree
     *
     * @return degree
     */
    public int getDegree() {
        return coeff.bitLength() - 1;
    }

    public String toString() {
        return coeff.toString(2);
    }

    private static BigInteger mul(BigInteger c, BigInteger a) {
        BigInteger r = BigInteger.valueOf(0);
        int s = c.bitLength();
        for (int i = 0; i < s; i++) {
            if (c.testBit(0)) {
                r = r.xor(a);
            }
            c = c.shiftRight(1);
            a = a.shiftLeft(1);
        }
        return r;
    }

    private static BigInteger[] div(BigInteger r, BigInteger d) {
        int s = d.bitLength();
        if (s < 1) {
            return null;
        }
        s = r.bitLength() - s;
        if (s < 1) {
            s = 0;
        }
        BigInteger p = BigInteger.valueOf(1).shiftLeft(s);
        BigInteger q = BigInteger.valueOf(0);
        d = d.shiftLeft(s);
        for (int i = 0; i <= s; i++) {
            if (r.bitLength() == d.bitLength()) {
                r = r.xor(d);
                q = q.xor(p);
            }
            d = d.shiftRight(1);
            p = p.shiftRight(1);
        }
        return new BigInteger[]{q, r};
    }

    private static BigInteger[] modInv(BigInteger nr, BigInteger r) {
        // extended euclidean algorithm
        BigInteger t = BigInteger.valueOf(0);
        BigInteger nt = BigInteger.valueOf(1);
        for (;;) {
            if (nr.bitLength() < 1) {
                break;
            }
            BigInteger[] q = div(r, nr);
            if (q == null) {
                return null;
            }
            BigInteger ot = nt;
            BigInteger or = nr;
            nt = t.xor(mul(q[0], nt));
            nr = r.xor(mul(q[0], nr));
            t = ot;
            r = or;
        }
        if (r.bitLength() > 1) {
            t = null;
        }
        return new BigInteger[]{t, r};
    }

    /**
     * addition
     *
     * @param o other
     * @return result
     */
    public cryPoly add(cryPoly o) {
        return new cryPoly(coeff.xor(o.coeff));
    }

    /**
     * subtract
     *
     * @param o other
     * @return result
     */
    public cryPoly sub(cryPoly o) {
        return new cryPoly(coeff.xor(o.coeff));
    }

    /**
     * multiplication
     *
     * @param o other
     * @return result
     */
    public cryPoly mul(cryPoly o) {
        return new cryPoly(mul(o.coeff, coeff));
    }

    /**
     * division
     *
     * @param o other
     * @return array of result,remainder
     */
    public cryPoly[] div(cryPoly o) {
        BigInteger[] r = div(coeff, o.coeff);
        if (r == null) {
            return null;
        }
        return new cryPoly[]{new cryPoly(r[0]), new cryPoly(r[1])};
    }

    /**
     * multiplicative inverse
     *
     * @param o other
     * @return array of result,gcd
     */
    public cryPoly[] modInv(cryPoly o) {
        BigInteger[] r = modInv(coeff, o.coeff);
        if (r == null) {
            return null;
        }
        if (r[0] == null) {
            return new cryPoly[]{null, new cryPoly(r[1])};
        }
        return new cryPoly[]{new cryPoly(r[0]), new cryPoly(r[1])};
    }

}
