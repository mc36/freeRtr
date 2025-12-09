package org.freertr.cry;

import java.io.File;
import java.io.RandomAccessFile;
import java.math.BigInteger;
import java.util.List;
import org.freertr.pipe.pipeSide;
import org.freertr.util.bits;

/**
 * crypto utils
 *
 * @author matecsaba
 */
public class cryUtils {

    private cryUtils() {
    }

    /**
     * convert buffer to big integer
     *
     * @param src source buffer
     * @param ofs offset in buffer
     * @param siz size in bytes
     * @return integer
     */
    public static BigInteger buffer2bigInt(byte[] src, int ofs, int siz) {
        byte[] buf = new byte[siz + 1];
        for (int i = 0; i < siz; i++) {
            buf[i + 1] = src[ofs + i];
        }
        buf[0] = 0;
        return new BigInteger(buf);
    }

    /**
     * save big integer to buffer
     *
     * @param o big integer
     * @param siz size of buffer
     * @return byte buffer
     */
    public static byte[] bigInt2buffer(BigInteger o, int siz) {
        byte[] b1 = o.toByteArray();
        byte[] b2 = new byte[siz];
        if (b1.length >= b2.length) {
            bits.byteCopy(b1, b1.length - b2.length, b2, 0, b2.length);
            return b2;
        }
        bits.byteFill(b2, 0, b2.length - b1.length, 0);
        bits.byteCopy(b1, 0, b2, b2.length - b1.length, b1.length);
        return b2;
    }

    /**
     * convert big unsigned integer to buffer
     *
     * @param b integer
     * @return buffer
     */
    public static byte[] bigUint2buf(BigInteger b) {
        byte[] dat = b.toByteArray();
        if (dat[0] != 0) {
            return dat;
        }
        byte[] buf = new byte[dat.length - 1];
        bits.byteCopy(dat, 1, buf, 0, buf.length);
        return buf;
    }

    /**
     * generate random big integer
     *
     * @param siz size in bits
     * @return random number
     */
    public static BigInteger randomBigInt(int siz) {
        byte[] buf = new byte[(siz / 8) + 1];
        bits.randomS().nextBytes(buf);
        buf[0] = 0;
        BigInteger i = new BigInteger(buf);
        i = i.mod(BigInteger.ONE.shiftLeft(siz).subtract(BigInteger.ONE));
        return i;
    }

    /**
     * generate random prime
     *
     * @param siz size in bits
     * @return random prime
     */
    public static BigInteger randomPrime(int siz) {
        for (;;) {
            BigInteger i = BigInteger.probablePrime(siz, bits.randomS());
            if (i.bitLength() != siz) {
                continue;
            }
            if (!testPrime(i)) {
                continue;
            }
            return i;
        }
    }

    /**
     * test if a prime
     *
     * @param i number to test
     * @return true if prime, false if not
     */
    public static boolean testPrime(BigInteger i) {
        return i.isProbablePrime(100);
    }

    /**
     * generate hash from text
     *
     * @param hsh hash to update
     * @param src lines to add
     * @param trm line terminator
     * @return false on success, true on error
     */
    public static boolean hashText(cryHashGeneric hsh, List<String> src, pipeSide.modTyp trm) {
        if (src == null) {
            return true;
        }
        byte[] buf = pipeSide.getEnding(trm);
        for (int i = 0; i < src.size(); i++) {
            hsh.update(src.get(i).getBytes());
            hsh.update(buf);
        }
        return false;
    }

    /**
     * generate hash from file
     *
     * @param hsh hash to update
     * @param src file to add
     * @return false on success, true on error
     */
    public static boolean hashFile(cryHashGeneric hsh, File src) {
        long pos = 0;
        long siz = -1;
        RandomAccessFile fr;
        try {
            fr = new RandomAccessFile(src, "r");
        } catch (Exception e) {
            return true;
        }
        try {
            siz = fr.length();
        } catch (Exception e) {
        }
        for (; pos < siz;) {
            final int max = 8192;
            long rndl = siz - pos;
            if (rndl > max) {
                rndl = max;
            }
            pos += rndl;
            int rndi = (int) rndl;
            byte[] buf = new byte[rndi];
            try {
                fr.read(buf, 0, rndi);
            } catch (Exception e) {
                siz = -1;
                break;
            }
            hsh.update(buf);
        }
        try {
            fr.close();
        } catch (Exception e) {
            return true;
        }
        return siz < 0;
    }

    /**
     * finish hash to hex
     *
     * @param hsh hash to finish
     * @return hex result
     */
    public static String hash2hex(cryHashGeneric hsh) {
        byte[] buf = hsh.finish();
        String s = "";
        for (int i = 0; i < buf.length; i++) {
            s += bits.toHexB(buf[i]);
        }
        return s.toLowerCase();
    }

}
