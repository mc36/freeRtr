package cry;

import java.io.File;
import java.io.RandomAccessFile;
import java.math.BigInteger;
import java.util.List;
import util.bits;

/**
 * crypto utils
 *
 * @author matecsaba
 */
public class cryUtils {

    private cryUtils() {
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
        for (int i = 0; i < buf.length; i++) {
            buf[i] = dat[i + 1];
        }
        return buf;
    }

    /**
     * convert buffer to big unsigned integer
     *
     * @param dat buffer
     * @return big integer
     */
    public static BigInteger buf2bigUint(byte[] dat) {
        byte[] buf = new byte[dat.length + 1];
        buf[0] = 0;
        bits.byteCopy(dat, 0, buf, 1, dat.length);
        return new BigInteger(buf);
    }

    /**
     * generate hash from text
     *
     * @param hsh hash to update
     * @param src lines to add
     * @return false on success, true on error
     */
    public static boolean hashText(cryHashGeneric hsh, List<String> src) {
        if (src == null) {
            return true;
        }
        for (int i = 0; i < src.size(); i++) {
            String s = src.get(i);
            byte[] buf = new byte[4];
            bits.msbPutD(buf, 0, s.length());
            hsh.update(buf);
            hsh.update(s.getBytes());
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
        long siz = 0;
        RandomAccessFile fr;
        try {
            fr = new RandomAccessFile(src, "r");
            siz = fr.length();
        } catch (Exception e) {
            return true;
        }
        for (; pos < siz;) {
            final int max = 8192;
            long rndl = siz - pos;
            if (rndl > max) {
                rndl = max;
            }
            pos += rndl;
            int rndi = (int) rndl;
            byte buf[] = new byte[rndi];
            try {
                fr.read(buf, 0, rndi);
            } catch (Exception e) {
                return true;
            }
            hsh.update(buf);
        }
        try {
            fr.close();
        } catch (Exception e) {
            return true;
        }
        return false;
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
