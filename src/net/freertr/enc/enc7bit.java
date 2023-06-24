package net.freertr.enc;

import java.util.ArrayList;
import java.util.List;

/**
 * a 8 bit to 7 bit filter
 *
 * @author matecsaba
 */
public class enc7bit {

    /**
     * default constructor
     */
    private enc7bit() {
    }

    /**
     * check if a byte is valud
     *
     * @param i byte to check
     * @return true if ok false otherwise
     */
    public static boolean checkByte(int i) {
        if (i == 0x20) {
            return true;
        }
        if (i < 0x20) {
            return false;
        }
        if (i >= 0x7f) {
            return false;
        }
        return true;
    }

    /**
     * convert one character
     *
     * @param i byte to convert
     * @return converted byte
     */
    public static byte doOneByte(int i) {
        if (checkByte(i)) {
            return (byte) (i & 0x7f);
        } else {
            return '_';
        }
    }

    /**
     * convert one character
     *
     * @param c char to convert
     * @return converted char
     */
    public final static char doOneChar(char c) {
        int i = (byte) c;
        int r = doOneByte((char) i);
        return (char) r;
    }

    /**
     * convert one string
     *
     * @param s string to convert
     * @return converted string
     */
    public final static String doOneString(String s) {
        if (s == null) {
            return "";
        }
        byte[] buf = s.getBytes();
        String r = "";
        boolean pok = true;
        for (int i = 0; i < s.length(); i++) {
            int o = buf[i];
            if (!checkByte(o)) {
                pok = false;
                if (!pok) {
                    continue;
                }
            }
            r += "" + (char) doOneByte(o);
            pok = true;
        }
        return r.trim();
    }

    /**
     * convert one array to string
     *
     * @param buf bytes to convert
     * @param beg beginning to prepend
     * @return converted array
     */
    public final static List<String> doOneArray(byte[] buf, String beg) {
        List<String> res = new ArrayList<String>();
        String s = "";
        for (int i = 0; i < buf.length; i++) {
            byte o = buf[i];
            if (checkByte(o)) {
                s += (char) o;
                continue;
            }
            if (s.length() < 1) {
                continue;
            }
            res.add(beg + res.size() + ":" + s.trim());
            s = "";
        }
        return res;
    }

}
