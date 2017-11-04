package cry;

import java.util.ArrayList;
import java.util.List;

/**
 * the base64 (rfc4648) encoding
 *
 * @author matecsaba
 */
public class cryBase64 {

    private cryBase64() {
    }

    /**
     * default line length in cleartext form
     */
    public static final int maxIn = 60;

    private static final String tab = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

    private static final String pad = "=";

    private static String encodePart(byte[] buf, int ofs, int siz) {
        if (siz < 1) {
            return "";
        }
        if (siz > 3) {
            return "????";
        }
        int o = 0;
        int p = 24;
        for (int i = 0; i < siz; i++) {
            o = ((buf[ofs + i]) & 0xff) | (o << 8);
            p = p - 8;
        }
        o = o << p;
        String s = "";
        for (int i = 0; i < 4; i++) {
            p = o & 0x3f;
            o = o >>> 6;
            s = tab.substring(p, p + 1) + s;
        }
        switch (siz) {
            case 1:
                return s.substring(0, 2) + pad + pad;
            case 2:
                return s.substring(0, 3) + pad;
            default:
                return s;
        }
    }

    private static byte[] decodePart(String str) {
        if (str.length() != 4) {
            return null;
        }
        int o = 0;
        int p = 0;
        for (int i = 0; i < 4; i++) {
            o = (o << 6);
            String a = str.substring(i, i + 1);
            if (a.equals(pad)) {
                continue;
            }
            int q = tab.indexOf(a);
            if (q < 0) {
                return null;
            }
            o = o | q;
            p = p + 6;
        }
        p = p / 8;
        byte[] buf = new byte[p];
        o = o >>> ((3 - p) * 8);
        for (int i = 0; i < p; i++) {
            buf[p - i - 1] = (byte) (o & 0xff);
            o = o >>> 8;
        }
        return buf;
    }

    /**
     * encode bytes
     *
     * @param buf buffer to encode
     * @param ofs offset where start
     * @param siz bytes to encode
     * @return encoded data
     */
    public static String encodeBytes(byte[] buf, int ofs, int siz) {
        final int max = 3;
        String s = "";
        for (;;) {
            if (siz < 1) {
                break;
            }
            int i = siz;
            if (i > max) {
                i = max;
            }
            s += encodePart(buf, ofs, i);
            ofs += i;
            siz -= i;
        }
        return s;
    }

    /**
     * encode bytes
     *
     * @param buf buffer to encode
     * @return encoded data
     */
    public static String encodeBytes(byte[] buf) {
        return encodeBytes(buf, 0, buf.length);
    }

    /**
     * encode string
     *
     * @param str string to encode
     * @return decoded data
     */
    public static String encodeString(String str) {
        byte[] buf = str.getBytes();
        return encodeBytes(buf, 0, buf.length);
    }

    /**
     * decode string
     *
     * @param str string to decode
     * @return decoded data
     */
    public static byte[] decodeBytes(String str) {
        final int max = 4;
        List<Byte> buf = new ArrayList<Byte>();
        for (;;) {
            int siz = str.length();
            if (siz > max) {
                siz = max;
            }
            if (siz < 1) {
                break;
            }
            byte[] prt = decodePart(str.substring(0, siz));
            str = str.substring(siz, str.length());
            if (prt == null) {
                return null;
            }
            for (int i = 0; i < prt.length; i++) {
                buf.add(prt[i]);
            }
        }
        byte[] res = new byte[buf.size()];
        for (int i = 0; i < res.length; i++) {
            res[i] = buf.get(i);
        }
        return res;
    }

    /**
     * decode string
     *
     * @param str string to decode
     * @return decoded data
     */
    public static String decodeString(String str) {
        byte[] res = decodeBytes(str);
        if (res == null) {
            return null;
        }
        return new String(res);
    }

}
