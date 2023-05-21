package net.freertr.enc;

/**
 * a 8 bit to 7 bit filter
 *
 * @author mc36
 */
public class enc7bit {

    private enc7bit() {
    }

    /**
     * convert one character
     *
     * @param i byte to convert
     * @return converted byte
     */
    public static int doOneByte(int i) {
        if (i < 0x20) {
            return 0x20;
        }
        if (i > 0x7f) {
            return 0x20;
        }
        return i;
    }

    /**
     * convert one character
     *
     * @param c char to convert
     * @return converted char
     */
    public static char doOneChar(char c) {
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
    public static String doOneString(String s) {
        byte[] b = s.getBytes();
        String r = "";
        for (int i = 0; i < s.length(); i++) {
            int c = doOneByte(b[i]);
            r += "" + (char) c;
        }
        return r;
    }

}
