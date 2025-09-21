package org.freertr.enc;

import java.util.ArrayList;
import java.util.List;
import org.freertr.util.bits;

/**
 * a 8 bit to 7 bit filter
 *
 * @author matecsaba
 */
public class enc7bit {

    /**
     * convert to bin
     *
     * @param l list to append
     * @param buf buffer
     * @param beg beginning
     */
    public static void buf2bin(List<String> l, byte[] buf, int beg) {
        String s = bits.toHexD(beg) + ":";
        for (int ps = 0; ps < buf.length;) {
            s += " " + bits.toBinB(buf[ps]);
            ps++;
            beg++;
            if ((ps & 3) != 0) {
                continue;
            }
            l.add(s);
            s = bits.toHexD(beg) + ":";
        }
        l.add(s);
    }

    /**
     * convert to hex
     *
     * @param l list to append
     * @param buf buffer
     * @param beg beginning
     * @param tab tabulator
     */
    public static void buf2hex(List<String> l, byte[] buf, int beg, String tab) {
        String s = tab + bits.toHexD(beg) + ":";
        for (int ps = 0; ps < buf.length;) {
            s += " " + bits.toHexB(buf[ps]);
            ps++;
            beg++;
            if ((ps & 3) == 0) {
                s += " ";
            }
            if ((ps & 15) != 0) {
                continue;
            }
            l.add(s);
            s = tab + bits.toHexD(beg) + ":";
        }
        l.add(s);
    }

    /**
     * default constructor
     */
    private enc7bit() {
    }

    /**
     * check if a byte is valid
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
                if (!pok) {
                    continue;
                }
                pok = false;
                r += " ";
                continue;
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
            res.add(beg + s.trim());
            s = "";
        }
        if (s.length() < 1) {
            return res;
        }
        res.add(beg + s.trim());
        return res;
    }

    /**
     * check if extended byte
     *
     * @param i byte to check
     * @return true if extended false if not
     */
    public static boolean extendedByte(int i) {
        if (i <= 0x20) {
            return true;
        }
        if (i >= 0x7f) {
            return true;
        }
        return false;
    }

    /**
     * decode extended byte
     *
     * @param i byte to decode
     * @return decoded byte
     */
    public static byte decodeExtByte(int i) {
        if (checkByte(i)) {
            return (byte) i;
        }
        if (!extendedByte(i)) {
            return (byte) i;
        }
        switch (i) {
            case 0x82:
                return ',';
            case 0x83:
                return 'f';
            case 0x84:
                return ',';
            case 0x85:
                return '.';
            case 0x86:
                return '+';
            case 0x89:
                return '%';
            case 0x8a:
                return 'S';
            case 0x8b:
                return '<';
            case 0x8c:
                return 'O';
            case 0x8e:
                return 'Z';
            case 0x91:
                return '\'';
            case 0x92:
                return '\'';
            case 0x93:
                return '"';
            case 0x94:
                return '"';
            case 0x95:
                return ' ';
            case 0x96:
                return '-';
            case 0x97:
                return '-';
            case 0x98:
                return '~';
            case 0x90:
                return 'T';
            case 0x9a:
                return 's';
            case 0x9b:
                return '>';
            case 0x9c:
                return 'p';
            case 0x9e:
                return 'z';
            case 0x9f:
                return 'Y';
            case 0xa0:
                return ' ';
            case 0xa1:
                return '!';
            case 0xa2:
                return 'C';
            case 0xa3:
                return 'L';
            case 0xa4:
                return '*';
            case 0xa5:
                return 'Y';
            case 0xa7:
                return '$';
            case 0xa8:
                return '"';
            case 0xa9:
                return 'c';
            case 0xae:
                return 'r';
            case 0xaf:
                return '_';
            case 0xb0:
                return 'o';
            case 0xb2:
                return '2';
            case 0xb3:
                return '3';
            case 0xb4:
                return '\'';
            case 0xb5:
                return 'u';
            case 0xb7:
                return '.';
            case 0xb8:
                return ',';
            case 0xb9:
                return '1';
            case 0xbc:
                return '_';
            case 0xbf:
                return '?';
            case 0xc0:
                return 'A';
            case 0xc1:
                return 'A';
            case 0xc2:
                return 'A';
            case 0xc3:
                return 'A';
            case 0xc4:
                return 'A';
            case 0xc5:
                return 'A';
            case 0xc6:
                return 'A';
            case 0xc7:
                return 'C';
            case 0xc8:
                return 'E';
            case 0xc9:
                return 'E';
            case 0xca:
                return 'E';
            case 0xcb:
                return 'E';
            case 0xcc:
                return 'I';
            case 0xcd:
                return 'I';
            case 0xce:
                return 'I';
            case 0xcf:
                return 'I';
            case 0xd0:
                return 'D';
            case 0xd1:
                return 'N';
            case 0xd2:
                return 'O';
            case 0xd3:
                return 'O';
            case 0xd4:
                return 'O';
            case 0xd5:
                return 'O';
            case 0xd6:
                return 'O';
            case 0xd7:
                return 'x';
            case 0xd8:
                return 'O';
            case 0xd9:
                return 'U';
            case 0xda:
                return 'U';
            case 0xdb:
                return 'U';
            case 0xdc:
                return 'U';
            case 0xdd:
                return 'Y';
            case 0xde:
                return '_';
            case 0xdf:
                return 'B';
            case 0xe0:
                return 'a';
            case 0xe1:
                return 'a';
            case 0xe2:
                return 'a';
            case 0xe3:
                return 'a';
            case 0xe4:
                return 'a';
            case 0xe5:
                return 'a';
            case 0xe6:
                return 'a';
            case 0xe7:
                return 'c';
            case 0xe8:
                return 'e';
            case 0xe9:
                return 'e';
            case 0xea:
                return 'e';
            case 0xeb:
                return 'e';
            case 0xec:
                return 'i';
            case 0xed:
                return 'i';
            case 0xee:
                return 'i';
            case 0xef:
                return 'i';
            case 0xf0:
                return 'd';
            case 0xf1:
                return 'n';
            case 0xf2:
                return 'o';
            case 0xf3:
                return 'o';
            case 0xf4:
                return 'o';
            case 0xf5:
                return 'o';
            case 0xf6:
                return 'o';
            case 0xf7:
                return '%';
            case 0xf8:
                return 'o';
            case 0xf9:
                return 'u';
            case 0xfa:
                return 'u';
            case 0xfb:
                return 'u';
            case 0xfc:
                return 'u';
            case 0xfd:
                return 'y';
            case 0xfe:
                return '_';
            case 0xff:
                return 'y';
            default:
                return '?';
        }
    }

    /**
     * decode extended ascii string
     *
     * @param s string to decode
     * @return decoded string
     */
    public final static String decodeExtStr(String s) {
        byte[] org = s.getBytes();
        byte[] res = new byte[org.length];
        for (int i = 0; i < org.length; i++) {
            byte c = org[i];
            byte r = decodeExtByte(c);
            res[i] = r;
        }
        return new String(res);
    }

    /**
     * convert a string to hacker writing
     *
     * @param lst list to convert
     * @return converted list
     */
    public final static List<String> decodeExtLst(List<String> lst) {
        List<String> res = new ArrayList<String>();
        for (int i = 0; i < lst.size(); i++) {
            String s = lst.get(i);
            String a = decodeExtStr(s);
            res.add(a);
        }
        return res;
    }

    /**
     * hack one character
     *
     * @param c character to hack
     * @return hacked character
     */
    public final static byte doHackOneChar(byte c) {
        switch (c) {
            case '0':
                return 'o';
            case '1':
                return 'i';
            case 'i':
                return '1';
            case 'l':
                return '1';
            case 'o':
                return '0';
            case 'e':
                return '3';
            case '3':
                return 'E';
            case '2':
                return 'Z';
            case 's':
                return '$';
            case 'z':
                return '2';
            case 'a':
                return '4';
            case '4':
                return 'A';
            case 't':
                return '7';
            case '7':
                return 'T';
            case '8':
                return 'B';
            case 'b':
                return '8';
            case 'v':
                return 'w';
            case 'w':
                return 'v';
            default:
                if (extendedByte(c)) {
                    return decodeExtByte(c);
                } else {
                    return doOneByte(c);
                }
        }
    }

    /**
     * convert a string to hacker writing
     *
     * @param s string to convert
     * @return converted string
     */
    public final static String toHackedStr(String s) {
        byte[] org = s.getBytes();
        byte[] res = new byte[org.length];
        for (int i = 0; i < org.length; i++) {
            byte c = org[i];
            byte r = doHackOneChar(c);
            res[i] = r;
        }
        return new String(res);
    }

    /**
     * convert a string to hacker writing
     *
     * @param lst list to convert
     * @return converted list
     */
    public final static List<String> toHackedLst(List<String> lst) {
        List<String> res = new ArrayList<String>();
        for (int i = 0; i < lst.size(); i++) {
            String s = lst.get(i);
            String a = toHackedStr(s);
            res.add(a);
        }
        return res;
    }

}
