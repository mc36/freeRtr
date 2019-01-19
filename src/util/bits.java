package util;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.io.RandomAccessFile;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.Locale;
import java.util.Random;
import java.util.TimeZone;

/**
 * bit manipulations
 *
 * @author matecsaba
 */
public class bits {

    private bits() {
    }

    private static Random randomSeed = new Random();

    private static int randomUsed = 0;

    private static void randomUpdate() {
        randomUsed++;
        if (randomUsed < 1024) {
            return;
        }
        randomUsed = 0;
        randomSeed = new Random();
    }

    /**
     * get random seed
     *
     * @return random seed
     */
    public static Random randomS() {
        randomUpdate();
        return randomSeed;
    }

    /**
     * generate random number
     *
     * @param min lower boundary (inclusive)
     * @param max upper boundary (exclusive)
     * @return generated number
     */
    public static int random(int min, int max) {
        randomUpdate();
        max -= min;
        return min + (randomSeed).nextInt(max);
    }

    /**
     * generate random byte
     *
     * @return generated number
     */
    public static int randomB() {
        return random(0, 0x7f);
    }

    /**
     * generate random word
     *
     * @return generated number
     */
    public static int randomW() {
        return random(0, 0x7f00);
    }

    /**
     * generate random double word
     *
     * @return generated number
     */
    public static int randomD() {
        return random(0, 0x7fffffff);
    }

    /**
     * generate random quad word
     *
     * @return generated number
     */
    public static long randomQ() {
        long v1 = random(0, 0x7fffffff);
        long v2 = random(0, 0x7fffffff);
        return (v1 << 32) | v2;
    }

    /**
     * read u8 from buffer
     *
     * @param buf buffer read from
     * @param ofs offset where read
     * @return value read
     */
    public static int getByte(byte[] buf, int ofs) {
        return buf[ofs] & 0xff;
    }

    /**
     * write u8 to buffer
     *
     * @param buf buffer write to
     * @param ofs offset where write
     * @param val value to write
     */
    public static void putByte(byte[] buf, int ofs, int val) {
        buf[ofs] = (byte) val;
    }

    /**
     * read MSB u16 from buffer
     *
     * @param buf buffer read from
     * @param ofs offset where read
     * @return value read
     */
    public static int msbGetW(byte[] buf, int ofs) {
        return ((buf[ofs] & 0xff) << 8) | (buf[ofs + 1] & 0xff);
    }

    /**
     * read MSB u32 from buffer
     *
     * @param buf buf buffer read from
     * @param ofs offset where read
     * @return value read
     */
    public static int msbGetD(byte[] buf, int ofs) {
        return ((buf[ofs + 0] & 0xff) << 24) | ((buf[ofs + 1] & 0xff) << 16)
                | ((buf[ofs + 2] & 0xff) << 8) | (buf[ofs + 3] & 0xff);
    }

    /**
     * read MSB u64 from buffer
     *
     * @param buf buf buffer read from
     * @param ofs offset where read
     * @return value read
     */
    public static long msbGetQ(byte[] buf, int ofs) {
        return ((long) (buf[ofs + 0] & 0xff) << 56)
                | ((long) (buf[ofs + 1] & 0xff) << 48)
                | ((long) (buf[ofs + 2] & 0xff) << 40)
                | ((long) (buf[ofs + 3] & 0xff) << 32)
                | ((long) (buf[ofs + 4] & 0xff) << 24)
                | ((buf[ofs + 5] & 0xff) << 16) | ((buf[ofs + 6] & 0xff) << 8)
                | (buf[ofs + 7] & 0xff);
    }

    /**
     * write MSB u16 to buffer
     *
     * @param buf buffer write to
     * @param ofs offset where write
     * @param val value to write
     */
    public static void msbPutW(byte[] buf, int ofs, int val) {
        buf[ofs + 0] = (byte) (val >>> 8);
        buf[ofs + 1] = (byte) val;
    }

    /**
     * write MSB u32 to buffer
     *
     * @param buf buffer write to
     * @param ofs offset where write
     * @param val value to write
     */
    public static void msbPutD(byte[] buf, int ofs, int val) {
        buf[ofs + 0] = (byte) (val >>> 24);
        buf[ofs + 1] = (byte) (val >>> 16);
        buf[ofs + 2] = (byte) (val >>> 8);
        buf[ofs + 3] = (byte) val;
    }

    /**
     * write MSB u64 to buffer
     *
     * @param buf buffer write to
     * @param ofs offset where write
     * @param val value to write
     */
    public static void msbPutQ(byte[] buf, int ofs, long val) {
        buf[ofs + 0] = (byte) (val >>> 56);
        buf[ofs + 1] = (byte) (val >>> 48);
        buf[ofs + 2] = (byte) (val >>> 40);
        buf[ofs + 3] = (byte) (val >>> 32);
        buf[ofs + 4] = (byte) (val >>> 24);
        buf[ofs + 5] = (byte) (val >>> 16);
        buf[ofs + 6] = (byte) (val >>> 8);
        buf[ofs + 7] = (byte) val;
    }

    /**
     * read LSB u16 from buffer
     *
     * @param buf buffer read from
     * @param ofs offset where read
     * @return value read
     */
    public static int lsbGetW(byte[] buf, int ofs) {
        return (buf[ofs] & 0xff) | ((buf[ofs + 1] & 0xff) << 8);
    }

    /**
     * read LSB u32 from buffer
     *
     * @param buf buffer read from
     * @param ofs offset where read
     * @return value read
     */
    public static int lsbGetD(byte[] buf, int ofs) {
        return (buf[ofs + 0] & 0xff) | ((buf[ofs + 1] & 0xff) << 8)
                | ((buf[ofs + 2] & 0xff) << 16) | ((buf[ofs + 3] & 0xff) << 24);
    }

    /**
     * read LSB u64 from buffer
     *
     * @param buf buffer read from
     * @param ofs offset where read
     * @return value read
     */
    public static long lsbGetQ(byte[] buf, int ofs) {
        return (buf[ofs + 0] & 0xff) | ((buf[ofs + 1] & 0xff) << 8)
                | ((buf[ofs + 2] & 0xff) << 16)
                | ((long) (buf[ofs + 3] & 0xff) << 24)
                | ((long) (buf[ofs + 4] & 0xff) << 32)
                | ((long) (buf[ofs + 5] & 0xff) << 40)
                | ((long) (buf[ofs + 6] & 0xff) << 48)
                | ((long) (buf[ofs + 7] & 0xff) << 56);
    }

    /**
     * write LSB u16 to buffer
     *
     * @param buf buffer write to
     * @param ofs offset where write
     * @param val value to write
     */
    public static void lsbPutW(byte[] buf, int ofs, int val) {
        buf[ofs] = (byte) val;
        buf[ofs + 1] = (byte) (val >>> 8);
    }

    /**
     * write LSB u32 to buffer
     *
     * @param buf buffer write to
     * @param ofs offset where write
     * @param val value to write
     */
    public static void lsbPutD(byte[] buf, int ofs, int val) {
        buf[ofs + 0] = (byte) val;
        buf[ofs + 1] = (byte) (val >>> 8);
        buf[ofs + 2] = (byte) (val >>> 16);
        buf[ofs + 3] = (byte) (val >>> 24);
    }

    /**
     * write LSB u64 to buffer
     *
     * @param buf buffer write to
     * @param ofs offset where write
     * @param val value to write
     */
    public static void lsbPutQ(byte[] buf, int ofs, long val) {
        buf[ofs + 0] = (byte) val;
        buf[ofs + 1] = (byte) (val >>> 8);
        buf[ofs + 2] = (byte) (val >>> 16);
        buf[ofs + 3] = (byte) (val >>> 24);
        buf[ofs + 4] = (byte) (val >>> 32);
        buf[ofs + 5] = (byte) (val >>> 40);
        buf[ofs + 6] = (byte) (val >>> 48);
        buf[ofs + 7] = (byte) (val >>> 56);
    }

    /**
     * convert from hex
     *
     * @param s hex string
     * @return converted
     */
    public static int fromHex(String s) {
        try {
            return Integer.parseInt(s, 16);
        } catch (Exception e) {
            return 0;
        }
    }

    /**
     * convert bytes to hex string
     *
     * @param buf bytes
     * @return hex representation of bytes
     */
    public static String toHex(byte[] buf) {
        String s = "";
        for (int i = 0; i < buf.length; i++) {
            s += toHexB(buf[i]);
        }
        return s;
    }

    /**
     * convert u8 to hex string
     *
     * @param val value to convert
     * @return hex representation of value
     */
    public static String toHexB(int val) {
        String a = Integer.toHexString(val & 0xff);
        while (a.length() < 2) {
            a = "0" + a;
        }
        return a;
    }

    /**
     * convert u16 to hex string
     *
     * @param val value to convert
     * @return hex representation of value
     */
    public static String toHexW(int val) {
        String a = Integer.toHexString(val & 0xffff);
        while (a.length() < 4) {
            a = "0" + a;
        }
        return a;
    }

    /**
     * convert u16 to hex string
     *
     * @param val value to convert
     * @return hex representation of value
     */
    public static String toHexD(int val) {
        String a = Integer.toHexString(val);
        while (a.length() < 8) {
            a = "0" + a;
        }
        return a;
    }

    /**
     * dump buffer to string
     *
     * @param buf buffer to dump
     * @param ofs where to start
     * @param len bytes to dump
     * @return string representing the dump
     */
    public static String byteDump(byte[] buf, int ofs, int len) {
        if (buf == null) {
            return "n/a";
        }
        String s = "";
        if (len < 0) {
            len = buf.length;
        }
        for (int i = 0; i < len; i++) {
            s = s + " " + toHexB(buf[ofs + i]);
        }
        return s;
    }

    /**
     * save bytes to file
     *
     * @param overwrite overwrite file
     * @param buf buffer to save
     * @param trg filename to use
     * @return false on success, true on error
     */
    public static boolean byteSave(boolean overwrite, byte[] buf, String trg) {
        try {
            RandomAccessFile fr = new RandomAccessFile(trg, "rw");
            if (overwrite) {
                fr.setLength(0);
            } else {
                fr.seek(fr.length());
            }
            fr.write(buf, 0, buf.length);
            fr.close();
            return false;
        } catch (Exception e) {
            return true;
        }
    }

    /**
     * copy objects from one buffer to another
     *
     * @param srcB source buffer
     * @param srcO source offset
     * @param trgB target buffer
     * @param trgO target offset
     * @param len bytes to copy
     */
    public static void objCopy(Object[] srcB, int srcO, Object[] trgB, int trgO, int len) {
        if (len > 128) {
            System.arraycopy(srcB, srcO, trgB, trgO, len);
            return;
        }
        if (srcO >= trgO) {
            for (int i = 0; i < len; i++) {
                trgB[trgO + i] = srcB[srcO + i];
            }
        } else {
            for (int i = len - 1; i >= 0; i--) {
                trgB[trgO + i] = srcB[srcO + i];
            }
        }
    }

    /**
     * copy bytes from one buffer to another
     *
     * @param srcB source buffer
     * @param srcO source offset
     * @param trgB target buffer
     * @param trgO target offset
     * @param len bytes to copy
     */
    public static void byteCopy(byte[] srcB, int srcO, byte[] trgB, int trgO, int len) {
        if (len > 128) {
            System.arraycopy(srcB, srcO, trgB, trgO, len);
            return;
        }
        if (srcO >= trgO) {
            for (int i = 0; i < len; i++) {
                trgB[trgO + i] = srcB[srcO + i];
            }
        } else {
            for (int i = len - 1; i >= 0; i--) {
                trgB[trgO + i] = srcB[srcO + i];
            }
        }
    }

    /**
     * fill bytes in buffer
     *
     * @param buf buffer to work in
     * @param ofs offset where to start
     * @param len bytes to fill
     * @param val value of filler bytes
     */
    public static void byteFill(byte[] buf, int ofs, int len, int val) {
        for (int i = 0; i < len; i++) {
            buf[ofs + i] = (byte) val;
        }
    }

    /**
     * compare bytes in different buffers
     *
     * @param buf1 buffer#1 to use
     * @param ofs1 offset within buffer#1
     * @param buf2 buffer#2 to use
     * @param ofs2 offset within buffer#2
     * @param len bytes to compare
     * @return -1 if buf1 less than buf2, 0 if buf1=buf2, +1 if buf1 greater
     * than buf2
     */
    public static int byteComp(byte[] buf1, int ofs1, byte[] buf2, int ofs2, int len) {
        for (int i = 0; i < len; i++) {
            int v1 = buf1[ofs1 + i] & 0xff;
            int v2 = buf2[ofs2 + i] & 0xff;
            if (v1 < v2) {
                return -1;
            }
            if (v1 > v2) {
                return +1;
            }
        }
        return 0;
    }

    /**
     * concatenate bytes
     *
     * @param b1 buffer 1
     * @param b2 buffer 2
     * @return concatenated buffer
     */
    public static byte[] byteConcat(byte[] b1, byte[] b2) {
        byte[] buf = new byte[b1.length + b2.length];
        byteCopy(b1, 0, buf, 0, b1.length);
        byteCopy(b2, 0, buf, b1.length, b2.length);
        return buf;
    }

    /**
     * calculate ip checksum for bytes
     *
     * @param buf buffer to work in
     * @param ofs offset where to start
     * @param len bytes to calculate
     * @param sum initial value of checksum (0 for new)
     * @return value of checksum
     */
    public static int byteIPsum(byte[] buf, int ofs, int len, int sum) {
        for (int i = 0; i < (len / 2); i++) {
            sum += lsbGetW(buf, ofs + i * 2);
        }
        if ((len & 1) != 0) {
            sum += getByte(buf, ofs + len - 1);
        }
        while ((sum & 0xffff) != sum) {
            sum = (sum & 0xffff) + (sum >>> 16);
        }
        return sum;
    }

    /**
     * calculate iso checksum for bytes
     *
     * @param buf buffer to work in
     * @param ofs offset where to start
     * @param len bytes to calculate
     * @param sum offset of checksum from initial offset
     * @return value of checksum
     */
    public static int byteISOsum(byte[] buf, int ofs, int len, int sum) {
        int c0 = 0;
        int c1 = 0;
        int old = msbGetW(buf, ofs + sum);
        msbPutW(buf, ofs + sum, 0);
        for (int i = 0; i < len; i++) {
            c0 += getByte(buf, ofs + i);
            c1 += c0;
            c0 %= 255;
            c1 %= 255;
        }
        msbPutW(buf, ofs + sum, old);
        int x = ((len - sum - 1) * c0 - c1) % 255;
        if (x <= 0) {
            x += 255;
        }
        int y = 510 - c0 - x;
        if (y > 255) {
            y -= 255;
        }
        return (x << 8) | (y & 0xFF);
    }

    /**
     * bit values in u32
     */
    public final static int[] bitVals = {
        0x00000001, 0x00000002, 0x00000004, 0x00000008,
        0x00000010, 0x00000020, 0x00000040, 0x00000080,
        0x00000100, 0x00000200, 0x00000400, 0x00000800,
        0x00001000, 0x00002000, 0x00004000, 0x00008000,
        0x00010000, 0x00020000, 0x00040000, 0x00080000,
        0x00100000, 0x00200000, 0x00400000, 0x00800000,
        0x01000000, 0x02000000, 0x04000000, 0x08000000,
        0x10000000, 0x20000000, 0x40000000, 0x80000000
    };

    /**
     * set bit value
     *
     * @param buf buffer to use
     * @param ofs offset where subbuffer begins
     * @param bit bit number to set
     * @param val bit value
     */
    public static void bitSet(byte[] buf, int ofs, int bit, boolean val) {
        ofs += bit / 8;
        bit &= 7;
        if (val) {
            buf[ofs] |= bitVals[bit];
        } else {
            buf[ofs] &= -bitVals[bit] - 1;
        }
    }

    /**
     * get bit value
     *
     * @param buf buffer to use
     * @param ofs offset where subbuffer begins
     * @param bit bit number to get
     * @return bit value
     */
    public static boolean bitGet(byte[] buf, int ofs, int bit) {
        ofs += bit / 8;
        bit &= 7;
        return ((buf[ofs] & bitVals[bit]) != 0);
    }

    /**
     * or bit value
     *
     * @param buf buffer to use
     * @param ofs offset where subbuffer begins
     * @param bit bit number to use
     * @param val bit value
     */
    public static void bitOr(byte[] buf, int ofs, int bit, boolean val) {
        if (val) {
            bitSet(buf, ofs, bit, true);
        }
    }

    /**
     * and bit value
     *
     * @param buf buffer to use
     * @param ofs offset where subbuffer begins
     * @param bit bit number to use
     * @param val bit value
     */
    public static void bitAnd(byte[] buf, int ofs, int bit, boolean val) {
        if (!val) {
            bitSet(buf, ofs, bit, false);
        }
    }

    /**
     * xor bit value
     *
     * @param buf buffer to use
     * @param ofs offset where subbuffer begins
     * @param bit bit number to use
     * @param val bit value
     */
    public static void bitXor(byte[] buf, int ofs, int bit, boolean val) {
        boolean od = bitGet(buf, ofs, bit);
        boolean nw = od ^ val;
        if (nw != val) {
            bitSet(buf, ofs, bit, nw);
        }
    }

    /**
     * not bit value
     *
     * @param buf buffer to use
     * @param ofs offset where subbuffer begins
     * @param bit bit number to use
     */
    public static void bitNot(byte[] buf, int ofs, int bit) {
        bitSet(buf, ofs, bit, !bitGet(buf, ofs, bit));
    }

    /**
     * convert seconds to string
     *
     * @param i time in seconds
     * @return time in string
     */
    public static String timeDump(long i) {
        final int secPerHour = 3600;
        final int secPerDay = secPerHour * 24;
        final int secPerYear = secPerDay * 365;
        String pref = "";
        if (i < 0) {
            pref = "-";
            i = -i;
        }
        if (i < secPerDay) {
            String a = padBeg((i / secPerHour) + "", 2, "0") + ":";
            i %= secPerHour;
            return pref + a + padBeg((i / 60) + "", 2, "0") + ":"
                    + padBeg((i % 60) + "", 2, "0");
        }
        if (i < secPerYear) {
            i /= secPerHour;
            return pref + (i / 24) + "d" + (i % 24) + "h";
        }
        i /= secPerDay;
        long o = i / 365;
        if (o > 18) {
            return pref + "never";
        } else {
            return pref + o + "y" + (i % 365) + "d";
        }
    }

    /**
     * convert time past to string
     *
     * @param i time in past
     * @return time past in string
     */
    public static String timePast(long i) {
        return timeDump((getTime() - i) / 1000);
    }

    /**
     * convert time left to string
     *
     * @param i time in future
     * @return time left in string
     */
    public static String timeLeft(long i) {
        return timeDump((i - getTime()) / 1000);
    }

    /**
     * get millisecs since 1970 jan 1
     *
     * @return current time
     */
    public static long getTime() {
        return new Date().getTime();
    }

    /**
     * get calendar
     *
     * @param zoNam zone name
     * @return calendar
     */
    public static Calendar getCalendar(String zoNam) {
        if (zoNam == null) {
            zoNam = "Z";
        }
        TimeZone tz = TimeZone.getTimeZone(zoNam);
        return new GregorianCalendar(tz);
    }

    /**
     * convert time to string
     *
     * @param zoNam time zone name
     * @param tim time in milliseconds
     * @param prt part to read: 1=date, 2=time, 3=both, 4=mail
     * @return string showing this time
     */
    public static String time2str(String zoNam, long tim, int prt) {
        switch (prt) {
            case 1:
                return time2num(zoNam, tim, 1) + "-" + padBeg(time2num(zoNam, tim, 2) + "", 2, "0") + "-" + padBeg(time2num(zoNam, tim, 3) + "", 2, "0");
            case 2:
                return padBeg(time2num(zoNam, tim, 4) + "", 2, "0") + ":" + padBeg(time2num(zoNam, tim, 5) + "", 2, "0") + ":" + padBeg(time2num(zoNam, tim, 6) + "", 2, "0");
            case 3:
                return time2str(zoNam, tim, 1) + " " + time2str(zoNam, tim, 2);
            case 4:
                DateFormat dat = new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss Z", Locale.US);
                dat.setTimeZone(getCalendar(zoNam).getTimeZone());
                return dat.format(new Date(tim));
            case 5:
                return padBeg(time2num(zoNam, tim, 4) + "", 2, "0") + padBeg(time2num(zoNam, tim, 5) + "", 2, "0") + padBeg(time2num(zoNam, tim, 6) + "", 2, "0");
            default:
                return null;
        }
    }

    /**
     * convert time to number
     *
     * @param zoNam time zone name
     * @param tim time in milliseconds
     * @param prt part to read: 1=year, 2=month, 3=day, 4=hour, 5=min, 6=sec,
     * 7=zoneOffset
     * @return integer of specified part
     */
    public static int time2num(String zoNam, long tim, int prt) {
        Calendar cal = getCalendar(zoNam);
        cal.setTime(new Date(tim));
        switch (prt) {
            case 1:
                return cal.get(Calendar.YEAR);
            case 2:
                return cal.get(Calendar.MONTH) + 1;
            case 3:
                return cal.get(Calendar.DAY_OF_MONTH);
            case 4:
                return cal.get(Calendar.HOUR_OF_DAY);
            case 5:
                return cal.get(Calendar.MINUTE);
            case 6:
                return cal.get(Calendar.SECOND);
            case 7:
                return cal.get(Calendar.ZONE_OFFSET);
            default:
                return 0;
        }
    }

    /**
     * convert string to time
     *
     * @param zoNam time zone name
     * @param str string to convert
     * @return time point
     */
    public static long str2time(String zoNam, String str) {
        str = str.replace(" ", "");
        str = str.replace(":", "");
        str = str.replace("-", "");
        str = str + "00000000000000";
        Calendar cal = getCalendar(zoNam);
        cal.set(Calendar.YEAR, str2num(str.substring(0, 4)));
        cal.set(Calendar.MONTH, str2num(str.substring(4, 6)) - 1);
        cal.set(Calendar.DAY_OF_MONTH, str2num(str.substring(6, 8)));
        cal.set(Calendar.HOUR_OF_DAY, str2num(str.substring(8, 10)));
        cal.set(Calendar.MINUTE, str2num(str.substring(10, 12)));
        cal.set(Calendar.SECOND, str2num(str.substring(12, 14)));
        cal.set(Calendar.MILLISECOND, 0);
        return cal.getTime().getTime();
    }

    /**
     * sleep some time without wakeups
     *
     * @param msec
     */
    public static void sleep(int msec) {
        if (msec < 1) {
            return;
        }
        notifier n = new notifier();
        n.sleep(msec);
    }

    /**
     * number to string
     *
     * @param num number to convert
     * @return converted number
     */
    public static String toUser(long num) {
        if (num < 10000) {
            return num + "";
        }
        num /= 1000;
        if (num < 10000) {
            return num + "k";
        }
        num /= 1000;
        if (num < 10000) {
            return num + "m";
        }
        num /= 1000;
        if (num < 10000) {
            return num + "g";
        }
        return num + "t";
    }

    /**
     * bandwidth to string
     *
     * @param bw bandwidth to convert
     * @return converted bandwidth
     */
    public static String bandwidth(long bw) {
        return toUser(bw) + "bps";
    }

    /**
     * convert bit to displayable format
     *
     * @param val value to test
     * @param mask value of bit to test
     * @param s name of the bit
     * @return uppercase name if set, lowercase if not
     */
    public static String bit2str(int val, int mask, String s) {
        if ((val & mask) == 0) {
            s = s.toLowerCase();
        } else {
            s = s.toUpperCase();
        }
        return s;
    }

    /**
     * pad up to ending for a minimum size
     *
     * @param str string to pad up
     * @param min minimum size to use
     * @param pad padding string
     * @return padded string
     */
    public static String padEnd(String str, int min, String pad) {
        for (; str.length() < min;) {
            str = str + pad;
        }
        return str;
    }

    /**
     * pad up to beginning for a minimum size
     *
     * @param str string to pad up
     * @param min minimum size to use
     * @param pad padding string
     * @return padded string
     */
    public static String padBeg(String str, int min, String pad) {
        for (; str.length() < min;) {
            str = pad + str;
        }
        return str;
    }

    /**
     * trim ending
     *
     * @param s string to trim
     * @return trimed string
     */
    public static String trimE(String s) {
        for (; s.endsWith(" ");) {
            s = s.substring(0, s.length() - 1);
        }
        return s;
    }

    /**
     * trim beginning
     *
     * @param s string to trim
     * @return trimed string
     */
    public static String trimB(String s) {
        for (; s.startsWith(" ");) {
            s = s.substring(1, s.length());
        }
        return s;
    }

    /**
     * convert string to number
     *
     * @param s string to convert
     * @return value of string, 0 if failed to convert
     */
    public static int str2num(String s) {
        int i = 0;
        s = s.trim();
        try {
            i = (int) Long.parseLong(s, 10);
        } catch (Exception e) {
        }
        return i;
    }

    /**
     * convert number to string
     *
     * @param i number to convert
     * @return value of number
     */
    public static String num2str(int i) {
        return "" + (((long) i) & 0xffffffffL);
    }

    /**
     * get percentage
     *
     * @param sub partial value
     * @param max total value
     * @return percentage
     */
    public static String percent(long sub, long max) {
        if (max < 1) {
            return "??%";
        }
        long res = (sub * 1000) / max;
        return (res / 10) + "." + (res % 10) + "%";
    }

    /**
     * convert string to list
     *
     * @param s string
     * @return list
     */
    public static List<String> str2lst(String s) {
        if (s == null) {
            s = "";
        }
        List<String> l = new ArrayList<String>();
        l.add(s);
        return l;
    }

    /**
     * convert list to string
     *
     * @param l list to convert
     * @param e end of line sign
     * @return string converted
     */
    public static String lst2str(List<String> l, String e) {
        String s = "";
        if (l == null) {
            return s;
        }
        for (int i = 0; i < l.size(); i++) {
            s += l.get(i) + e;
        }
        return s;
    }

    /**
     * find matching line
     *
     * @param lst list
     * @param str string
     * @return line number, less thatn zero if not found
     */
    public static int lstFnd(List<String> lst, String str) {
        for (int i = 0; i < lst.size(); i++) {
            if (lst.get(i).matches(str)) {
                return i;
            }
        }
        return -1;
    }

    /**
     * prepend lines with numbers
     *
     * @param lst list to use
     * @param update update oroginal list
     * @return list
     */
    public static List<String> lst2lin(List<String> lst, boolean update) {
        if (update) {
            return lst2lin(lst);
        }
        List<String> cpy = new ArrayList<String>();
        cpy.addAll(lst);
        lst2lin(cpy);
        return cpy;
    }

    /**
     * prepend lines with text
     *
     * @param lst list to use
     * @param pre string to prepend
     * @param update update oroginal list
     * @return list
     */
    public static List<String> lst2pre(List<String> lst, String pre,
            boolean update) {
        if (update) {
            return lst2pre(lst, pre);
        }
        List<String> cpy = new ArrayList<String>();
        cpy.addAll(lst);
        lst2pre(cpy, pre);
        return cpy;
    }

    private static List<String> lst2lin(List<String> lst) {
        if (lst == null) {
            return null;
        }
        for (int i = 0; i < lst.size(); i++) {
            lst.set(i, padBeg("" + (i + 1), 6, " ") + ": " + lst.get(i));
        }
        return lst;
    }

    private static List<String> lst2pre(List<String> lst, String pre) {
        for (int i = 0; i < lst.size(); i++) {
            lst.set(i, pre + lst.get(i));
        }
        return lst;
    }

    /**
     * save buffer to text file
     *
     * @param overwrite overwrite file
     * @param buf buffer to save
     * @param fn name of file
     * @return false if succeeded, true if error happened
     */
    public static boolean buf2txt(boolean overwrite, List<String> buf, String fn) {
        try {
            FileOutputStream ot = new FileOutputStream(fn, !overwrite);
            PrintStream pr = new PrintStream(ot);
            for (int i = 0; i < buf.size(); i++) {
                pr.println(buf.get(i));
            }
            pr.flush();
            pr.close();
        } catch (Exception e) {
            return true;
        }
        return false;
    }

    /**
     * load buffer from text file
     *
     * @param fn name of file
     * @return list of lines, null if error happened
     */
    public static List<String> txt2buf(String fn) {
        List<String> ln = new ArrayList<String>();
        try {
            FileInputStream in = new FileInputStream(fn);
            BufferedReader rd = new BufferedReader(new InputStreamReader(in));
            while (rd.ready()) {
                ln.add(rd.readLine());
            }
            rd.close();
        } catch (Exception e) {
            return null;
        }
        return ln;
    }

}
