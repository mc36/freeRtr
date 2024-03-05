package org.freertr.enc;

import java.util.ArrayList;
import java.util.List;
import org.freertr.pack.packHolder;

/**
 * protocol buffers handler
 *
 * @author matecsaba
 */
public class encPrtbuf {

    /**
     * original bytes
     */
    public packHolder orig;

    /**
     * elements
     */
    public List<encPrtbufEntry> data;

    /**
     * clear new instance
     */
    public encPrtbuf() {
        clear();
    }

    /**
     * clear all data
     */
    public void clear() {
        data = new ArrayList<encPrtbufEntry>();
        orig = null;
    }

    /**
     * copy from other xml
     *
     * @param src original xml to copy
     */
    public void copyBytes(encPrtbuf src) {
        clear();
        orig = src.orig;
        for (int i = 0; i < src.data.size(); i++) {
            data.add(src.data.get(i).copyBytes());
        }
    }

    /**
     * get field
     *
     * @param num number
     * @param seq sequence number
     * @return field, null if not found
     */
    public encPrtbufEntry getField(int num, int seq) {
        for (int i = 0; i < data.size(); i++) {
            encPrtbufEntry dat = data.get(i);
            if (dat.num != num) {
                continue;
            }
            seq--;
            if (seq < 0) {
                return dat;
            }
        }
        return null;
    }

    /**
     * put field
     *
     * @param num number
     * @param typ type
     * @param val value
     */
    public void putField(int num, int typ, long val) {
        encPrtbufEntry dat = new encPrtbufEntry();
        dat.num = num;
        dat.typ = typ;
        dat.val = val;
        data.add(dat);
    }

    /**
     * put field
     *
     * @param num number
     * @param typ type
     * @param val value
     */
    public void putField(int num, int typ, byte[] val) {
        encPrtbufEntry dat = new encPrtbufEntry();
        dat.num = num;
        dat.typ = typ;
        dat.dat = val;
        data.add(dat);
    }

    /**
     * encode zigzag
     *
     * @param num number
     * @return encoded
     */
    public static long toZigzag(long num) {
        return (num >> 63) ^ (num << 1);
    }

    /**
     * decode zigzag
     *
     * @param num number
     * @return decoded
     */
    public static long fromZigzag(long num) {
        return (num >>> 1) ^ (-(num & 1));
    }

    /**
     * read one varint
     *
     * @param pck packet to read
     * @return value
     */
    public static long getVarint(packHolder pck) {
        long res = 0;
        int pos = 0;
        for (;;) {
            if (pos >= pck.dataSize()) {
                break;
            }
            long val = pck.getByte(pos);
            res |= (val & 0x7f) << (pos * 7);
            pos++;
            if ((val & 0x80) == 0) {
                break;
            }
        }
        pck.getSkip(pos);
        return res;
    }

    /**
     * put one varint
     *
     * @param pck packet to update
     * @param val value
     */
    public static void putVarint(packHolder pck, long val) {
        int pos = 0;
        for (;;) {
            int cur = (int) (val & 0x7f);
            val = val >>> 7;
            if (val != 0) {
                cur |= 0x80;
            }
            pck.putByte(pos, cur);
            pos++;
            if (val == 0) {
                break;
            }
        }
        pck.putSkip(pos);
    }

    /**
     * get key value pair
     *
     * @param pck packet to read
     * @return kv pair, null if error happened
     */
    public static encPrtbufEntry getKeyValue(packHolder pck) {
        if (pck.dataSize() < 1) {
            return null;
        }
        encPrtbufEntry res = new encPrtbufEntry();
        int hdr = (int) getVarint(pck);
        res.num = hdr >>> 3;
        res.typ = hdr & 7;
        switch (res.typ) {
            case encPrtbufEntry.tpInt: // varint
                res.val = getVarint(pck);
                break;
            case encPrtbufEntry.tp64b: // 64bit
                res.val = pck.lsbGetQ(0);
                pck.getSkip(8);
                break;
            case encPrtbufEntry.tp32b: // 32bit
                res.val = pck.lsbGetD(0);
                pck.getSkip(4);
                break;
            case encPrtbufEntry.tpBuf: // length delimited
                hdr = (int) getVarint(pck);
                if (hdr > pck.dataSize()) {
                    return null;
                }
                res.dat = new byte[hdr];
                pck.getCopy(res.dat, 0, 0, res.dat.length);
                pck.getSkip(res.dat.length);
                break;
            default:
                return null;
        }
        return res;
    }

    /**
     * get key value pair
     *
     * @param pck packet to read
     * @param kv kv pair
     * @return false on success, true on error
     */
    public static boolean putKeyValue(packHolder pck, encPrtbufEntry kv) {
        putVarint(pck, (kv.num << 3) | kv.typ);
        switch (kv.typ) {
            case encPrtbufEntry.tpInt: // varint
                putVarint(pck, kv.val);
                break;
            case encPrtbufEntry.tp64b: // 64bit
                pck.lsbPutQ(0, kv.val);
                pck.putSkip(8);
                break;
            case encPrtbufEntry.tp32b: // 32bit
                pck.lsbPutD(0, (int) kv.val);
                pck.putSkip(4);
                break;
            case encPrtbufEntry.tpBuf: // length delimited
                putVarint(pck, kv.dat.length);
                pck.putCopy(kv.dat, 0, 0, kv.dat.length);
                pck.putSkip(kv.dat.length);
                break;
            default:
                return true;
        }
        return false;
    }

    /**
     * convert bytes to xml
     *
     * @param pck bytes to parse
     * @return false on success, true on error
     */
    public boolean fromPacket(packHolder pck) {
        orig = pck;
        int os = pck.dataSize();
        for (;;) {
            encPrtbufEntry res = getKeyValue(pck);
            if (res == null) {
                break;
            }
            data.add(res);
        }
        if (pck.dataSize() != 0) {
            return true;
        }
        pck.setBytesLeft(os);
        return false;
    }

    /**
     * convert bytes to protobuf
     *
     * @param pck string to convert
     * @return converted protobuf
     */
    public static encPrtbuf parseOne(packHolder pck) {
        encPrtbuf res = new encPrtbuf();
        res.fromPacket(pck);
        return res;
    }

    /**
     * convert to protobuf
     *
     * @param pck packet to append
     */
    public void toPacket(packHolder pck) {
        for (int i = 0; i < data.size(); i++) {
            putKeyValue(pck, data.get(i));
            pck.merge2end();
        }
    }

    /**
     * convert to protobuf
     *
     * @return encoded
     */
    public packHolder toPacket() {
        packHolder pck = new packHolder(true, true);
        toPacket(pck);
        return pck;
    }

    /**
     * convert to string
     *
     * @return lines of string
     */
    public List<String> show() {
        List<String> l = new ArrayList<String>();
        for (int i = 0; i < data.size(); i++) {
            l.add("" + data.get(i));
        }
        return l;
    }

}
