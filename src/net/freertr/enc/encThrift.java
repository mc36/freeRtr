package net.freertr.enc;

import java.util.ArrayList;
import java.util.List;
import net.freertr.pack.packHolder;

/**
 * thrift buffers handler
 *
 * @author matecsaba
 */
public class encThrift {

    /**
     * original bytes
     */
    public packHolder orig;

    /**
     * elements
     */
    public List<encThriftEntry> data;

    /**
     * clear new instance
     */
    public encThrift() {
        clear();
    }

    /**
     * clear all data
     */
    public void clear() {
        data = new ArrayList<encThriftEntry>();
        orig = null;
    }

    /**
     * copy from other xml
     *
     * @param src original xml to copy
     */
    public void copyBytes(encThrift src) {
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
    public encThriftEntry getField(int num, int seq) {
        return encThriftEntry.getField(data, num, seq);
    }

    /**
     * put field
     *
     * @param num number
     * @param typ type
     * @param val value
     */
    public void putField(int num, int typ, long val) {
        data.add(encThriftEntry.genField(num, typ, val));
    }

    /**
     * put field
     *
     * @param num number
     * @param typ type
     * @param val value
     */
    public void putField(int num, int typ, byte[] val) {
        data.add(encThriftEntry.genField(num, typ, val));
    }

    /**
     * put field
     *
     * @param num number
     * @param typ type
     * @param val value
     */
    public void putField(int num, int typ, List<encThriftEntry> val) {
        data.add(encThriftEntry.genField(num, typ, val));
    }

    /**
     * get key value pair
     *
     * @param pck packet to read
     * @return kv pair, null if error happened
     */
    public static encThriftEntry getStruct(packHolder pck) {
        if (pck.dataSize() < 1) {
            return null;
        }
        encThriftEntry res = new encThriftEntry();
        res.typ = pck.getByte(0);
        res.num = pck.msbGetW(1);
        pck.getSkip(3);
        switch (res.typ) {
            case encThriftEntry.tpI8:
                res.val = pck.getByte(0);
                pck.getSkip(1);
                break;
            case encThriftEntry.tpI16:
                res.val = pck.msbGetW(0);
                pck.getSkip(2);
                break;
            case encThriftEntry.tpI32:
                res.val = pck.msbGetD(0);
                pck.getSkip(4);
                break;
            case encThriftEntry.tpI64:
                res.val = pck.msbGetQ(0);
                pck.getSkip(8);
                break;
            case encThriftEntry.tpBool:
                res.val = pck.getByte(0);
                pck.getSkip(1);
                break;
            case encThriftEntry.tpDbl:
                res.val = pck.msbGetQ(0);
                pck.getSkip(8);
                break;
            case encThriftEntry.tpBin:
                res.dat = new byte[pck.msbGetD(0)];
                pck.getSkip(4);
                pck.getCopy(res.dat, 0, 0, res.dat.length);
                pck.getSkip(res.dat.length);
                break;
            case encThriftEntry.tpEnd:
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
    public static boolean putStruct(packHolder pck, encThriftEntry kv) {
        pck.putByte(0, kv.typ);
        pck.msbPutW(1, kv.num);
        switch (kv.typ) {
            case encThriftEntry.tpI8:
                pck.putByte(0, (int) kv.val);
                pck.putSkip(1);
                break;
            case encThriftEntry.tpI16:
                pck.msbPutW(0, (int) kv.val);
                pck.putSkip(2);
                break;
            case encThriftEntry.tpI32:
                pck.msbPutD(0, (int) kv.val);
                pck.putSkip(4);
                break;
            case encThriftEntry.tpI64:
                pck.msbPutQ(0, kv.val);
                pck.putSkip(8);
                break;
            case encThriftEntry.tpBool:
                pck.putByte(0, (int) kv.val);
                pck.putSkip(1);
                break;
            case encThriftEntry.tpDbl:
                pck.msbPutQ(0, kv.val);
                pck.putSkip(8);
                break;
            case encThriftEntry.tpBin:
                pck.msbPutD(0, kv.dat.length);
                pck.putSkip(4);
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
            encThriftEntry res = getStruct(pck);
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
    public static encThrift parseOne(packHolder pck) {
        encThrift res = new encThrift();
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
            putStruct(pck, data.get(i));
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
