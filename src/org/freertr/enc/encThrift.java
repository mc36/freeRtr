package org.freertr.enc;

import java.util.ArrayList;
import java.util.List;
import org.freertr.pack.packHolder;

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

    private static boolean getHead(encThriftEntry elm, packHolder pck) {
        if (pck.dataSize() < 1) {
            return true;
        }
        elm.typ = pck.getByte(0);
        elm.num = pck.msbGetW(1);
        pck.getSkip(3);
        return false;
    }

    private static boolean putHead(encThriftEntry elm, packHolder pck) {
        pck.putByte(0, elm.typ);
        pck.msbPutW(1, elm.num);
        pck.putSkip(3);
        return false;
    }

    private static boolean getData(encThriftEntry elm, packHolder pck) {
        switch (elm.typ) {
            case encThriftEntry.tpI8:
            case encThriftEntry.tpBool:
                elm.val = pck.getByte(0);
                pck.getSkip(1);
                return false;
            case encThriftEntry.tpI16:
                elm.val = pck.msbGetW(0);
                pck.getSkip(2);
                return false;
            case encThriftEntry.tpI32:
                elm.val = pck.msbGetD(0);
                pck.getSkip(4);
                return false;
            case encThriftEntry.tpI64:
            case encThriftEntry.tpDbl:
                elm.val = pck.msbGetQ(0);
                pck.getSkip(8);
                return false;
            case encThriftEntry.tpBin:
                int siz = pck.msbGetD(0);
                pck.getSkip(4);
                if (siz > pck.dataSize()) {
                    return true;
                }
                elm.dat = new byte[siz];
                pck.getCopy(elm.dat, 0, 0, elm.dat.length);
                pck.getSkip(elm.dat.length);
                return false;
            case encThriftEntry.tpEnd:
                pck.getSkip(-2);
                return false;
            case encThriftEntry.tpStr:
                elm.elm = new ArrayList<encThriftEntry>();
                for (;;) {
                    encThriftEntry cur = new encThriftEntry();
                    if (getHead(cur, pck)) {
                        return true;
                    }
                    if (getData(cur, pck)) {
                        return true;
                    }
                    if (cur.typ == encThriftEntry.tpEnd) {
                        break;
                    }
                    elm.elm.add(cur);
                }
                return false;
            case encThriftEntry.tpLst:
            case encThriftEntry.tpSet:
                elm.elm = new ArrayList<encThriftEntry>();
                elm.typV = pck.getByte(0);
                int len = pck.msbGetD(1);
                pck.getSkip(5);
                for (int i = 0; i < len; i++) {
                    encThriftEntry cur = new encThriftEntry();
                    cur.typ = elm.typV;
                    cur.num = i;
                    if (getData(cur, pck)) {
                        return true;
                    }
                    elm.elm.add(cur);
                }
                return false;
            case encThriftEntry.tpMap:
                elm.elm = new ArrayList<encThriftEntry>();
                elm.typK = pck.getByte(0);
                elm.typV = pck.getByte(1);
                len = pck.msbGetD(2);
                pck.getSkip(6);
                for (int i = 0; i < len; i++) {
                    encThriftEntry cur = new encThriftEntry();
                    cur.typ = elm.typK;
                    cur.num = i;
                    if (getData(cur, pck)) {
                        return true;
                    }
                    elm.elm.add(cur);
                    cur = new encThriftEntry();
                    cur.typ = elm.typV;
                    cur.num = i;
                    if (getData(cur, pck)) {
                        return true;
                    }
                    elm.elm.add(cur);
                }
                return false;
            default:
                return true;
        }
    }

    private static boolean putData(encThriftEntry elm, packHolder pck) {
        switch (elm.typ) {
            case encThriftEntry.tpI8:
            case encThriftEntry.tpBool:
                pck.putByte(0, (int) elm.val);
                pck.putSkip(1);
                return false;
            case encThriftEntry.tpI16:
                pck.msbPutW(0, (int) elm.val);
                pck.putSkip(2);
                return false;
            case encThriftEntry.tpI32:
                pck.msbPutD(0, (int) elm.val);
                pck.putSkip(4);
                return false;
            case encThriftEntry.tpI64:
            case encThriftEntry.tpDbl:
                pck.msbPutQ(0, elm.val);
                pck.putSkip(8);
                return false;
            case encThriftEntry.tpBin:
                pck.msbPutD(0, elm.dat.length);
                pck.putSkip(4);
                pck.putCopy(elm.dat, 0, 0, elm.dat.length);
                pck.putSkip(elm.dat.length);
                return false;
            case encThriftEntry.tpEnd:
                pck.putSkip(-2);
                return false;
            case encThriftEntry.tpStr:
                for (int i = 0; i < elm.elm.size(); i++) {
                    encThriftEntry cur = elm.elm.get(i);
                    if (putHead(cur, pck)) {
                        return true;
                    }
                    if (putData(cur, pck)) {
                        return true;
                    }
                    pck.merge2end();
                }
                pck.putByte(0, encThriftEntry.tpEnd);
                pck.putSkip(1);
                return false;
            case encThriftEntry.tpLst:
            case encThriftEntry.tpSet:
                int len = elm.elm.size();
                pck.putByte(0, elm.typV);
                pck.msbPutD(1, len);
                pck.putSkip(5);
                for (int i = 0; i < len; i++) {
                    encThriftEntry cur = elm.elm.get(i);
                    if (cur.typ != elm.typV) {
                        return true;
                    }
                    if (putData(cur, pck)) {
                        return true;
                    }
                    pck.merge2end();
                }
                return false;
            case encThriftEntry.tpMap:
                len = elm.elm.size() / 2;
                pck.putByte(0, elm.typK);
                pck.putByte(1, elm.typV);
                pck.msbPutD(2, len);
                pck.putSkip(6);
                for (int i = 0; i < len; i++) {
                    encThriftEntry cur = elm.elm.get(i * 2);
                    if (cur.typ != elm.typK) {
                        return true;
                    }
                    if (putData(cur, pck)) {
                        return true;
                    }
                    cur = elm.elm.get((i * 2) + 1);
                    if (cur.typ != elm.typV) {
                        return true;
                    }
                    if (putData(cur, pck)) {
                        return true;
                    }
                    pck.merge2end();
                }
                return false;
            default:
                return true;
        }
    }

    /**
     * get key value pair
     *
     * @param pck packet to read
     * @return element, null if error happened
     */
    public static encThriftEntry getEntry(packHolder pck) {
        encThriftEntry res = new encThriftEntry();
        if (getHead(res, pck)) {
            return null;
        }
        if (getData(res, pck)) {
            return null;
        }
        return res;
    }

    /**
     * get key value pair
     *
     * @param pck packet to read
     * @param elm element
     * @return false on success, true on error
     */
    public static boolean putEntry(packHolder pck, encThriftEntry elm) {
        if (putHead(elm, pck)) {
            return true;
        }
        return putData(elm, pck);
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
            encThriftEntry res = getEntry(pck);
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
            putEntry(pck, data.get(i));
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
            l.addAll(data.get(i).show(""));
        }
        return l;
    }

}
