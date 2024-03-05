package org.freertr.enc;

import java.util.ArrayList;
import java.util.List;
import org.freertr.util.bits;

/**
 * thrift entry
 *
 * @author matecsaba
 */
public class encThriftEntry {

    /**
     * create instance
     */
    public encThriftEntry() {
    }

    /**
     * end of struct
     */
    public final static int tpEnd = 0;

    /**
     * boolean
     */
    public final static int tpBool = 2;

    /**
     * i8
     */
    public final static int tpI8 = 3;

    /**
     * i16
     */
    public final static int tpI16 = 6;

    /**
     * i32
     */
    public final static int tpI32 = 8;

    /**
     * i64
     */
    public final static int tpI64 = 10;

    /**
     * double
     */
    public final static int tpDbl = 4;

    /**
     * binary
     */
    public final static int tpBin = 11;

    /**
     * struct
     */
    public final static int tpStr = 12;

    /**
     * map
     */
    public final static int tpMap = 13;

    /**
     * set
     */
    public final static int tpSet = 14;

    /**
     * list
     */
    public final static int tpLst = 15;

    /**
     * uuid
     */
    public final static int tpUid = 16;

    /**
     * wire type
     */
    public int typ;

    /**
     * key type
     */
    public int typK;

    /**
     * value type
     */
    public int typV;

    /**
     * field id
     */
    public int num;

    /**
     * value, if dat==null
     */
    public long val;

    /**
     * data
     */
    public byte[] dat;

    /**
     * elements
     */
    public List<encThriftEntry> elm;

    public String toString() {
        String a = "num=" + num + " typ=" + typ + " val=";
        if (dat != null) {
            return a + bits.byteDump(dat, 0, -1);
        }
        if (elm == null) {
            return a + val;
        }
        a += "(";
        for (int i = 0; i < elm.size(); i++) {
            a += " " + elm.get(i);
        }
        return a + ")";
    }

    /**
     * convert to string
     *
     * @param beg beginning
     * @return lines of string
     */
    public List<String> show(String beg) {
        List<String> l = new ArrayList<String>();
        String a = beg + "num=" + num + " typ=" + typ + " val=";
        if (dat != null) {
            l.add(a + bits.byteDump(dat, 0, -1));
            return l;
        }
        if (elm == null) {
            l.add(a + val);
            return l;
        }
        l.add(a + "(");
        for (int i = 0; i < elm.size(); i++) {
            l.addAll(elm.get(i).show(beg + "  "));
        }
        l.add(beg + ")");
        return l;
    }

    /**
     * copy bytes
     *
     * @return copy
     */
    public encThriftEntry copyBytes() {
        encThriftEntry res = new encThriftEntry();
        res.num = num;
        res.typ = typ;
        res.val = val;
        if (dat != null) {
            res.dat = new byte[dat.length];
            bits.byteCopy(dat, 0, res.dat, 0, dat.length);
        }
        if (elm == null) {
            return res;
        }
        res.elm = new ArrayList<encThriftEntry>();
        for (int i = 0; i < elm.size(); i++) {
            res.elm.add(elm.get(i).copyBytes());
        }
        return res;
    }

    /**
     * get field
     *
     * @param data where from get
     * @param num number
     * @param seq sequence number
     * @return field, null if not found
     */
    protected static encThriftEntry getField(List<encThriftEntry> data, int num, int seq) {
        for (int i = 0; i < data.size(); i++) {
            encThriftEntry dat = data.get(i);
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
     * @return field, null if not found
     */
    protected static encThriftEntry genField(int num, int typ, long val) {
        encThriftEntry dat = new encThriftEntry();
        dat.num = num;
        dat.typ = typ;
        dat.val = val;
        return dat;
    }

    /**
     * put field
     *
     * @param num number
     * @param typ type
     * @param val value
     * @return field, null if not found
     */
    protected static encThriftEntry genField(int num, int typ, byte[] val) {
        encThriftEntry dat = new encThriftEntry();
        dat.num = num;
        dat.typ = typ;
        dat.dat = val;
        return dat;
    }

    /**
     * put field
     *
     * @param num number
     * @param typ type
     * @param val value
     * @return field, null if not found
     */
    protected static encThriftEntry genField(int num, int typ, List<encThriftEntry> val) {
        encThriftEntry dat = new encThriftEntry();
        dat.num = num;
        dat.typ = typ;
        dat.elm = val;
        return dat;
    }

    /**
     * get field
     *
     * @param num number
     * @param seq sequence number
     * @return field, null if not found
     */
    public encThriftEntry getField(int num, int seq) {
        if (elm == null) {
            return null;
        }
        return getField(elm, num, seq);
    }

    private void initElements() {
        if (elm != null) {
            return;
        }
        elm = new ArrayList<encThriftEntry>();
    }

    /**
     * put field
     *
     * @param num number
     * @param typ type
     * @param val value
     */
    public void putField(int num, int typ, long val) {
        initElements();
        elm.add(genField(num, typ, val));
    }

    /**
     * put field
     *
     * @param num number
     * @param typ type
     * @param val value
     */
    public void putField(int num, int typ, byte[] val) {
        initElements();
        elm.add(genField(num, typ, val));
    }

    /**
     * put field
     *
     * @param num number
     * @param typ type
     * @param val value
     */
    public void putField(int num, int typ, List<encThriftEntry> val) {
        initElements();
        elm.add(genField(num, typ, val));
    }

    /**
     * set field
     *
     * @param k type
     * @param v type
     */
    public void putTypKV(int k, int v) {
        encThriftEntry c = elm.get(elm.size() - 1);
        c.typK = k;
        c.typV = v;
    }

}
