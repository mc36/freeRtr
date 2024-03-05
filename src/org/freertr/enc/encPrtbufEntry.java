package org.freertr.enc;

import org.freertr.pack.packHolder;
import org.freertr.util.bits;

/**
 * protobuf entry
 *
 * @author matecsaba
 */
public class encPrtbufEntry {

    /**
     * create instance
     */
    public encPrtbufEntry() {
    }

    /**
     * varint
     */
    public final static int tpInt = 0;

    /**
     * fixed 64bit
     */
    public final static int tp64b = 1;

    /**
     * fixed 32bit
     */
    public final static int tp32b = 5;

    /**
     * length delimited
     */
    public final static int tpBuf = 2;

    /**
     * field number
     */
    public int num;

    /**
     * wire type
     */
    public int typ;

    /**
     * value, if dat==null
     */
    public long val;

    /**
     * data
     */
    public byte[] dat;

    public String toString() {
        String a = "num=" + num + " typ=" + typ + " val=";
        if (dat == null) {
            return a + val;
        }
        return a + bits.byteDump(dat, 0, -1);
    }

    /**
     * get data
     *
     * @param pck packet to update
     */
    public void getPacket(packHolder pck) {
        pck.putCopy(dat, 0, 0, dat.length);
        pck.putSkip(dat.length);
        pck.merge2end();
    }

    /**
     * get data
     *
     * @param pb protobuf to update
     * @return true on error, false on success
     */
    public boolean getProtobuf(encPrtbuf pb) {
        packHolder pck = new packHolder(true, true);
        getPacket(pck);
        return pb.fromPacket(pck);
    }

    /**
     * get data
     *
     * @return value
     */
    public String getString() {
        return new String(dat);
    }

    /**
     * get data
     *
     * @return value
     */
    public int getFix32() {
        return bits.lsbGetD(dat, 0);
    }

    /**
     * get data
     *
     * @return value
     */
    public long getFix64() {
        return bits.lsbGetQ(dat, 0);
    }

    /**
     * copy bytes
     *
     * @return copy
     */
    public encPrtbufEntry copyBytes() {
        encPrtbufEntry res = new encPrtbufEntry();
        res.num = num;
        res.typ = typ;
        res.val = val;
        if (dat == null) {
            return res;
        }
        res.dat = new byte[dat.length];
        bits.byteCopy(dat, 0, res.dat, 0, dat.length);
        return res;
    }

}
