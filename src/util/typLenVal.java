package util;

import addr.addrType;
import pack.packHolder;

/**
 * type length value handler
 *
 * @author matecsaba
 */
public class typLenVal {

    /**
     * where to store value
     */
    public byte valDat[];

    /**
     * size of value stored
     */
    public int valSiz;

    /**
     * type of value stored
     */
    public int valTyp;

    private boolean msb;

    private int typOfs;

    private int typShr;

    private int typAnd;

    private int typClr;

    private int lenSub;

    private int lenMul;

    private int lenOfs;

    private int lenShr;

    private int lenAnd;

    private int lenClr;

    private int skipSiz;

    private int padSiz;

    private int padSub;

    /**
     * create new type length value instance
     *
     * @param old clone from
     */
    public typLenVal(typLenVal old) {
        valDat = new byte[old.valDat.length];
        msb = old.msb;
        typOfs = old.typOfs;
        typShr = old.typShr;
        typAnd = old.typAnd;
        typClr = old.typClr;
        lenSub = old.lenSub;
        lenMul = old.lenMul;
        lenOfs = old.lenOfs;
        lenShr = old.lenShr;
        lenAnd = old.lenAnd;
        lenClr = old.lenClr;
        skipSiz = old.skipSiz;
        padSiz = old.padSiz;
        padSub = old.padSub;
    }

    /**
     * create new type length value instance
     *
     * @param typeBeg bit position of type
     * @param typeSiz bit size of type
     * @param sizeBeg bit position of size
     * @param sizeSiz bit size of size
     * @param sizeMul length multiplier
     * @param sizeSub length substract
     * @param headSiz byte size of header to skip
     * @param paddingSiz byte size of padding
     * @param paddingSub byte substract from padding
     * @param bufSiz byte size of buffer to allocate
     * @param byteOrder bit order, true=msb, false=lsb
     */
    public typLenVal(int typeBeg, int typeSiz, int sizeBeg, int sizeSiz, int sizeMul, int sizeSub, int headSiz, int paddingSiz, int paddingSub, int bufSiz, boolean byteOrder) {
        valDat = new byte[bufSiz];
        typOfs = typeBeg / 8;
        lenOfs = sizeBeg / 8;
        typShr = typeBeg % 8;
        lenShr = sizeBeg % 8;
        typAnd = bits.bitVals[typeSiz] - 1;
        lenAnd = bits.bitVals[sizeSiz] - 1;
        if (byteOrder) {
            typShr = 32 - typShr - typeSiz;
            lenShr = 32 - lenShr - sizeSiz;
        }
        typClr = -1 - (typAnd << typShr);
        lenClr = -1 - (lenAnd << typShr);
        lenMul = sizeMul;
        lenSub = sizeSub;
        msb = byteOrder;
        skipSiz = headSiz;
        padSiz = paddingSiz;
        padSub = paddingSub;
    }

    /**
     * get one tlv
     *
     * @param pck packet to get from
     * @return false if successful, true if error happened
     */
    public boolean getBytes(packHolder pck) {
        int typ, len;
        valTyp = 0;
        valSiz = 0;
        if (pck.dataSize() < skipSiz) {
            return true;
        }
        if (msb) {
            typ = pck.msbGetD(typOfs);
            len = pck.msbGetD(lenOfs);
        } else {
            typ = pck.lsbGetD(typOfs);
            len = pck.lsbGetD(lenOfs);
        }
        typ = (typ >>> typShr) & typAnd;
        len = (len >>> lenShr) & lenAnd;
        len = (len * lenMul) - lenSub;
        if (len < 0) {
            return true;
        }
        if (len > valDat.length) {
            return true;
        }
        if (len > (pck.dataSize() - skipSiz)) {
            return true;
        }
        pck.getSkip(skipSiz);
        valSiz = len;
        valTyp = typ;
        for (int i = 0; i < len; i++) {
            valDat[i] = (byte) pck.getByte(i);
        }
        pck.getSkip(len);
        if (((len - padSub) % padSiz) != 0) {
            pck.getSkip(padSiz - ((len - padSub) % padSiz));
        }
        return false;
    }

    /**
     * get readed tlv as byte buffer
     *
     * @return readed bytes
     */
    public byte[] copyBytes() {
        byte buf[] = new byte[valSiz];
        bits.byteCopy(valDat, 0, buf, 0, buf.length);
        return buf;
    }

    /**
     * put one tlv
     *
     * @param pck packet to put to
     * @param typ type of value
     * @param bits bits to set
     * @param len length of value
     * @param val value
     */
    public void putBytes(packHolder pck, int bits[], int typ, int len, byte val[]) {
        pck.putFill(0, skipSiz, 0);
        if (typOfs <= lenOfs) {
            putType(pck, typ);
            putSize(pck, len);
        } else {
            putSize(pck, len);
            putType(pck, typ);
        }
        if (bits != null) {
            for (int i = 0; i < bits.length; i++) {
                pck.putBit(0, bits[i], true);
            }
        }
        pck.putSkip(skipSiz);
        for (int i = 0; i < len; i++) {
            pck.putByte(i, val[i]);
        }
        pck.putSkip(len);
        if (((len + padSub) % padSiz) != 0) {
            int p = padSiz - ((len + padSub) % padSiz);
            pck.putFill(0, p, 0);
            pck.putSkip(p);
        }
    }

    private void putSize(packHolder pck, int len) {
        int o = (len + lenSub) / lenMul;
        o = (0 & lenClr) | (o << lenShr);
        if (msb) {
            pck.msbPutD(lenOfs, o);
        } else {
            pck.lsbPutD(lenOfs, o);
        }
    }

    private void putType(packHolder pck, int typ) {
        int o = (0 & typClr) | (typ << typShr);
        if (msb) {
            pck.msbPutD(typOfs, o);
        } else {
            pck.lsbPutD(typOfs, o);
        }
    }

    /**
     * put one tlv
     *
     * @param pck packet to put to
     * @param typ type of value
     * @param len length of value
     * @param val value
     */
    public void putBytes(packHolder pck, int typ, int len, byte val[]) {
        putBytes(pck, null, typ, len, val);
    }

    /**
     * put one tlv
     *
     * @param pck packet to put to
     * @param typ type of value
     * @param val value
     */
    public void putBytes(packHolder pck, int typ, byte val[]) {
        putBytes(pck, null, typ, val.length, val);
    }

    /**
     * put one tlv
     *
     * @param pck packet to put to
     * @param bits bits to set
     * @param typ type of value
     * @param val value
     */
    public void putBytes(packHolder pck, int bits[], int typ, byte val[]) {
        putBytes(pck, bits, typ, val.length, val);
    }

    /**
     * put one tlv
     *
     * @param pck packet to put to
     * @param typ type of value
     */
    public void putBytes(packHolder pck, int typ) {
        putBytes(pck, null, typ, valSiz, valDat);
    }

    /**
     * put one tlv
     *
     * @param pck packet to put to
     * @param bits bits to set
     * @param typ type of value
     * @param str string to put
     */
    public void putStr(packHolder pck, int[] bits, int typ, String str) {
        if (str == null) {
            putBytes(pck, bits, typ, 0, null);
            return;
        }
        putBytes(pck, bits, typ, str.length(), str.getBytes());
    }

    /**
     * put one tlv
     *
     * @param pck packet to put to
     * @param typ type of value
     * @param str string to put
     */
    public void putStr(packHolder pck, int typ, String str) {
        putStr(pck, null, typ, str);
    }

    /**
     * put one address
     *
     * @param pck packet to put to
     * @param typ type of value
     * @param adr address to put
     */
    public void putAddr(packHolder pck, int typ, addrType adr) {
        putBytes(pck, null, typ, adr.getSize(), adr.getBytes());
    }

    /**
     * put readed tlv back
     *
     * @param pck packet to write to
     */
    public void putThis(packHolder pck) {
        putBytes(pck, null, valTyp, valSiz, valDat);
    }

    /**
     * get buffer as string
     *
     * @return string
     */
    public String getStr() {
        return new String(valDat, 0, valSiz);
    }

    /**
     * return header size
     *
     * @return header size
     */
    public int getHeadSize() {
        return skipSiz;
    }

    /**
     * dump this type length value
     *
     * @return dump of tlv
     */
    public String dump() {
        return "type=" + valTyp + " value=" + bits.byteDump(valDat, 0, valSiz);
    }

}
