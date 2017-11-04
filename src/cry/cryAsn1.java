package cry;

import addr.addrIPv4;
import addr.addrIPv6;
import addr.addrMac;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import pack.packHolder;
import util.bits;
import util.cmds;

/**
 * abstract syntax notation 1
 *
 * @author matecsaba
 */
public class cryAsn1 {

    /**
     * class of tag
     */
    public int cls;

    /**
     * true=constructed, false=primitive
     */
    public boolean cnst;

    /**
     * number of tag
     */
    public int tag;

    /**
     * oid of tag
     */
    public int[] oid;

    /**
     * bytes in tag
     */
    public byte[] buf;

    /**
     * end of construction
     */
    public static final int tagEoc = 0x00;

    /**
     * boolean
     */
    public static final int tagBoolean = 0x01;

    /**
     * integer
     */
    public static final int tagInteger = 0x02;

    /**
     * bit string
     */
    public static final int tagBitString = 0x03;

    /**
     * octet string
     */
    public static final int tagOctetString = 0x04;

    /**
     * null
     */
    public static final int tagNull = 0x05;

    /**
     * object identifier
     */
    public static final int tagObjectID = 0x06;

    /**
     * object descriptor
     */
    public static final int tagObjectDesc = 0x07;

    /**
     * external
     */
    public static final int tagExternal = 0x08;

    /**
     * real
     */
    public static final int tagReal = 0x09;

    /**
     * enumerated
     */
    public static final int tagEnumerated = 0x0a;

    /**
     * utf8 string
     */
    public static final int tagUTF8string = 0x0c;

    /**
     * sequence
     */
    public static final int tagSequence = 0x10;

    /**
     * set
     */
    public static final int tagSet = 0x11;

    /**
     * numeric string
     */
    public static final int tagNumericString = 0x12;

    /**
     * printable string
     */
    public static final int tagPrintableString = 0x13;

    /**
     * teletex string
     */
    public static final int tagTeletexString = 0x14;

    /**
     * videotex string
     */
    public static final int tagVideotexString = 0x15;

    /**
     * ia5 string
     */
    public static final int tagIA5string = 0x16;

    /**
     * utc time
     */
    public static final int tagUTCtime = 0x17;

    /**
     * general time
     */
    public static final int tagGeneralTime = 0x18;

    /**
     * graphic string
     */
    public static final int tagGraphicString = 0x19;

    /**
     * visible string
     */
    public static final int tagVisibleString = 0x1A;

    /**
     * general string
     */
    public static final int tagGeneralString = 0x1B;

    /**
     * convert tag to value
     *
     * @param i tag to convert
     * @return value
     */
    public String tag2string(int i) {
        switch (i) {
            case tagEoc:
                return "eoc";
            case tagBoolean:
                return "boolean";
            case tagInteger:
                return "integer";
            case tagBitString:
                return "bitstring";
            case tagOctetString:
                return "octetstring";
            case tagNull:
                return "null";
            case tagObjectID:
                return "objectid";
            case tagObjectDesc:
                return "objectdesc";
            case tagExternal:
                return "external";
            case tagReal:
                return "real";
            case tagEnumerated:
                return "enumerated";
            case tagUTF8string:
                return "utf8string";
            case tagSequence:
                return "sequence";
            case tagSet:
                return "set";
            case tagNumericString:
                return "numeric";
            case tagPrintableString:
                return "printable";
            case tagTeletexString:
                return "teletex";
            case tagVideotexString:
                return "videotex";
            case tagIA5string:
                return "ia5string";
            case tagUTCtime:
                return "utcTime";
            case tagGeneralTime:
                return "genTime";
            case tagGraphicString:
                return "graphic";
            case tagVisibleString:
                return "visible";
            case tagGeneralString:
                return "general";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * string to tag
     *
     * @param s string
     * @return tag
     */
    public static int string2tag(String s) {
        if (s.equals("general")) {
            return tagGeneralString;
        }
        if (s.equals("visible")) {
            return tagVisibleString;
        }
        if (s.equals("graphic")) {
            return tagGraphicString;
        }
        if (s.equals("genTime")) {
            return tagGeneralTime;
        }
        if (s.equals("utcTime")) {
            return tagUTCtime;
        }
        if (s.equals("ia5string")) {
            return tagIA5string;
        }
        if (s.equals("videotex")) {
            return tagVideotexString;
        }
        if (s.equals("teletex")) {
            return tagTeletexString;
        }
        if (s.equals("printable")) {
            return tagPrintableString;
        }
        if (s.equals("numeric")) {
            return tagNumericString;
        }
        if (s.equals("set")) {
            return tagSet;
        }
        if (s.equals("sequence")) {
            return tagSequence;
        }
        if (s.equals("utf8string")) {
            return tagUTF8string;
        }
        if (s.equals("enumerated")) {
            return tagEnumerated;
        }
        if (s.equals("real")) {
            return tagReal;
        }
        if (s.equals("external")) {
            return tagExternal;
        }
        if (s.equals("objectdesc")) {
            return tagObjectDesc;
        }
        if (s.equals("objectid")) {
            return tagObjectID;
        }
        if (s.equals("null")) {
            return tagNull;
        }
        if (s.equals("octetstring")) {
            return tagOctetString;
        }
        if (s.equals("bitstring")) {
            return tagBitString;
        }
        if (s.equals("integer")) {
            return tagInteger;
        }
        if (s.equals("boolean")) {
            return tagBoolean;
        }
        if (s.equals("eoc")) {
            return tagEoc;
        }
        return bits.str2num(s);
    }

    /**
     * convert bytes to oid
     *
     * @param src bytes
     * @return converted oid
     */
    public static int[] buf2oid(byte[] src) {
        int[] trg = new int[src.length + 1];
        trg[0] = (src[0] & 0xff) / 40;
        trg[1] = (src[0] & 0xff) % 40;
        for (int i = 1; i < src.length; i++) {
            trg[i + 1] = src[i] & 0xff;
        }
        return trg;
    }

    /**
     * convert oid to bytes
     *
     * @param src oid
     * @return bytes
     */
    public static byte[] oid2buf(int[] src) {
        byte[] trg = new byte[src.length - 1];
        trg[0] = (byte) ((src[0] * 40) + (src[1] % 40));
        for (int i = 1; i < trg.length; i++) {
            trg[i] = (byte) src[i + 1];
        }
        return trg;
    }

    /**
     * convert oid to string
     *
     * @param oid oid
     * @return string
     */
    public static String oid2str(int[] oid) {
        if (oid == null) {
            return "";
        }
        if (oid.length < 1) {
            return "";
        }
        String s = "";
        for (int i = 0; i < oid.length;) {
            int o = oid[i] & 0xff;
            if ((o & 0x80) == 0) {
                s += "." + o;
                i++;
                continue;
            }
            int p = 0;
            for (; i < oid.length;) {
                o = oid[i] & 0xff;
                p = (p << 7) | (o & 0x7f);
                i++;
                if ((o & 0x80) == 0) {
                    break;
                }
            }
            s += "." + p;
        }
        s = s.substring(1, s.length());
        return s;
    }

    /**
     * convert string to oid
     *
     * @param s string
     * @return oid
     */
    public static int[] str2oid(String s) {
        s += ".";
        List<Integer> b1 = new ArrayList<Integer>();
        for (;;) {
            if (s.length() < 1) {
                break;
            }
            int i = s.indexOf(".");
            if (i < 1) {
                break;
            }
            int o = bits.str2num(s.substring(0, i));
            s = s.substring(i + 1, s.length());
            if ((o & 0x7f) == o) {
                b1.add(o);
                continue;
            }
            List<Integer> b2 = new ArrayList<Integer>();
            for (; o != 0;) {
                b2.add(o & 0x7f);
                o >>>= 7;
            }
            for (o = b2.size() - 1; o >= 0; o--) {
                int p = b2.get(o);
                if (o > 0) {
                    p |= 0x80;
                }
                b1.add(p);
            }
        }
        int[] b2 = new int[b1.size()];
        for (int i = 0; i < b2.length; i++) {
            b2[i] = b1.get(i);
        }
        return b2;
    }

    /**
     * read one tag from stream
     *
     * @param pck packet to read from
     * @return false on success, true on error
     */
    public boolean tagRead(packHolder pck) {
        if (pck.dataSize() < 2) {
            return true;
        }
        int i = pck.getByte(0);
        pck.getSkip(1);
        cls = (i >>> 6) & 3;
        cnst = (i & 0x20) != 0;
        tag = i & 0x1f;
        if (tag == 0x1f) {
            tag = 0;
            for (;;) {
                i = pck.getByte(0);
                pck.getSkip(1);
                tag = (tag << 7) | (i & 0x7f);
                if ((tag & 0x80) == 0) {
                    break;
                }
            }
        }
        i = pck.getByte(0);
        pck.getSkip(1);
        if ((i & 0x80) == 0) {
            buf = new byte[i];
        } else {
            int o = 0;
            int p = i & 0x7f;
            for (i = 0; i < p; i++) {
                o = (o << 8) | pck.getByte(0);
                pck.getSkip(1);
            }
            buf = new byte[o & 0xffff];
        }
        if (pck.dataSize() < buf.length) {
            return true;
        }
        for (i = 0; i < buf.length; i++) {
            buf[i] = (byte) pck.getByte(i);
        }
        pck.getSkip(buf.length);
        return false;
    }

    /**
     * write one tag to stream
     *
     * @param pck packet to read from
     */
    public void tagWrite(packHolder pck) {
        int i = 0;
        if (cnst) {
            i = 0x20;
        }
        if ((tag & 0x1f) == tag) {
            pck.putByte(0, (cls << 6) | i | tag);
            pck.putSkip(1);
        } else {
            pck.putByte(0, (cls << 6) | i | 0x1f);
            pck.putSkip(1);
            byte[] b = new byte[16];
            int p = tag;
            int o = 0;
            for (;;) {
                if (p == 0) {
                    break;
                }
                b[o] = (byte) (p & 0x7f);
                o++;
                p = p >>> 7;
            }
            for (i = 0; i < o; i++) {
                p = b[o - i - 1];
                if (i < o - 1) {
                    p = p | 0x80;
                }
                pck.putByte(i, p);
            }
            pck.putSkip(o);
        }
        if ((buf.length & 0x7f) == buf.length) {
            pck.putByte(0, buf.length);
            pck.putSkip(1);
        } else {
            byte[] b = new byte[16];
            int p = buf.length;
            int o = 0;
            for (;;) {
                if (p == 0) {
                    break;
                }
                b[o] = (byte) (p & 0xff);
                o++;
                p = p >>> 8;
            }
            pck.putByte(0, o | 0x80);
            pck.putSkip(1);
            for (i = 0; i < o; i++) {
                pck.putByte(i, b[o - i - 1]);
            }
            pck.putSkip(o);
        }
        for (i = 0; i < buf.length; i++) {
            pck.putByte(i, buf[i]);
        }
        pck.putSkip(buf.length);
    }

    /**
     * return data as a packet
     *
     * @return data as packet
     */
    public packHolder getPack() {
        packHolder pck = new packHolder(true, true);
        pck.clear();
        pck.putCopy(buf, 0, 0, buf.length);
        pck.putSkip(buf.length);
        pck.merge2beg();
        return pck;
    }

    /**
     * return data as big integer
     *
     * @return integer value
     */
    public BigInteger getBigInt() {
        BigInteger i = new BigInteger(buf);
        return i;
    }

    /**
     * save packet data as sequence
     *
     * @param pck packet data to save
     */
    public void putSequence(packHolder pck) {
        pck.mergeHeader(0, 0);
        buf = pck.getCopy();
        cnst = true;
        tag = tagSequence;
        cls = 0;
    }

    /**
     * save packet data as sequence
     *
     * @param pck packet data to save
     */
    public void putSet(packHolder pck) {
        pck.mergeHeader(0, 0);
        buf = pck.getCopy();
        cnst = true;
        tag = tagSet;
        cls = 0;
    }

    /**
     * save packet data as bitString
     *
     * @param pck packet data to save
     */
    public void putBitString(packHolder pck) {
        pck.mergeHeader(0, 0);
        buf = pck.getCopy();
        cnst = false;
        tag = tagBitString;
        cls = 0;
    }

    /**
     * save packet data as bitString
     *
     * @param pck packet data to save
     */
    public void putOctString(packHolder pck) {
        pck.mergeHeader(0, 0);
        buf = pck.getCopy();
        cnst = false;
        tag = tagOctetString;
        cls = 0;
    }

    /**
     * save packet data as bitString
     *
     * @param pck packet data to save
     */
    public void putEoc(packHolder pck) {
        pck.mergeHeader(0, 0);
        buf = pck.getCopy();
        cnst = true;
        tag = tagEoc;
        cls = 2;
    }

    /**
     * save packet data as bitString
     *
     * @param pck packet data to save
     */
    public void putEoc2(packHolder pck) {
        pck.mergeHeader(0, 0);
        buf = pck.getCopy();
        cnst = true;
        tag = tagBoolean;
        cls = 2;
    }

    /**
     * save big integer as integer
     *
     * @param b integer
     */
    public void putBigInt(BigInteger b) {
        buf = b.toByteArray();
        cnst = false;
        tag = tagInteger;
        cls = 0;
    }

    /**
     * save integer array as object id
     *
     * @param oid object id
     */
    public void putObjectId(int[] oid) {
        buf = new byte[oid.length];
        for (int i = 0; i < buf.length; i++) {
            buf[i] = (byte) (oid[i] & 0xff);
        }
        cnst = false;
        tag = tagObjectID;
        cls = 0;
    }

    /**
     * save null
     */
    public void putNull() {
        buf = new byte[0];
        cnst = false;
        tag = tagNull;
        cls = 0;
    }

    /**
     * save utc time
     *
     * @param tim time to save
     */
    public void putUTCtime(long tim) {
        String s = bits.time2str(null, tim, 3) + "Z";
        s = s.substring(2, s.length());
        s = s.replace(" ", "");
        s = s.replace(":", "");
        s = s.replace("-", "");
        buf = s.getBytes();
        cnst = false;
        tag = tagUTCtime;
        cls = 0;
    }

    public String toString() {
        String a = "";
        if (!cnst) {
            a = " data=" + bits.byteDump(buf, 0, -1);
            if (tag == tagObjectID) {
                a += " (" + oid2str(buf2oid(buf)) + ")";
            }
        } else {
            a = " size=" + buf.length;
        }
        return "class=" + cls + " construct=" + cnst + " tag=" + tag2string(tag) + a;
    }

    /**
     * convert from string
     *
     * @param cmd string to convert
     * @return false on success, true on error
     */
    public boolean fromString(cmds cmd) {
        for (;;) {
            String s = cmd.word();
            if (s.length() < 1) {
                break;
            }
            String a = "";
            int i = s.indexOf("=");
            if (i >= 0) {
                a = s.substring(i + 1, s.length());
                s = s.substring(0, i);
            }
            if (s.equals("tag")) {
                tag = string2tag(a);
                continue;
            }
            if (s.equals("class")) {
                cls = bits.str2num(a);
                continue;
            }
            if (s.equals("construct")) {
                cnst = a.equals("true");
                continue;
            }
            if (s.equals("string")) {
                a += cmd.getRemaining();
                buf = a.getBytes();
                return false;
            }
            if (s.equals("int32")) {
                buf = new byte[4];
                bits.msbPutD(buf, 0, bits.str2num(a));
                return false;
            }
            if (s.equals("int64")) {
                buf = new byte[8];
                bits.msbPutQ(buf, 0, bits.str2num(a));
                return false;
            }
            if (s.equals("adrip4")) {
                addrIPv4 ad = new addrIPv4();
                ad.fromString(a);
                buf = new byte[addrIPv4.size];
                ad.toBuffer(buf, 0);
                return false;
            }
            if (s.equals("adrip6")) {
                addrIPv6 ad = new addrIPv6();
                ad.fromString(a);
                buf = new byte[addrIPv6.size];
                ad.toBuffer(buf, 0);
                return false;
            }
            if (s.equals("adrmac")) {
                addrMac ad = new addrMac();
                ad.fromString(a);
                buf = new byte[addrMac.size];
                ad.toBuffer(buf, 0);
                return false;
            }
            if (s.equals("oid")) {
                buf = oid2buf(str2oid(a));
                return false;
            }
            if (s.equals("data")) {
                break;
            }
            return true;
        }
        if (cmd.size() < 1) {
            return false;
        }
        List<Integer> lst = new ArrayList<Integer>();
        for (;;) {
            String s = cmd.word();
            if (s.length() < 1) {
                break;
            }
            lst.add(bits.fromHex(s));
        }
        buf = new byte[lst.size()];
        for (int i = 0; i < buf.length; i++) {
            buf[i] = (byte) ((int) lst.get(i));
        }
        return false;
    }

    /**
     * read big integer
     *
     * @param pck packet to use
     * @return integer readed
     */
    public static BigInteger readBigInt(packHolder pck) {
        cryAsn1 a = new cryAsn1();
        if (a.tagRead(pck)) {
            return null;
        }
        if (a.cnst) {
            return null;
        }
        if (a.tag != cryAsn1.tagInteger) {
            return null;
        }
        return a.getBigInt();
    }

    /**
     * save sequence to packet
     *
     * @param trg target packet holder
     * @param src source packet holder
     */
    public static void writeSequence(packHolder trg, packHolder src) {
        cryAsn1 a = new cryAsn1();
        a.putSequence(src);
        a.tagWrite(trg);
        trg.merge2end();
    }

    /**
     * save set to packet
     *
     * @param trg target packet holder
     * @param src source packet holder
     */
    public static void writeSet(packHolder trg, packHolder src) {
        cryAsn1 a = new cryAsn1();
        a.putSet(src);
        a.tagWrite(trg);
        trg.merge2end();
    }

    /**
     * save bit string to packet
     *
     * @param trg target packet holder
     * @param src source packet holder
     */
    public static void writeBitString(packHolder trg, packHolder src) {
        cryAsn1 a = new cryAsn1();
        a.putBitString(src);
        a.tagWrite(trg);
        trg.merge2end();
    }

    /**
     * save octet string to packet
     *
     * @param trg target packet holder
     * @param src source packet holder
     */
    public static void writeOctString(packHolder trg, packHolder src) {
        cryAsn1 a = new cryAsn1();
        a.putOctString(src);
        a.tagWrite(trg);
        trg.merge2end();
    }

    /**
     * save sequence to packet
     *
     * @param trg target packet holder
     * @param src source packet holder
     */
    public static void writeEoc(packHolder trg, packHolder src) {
        cryAsn1 a = new cryAsn1();
        a.putEoc(src);
        a.tagWrite(trg);
        trg.merge2end();
    }

    /**
     * save sequence to packet
     *
     * @param trg target packet holder
     * @param src source packet holder
     */
    public static void writeEoc2(packHolder trg, packHolder src) {
        cryAsn1 a = new cryAsn1();
        a.putEoc2(src);
        a.tagWrite(trg);
        trg.merge2end();
    }

    /**
     * save big integer to packet
     *
     * @param pck packet holder
     * @param b big integer
     */
    public static void writeBigInt(packHolder pck, BigInteger b) {
        cryAsn1 a = new cryAsn1();
        a.putBigInt(b);
        a.tagWrite(pck);
        pck.merge2end();
    }

    /**
     * save object id to packet
     *
     * @param pck packet holder
     * @param oid object id
     */
    public static void writeObjectId(packHolder pck, int[] oid) {
        cryAsn1 a = new cryAsn1();
        a.putObjectId(oid);
        a.tagWrite(pck);
        pck.merge2end();
    }

    /**
     * save null to packet
     *
     * @param pck packet holder
     */
    public static void writeNull(packHolder pck) {
        cryAsn1 a = new cryAsn1();
        a.putNull();
        a.tagWrite(pck);
        pck.merge2end();
    }

    /**
     * dump packet
     *
     * @param lst list to use
     * @param beg beginning
     * @param p1 packet to dump
     */
    public static void dumpPack(List<String> lst, String beg, packHolder p1) {
        for (;;) {
            cryAsn1 a = new cryAsn1();
            if (a.tagRead(p1)) {
                break;
            }
            lst.add(beg + a);
            if (!a.cnst) {
                continue;
            }
            packHolder p2 = a.getPack();
            dumpPack(lst, beg + "  ", p2);
        }
    }

    /**
     * dump packet
     *
     * @param beg beginning
     * @param p1 packet to dump
     * @return list of lines
     */
    public static List<String> dumpPack(String beg, packHolder p1) {
        List<String> l = new ArrayList<String>();
        dumpPack(l, beg, p1);
        return l;
    }

}
