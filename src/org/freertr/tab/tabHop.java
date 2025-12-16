package org.freertr.tab;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrIPv6;
import org.freertr.pack.packHolder;
import org.freertr.util.bits;
import org.freertr.enc.encTlv;

/**
 * one explicit route hop
 *
 * @author matecsaba
 */
public class tabHop {

    /**
     * create instance
     */
    public tabHop() {
    }

    /**
     * address of hop
     */
    public addrIP adr = new addrIP();

    /**
     * true=strict, false=loose
     */
    public boolean strict = true;

    /**
     * true=index, false=label
     */
    public boolean index = false;

    /**
     * label value
     */
    public int label = 0;

    public String toString() {
        String a;
        if (strict) {
            a = "S";
        } else {
            a = "L";
        }
        if (label != 0) {
            a += "/";
            if (index) {
                a += "I";
            } else {
                a += "V";
            }
            a += "/" + label;
        }
        return a + "/" + adr;
    }

    /**
     * from tlv
     *
     * @param tlv tlv to use
     * @return false on success, true on error
     */
    public boolean fromTvl(encTlv tlv) {
        strict = (tlv.valTyp & 0x80) == 0;
        switch (tlv.valTyp & 0x7f) {
            case 1: // ipv4
                addrIPv4 adr4 = new addrIPv4();
                adr4.fromBuf(tlv.valDat, 0);
                adr.fromIPv4addr(adr4);
                return false;
            case 2: // ipv6
                addrIPv6 adr6 = new addrIPv6();
                adr6.fromBuf(tlv.valDat, 0);
                adr.fromIPv6addr(adr6);
                return false;
            case 36: // sid
                if ((tlv.valDat[1] & 0x0c) != 0) {
                    return true;
                }
                index = (tlv.valDat[1] & 1) == 0;
                label = bits.msbGetD(tlv.valDat, 2);
                switch (tlv.valDat[0] >>> 4) {
                    case 1:
                        adr4 = new addrIPv4();
                        adr4.fromBuf(tlv.valDat, 6);
                        adr.fromIPv4addr(adr4);
                        break;
                    case 2:
                        adr6 = new addrIPv6();
                        adr6.fromBuf(tlv.valDat, 6);
                        adr.fromIPv6addr(adr6);
                        break;
                    default:
                        return true;
                }
                return false;
            default:
                return true;
        }
    }

    /**
     * to tlv
     *
     * @param tlv tlv to use
     */
    public void toTlv(encTlv tlv) {
        if (label != 0) {
            if (adr.isIPv4()) {
                tlv.valDat[0] = 0x10; // nai type
            } else {
                tlv.valDat[0] = 0x20; // nai type
            }
            tlv.valDat[1] = 0; // flags
            if (!index) {
                tlv.valDat[1] |= 1; // mpls flag
            }
            bits.msbPutD(tlv.valDat, 2, label);
            if (adr.isIPv4()) {
                adr.toIPv4().toBuffer(tlv.valDat, 6);
                tlv.valSiz = addrIPv4.size + 6;
            } else {
                adr.toIPv6().toBuffer(tlv.valDat, 6);
                tlv.valSiz = addrIPv6.size + 6;
            }
            tlv.valTyp = 36; // sid
            if (!strict) {
                tlv.valTyp |= 0x80;
            }
            return;
        }
        if (adr.isIPv4()) {
            adr.toIPv4().toBuffer(tlv.valDat, 0);
            tlv.valSiz = addrIPv4.size;
            tlv.valTyp = 1;
        } else {
            adr.toIPv6().toBuffer(tlv.valDat, 0);
            tlv.valSiz = addrIPv6.size;
            tlv.valTyp = 2;
        }
        bits.putByte(tlv.valDat, tlv.valSiz + 0, tlv.valSiz * 8); // prefix length
        bits.putByte(tlv.valDat, tlv.valSiz + 1, 0); // reserved
        tlv.valSiz += 2;
        if (!strict) {
            tlv.valTyp |= 0x80;
        }
    }

    /**
     * get tlv handler
     *
     * @return tlv
     */
    public static encTlv getTlv() {
        return new encTlv(0, 8, 8, 8, 1, 2, 2, 1, 0, 512, true);
    }

    /**
     * parse hops
     *
     * @param pck packet to read
     * @return list of hops, null on error
     */
    public static List<tabHop> parseHops(packHolder pck) {
        encTlv tlv = getTlv();
        List<tabHop> lst = new ArrayList<tabHop>();
        for (;;) {
            if (tlv.getBytes(pck)) {
                break;
            }
            tabHop hop = new tabHop();
            if (hop.fromTvl(tlv)) {
                return null;
            }
            lst.add(hop);
        }
        return lst;
    }

    /**
     * create hops
     *
     * @param pck packet to append
     * @param lst list of hops
     */
    public static void createHops(packHolder pck, List<tabHop> lst) {
        encTlv tlv = getTlv();
        for (int i = 0; i < lst.size(); i++) {
            tabHop ntry = lst.get(i);
            if (ntry == null) {
                continue;
            }
            ntry.toTlv(tlv);
            tlv.putThis(pck);
            pck.merge2end();
        }
    }

    /**
     * dump list
     *
     * @param lst list
     * @return string
     */
    public static String dumpList(List<tabHop> lst) {
        if (lst == null) {
            return "null";
        }
        String s = "";
        for (int i = 0; i < lst.size(); i++) {
            tabHop ntry = lst.get(i);
            s += " " + ntry;
        }
        return s;
    }

    /**
     * copy data
     *
     * @return copy
     */
    public tabHop copyBytes() {
        tabHop res = new tabHop();
        res.adr = adr.copyBytes();
        res.strict = strict;
        res.index = index;
        res.label = label;
        return res;
    }

}
