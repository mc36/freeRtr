package pack;

import addr.addrIP;
import addr.addrIPv4;
import addr.addrIPv6;
import java.util.Comparator;
import rtr.rtrBgpUtil;
import util.bits;
import util.typLenVal;

/**
 * pseudo wire over mpls (rfc4447) fec
 *
 * @author matecsaba
 */
public class packLdpPwe implements Comparator<packLdpPwe> {

    /**
     * label value
     */
    public int label;

    /**
     * type of pseudowire
     */
    public int typ;

    /**
     * control word
     */
    public boolean ctrlWrd;

    /**
     * generalized
     */
    public boolean general;

    /**
     * group id
     */
    public int grp;

    /**
     * virtual circuit id
     */
    public long vcid;

    /**
     * mtu value
     */
    public int mtu;

    /**
     * description
     */
    public String desc;

    /**
     * vc cv
     */
    public int vccv = -1;

    /**
     * source address
     */
    public addrIP srcA;

    /**
     * target address
     */
    public addrIP trgA;

    /**
     * frame relay dlci
     */
    public final static int pwtFrDlci = 1;

    /**
     * atm aal5
     */
    public final static int pwtAtmAal5 = 2;

    /**
     * atm port
     */
    public final static int pwtAtmPort = 3;

    /**
     * ethernet vlan
     */
    public final static int pwtEthVlan = 4;

    /**
     * ethernet port
     */
    public final static int pwtEthPort = 5;

    /**
     * hdlc
     */
    public final static int pwtHdlc = 6;

    /**
     * ppp
     */
    public final static int pwtPpp = 7;

    /**
     * atm vcc
     */
    public final static int pwtAtmVcc = 9;

    /**
     * atm vpc
     */
    public final static int pwtAtmVpc = 10;

    /**
     * raw ip
     */
    public final static int pwtIp = 11;

    /**
     * mtu
     */
    public final static int iprMtu = 1;

    /**
     * description
     */
    public final static int iprDesc = 3;

    /**
     * vccv
     */
    public final static int iprVccv = 12;

    private typLenVal tlv = new typLenVal(0, 8, 8, 8, 1, 2, 2, 1, 0, 512, true);

    /**
     * convert type to string
     *
     * @param i type
     * @return string
     */
    public static String type2string(int i) {
        switch (i) {
            case pwtFrDlci:
                return "fr-dlci";
            case pwtAtmAal5:
                return "atm-aal5";
            case pwtAtmPort:
                return "atm-port";
            case pwtEthVlan:
                return "vlan";
            case pwtEthPort:
                return "ethernet";
            case pwtHdlc:
                return "hdlc";
            case pwtPpp:
                return "ppp";
            case pwtAtmVcc:
                return "atm-vcc";
            case pwtAtmVpc:
                return "atm-vpc";
            case pwtIp:
                return "ip";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * convert string to type
     *
     * @param s string
     * @return type, -1 if error
     */
    public static int string2type(String s) {
        if (s.equals("fr-dlci")) {
            return pwtFrDlci;
        }
        if (s.equals("atm-aal5")) {
            return pwtAtmAal5;
        }
        if (s.equals("atm-port")) {
            return pwtAtmPort;
        }
        if (s.equals("vlan")) {
            return pwtEthVlan;
        }
        if (s.equals("ethernet")) {
            return pwtEthPort;
        }
        if (s.equals("hdlc")) {
            return pwtHdlc;
        }
        if (s.equals("ppp")) {
            return pwtPpp;
        }
        if (s.equals("atm-vcc")) {
            return pwtAtmVcc;
        }
        if (s.equals("atm-vpc")) {
            return pwtAtmVpc;
        }
        if (s.equals("ip")) {
            return pwtIp;
        }
        return -1;
    }

    public int compare(packLdpPwe o1, packLdpPwe o2) {
        if (o1.vcid < o2.vcid) {
            return -1;
        }
        if (o1.vcid > o2.vcid) {
            return +1;
        }
        return 0;
    }

    public String toString() {
        return type2string(typ) + "|" + ctrlWrd + "|" + grp + "|" + vcid + "|" + mtu + "|" + vccv + "|" + label + "|" + desc;
    }

    /**
     * parse type1 fec element
     *
     * @param pck packet to use
     */
    public void parseFec1(packHolder pck) {
        typ = pck.msbGetW(0); // type
        ctrlWrd = (typ & 0x8000) != 0;
        typ &= 0x7fff;
        int len = pck.getByte(2);
        grp = pck.msbGetD(3);
        pck.getSkip(7);
        if (len < 1) {
            return;
        }
        byte[] buf = new byte[len];
        pck.getCopy(buf, 0, 0, len);
        pck.getSkip(len);
        pck = new packHolder(true, true);
        pck.putCopy(buf, 0, 0, len);
        pck.putSkip(len);
        pck.merge2beg();
        vcid = pck.msbGetD(0);
        pck.getSkip(4);
        parseParams(pck);
    }

    /**
     * parse type2 fec element
     *
     * @param pck packet to use
     */
    public void parseFec2(packHolder pck) {
        typ = pck.msbGetW(0); // type
        ctrlWrd = (typ & 0x8000) != 0;
        typ &= 0x7fff;
        int len = pck.getByte(2);
        pck.getSkip(3);
        vcid = pck.msbGetQ(2); // agi value
        pck.getSkip(len);
    }

    /**
     * parse parameters
     *
     * @param pck parameters
     */
    public void parseParams(packHolder pck) {
        for (;;) {
            if (tlv.getBytes(pck)) {
                break;
            }
            switch (tlv.valTyp) {
                case iprMtu:
                    mtu = bits.msbGetW(tlv.valDat, 0);
                    break;
                case iprDesc:
                    desc = tlv.getStr();
                    break;
                case iprVccv:
                    vccv = bits.msbGetW(tlv.valDat, 0);
                    break;
            }
        }
    }

    /**
     * create parameters
     *
     * @return packet
     */
    public byte[] createParams() {
        packHolder pck = new packHolder(true, true);
        if (mtu > 0) {
            bits.msbPutW(tlv.valDat, 0, mtu);
            tlv.putBytes(pck, iprMtu, 2, tlv.valDat);
        }
        if (desc != null) {
            tlv.putStr(pck, iprDesc, desc);
        }
        if (vccv >= 0) {
            bits.msbPutW(tlv.valDat, 0, vccv);
            tlv.putBytes(pck, iprVccv, 2, tlv.valDat);
        }
        pck.merge2beg();
        return pck.getCopy();
    }

    private int createAddr(packHolder pck, int ofs, addrIP adr) {
        int i;
        int o;
        if (adr.isIPv4()) {
            o = rtrBgpUtil.afiIpv4;
            i = addrIPv4.size;
            pck.putAddr(ofs + 2, adr.toIPv4());
        } else {
            o = rtrBgpUtil.afiIpv6;
            i = addrIPv6.size;
            pck.putAddr(ofs + 2, adr.toIPv6());
        }
        pck.putByte(ofs, o >>> 16); // type
        pck.putByte(ofs + 1, i); // size
        return i + 2;
    }

    /**
     * create type1 fec element
     *
     * @param pck packet to use
     * @param justCore just core components
     */
    public void createFec1(packHolder pck, boolean justCore) {
        byte[] buf;
        if (justCore) {
            buf = new byte[0];
        } else {
            buf = createParams();
        }
        int i = typ;
        if (ctrlWrd) {
            i |= 0x8000;
        }
        pck.msbPutW(0, i);
        pck.putByte(2, buf.length + 4);
        pck.msbPutD(3, grp);
        pck.msbPutD(7, (int) vcid);
        pck.putSkip(11);
        pck.putCopy(buf, 0, 0, buf.length);
        pck.putSkip(buf.length);
    }

    /**
     * create type2 fec element
     *
     * @param pck packet to use
     */
    public void createFec2(packHolder pck) {
        int i = typ;
        if (ctrlWrd) {
            i |= 0x8000;
        }
        pck.msbPutW(0, i);
        pck.putByte(3, 1); // agi type
        pck.putByte(4, 8); // agi length
        pck.msbPutQ(5, vcid); // agi value
        i = 13;
        i += createAddr(pck, i, srcA); // source address
        i += createAddr(pck, i, trgA); // target address
        pck.putByte(2, i - 3); // info length
        pck.putSkip(i);
    }

    /**
     * check if differs from other
     *
     * @param other other to test
     * @return false on equals, true on differs
     */
    public boolean differs(packLdpPwe other) {
        if (other == null) {
            return true;
        }
        if (general != other.general) {
            return true;
        }
        if (grp != other.grp) {
            return true;
        }
        if (vcid != other.vcid) {
            return true;
        }
        if (typ != other.typ) {
            return true;
        }
        if (label != other.label) {
            return true;
        }
        if (mtu != other.mtu) {
            return true;
        }
        return false;
    }

}
