package pack;

import addr.addrIP;
import addr.addrIPv4;
import addr.addrIPv6;
import addr.addrPrefix;
import ip.ipFwdIface;
import java.util.ArrayList;
import java.util.List;
import rtr.rtrBgpUtil;
import tab.tabRouteEntry;
import util.bits;
import util.typLenVal;

/**
 * protocol independent multicast (rfc4601) packet
 *
 * @author matecsaba
 */
public class packPim {

    /**
     * protocol number
     */
    public final static int proto = 103;

    /**
     * size of header
     */
    public final static int size = 4;

    /**
     * hello
     */
    public static final int typHello = 0;

    /**
     * register
     */
    public static final int typReg = 1;

    /**
     * register ack
     */
    public static final int typRegStop = 2;

    /**
     * join/prune
     */
    public static final int typJoin = 3;

    /**
     * bootstrap
     */
    public static final int typBtstrp = 4;

    /**
     * assert
     */
    public static final int typAssert = 5;

    /**
     * graft
     */
    public static final int typGrft = 6;

    /**
     * graft ack
     */
    public static final int typGrftAck = 7;

    /**
     * candidate rp
     */
    public static final int typCndRp = 8;

    /**
     * state refresh
     */
    public static final int typSttFrsh = 9;

    /**
     * designated forwarder election
     */
    public static final int typDfElect = 10;

    /**
     * hold time
     */
    public final static int tlvHoldTime = 1;

    /**
     * prune delay
     */
    public final static int tlvPrnDly = 2;

    /**
     * dr priority
     */
    public final static int tlvDrPri = 19;

    /**
     * generation id
     */
    public final static int tlvGenId = 20;

    /**
     * state refresh
     */
    public final static int tlvSttfrsh = 21;

    /**
     * bidirection capable
     */
    public final static int tlvBiDir = 22;

    /**
     * address list
     */
    public final static int tlvAdrLst = 24;

    /**
     * join attribute
     */
    public final static int tlvJoinAtr = 26;

    private typLenVal tlv = new typLenVal(0, 16, 16, 16, 1, 0, 4, 1, 0, 1024, true);

    /**
     * type of message
     */
    public int typ;

    /**
     * hold time
     */
    public int valHoldTime;

    /**
     * generation id
     */
    public int valGenId;

    /**
     * dr priority
     */
    public int valDrPri;

    /**
     * address list
     */
    public addrIP valAdrLst;

    /**
     * upstream neighbor
     */
    public addrIP upstream;

    /**
     * groups
     */
    public List<packPimGrp> groups;

    public String toString() {
        String a = "";
        if (groups != null) {
            for (int i = 0; i < groups.size(); i++) {
                a += " " + groups.get(i);
            }
        }
        return type2string(typ) + " hold=" + valHoldTime + " gen=" + valGenId + " pri=" + valDrPri + " adr=" + valAdrLst + " up=" + upstream + " grp=" + a;
    }

    /**
     * convert type to string
     *
     * @param i type
     * @return string
     */
    public static String type2string(int i) {
        switch (i) {
            case typHello:
                return "hello";
            case typReg:
                return "register";
            case typRegStop:
                return "regAck";
            case typJoin:
                return "join";
            case typBtstrp:
                return "bootstrap";
            case typAssert:
                return "assert";
            case typGrft:
                return "graft";
            case typGrftAck:
                return "graftAck";
            case typCndRp:
                return "candRp";
            case typSttFrsh:
                return "stateRefresh";
            case typDfElect:
                return "dfElect";
            default:
                return "unknown=" + i;
        }
    }

    private void writeUniAddr(packHolder pck, addrIP adr) {
        pck.putByte(1, 0); // encoding
        if (adr.isIPv4()) {
            addrIPv4 a4 = adr.toIPv4();
            pck.putByte(0, rtrBgpUtil.afiIpv4 >>> 16); // afi
            pck.putAddr(2, a4);
            pck.putSkip(addrIPv4.size + 2);
        } else {
            addrIPv6 a6 = adr.toIPv6();
            pck.putByte(0, rtrBgpUtil.afiIpv6 >>> 16); // afi
            pck.putAddr(2, a6);
            pck.putSkip(addrIPv6.size + 2);
        }
    }

    private addrIP readUniAddr(packHolder pck) {
        int afi = pck.getByte(0) << 16; // afi
        int enc = pck.getByte(1); // encoding
        pck.getSkip(2);
        if (enc != 0) {
            return null;
        }
        addrIP adr = new addrIP();
        switch (afi) {
            case rtrBgpUtil.afiIpv4:
                addrIPv4 a4 = new addrIPv4();
                pck.getAddr(a4, 0);
                pck.getSkip(addrIPv4.size);
                adr.fromIPv4addr(a4);
                return adr;
            case rtrBgpUtil.afiIpv6:
                addrIPv6 a6 = new addrIPv6();
                pck.getAddr(a6, 0);
                pck.getSkip(addrIPv6.size);
                adr.fromIPv6addr(a6);
                return adr;
            default:
                return null;
        }
    }

    private void writeGrpAddr(packHolder pck, addrPrefix<addrIP> prf, int flg) {
        tabRouteEntry<addrIP> pref = new tabRouteEntry<addrIP>();
        pref.prefix = prf;
        pck.putByte(1, 0); // encoding
        pck.putByte(2, flg); // flags
        if (prf.network.isIPv4()) {
            pck.putByte(0, rtrBgpUtil.afiIpv4 >>> 16);
            pck.putSkip(3);
            rtrBgpUtil.writePrefix(rtrBgpUtil.safiIp4uni, pck, pref);
        } else {
            pck.putByte(0, rtrBgpUtil.afiIpv6 >>> 16);
            pck.putSkip(3);
            rtrBgpUtil.writePrefix(rtrBgpUtil.safiIp6uni, pck, pref);
        }
    }

    private addrPrefix<addrIP> readGrpAddr(packHolder pck) {
        int afi = pck.getByte(0) << 16; // afi
        int enc = pck.getByte(1); // encoding
        // int flg = pck.getByte(2); // flags
        pck.getSkip(3);
        if (enc != 0) {
            return null;
        }
        tabRouteEntry<addrIP> prf = rtrBgpUtil.readPrefix(afi, false, pck);
        return prf.prefix;
    }

    /**
     * parse one header
     *
     * @param pck packet to use
     * @return false on success, true on error
     */
    public boolean parseHeader(packHolder pck) {
        typ = pck.getByte(0); // type + version
        if ((typ & 0xf0) != 0x20) {
            return true;
        }
        typ &= 0xf;
        if (pck.IPtrg.isIPv4()) {
            if (pck.getIPsum(0, pck.dataSize(), 0) != 0xffff) { // sum
                return true;
            }
        } else {
            int i = pck.pseudoIPsum(pck.dataSize());
            if (pck.getIPsum(0, pck.dataSize(), i) != 0xffff) { // sum
                return true;
            }
        }
        pck.getSkip(size);
        return false;
    }

    /**
     * create one header
     *
     * @param pck packet to use
     * @param ifc interface to use
     * @param trg target address, null=multicast
     */
    public void createHeader(packHolder pck, ipFwdIface ifc, addrIP trg) {
        pck.IPdf = false;
        pck.IPttl = 255;
        pck.IPtos = 0;
        pck.IPprt = packPim.proto;
        pck.IPsrc.setAddr(ifc.addr);
        pck.merge2beg();
        pck.putByte(0, 0x20 | typ); // type + version
        pck.putByte(1, 0); // reserved
        pck.msbPutW(2, 0); // checksum
        if (pck.IPsrc.isIPv4()) {
            if (trg == null) {
                pck.IPtrg.fromString("224.0.0.13");
            } else {
                pck.IPtrg.setAddr(trg);
            }
            int i = pck.putIPsum(0, size, 0);
            i = pck.getIPsum(0, pck.dataSize(), i);
            pck.lsbPutW(2, 0xffff - i); // checksum
        } else {
            if (trg == null) {
                pck.IPtrg.fromString("ff02::d");
            } else {
                pck.IPtrg.setAddr(trg);
            }
            int i = pck.pseudoIPsum(size + pck.dataSize());
            i = pck.putIPsum(0, size, i);
            i = pck.getIPsum(0, pck.dataSize(), i);
            pck.lsbPutW(2, 0xffff - i); // checksum
        }
        pck.putSkip(size);
        pck.merge2beg();
    }

    /**
     * parse payload tlvs
     *
     * @param pck packet to use
     * @return false on success, true on error
     */
    public boolean parsePayload(packHolder pck) {
        switch (typ) {
            case typHello:
                return parseHello(pck);
            case typJoin:
                return parseJoin(pck);
            default:
                return true;
        }
    }

    /**
     * parse hello tlvs
     *
     * @param pck packet to use
     * @return false on success, true on error
     */
    public boolean parseHello(packHolder pck) {
        for (;;) {
            if (tlv.getBytes(pck)) {
                break;
            }
            switch (tlv.valTyp) {
                case tlvHoldTime: // hold time
                    valHoldTime = bits.msbGetW(tlv.valDat, 0);
                    break;
                case tlvDrPri: // dr priority
                    valDrPri = bits.msbGetD(tlv.valDat, 0);
                    break;
                case tlvGenId: // generation id
                    valGenId = bits.msbGetD(tlv.valDat, 0);
                    break;
            }
        }
        return false;
    }

    /**
     * fill hello values
     *
     * @param helo hello interval
     * @param dr priority
     * @param gen gen id
     * @param adr interface address
     */
    public void fillHello(int helo, int dr, int gen, addrIP adr) {
        valHoldTime = (helo / 1000) * 3;
        valDrPri = dr;
        valGenId = gen;
        valAdrLst = adr.copyBytes();
    }

    /**
     * create hello tlvs
     *
     * @param pck packet to use
     */
    public void createHello(packHolder pck) {
        typ = typHello;
        bits.msbPutW(tlv.valDat, 0, valHoldTime);
        tlv.putBytes(pck, tlvHoldTime, 2, tlv.valDat);
        bits.msbPutD(tlv.valDat, 0, valDrPri);
        tlv.putBytes(pck, tlvDrPri, 4, tlv.valDat);
        bits.msbPutD(tlv.valDat, 0, valGenId);
        tlv.putBytes(pck, tlvGenId, 4, tlv.valDat);
        packHolder p = new packHolder(true, true);
        writeUniAddr(p, valAdrLst);
        p.merge2beg();
        tlv.putBytes(pck, tlvAdrLst, p.getCopy());
    }

    /**
     * parse join payload
     *
     * @param pck packet to use
     * @return false on success, true on error
     */
    public boolean parseJoin(packHolder pck) {
        upstream = readUniAddr(pck);
        if (upstream == null) {
            return true;
        }
        int grups = pck.getByte(1); // number of groups
        valHoldTime = pck.msbGetW(2); // hold time
        pck.getSkip(4);
        groups = new ArrayList<packPimGrp>();
        for (int o = 0; o < grups; o++) {
            packPimGrp grp = new packPimGrp();
            grp.group = readGrpAddr(pck);
            if (grp.group == null) {
                return true;
            }
            int joins = pck.msbGetW(0); // joined sources
            int prunes = pck.msbGetW(2); // pruned sources
            pck.getSkip(4);
            for (int i = 0; i < joins; i++) {
                addrPrefix<addrIP> src = readGrpAddr(pck);
                if (src == null) {
                    return true;
                }
                grp.joins.add(src);
            }
            for (int i = 0; i < prunes; i++) {
                addrPrefix<addrIP> src = readGrpAddr(pck);
                if (src == null) {
                    return true;
                }
                grp.prunes.add(src);
            }
            groups.add(grp);
        }
        return false;
    }

    /**
     * fill join values
     *
     * @param ups upstream
     * @param grp group
     * @param src source
     * @param helo hello interval, negative to prune
     */
    public void fillJoin(addrIP ups, addrIP grp, addrIP src, int helo) {
        upstream = ups.copyBytes();
        if (helo < 1) {
            valHoldTime = 5;
        } else {
            valHoldTime = (helo / 1000) * 3;
        }
        groups = new ArrayList<packPimGrp>();
        packPimGrp g = new packPimGrp();
        g.group = new addrPrefix<addrIP>(grp, grp.maxBits());
        if (helo < 1) {
            g.prunes.add(new addrPrefix<addrIP>(src, src.maxBits()));
        } else {
            g.joins.add(new addrPrefix<addrIP>(src, src.maxBits()));
        }
        groups.add(g);
    }

    /**
     * create join payload
     *
     * @param pck packet to use
     */
    public void createJoin(packHolder pck) {
        typ = typJoin;
        writeUniAddr(pck, upstream);
        pck.putByte(0, 0); // reserved
        pck.putByte(1, groups.size()); // number of groups
        pck.msbPutW(2, valHoldTime); // hold time
        pck.putSkip(4);
        for (int o = 0; o < groups.size(); o++) {
            packPimGrp grp = groups.get(o);
            writeGrpAddr(pck, grp.group, 0);
            pck.msbPutW(0, grp.joins.size());
            pck.msbPutW(2, grp.prunes.size());
            pck.putSkip(4);
            for (int i = 0; i < grp.joins.size(); i++) {
                writeGrpAddr(pck, grp.joins.get(i), 4);
            }
            for (int i = 0; i < grp.prunes.size(); i++) {
                writeGrpAddr(pck, grp.prunes.get(i), 4);
            }
        }
    }

}
