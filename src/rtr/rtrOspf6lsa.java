package rtr;

import java.util.Comparator;

import pack.packHolder;
import util.bits;
import addr.addrIPv4;

/**
 * ospfv3 lsa
 *
 * @author matecsaba
 */
public class rtrOspf6lsa implements Comparator<rtrOspf6lsa> {

    /**
     * header size
     */
    public static final int headSize = 20;

    /**
     * request size
     */
    public static final int reqSize = 12;

    /**
     * router lsa
     */
    public static final int lsaRouter = 0x2001;

    /**
     * network lsa
     */
    public static final int lsaNetwork = 0x2002;

    /**
     * inter area prefix lsa
     */
    public static final int lsaInterPrf = 0x2003;

    /**
     * inter area router lsa
     */
    public static final int lsaInterRtr = 0x2004;

    /**
     * as external lsa
     */
    public static final int lsaAsExt = 0x4005;

    /**
     * group membership lsa
     */
    public static final int lsaGrpMem = 0x2006;

    /**
     * nssa external lsa
     */
    public static final int lsaNssaExt = 0x2007;

    /**
     * link lsa
     */
    public static final int lsaLink = 0x0008;

    /**
     * intra area prefix lsa
     */
    public static final int lsaPrefix = 0x2009;

    /**
     * intra area traffic engineering
     */
    public static final int lsaTraffEng = 0xa00a;

    /**
     * graceful restart
     */
    public static final int lsaGrace = 0x000b;

    /**
     * router information lsa
     */
    public static final int lsaRtrInfo = 0xa00c;

    /**
     * extended router lsa
     */
    public static final int lsaErouter = 0xa021;

    /**
     * extended network lsa
     */
    public static final int lsaEnetwork = 0xa022;

    /**
     * extended inter area prefix lsa
     */
    public static final int lsaEinterPrf = 0xa023;

    /**
     * extended inter area router lsa
     */
    public static final int lsaEinterRtr = 0xa024;

    /**
     * extended as external lsa
     */
    public static final int lsaEasExt = 0xc025;

    /**
     * extended group membership lsa
     */
    public static final int lsaEgrpMem = 0xa026;

    /**
     * extended nssa external lsa
     */
    public static final int lsaEnssaExt = 0xa027;

    /**
     * extended link lsa
     */
    public static final int lsaElink = 0x8028;

    /**
     * extended intra area prefix lsa
     */
    public static final int lsaEprefix = 0xa029;

    /**
     * point to point link
     */
    public static final int lnkP2p = 1;

    /**
     * transit network
     */
    public static final int lnkTrns = 2;

    /**
     * virtual link
     */
    public static final int lnkVirt = 4;

    /**
     * max lsa age
     */
    public static final int lsaMaxAge = 3600000;

    /**
     * no unicast
     */
    public static final int prefNoUni = 0x01;

    /**
     * local address
     */
    public static final int prefLocAdr = 0x02;

    /**
     * propagate
     */
    public static final int prefProp = 0x08;

    /**
     * down
     */
    public static final int prefDown = 0x10;

    /**
     * node
     */
    public static final int prefNode = 0x20;

    /**
     * router link tlv
     */
    public static final int tlvRouter = 0x0001;

    /**
     * attached routers tlv
     */
    public static final int tlvAttach = 0x0002;

    /**
     * inter area prefix tlv
     */
    public static final int tlvInterPrf = 0x0003;

    /**
     * inter area router tlv
     */
    public static final int tlvInterRtr = 0x0004;

    /**
     * as external tlv
     */
    public static final int tlvAsExt = 0x0005;

    /**
     * intra area prefix tlv
     */
    public static final int tlvPrefix = 0x0006;

    /**
     * ipv6 link tlv
     */
    public static final int tlvLink6 = 0x0007;

    /**
     * ipv4 link tlv
     */
    public static final int tlvLink4 = 0x0008;

    /**
     * time when lsa created
     */
    public long created;

    /**
     * do not age lsa
     */
    public boolean doNotAge;

    /**
     * type of lsa
     */
    public int lsaType;

    /**
     * link state id
     */
    public int lsaID;

    /**
     * advertising router id
     */
    public addrIPv4 rtrID;

    /**
     * link state sequence number
     */
    public int sequence;

    /**
     * checksum of stored data
     */
    public int bufSum;

    /**
     * link state data
     */
    protected byte[] bufDat;

    /**
     * create empty lsa
     */
    public rtrOspf6lsa() {
        lsaID = 0;
        rtrID = new addrIPv4();
        bufDat = new byte[0];
    }

    /**
     * convert lsa type to string
     *
     * @param i lsa type
     * @return converted string
     */
    public static String lsaType2string(int i) {
        switch (i) {
            case lsaRouter:
                return "router";
            case lsaNetwork:
                return "network";
            case lsaInterPrf:
                return "iaPrefix";
            case lsaInterRtr:
                return "iaRouter";
            case lsaAsExt:
                return "asExternal";
            case lsaGrpMem:
                return "groupMembership";
            case lsaNssaExt:
                return "nssaExternal";
            case lsaLink:
                return "link";
            case lsaPrefix:
                return "prefix";
            case lsaTraffEng:
                return "traffEng";
            case lsaGrace:
                return "grace";
            case lsaRtrInfo:
                return "rtrInfo";
            case lsaErouter:
                return "ext-router";
            case lsaEnetwork:
                return "ext-network";
            case lsaEinterPrf:
                return "ext-iaPrefix";
            case lsaEinterRtr:
                return "ext-iaRouter";
            case lsaEasExt:
                return "ext-asExternal";
            case lsaEgrpMem:
                return "ext-groupMembership";
            case lsaEnssaExt:
                return "ext-nssaExternal";
            case lsaElink:
                return "ext-link";
            case lsaEprefix:
                return "ext-prefix";
            default:
                return "unknown=" + i;
        }
    }

    public String toString() {
        String a;
        if (bufDat == null) {
            a = "?";
        } else {
            a = "" + bufDat.length;
        }
        String b;
        if (doNotAge) {
            b = "dna";
        } else {
            b = bits.timePast(created);
        }
        return rtrID + "|" + lsaID + "|" + bits.toHexD(sequence) + "|" + lsaType2string(lsaType) + "|" + a + "|" + b;
    }

    /**
     * check if other is newer
     *
     * @param other other lsa to compare to
     * @return true if other newer, false if this is newer
     */
    public boolean otherNewer(rtrOspf6lsa other) {
        final int mask = 0x7fffffff;
        if ((other.sequence & mask) > (sequence & mask)) {
            return true;
        }
        return false;
    }

    /**
     * clone this lsa
     *
     * @param payload copy payload too
     * @return copied lsa object
     */
    public rtrOspf6lsa copyBytes(boolean payload) {
        rtrOspf6lsa n = new rtrOspf6lsa();
        n.created = created;
        n.sequence = sequence;
        n.lsaType = lsaType;
        n.lsaID = lsaID;
        n.rtrID = rtrID.copyBytes();
        n.bufSum = bufSum;
        if (!payload) {
            return n;
        }
        n.bufDat = new byte[bufDat.length];
        bits.byteCopy(bufDat, 0, n.bufDat, 0, bufDat.length);
        return n;
    }

    public int compare(rtrOspf6lsa o1, rtrOspf6lsa o2) {
        if (o1.lsaType < o2.lsaType) {
            return -1;
        }
        if (o1.lsaType > o2.lsaType) {
            return +1;
        }
        if (o1.lsaID < o2.lsaID) {
            return -1;
        }
        if (o1.lsaID > o2.lsaID) {
            return +1;
        }
        return o1.rtrID.compare(o1.rtrID, o2.rtrID);
    }

    /**
     * write request
     *
     * @param pck packet to write to
     * @param ofs offset where to write
     * @return bytes written
     */
    public int writeReq(packHolder pck, int ofs) {
        pck.msbPutD(ofs + 0, lsaType); // type
        pck.msbPutD(ofs + 4, lsaID); // lsa id
        pck.putAddr(ofs + 8, rtrID); // router id
        return reqSize;
    }

    /**
     * read request
     *
     * @param pck packet to read from
     * @param ofs offset where from read
     * @return bytes readed, negative on error
     */
    public int readReq(packHolder pck, int ofs) {
        if (pck.dataSize() < reqSize) {
            return -1;
        }
        lsaType = pck.msbGetD(ofs + 0); // type
        lsaID = pck.msbGetD(ofs + 4); // lsa id
        pck.getAddr(rtrID, ofs + 8); // router id
        return reqSize;
    }

    /**
     * write lsa
     *
     * @param pck packet to write to
     * @param ofs offset where to write
     * @param payload copy payload too
     * @return bytes written
     */
    public int writeData(packHolder pck, int ofs, boolean payload) {
        int i;
        if (doNotAge) {
            i = (int) created;
        } else {
            i = (int) ((bits.getTime() - created) / 1000);
        }
        if (i < 0) {
            i = 0;
        }
        if (i > 0x7fff) {
            i = 0x7fff;
        }
        if (doNotAge) {
            i |= 0x8000;
        }
        pck.msbPutW(ofs + 0, i); // age
        pck.msbPutW(ofs + 2, lsaType); // type
        pck.msbPutD(ofs + 4, lsaID); // lsa id
        pck.putAddr(ofs + 8, rtrID); // router id
        pck.msbPutD(ofs + 12, sequence); // sequence number
        pck.msbPutW(ofs + 16, bufSum); // checksum
        pck.msbPutW(ofs + 18, bufDat.length + headSize); // size
        if (!payload) {
            return headSize;
        }
        pck.putCopy(bufDat, 0, ofs + headSize, bufDat.length);
        return headSize + bufDat.length;
    }

    /**
     * read lsa
     *
     * @param pck packet to read from
     * @param ofs offset where from read
     * @param payload copy payload too
     * @return bytes readed, negative on error
     */
    public int readData(packHolder pck, int ofs, boolean payload) {
        if (pck.dataSize() < (ofs + headSize)) {
            return -1;
        }
        int i = pck.msbGetW(ofs + 0); // age
        doNotAge = (i & 0x8000) != 0;
        if (doNotAge) {
            created = i;
        } else {
            created = bits.getTime() - (i * 1000);
        }
        lsaType = pck.msbGetW(ofs + 2); // type
        lsaID = pck.msbGetD(ofs + 4); // lsa id
        pck.getAddr(rtrID, ofs + 8); // router id
        sequence = pck.msbGetD(ofs + 12); // sequence number
        bufSum = pck.msbGetW(ofs + 16); // checksum
        i = pck.msbGetW(ofs + 18); // length
        if (i < headSize) {
            return -2;
        }
        if (!payload) {
            return headSize;
        }
        bufDat = new byte[i - headSize];
        if (pck.dataSize() < (ofs + headSize + bufDat.length)) {
            return -3;
        }
        if (bufSum != pck.getISOsum(ofs + 2, headSize + bufDat.length - 2, 14)) {
            return -4;
        }
        pck.getCopy(bufDat, 0, ofs + headSize, bufDat.length);
        return headSize + bufDat.length;
    }

    /**
     * calculate lsa checksum
     */
    public void generateCheckSum() {
        packHolder pck = new packHolder(true, true);
        int i = writeData(pck, 0, true);
        bufSum = pck.putISOsum(2, i - 2, 14);
    }

    /**
     * get payload as packet
     *
     * @return payload
     */
    public packHolder getPayload() {
        packHolder pck = new packHolder(true, true);
        pck.putCopy(bufDat, 0, 0, bufDat.length);
        pck.putSkip(bufDat.length);
        pck.merge2beg();
        return pck;
    }

    /**
     * check if content differs
     *
     * @param other other lsa to check
     * @return true if yes, false if no
     */
    public boolean contentDiffers(rtrOspf6lsa other) {
        if (other == null) {
            return true;
        }
        if (bufDat.length != other.bufDat.length) {
            return true;
        }
        return bits.byteComp(bufDat, 0, other.bufDat, 0, bufDat.length) != 0;
    }

}
