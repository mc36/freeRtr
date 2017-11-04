package rtr;

import java.util.Comparator;

import pack.packHolder;
import util.bits;
import addr.addrIPv4;

/**
 * ospfv2 lsa
 *
 * @author matecsaba
 */
public class rtrOspf4lsa implements Comparator<rtrOspf4lsa> {

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
    public static final int lsaRouter = 1;

    /**
     * network lsa
     */
    public static final int lsaNetwork = 2;

    /**
     * network summary lsa
     */
    public static final int lsaSumNet = 3;

    /**
     * asbr summary lsa
     */
    public static final int lsaSumAsBr = 4;

    /**
     * as external lsa
     */
    public static final int lsaAsExt = 5;

    /**
     * group membership lsa
     */
    public static final int lsaGrpMem = 6;

    /**
     * nssa external lsa
     */
    public static final int lsaNssaExt = 7;

    /**
     * link opaque lsa
     */
    public static final int lsaOpLink = 9;

    /**
     * area opaque lsa
     */
    public static final int lsaOpArea = 10;

    /**
     * as opaque lsa
     */
    public static final int lsaOpWhole = 11;

    /**
     * point to point link
     */
    public static final int lnkP2p = 1;

    /**
     * transit network
     */
    public static final int lnkTrns = 2;

    /**
     * stub network
     */
    public static final int lnkStub = 3;

    /**
     * virtual link
     */
    public static final int lnkVirt = 4;

    /**
     * max lsa age
     */
    public static final int lsaMaxAge = 3600000;

    /**
     * time when lsa created
     */
    public long created;

    /**
     * do not age lsa
     */
    public boolean doNotAge;

    /**
     * optional capability bits
     */
    public int capability;

    /**
     * type of lsa
     */
    public int lsaType;

    /**
     * link state id
     */
    public addrIPv4 lsaID;

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
    public rtrOspf4lsa() {
        lsaID = new addrIPv4();
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
            case lsaSumNet:
                return "summaryNetwork";
            case lsaSumAsBr:
                return "summaryAsbr";
            case lsaAsExt:
                return "asExternal";
            case lsaGrpMem:
                return "groupMembership";
            case lsaNssaExt:
                return "nssaExternal";
            case lsaOpLink:
                return "opaque-link";
            case lsaOpArea:
                return "opaque-area";
            case lsaOpWhole:
                return "opaque-as";
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
    public boolean otherNewer(rtrOspf4lsa other) {
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
    public rtrOspf4lsa copyBytes(boolean payload) {
        rtrOspf4lsa n = new rtrOspf4lsa();
        n.created = created;
        n.capability = capability;
        n.sequence = sequence;
        n.lsaType = lsaType;
        n.lsaID = lsaID.copyBytes();
        n.rtrID = rtrID.copyBytes();
        n.bufSum = bufSum;
        if (!payload) {
            return n;
        }
        n.bufDat = new byte[bufDat.length];
        bits.byteCopy(bufDat, 0, n.bufDat, 0, bufDat.length);
        return n;
    }

    public int compare(rtrOspf4lsa o1, rtrOspf4lsa o2) {
        if (o1.lsaType < o2.lsaType) {
            return -1;
        }
        if (o1.lsaType > o2.lsaType) {
            return +1;
        }
        int i = o1.lsaID.compare(o1.lsaID, o2.lsaID);
        if (i != 0) {
            return i;
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
        pck.putAddr(ofs + 4, lsaID); // lsa id
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
        pck.getAddr(lsaID, ofs + 4); // lsa id
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
        pck.putByte(ofs + 2, capability); // options
        pck.putByte(ofs + 3, lsaType); // type
        pck.putAddr(ofs + 4, lsaID); // lsa id
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
        capability = pck.getByte(ofs + 2); // options
        lsaType = pck.getByte(ofs + 3); // type
        pck.getAddr(lsaID, ofs + 4); // lsa id
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
    public boolean contentDiffers(rtrOspf4lsa other) {
        if (other == null) {
            return true;
        }
        if (bufDat.length != other.bufDat.length) {
            return true;
        }
        if (capability != other.capability) {
            return true;
        }
        return bits.byteComp(bufDat, 0, other.bufDat, 0, bufDat.length) != 0;
    }

}
