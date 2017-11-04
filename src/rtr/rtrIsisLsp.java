package rtr;

import java.util.Comparator;

import pack.packHolder;
import util.bits;
import addr.addrIsis;

/**
 * isis lsp
 *
 * @author matecsaba
 */
public class rtrIsisLsp implements Comparator<rtrIsisLsp> {

    /**
     * header size
     */
    public static final int headSize = 19;

    /**
     * sequence number size
     */
    public static final int seqSize = 16;

    /**
     * lsp id size
     */
    public static final int idSize = 8;

    /**
     * partition repair
     */
    public static final int flgPart = 0x80;

    /**
     * default attached
     */
    public static final int flgAttach = 0x08;

    /**
     * overloaded
     */
    public static final int flgOver = 0x04;

    /**
     * level info
     */
    public static final int flgLevel = 0x03;

    /**
     * area addresses
     */
    public static final int tlvAreaAddr = 1;

    /**
     * is neighbor reachability
     */
    public static final int tlvIsNeigh = 2;

    /**
     * es neighbor reachability
     */
    public static final int tlvEsNeigh = 3;

    /**
     * prefix neighbors
     */
    public static final int tlvPrefNeigh = 5;

    /**
     * lan neighbors
     */
    public static final int tlvLanNeigh = 6;

    /**
     * padding
     */
    public static final int tlvPadding = 8;

    /**
     * lsp entries
     */
    public static final int tlvLspEntries = 9;

    /**
     * authentication
     */
    public static final int tlvAuthen = 10;

    /**
     * optional checksum
     */
    public static final int tlvChecksum = 12;

    /**
     * lsp buffer size
     */
    public static final int tlvLspBufSiz = 14;

    /**
     * extended is reachability
     */
    public static final int tlvExtIsNeigh = 22;

    /**
     * is neighbor attribute
     */
    public static final int tlvIsNeighAttr = 23;

    /**
     * is alias id
     */
    public static final int tlvIsAlias = 24;

    /**
     * ipv4 internal reachability
     */
    public static final int tlvIpv4intReach = 128;

    /**
     * protocol supported
     */
    public static final int tlvProtSupp = 129;

    /**
     * ipv4 external reachability
     */
    public static final int tlvIpv4extReach = 130;

    /**
     * idrp information
     */
    public static final int tlvIdrpInfo = 131;

    /**
     * ipv4 interface address
     */
    public static final int tlvIpv4addr = 132;

    /**
     * ipv4 traffic engineering id
     */
    public static final int tlvIpv4teId = 134;

    /**
     * extended ipv4 reachability
     */
    public static final int tlvExtIpv4reach = 135;

    /**
     * dynamic name
     */
    public static final int tlvHostName = 137;

    /**
     * ipv4 shared risk link group
     */
    public static final int tlvIpv4srlg = 138;

    /**
     * ipv6 shared risk link group
     */
    public static final int tlvIpv6srlg = 139;

    /**
     * ipv6 traffic engineering id
     */
    public static final int tlvIpv6teId = 140;

    /**
     * restart
     */
    public static final int tlvRestart = 211;

    /**
     * multi topology is neighbor reachability
     */
    public static final int tlvMtIsNeigh = 222;

    /**
     * multi topology is neighbor attributes
     */
    public static final int tlvMtNeighAttr = 223;

    /**
     * multi topology identified
     */
    public static final int tlvMultiTopo = 229;

    /**
     * ipv6 interface address
     */
    public static final int tlvIpv6addr = 232;

    /**
     * multi topology ipv4 reachability
     */
    public static final int tlvMtIpv4reach = 235;

    /**
     * ipv6 reachability
     */
    public static final int tlvIpv6reach = 236;

    /**
     * multi topology ipv6 reachability
     */
    public static final int tlvMtIpv6reach = 237;

    /**
     * point to point 3-way adjacency state
     */
    public static final int tlvHandshake = 240;

    /**
     * is-is router capability
     */
    public static final int tlvRouterCapa = 242;

    /**
     * experimental
     */
    public static final int tlvExperimental = 250;

    /**
     * source id
     */
    public addrIsis srcID;

    /**
     * pseudonode id
     */
    public int nodID;

    /**
     * lsp number
     */
    public int lspNum;

    /**
     * time when lsp expires
     */
    public long expires;

    /**
     * link state sequence number
     */
    public int sequence;

    /**
     * lsp flags paaaaoll
     */
    public int flags;

    /**
     * checksum of stored data
     */
    public int bufSum;

    /**
     * link state data
     */
    protected byte[] bufDat;

    /**
     * create empty lsp
     */
    public rtrIsisLsp() {
        srcID = new addrIsis();
        bufDat = new byte[0];
    }

    /**
     * clone this lsp
     *
     * @param payload copy payload too
     * @return copied lsp object
     */
    public rtrIsisLsp copyBytes(boolean payload) {
        rtrIsisLsp n = new rtrIsisLsp();
        n.expires = expires;
        n.sequence = sequence;
        n.srcID = srcID.copyBytes();
        n.nodID = nodID;
        n.lspNum = lspNum;
        n.flags = flags;
        n.bufSum = bufSum;
        if (!payload) {
            return n;
        }
        n.bufDat = new byte[bufDat.length];
        bits.byteCopy(bufDat, 0, n.bufDat, 0, bufDat.length);
        return n;
    }

    public int compare(rtrIsisLsp o1, rtrIsisLsp o2) {
        int i = o1.srcID.compare(o1.srcID, o2.srcID);
        if (i != 0) {
            return i;
        }
        if (o1.nodID < o2.nodID) {
            return -1;
        }
        if (o1.nodID > o2.nodID) {
            return +1;
        }
        if (o1.lspNum < o2.lspNum) {
            return -1;
        }
        if (o1.lspNum > o2.lspNum) {
            return +1;
        }
        return 0;
    }

    public String toString() {
        String a;
        if (bufDat == null) {
            a = "?";
        } else {
            a = "" + bufDat.length;
        }
        return srcID + "." + bits.toHexB(nodID) + "-" + bits.toHexB(lspNum) + "|" + bits.toHexD(sequence) + "|" + bits.bit2str(flags, flgAttach, "a") + bits.bit2str(flags, flgPart, "p") + bits.bit2str(flags, flgOver, "o") + "|" + a + "|" + bits.timeLeft(expires);
    }

    /**
     * check if other is newer
     *
     * @param other other lsp to compare to
     * @return true if other newer, false if this is newer
     */
    public boolean otherNewer(rtrIsisLsp other) {
        final int mask = 0x7fffffff;
        if ((other.sequence & mask) > (sequence & mask)) {
            return true;
        }
        return false;
    }

    /**
     * get remaining lifetime
     *
     * @param allowNeg not allow negative
     * @return seconds
     */
    public int getTimeRemain(boolean allowNeg) {
        int i = (int) ((expires - bits.getTime()) / 1000);
        if (allowNeg) {
            return i;
        }
        if (i < 0) {
            i = 0;
        }
        if (i > 0xffff) {
            i = 0xffff;
        }
        return i;
    }

    /**
     * set remaining lifetime
     *
     * @param i seconds
     */
    public void setTimeRemain(int i) {
        expires = bits.getTime() + (i * 1000);
    }

    /**
     * set id value to first/last
     *
     * @param first first, or not
     */
    public void setIDvalue(boolean first) {
        int i;
        if (first) {
            i = 0;
        } else {
            i = 255;
        }
        srcID.fillBytes(i);
        nodID = i;
        lspNum = i;
    }

    /**
     * write lsp id
     *
     * @param pck packet to write to
     * @param ofs offset where to write
     * @return bytes written
     */
    public int writeId(packHolder pck, int ofs) {
        pck.putAddr(ofs + 0, srcID); // source id
        pck.putByte(ofs + 6, nodID); // node id
        pck.putByte(ofs + 7, lspNum); // lsp number
        return idSize;
    }

    /**
     * read lsp id
     *
     * @param pck packet to read from
     * @param ofs offset where from read
     * @return bytes readed, negative on error
     */
    public int readId(packHolder pck, int ofs) {
        pck.getAddr(srcID, ofs + 0); // source id
        nodID = pck.getByte(ofs + 6); // pseudonode id
        lspNum = pck.getByte(ofs + 7); // lsp number
        return idSize;
    }

    /**
     * write sequence number
     *
     * @param pck packet to write to
     * @param ofs offset where to write
     * @return bytes written
     */
    public int writeSeq(packHolder pck, int ofs) {
        pck.msbPutW(ofs + 0, getTimeRemain(false)); // lifetime remaining
        writeId(pck, ofs + 2); // lsp id
        pck.msbPutD(ofs + 10, sequence); // sequence number
        pck.msbPutW(ofs + 14, bufSum); // checksum
        return seqSize;
    }

    /**
     * read sequence number
     *
     * @param pck packet to read from
     * @param ofs offset where from read
     * @return bytes readed, negative on error
     */
    public int readSeq(packHolder pck, int ofs) {
        setTimeRemain(pck.msbGetW(ofs + 0)); // remaining lifetime
        readId(pck, ofs + 2); // lsp id
        sequence = pck.msbGetD(ofs + 10); // sequence number
        bufSum = pck.msbGetW(ofs + 14); // checksum
        return seqSize;
    }

    /**
     * write lsp
     *
     * @param pck packet to write to
     * @param ofs offset where to write
     * @return bytes written
     */
    public int writeData(packHolder pck, int ofs) {
        pck.msbPutW(ofs + 0, bufDat.length + headSize + 8); // pdu length
        pck.msbPutW(ofs + 2, getTimeRemain(false)); // remaining lifetime
        writeId(pck, ofs + 4); // lsp id
        pck.msbPutD(ofs + 12, sequence); // sequence number
        pck.msbPutW(ofs + 16, bufSum); // checksum
        pck.putByte(ofs + 18, flags); // flags
        pck.putCopy(bufDat, 0, ofs + headSize, bufDat.length);
        return headSize + bufDat.length;
    }

    /**
     * read lsp
     *
     * @param pck packet to read from
     * @param ofs offset where from read
     * @return bytes readed, negative on error
     */
    public int readData(packHolder pck, int ofs) {
        if (pck.dataSize() < (ofs + headSize)) {
            return -1;
        }
        int i = pck.msbGetW(ofs + 0) - 8; // pdu length
        if (i < headSize) {
            return -2;
        }
        setTimeRemain(pck.msbGetW(ofs + 2)); // remaining lifetime
        readId(pck, ofs + 4); // lsp id
        sequence = pck.msbGetD(ofs + 12); // sequence number
        bufSum = pck.msbGetW(ofs + 16); // checksum
        flags = pck.getByte(ofs + 18); // flags
        bufDat = new byte[i - headSize];
        if (pck.dataSize() < (ofs + i)) {
            return -3;
        }
        if (bufSum != pck.getISOsum(ofs + 4, i - 4, 12)) {
            return -4;
        }
        pck.getCopy(bufDat, 0, ofs + headSize, bufDat.length);
        return headSize + bufDat.length;
    }

    /**
     * calculate lsp checksum
     */
    public void generateCheckSum() {
        packHolder pck = new packHolder(true, true);
        int i = writeData(pck, 0);
        bufSum = pck.putISOsum(4, i - 4, 12);
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
     * @param other other lsp to check
     * @return true if yes, false if no
     */
    public boolean contentDiffers(rtrIsisLsp other) {
        if (other == null) {
            return true;
        }
        if (bufDat.length != other.bufDat.length) {
            return true;
        }
        if (flags != other.flags) {
            return true;
        }
        return bits.byteComp(bufDat, 0, other.bufDat, 0, bufDat.length) != 0;
    }

}
