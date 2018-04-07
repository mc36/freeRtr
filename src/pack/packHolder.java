package pack;

import addr.addrIP;
import addr.addrMac;
import addr.addrType;
import cry.cryEncrGeneric;
import cry.cryHashGeneric;
import java.math.BigInteger;
import pipe.pipeSide;
import util.bits;

/**
 * contains one packet from ingress to egress
 *
 * @author matecsaba
 */
public class packHolder {

    /**
     * maximum size of packet
     */
    public final static int maxData = 32768;

    /**
     * maximum size of header
     */
    public final static int maxHead = 16384;

    /**
     * header of packet this is used to contain header bytes from current layer.
     * after the current layer finishes, should merge header to data.
     */
    private byte[] headD;

    /**
     * payload of packet
     */
    private byte[] dataD;

    /**
     * size of payload part
     */
    private int dataE;

    /**
     * beginning of payload part
     */
    private int dataO;

    /**
     * size of header
     */
    private int headS;

    /**
     * class of service
     */
    public int ETHcos;

    /**
     * virtual lan id
     */
    public int ETHvlan;

    /**
     * virtual type
     */
    public int ETHtype;

    /**
     * source ethernet address
     */
    public final addrMac ETHsrc = new addrMac();

    /**
     * target ethernet address
     */
    public final addrMac ETHtrg = new addrMac();

    /**
     * nsh service path
     */
    public int NSHsp;

    /**
     * nsh service index
     */
    public int NSHsi;

    /**
     * nsh ttl
     */
    public int NSHttl;

    /**
     * nsh metadata type
     */
    public int NSHmdt;

    /**
     * nsh metadata value
     */
    public byte[] NSHmdv = new byte[0];

    /**
     * mpls label
     */
    public int MPLSlabel;

    /**
     * mpls experimental
     */
    public int MPLSexp;

    /**
     * mpls ttl
     */
    public int MPLSttl;

    /**
     * mpls bottom of stack
     */
    public boolean MPLSbottom;

    /**
     * mpls entropy
     */
    public int MPLSrnd;

    /**
     * bier bitstring
     */
    public BigInteger BIERbs;

    /**
     * bier oam
     */
    public int BIERoam;

    /**
     * bier ingerss id
     */
    public int BIERid;

    /**
     * bier set id
     */
    public int BIERsi;

    /**
     * bier bitstring length
     */
    public int BIERbsl;

    /**
     * source ip address
     */
    public final addrIP IPsrc = new addrIP();

    /**
     * target ip address
     */
    public final addrIP IPtrg = new addrIP();

    /**
     * protocol after ip
     */
    public int IPprt;

    /**
     * size of ip header
     */
    public int IPsiz;

    /**
     * version of ip header
     */
    public int IPver;

    /**
     * type of service (dscp in upper 6)
     */
    public int IPtos;

    /**
     * time to live
     */
    public int IPttl;

    /**
     * identification number
     */
    public int IPid;

    /**
     * don't fragment bit
     */
    public boolean IPdf;

    /**
     * more fragment bit
     */
    public boolean IPmf;

    /**
     * fragment offset
     */
    public int IPfrg;

    /**
     * router alert type
     */
    public int IPalrt;

    /**
     * multicast destination
     */
    public boolean IPmlt;

    /**
     * broadcast destination
     */
    public boolean IPbrd;

    /**
     * linklocal destination
     */
    public boolean IPlnk;

    /**
     * internal usage, times encapsulated
     */
    public int INTsent;

    /**
     * internal usage, time arrived
     */
    public long INTtime;

    /**
     * internal usage, interface id
     */
    public int INTiface;

    /**
     * internal usage, protocol id
     */
    public int INTupper;

    /**
     * internal usage, class id
     */
    public int INTclass;

    /**
     * internal usage, qos group
     */
    public int INTqosGrp;

    /**
     * icmp type, code
     */
    public int ICMPtc;

    /**
     * tcp/udp source port
     */
    public int UDPsrc;

    /**
     * tcp/udp target port
     */
    public int UDPtrg;

    /**
     * tcp/udp header size
     */
    public int UDPsiz;

    /**
     * tcp sequence number
     */
    public int TCPseq;

    /**
     * tcp acknowledge number
     */
    public int TCPack;

    /**
     * tcp flags
     */
    public int TCPflg;

    /**
     * tcp window
     */
    public int TCPwin;

    /**
     * urgent pointer
     */
    public int TCPurg;

    /**
     * max segment size
     */
    public int TCPmss;

    /**
     * payload type
     */
    public int RTPtyp;

    /**
     * sync source
     */
    public int RTPsrc;

    /**
     * clear all variables except buffers
     */
    public void clear() {
        dataE = 0;
        dataO = 0;
        headS = 0;
        ETHcos = 0;
        ETHvlan = 0;
        ETHtype = 0;
        ETHsrc.fillBytes(0);
        ETHtrg.fillBytes(0);
        NSHttl = 0;
        NSHmdt = 0;
        NSHmdv = new byte[0];
        NSHsp = 0;
        NSHsi = 0;
        MPLSlabel = 0;
        MPLSexp = 0;
        MPLSttl = 0;
        MPLSrnd = 0;
        MPLSbottom = false;
        BIERbs = null;
        BIERoam = 0;
        BIERsi = 0;
        BIERid = 0;
        BIERbsl = 0;
        IPsrc.fillBytes(0);
        IPtrg.fillBytes(0);
        IPprt = 0;
        IPsiz = 0;
        IPver = 0;
        IPttl = 0;
        IPid = 0;
        IPtos = 0;
        IPdf = false;
        IPmf = false;
        IPfrg = 0;
        IPalrt = -1;
        IPmlt = false;
        IPbrd = false;
        IPlnk = false;
        INTupper = 0;
        INTiface = 0;
        INTclass = 0;
        INTtime = 0;
        INTsent = 0;
        INTqosGrp = 0;
        ICMPtc = 0;
        UDPsrc = 0;
        UDPtrg = 0;
        UDPsiz = 0;
        TCPseq = 0;
        TCPack = 0;
        TCPflg = 0;
        TCPwin = 0;
        TCPurg = 0;
        TCPmss = 0;
        RTPtyp = 0;
        RTPsrc = 0;
    }

    /**
     * copy whole state from other packet
     *
     * @param src source packet
     * @param copyHdr copy header buffer or just link it
     * @param copyDat copy data buffer or just link it
     */
    public void copyFrom(packHolder src, boolean copyHdr, boolean copyDat) {
        final int marginCopy = 64;
        clear();
        dataE = src.dataE;
        dataO = src.dataO;
        headS = src.headS;
        ETHcos = src.ETHcos;
        ETHvlan = src.ETHvlan;
        ETHtype = src.ETHtype;
        ETHsrc.setAddr(src.ETHsrc);
        ETHtrg.setAddr(src.ETHtrg);
        NSHttl = src.NSHttl;
        NSHmdt = src.NSHmdt;
        NSHmdv = bits.byteConcat(src.NSHmdv, new byte[0]);
        NSHsp = src.NSHsp;
        NSHsi = src.NSHsi;
        MPLSlabel = src.MPLSlabel;
        MPLSexp = src.MPLSexp;
        MPLSttl = src.MPLSttl;
        MPLSrnd = src.MPLSrnd;
        MPLSbottom = src.MPLSbottom;
        BIERbs = src.BIERbs;
        BIERoam = src.BIERoam;
        BIERsi = src.BIERsi;
        BIERid = src.BIERid;
        BIERbsl = src.BIERbsl;
        IPsrc.setAddr(src.IPsrc);
        IPtrg.setAddr(src.IPtrg);
        IPprt = src.IPprt;
        IPsiz = src.IPsiz;
        IPver = src.IPver;
        IPttl = src.IPttl;
        IPid = src.IPid;
        IPtos = src.IPtos;
        IPdf = src.IPdf;
        IPmf = src.IPmf;
        IPfrg = src.IPfrg;
        IPalrt = src.IPalrt;
        IPmlt = src.IPmlt;
        IPbrd = src.IPbrd;
        IPlnk = src.IPlnk;
        INTupper = src.INTupper;
        INTiface = src.INTiface;
        INTclass = src.INTclass;
        INTtime = src.INTtime;
        INTsent = src.INTsent;
        INTqosGrp = src.INTqosGrp;
        ICMPtc = src.ICMPtc;
        UDPsrc = src.UDPsrc;
        UDPtrg = src.UDPtrg;
        UDPsiz = src.UDPsiz;
        TCPseq = src.TCPseq;
        TCPack = src.TCPack;
        TCPflg = src.TCPflg;
        TCPwin = src.TCPwin;
        TCPurg = src.TCPurg;
        TCPmss = src.TCPmss;
        RTPtyp = src.RTPtyp;
        RTPsrc = src.RTPsrc;
        if (copyHdr) {
            int i = headS + marginCopy;
            if (i > headD.length) {
                i = headD.length;
            }
            bits.byteCopy(src.headD, 0, headD, 0, i);
        } else {
            headD = src.headD;
        }
        if (copyDat) {
            int i = dataE + marginCopy;
            if (i > dataD.length) {
                i = dataD.length;
            }
            bits.byteCopy(src.dataD, 0, dataD, 0, i);
        } else {
            dataD = src.dataD;
        }
    }

    /**
     * creates new packet container
     *
     * @param hdr allocate buffer to header
     * @param dat allocate buffer to data
     */
    public packHolder(boolean hdr, boolean dat) {
        clear();
        allocNorm(hdr, dat);
    }

    /**
     * convert this packet to string
     *
     * @return hex dump of packet
     */
    public String dump() {
        int i = dataO - 64;
        if (i < 0) {
            i = 0;
        }
        return bits.byteDump(dataD, i, dataO - i) + " |" + bits.byteDump(headD, 0, headS) + " |" + bits.byteDump(dataD, dataO, dataE - dataO);
    }

    /**
     * skip bytes in payload should called when current layer finished reading
     * header
     *
     * @param bytes bytes parsed at this layer
     */
    public void getSkip(int bytes) {
        dataO += bytes;
    }

    /**
     * bytes still unreaded in payload
     *
     * @return bytes left
     */
    public int dataSize() {
        return dataE - dataO;
    }

    /**
     * byte position in payload
     *
     * @return byte offset
     */
    public int dataOffset() {
        return dataO;
    }

    /**
     * bytes written by current layer
     *
     * @return bytes in header
     */
    public int headSize() {
        return headS;
    }

    /**
     * set number of remaining bytes sets the end pointer (trunacetes)
     *
     * @param i number of bytes
     */
    public void setDataSize(int i) {
        dataE = dataO + i;
    }

    /**
     * set bytes left sets by keeping end pointer
     *
     * @param i bytes left
     */
    public void setBytesLeft(int i) {
        dataO = dataE - i;
    }

    /**
     * skip bytes in header buffer
     *
     * @param bytes bytes written by current layer
     */
    public void putSkip(int bytes) {
        headS += bytes;
    }

    /**
     * start writing header to header buffer
     */
    public void putStart() {
        headS = 0;
    }

    /**
     * read zero terminated ascii from header buffer
     *
     * @param rofs offset from read
     * @param max maximum string size
     * @param term terminator character
     * @return string read
     */
    public String getAsciiZ(int rofs, int max, int term) {
        byte[] buf = new byte[max];
        for (int i = 0; i < max; i++) {
            buf[i] = dataD[dataO + rofs + i];
        }
        for (int i = max - 1; i >= 0; i--) {
            if (buf[i] == term) {
                max = i;
            }
        }
        return new String(buf, 0, max);
    }

    /**
     * write zero terminated ascii from header buffer
     *
     * @param rofs offset from read
     * @param max maximum string size
     * @param str string to write
     * @param fill filler character
     */
    public void putAsciiZ(int rofs, int max, String str, int fill) {
        byte[] buf1 = new byte[max];
        byte[] buf2 = str.getBytes();
        if (max > buf2.length) {
            max = buf2.length;
        }
        bits.byteCopy(buf2, 0, buf1, 0, max);
        for (int i = max; i < buf1.length; i++) {
            buf1[i] = (byte) fill;
        }
        bits.byteCopy(buf1, 0, headD, headS + rofs, buf1.length);
    }

    /**
     * read u8 from header buffer
     *
     * @param rofs offset from read
     * @return value read
     */
    public int getByte(int rofs) {
        return bits.getByte(dataD, dataO + rofs);
    }

    /**
     * read a bit to header buffer
     *
     * @param rofs byte offset where to write
     * @param bit bit offset where to write
     * @return value to write
     */
    public boolean getBit(int rofs, int bit) {
        return bits.bitGet(dataD, dataO + rofs, bit);
    }

    /**
     * read MSB u16 from header buffer
     *
     * @param rofs offset from read
     * @return value read
     */
    public int msbGetW(int rofs) {
        return bits.msbGetW(dataD, dataO + rofs);
    }

    /**
     * read MSB u32 from header buffer
     *
     * @param rofs offset from read
     * @return value read
     */
    public int msbGetD(int rofs) {
        return bits.msbGetD(dataD, dataO + rofs);
    }

    /**
     * read MSB u32 from header buffer
     *
     * @param rofs offset from read
     * @return value read
     */
    public long msbGetQ(int rofs) {
        return bits.msbGetQ(dataD, dataO + rofs);
    }

    /**
     * read LSB u16 from header buffer
     *
     * @param rofs offset from read
     * @return value read
     */
    public int lsbGetW(int rofs) {
        return bits.lsbGetW(dataD, dataO + rofs);
    }

    /**
     * read LSB u32 from header buffer
     *
     * @param rofs offset from read
     * @return value read
     */
    public int lsbGetD(int rofs) {
        return bits.lsbGetD(dataD, dataO + rofs);
    }

    /**
     * read LSB u64 from header buffer
     *
     * @param rofs offset from read
     * @return value read
     */
    public long lsbGetQ(int rofs) {
        return bits.lsbGetQ(dataD, dataO + rofs);
    }

    /**
     * write u8 to header buffer
     *
     * @param rofs offset where to write
     * @param val value to write
     */
    public void putByte(int rofs, int val) {
        bits.putByte(headD, headS + rofs, val);
    }

    /**
     * write a bit to header buffer
     *
     * @param rofs byte offset where to write
     * @param bit bit offset where to write
     * @param val value to write
     */
    public void putBit(int rofs, int bit, boolean val) {
        bits.bitSet(headD, headS + rofs, bit, val);
    }

    /**
     * write MSB u16 to header buffer
     *
     * @param rofs offset where to write
     * @param val value to write
     */
    public void msbPutW(int rofs, int val) {
        bits.msbPutW(headD, headS + rofs, val);
    }

    /**
     * write MSB u32 to header buffer
     *
     * @param rofs offset where to write
     * @param val value to write
     */
    public void msbPutD(int rofs, int val) {
        bits.msbPutD(headD, headS + rofs, val);
    }

    /**
     * write MSB u64 to header buffer
     *
     * @param rofs offset where to write
     * @param val value to write
     */
    public void msbPutQ(int rofs, long val) {
        bits.msbPutQ(headD, headS + rofs, val);
    }

    /**
     * write LSB u16 to header buffer
     *
     * @param rofs offset where to write
     * @param val value to write
     */
    public void lsbPutW(int rofs, int val) {
        bits.lsbPutW(headD, headS + rofs, val);
    }

    /**
     * write LSB u32 to header buffer
     *
     * @param rofs offset where to write
     * @param val value to write
     */
    public void lsbPutD(int rofs, int val) {
        bits.lsbPutD(headD, headS + rofs, val);
    }

    /**
     * write LSB u64 to header buffer
     *
     * @param rofs offset where to write
     * @param val value to write
     */
    public void lsbPutQ(int rofs, long val) {
        bits.lsbPutQ(headD, headS + rofs, val);
    }

    /**
     * fill in header buffer
     *
     * @param rofs offset where to write
     * @param len bytes to write
     * @param val filler byte
     */
    public void putFill(int rofs, int len, int val) {
        bits.byteFill(headD, headS + rofs, len, val);
    }

    /**
     * write to header buffer from other buffer
     *
     * @param buf source buffer
     * @param ofs source offset
     * @param rofs where to copy
     * @param len bytes to copy
     */
    public void putCopy(byte[] buf, int ofs, int rofs, int len) {
        bits.byteCopy(buf, ofs, headD, headS + rofs, len);
    }

    /**
     * read from data buffer to other buffer
     *
     * @param buf target buffer
     * @param ofs target offset
     * @param rofs where from copy
     * @param len bytes to copy
     */
    public void getCopy(byte[] buf, int ofs, int rofs, int len) {
        bits.byteCopy(dataD, dataO + rofs, buf, ofs, len);
    }

    /**
     * read from data buffer to other buffer
     *
     * @return bytes in data buffer
     */
    public byte[] getCopy() {
        byte[] buf = new byte[dataSize()];
        getCopy(buf, 0, 0, buf.length);
        return buf;
    }

    /**
     * write to header buffer from address
     *
     * @param rofs where to copy
     * @param adr address to write
     */
    public void putAddr(int rofs, addrType adr) {
        bits.byteCopy(adr.getBytes(), 0, headD, headS + rofs, adr.getSize());
    }

    /**
     * read address from packet
     *
     * @param adr address to read
     * @param rofs where from copy
     */
    public void getAddr(addrType adr, int rofs) {
        adr.fromBuf(dataD, dataO + rofs);
    }

    /**
     * calculate iso checksum over packet data bytes
     *
     * @param rofs where from start
     * @param len bytes to calculate
     * @param sum offset of checksum from initial offset
     * @return computed checksum
     */
    public int getISOsum(int rofs, int len, int sum) {
        return bits.byteISOsum(dataD, dataO + rofs, len, sum);
    }

    /**
     * calculate iso checksum over packet header bytes
     *
     * @param rofs where from start
     * @param len bytes to calculate
     * @param sum offset of checksum from initial offset
     * @return computed checksum
     */
    public int putISOsum(int rofs, int len, int sum) {
        return bits.byteISOsum(headD, headS + rofs, len, sum);
    }

    /**
     * calculate ip checksum over packet data bytes
     *
     * @param rofs where from start
     * @param len bytes to calculate
     * @param sum initial checksum
     * @return computed checksum
     */
    public int getIPsum(int rofs, int len, int sum) {
        return bits.byteIPsum(dataD, dataO + rofs, len, sum);
    }

    /**
     * calculate ip checksum over packet header bytes
     *
     * @param rofs where from start
     * @param len bytes to calculate
     * @param sum initial checksum
     * @return computed checksum
     */
    public int putIPsum(int rofs, int len, int sum) {
        return bits.byteIPsum(headD, headS + rofs, len, sum);
    }

    /**
     * calculate ip pseudo header checksum
     *
     * @param len length of payload
     * @return checksum calculated
     */
    public int pseudoIPsum(int len) {
        byte buf[] = new byte[4];
        bits.msbPutW(buf, 0, IPprt);
        bits.msbPutW(buf, 2, len);
        int sum = bits.byteIPsum(IPsrc.getBytes(), 0, IPsrc.getSize(), 0);
        sum = bits.byteIPsum(IPtrg.getBytes(), 0, IPtrg.getSize(), sum);
        return bits.byteIPsum(buf, 0, buf.length, sum);
    }

    /**
     * allocate new buffers
     *
     * @param hdr header buffer
     * @param dat data buffer
     */
    public void allocNorm(boolean hdr, boolean dat) {
        if (hdr) {
            headD = new byte[maxHead];
        }
        if (dat) {
            dataD = new byte[maxData];
        }
    }

    /**
     * allocate new buffers
     *
     * @param hdr size of header buffer
     * @param dat size of data buffer
     */
    public void allocHuge(int hdr, int dat) {
        if (hdr > 0) {
            headD = new byte[hdr];
        }
        if (dat > 0) {
            dataD = new byte[dat];
        }
    }

    /**
     * merge header before payload
     *
     * @param newOfs force beginning in data buffer (-1=no force)
     * @param headKeep byte to keep at beginning of header buffer
     */
    public void mergeHeader(int newOfs, int headKeep) {
        final int marginMove = 512;
        if (headS < 0) {
            headS = 0;
        }
        if (headKeep > headS) {
            headKeep = headS;
        }
        if (headKeep < 0) {
            headKeep = 0;
        }
        dataE -= dataO;
        if (dataE < 0) {
            dataE = 0;
        }
        headS -= headKeep;
        if (newOfs < 0) {
            newOfs = dataO - headS;
            if (newOfs < 0) {
                newOfs = dataD.length - dataE - headS - marginMove;
            }
        }
        if (dataO != (newOfs + headS)) {
            bits.byteCopy(dataD, dataO, dataD, newOfs + headS, dataE);
        }
        bits.byteCopy(headD, headKeep, dataD, newOfs, headS);
        dataE += headS;
        dataE += newOfs;
        dataO = newOfs;
        headS = headKeep;
    }

    /**
     * merge header to beginning of data
     */
    public void merge2beg() {
        mergeHeader(-1, 0);
    }

    /**
     * merge header to end of data
     */
    public void merge2end() {
        if ((headS + dataE) >= dataD.length) {
            mergeHeader(0, headS);
        }
        bits.byteCopy(headD, 0, dataD, dataE, headS);
        dataE += headS;
        headS = 0;
    }

    /**
     * unmerge bytes from beginning of payload to header buffer
     *
     * @param len
     */
    public void unMergeBytes(int len) {
        bits.byteCopy(dataD, dataO, headD, headS, len);
        dataO += len;
        headS += len;
    }

    /**
     * clone this packet
     *
     * @param allocHdr allocate new header buffer or just link it
     * @param allocDat allocate new data buffer or just link it
     * @return copied packet object
     */
    public packHolder copyBytes(boolean allocHdr, boolean allocDat) {
        packHolder n = new packHolder(allocHdr, allocDat);
        n.copyFrom(this, allocHdr, allocDat);
        return n;
    }

    /**
     * get pcap header
     *
     * @param typ type of packet: 1=ethernet, 9=ppp, 104=hdlc, 107=framerelay,
     * 228=ip4, 229=ip6
     * @return byte representing header
     */
    public static byte[] getPcapHeader(int typ) {
        byte[] buf = new byte[24];
        bits.msbPutD(buf, 0, 0xa1b2c3d4); // magic number
        bits.msbPutD(buf, 4, 0x00020004); // version
        bits.msbPutD(buf, 8, 0); // timezone
        bits.msbPutD(buf, 12, 0); // precision
        bits.msbPutD(buf, 16, 0); // max length
        bits.msbPutD(buf, 20, typ); // network type
        return buf;
    }

    /**
     * convert packet to pcap format
     *
     * @param tim time
     * @param prepend prepend ethernet header
     * @return byte representing packet in pcap
     */
    public byte[] convertToPcap(long tim, boolean prepend) {
        int hdr = 16;
        if (prepend) {
            hdr += addrMac.sizeX2;
        }
        final int siz = dataE - dataO;
        byte[] buf = new byte[hdr + headS + siz];
        bits.msbPutD(buf, 0, (int) (tim / 1000)); // seconds
        bits.msbPutD(buf, 4, (int) (tim % 1000)); // microsecs
        bits.msbPutD(buf, 8, buf.length - 16); // included len
        bits.msbPutD(buf, 12, buf.length - 16); // original len
        if (prepend) {
            ETHtrg.toBuffer(buf, 16);
            ETHsrc.toBuffer(buf, 22);
        }
        bits.byteCopy(headD, 0, buf, hdr, headS);
        bits.byteCopy(dataD, dataO, buf, hdr + headS, siz);
        return buf;
    }

    /**
     * convert packet to K12 format
     *
     * @param tim time
     * @return string representing packet in K12
     */
    public String convertToK12(long tim) {
        String a = " " + dump() + " ";
        a = a.replaceAll(" ", "|");
        String b = "X" + a;
        while (!a.equals(b)) {
            b = a;
            a = a.replaceAll("\\|\\|", "|");
        }
        b = bits.time2str(null, tim, 2) + "," + bits.padBeg((tim % 1000) + "", 3, "0") + ",000";
        return "+---------+---------------+----------+\n" + b + "   ETHER\n" + "|0   " + a + "\n\n";
    }

    /**
     * convert packet from K12 format
     *
     * @param s packet in K12 representation
     * @return false on success, true on error
     */
    public boolean convertFromK12(String s) {
        if (!s.startsWith("|0   |")) {
            return true;
        }
        s = " " + s.substring(4, s.length()) + " ";
        s = s.replaceAll("\\|", " ").replaceAll(" 0x", " ");
        for (;;) {
            s = s.trim();
            int i = s.indexOf(" ");
            String a;
            if (i < 0) {
                a = s;
                s = "";
            } else {
                a = s.substring(0, i).trim();
                s = s.substring(i, s.length());
            }
            if (a.length() < 1) {
                break;
            }
            try {
                i = Integer.parseInt(a, 16);
            } catch (Exception e) {
                return true;
            }
            putByte(0, i);
            putSkip(1);
            merge2end();
        }
        return false;
    }

    /**
     * get data buffer
     *
     * @return byte array of data buffer
     */
    public byte[] getDataArray() {
        return dataD;
    }

    /**
     * get head buffer
     *
     * @return byte array of head buffer
     */
    public byte[] getHeadArray() {
        return headD;
    }

    /**
     * put default values for sending
     */
    public void putDefaults() {
        IPttl = -1;
        IPtos = -1;
        IPdf = false;
        IPmlt = false;
        IPbrd = false;
        IPlnk = false;
    }

    /**
     * read bytes from data to pipeline
     *
     * @param pipe pipeline side
     * @param rofs where from copy
     * @param len bytes to copy
     * @param blocking 1=nonblocking 2=blocking 3=more
     * @return value returned by put on pipeline
     */
    public int pipeSend(pipeSide pipe, int rofs, int len, int blocking) {
        switch (blocking) {
            case 1:
                return pipe.nonBlockPut(dataD, dataO + rofs, len);
            case 2:
                return pipe.blockingPut(dataD, dataO + rofs, len);
            case 3:
                return pipe.morePut(dataD, dataO + rofs, len);
            default:
                return 0;
        }
    }

    /**
     * write to header buffer from pipeline
     *
     * @param pipe pipeline side
     * @param rofs where to copy
     * @param len bytes to copy
     * @param blocking xxx1=nondestructive xxx2=nonblocking xxx3=blocking
     * xxx4=more xx0x=and nothing xx1x=and skip after xx2x=and skip after and
     * merge2beg xx3x=and skip after and merge2end xx4x=and add to data size
     * x0xx=head area x1xx=data area
     * @return value returned by nonblocking get on pipeline
     */
    public int pipeRecv(pipeSide pipe, int rofs, int len, int blocking) {
        byte buf[] = null;
        switch ((blocking / 100) % 10) {
            case 0:
                rofs += headS;
                buf = headD;
                break;
            case 1:
                rofs += dataO;
                buf = dataD;
                break;
            default:
                return 0;
        }
        int max = buf.length - rofs;
        if (len < 1) {
            len = max;
        }
        if (len > max) {
            len = max;
        }
        if (len < 1) {
            return 0;
        }
        int res;
        switch (blocking % 10) {
            case 1:
                res = pipe.nonDestructiveGet(buf, rofs, len);
                break;
            case 2:
                res = pipe.nonBlockGet(buf, rofs, len);
                break;
            case 3:
                res = pipe.blockingGet(buf, rofs, len);
                break;
            case 4:
                res = pipe.moreGet(buf, rofs, len);
                break;
            default:
                res = 0;
                break;
        }
        if (res < 1) {
            return res;
        }
        switch ((blocking / 10) % 10) {
            case 0:
                break;
            case 1:
                putSkip(res);
                break;
            case 2:
                putSkip(res);
                merge2beg();
                break;
            case 3:
                putSkip(res);
                merge2end();
                break;
            case 4:
                setDataSize(rofs + res);
                break;
            default:
                res = 0;
        }
        return res;
    }

    /**
     * update hash from data
     *
     * @param hsh hash to update
     * @param ofs offset from where
     * @param siz bytes to calculate
     */
    public void hashData(cryHashGeneric hsh, int ofs, int siz) {
        hsh.update(dataD, dataO + ofs, siz);
    }

    /**
     * update hash from header
     *
     * @param hsh hash to update
     * @param ofs offset from where
     * @param siz bytes to calculate
     */
    public void hashHead(cryHashGeneric hsh, int ofs, int siz) {
        hsh.update(headD, ofs, siz);
    }

    /**
     * update encription in data
     *
     * @param enc encrypter to update
     * @param ofs offset from where
     * @param siz bytes to calculate
     */
    public void encrData(cryEncrGeneric enc, int ofs, int siz) {
        enc.update(dataD, dataO + ofs, siz);
    }

    /**
     * update encription in header
     *
     * @param enc encrypter to update
     * @param ofs offset from where
     * @param siz bytes to calculate
     */
    public void encrHead(cryEncrGeneric enc, int ofs, int siz) {
        enc.update(headD, ofs, siz);
    }

}
