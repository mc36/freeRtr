package pack;

import addr.addrIP;
import addr.addrIPv4;
import addr.addrIPv6;
import addr.addrMac;
import ip.ipFwdIface;

/**
 * virtual router redundancy protocol (rfc5798) packet
 *
 * @author matecsaba
 */
public class packVrrp {

    /**
     * protocol number
     */
    public static final int proto = 112;

    /**
     * advertisement
     */
    public static final int typAdvert = 1;

    /**
     * protocol version
     */
    public int version;

    /**
     * packet type
     */
    public int type;

    /**
     * group number
     */
    public int group;

    /**
     * ip version
     */
    public boolean ipv4;

    /**
     * hello interval
     */
    public int hello;

    /**
     * priority
     */
    public int priority;

    /**
     * virtual ip address
     */
    public addrIP virtual;

    public String toString() {
        return "ver=" + version + " ip4=" + ipv4 + " type=" + type2string(type) + " helo=" + hello + " pri=" + priority + " grp=" + group + " virt=" + virtual;
    }

    /**
     * convert type to string
     *
     * @param i type
     * @return string
     */
    public static String type2string(int i) {
        switch (i) {
            case typAdvert:
                return "advertisement";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * generate group mac address
     *
     * @return generated
     */
    public addrMac genMacAddr() {
        addrMac adr = new addrMac();
        if (ipv4) {
            adr.fromString("0000.5e00.0100");
        } else {
            adr.fromString("0000.5e00.0200");
        }
        byte[] buf = adr.getBytes();
        buf[buf.length - 1] = (byte) (group & 0xff);
        return adr;
    }

    /**
     * generate target ip address
     *
     * @return generated
     */
    public addrIP genIpAddr() {
        addrIP adr = new addrIP();
        if (ipv4) {
            adr.fromString("224.0.0.18");
        } else {
            adr.fromString("ff02::12");
        }
        return adr;
    }

    private addrIP getAddr(packHolder pck, int ofs) {
        addrIP res = new addrIP();
        if (ipv4) {
            addrIPv4 a4 = new addrIPv4();
            pck.getAddr(a4, ofs);
            res.fromIPv4addr(a4);
        } else {
            addrIPv6 a6 = new addrIPv6();
            pck.getAddr(a6, ofs);
            res.fromIPv6addr(a6);
        }
        return res;
    }

    private int putAddr(packHolder pck, int ofs, addrIP adr) {
        if (ipv4) {
            addrIPv4 a4 = adr.toIPv4();
            pck.putAddr(ofs, a4);
            return addrIPv4.size;
        } else {
            addrIPv6 a6 = adr.toIPv6();
            pck.putAddr(ofs, a6);
            return addrIPv6.size;
        }
    }

    /**
     * parse one packet
     *
     * @param pck packet to read
     * @return false on success, true on error
     */
    public boolean parsePacket(packHolder pck) {
        type = pck.getByte(0); // version + type
        version = type >>> 4;
        type &= 0xf;
        int i;
        if (version >= 3) {
            i = pck.pseudoIPsum(pck.dataSize());
        } else {
            i = 0;
        }
        if (pck.getIPsum(0, pck.dataSize(), i) != 0xffff) { // sum
            return true;
        }
        group = pck.getByte(1); // group id
        priority = pck.getByte(2); // priority
        if (pck.getByte(3) < 1) { // address count
            return true;
        }
        if (version >= 3) {
            hello = (pck.msbGetW(4) & 0xfff) * 10;
        } else {
            // int authen = pck.getByte(4); // authentication type
            hello = pck.getByte(5) * 1000; // hello interval
        }
        virtual = getAddr(pck, 8); // address
        return false;
    }

    /**
     * create one packet
     *
     * @param pck packet to update
     * @param ifc interface to use
     */
    public void createPacket(packHolder pck, ipFwdIface ifc) {
        pck.IPdf = false;
        pck.IPttl = 255;
        pck.IPtos = 0;
        pck.IPprt = proto;
        pck.IPtrg.setAddr(genIpAddr());
        pck.IPsrc.setAddr(ifc.addr);
        pck.putByte(0, (version << 4) | type); // version + type
        pck.putByte(1, group); // group id
        pck.putByte(2, priority); // priority
        pck.putByte(3, 1); // number of addresses
        if (version >= 3) {
            pck.msbPutW(4, hello / 10); // hello interval
        } else {
            pck.putByte(4, 0); // authentication type
            pck.putByte(5, hello / 1000); // hello interval
        }
        pck.msbPutW(6, 0); // checksum
        int s = putAddr(pck, 8, virtual) + 8;
        int i;
        if (version >= 3) {
            i = pck.pseudoIPsum(s);
        } else {
            i = 0;
        }
        i = pck.putIPsum(0, s, i);
        pck.lsbPutW(6, 0xffff - i); // checksum
        pck.putSkip(s); // address
        if (version < 3) {
            pck.msbPutD(0, 0); // authentication 1
            pck.msbPutD(4, 0); // authentication 2
            pck.putSkip(8);
        }
        pck.merge2beg();
    }

}
