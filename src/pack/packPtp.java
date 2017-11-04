package pack;

import addr.addrIP;
import addr.addrMac;
import util.bits;

/**
 * precision time protocol (ieee1588) packet
 *
 * @author matecsaba
 */
public class packPtp {

    /**
     * ethertype
     */
    public final static int ethtyp = 0x88f7;

    /**
     * size
     */
    public final static int size = 44;

    /**
     * sync port
     */
    public final static int portS = 319;

    /**
     * follow port
     */
    public final static int portF = 320;

    /**
     * time offset
     */
    public long offset;

    /**
     * clock id
     */
    public long clock;

    /**
     * subdomain
     */
    public int domain;

    /**
     * sequence
     */
    public int sequence;

    /**
     * port id
     */
    public int port;

    /**
     * set mac address
     *
     * @param adr address to update
     */
    public static void setMac(addrMac adr) {
        adr.fromString("011b:1900:0000");
    }

    /**
     * set ip address
     *
     * @param ipv4 set for ipv4
     * @param adr address to update
     */
    public static void setIP(boolean ipv4, addrIP adr) {
        if (ipv4) {
            adr.fromString("224.0.1.129");
        } else {
            adr.fromString("ff02::181");
        }
    }

    /**
     * dump packet
     *
     * @return contents
     */
    public String dump() {
        return "dom=" + domain + " clk=" + clock + " prt=" + port + " seq=" + sequence + " ofs=" + offset;
    }

    /**
     * generate sync message
     *
     * @param pck packet to update
     */
    public void createSync(packHolder pck) {
        createPack(pck, 0, 0x200, 0);
        pck.UDPsrc = portS;
        pck.UDPtrg = portS;
    }

    /**
     * generate follow message
     *
     * @param pck packet to update
     */
    public void createFollow(packHolder pck) {
        createPack(pck, 8, 0x000, 2);
        pck.UDPsrc = portF;
        pck.UDPtrg = portF;
    }

    /**
     * generate layer2 header
     *
     * @param pck packet to update
     */
    public static void genHead(packHolder pck) {
        pck.msbPutW(0, ethtyp); // ethertype
        pck.putSkip(2);
        pck.merge2beg();
    }

    private void createPack(packHolder pck, int flg1, int flg2, int ctrl) {
        pck.putByte(0, flg1); // flags
        pck.putByte(1, 2); // version
        pck.msbPutW(2, size); // length
        pck.putByte(4, domain); // subdomain
        pck.putByte(5, 0); // reserved
        pck.msbPutW(6, flg2); // flags
        pck.msbPutQ(8, 0); // correction
        pck.msbPutD(16, 0); // reserved
        pck.msbPutQ(20, clock); // clock id
        pck.msbPutW(28, port); // port id
        pck.msbPutW(30, sequence); // sequence
        long tim = bits.getTime() + offset;
        pck.msbPutQ(32, (int) (tim / 1000)); // seconds
        pck.putByte(32, ctrl); // control
        pck.putByte(33, 0); // message period
        pck.msbPutD(40, (int) ((tim % 1000) * 1000)); // nanoseconds
        pck.putSkip(size);
        pck.merge2beg();
    }

    /**
     * parse packet
     *
     * @param pck packet to read
     * @return false on success, true on error
     */
    public boolean parsePacket(packHolder pck) {
        long tim = bits.getTime();
        if (pck.dataSize() < size) {
            return true;
        }
        if (pck.getByte(1) != 2) { // version
            return true;
        }
        if (pck.msbGetW(2) < size) { // length
            return true;
        }
        switch (pck.getByte(0) & 0xf) { // type
            case 0: // sync
                break;
            case 8: // follow
                break;
            default:
                return true;
        }
        domain = pck.getByte(4); // subdomain
        sequence = pck.msbGetW(30); // sequence
        clock = pck.msbGetQ(20); // clock
        port = pck.msbGetW(28); // sequence
        offset = (pck.msbGetQ(32) & 0xffffffffffffl) * 1000; // seconds
        offset += pck.msbGetD(40) / 1000; // nanoseconds
        offset -= tim;
        return false;
    }

}
