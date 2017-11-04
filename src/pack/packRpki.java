package pack;

import addr.addrIPv4;
import addr.addrIPv6;
import addr.addrPrefix;
import pipe.pipeSide;

/**
 * resource public key infrastructure (rfc6810) packet
 *
 * @author matecsaba
 */
public class packRpki {

    /**
     * port number
     */
    public static final int portNum = 323;

    /**
     * size of header
     */
    public static final int size = 8;

    /**
     * serial notify
     */
    public static final int msgSerialNotify = 0;

    /**
     * serial query
     */
    public static final int msgSerialQuery = 1;

    /**
     * reset query
     */
    public static final int msgResetQuery = 2;

    /**
     * cache response
     */
    public static final int msgCacheReply = 3;

    /**
     * ipv4 prefix
     */
    public static final int msgIpv4addr = 4;

    /**
     * ipv6 prefix
     */
    public static final int msgIpv6addr = 6;

    /**
     * end of data
     */
    public static final int msgEndData = 7;

    /**
     * cache reset
     */
    public static final int msgCacheReset = 8;

    /**
     * error report
     */
    public static final int msgErrorReport = 10;

    /**
     * type
     */
    public int typ;

    /**
     * session id
     */
    public int sess;

    /**
     * serial number
     */
    public int serial;

    /**
     * withdraw
     */
    public boolean withdraw;

    /**
     * prefix4
     */
    public addrPrefix<addrIPv4> pref4;

    /**
     * prefix6
     */
    public addrPrefix<addrIPv6> pref6;

    /**
     * max length
     */
    public int max;

    /**
     * as number
     */
    public int as;

    /**
     * dump packet
     *
     * @return string
     */
    public String dump() {
        return "typ=" + type2string(typ) + " sess=" + sess + " serial=" + serial + " wd=" + withdraw + " pref=" + pref4 + "/" + pref6 + " max=" + max + " as=" + as;
    }

    /**
     * convert type to string
     *
     * @param i type
     * @return string
     */
    public static final String type2string(int i) {
        switch (i) {
            case msgSerialNotify:
                return "serialNotify";
            case msgSerialQuery:
                return "serialQuery";
            case msgResetQuery:
                return "resetQuery";
            case msgCacheReply:
                return "cacheReply";
            case msgIpv4addr:
                return "ipv4prefix";
            case msgIpv6addr:
                return "ipv6prefix";
            case msgEndData:
                return "end";
            case msgCacheReset:
                return "cacheReset";
            case msgErrorReport:
                return "error";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * receive packet
     *
     * @param conn pipe to use
     * @return false on success, true on error
     */
    public boolean recvPack(pipeSide conn) {
        packHolder pck = new packHolder(true, true);
        if (pck.pipeRecv(conn, 0, size, 144) != size) {
            return true;
        }
        if (pck.getByte(0) != 0) { // version
            return true;
        }
        typ = pck.getByte(1); // type
        sess = pck.msbGetW(2); // session id
        int len = pck.msbGetD(4); // length
        if (len < size) {
            return true;
        }
        len -= size;
        pck.clear();
        if (len > 0) {
            if (pck.pipeRecv(conn, 0, len, 144) != len) {
                return true;
            }
        }
        switch (typ) {
            case msgSerialNotify:
                serial = pck.msbGetD(0); // serial
                break;
            case msgSerialQuery:
                serial = pck.msbGetD(0); // serial
                break;
            case msgResetQuery:
                break;
            case msgCacheReply:
                break;
            case msgIpv4addr:
                withdraw = (pck.getByte(0) & 1) == 0; // flags
                max = pck.getByte(2); // max
                addrIPv4 adr4 = new addrIPv4();
                pck.getAddr(adr4, 4); // address
                pref4 = new addrPrefix<addrIPv4>(adr4, pck.getByte(1));
                as = pck.msbGetD(8); // as
                break;
            case msgIpv6addr:
                withdraw = (pck.getByte(0) & 1) == 0; // flags
                max = pck.getByte(2); // max
                addrIPv6 adr6 = new addrIPv6();
                pck.getAddr(adr6, 4); // address
                pref6 = new addrPrefix<addrIPv6>(adr6, pck.getByte(1));
                as = pck.msbGetD(20); // as
                break;
            case msgEndData:
                serial = pck.msbGetD(0); // serial
                break;
            case msgCacheReset:
                break;
            case msgErrorReport:
                break;
            default:
                return true;
        }
        return false;
    }

    /**
     * send packet
     *
     * @param conn pipe to use
     */
    public void sendPack(pipeSide conn) {
        packHolder pck = new packHolder(true, true);
        switch (typ) {
            case msgSerialNotify:
                pck.msbPutD(0, serial); // serial
                pck.putSkip(4);
                break;
            case msgSerialQuery:
                pck.msbPutD(0, serial); // serial
                pck.putSkip(4);
                break;
            case msgResetQuery:
                break;
            case msgCacheReply:
                break;
            case msgIpv4addr:
                if (withdraw) {
                    pck.putByte(0, 0);
                } else {
                    pck.putByte(0, 1);
                }
                pck.putByte(1, pref4.maskLen); // prefix length
                pck.putByte(2, max); // max
                pck.putByte(3, 0); // reserved
                pck.putAddr(4, pref4.network); // address
                pck.msbPutD(8, as); // as
                pck.putSkip(12);
                break;
            case msgIpv6addr:
                if (withdraw) {
                    pck.putByte(0, 0);
                } else {
                    pck.putByte(0, 1);
                }
                pck.putByte(1, pref6.maskLen); // prefix length
                pck.putByte(2, max); // max
                pck.putByte(3, 0); // reserved
                pck.putAddr(4, pref6.network); // address
                pck.msbPutD(20, as); // as
                pck.putSkip(24);
                break;
            case msgEndData:
                pck.msbPutD(0, serial); // serial
                pck.putSkip(4);
                break;
            case msgCacheReset:
                break;
            case msgErrorReport:
                break;
            default:
                return;
        }
        pck.merge2beg();
        pck.putByte(0, 0); // version
        pck.putByte(1, typ); // type
        pck.msbPutW(2, sess); // session id
        pck.msbPutD(4, pck.dataSize() + size); // length
        pck.putSkip(size);
        pck.merge2beg();
        pck.pipeSend(conn, 0, pck.dataSize(), 2);
    }

}
