package pack;

import cry.cryHashCrc32;
import cry.cryHashGeneric;
import pipe.pipeSide;
import util.bits;

/**
 * nagios remote plugin packet
 *
 * @author matecsaba
 */
public class packNrpe {

    /**
     * port number
     */
    public static final int portNum = 5666;

    /**
     * header size
     */
    public static final int size = 10;

    /**
     * v2 buf size
     */
    public static final int size2 = 1024;

    /**
     * v2 rnd size
     */
    public static final int size3 = 2;

    /**
     * v3 hdr size
     */
    public static final int size4 = 6;

    /**
     * v3 size diff
     */
    public static final int size5 = 3;

    /**
     * query
     */
    public static final int tyReq = 1;

    /**
     * response
     */
    public static final int tyRep = 2;

    /**
     * unknown
     */
    public static final int coUnk = 3;

    /**
     * critical
     */
    public static final int coCri = 2;

    /**
     * warning
     */
    public static final int coWar = 1;

    /**
     * ok
     */
    public static final int coOk = 0;

    /**
     * version
     */
    public int ver;

    /**
     * type
     */
    public int typ;

    /**
     * code
     */
    public int cod;

    /**
     * buffer
     */
    public String str;

    /**
     * line separator
     */
    public final byte[] sep;

    /**
     * create one instance
     */
    public packNrpe() {
        sep = pipeSide.getEnding(pipeSide.modTyp.modeLF);
    }

    /**
     * dump packet
     *
     * @return string
     */
    public String dump() {
        return "ver=" + ver + " typ=" + typ + " cod=" + cod + " buf=" + str;
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
        ver = pck.msbGetW(0);
        typ = pck.msbGetW(2);
        int crc = pck.msbGetD(4);
        cod = pck.msbGetW(8);
        byte[] buf = pck.getCopy();
        bits.byteFill(buf, 4, 4, 0);
        cryHashGeneric hsh = new cryHashCrc32();
        hsh.init();
        hsh.update(buf);
        switch (ver) {
            case 2:
                pck.clear();
                if (pck.pipeRecv(conn, 0, size2, 144) != size2) {
                    return true;
                }
                buf = pck.getCopy();
                hsh.update(buf);
                pck.clear();
                if (pck.pipeRecv(conn, 0, size3, 144) != size3) {
                    return true;
                }
                hsh.update(pck.getCopy());
                break;
            case 3:
                pck.clear();
                if (pck.pipeRecv(conn, 0, size4, 144) != size4) {
                    return true;
                }
                hsh.update(pck.getCopy());
                int len = pck.msbGetD(2) + size5;
                pck.clear();
                if (pck.pipeRecv(conn, 0, len, 144) != len) {
                    return true;
                }
                buf = pck.getCopy();
                hsh.update(buf);
                break;
            default:
                return true;
        }
        if (bits.msbGetD(hsh.finish(), 0) != crc) {
            return true;
        }
        pck.clear();
        pck.putCopy(buf, 0, 0, buf.length);
        pck.putSkip(buf.length);
        pck.merge2beg();
        str = pck.getAsciiZ(0, buf.length, 0);
        return false;
    }

    /**
     * send packet
     *
     * @param conn pipe to use
     */
    public void sendPack(pipeSide conn) {
        packHolder pck = new packHolder(true, true);
        switch (ver) {
            case 2:
                pck.putAsciiZ(0, size2, str, 0);
                pck.putSkip(size2);
                pck.msbPutD(0, bits.randomD());
                pck.putSkip(size3);
                pck.merge2beg();
                break;
            case 3:
                int len = str.length() + 4;
                pck.msbPutW(0, 0); // pad
                pck.msbPutD(2, len - size5);
                pck.putSkip(size4);
                pck.putAsciiZ(0, len, str, 0);
                pck.putSkip(len);
                pck.merge2beg();
                break;
            default:
                return;
        }
        byte[] buf = new byte[size];
        bits.msbPutW(buf, 0, ver);
        bits.msbPutW(buf, 2, typ);
        bits.msbPutD(buf, 4, 0); // crc
        bits.msbPutW(buf, 8, cod);
        cryHashGeneric hsh = new cryHashCrc32();
        hsh.init();
        hsh.update(buf);
        hsh.update(pck.getCopy());
        pck.putCopy(buf, 0, 0, size);
        pck.putCopy(hsh.finish(), 0, 4, 4); // crc
        pck.putSkip(size);
        pck.merge2beg();
        pck.pipeSend(conn, 0, pck.dataSize(), 2);
    }

}
