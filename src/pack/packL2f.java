package pack;

import cry.cryHashFcs16;
import util.bits;

/**
 * layer two forwarding protocol (rfc2341) packet
 *
 * @author matecsaba
 */
public class packL2f {

    /**
     * port number
     */
    public static final int port = 1701;

    /**
     * management frame
     */
    public static final int prtMgmt = 1;

    /**
     * ppp frame
     */
    public static final int prtPpp = 2;

    /**
     * slip frame
     */
    public static final int prtSlip = 3;

    /**
     * configuration
     */
    public static final int typConf = 1;

    /**
     * conf name
     */
    public static final int typConfName = 2;

    /**
     * conf challenge
     */
    public static final int typConfChal = 3;

    /**
     * conf clid
     */
    public static final int typConfClid = 4;

    /**
     * open
     */
    public static final int typOpen = 2;

    /**
     * open name
     */
    public static final int typOpenName = 1;

    /**
     * open challenge
     */
    public static final int typOpenChal = 2;

    /**
     * open response
     */
    public static final int typOpenResp = 3;

    /**
     * open lcp confack accepted from client
     */
    public static final int typOpenLcp1 = 4;

    /**
     * open lcp confack sent to client
     */
    public static final int typOpenLcp2 = 5;

    /**
     * open authentication used
     */
    public static final int typOpenAuth = 6;

    /**
     * open auth id
     */
    public static final int typOpenAuid = 7;

    /**
     * open first lcp from client
     */
    public static final int typOpenLcp0 = 8;

    /**
     * close
     */
    public static final int typClose = 3;

    /**
     * close reason
     */
    public static final int typCloseWhy = 1;

    /**
     * close string
     */
    public static final int typCloseStr = 2;

    /**
     * echo request
     */
    public static final int typEchoReq = 4;

    /**
     * echo response
     */
    public static final int typEchoRes = 5;

    /**
     * protocol
     */
    public int proto = -1;

    /**
     * sequence
     */
    public int seq = -1;

    /**
     * multiplex id
     */
    public int multi;

    /**
     * client id
     */
    public int client;

    /**
     * checksum
     */
    public boolean chksum;

    /**
     * key
     */
    public int key;

    /**
     * type
     */
    public int type = -1;

    /**
     * name
     */
    public String valName;

    /**
     * client id
     */
    public int valClid = -1;

    /**
     * auth type
     */
    public int valAuth = -1;

    /**
     * auth id
     */
    public int valAuid = -1;

    /**
     * challenge
     */
    public byte[] valChal;

    /**
     * response
     */
    public byte[] valResp;

    /**
     * convert type to string
     *
     * @param i type
     * @return string
     */
    public static String type2string(int i) {
        switch (i) {
            case typConf:
                return "conf";
            case typOpen:
                return "open";
            case typClose:
                return "close";
            case typEchoReq:
                return "echo-req";
            case typEchoRes:
                return "echo-res";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * parse header
     *
     * @param pck packet to use
     * @return false on success, true on error
     */
    public boolean parseHeader(packHolder pck) {
        int flags = pck.msbGetW(0);
        if ((flags & 0x0008) != 0) {
            int siz = pck.dataSize();
            if (siz < 2) {
                return true;
            }
            cryHashFcs16 sum = new cryHashFcs16();
            sum.init();
            pck.hashData(sum, 0, siz - 2);
            byte[] cb = sum.finish();
            byte[] cg = new byte[cb.length];
            pck.getCopy(cg, 0, siz - cg.length, cg.length);
            if (bits.byteComp(cg, 0, cb, 0, cg.length) != 0) {
                return true;
            }
            pck.setDataSize(siz - cg.length);
            chksum = true;
        }
        proto = pck.getByte(2);
        seq = pck.getByte(3);
        pck.getSkip(4);
        if ((flags & 0x7) != 1) {
            return true;
        }
        if ((flags & 0x1000) == 0) {
            seq = -1;
        }
        multi = pck.msbGetW(0);
        client = pck.msbGetW(2);
        int ofs = pck.msbGetW(4) - 4;
        if (ofs < 7) {
            return true;
        }
        pck.setDataSize(ofs);
        pck.getSkip(6);
        if ((flags & 0x8000) != 0) {
            ofs = pck.msbGetW(0);
            pck.getSkip(2);
        } else {
            ofs = 0;
        }
        if ((flags & 0x4000) != 0) {
            key = pck.msbGetD(0);
            pck.getSkip(4);
        }
        pck.putSkip(ofs);
        if (proto != prtMgmt) {
            return false;
        }
        type = pck.getByte(0);
        pck.getSkip(1);
        return false;
    }

    /**
     * create header
     *
     * @param pck packet to use
     */
    public void createHeader(packHolder pck) {
        pck.merge2beg();
        int flags = 0x8001;
        if (proto == prtMgmt) {
            flags |= 0x1000;
            pck.putByte(0, type);
            pck.putSkip(1);
            pck.merge2beg();
        }
        if (key != 0) {
            flags |= 0x4000;
            pck.msbPutD(0, key);
            pck.putSkip(4);
            pck.merge2beg();
        }
        if ((flags & 0x8000) != 0) {
            pck.msbPutW(0, 0);
            pck.putSkip(2);
            pck.merge2beg();
        }
        pck.msbPutW(0, multi);
        pck.msbPutW(2, client);
        pck.msbPutW(4, pck.dataSize() + 10);
        pck.putSkip(6);
        pck.merge2beg();
        if (seq >= 0) {
            flags |= 0x1000;
        }
        if (chksum) {
            flags |= 0x0008;
        }
        pck.msbPutW(0, flags);
        pck.putByte(2, proto);
        pck.putByte(3, seq);
        pck.putSkip(4);
        pck.merge2beg();
        if (!chksum) {
            return;
        }
        cryHashFcs16 sum = new cryHashFcs16();
        sum.init();
        pck.hashData(sum, 0, pck.dataSize());
        byte[] cb = sum.finish();
        pck.putCopy(cb, 0, 0, cb.length);
        pck.putSkip(cb.length);
        pck.merge2end();
    }

    /**
     * calculate key
     *
     * @param buf buffer to use
     * @return calculated key
     */
    public static int calcKey(byte[] buf) {
        int i = bits.msbGetD(buf, 0);
        i ^= bits.msbGetD(buf, 4);
        i ^= bits.msbGetD(buf, 8);
        i ^= bits.msbGetD(buf, 12);
        return i;
    }

    private static byte[] getData(packHolder pck) {
        int len = pck.getByte(0);
        pck.getSkip(1);
        byte[] buf = new byte[len];
        pck.getCopy(buf, 0, 0, len);
        pck.getSkip(len);
        return buf;
    }

    private static void putData(packHolder pck, int typ, byte[] buf) {
        pck.putByte(0, typ);
        pck.putSkip(1);
        if (buf == null) {
            return;
        }
        pck.putByte(0, buf.length);
        pck.putSkip(1);
        pck.putCopy(buf, 0, 0, buf.length);
        pck.putSkip(buf.length);
    }

    /**
     * parse contents
     *
     * @param pck packet to use
     * @return true on error
     */
    public boolean parseConf(packHolder pck) {
        for (;;) {
            if (pck.dataSize() < 1) {
                return false;
            }
            int typ = pck.getByte(0);
            pck.getSkip(1);
            switch (typ) {
                case typConfChal:
                    valChal = getData(pck);
                    break;
                case typConfName:
                    valName = new String(getData(pck));
                    break;
                case typConfClid:
                    valClid = pck.msbGetD(0);
                    pck.getSkip(4);
                    break;
                default:
                    return true;
            }
        }
    }

    /**
     * create contents
     *
     * @param pck packet to use
     * @param host hostname
     * @param chal challenge
     * @param clid client id
     */
    public void createConf(packHolder pck, String host, byte[] chal, int clid) {
        valName = host;
        valChal = chal;
        valClid = clid;
        type = typConf;
        proto = prtMgmt;
        putData(pck, typConfName, host.getBytes());
        if (chal != null) {
            putData(pck, typConfChal, chal);
        }
        pck.putByte(0, typConfClid);
        pck.msbPutD(1, clid);
        pck.putSkip(5);
    }

    /**
     * parse contents
     *
     * @param pck packet to use
     * @return true on error
     */
    public boolean parseOpen(packHolder pck) {
        for (;;) {
            if (pck.dataSize() < 1) {
                return false;
            }
            int typ = pck.getByte(0);
            pck.getSkip(1);
            switch (typ) {
                case typOpenResp:
                    valResp = getData(pck);
                    break;
                case typOpenChal:
                    valChal = getData(pck);
                    break;
                case typOpenAuth:
                    valAuth = pck.getByte(0);
                    pck.getSkip(1);
                    break;
                case typOpenAuid:
                    valAuid = pck.getByte(0);
                    pck.getSkip(1);
                    break;
                case typOpenLcp0:
                case typOpenLcp1:
                case typOpenLcp2:
                    pck.getSkip(pck.msbGetW(0) + 2);
                    break;
                case typOpenName:
                    valName = new String(getData(pck));
                    break;
                default:
                    return true;
            }
        }
    }

    /**
     * create contents
     *
     * @param pck packet to use
     * @param resp response
     */
    public void createOpen(packHolder pck, byte[] resp) {
        valResp = resp;
        type = typOpen;
        proto = prtMgmt;
        if (resp != null) {
            putData(pck, typConfChal, resp);
        }
    }

    /**
     * parse contents
     *
     * @param pck packet to use
     */
    public void parseEcho(packHolder pck) {
        valResp = pck.getCopy();
    }

    /**
     * create contents
     *
     * @param pck packet to use
     * @param dat data
     */
    public void createEchoReq(packHolder pck, byte[] dat) {
        type = typEchoReq;
        proto = prtMgmt;
        valResp = dat;
        pck.putCopy(dat, 0, 0, dat.length);
        pck.putSkip(dat.length);
    }

    /**
     * create contents
     *
     * @param pck packet to use
     * @param dat data
     */
    public void createEchoRes(packHolder pck, byte[] dat) {
        type = typEchoRes;
        proto = prtMgmt;
        valResp = dat;
        pck.putCopy(dat, 0, 0, dat.length);
        pck.putSkip(dat.length);
    }

    /**
     * parse contents
     *
     * @param pck packet to use
     * @return true on error
     */
    public boolean parseClose(packHolder pck) {
        for (;;) {
            if (pck.dataSize() < 1) {
                return false;
            }
            int typ = pck.getByte(0);
            pck.getSkip(1);
            switch (typ) {
                case typCloseWhy:
                    valClid = pck.msbGetD(0);
                    pck.getSkip(4);
                    break;
                case typCloseStr:
                    pck.getSkip(pck.msbGetW(0) + 2);
                    break;
                default:
                    return true;
            }
        }
    }

    /**
     * create contents
     *
     * @param pck packet to use
     * @param cod code
     */
    public void createClose(packHolder pck, int cod) {
        valClid = cod;
        type = typClose;
        proto = prtMgmt;
        pck.putByte(0, typConfClid);
        pck.msbPutD(1, cod);
        pck.putSkip(5);
    }

    /**
     * dump current message
     *
     * @return string
     */
    public String dump() {
        return "proto=" + proto + " type=" + type2string(type) + " seq=" + seq + " client=" + client + " multi=" + multi + " key=" + key + " name=" + valName + " clid=" + valClid + " chal=" + bits.byteDump(valChal, 0, -1) + " resp=" + bits.byteDump(valResp, 0, -1) + " auth=" + valAuth + " auid=" + valAuid;
    }

}
