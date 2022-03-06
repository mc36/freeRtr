package net.freertr.pack;

import net.freertr.pipe.pipeSide;
import net.freertr.util.typLenVal;

/**
 * pulse protocol
 *
 * @author matecsaba
 */
public class packPulse {

    /**
     * size of header
     */
    public final static int size = 16;

    /**
     * juniper
     */
    public final static int vendJunos = 0xa4c;

    /**
     * juniper2
     */
    public final static int vendJunos2 = 0x583;

    /**
     * tcg
     */
    public final static int vendTcg = 0x5597;

    /**
     * authentication beginning
     */
    public final static int authBeg = (vendJunos << 8) + 1;
    
    private final pipeSide pipe;

    /**
     * vendor
     */
    public int vendor;

    /**
     * type
     */
    public int type;

    /**
     * sequence number
     */
    public int sequence;

    /**
     * create new instance
     *
     * @param conn connection to use
     */
    public packPulse(pipeSide conn) {
        pipe = conn;
    }

    /**
     * send one packet
     *
     * @param pck packet to end
     */
    public void sendPack(packHolder pck) {
        pck.msbPutD(0, vendor);
        pck.msbPutD(4, type);
        pck.msbPutD(8, size + pck.dataSize());
        pck.msbPutD(12, sequence);
        pck.putSkip(size);
        pck.merge2beg();
        pck.pipeSend(pipe, 0, pck.dataSize(), 3);
        sequence++;
    }

    /**
     * receive one packet
     *
     * @param pck packet to receive
     * @return false on success, true on error
     */
    public boolean recvPack(packHolder pck) {
        pck.clear();
        if (pck.pipeRecv(pipe, 0, size, 144) != size) {
            return true;
        }
        int len = pck.msbGetD(8) - size;
        vendor = pck.msbGetD(0);
        type = pck.msbGetD(4);
        sequence = pck.msbGetD(12);
        pck.getSkip(size);
        if (len < 1) {
            return false;
        }
        if (pck.pipeRecv(pipe, 0, len, 144) != len) {
            return true;
        }
        return false;
    }

    /**
     * dump one packet
     *
     * @return contents
     */
    public String dumpPack() {
        return "vnd=" + vendor + " typ=" + type + " seq=" + sequence;
    }

    /**
     * send one authentication
     *
     * @param pck packet to end
     */
    public void sendAuth(packHolder pck) {
        pck.msbPutD(0, authBeg);
        pck.putSkip(4);
        pck.merge2beg();
        sendPack(pck);
    }

    /**
     * receive one authentication
     *
     * @param pck packet to receive
     * @return false on success, true on error
     */
    public boolean recvAuth(packHolder pck) {
        if (recvPack(pck)) {
            return true;
        }
        if (pck.msbGetD(0) != authBeg) {
            return true;
        }
        pck.getSkip(4);
        return false;
    }

    /**
     * put eap header
     *
     * @param pck packet to receive
     * @param cod code
     * @param id ident
     * @param typ type
     * @param sub subtype
     */
    public static void putEap(packHolder pck, int cod, int id, int typ, int sub) {
        pck.putByte(0, cod);
        pck.putByte(1, id);
        pck.putSkip(4);
        if (typ == 0xfe) {
            pck.msbPutW(0, 0xfe00);
            pck.msbPutW(2, vendJunos);
            pck.msbPutD(4, sub);
            pck.putSkip(8);
        } else {
            pck.putByte(0, typ);
            pck.putSkip(1);
        }
        int siz = pck.headSize();
        pck.putSkip(-siz);
        pck.msbPutW(2, pck.dataSize() + siz);
        pck.putSkip(siz);
        pck.merge2beg();
    }

    /**
     * get eap header
     * @param pck packet to receive
     */
    public static void getEap(packHolder pck) {
        pck.getSkip(4);
        int typ = pck.getByte(0);
        if (typ == 0xfe) {
            pck.getSkip(8);
        } else {
            pck.getSkip(1);
        }
    }

    /**
     * put avp header
     *
     * @param pck packet to receive
     * @param typ type
     * @param buf bytes
     */
    public static void putAvp(packHolder pck, int typ, byte[]buf) {
        pck.msbPutD(0, typ);
        pck.msbPutW(4, 0x8000);
        pck.msbPutW(6, buf.length + 12);
        pck.msbPutD(8, vendJunos2);
        pck.putSkip(12);
        pck.putCopy(buf, 0, 0, buf.length);
        pck.putSkip(buf.length);
        for (;;) {
            if ((pck.headSize() & 3) == 0) {
                break;
            }
            pck.putByte(0, 0);
            pck.putSkip(1);
        }
        pck.merge2end();
    }

    /**
     * get avp header
     *
     * @param pck packet to receive
     * @return tlv, null if error
     */
    public static typLenVal getAvp(packHolder pck) {
        typLenVal tlv = new typLenVal(0, 16, 16, 16, 1, 0, 4, 1, 0, 1024, true);
        if (pck.dataSize() < 12) {
            return null;
        }
        tlv.valTyp = pck.msbGetD(0);
        tlv.valSiz = pck.msbGetW(6) - 12;
        pck.getSkip(12);
        if (tlv.valSiz < 0) {
            return null;
        }
        if (pck.dataSize() < tlv.valSiz) {
            return null;
        }
        pck.getCopy(tlv.valDat, 0, 0, tlv.valSiz);
        pck.getSkip(tlv.valSiz);
        return tlv;
    }
    
}
