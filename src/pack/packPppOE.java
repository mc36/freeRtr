package pack;

import util.typLenVal;

/**
 * ppp over ethernet (rfc2516) client protocol handler
 *
 * @author matecsaba
 */
public class packPppOE {

    /**
     * code number
     */
    public int cod;

    /**
     * session id
     */
    public int ses;

    /**
     * ethertype of control packets
     */
    public final static int typeCtr = 0x8863;

    /**
     * ethertype of data packets
     */
    public final static int typeDat = 0x8864;

    /**
     * size of header
     */
    public final static int size = 8;

    /**
     * type length value
     */
    public final static typLenVal tlv = new typLenVal(0, 16, 16, 16, 1, 0, 4, 1, 0, 512, true);

    /**
     * data packet
     */
    public static final int codeData = 0x00;

    /**
     * Active Discovery Initiation
     */
    public static final int codePadI = 0x09;

    /**
     * Active Discovery Offer
     */
    public static final int codePadO = 0x07;

    /**
     * Active Discovery Request
     */
    public static final int codePadR = 0x19;

    /**
     * Active Discovery Session-confirmation
     */
    public static final int codePadS = 0x65;

    /**
     * Active Discovery Terminate
     */
    public static final int codePadT = 0xa7;

    /**
     * end of list
     */
    public static final int typeEol = 0x0000;

    /**
     * service name
     */
    public static final int typeSrvNam = 0x0101;

    /**
     * ac name
     */
    public static final int typeACnam = 0x0102;

    /**
     * host uniq
     */
    public static final int typeHstUnq = 0x0103;

    /**
     * ac cookie
     */
    public static final int typeACcok = 0x0104;

    /**
     * vendor specific
     */
    public static final int typeVndSpc = 0x0105;

    /**
     * relay session id
     */
    public static final int typeRlySes = 0x0110;

    /**
     * service name error
     */
    public static final int typeSrvNm = 0x0201;

    /**
     * ac system error
     */
    public static final int typeSysErr = 0x0202;

    /**
     * generic error
     */
    public static final int typeGenErr = 0x0203;

    /**
     * convert code to string
     *
     * @param i code to convert
     * @return string representation
     */
    public static String code2string(int i) {
        switch (i) {
            case codeData:
                return "data";
            case codePadI:
                return "padi";
            case codePadO:
                return "pado";
            case codePadR:
                return "padr";
            case codePadS:
                return "pads";
            case codePadT:
                return "padt";
        }
        return "unknown:" + i;
    }

    /**
     * convert type to string
     *
     * @param i type to convert
     * @return string representation
     */
    public static String type2string(int i) {
        switch (i) {
            case typeEol:
                return "end of list";
            case typeSrvNam:
                return "service name";
            case typeACnam:
                return "ac name";
            case typeHstUnq:
                return "host uniq";
            case typeACcok:
                return "ac cookie";
            case typeVndSpc:
                return "vendor specific";
            case typeRlySes:
                return "relay session id";
            case typeSrvNm:
                return "service name error";
            case typeSysErr:
                return "ac system error";
            case typeGenErr:
                return "generic error";
        }
        return "unknown:" + i;
    }

    /**
     * update one packet
     *
     * @param pck packet to update
     * @param c code
     * @param s session
     */
    public static void updateHeader(packHolder pck, int c, int s) {
        packPppOE p = new packPppOE();
        p.cod = c;
        p.ses = s;
        p.updateHeader(pck);
    }

    /**
     * update one packet
     *
     * @param pck packet to update
     */
    public void updateHeader(packHolder pck) {
        pck.merge2beg();
        if (cod == codeData) {
            pck.msbPutW(0, typeDat);
        } else {
            pck.msbPutW(0, typeCtr);
        }
        pck.putByte(2, 0x11);
        pck.putByte(3, cod);
        pck.msbPutW(4, ses);
        pck.msbPutW(6, pck.dataSize());
        pck.putSkip(size);
        pck.merge2beg();
    }

    /**
     * parse header
     *
     * @param pck packet to read
     * @return false on success, true on error
     */
    public boolean parseHeader(packHolder pck) {
        int typ = pck.msbGetW(0);
        if (pck.getByte(2) != 0x11) { // version
            return true;
        }
        cod = pck.getByte(3);
        ses = pck.msbGetW(4);
        int len = pck.msbGetW(6);
        if (cod == codeData) {
            if (typ != typeDat) {
                return true;
            }
        } else {
            if (typ != typeCtr) {
                return true;
            }
        }
        pck.getSkip(packPppOE.size);
        if (len > pck.dataSize()) {
            return true;
        }
        pck.setDataSize(len);
        return false;
    }

}
