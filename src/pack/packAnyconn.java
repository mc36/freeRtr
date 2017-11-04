package pack;

import pipe.pipeSide;

/**
 * anyconnect protocol
 *
 * @author matecsaba
 */
public class packAnyconn {

    /**
     * size of header
     */
    public final static int size = 8;

    /**
     * header magic
     */
    public final static int magic = 0x53544601;

    /**
     * data
     */
    public static final int typData = 0;

    /**
     * dpd request
     */
    public static final int typDpdReq = 3;

    /**
     * dpd response
     */
    public static final int typDpdRep = 4;

    /**
     * disconnect
     */
    public static final int typDisc = 5;

    /**
     * keepalive
     */
    public static final int typKeep = 7;

    /**
     * compressed data
     */
    public static final int typComp = 8;

    /**
     * terminate
     */
    public static final int typTerm = 9;

    /**
     * message type
     */
    public int msgTyp;

    private final pipeSide pipe;

    /**
     * convert type to string
     *
     * @param i type
     * @return string
     */
    public static String type2string(int i) {
        switch (i) {
            case typData:
                return "data";
            case typDpdReq:
                return "dpdReq";
            case typDpdRep:
                return "dpdRep";
            case typDisc:
                return "disconnect";
            case typKeep:
                return "keepalive";
            case typComp:
                return "compressed";
            case typTerm:
                return "terminate";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * create new instance
     *
     * @param conn connection to use
     */
    public packAnyconn(pipeSide conn) {
        pipe = conn;
    }

    /**
     * dump this packet
     *
     * @return packet dump
     */
    public String dump() {
        return "type=" + type2string(msgTyp);
    }

    /**
     * send one packet
     *
     * @param pck packet to end
     */
    public void sendPack(packHolder pck) {
        pck.msbPutD(0, magic);
        pck.msbPutW(4, pck.dataSize());
        pck.lsbPutW(6, msgTyp);
        pck.putSkip(size);
        pck.merge2beg();
        pck.pipeSend(pipe, 0, pck.dataSize(), 3);
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
        if (pck.msbGetD(0) != magic) {
            return true;
        }
        int len = pck.msbGetW(4);
        msgTyp = pck.lsbGetW(6);
        pck.getSkip(size);
        if (len < 1) {
            return false;
        }
        if (pck.pipeRecv(pipe, 0, len, 144) != len) {
            return true;
        }
        return false;
    }

}
