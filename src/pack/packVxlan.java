package pack;

/**
 * vxlan (rfc7348) packet header
 *
 * @author matecsaba
 */
public class packVxlan {

    /**
     * udp port number
     */
    public static final int port = 4789;

    /**
     * size of header
     */
    public static final int size = 8;

    /**
     * version mask
     */
    public static final int flgVER = 0x30;

    /**
     * valid vmi
     */
    public static final int flgVNi = 0x08;

    /**
     * valid np
     */
    public static final int flgPRT = 0x04;

    /**
     * valid bum
     */
    public static final int flgBUM = 0x02;

    /**
     * valid oam
     */
    public static final int flgOAM = 0x01;

    /**
     * instance id
     */
    public int instance;

    /**
     * protocol id
     */
    public int protocol;

    /**
     * parse header
     *
     * @param pck packet to parse
     * @return false on success, true on error
     */
    public boolean parseHeader(packHolder pck) {
        if (pck.dataSize() < size) {
            return true;
        }
        int flg = pck.getByte(0);
        if ((flg & flgVNi) == 0) {
            return true;
        }
        if ((flg & flgPRT) != 0) {
            protocol = pck.getByte(3);
        }
        instance = pck.msbGetD(4) >>> 8;
        pck.getSkip(size);
        return false;
    }

    /**
     * create header
     *
     * @param pck packet to update
     */
    public void createHeader(packHolder pck) {
        pck.msbPutD(0, (flgVNi << 24) | protocol);
        pck.msbPutD(4, instance << 8);
        pck.putSkip(size);
    }

}
