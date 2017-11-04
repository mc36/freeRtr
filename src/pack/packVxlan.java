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
     * valid vmi
     */
    public static final int flgVNi = 0x08;

    /**
     * instance id
     */
    public int instance;

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
        if ((pck.getByte(0) & flgVNi) == 0) {
            return true;
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
        pck.msbPutD(0, flgVNi << 24);
        pck.msbPutD(4, instance << 8);
        pck.putSkip(size);
    }

}
