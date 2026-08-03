package org.freertr.pack;

/**
 * vxlan (rfc7348) packet header
 *
 * @author matecsaba
 */
public class packVxlan {

    /**
     * create instance
     */
    public packVxlan() {
    }

    /**
     * udp port number
     */
    public final static int port = 4789;

    /**
     * size of header
     */
    public final static int size = 8;

    /**
     * version mask
     */
    public final static int flgVER = 0x30;

    /**
     * valid vmi
     */
    public final static int flgVNi = 0x08;

    /**
     * valid np
     */
    public final static int flgPRT = 0x04;

    /**
     * valid bum
     */
    public final static int flgBUM = 0x02;

    /**
     * valid oam
     */
    public final static int flgOAM = 0x01;

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
