package org.freertr.pack;

/**
 * control and provision of wireless access points (rfc5415) packet
 *
 * @author matecsaba
 */
public class packCapwap {

    /**
     * create instance
     */
    public packCapwap() {
    }

    /**
     * port to use
     */
    public final static int port = 5247;

    /**
     * header size
     */
    public final static int size = 8;

    /**
     * header magic
     */
    public final static int magic = 0x100000;

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
        int i = pck.msbGetD(0); // magic
        if (i != magic) {
            return true;
        }
        i = pck.msbGetD(4); // fragment
        if (i != 0) {
            return true;
        }
        pck.getSkip(size);
        return false;
    }

    /**
     * create header
     *
     * @param pck packet to update
     */
    public void createHeader(packHolder pck) {
        pck.msbPutD(0, magic); // preamble
        pck.msbPutD(4, 0); // fragment
        pck.putSkip(size);
        pck.merge2beg();
    }

}
