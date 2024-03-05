package org.freertr.pack;

/**
 * lightweight access point protocol (rfc5412) packet
 *
 * @author matecsaba
 */
public class packLwapp {

    /**
     * create instance
     */
    public packLwapp() {
    }

    /**
     * port to use
     */
    public final static int port = 12222;

    /**
     * header size
     */
    public final static int size = 6;

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
        int i = pck.msbGetW(0); // flags
        if (i != 0) {
            return true;
        }
        i = pck.msbGetW(2); // length
        pck.getSkip(size);
        if (pck.dataSize() < i) {
            return true;
        }
        pck.setDataSize(i);
        return false;
    }

    /**
     * create header
     *
     * @param pck packet to update
     */
    public void createHeader(packHolder pck) {
        pck.msbPutW(0, 0); // flags
        pck.msbPutW(2, pck.dataSize()); // length
        pck.msbPutW(4, 0); // status
        pck.putSkip(size);
        pck.merge2beg();
    }

}
