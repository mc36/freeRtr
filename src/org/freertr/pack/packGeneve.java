package org.freertr.pack;

import org.freertr.ifc.ifcBridge;

/**
 * generic network virtualization encapsulation (rfc8926) packet handler
 *
 * @author matecsaba
 */
public class packGeneve {

    /**
     * create instance
     */
    public packGeneve() {
    }

    /**
     * udp port number
     */
    public final static int port = 6081;

    /**
     * size of header
     */
    public final static int size = 8;

    /**
     * oam frame
     */
    public final static int flgO = 0x80;

    /**
     * critical options
     */
    public final static int flgC = 0x40;

    /**
     * virtual network id
     */
    public int vni;

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
        int hvl = pck.getByte(0); // version + header length
        if ((hvl & 0xc0) != 0) {
            return true;
        }
        int flg = pck.getByte(1); // flags
        if ((flg & flgO) != 0) {
            return true;
        }
        if (pck.msbGetW(2) != ifcBridge.serialType) {
            return true;
        }
        vni = pck.msbGetD(4) >>> 8;
        pck.getSkip(size);
        pck.getSkip((hvl & 0x3f) << 2);
        return false;
    }

    /**
     * create header
     *
     * @param pck packet to update
     */
    public void createHeader(packHolder pck) {
        pck.msbPutW(0, 0); // version + header length + flags
        pck.msbPutW(2, ifcBridge.serialType); // type
        pck.msbPutD(4, vni << 8);
        pck.putSkip(size);
    }

}
