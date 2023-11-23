package net.freertr.ifc;

import net.freertr.pack.packHolder;

/**
 * trill (rfc6325) base header
 *
 * @author matecsaba
 */
public class ifcTrillBas {

    /**
     * ethertype
     */
    public final static int type = 0x22f3;

    /**
     * size
     */
    public final static int size = 8;

    /**
     * parse header
     *
     * @param pck packet to parse
     * @return true on error, false on success
     */
    public boolean parseHeader(packHolder pck) {
        if (pck.msbGetW(0) != type) {
            return true;
        }
        if ((pck.getByte(2) & 0xc0) != 0) {
            return true;
        }
        pck.NSHttl = pck.getByte(3) & 0x1f;
        pck.NSHsi = pck.msbGetW(4);
        pck.NSHsp = pck.msbGetW(6);
        pck.getSkip(size);
        return false;
    }

    /**
     * put header
     *
     * @param pck packet to update
     */
    public void putHeader(packHolder pck) {
        pck.merge2beg();
        pck.msbPutW(0, type);
        pck.msbPutW(2, pck.NSHttl & 0x1f);
        pck.msbPutW(4, pck.NSHsi);
        pck.msbPutW(6, pck.NSHsp);
        pck.putSkip(size);
        pck.merge2beg();
    }

}
