package org.freertr.pack;

import org.freertr.addr.addrIP;

/**
 * upnp packet
 *
 * @author matecsaba
 */
public class packUpnp {

    /**
     * create instance
     */
    public packUpnp() {
    }

    /**
     * port number
     */
    public final static int portNum = 1900;

    /**
     * keepalive
     */
    public final static int typKeep = 1;

    /**
     * data
     */
    public final static int typData = 2;

    /**
     * header
     */
    public final static int size = 37;

    /**
     * packet type
     */
    public int typ;

    /**
     * source port
     */
    public int prtS;

    /**
     * target port
     */
    public int prtT;

    /**
     * source address
     */
    public addrIP adrS = new addrIP();

    /**
     * target address
     */
    public addrIP adrT = new addrIP();

    /**
     * parse one packet
     *
     * @param pck packet to update
     */
    public void parsePacket(packHolder pck) {
        typ = pck.getByte(0); // type
        prtS = pck.msbGetW(1); // source port
        prtT = pck.msbGetW(3); // target port
        pck.getAddr(adrS, 5); // source address
        pck.getAddr(adrT, 21); // target address
        pck.getSkip(size);
    }

    /**
     * create one packet
     *
     * @param pck packet to update
     */
    public void createPacket(packHolder pck) {
        pck.putByte(0, typ); // type
        pck.msbPutW(1, prtS); // source port
        pck.msbPutW(3, prtT); // target port
        pck.putAddr(5, adrS); // source address
        pck.putAddr(5, adrT); // target address
        pck.putSkip(size);
        pck.merge2beg();
    }

}
