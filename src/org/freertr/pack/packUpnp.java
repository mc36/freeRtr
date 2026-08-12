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
    public final static int size = 21;

    /**
     * packet type
     */
    public int typ;

    /**
     * client port
     */
    public int prtC;

    /**
     * server port
     */
    public int prtS;

    /**
     * address
     */
    public addrIP addr = new addrIP();

    /**
     * parse one packet
     *
     * @param pck packet to update
     */
    public void parsePacket(packHolder pck) {
        typ = pck.getByte(0); // type
        prtC = pck.msbGetW(1); // client port
        prtS = pck.msbGetW(3); // server port
        pck.getAddr(addr, 5); // address
        pck.getSkip(size);
    }

    /**
     * create one packet
     *
     * @param pck packet to update
     */
    public void createPacket(packHolder pck) {
        pck.putByte(0, typ); // type
        pck.msbPutW(1, prtC); // client port
        pck.msbPutW(3, prtS); // server port
        pck.putAddr(5, addr); // address
        pck.putSkip(size);
        pck.merge2beg();
    }

}
