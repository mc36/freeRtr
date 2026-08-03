package org.freertr.serv;

import org.freertr.addr.addrIP;
import org.freertr.pack.packHolder;

/**
 * upnp packet
 *
 * @author matecsaba
 */
public class servUpnpPck {

    /**
     * create instance
     */
    public servUpnpPck() {
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
     * packet type
     */
    public int typ;

    /**
     * port
     */
    public int port;

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
        port = pck.msbGetW(1); // port
        pck.getSkip(3);
        addr = new addrIP();
        pck.getAddr(addr, 0); // address
        pck.getSkip(addrIP.size);
    }

    /**
     * create one packet
     *
     * @param pck packet to update
     */
    public void createPacket(packHolder pck) {
        pck.putByte(0, typ); // type
        pck.msbPutW(1, port); // port
        pck.putSkip(3);
        pck.putAddr(0, addr); // address
        pck.putSkip(addrIP.size);
        pck.merge2beg();
    }

}
