package pack;

import addr.addrIP;

/**
 * our proprietary upnp forwarder packets
 *
 * @author matecsaba
 */
public class packUpnpFwd {

    /**
     * port number
     */
    public static final int portNum = 1900;

    /**
     * keepalive
     */
    public static final int typKeep = 1;

    /**
     * data
     */
    public static final int typData = 2;

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
