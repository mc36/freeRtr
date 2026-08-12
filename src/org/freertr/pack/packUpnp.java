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
     * get group address
     *
     * @param ip4 true of ipv4, false for ipv6
     * @return group to use
     */
    public static addrIP getGroup(boolean ip4) {
        addrIP grp = new addrIP();
        if (ip4) {
            grp.fromString("239.255.255.250");
        } else {
            grp.fromString("ff02::c");
        }
        return grp;
    }

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
