package net.freertr.ip;

import net.freertr.prt.prtTcp;
import net.freertr.prt.prtUdp;
import net.freertr.util.counter;

/**
 * dynamic link exchange (rfc8175) protocol
 *
 * @author matecsaba
 */
public class ipDlep {

    /**
     * counter
     */
    public counter cntr = new counter();

    private final ipFwd fwdCore;

    private final prtUdp udpCore;

    private final prtTcp tcpCore;

    private final ipIfc iface;

    private final boolean client;

    /**
     * create one interface handler
     *
     * @param fwdr forwarder to use
     * @param udpr udp to use
     * @param tcpr tcp to use
     * @param ifc interface to use
     * @param clnt true for client or false for server
     */
    public ipDlep(ipFwd fwdr, prtUdp udpr, prtTcp tcpr, ipIfc ifc, boolean clnt) {
        fwdCore = fwdr;
        udpCore = udpr;
        tcpCore = tcpr;
        iface = ifc;
        client = clnt;
    }

    public String toString() {
        return client ? "client" : "server";
    }

    /**
     * stop working
     */
    public void stopWork() {
        ////////////
    }

}
