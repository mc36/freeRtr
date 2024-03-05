package org.freertr.prt;

import org.freertr.addr.addrIP;
import org.freertr.ip.ipFwdIface;
import org.freertr.pack.packHolder;
import org.freertr.util.counter;
import org.freertr.util.state;

/**
 * datagram servers have to use this to able to work with protocols
 *
 * @author matecsaba
 */
public interface prtServP {

    /**
     * notified that listening interface closed
     *
     * @param ifc iface closing
     */
    public void closedInterface(ipFwdIface ifc);

    /**
     * called by protocol handler when connection coming
     *
     * @param id id number to reference connection
     * @return false to accept connection, true to refuse it
     */
    public boolean datagramAccept(prtGenConn id);

    /**
     * called by protocol handler when connection ready
     *
     * @param id id number to reference connection
     */
    public void datagramReady(prtGenConn id);

    /**
     * notified that connectoin closed
     *
     * @param id id number to reference connection
     */
    public void datagramClosed(prtGenConn id);

    /**
     * notified that periodic work have to done
     *
     * @param id id number to reference connection
     */
    public void datagramWork(prtGenConn id);

    /**
     * notified that packet arrived
     *
     * @param id id number to reference connection
     * @param pck the packet
     * @return return false if successful, true if error happened
     */
    public boolean datagramRecv(prtGenConn id, packHolder pck);

    /**
     * notified that error arrived
     *
     * @param id id number to reference connection
     * @param pck the packet
     * @param rtr reporting router
     * @param err error happened
     * @param lab error label
     * @return return false if successful, true if error happened
     */
    public boolean datagramError(prtGenConn id, packHolder pck, addrIP rtr, counter.reasons err, int lab);

    /**
     * notified that state changed
     *
     * @param id id number to reference connection
     * @param stat state
     * @return return false if successful, true if error happened
     */
    public boolean datagramState(prtGenConn id, state.states stat);

}
