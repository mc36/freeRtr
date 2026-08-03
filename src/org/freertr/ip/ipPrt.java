package org.freertr.ip;

import org.freertr.addr.addrIP;
import org.freertr.pack.packHolder;
import org.freertr.util.counter;
import org.freertr.util.state;

/**
 * ip protocols have to use it to be able to work with ip forwarder
 *
 * @author matecsaba
 */
public interface ipPrt {

    /**
     * get protocol number
     *
     * @return the number of protocol
     */
    public int getProtoNum();

    /**
     * signal loss of this interface
     *
     * @param iface interface number on where
     */
    public void closeUp(ipFwdIface iface);

    /**
     * signal state change of this interface
     *
     * @param iface interface number on where
     * @param stat state of interface
     */
    public void setState(ipFwdIface iface, state.states stat);

    /**
     * signal packet arrival
     *
     * @param rxIfc receiver interface
     * @param pck packet received
     */
    public void recvPack(ipFwdIface rxIfc, packHolder pck);

    /**
     * signal packet arrival
     *
     * @param rxIfc receiver interface
     * @param pck packet received
     * @return false if processed successfully, true if not
     */
    public boolean alertPack(ipFwdIface rxIfc, packHolder pck);

    /**
     * got one error reporting packet
     *
     * @param err error code
     * @param rtr reporting router
     * @param rxIfc receiver interface
     * @param pck packet received
     */
    public void errorPack(counter.reasons err, addrIP rtr, ipFwdIface rxIfc, packHolder pck);

    /**
     * get interface counter
     *
     * @return the counter
     */
    public counter getCounter();

}
