package ifc;

import addr.addrType;
import pack.packHolder;
import util.counter;
import util.state;

/**
 * outgoing packet handler header
 *
 * @author matecsaba
 */
public interface ifcDn {

    /**
     * send this packet
     *
     * @param pck packet to send
     */
    public void sendPack(packHolder pck);

    /**
     * get hardware address
     *
     * @return hw address
     */
    public addrType getHwAddr();

    /**
     * set filter criteria
     *
     * @param promisc need promiscous mode
     */
    public void setFilter(boolean promisc);

    /**
     * get state of interface
     *
     * @return state of line protocol
     */
    public state.states getState();

    /**
     * signal that upper going to terminate
     */
    public void closeDn();

    /**
     * signal that upper flapped the protocol
     */
    public void flapped();

    /**
     * get interface counter
     *
     * @return the counter
     */
    public counter getCounter();

    /**
     * get interface mtu
     *
     * @return the mtu
     */
    public int getMTUsize();

    /**
     * get interface bandwidth
     *
     * @return bits/second
     */
    public long getBandwidth();

    /**
     * set the uplink that receives our packets
     *
     * @param server worker interface
     */
    public void setUpper(ifcUp server);

}
