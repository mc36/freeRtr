package ip;

import addr.addrIP;
import addr.addrType;
import ifc.ifcUp;
import pack.packHolder;
import user.userFormat;
import util.counter;
import util.state;

/**
 * ip interface handlers have to extend it to be able to work with ip forwarder
 *
 * @author matecsaba
 */
public interface ipIfc {

    /**
     * set upper layer
     *
     * @param up forwarding handler
     * @param hdr interface handler
     */
    public void setUpper(ipFwd up, ipFwdIface hdr);

    /**
     * set filter criteria
     *
     * @param promisc promiscous mode
     */
    public void setFilter(boolean promisc);

    /**
     * forward raw protocol packet
     *
     * @param pck packet to send
     * @param nexthop next hop ip address
     */
    public abstract void sendProto(packHolder pck, addrIP nexthop);

    /**
     * forward mpls tagged packet
     *
     * @param pck packet to send
     * @param nexthop next hop ip address
     */
    public abstract void sendMpls(packHolder pck, addrIP nexthop);

    /**
     * update layer2 rewrite info
     *
     * destionaton mac header will be read from packet offset 0.
     *
     * @param l2info layer 2 address
     * @param nexthop next hop ip address
     */
    public abstract void updateL2info(addrType l2info, addrIP nexthop);

    /**
     * get layer2 rewrite info
     *
     * @param nexthop next hop ip address
     * @return mac address, null if not found
     */
    public abstract addrType getL2info(addrIP nexthop);

    /**
     * get layer2 rewrite info
     *
     * @param seq sequence
     * @param nexthop nexthop
     * @param mac mac address
     * @return false on success, true on error
     */
    public abstract boolean getL2info(int seq, addrIP nexthop, addrType mac);

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
     * get interface state
     *
     * @return the state
     */
    public state.states getState();

    /**
     * get interface bandwidth
     *
     * @return bits/second
     */
    public long getBandwidth();

    /**
     * got icmp packet
     *
     * @param pck packet parsed
     */
    public void gotIcmpPack(packHolder pck);

    /**
     * check if my address
     *
     * @param adr address to test
     * @return true if my address, false if not
     */
    public boolean checkMyAddress(addrIP adr);

    /**
     * check if my alias
     *
     * @param adr address to test
     * @return true if my address, false if not
     */
    public addrType checkMyAlias(addrIP adr);

    /**
     * check if address is connected
     *
     * @param adr address to test
     * @return return true if connected, false if not
     */
    public boolean checkConnected(addrIP adr);

    /**
     * get link local address
     *
     * @return return link local address, null if none
     */
    public addrIP getLinkLocalAddr();

    /**
     * get arp cache in text
     *
     * @return result
     */
    public userFormat getShArp();

    /**
     * get peer handler
     *
     * @return peer handler
     */
    public ifcUp getPeerHdr();

    /**
     * get ethertype
     *
     * @return value
     */
    public int getEthtyp();

}
