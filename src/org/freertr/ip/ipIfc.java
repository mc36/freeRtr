package org.freertr.ip;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrType;
import org.freertr.ifc.ifcPolka;
import org.freertr.ifc.ifcUp;
import org.freertr.pack.packHolder;
import org.freertr.tab.tabRateLimit;
import org.freertr.user.userFormat;
import org.freertr.util.counter;
import org.freertr.util.state;

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
     * update ethernet header
     *
     * @param pck packet to update
     * @param nexthop nexthop to query for
     * @param typ ethertype to insert
     * @return true on error, false on success
     */
    public boolean createETHheader(packHolder pck, addrIP nexthop, int typ);

    /**
     * forward raw protocol packet
     *
     * @param pck packet to send
     * @param nexthop next hop ip address
     */
    public void sendProto(packHolder pck, addrIP nexthop);

    /**
     * forward mpls tagged packet
     *
     * @param pck packet to send
     * @param nexthop next hop ip address
     */
    public void sendMpls(packHolder pck, addrIP nexthop);

    /**
     * forward polka tagged packet
     *
     * @param pck packet to send
     * @param nexthop next hop ip address
     */
    public void sendPolka(packHolder pck, addrIP nexthop);

    /**
     * get polka handler
     *
     * @return polka handler
     */
    public ifcPolka getPolka();

    /**
     * send layer2 packet
     *
     * @param l2info layer 2 address
     * @param nexthop ip address
     */
    public void sendL2info(addrType l2info, addrIP nexthop);

    /**
     * get local layer2 info
     *
     * @return local layer2 address
     */
    public addrType getL2info();

    /**
     * update layer2 rewrite info
     *
     * destionaton mac header will be read from packet offset 0.
     *
     * @param mod mode: 0=add aging, 1=add static, 2=delete static
     * @param l2info layer 2 address
     * @param nexthop next hop ip address
     */
    public void updateL2info(int mod, addrType l2info, addrIP nexthop);

    /**
     * get layer2 rewrite info
     *
     * @param nexthop next hop ip address
     * @return mac address, null if not found
     */
    public addrType getL2info(addrIP nexthop);

    /**
     * get layer2 rewrite info
     *
     * @param seq sequence
     * @param nexthop nexthop
     * @param mac mac address
     * @return false on success, true on error
     */
    public boolean getL2info(int seq, addrIP nexthop, addrType mac);

    /**
     * get static layer2 rewrite info
     *
     * @param lst list to append
     * @param beg beginning
     */
    public void getL2info(List<String> lst, String beg);

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
     * set link local address
     *
     * @param adr address to set
     */
    public void setLinkLocalAddr(addrIP adr);

    /**
     * get cache in text
     *
     * @return result
     */
    public userFormat getShCache();

    /**
     * get cache dynamic
     *
     * @return enabled
     */
    public boolean getCacheDynmc();

    /**
     * get cache dynamic
     *
     * @param ena enable
     */
    public void setCacheDynmc(boolean ena);

    /**
     * get cache timeout
     *
     * @return result
     */
    public int getCacheTimer();

    /**
     * set cache timeout
     *
     * @param tim timeout
     */
    public void setCacheTimer(int tim);

    /**
     * get cache retry
     *
     * @return result
     */
    public int getCacheRetry();

    /**
     * set cache retry
     *
     * @param tim timeout
     */
    public void setCacheRetry(int tim);

    /**
     * get cache rate
     *
     * @return result
     */
    public tabRateLimit getCacheRate();

    /**
     * set cache rate
     *
     * @param rat rate
     */
    public void setCacheRate(tabRateLimit rat);

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
