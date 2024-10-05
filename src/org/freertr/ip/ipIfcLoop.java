package org.freertr.ip;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrIPv6;
import org.freertr.addr.addrPrefix;
import org.freertr.addr.addrType;
import org.freertr.ifc.ifcPolka;
import org.freertr.ifc.ifcUp;
import org.freertr.pack.packHolder;
import org.freertr.tab.tabRateLimit;
import org.freertr.user.userFormat;
import org.freertr.util.counter;
import org.freertr.util.state;

/**
 * loopback interface
 *
 * @author matecsaba
 */
public class ipIfcLoop implements ipIfc {

    /**
     * create instance
     */
    public ipIfcLoop() {
    }

    /**
     * default arp/nd cache timeout
     */
    public final static int defaultCacheTime = 6 * 60 * 1000;

    /**
     * default arp/nd retry timeout
     */
    public final static int defaultRetryTime = 3 * 60 * 1000;

    private counter cntr = new counter();

    private ipFwd upper;

    private ipFwdIface ifcHdr;

    private addrIP ipaddr = new addrIP();

    private int ipmask;

    private int cacheTime = defaultCacheTime;

    private int cacheRetry = defaultRetryTime;

    /**
     * set ip network
     *
     * @param addr address
     * @param mask mask
     */
    public void setIPv4addr(addrIPv4 addr, int mask) {
        ipaddr = new addrIP();
        ipaddr.fromIPv4addr(addr);
        addrIPv4 m2 = new addrIPv4();
        m2.fromNetmask(mask);
        addrIP m1 = new addrIP();
        m1.fromIPv4mask(m2);
        ipmask = m1.toNetmask();
        if (upper == null) {
            return;
        }
        upper.ifaceAddr(ifcHdr, ipaddr, ipmask);
    }

    /**
     * set ip network
     *
     * @param addr address
     * @param mask mask
     */
    public void setIPv6addr(addrIPv6 addr, int mask) {
        ipaddr = new addrIP();
        ipaddr.fromIPv6addr(addr);
        addrIPv6 m2 = new addrIPv6();
        m2.fromNetmask(mask);
        addrIP m1 = new addrIP();
        m1.fromIPv6mask(m2);
        ipmask = m1.toNetmask();
        if (upper == null) {
            return;
        }
        upper.ifaceAddr(ifcHdr, ipaddr, ipmask);
    }

    /**
     * create an interface from my address
     *
     * @return forwarder interface
     */
    public ipFwdIface getFwdIface() {
        ipFwdIface ifc = new ipFwdIface(-1, this);
        ifc.addr = new addrIP();
        ifc.network = new addrPrefix<addrIP>(ipaddr, ipmask);
        ifc.ready = true;
        ifc.mtu = 1500;
        ifc.bandwidth = 100000000;
        ifc.addr = ipaddr.copyBytes();
        ifc.mask = ipmask;
        return ifc;
    }

    /**
     * set upper layer
     *
     * @param up upper
     * @param hdr interface
     */
    public void setUpper(ipFwd up, ipFwdIface hdr) {
        upper = up;
        ifcHdr = hdr;
    }

    public void setFilter(boolean promisc) {
    }

    public boolean createETHheader(packHolder pck, addrIP nexthop, int typ) {
        return true;
    }

    public void sendProto(packHolder pck, addrIP nexthop) {
        cntr.rx(pck);
    }

    public void sendMpls(packHolder pck, addrIP nexthop) {
        cntr.rx(pck);
    }

    public void sendPolka(packHolder pck, addrIP nextHop) {
        cntr.rx(pck);
    }

    public ifcPolka getPolka() {
        return null;
    }

    public void sendL2info(addrType l2info, addrIP nexthop) {
    }

    public addrType getL2info() {
        return null;
    }

    public String toString() {
        return "<loop>";
    }

    public void updateL2info(int mod, addrType mac, addrIP nexthop) {
    }

    public addrType getL2info(addrIP nexthop) {
        return null;
    }

    public boolean getL2info(int seq, addrIP nexthop, addrType mac) {
        return true;
    }

    public void getL2info(List<String> lst, String beg) {
    }

    public counter getCounter() {
        return cntr;
    }

    public int getMTUsize() {
        return 65535;
    }

    public state.states getState() {
        return state.states.up;
    }

    public long getBandwidth() {
        return 1000000000;
    }

    public void gotIcmpPack(packHolder pck) {
    }

    public boolean checkMyAddress(addrIP adr) {
        return ipaddr.compareTo(adr) == 0;
    }

    public addrType checkMyAlias(addrIP adr) {
        return ifcHdr.adrChk(adr);
    }

    public boolean checkConnected(addrIP adr) {
        return checkMyAddress(adr);
    }

    public addrIP getLinkLocalAddr() {
        return null;
    }

    public void setLinkLocalAddr(addrIP adr) {
    }

    public boolean getCacheDynmc() {
        return true;
    }

    public void setCacheDynmc(boolean ena) {
    }

    public userFormat getShCache() {
        return null;
    }

    public int getCacheTimer() {
        return cacheTime;
    }

    public void setCacheTimer(int tim) {
        cacheTime = tim;
    }

    public int getCacheRetry() {
        return cacheRetry;
    }

    public void setCacheRetry(int tim) {
        cacheRetry = tim;
    }

    public tabRateLimit getCacheRate() {
        return null;
    }

    public void setCacheRate(tabRateLimit rat) {
    }

    public ifcUp getPeerHdr() {
        return null;
    }

    public int getEthtyp() {
        return -1;
    }

}
