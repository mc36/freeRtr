package ip;

import addr.addrIP;
import addr.addrIPv4;
import addr.addrIPv6;
import addr.addrType;
import ifc.ifcUp;
import java.util.List;
import pack.packHolder;
import user.userFormat;
import util.counter;
import util.state;

/**
 * loopback interface
 *
 * @author matecsaba
 */
public class ipIfcLoop implements ipIfc {

    /**
     * default arp/nd cache timeout
     */
    public final static int defaultCacheTime = 8 * 60 * 1000;

    private counter cntr = new counter();

    private ipFwd upper;

    private ipFwdIface ifcHdr;

    private addrIP ipaddr = new addrIP();

    private int ipmask;

    private int cacheTime = defaultCacheTime;

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
        ipmask = mask;
        addrIPv6 m2 = new addrIPv6();
        m2.fromNetmask(mask);
        addrIP m1 = new addrIP();
        m1.fromIPv6mask(m2);
        int ipm = m1.toNetmask();
        upper.ifaceAddr(ifcHdr, ipaddr, ipm);
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

    public void sendProto(packHolder pck, addrIP nexthop) {
        cntr.rx(pck);
    }

    public void sendMpls(packHolder pck, addrIP nexthop) {
        cntr.rx(pck);
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
        return (ipaddr.compare(ipaddr, adr) == 0);
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

    public userFormat getShCache() {
        return null;
    }

    public int getCacheTimer() {
        return cacheTime;
    }

    public void setCacheTimer(int tim) {
        cacheTime = tim;
    }

    public ifcUp getPeerHdr() {
        return null;
    }

    public int getEthtyp() {
        return -1;
    }

}
