package org.freertr.ip;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrMac;
import org.freertr.addr.addrPrefix;
import org.freertr.addr.addrType;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcPolka;
import org.freertr.ifc.ifcUp;
import org.freertr.pack.packHolder;
import org.freertr.tab.tabRateLimit;
import org.freertr.user.userFormat;
import org.freertr.util.counter;
import org.freertr.util.logger;
import org.freertr.util.state;

/**
 * processes ipv4 packets from interfaces
 *
 * @author matecsaba
 */
public class ipIfc4 implements ipIfc, ifcUp {

    /**
     * ethertype of my packets
     */
    public final static int type = 0x800;

    private ipIfc4arp arpCache = null;

    private addrIP ipaddr = new addrIP();

    private addrPrefix<addrIP> prefix;

    /**
     * redirect packets
     */
    public ipIfc4 redirect;

    /**
     * other interface handler
     */
    protected ipIfc otherHdr;

    /**
     * interface handler
     */
    protected ipFwdIface ifcHdr;

    private ifcDn lower = new ifcNull();

    private ipMpls mpls = null;

    private ifcPolka polka = null;

    /**
     * forwarder
     */
    protected ipFwd upper = null;

    private counter cntr = new counter();

    public counter getCounter() {
        return cntr;
    }

    public boolean checkMyAddress(addrIP adr) {
        return ipaddr.compareTo(adr) == 0;
    }

    public addrType checkMyAlias(addrIP adr) {
        return ifcHdr.adrChk(adr);
    }

    public boolean checkConnected(addrIP adr) {
        return prefix.matches(adr);
    }

    public addrIP getLinkLocalAddr() {
        return null;
    }

    public void setLinkLocalAddr(addrIP adr) {
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
        cntr = hdr.cntr;
        setState(lower.getState());
    }

    public void setFilter(boolean promisc) {
        lower.setFilter(promisc);
    }

    /**
     * set parent
     *
     * @param parent parent
     */
    public void setParent(ifcDn parent) {
        lower = parent;
        if (arpCache != null) {
            arpCache.setParent(parent);
        }
    }

    /**
     * set state
     *
     * @param stat state
     */
    public void setState(state.states stat) {
        cntr.stateChange(stat);
        if (arpCache != null) {
            arpCache.setState(stat);
        }
        upper.ifaceState(ifcHdr, stat);
    }

    /**
     * close interface
     */
    public void closeUp() {
        if (arpCache != null) {
            arpCache.closeUp();
        }
        upper.ifaceDel(ifcHdr);
    }

    /**
     * create new ip receiver
     *
     * @param needArp true if need arp cache
     */
    public ipIfc4(boolean needArp) {
        if (needArp) {
            arpCache = new ipIfc4arp(this);
        } else {
            arpCache = null;
        }
    }

    /**
     * set mpls forwarder
     *
     * @param m lower layer
     */
    public void setMpls(ipMpls m) {
        mpls = m;
    }

    /**
     * set other forwarder
     *
     * @param o lower layer
     */
    public void setOther(ipIfc o) {
        otherHdr = o;
    }

    /**
     * set polka forwarder
     *
     * @param p lower layer
     */
    public void setPolka(ifcPolka p) {
        polka = p;
    }

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
        int ipm = m1.toNetmask();
        if (arpCache != null) {
            arpCache.setIPv4addr(addr, mask);
        }
        prefix = new addrPrefix<addrIP>(ipaddr, ipm);
        upper.ifaceAddr(ifcHdr, ipaddr, ipm);
    }

    public boolean createETHheader(packHolder pck, addrIP nexthop, int typ) {
        if (!nexthop.isIPv4()) {
            if (otherHdr == null) {
                return true;
            }
            return otherHdr.createETHheader(pck, nexthop, typ);
        }
        pck.msbPutW(0, typ);
        pck.putSkip(2);
        pck.merge2beg();
        pck.putStart();
        if (arpCache == null) {
            return false;
        }
        return arpCache.readMACheader(pck, nexthop.toIPv4());
    }

    /**
     * received packet
     *
     * @param pck packet
     */
    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        int ethTyp = pck.msbGetW(0); // ethertype
        if ((arpCache != null) && (ethTyp == ipIfc4arp.type)) {
            arpCache.recvPack(pck);
            return;
        }
        if (ethTyp != type) {
            cntr.drop(pck, counter.reasons.badEthTyp);
            return;
        }
        pck.getSkip(2);
        upper.ifacePack(ifcHdr, pck);
    }

    public void sendProto(packHolder pck, addrIP nexthop) {
        if (createETHheader(pck, nexthop, type)) {
            cntr.drop(pck, counter.reasons.notInTab);
            return;
        }
        if (redirect != null) {
            redirect.cntr.tx(pck);
            redirect.lower.sendPack(pck);
            return;
        }
        cntr.tx(pck);
        lower.sendPack(pck);
    }

    public void sendMpls(packHolder pck, addrIP nexthop) {
        if (mpls == null) {
            logger.info("protocol not enabled on " + lower);
            return;
        }
        if (createETHheader(pck, nexthop, mpls.ethtyp)) {
            cntr.drop(pck, counter.reasons.notInTab);
            return;
        }
        mpls.send2eth(pck);
    }

    public void sendPolka(packHolder pck, addrIP nexthop) {
        if (polka == null) {
            logger.info("protocol not enabled on " + lower);
            return;
        }
        ifcPolka.createPolkaHeader(pck);
        if (createETHheader(pck, nexthop, ifcPolka.type)) {
            cntr.drop(pck, counter.reasons.notInTab);
            return;
        }
        polka.send2eth(pck);
    }

    public ifcPolka getPolka() {
        return polka;
    }

    public void sendL2info(addrType l2info, addrIP nexthop) {
        if (arpCache == null) {
            return;
        }
        arpCache.sendARPheader((addrMac) l2info, nexthop.toIPv4());
    }

    public addrType getL2info() {
        if (arpCache == null) {
            return null;
        }
        return arpCache.getLocalMac();
    }

    public void updateL2info(int mod, addrType l2info, addrIP nexthop) {
        if (arpCache == null) {
            return;
        }
        arpCache.updateMACheader(mod, (addrMac) l2info, nexthop.toIPv4());
    }

    public addrType getL2info(addrIP nexthop) {
        if (arpCache == null) {
            return null;
        }
        return arpCache.getMACaddr(nexthop.toIPv4());
    }

    public boolean getL2info(int seq, addrIP nexthop, addrType mac) {
        if (arpCache == null) {
            return true;
        }
        return arpCache.getMACaddr(seq, nexthop, (addrMac) mac);
    }

    public void getL2info(List<String> lst, String beg) {
        if (arpCache == null) {
            return;
        }
        arpCache.getMACaddr(lst, beg);
    }

    public String toString() {
        return "" + lower;
    }

    public int getMTUsize() {
        return lower.getMTUsize();
    }

    public state.states getState() {
        return lower.getState();
    }

    public long getBandwidth() {
        return lower.getBandwidth();
    }

    public void gotIcmpPack(packHolder pck) {
    }

    public userFormat getShCache() {
        if (arpCache == null) {
            return null;
        }
        return arpCache.getShCache();
    }

    public boolean getCacheDynmc() {
        if (arpCache == null) {
            return true;
        }
        return arpCache.arpCacheDynamic;
    }

    public void setCacheDynmc(boolean ena) {
        if (arpCache == null) {
            return;
        }
        arpCache.arpCacheDynamic = ena;
    }

    public int getCacheTimer() {
        if (arpCache == null) {
            return ipIfcLoop.defaultCacheTime;
        }
        return arpCache.arpCacheTimeout;
    }

    public void setCacheTimer(int tim) {
        if (arpCache == null) {
            return;
        }
        arpCache.arpCacheTimeout = tim;
    }

    public int getCacheRetry() {
        if (arpCache == null) {
            return ipIfcLoop.defaultRetryTime;
        }
        return arpCache.arpCacheRetry;
    }

    public void setCacheRetry(int tim) {
        if (arpCache == null) {
            return;
        }
        arpCache.arpCacheRetry = tim;
    }

    public tabRateLimit getCacheRate() {
        if (arpCache == null) {
            return null;
        }
        return arpCache.arpQueryRate;
    }

    public void setCacheRate(tabRateLimit rat) {
        if (arpCache == null) {
            return;
        }
        arpCache.arpQueryRate = rat;
    }

    public ifcUp getPeerHdr() {
        return arpCache;
    }

    public int getEthtyp() {
        return type;
    }

}
