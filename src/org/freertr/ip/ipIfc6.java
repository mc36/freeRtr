package org.freertr.ip;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv6;
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
import org.freertr.util.bits;
import org.freertr.util.counter;
import org.freertr.util.logger;
import org.freertr.util.state;

/**
 * processes ipv6 packets from interfaces
 *
 * @author matecsaba
 */
public class ipIfc6 implements ipIfc, ifcUp {

    /**
     * type of ipv6 packets
     */
    public final static int type = 0x86dd;

    /**
     * router advertisement interval
     */
    public int rtrAdvInterval = 120 * 1000;

    /**
     * router validity interval
     */
    public int rtrAdvValidity = 604800 * 1000;

    /**
     * suppress router advertisements
     */
    public boolean rtrAdvSuppress = false;

    /**
     * dns information
     */
    public addrIP rtrAdvDns1;

    /**
     * dns information
     */
    public addrIP rtrAdvDns2;

    /**
     * domain information
     */
    public String rtrAdvDom;

    private ipIfc6nei neiCache = null;

    private addrIP ipaddr = new addrIP();

    private addrIP lladdr = new addrIP();

    private addrPrefix<addrIP> prefix;

    private int ipmask;

    /**
     * redirect packets
     */
    public ipIfc6 redirect;

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

    /**
     * timer
     */
    protected ipIfc6timer timer;

    private ipIcmp6 icc = new ipIcmp6();

    private counter cntr = new counter();

    public counter getCounter() {
        return cntr;
    }

    public boolean checkMyAddress(addrIP adr) {
        if (lladdr.compareTo(adr) == 0) {
            return true;
        }
        return ipaddr.compareTo(adr) == 0;
    }

    public addrType checkMyAlias(addrIP adr) {
        return ifcHdr.adrChk(adr);
    }

    public boolean checkConnected(addrIP adr) {
        return adr.toIPv6().isLinkLocal() || prefix.matches(adr);
    }

    public addrIP getLinkLocalAddr() {
        return lladdr;
    }

    public void setLinkLocalAddr(addrIP adr) {
        lladdr = adr.copyBytes();
        if (neiCache != null) {
            neiCache.setLinkLocalAddr(adr);
        }
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
        addrMac hwaddr;
        lower = parent;
        if (neiCache != null) {
            neiCache.setParent(parent);
            hwaddr = (addrMac) lower.getHwAddr();
        } else {
            hwaddr = addrMac.getRandom();
        }
        lladdr.fromIPv6addr(addrIPv6.genLinkLocal(hwaddr));
    }

    public userFormat getShCache() {
        if (neiCache == null) {
            return null;
        }
        return neiCache.getShCache();
    }

    public boolean getCacheDynmc() {
        if (neiCache == null) {
            return true;
        }
        return neiCache.neiCacheDynamic;
    }

    public void setCacheDynmc(boolean ena) {
        if (neiCache == null) {
            return;
        }
        neiCache.neiCacheDynamic = ena;
    }

    public int getCacheTimer() {
        if (neiCache == null) {
            return ipIfcLoop.defaultCacheTime;
        }
        return neiCache.neiCacheTimeout;
    }

    public void setCacheTimer(int tim) {
        if (neiCache == null) {
            return;
        }
        neiCache.neiCacheTimeout = tim;
    }

    public int getCacheRetry() {
        if (neiCache == null) {
            return ipIfcLoop.defaultRetryTime;
        }
        return neiCache.neiCacheRetry;
    }

    public void setCacheRetry(int tim) {
        if (neiCache == null) {
            return;
        }
        neiCache.neiCacheRetry = tim;
    }

    public tabRateLimit getCacheRate() {
        if (neiCache == null) {
            return null;
        }
        return neiCache.neiQueryRate;
    }

    public void setCacheRate(tabRateLimit rat) {
        if (neiCache == null) {
            return;
        }
        neiCache.neiQueryRate = rat;
    }

    /**
     * get hardware address
     *
     * @return hw address
     */
    public addrType getHWaddr() {
        return lower.getHwAddr().copyBytes();
    }

    /**
     * set state
     *
     * @param stat state
     */
    public void setState(state.states stat) {
        cntr.stateChange(stat);
        resetTimer(state.toUsable(stat) == state.states.up);
        if (neiCache != null) {
            neiCache.setState(stat);
        }
        upper.ifaceState(ifcHdr, stat);
    }

    /**
     * close interface
     */
    public void closeUp() {
        resetTimer(false);
        if (neiCache != null) {
            neiCache.closeUp();
        }
        upper.ifaceDel(ifcHdr);
    }

    /**
     * create new ip receiver
     *
     * @param needNei true if need neighbor cache
     */
    public ipIfc6(boolean needNei) {
        if (needNei) {
            neiCache = new ipIfc6nei(this);
        } else {
            neiCache = null;
        }
        resetTimer(true);
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
    public void setIPv6addr(addrIPv6 addr, int mask) {
        if (addr.isLinkLocal()) {
            lladdr.setAddr(addr);
        }
        ipaddr = new addrIP();
        ipaddr.fromIPv6addr(addr);
        ipmask = mask;
        addrIPv6 m2 = new addrIPv6();
        m2.fromNetmask(mask);
        addrIP m1 = new addrIP();
        m1.fromIPv6mask(m2);
        int ipm = m1.toNetmask();
        prefix = new addrPrefix<addrIP>(ipaddr, ipm);
        if (neiCache != null) {
            neiCache.setIPv6addr(addr, mask);
        }
        upper.ifaceAddr(ifcHdr, ipaddr, ipm);
    }

    public boolean createETHheader(packHolder pck, addrIP nexthop, int typ) {
        if (nexthop.isIPv4()) {
            if (otherHdr == null) {
                return true;
            }
            return otherHdr.createETHheader(pck, nexthop, typ);
        }
        pck.msbPutW(0, typ);
        pck.putSkip(2);
        pck.merge2beg();
        pck.putStart();
        if (neiCache == null) {
            return false;
        }
        return neiCache.readMACheader(pck, nexthop.toIPv6());
    }

    /**
     * received packet
     *
     * @param pck packet
     */
    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        if (pck.msbGetW(0) != type) {// ethertype
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
        if (neiCache == null) {
            return;
        }
        packHolder pck = new packHolder(true, true);
        icc.createNeighAdv(l2info, pck, addrIPv6.getAllNodes(), nexthop.toIPv6(), false);
        if (createETHheader(pck, pck.IPtrg, type)) {
            cntr.drop(pck, counter.reasons.notInTab);
            return;
        }
        pck.ETHsrc.setAddr(l2info);
        cntr.tx(pck);
        lower.sendPack(pck);
    }

    public addrType getL2info() {
        if (neiCache == null) {
            return null;
        }
        return neiCache.getLocalMac();
    }

    public void updateL2info(int mod, addrType l2info, addrIP nexthop) {
        if (neiCache == null) {
            return;
        }
        neiCache.updateMACheader(mod, (addrMac) l2info, nexthop.toIPv6());
    }

    public addrType getL2info(addrIP nexthop) {
        if (neiCache == null) {
            return null;
        }
        return neiCache.getMACaddr(nexthop.toIPv6());
    }

    public boolean getL2info(int seq, addrIP nexthop, addrType mac) {
        if (neiCache == null) {
            return true;
        }
        return neiCache.getMACaddr(seq, nexthop, (addrMac) mac);
    }

    public void getL2info(List<String> lst, String beg) {
        if (neiCache == null) {
            return;
        }
        neiCache.getMACaddr(lst, beg);
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
        if (neiCache == null) {
            return;
        }
        neiCache.gotIcmpPack(pck);
    }

    /**
     * setup router advertise sender
     *
     * @param needRun true if needed
     */
    public synchronized void resetTimer(boolean needRun) {
        timer = null;
        if (!needRun) {
            return;
        }
        if (rtrAdvInterval < 1) {
            return;
        }
        timer = new ipIfc6timer(this);
        timer.start();
    }

    /**
     * send advertisements
     */
    public void sendAdverts() {
        packHolder pck = new packHolder(true, true);
        if (ifcHdr != null) {
            for (int i = 0;; i++) {
                addrIP adr = ifcHdr.adrGetIp(i);
                if (adr == null) {
                    break;
                }
                addrMac mac = ifcHdr.adrGetMac(i);
                if (mac == null) {
                    break;
                }
                sendL2info(mac, adr);
            }
        }
        addrType hwa = lower.getHwAddr();
        icc.createNeighAdv(hwa, pck, addrIPv6.getAllNodes(), lladdr.toIPv6(), false);
        sendProto(pck, pck.IPtrg);
        icc.createNeighAdv(hwa, pck, addrIPv6.getAllNodes(), ipaddr.toIPv6(), false);
        sendProto(pck, pck.IPtrg);
        if (rtrAdvInterval < 1) {
            return;
        }
        if (rtrAdvSuppress) {
            return;
        }
        addrIPv6 dns1 = null;
        if (rtrAdvDns1 != null) {
            dns1 = rtrAdvDns1.toIPv6();
        }
        addrIPv6 dns2 = null;
        if (rtrAdvDns2 != null) {
            dns2 = rtrAdvDns2.toIPv6();
        }
        icc.createRouterAdv(hwa, pck, addrIPv6.getAllNodes(), lladdr.toIPv6(), ipaddr.toIPv6(), ipmask, ifcHdr.mtu + ipCor6.size, dns1, dns2, rtrAdvDom, rtrAdvValidity);
        sendProto(pck, pck.IPtrg);
    }

    public ifcUp getPeerHdr() {
        return neiCache;
    }

    public int getEthtyp() {
        return type;
    }

}

class ipIfc6timer implements Runnable {

    private ipIfc6 parent;

    public ipIfc6timer(ipIfc6 prnt) {
        parent = prnt;
    }

    public void start() {
        logger.startThread(this);
    }

    public void run() {
        try {
            for (;;) {
                bits.sleep(parent.rtrAdvInterval);
                if (parent.timer != this) {
                    break;
                }
                parent.sendAdverts();
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}
