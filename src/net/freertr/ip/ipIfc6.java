package net.freertr.ip;

import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrIPv6;
import net.freertr.addr.addrMac;
import net.freertr.addr.addrPrefix;
import net.freertr.addr.addrType;
import net.freertr.ifc.ifcDn;
import net.freertr.ifc.ifcMpolka;
import net.freertr.ifc.ifcNull;
import net.freertr.ifc.ifcPolka;
import net.freertr.ifc.ifcUp;
import net.freertr.pack.packHolder;
import net.freertr.user.userFormat;
import net.freertr.util.counter;
import net.freertr.util.logger;
import net.freertr.util.state;

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
     * interface handler
     */
    protected ipFwdIface ifcHdr;

    private ifcDn lower = new ifcNull();

    private ipMpls mpls = null;

    private ifcPolka polka = null;

    private ifcMpolka mpolka = null;

    /**
     * forwarder
     */
    protected ipFwd upper = null;

    private Timer timer;

    private ipIcmp6 icc = new ipIcmp6();

    private counter cntr = new counter();

    public counter getCounter() {
        return cntr;
    }

    public boolean checkMyAddress(addrIP adr) {
        if (lladdr.compare(lladdr, adr) == 0) {
            return true;
        }
        return (ipaddr.compare(ipaddr, adr) == 0);
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
     * set polka forwarder
     *
     * @param p lower layer
     */
    public void setPolka(ifcPolka p) {
        polka = p;
    }

    /**
     * set mpolka forwarder
     *
     * @param p lower layer
     */
    public void setMpolka(ifcMpolka p) {
        mpolka = p;
    }

    /**
     * set ip network
     *
     * @param addr address
     * @param mask mask
     */
    public void setIPv6addr(addrIPv6 addr, int mask) {
        resetTimer(false);
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
        if (neiCache != null) {
            neiCache.setIPv6addr(addr, mask);
        }
        prefix = new addrPrefix<addrIP>(ipaddr, ipm);
        upper.ifaceAddr(ifcHdr, ipaddr, ipm);
        resetTimer(true);
    }

    private boolean createETHheader(packHolder pck, addrIP nexthop, int typ) {
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
            return;
        }
        if (createETHheader(pck, nexthop, ipMpls.typeU)) {
            cntr.drop(pck, counter.reasons.notInTab);
            return;
        }
        mpls.send2eth(pck);
    }

    public void sendPolka(packHolder pck, addrIP nexthop) {
        if (polka == null) {
            return;
        }
        ifcPolka.createPolkaHeader(pck);
        if (createETHheader(pck, nexthop, ifcPolka.type)) {
            cntr.drop(pck, counter.reasons.notInTab);
            return;
        }
        polka.send2eth(pck);
    }

    public void sendMpolka(packHolder pck, addrIP nexthop) {
        if (mpolka == null) {
            return;
        }
        ifcMpolka.createMpolkaHeader(pck);
        if (createETHheader(pck, nexthop, ifcMpolka.type)) {
            cntr.drop(pck, counter.reasons.notInTab);
            return;
        }
        mpolka.send2eth(pck);
    }

    public ifcPolka getPolka() {
        return polka;
    }

    public ifcMpolka getMpolka() {
        return mpolka;
    }

    public void sendL2info(addrType l2info, addrIP nexthop) {
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
        try {
            timer.cancel();
        } catch (Exception e) {
        }
        timer = null;
        if (!needRun) {
            return;
        }
        if (rtrAdvInterval < 1) {
            return;
        }
        ipIfc6timer task = new ipIfc6timer(this);
        timer = new Timer();
        timer.schedule(task, 500, rtrAdvInterval);
    }

    /**
     * send advertisements
     */
    public void sendAdverts() {
        packHolder pck = new packHolder(true, true);
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

class ipIfc6timer extends TimerTask {

    ipIfc6 parent;

    public ipIfc6timer(ipIfc6 prnt) {
        parent = prnt;
    }

    public void run() {
        try {
            parent.sendAdverts();
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}
