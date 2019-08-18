package ip;

import addr.addrIP;
import addr.addrIPv6;
import addr.addrMac;
import addr.addrPrefix;
import addr.addrType;
import ifc.ifcDn;
import ifc.ifcNull;
import ifc.ifcUp;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
import pack.packHolder;
import user.userFormat;
import util.counter;
import util.logger;
import util.state;

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
     * suppress router advertisements
     */
    public boolean rtrAdvSuppress = false;

    /**
     * dns information
     */
    public addrIP rtrAdvDns;

    private ipIfc6nei neiCache = null;

    private addrIP ipaddr = new addrIP();

    private addrIP lladdr = new addrIP();

    private addrPrefix<addrIP> prefix;

    private int ipmask;

    private boolean needType;

    /**
     * interface handler
     */
    protected ipFwdIface ifcHdr;

    private ifcDn lower = new ifcNull();

    private ipMpls mpls = null;

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

    /**
     * get hardware address
     *
     * @return hw address
     */
    public addrType getHWaddr() {
        return lower.getHwAddr().copyBytes();
    }

    /**
     * get link local address
     *
     * @return link local address
     */
    public addrIP getLLaddr() {
        return lladdr.copyBytes();
    }

    /**
     * set state
     *
     * @param stat state
     */
    public void setState(state.states stat) {
        cntr.stateChange(stat);
        if (neiCache != null) {
            neiCache.setState(stat);
        }
        upper.ifaceState(ifcHdr, stat);
    }

    /**
     * close interface
     */
    public void closeUp() {
        if (neiCache != null) {
            neiCache.closeUp();
        }
        upper.ifaceDel(ifcHdr);
    }

    /**
     * create new ip receiver
     *
     * @param needNei true if need neighbor cache
     * @param needTyp true if ethertype is needed
     */
    public ipIfc6(boolean needNei, boolean needTyp) {
        needType = needTyp;
        needNei &= needTyp;
        if (needNei) {
            neiCache = new ipIfc6nei(this);
        } else {
            neiCache = null;
        }
        resetTimer();
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
        if (neiCache != null) {
            neiCache.setIPv6addr(addr, mask);
        }
        prefix = new addrPrefix<addrIP>(ipaddr, ipm);
        upper.ifaceAddr(ifcHdr, ipaddr, ipm);
    }

    private boolean createETHheader(packHolder pck, addrIP nexthop, int typ) {
        if (!needType) {
            return false;
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
        if (!needType) {
            upper.ifacePack(ifcHdr, pck);
            return;
        }
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
     */
    public void resetTimer() {
        try {
            timer.cancel();
        } catch (Exception e) {
        }
        timer = null;
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
        icc.createNeighAdv(lower.getHwAddr(), pck, addrIPv6.getAllNodes(), lladdr.toIPv6(), false);
        sendProto(pck, pck.IPtrg);
        icc.createNeighAdv(lower.getHwAddr(), pck, addrIPv6.getAllNodes(), ipaddr.toIPv6(), false);
        sendProto(pck, pck.IPtrg);
        if (rtrAdvInterval < 1) {
            return;
        }
        if (rtrAdvSuppress) {
            return;
        }
        addrIPv6 dns = null;
        if (rtrAdvDns != null) {
            dns = rtrAdvDns.toIPv6();
        }
        icc.createRouterAdv(lower.getHwAddr(), pck, addrIPv6.getAllNodes(), lladdr.toIPv6(), ipaddr.toIPv6(), ipmask, ifcHdr.mtu, dns);
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
