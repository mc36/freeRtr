package org.freertr.serv;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrMac;
import org.freertr.addr.addrPrefix;
import org.freertr.addr.addrType;
import org.freertr.cfg.cfgIfc;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcEthTyp;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcUp;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipIfc4;
import org.freertr.ip.ipIfc4arp;
import org.freertr.pack.packHolder;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtTcp;
import org.freertr.rtr.rtrBgp;
import org.freertr.rtr.rtrBgpNeigh;
import org.freertr.rtr.rtrBgpParam;
import org.freertr.rtr.rtrBgpSpeak;
import org.freertr.rtr.rtrBgpUtil;
import org.freertr.tab.tabRoute;
import org.freertr.tab.tabRouteEntry;
import org.freertr.util.bits;
import org.freertr.util.counter;
import org.freertr.util.logger;
import org.freertr.util.state;

/**
 * one stack interface
 *
 * @author matecsaba
 */
public class servStackIfc implements Runnable, Comparable<servStackIfc>, ifcUp {

    private final static int magic1 = 0x00010000 | ipIfc4.type;

    private final static int magic2 = 0x06040bad;

    /**
     * interface id
     */
    protected cfgIfc pi;

    /**
     * id
     */
    protected int id;

    private final servStackFwd lower;

    private final counter cntr = new counter();

    /**
     * metric
     */
    protected int metric = 10;

    /**
     * peer ip
     */
    protected addrIP bgpAdr;

    /**
     * local asn
     */
    protected int bgpAsn;

    /**
     * bgp pipe
     */
    protected pipeSide bgpPip;

    /**
     * bgp interface
     */
    protected ipFwdIface bgpIfc;

    /**
     * random id
     */
    protected int randId;

    /**
     * last timestamp
     */
    protected long lastTime;

    /**
     * last forwarder
     */
    protected servStackFwd lastFwdr;

    /**
     * last portid
     */
    protected servStackIfc lastPort;

    /**
     * last mac
     */
    protected addrMac lastMac = new addrMac();

    /**
     * ready to use
     */
    protected boolean ready;

    /**
     * interface handler
     */
    protected ifcDn parent = new ifcNull();

    /**
     * ethertype handler
     */
    protected ifcEthTyp ifc;

    /**
     * create instance
     *
     * @param prnt parent
     * @param ifc interface
     */
    protected servStackIfc(servStackFwd prnt, cfgIfc ifc) {
        pi = ifc;
        lower = prnt;
        randId = bits.randomD();
    }

    /**
     * get hardware forwarder info
     *
     * @return offload info
     */
    protected String getShGenOneLiner() {
        if (pi == null) {
            return "n/a,";
        }
        return pi.name + ",";
    }

    public String toString() {
        return "" + pi;
    }

    public int compareTo(servStackIfc o) {
        return pi.compareTo(o.pi);
    }

    /**
     * get source address
     *
     * @return mac address
     */
    protected addrMac getMac() {
        addrType adr = ifc.getHwAddr();
        if (adr.getSize() != addrMac.size) {
            return new addrMac();
        }
        return (addrMac) adr;
    }

    /**
     * send keepalive packet
     */
    protected void sendHello() {
        packHolder pck = new packHolder(true, true);
        pck.msbPutW(0, ipIfc4arp.type);
        pck.putSkip(2);
        pck.putFill(0, ipIfc4arp.size, 0);
        pck.msbPutD(0, magic1);
        pck.msbPutD(4, magic2);
        pck.msbPutD(8, lower.lower.randId);
        pck.msbPutD(12, lower.id);
        pck.msbPutD(16, id);
        pck.msbPutD(20, randId);
        pck.putSkip(ipIfc4arp.size);
        pck.merge2beg();
        pck.ETHsrc.setAddr(getMac());
        pck.ETHtrg.setAddr(addrMac.getBroadcast());
        pck.ETHtype = ipIfc4arp.type;
        parent.sendPack(pck);
    }

    public void recvPack(packHolder pck) {
        int typ = pck.msbGetW(0);
        if (typ != ipIfc4arp.type) {
            logger.info("got invalid (" + bits.toHexW(typ) + ") packet on " + ifc);
            return;
        }
        pck.getSkip(2);
        if (pck.dataSize() < ipIfc4arp.size) {
            logger.info("got truncated packet on " + ifc);
            return;
        }
        if (pck.msbGetD(0) != magic1) {
            logger.info("got invalid magic on " + ifc);
            return;
        }
        if (pck.msbGetD(4) != magic2) {
            logger.info("got invalid magic on " + ifc);
            return;
        }
        if (pck.msbGetD(8) != lower.lower.randId) {
            logger.info("got invalid cluster on " + ifc);
            return;
        }
        int i = pck.msbGetD(12);
        if (i == lower.id) {
            logger.info("got looping packet on " + ifc);
            return;
        }
        if ((i < 0) || (i >= lower.lower.fwds.size())) {
            logger.info("got invalid forwarder id on " + ifc);
            return;
        }
        lastFwdr = lower.lower.fwds.get(i);
        i = pck.msbGetD(16);
        if ((i < 0) || (i >= lastFwdr.ifaces.size())) {
            logger.info("got invalid interface id on " + ifc);
            return;
        }
        lastPort = lastFwdr.ifaces.get(i);
        lastMac.setAddr(lastPort.getMac());
        int lastRand = pck.msbGetD(20);
        if (lastPort.randId != lastRand) {
            logger.info("got invalid random id on " + ifc);
            return;
        }
        lastTime = bits.getTime();
    }

    public void setParent(ifcDn lower) {
        parent = lower;
    }

    public void setState(state.states stat) {
    }

    public void closeUp() {
    }

    public counter getCounter() {
        return cntr;
    }

    /**
     * start work
     */
    protected void startWork() {
        logger.startThread(this);
    }

    /**
     * start work
     */
    protected void stopWork() {
        if (bgpAdr != null) {
            ifc.delET(-1);
            return;
        }
        bgpAsn = 0;
        if (bgpPip != null) {
            bgpPip.setClose();
        }
    }

    public void run() {
        try {
            for (;;) {
                bits.sleep(1000);
                if (bgpAsn == 0) {
                    break;
                }
                doRound();
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

    private tabRouteEntry<addrIP> generateRoute(int id, int met) {
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        ntry.best.nextHop = bgpIfc.addr.copyBytes();
        ntry.best.labelRem = new ArrayList<Integer>();
        ntry.best.labelRem.add(lower.lower.bckplnLab[id].label);
        ntry.best.pathSeq = new ArrayList<Integer>();
        ntry.best.pathSeq.add(bgpAsn);
        ntry.best.metric = met;
        addrIP adr = new addrIP();
        bits.msbPutD(adr.getBytes(), addrIP.size - 4, id);
        adr.setAdd(adr, lower.lower.advertBase);
        ntry.prefix = new addrPrefix<addrIP>(adr, addrIP.size * 8);
        return ntry;
    }

    private void doRound() {
        lastTime = 0;
        bgpIfc = pi.getFwdIfc(bgpAdr);
        if (bgpIfc == null) {
            logger.info("protocol not enabled on " + ifc);
            return;
        }
        if (!bgpIfc.lower.checkConnected(bgpAdr)) {
            logger.info("peer not connected on " + ifc);
            return;
        }
        prtTcp bgpTcp = pi.vrfFor.getTcp(bgpAdr);
        bgpPip = bgpTcp.streamConnect(new pipeLine(32768, false), bgpIfc, 0, bgpAdr, rtrBgp.port, "stack", -1, null, -1, -1);
        if (bgpPip == null) {
            logger.info("unable to connect on " + ifc);
            return;
        }
        bgpPip.setTime(120000);
        if (bgpPip.wait4ready(120000)) {
            bgpPip.setClose();
            return;
        }
        try {
            addrType adr = bgpIfc.lower.getL2info(bgpAdr);
            if (adr == null) {
                logger.info("got no l2 info on " + ifc);
                bgpPip.setClose();
                return;
            }
            lastMac.setAddr(adr);
        } catch (Exception e) {
            bgpPip.setClose();
            return;
        }
        rtrBgp bgp = new rtrBgp(pi.vrfFor.getFwd(bgpAdr), pi.vrfFor, null, 0);
        rtrBgpNeigh nei = new rtrBgpNeigh(bgp, bgpAdr);
        nei.localAs = bgpAsn;
        int safi;
        if (lower.lower.advertBase.isIPv4()) {
            safi = rtrBgpUtil.safiIp4lab;
        } else {
            safi = rtrBgpUtil.safiIp6lab;
        }
        nei.addrFams = rtrBgpParam.boolsSet(false);
        int idx = bgp.safi2idx(safi);
        nei.addrFams[idx] = true;
        rtrBgpSpeak spk = new rtrBgpSpeak(bgp, nei, bgpPip, 0);
        bgpPip.setTime(nei.holdTimer);
        spk.sendOpen();
        spk.sendKeepAlive();
        List<tabRouteEntry<addrIP>> lst = new ArrayList<tabRouteEntry<addrIP>>();
        tabRoute<addrIP> done = new tabRoute< addrIP>("done");
        packHolder pck = new packHolder(true, true);
        packHolder tmp = new packHolder(true, true);
        int bgpVer = -1;
        for (int o = 1000;; o++) {
            bits.sleep(1000);
            if (o > 30) {
                spk.sendKeepAlive();
                o = 0;
            }
            int i = bgpPip.ready2rx();
            if (i > 0) {
                bgpPip.moreSkip(i);
                lastTime = bits.getTime();
            }
            i = bgpPip.ready2tx();
            if (i < 0) {
                break;
            }
            if (bgpVer == lower.lower.vers) {
                continue;
            }
            bgpVer = lower.lower.vers;
            tabRoute<addrIP> need = new tabRoute< addrIP>("need");
            tabRouteEntry<addrIP> ntry = generateRoute(lower.id, metric);
            need.add(tabRoute.addType.always, ntry, false, false);
            for (i = 0; i < lower.routes.size(); i++) {
                ntry = lower.routes.get(i);
                int p = servStack.addr2forwarder(ntry.best.nextHop);
                if (p < 0) {
                    continue;
                }
                p = servStack.addr2forwarder(ntry.prefix.network);
                if (p < 0) {
                    continue;
                }
                ntry = generateRoute(p, metric + ntry.best.metric);
                need.add(tabRoute.addType.always, ntry, false, false);
            }
            for (i = done.size() - 1; i >= 0; i--) {
                ntry = done.get(i);
                if (need.find(ntry) != null) {
                    continue;
                }
                done.del(ntry);
                lst.clear();
                lst.add(ntry);
                pck.clear();
                rtrBgpUtil.createWithdraw(spk, pck, tmp, idx, false, lst);
                spk.packSend(pck, rtrBgpUtil.msgUpdate);
            }
            for (i = 0; i < need.size(); i++) {
                ntry = need.get(i);
                if (ntry.differs(tabRoute.addType.notyet, done.find(ntry)) == 0) {
                    continue;
                }
                done.add(tabRoute.addType.always, ntry, true, true);
                lst.clear();
                lst.add(ntry);
                pck.clear();
                rtrBgpUtil.createReachable(spk, pck, tmp, idx, false, lst);
                spk.packSend(pck, rtrBgpUtil.msgUpdate);
            }
        }
        bgpPip.setClose();
    }

    /**
     * get current status
     *
     * @return timeout in future
     */
    protected long getState() {
        if (bgpAdr == null) {
            if (ifc.getState() != state.states.up) {
                return -1;
            }
            sendHello();
            return lastTime + lower.lower.discoTim;
        }
        if (bgpPip == null) {
            return -1;
        }
        if (bgpPip.isClosed() != 0) {
            return -1;
        }
        return lastTime + bgpPip.getTime();
    }

    /**
     * get show ports
     *
     * @return string
     */
    protected String getShPorts() {
        return pi + "|" + metric + "|" + ready + "|" + lastFwdr + "|" + lastPort + "|" + bgpAdr + "|" + lastMac;
    }

}
