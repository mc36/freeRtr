package org.freertr.rtr;

import org.freertr.addr.addrIP;
import org.freertr.addr.addrPrefix;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgVrf;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdBier;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipFwdMcast;
import org.freertr.ip.ipPrt;
import org.freertr.pack.packHolder;
import org.freertr.pack.packPim;
import org.freertr.pack.packPimGrp;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabRouteEntry;
import org.freertr.user.userFormat;
import org.freertr.util.bits;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.state;

/**
 * protocol independent multicast (rfc4601) interface
 *
 * @author matecsaba
 */
public class rtrPimIface implements ipPrt {

    /**
     * hello interval
     */
    public int helloInterval = 30000;

    /**
     * dr priority
     */
    public int drPriority = 1;

    /**
     * time to wait between packets
     */
    public int interPackTime = 20;

    /**
     * bier id, 0 to disable tunneling
     */
    public int bierTunnel = 0;

    /**
     * source of join messages
     */
    public ipFwdIface joinSource = null;

    /**
     * allow receive of routes
     */
    public boolean allowRx = true;

    /**
     * allow transmit of routes
     */
    public boolean allowTx = true;

    /**
     * bfd enabled
     */
    public boolean bfdTrigger;

    /**
     * extra groups
     */
    public tabGen<ipFwdMcast> extra = new tabGen<ipFwdMcast>();

    private int generationId;

    private ipFwd fwdCore;

    private ipFwdIface iface;

    private counter cntr = new counter();

    private tabGen<rtrPimNeigh> neighs = new tabGen<rtrPimNeigh>();

    /**
     * keepalive
     */
    protected rtrPimIfaceHello keepTimer;

    /**
     * create new instance
     *
     * @param fwd forwarder
     * @param ifc interface
     */
    public rtrPimIface(ipFwd fwd, ipFwdIface ifc) {
        fwdCore = fwd;
        iface = ifc;
        generationId = bits.randomD();
    }

    /**
     * setup timer thread
     *
     * @param shutdown set true to shut down
     */
    public void restartTimer(boolean shutdown) {
        keepTimer = null;
        if (shutdown) {
            return;
        }
        if (helloInterval < 1) {
            return;
        }
        keepTimer = new rtrPimIfaceHello(this);
        keepTimer.start();
    }

    /**
     * unregister from ip
     */
    public void unregister2ip() {
        if (debugger.rtrPimEvnt) {
            logger.debug("unregister " + iface);
        }
        fwdCore.protoDel(this, iface, null);
        restartTimer(true);
    }

    /**
     * register to ip
     */
    public void register2ip() {
        if (debugger.rtrPimEvnt) {
            logger.debug("register " + iface);
        }
        fwdCore.protoAdd(this, iface, null);
        restartTimer(false);
    }

    public String toString() {
        return "pim";
    }

    /**
     * get protocol number
     *
     * @return number
     */
    public int getProtoNum() {
        return packPim.proto;
    }

    /**
     * close interface
     *
     * @param iface interface
     */
    public void closeUp(ipFwdIface iface) {
    }

    /**
     * get counter
     *
     * @return counter
     */
    public counter getCounter() {
        return cntr;
    }

    /**
     * received packet
     *
     * @param rxIfc interface
     * @param pckBin packet
     */
    public void recvPack(ipFwdIface rxIfc, packHolder pckBin) {
        if (!allowRx) {
            return;
        }
        cntr.rx(pckBin);
        packPim pckPim = new packPim();
        if (pckPim.parseHeader(pckBin)) {
            cntr.drop(pckBin, counter.reasons.badHdr);
            return;
        }
        if (pckPim.parsePayload(pckBin)) {
            cntr.drop(pckBin, counter.reasons.badCod);
            return;
        }
        if (debugger.rtrPimTraf) {
            logger.debug("rx " + pckPim + " from " + pckBin.IPsrc);
        }
        rtrPimNeigh nei = new rtrPimNeigh(this, pckBin.IPsrc);
        switch (pckPim.typ) {
            case packPim.typJoin:
                if (pckPim.upstream.compareTo(iface.addr) != 0) {
                    break;
                }
                int tim = pckPim.valHoldTime * 1000;
                for (int o = 0; o < pckPim.groups.size(); o++) {
                    packPimGrp grp = pckPim.groups.get(o);
                    if (!grp.group.wildcard.isEmpty()) {
                        continue;
                    }
                    int lab = 0;
                    ipFwd fwd = fwdCore;
                    if (grp.rd != 0) {
                        cfgVrf vrf = cfgAll.findRd(grp.group.network.isIPv4(), grp.rd);
                        if (vrf == null) {
                            continue;
                        }
                        if (bierTunnel < 1) {
                            continue;
                        }
                        fwd = vrf.getFwd(grp.group.network);
                        lab = fwd.commonLabel.label;
                    }
                    for (int i = 0; i < grp.joins.size(); i++) {
                        addrPrefix<addrIP> src = grp.joins.get(i);
                        if (!src.wildcard.isEmpty()) {
                            continue;
                        }
                        if (bierTunnel > 0) {
                            fwd.mcastAddFloodBier(grp.group.network, src.network, lab, fwdCore, pckBin.IPsrc, bierTunnel, tim);
                        } else {
                            fwd.mcastAddFloodIfc(grp.group.network, src.network, iface, tim);
                        }
                    }
                    for (int i = 0; i < grp.prunes.size(); i++) {
                        addrPrefix<addrIP> src = grp.prunes.get(i);
                        if (!src.wildcard.isEmpty()) {
                            continue;
                        }
                        if (bierTunnel > 0) {
                            fwd.mcastDelFloodBier(grp.group.network, src.network, lab, fwdCore, pckBin.IPsrc);
                        } else {
                            fwd.mcastDelFloodIfc(grp.group.network, src.network, iface);
                        }
                    }
                }
                break;
            case packPim.typHello:
                rtrPimNeigh old = neighs.add(nei);
                if (old != null) {
                    nei = old;
                } else {
                    nei.upTime = bits.getTime();
                    logger.warn("neighbor " + nei.peer + " up");
                    if (bfdTrigger) {
                        iface.bfdAdd(pckBin.IPsrc, nei, "pim");
                    }
                }
                nei.last = bits.getTime();
                nei.hold = pckPim.valHoldTime * 1000;
                nei.pri = pckPim.valDrPri;
                break;
        }
    }

    /**
     * alert packet
     *
     * @param rxIfc interface
     * @param pck packet
     * @return false if success, true if error
     */
    public boolean alertPack(ipFwdIface rxIfc, packHolder pck) {
        return true;
    }

    /**
     * error packet
     *
     * @param err error code
     * @param rtr address
     * @param rxIfc interface
     * @param pck packet
     */
    public void errorPack(counter.reasons err, addrIP rtr, ipFwdIface rxIfc, packHolder pck) {
    }

    /**
     * purge neighbors
     */
    public synchronized void purgeNeighs() {
        long tim = bits.getTime();
        for (int i = neighs.size(); i >= 0; i--) {
            rtrPimNeigh nei = neighs.get(i);
            if (nei == null) {
                continue;
            }
            if ((nei.last + nei.hold) > tim) {
                continue;
            }
            neighs.del(nei);
            logger.error("neighbor " + nei.peer + " down");
            iface.bfdDel(nei.peer, nei);
        }
    }

    /**
     * send hello packet
     */
    public void sendHello() {
        if (bierTunnel > 0) {
            return;
        }
        if (!allowTx) {
            return;
        }
        packHolder pckBin = new packHolder(true, true);
        packPim pckPim = new packPim();
        pckPim.fillHello(helloInterval, drPriority, generationId, iface.addr);
        pckPim.createHello(pckBin);
        if (debugger.rtrPimTraf) {
            logger.debug("tx " + pckPim);
        }
        pckPim.createHeader(pckBin, iface, null);
        fwdCore.protoPack(iface, null, pckBin);
    }

    /**
     * send one join
     *
     * @param grp group to join
     * @param ups upstream address
     * @param need 1=join, 0=prune
     */
    public void sendJoin(ipFwdMcast grp, addrIP ups, int need) {
        if (!allowTx) {
            return;
        }
        if (bierTunnel > 0) {
            if (ups == null) {
                ups = grp.source;
            }
            tabRouteEntry<addrIP> rou = fwdCore.actualM.route(ups);
            if (rou == null) {
                return;
            }
            if (rou.best.oldHop != null) {
                ups = rou.best.oldHop;
            }
        } else {
            if (ups == null) {
                ups = grp.upstream;
            }
        }
        if (ups == null) {
            return;
        }
        packHolder pckBin = new packHolder(true, true);
        packPim pckPim = new packPim();
        pckPim.fillJoin(ups, grp.rd, grp.group, grp.source, helloInterval * need);
        pckPim.createJoin(pckBin);
        if (debugger.rtrPimTraf) {
            logger.debug("tx " + pckPim + " on " + iface);
        }
        if (bierTunnel < 1) {
            ups = null;
        }
        if (joinSource != null) {
            pckPim.createHeader(pckBin, joinSource, ups);
        } else {
            pckPim.createHeader(pckBin, iface, ups);
        }
        if (bierTunnel < 1) {
            fwdCore.protoPack(iface, null, pckBin);
            return;
        }
        if (ups == null) {
            return;
        }
        fwdCore.createIPheader(pckBin);
        pckBin.ETHtype = iface.lower.getEthtyp();
        ipFwdBier clnt = new ipFwdBier(bierTunnel);
        clnt.addPeer(fwdCore, ups, 0, -1);
        clnt.updatePeers();
        clnt.sendPack(pckBin);
    }

    /**
     * send join messages
     */
    public void sendJoins() {
        for (int i = 0; i < fwdCore.groups.size(); i++) {
            ipFwdMcast grp = fwdCore.groups.get(i);
            if (grp.iface == null) {
                continue;
            }
            if (grp.iface.ifwNum != iface.ifwNum) {
                continue;
            }
            bits.sleep(interPackTime);
            sendJoin(grp, null, 1);
        }
        for (int i = 0; i < extra.size(); i++) {
            ipFwdMcast grp = extra.get(i);
            bits.sleep(interPackTime);
            sendJoin(grp, grp.upstream, 1);
        }
    }

    /**
     * list neighbors
     *
     * @param l list to append
     */
    public void getShNeighs(userFormat l) {
        for (int i = 0; i < neighs.size(); i++) {
            rtrPimNeigh nei = neighs.get(i);
            if (nei == null) {
                continue;
            }
            l.add(iface + "|" + nei.getShNeigh());
        }
    }

    /**
     * get number of neighbors
     *
     * @return number of neighbors
     */
    public int neighCount() {
        return neighs.size();
    }

    /**
     * set state
     *
     * @param iface interface
     * @param stat state
     */
    public void setState(ipFwdIface iface, state.states stat) {
    }

}

class rtrPimNeigh implements Comparable<rtrPimNeigh>, rtrBfdClnt {

    /**
     * parent
     */
    public final rtrPimIface lower;

    /**
     * address of peer
     */
    public final addrIP peer;

    /**
     * last hello
     */
    public long last;

    /**
     * hold time
     */
    public int hold;

    /**
     * dr priority
     */
    public int pri;

    /**
     * uptime
     */
    public long upTime;

    /**
     * create instance
     *
     * @param parent parent
     * @param adr address
     */
    public rtrPimNeigh(rtrPimIface parent, addrIP adr) {
        lower = parent;
        peer = adr.copyBytes();
    }

    public int compareTo(rtrPimNeigh o) {
        return peer.compareTo(o.peer);
    }

    /**
     * get neighbors line
     *
     * @return string
     */
    public String getShNeigh() {
        return peer + "|" + pri + "|" + bits.timePast(upTime);
    }

    public void bfdPeerDown() {
        last = 0;
        lower.purgeNeighs();
    }

}

class rtrPimIfaceHello implements Runnable {

    private final rtrPimIface lower;

    public rtrPimIfaceHello(rtrPimIface parent) {
        lower = parent;
    }

    public void start() {
        new Thread(this).start();
    }

    public void run() {
        try {
            for (;;) {
                if (lower.keepTimer != this) {
                    break;
                }
                lower.purgeNeighs();
                lower.sendHello();
                lower.sendJoins();
                bits.sleep(lower.helloInterval);
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}
