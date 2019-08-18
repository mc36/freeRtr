package rtr;

import addr.addrIP;
import addr.addrPrefix;
import clnt.clntMplsBier;
import ip.ipFwd;
import ip.ipFwdIface;
import ip.ipFwdMcast;
import ip.ipPrt;
import java.util.Timer;
import java.util.TimerTask;
import pack.packHolder;
import pack.packPim;
import pack.packPimGrp;
import tab.tabGen;
import tab.tabRouteEntry;
import user.userFormat;
import util.bits;
import util.counter;
import util.debugger;
import util.logger;
import util.state;

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

    private int generationId;

    private ipFwd fwdCore;

    private ipFwdIface iface;

    private counter cntr = new counter();

    private tabGen<rtrPimNeigh> neighs = new tabGen<rtrPimNeigh>();

    private Timer keepTimer;

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
        try {
            keepTimer.cancel();
        } catch (Exception e) {
        }
        keepTimer = null;
        if (shutdown) {
            return;
        }
        if (helloInterval < 1) {
            return;
        }
        keepTimer = new Timer();
        rtrPimIfaceHello task = new rtrPimIfaceHello(this);
        keepTimer.schedule(task, 500, helloInterval);
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
        rtrPimNeigh nei = new rtrPimNeigh();
        switch (pckPim.typ) {
            case packPim.typJoin:
                if (pckPim.upstream.compare(pckPim.upstream, iface.addr) != 0) {
                    break;
                }
                int tim = pckPim.valHoldTime * 1000;
                for (int o = 0; o < pckPim.groups.size(); o++) {
                    packPimGrp grp = pckPim.groups.get(o);
                    if (!grp.group.wildcard.isFilled(0)) {
                        continue;
                    }
                    for (int i = 0; i < grp.joins.size(); i++) {
                        addrPrefix<addrIP> src = grp.joins.get(i);
                        if (!src.wildcard.isFilled(0)) {
                            continue;
                        }
                        if (bierTunnel > 0) {
                            fwdCore.mcastAddFloodBier(grp.group.network, src.network, pckBin.IPsrc, bierTunnel, iface.lower.getEthtyp(), tim);
                        } else {
                            fwdCore.mcastAddFloodIfc(grp.group.network, src.network, iface, tim);
                        }
                    }
                    for (int i = 0; i < grp.prunes.size(); i++) {
                        addrPrefix<addrIP> src = grp.prunes.get(i);
                        if (!src.wildcard.isFilled(0)) {
                            continue;
                        }
                        if (bierTunnel > 0) {
                            fwdCore.mcastDelFloodBier(grp.group.network, src.network, pckBin.IPsrc);
                        } else {
                            fwdCore.mcastDelFloodIfc(grp.group.network, src.network, iface);
                        }
                    }
                }
                break;
            case packPim.typHello:
                nei.peer = pckBin.IPsrc.copyBytes();
                rtrPimNeigh old = neighs.add(nei);
                if (old != null) {
                    nei = old;
                } else {
                    nei.upTime = bits.getTime();
                    logger.warn("neighbor " + nei.peer + " up");
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
    public void purgeNeighs() {
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
        fwdCore.protoPack(iface, pckBin);
    }

    /**
     * send one join
     *
     * @param grp group to join
     * @param need 1=join, 0=prune
     */
    public void sendJoin(ipFwdMcast grp, int need) {
        if (!allowTx) {
            return;
        }
        addrIP ups;
        if (bierTunnel > 0) {
            ups = grp.source;
            tabRouteEntry<addrIP> rou = fwdCore.actualM.route(ups);
            if (rou == null) {
                return;
            }
            if (rou.oldHop != null) {
                ups = rou.oldHop;
            }
        } else {
            ups = grp.upstream;
        }
        packHolder pckBin = new packHolder(true, true);
        packPim pckPim = new packPim();
        pckPim.fillJoin(ups, grp.group, grp.source, helloInterval * need);
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
            fwdCore.protoPack(iface, pckBin);
            return;
        }
        fwdCore.createIPheader(pckBin);
        pckBin.msbPutW(0, iface.lower.getEthtyp());
        pckBin.putSkip(2);
        pckBin.merge2beg();
        clntMplsBier clnt = new clntMplsBier();
        clnt.fwdCor = fwdCore;
        clnt.srcId = bierTunnel;
        clnt.addTarget(ups);
        clnt.workStart();
        clnt.wait4setup(5000);
        clnt.sendPack(pckBin);
        clnt.workStop();
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
            sendJoin(grp, 1);
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

class rtrPimIfaceHello extends TimerTask {

    private final rtrPimIface lower;

    public rtrPimIfaceHello(rtrPimIface parent) {
        lower = parent;
    }

    public void run() {
        try {
            lower.purgeNeighs();
            lower.sendHello();
            lower.sendJoins();
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}
