package rtr;

import addr.addrIP;
import java.util.Comparator;
import java.util.Timer;
import java.util.TimerTask;

import pack.packHolder;
import tab.tabGen;
import util.bits;
import util.counter;
import util.debugger;
import util.logger;
import addr.addrIPv4;
import addr.addrIPv6;
import ip.ipMpls;
import tab.tabLabel;
import tab.tabLabelNtry;

/**
 * ospfv3 neighbor
 *
 * @author matecsaba
 */
public class rtrOspf6neigh implements rtrBfdClnt, Comparator<rtrOspf6neigh> {

    private final rtrOspf6 lower;

    private Timer keepTimer;

    private int updPos;

    private int ddSeq;

    private int ddPos;

    private boolean ddMst;

    private boolean ddMorR;

    private boolean ddMorL;

    private final tabGen<rtrOspf6lsa> advert;

    private final tabGen<rtrOspf6lsa> request;

    private long lastHeard;

    private int state;

    private final static int stDown = 1;

    private final static int stInit = 2;

    private final static int stXchg = 3;

    private final static int stFull = 4;

    /**
     * set true for static neighbors
     */
    protected boolean statNeigh = false;

    /**
     * area this neighbor belongs
     */
    protected final rtrOspf6area area;

    /**
     * interface this neighbor belongs
     */
    protected final rtrOspf6iface iface;

    /**
     * peer address
     */
    protected final addrIPv6 peer;

    /**
     * peer router id
     */
    protected addrIPv4 rtrID;

    /**
     * peer priority
     */
    protected int rtrPri;

    /**
     * peer interface id
     */
    protected int rtrInt;

    /**
     * dead interval
     */
    protected int deadInt;

    /**
     * peer dr knowledge
     */
    protected addrIPv4 peerDR;

    /**
     * peer bdr knowledge
     */
    protected addrIPv4 peerBDR;

    /**
     * seen myself on neighbor list
     */
    protected boolean seenMyself;

    /**
     * uptime
     */
    protected long upTime;

    /**
     * segment routing label
     */
    protected tabLabelNtry segrouLab;

    private final static int lsaPerDescr = 16;

    /**
     * hello packet
     */
    public final static int msgTypHello = 1;

    /**
     * database description
     */
    public final static int msgTypDBdsc = 2;

    /**
     * link state request
     */
    public final static int msgTypLSreq = 3;

    /**
     * link state update
     */
    public final static int msgTypLSupd = 4;

    /**
     * link state ack
     */
    public final static int msgTypLSack = 5;

    /**
     * convert message type to string
     *
     * @param i message type
     * @return string
     */
    public static String msgTyp2string(int i) {
        switch (i) {
            case msgTypHello:
                return "hello";
            case msgTypDBdsc:
                return "dbDescription";
            case msgTypLSreq:
                return "lsRequest";
            case msgTypLSupd:
                return "lsUpdate";
            case msgTypLSack:
                return "lsAck";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * create one instance
     *
     * @param parent the ospf protocol
     * @param ifc the interface to work on
     * @param adr interface address of peer
     */
    public rtrOspf6neigh(rtrOspf6 parent, rtrOspf6iface ifc, addrIPv6 adr) {
        lower = parent;
        iface = ifc;
        peer = adr.copyBytes();
        area = iface.area;
        rtrID = new addrIPv4();
        peerDR = new addrIPv4();
        peerBDR = new addrIPv4();
        advert = new tabGen<rtrOspf6lsa>();
        request = new tabGen<rtrOspf6lsa>();
        deadInt = iface.deadTimer;
        lastHeard = bits.getTime();
        state = stDown;
    }

    public String toString() {
        return "ospf with " + peer;
    }

    public int compare(rtrOspf6neigh o1, rtrOspf6neigh o2) {
        return o1.peer.compare(o1.peer, o2.peer);
    }

    /**
     * check if fully loaded
     *
     * @return true if yes, false if not
     */
    public boolean isFull() {
        return state == stFull;
    }

    /**
     * check if other is better dr
     *
     * @param other other neighbor
     * @return true if better, false if not
     */
    protected boolean otherBetterDR(rtrOspf6neigh other) {
        if (other.rtrPri > rtrPri) {
            return true;
        }
        if (other.rtrPri < rtrPri) {
            return false;
        }
        return rtrID.compare(other.rtrID, rtrID) > 0;
    }

    /**
     * send one packet on this neighbor
     *
     * @param pck packet to send
     * @param typ type of packet
     */
    protected void packSend(packHolder pck, int typ) {
        pck.IPdf = false;
        pck.IPttl = 255;
        pck.IPtos = 0;
        pck.IPprt = rtrOspf6.protoNum;
        pck.IPsrc.setAddr(iface.iface.addr);
        pck.IPtrg.fromIPv6addr(peer);
        iface.mkPackHead(pck, typ);
        lower.fwdCore.protoPack(iface.iface, pck);
    }

    /**
     * received one packet
     *
     * @param pck packet received
     */
    protected void recvPack(packHolder pck) {
        if (pck.dataSize() < rtrOspf6.sizeHead) {
            logger.info("got too small from " + peer);
            iface.cntr.drop(pck, counter.reasons.tooSmall);
            return;
        }
        int i = pck.getByte(0); // version
        if (i != rtrOspf6.verNum) {
            logger.info("got bad version from " + peer);
            iface.cntr.drop(pck, counter.reasons.badVer);
            return;
        }
        int typ = pck.getByte(1); // packet type
        i = pck.msbGetW(2); // size
        if (i > pck.dataSize()) {
            logger.info("got truncated from " + peer);
            iface.cntr.drop(pck, counter.reasons.tooSmall);
            return;
        }
        if (i < rtrOspf6.sizeHead) {
            logger.info("got too small from " + peer);
            iface.cntr.drop(pck, counter.reasons.tooSmall);
            return;
        }
        pck.setDataSize(i);
        addrIPv4 adr = new addrIPv4();
        pck.getAddr(adr, 4); // neighbor router id
        if (adr.compare(adr, rtrID) != 0) {
            rtrID = adr.copyBytes();
            state = stDown;
        }
        i = pck.msbGetD(8); // area id
        if (i != area.area) {
            logger.info("got invalid area from " + peer);
            iface.cntr.drop(pck, counter.reasons.badID);
            return;
        }
        // int sum = pck.msbGetW(12); // checksum
        i = pck.pseudoIPsum(pck.dataSize());
        if (pck.getIPsum(0, pck.dataSize(), i) != 0xffff) {
            logger.info("got bad checksum from " + peer);
            iface.cntr.drop(pck, counter.reasons.badSum);
            return;
        }
        i = pck.getByte(14); // instance id
        if (i != iface.instance) {
            logger.info("got invalid instance from " + peer);
            iface.cntr.drop(pck, counter.reasons.badCod);
            return;
        }
        pck.getSkip(rtrOspf6.sizeHead);
        if (debugger.rtrOspf6traf) {
            logger.debug("got " + msgTyp2string(typ) + " from " + peer);
        }
        lastHeard = bits.getTime();
        switch (typ) {
            case msgTypHello:
                recvHello(pck);
                break;
            case msgTypLSack:
                recvAcknow(pck);
                break;
            case msgTypLSupd:
                recvUpdate(pck);
                break;
            case msgTypLSreq:
                recvRequest(pck);
                break;
            case msgTypDBdsc:
                recvDescr(pck);
                break;
            default:
                logger.info("got invalid packet from " + peer);
                iface.cntr.drop(pck, counter.reasons.badTyp);
                break;
        }
    }

    private void recvHello(packHolder pck) {
        rtrInt = pck.msbGetD(0); // interface id
        rtrPri = pck.getByte(4); // dr priority
        // int capa = pck.msbGetD(4) & 0xffffff; // optional capabilities
        // int helloInt = pck.msbGetW(8); // hello interval
        deadInt = pck.msbGetW(10) * 1000; // dead interval
        pck.getAddr(peerDR, 12); // dr address
        pck.getAddr(peerBDR, 16); // bdr address
        pck.getSkip(20);
        seenMyself = false;
        for (;;) {
            if (pck.dataSize() < addrIPv4.size) {
                break;
            }
            addrIPv4 adr = new addrIPv4();
            pck.getAddr(adr, 0); // neighbor
            pck.getSkip(addrIPv4.size);
            if (adr.compare(adr, lower.routerID) == 0) {
                seenMyself = true;
                break;
            }
        }
        if (!seenMyself) {
            if (state == stDown) {
                iface.sendHello();
                return;
            }
            logger.error("neighbor " + peer + " forgot us");
            iface.iface.bfdDel(peer, this);
            tabLabel.release(segrouLab, 17);
            state = stDown;
            area.schedWork(7);
            return;
        }
        if (state > stDown) {
            return;
        }
        startXchg();
        doRetrans();
    }

    private void recvDescr(packHolder pck) {
        // int capa = pck.msbGetD(0); // optional capabilities
        // int mtu = pck.msbGetW(4); // interface mtu
        int flags = pck.msbGetW(6); // db flags
        int seq = pck.msbGetD(8); // sequence number
        pck.getSkip(12);
        ddMorR = (flags & rtrOspf6iface.dscrMore) != 0;
        boolean remInit = (flags & rtrOspf6iface.dscrInit) != 0;
        boolean remMstr = (flags & rtrOspf6iface.dscrMstr) != 0;
        switch (state) {
            case stInit:
                if (remMstr == ddMst) {
                    return;
                }
                if (ddMst) {
                    if (remInit) {
                        return;
                    }
                    if (seq != ddSeq) {
                        return;
                    }
                    ddSeq++;
                } else {
                    if (!remInit) {
                        return;
                    }
                    ddSeq = seq;
                }
                doDescrList(pck);
                state = stXchg;
                doRetrans();
                return;
            case stXchg:
                if (remInit) {
                    startXchg();
                    doRetrans();
                    return;
                }
                if (remMstr == ddMst) {
                    return;
                }
                doDescrList(pck);
                if (ddMst) {
                    if (seq != ddSeq) {
                        return;
                    }
                } else if (seq != (ddSeq + 1)) {
                    return;
                }
                ddSeq++;
                ddPos += lsaPerDescr;
                doRetrans();
                return;
            case stFull:
                if (remInit) {
                    startXchg();
                    doRetrans();
                    return;
                }
                if (remMstr == ddMst) {
                    return;
                }
                doDescrList(pck);
                pck.clear();
                int i;
                if (ddMst) {
                    i = rtrOspf6iface.dscrMstr;
                } else {
                    i = 0;
                }
                iface.mkDescrPack(pck, i, ddSeq);
                packSend(pck, rtrOspf6neigh.msgTypDBdsc);
                return;
            default:
                return;
        }
    }

    private void doDescrList(packHolder pck) {
        for (;;) {
            if (pck.dataSize() < 1) {
                break;
            }
            rtrOspf6lsa lsa = new rtrOspf6lsa();
            int i = lsa.readData(pck, 0, false);
            if (i < 0) {
                logger.info("got bad lsa from " + peer);
                break;
            }
            pck.getSkip(i);
            if (debugger.rtrOspf6traf) {
                logger.debug("lsa " + lsa);
            }
            rtrOspf6lsa old = area.lsas.find(lsa);
            if (old == null) {
                request.add(lsa);
                continue;
            }
            if (!old.otherNewer(lsa)) {
                continue;
            }
            request.add(lsa);
        }
    }

    private void recvUpdate(packHolder pck) {
        if (state < stXchg) {
            return;
        }
        if (pck.dataSize() < 4) {
            return;
        }
        int m = pck.msbGetD(0);
        pck.getSkip(4);
        tabGen<rtrOspf6lsa> lst = new tabGen<rtrOspf6lsa>();
        boolean seenOwn = false;
        for (int o = 0; o < m; o++) {
            rtrOspf6lsa lsa = new rtrOspf6lsa();
            int i = lsa.readData(pck, 0, true);
            if (i < 0) {
                logger.info("got bad lsa from " + peer);
                break;
            }
            pck.getSkip(i);
            if (debugger.rtrOspf6traf) {
                logger.debug("lsa " + lsa);
            }
            lst.put(lsa);
            request.del(lsa);
            rtrOspf6lsa old = area.lsas.add(lsa);
            if (old == null) {
                advert.put(lsa.copyBytes(false));
                seenOwn |= lower.routerID.compare(lower.routerID, lsa.rtrID) == 0;
                continue;
            }
            if (!old.otherNewer(lsa)) {
                if (!lsa.otherNewer(old)) {
                    advert.put(lsa.copyBytes(false));
                    continue;
                }
                advert.del(lsa);
                continue;
            }
            area.lsas.put(lsa);
            advert.put(lsa.copyBytes(false));
            seenOwn |= lower.routerID.compare(lower.routerID, lsa.rtrID) == 0;
        }
        if (lst.size() < 1) {
            doRetrans();
            return;
        }
        if (seenOwn) {
            area.schedWork(1);
        }
        if (request.size() < 1) {
            area.schedWork(6);
        }
        pck.clear();
        for (int o = 0; o < lst.size(); o++) {
            rtrOspf6lsa lsa = lst.get(o);
            if (lsa == null) {
                continue;
            }
            int i = lsa.writeData(pck, 0, false);
            pck.putSkip(i);
        }
        packSend(pck, rtrOspf6neigh.msgTypLSack);
        doRetrans();
    }

    private void recvRequest(packHolder pck) {
        if (state < stXchg) {
            return;
        }
        tabGen<rtrOspf6lsa> lst = new tabGen<rtrOspf6lsa>();
        for (;;) {
            rtrOspf6lsa lsa = new rtrOspf6lsa();
            int i = lsa.readReq(pck, 0);
            if (i < 0) {
                break;
            }
            pck.getSkip(i);
            if (debugger.rtrOspf6traf) {
                logger.debug("lsa " + lsa);
            }
            lsa = area.lsas.find(lsa);
            if (lsa == null) {
                startXchg();
                continue;
            }
            lst.put(lsa);
        }
        if (lst.size() < 1) {
            return;
        }
        for (int i = 0; i < lst.size(); i++) {
            rtrOspf6lsa lsa = lst.get(i);
            if (lsa == null) {
                continue;
            }
            pck.clear();
            iface.mkLSupdate(pck, lsa);
            packSend(pck, rtrOspf6neigh.msgTypLSupd);
        }
    }

    private void recvAcknow(packHolder pck) {
        if (state < stXchg) {
            return;
        }
        for (;;) {
            if (pck.dataSize() < 1) {
                break;
            }
            rtrOspf6lsa lsa = new rtrOspf6lsa();
            int i = lsa.readData(pck, 0, false);
            if (i < 0) {
                logger.info("got bad lsa from " + peer);
                break;
            }
            pck.getSkip(i);
            if (debugger.rtrOspf6traf) {
                logger.debug("lsa " + lsa);
            }
            advert.put(lsa);
        }
        doRetrans();
    }

    /**
     * setup timer thread
     *
     * @param shutdown set true to shut down
     */
    protected void restartTimer(boolean shutdown) {
        try {
            keepTimer.cancel();
        } catch (Exception e) {
        }
        keepTimer = null;
        if (shutdown) {
            return;
        }
        keepTimer = new Timer();
        rtrOspf6neighRetrans task = new rtrOspf6neighRetrans(this);
        keepTimer.schedule(task, 500, iface.retransTimer);
    }

    /**
     * start this neighbor
     */
    protected void startNow() {
        if (debugger.rtrOspf6evnt) {
            logger.debug("starting neighbor " + peer);
        }
        upTime = bits.getTime();
        restartTimer(false);
    }

    /**
     * stow this neighbor
     */
    protected void stopNow() {
        logger.error("neighbor " + peer + " down");
        iface.iface.bfdDel(peer, this);
        tabLabel.release(segrouLab, 17);
        state = stDown;
        seenMyself = false;
        area.schedWork(7);
        if (!statNeigh) {
            restartTimer(true);
            iface.neighs.del(this);
            return;
        }
        state = stDown;
        rtrID = new addrIPv4();
        peerBDR = new addrIPv4();
        peerDR = new addrIPv4();
        lastHeard = bits.getTime();
    }

    public void bfdPeerDown() {
        stopNow();
    }

    private void startXchg() {
        if (!iface.shouldIpeer(rtrID)) {
            return;
        }
        state = stInit;
        ddSeq = bits.randomW();
        ddMst = rtrID.compare(lower.routerID, rtrID) > 0;
        if (debugger.rtrOspf6evnt) {
            logger.debug("starting exchange with " + peer + ", seq=" + ddSeq + " master=" + ddMst);
        }
        advert.clear();
        request.clear();
        ddPos = 0;
        ddMorR = true;
        ddMorL = true;
        for (int i = 0; i < area.lsas.size(); i++) {
            rtrOspf6lsa lsa = area.lsas.get(i);
            if (lsa == null) {
                continue;
            }
            advert.add(lsa.copyBytes(false));
        }
    }

    /**
     * test if no retransmission needed
     *
     * @return true if not needed
     */
    protected boolean noTimerNeeded() {
        return (state == stXchg) && (!ddMst);
    }

    /**
     * check timeout
     */
    protected void checkTimeout() {
        if ((bits.getTime() - lastHeard) > deadInt) {
            stopNow();
            area.schedWork(7);
            return;
        }
    }

    /**
     * do retransmit work
     */
    protected synchronized void doRetrans() {
        if (!iface.shouldIpeer(rtrID)) {
            state = stDown;
            return;
        }
        switch (state) {
            case stDown:
                break;
            case stInit:
                doRetInit();
                break;
            case stXchg:
                doRetXchg();
                break;
            case stFull:
                doRetFull();
                break;
            default:
                state = stDown;
                break;
        }
    }

    private void doRetInit() {
        packHolder pck = new packHolder(true, true);
        iface.mkDescrPack(pck, rtrOspf6iface.dscrInit | rtrOspf6iface.dscrMore | rtrOspf6iface.dscrMstr, ddSeq);
        packSend(pck, rtrOspf6neigh.msgTypDBdsc);
    }

    private void doRetXchg() {
        packHolder pck = new packHolder(true, true);
        for (int i = 0; i < lsaPerDescr; i++) {
            rtrOspf6lsa lsa = advert.get(ddPos + i);
            if (lsa == null) {
                continue;
            }
            lsa = area.lsas.find(lsa);
            if (lsa == null) {
                continue;
            }
            if (debugger.rtrOspf6traf) {
                logger.debug("lsa " + lsa);
            }
            int o = lsa.writeData(pck, 0, false);
            pck.putSkip(o);
        }
        int i;
        if (ddMst) {
            i = rtrOspf6iface.dscrMstr;
        } else {
            i = 0;
        }
        if (!(ddMorL | ddMorR)) {
            logger.warn("neighbor " + peer + " up");
            if (lower.segrouLab != null) {
                addrIP per = new addrIP();
                per.fromIPv6addr(peer);
                segrouLab = tabLabel.allocate(17);
                segrouLab.setFwdMpls(17, lower.fwdCore, iface.iface, per, tabLabel.int2labels(ipMpls.labelImp));
            }
            state = stFull;
            area.schedWork(7);
            if (iface.bfdTrigger) {
                iface.iface.bfdAdd(peer, this, "ospf");
            }
            return;
        }
        ddMorL = false;
        if (ddPos < advert.size()) {
            i |= rtrOspf6iface.dscrMore;
            ddMorL = true;
        }
        iface.mkDescrPack(pck, i, ddSeq);
        packSend(pck, rtrOspf6neigh.msgTypDBdsc);
    }

    private void doRetFull() {
        packHolder pck = new packHolder(true, true);
        int i = request.size();
        if (i > 0) {
            i = bits.random(0, i);
            rtrOspf6lsa lsa = request.get(i);
            if (lsa == null) {
                return;
            }
            i = lsa.writeReq(pck, 0);
            pck.putSkip(i);
            packSend(pck, rtrOspf6neigh.msgTypLSreq);
            if (debugger.rtrOspf6traf) {
                logger.debug("lsa " + lsa);
            }
            return;
        }
        for (i = advert.size(); i >= 0; i--) {
            rtrOspf6lsa lsa = advert.get(i);
            if (lsa == null) {
                continue;
            }
            rtrOspf6lsa old = area.lsas.find(lsa);
            if (old == null) {
                advert.del(lsa);
                continue;
            }
            if (!lsa.otherNewer(old)) {
                continue;
            }
            advert.del(old);
        }
        for (i = 0; i < area.lsas.size(); i++) {
            updPos = (updPos + 1) % area.lsas.size();
            rtrOspf6lsa lsa = area.lsas.get(updPos);
            if (lsa == null) {
                continue;
            }
            if (advert.find(lsa) != null) {
                continue;
            }
            pck.clear();
            iface.mkLSupdate(pck, lsa);
            packSend(pck, rtrOspf6neigh.msgTypLSupd);
            if (debugger.rtrOspf6traf) {
                logger.debug("lsa " + lsa);
            }
            break;
        }
    }

}

class rtrOspf6neighRetrans extends TimerTask {

    private rtrOspf6neigh lower;

    public rtrOspf6neighRetrans(rtrOspf6neigh parent) {
        lower = parent;
    }

    public void run() {
        try {
            lower.checkTimeout();
            if (lower.noTimerNeeded()) {
                return;
            }
            lower.doRetrans();
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}
