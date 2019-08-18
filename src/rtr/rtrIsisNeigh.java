package rtr;

import addr.addrClns;
import addr.addrIP;
import addr.addrIsis;
import addr.addrMac;
import ip.ipMpls;
import java.util.Comparator;
import java.util.Timer;
import java.util.TimerTask;
import pack.packHolder;
import tab.tabGen;
import tab.tabLabel;
import tab.tabLabelNtry;
import util.bits;
import util.counter;
import util.debugger;
import util.logger;
import util.typLenVal;

/**
 * isis neighbor
 *
 * @author matecsaba
 */
public class rtrIsisNeigh implements rtrBfdClnt, Comparator<rtrIsisNeigh> {

    /**
     * peer mac address
     */
    protected final addrMac ethAddr;

    /**
     * system id
     */
    protected addrIsis rtrID;

    /**
     * peer interface address
     */
    protected addrIP ifcAddr;

    /**
     * hold time
     */
    protected int holdTime;

    /**
     * dis priority
     */
    protected int rtrPri;

    /**
     * peer dis address knowledge
     */
    protected addrIsis peerDisA;

    /**
     * peer dis interface knowledge
     */
    protected int peerDisI;

    /**
     * peer circuit id
     */
    protected int peerCirc;

    /**
     * peer extended circuit id
     */
    protected int peerExtCirc;

    /**
     * peer adjacency state
     */
    protected int peerAdjState;

    /**
     * peer in other area
     */
    protected boolean foreignArea;

    /**
     * uptime
     */
    protected long upTime;

    /**
     * segment routing label
     */
    protected tabLabelNtry segrouLab;

    /**
     * the level
     */
    protected final rtrIsisLevel level;

    private int updPos;

    private final rtrIsis lower;

    private final rtrIsisIface iface;

    private long lastHeard;

    private Timer keepTimer;

    private final tabGen<rtrIsisLsp> advert;

    private final tabGen<rtrIsisLsp> request;

    /**
     * level2 hello pdu
     */
    public static final int msgTypL1hello = 0x0f;

    /**
     * level1 hello pdu
     */
    public static final int msgTypL2hello = 0x10;

    /**
     * p2p hello pdu
     */
    public static final int msgTypP2Phello = 0x11;

    /**
     * level1 link state pdu
     */
    public static final int msgTypL1lsp = 0x12;

    /**
     * level2 link state pdu
     */
    public static final int msgTypL2lsp = 0x14;

    /**
     * level1 complete sequence numbers pdu
     */
    public static final int msgTypL1csnp = 0x18;

    /**
     * level2 complete sequence numbers pdu
     */
    public static final int msgTypL2csnp = 0x19;

    /**
     * level1 partial sequence numbers pdu
     */
    public static final int msgTypL1psnp = 0x1a;

    /**
     * level2 partial sequence numbers pdu
     */
    public static final int msgTypL2psnp = 0x1b;

    /**
     * down
     */
    public final static int statDown = 2;

    /**
     * initing
     */
    public final static int statInit = 1;

    /**
     * up
     */
    public final static int statUp = 0;

    /**
     * convert message type to string
     *
     * @param i message type
     * @return string
     */
    public static String msgTyp2string(int i) {
        switch (i) {
            case msgTypL1hello:
                return "l1hello";
            case msgTypL2hello:
                return "l2hello";
            case msgTypP2Phello:
                return "p2pHello";
            case msgTypL1lsp:
                return "l1lsp";
            case msgTypL2lsp:
                return "l2lsp";
            case msgTypL1csnp:
                return "l1csnp";
            case msgTypL2csnp:
                return "l2csnp";
            case msgTypL1psnp:
                return "l1psnp";
            case msgTypL2psnp:
                return "l2psnp";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * convert message type to level
     *
     * @param i message type
     * @return level of message
     */
    public static int msgTyp2level(int i) {
        switch (i) {
            case msgTypL1csnp:
            case msgTypL1psnp:
            case msgTypL1lsp:
            case msgTypL1hello:
                return 1;
            case msgTypL2csnp:
            case msgTypL2psnp:
            case msgTypL2lsp:
            case msgTypL2hello:
                return 2;
            case msgTypP2Phello:
                return 3;
            default:
                return 0;
        }
    }

    /**
     * convert message type to header size
     *
     * @param i message type
     * @return header size
     */
    public static int msgTyp2headSiz(int i) {
        switch (i) {
            case msgTypP2Phello:
                return 20;
            case msgTypL1hello:
            case msgTypL2hello:
                return 27;
            case msgTypL1lsp:
            case msgTypL2lsp:
                return 27;
            case msgTypL1csnp:
            case msgTypL2csnp:
                return 33;
            case msgTypL1psnp:
            case msgTypL2psnp:
                return 17;
            default:
                return 0;
        }
    }

    /**
     * create one instance
     *
     * @param parent the isis protocol
     * @param lev the isis level
     * @param ifc the isis interface
     * @param adr address of peer
     */
    public rtrIsisNeigh(rtrIsis parent, rtrIsisLevel lev, rtrIsisIface ifc, addrMac adr) {
        lower = parent;
        iface = ifc;
        level = lev;
        ethAddr = adr.copyBytes();
        rtrID = new addrIsis();
        peerDisA = new addrIsis();
        peerAdjState = statDown;
        lastHeard = bits.getTime();
        advert = new tabGen<rtrIsisLsp>();
        request = new tabGen<rtrIsisLsp>();
    }

    public int compare(rtrIsisNeigh o1, rtrIsisNeigh o2) {
        if (o1.level.level < o2.level.level) {
            return -1;
        }
        if (o1.level.level > o2.level.level) {
            return +1;
        }
        return ethAddr.compare(o1.ethAddr, o2.ethAddr);
    }

    public String toString() {
        return "isis with l" + level.level + " " + ethAddr;
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
        rtrIsisNeighRetrans task = new rtrIsisNeighRetrans(this);
        keepTimer.schedule(task, 500, iface.retransTimer);
    }

    /**
     * start this neighbor
     */
    protected void startNow() {
        if (debugger.rtrIsisEvnt) {
            logger.debug("starting neighbor l" + level.level + " " + ethAddr);
        }
        restartTimer(false);
        upTime = bits.getTime();
    }

    /**
     * stow this neighbor
     */
    protected void stopNow() {
        logger.error("neighbor l" + level.level + " " + ifcAddr + " down");
        iface.iface.bfdDel(ifcAddr, this);
        peerAdjState = statDown;
        tabLabel.release(segrouLab, 15);
        level.schedWork(7);
        restartTimer(true);
        iface.neighs.del(this);
    }

    /**
     * stop work
     */
    public void bfdPeerDown() {
        stopNow();
    }

    /**
     * get handshake status
     *
     * @return status
     */
    protected int getMyHandshake() {
        switch (peerAdjState) {
            case statDown:
                return statInit;
            case statInit:
            case statUp:
                return statUp;
            default:
                return statDown;
        }
    }

    /**
     * received one packet
     *
     * @param pck packet to parse
     * @param typ type of packet
     */
    public void recvPack(packHolder pck, int typ) {
        if (debugger.rtrIsisTraf) {
            logger.debug("got " + msgTyp2string(typ) + " from l" + level.level + " " + ethAddr);
        }
        lastHeard = bits.getTime();
        switch (typ) {
            case msgTypL1hello:
            case msgTypL2hello:
                recvHelloLan(pck);
                break;
            case msgTypP2Phello:
                recvHelloP2p(pck);
                break;
            case msgTypL1lsp:
            case msgTypL2lsp:
                recvLsp(pck);
                break;
            case msgTypL1csnp:
            case msgTypL2csnp:
                recvCsnp(pck);
                break;
            case msgTypL1psnp:
            case msgTypL2psnp:
                recvPsnp(pck);
                break;
            default:
                logger.info("got invalid packet from l" + level.level + " " + ethAddr);
                iface.cntr.drop(pck, counter.reasons.badTyp);
                break;
        }
    }

    private void recvHelloP2p(packHolder pck) {
        int i = pck.getByte(0); // circuit type
        if ((i & level.level) == 0) {
            logger.info("got invalid level from l" + level.level + " " + ethAddr);
            iface.cntr.drop(pck, counter.reasons.badLen);
            return;
        }
        pck.getAddr(rtrID, 1); // system id
        holdTime = pck.msbGetW(7) * 1000; // hold time
        i = pck.msbGetW(9) - 8; // pdu length
        if (pck.dataSize() < i) {
            logger.info("got truncated from l" + level.level + " " + ethAddr);
            return;
        }
        pck.setDataSize(i);
        peerCirc = pck.getByte(11); // circuit id
        pck.getSkip(12);
        readHelloTlvs(pck);
    }

    private void recvHelloLan(packHolder pck) {
        int i = pck.getByte(0); // circuit type
        if ((i & level.level) == 0) {
            logger.info("got invalid level from l" + level.level + " " + ethAddr);
            iface.cntr.drop(pck, counter.reasons.badLen);
            return;
        }
        pck.getAddr(rtrID, 1); // system id
        holdTime = pck.msbGetW(7) * 1000; // hold time
        i = pck.msbGetW(9) - 8; // pdu length
        if (pck.dataSize() < i) {
            logger.info("got truncated from l" + level.level + " " + ethAddr);
            return;
        }
        pck.setDataSize(i);
        rtrPri = pck.getByte(11) & 0x7f; // dis priority
        pck.getAddr(peerDisA, 12); // dis address
        peerDisI = pck.getByte(18); // dis circuit
        pck.getSkip(19);
        readHelloTlvs(pck);
    }

    private void readHelloTlvs(packHolder pck) {
        typLenVal tlv = rtrIsis.getTlv();
        int oldAdjSt = peerAdjState;
        peerAdjState = statDown;
        foreignArea = false;
        peerExtCirc = 0;
        ifcAddr = new addrIP();
        boolean protoSupp = false;
        boolean areaAddr = false;
        boolean seenOwn = false;
        byte[] remAuth = null;
        for (;;) {
            if (tlv.getBytes(pck)) {
                break;
            }
            switch (tlv.valTyp) {
                case rtrIsisLsp.tlvHandshake:
                    peerAdjState = bits.getByte(tlv.valDat, 0); // adjacency state
                    peerExtCirc = bits.msbGetD(tlv.valDat, 1); // circuit id
                    break;
                case rtrIsisLsp.tlvProtSupp:
                    for (int i = 0; i < tlv.valSiz; i++) {
                        protoSupp |= bits.getByte(tlv.valDat, i) == lower.getNLPIDval();
                    }
                    break;
                case rtrIsisLsp.tlvAuthen:
                    remAuth = tlv.copyBytes();
                    break;
                case rtrIsisLsp.tlvAreaAddr:
                    for (int i = 0; i < tlv.valSiz;) {
                        addrClns cl = new addrClns();
                        cl.fromBuf(tlv.valDat, i);
                        i += cl.getAddrLen();
                        cl.fillUnunsed();
                        int o = cl.compare(cl, lower.areaID);
                        areaAddr |= o == 0;
                        foreignArea |= o != 0;
                    }
                    break;
                case rtrIsisLsp.tlvIpv4addr:
                case rtrIsisLsp.tlvIpv6addr:
                    lower.getAddrIface(tlv, ifcAddr);
                    break;
                case rtrIsisLsp.tlvLanNeigh:
                    for (int i = 0; i < tlv.valSiz;) {
                        addrMac mc = new addrMac();
                        mc.fromBuf(tlv.valDat, i);
                        i += addrMac.size;
                        seenOwn |= mc.compare(mc, iface.hwaddr) == 0;
                    }
                    break;
            }
        }
        if (!iface.netPnt2pnt) {
            if (seenOwn) {
                peerAdjState = statUp;
            } else {
                peerAdjState = statInit;
            }
        }
        int i = 1;
        byte[] locAuth = iface.getAuthData();
        if (locAuth == null) {
            if (remAuth == null) {
                i = 0;
            }
        } else if (remAuth != null) {
            if (locAuth.length == remAuth.length) {
                i = bits.byteComp(locAuth, 0, remAuth, 0, locAuth.length);
            }
        }
        if (i != 0) {
            peerAdjState = statDown;
            logger.info("got bad authentication from l" + level.level + " " + ethAddr);
            iface.cntr.drop(pck, counter.reasons.badKey);
            return;
        }
        if (!protoSupp) {
            peerAdjState = statDown;
            logger.info("got bad protocol from l" + level.level + " " + ethAddr);
            iface.cntr.drop(pck, counter.reasons.badProto);
            return;
        }
        if (ifcAddr.isFilled(0)) {
            peerAdjState = statDown;
            logger.info("got no address from l" + level.level + " " + ethAddr);
            iface.cntr.drop(pck, counter.reasons.badAddr);
            return;
        }
        if ((level.level == 1) && (!areaAddr)) {
            peerAdjState = statDown;
            logger.info("got bad area from l" + level.level + " " + ethAddr);
            iface.cntr.drop(pck, counter.reasons.badAddr);
            return;
        }
        if (oldAdjSt == peerAdjState) {
            return;
        }
        if (peerAdjState != statUp) {
            if (oldAdjSt != statUp) {
                return;
            }
            logger.error("neighbor l" + level.level + " " + ifcAddr + " forgot us");
            iface.iface.bfdDel(ifcAddr, this);
            tabLabel.release(segrouLab, 15);
            peerAdjState = statDown;
            level.schedWork(7);
            return;
        }
        logger.warn("neighbor l" + level.level + " " + ifcAddr + " up");
        advert.clear();
        if (lower.segrouLab != null) {
            segrouLab = tabLabel.allocate(15);
            segrouLab.setFwdMpls(15, lower.fwdCore, iface.iface, ifcAddr, tabLabel.int2labels(ipMpls.labelImp));
        }
        level.schedWork(7);
        if (iface.bfdTrigger) {
            iface.iface.bfdAdd(ifcAddr, this, "isis");
        }
    }

    private tabGen<rtrIsisLsp> readLspList(packHolder pck) {
        tabGen<rtrIsisLsp> l = new tabGen<rtrIsisLsp>();
        typLenVal tlv = rtrIsis.getTlv();
        for (;;) {
            if (tlv.getBytes(pck)) {
                break;
            }
            if (tlv.valTyp != rtrIsisLsp.tlvLspEntries) {
                continue;
            }
            packHolder p = new packHolder(true, true);
            p.putCopy(tlv.valDat, 0, 0, tlv.valSiz);
            p.putSkip(tlv.valSiz);
            p.merge2beg();
            for (;;) {
                if (p.dataSize() < 1) {
                    break;
                }
                rtrIsisLsp lsp = new rtrIsisLsp();
                int i = lsp.readSeq(p, 0);
                if (i < 0) {
                    break;
                }
                p.getSkip(i);
                if (debugger.rtrIsisTraf) {
                    logger.debug("lsp " + lsp);
                }
                l.add(lsp);
            }
        }
        return l;
    }

    private void recvLsp(packHolder pck) {
        if (peerAdjState != statUp) {
            return;
        }
        rtrIsisLsp lsp = new rtrIsisLsp();
        if (lsp.readData(pck, 0) < 0) {
            logger.info("got bad lsp from l" + level.level + " " + ifcAddr);
            iface.cntr.drop(pck, counter.reasons.badSum);
            return;
        }
        if (debugger.rtrIsisTraf) {
            logger.debug("lsp " + lsp);
        }
        request.del(lsp);
        if (iface.shouldIanswer(level.level, rtrID)) {
            iface.sendPsnpPack(lsp, level.level);
        }
        rtrIsisLsp old = level.lsps.add(lsp);
        if (old == null) {
            advert.put(lsp.copyBytes(false));
            if (lower.routerID.compare(lower.routerID, lsp.srcID) == 0) {
                level.schedWork(1);
            }
            level.schedWork(6);
            return;
        }
        if (!old.otherNewer(lsp)) {
            if (!lsp.otherNewer(old)) {
                advert.put(lsp.copyBytes(false));
                return;
            }
            advert.del(lsp);
            return;
        }
        level.lsps.put(lsp);
        advert.put(lsp.copyBytes(false));
        if (lower.routerID.compare(lower.routerID, lsp.srcID) == 0) {
            level.schedWork(1);
        }
        level.schedWork(6);
        doRetrans();
    }

    private void recvPsnp(packHolder pck) {
        if (peerAdjState != statUp) {
            return;
        }
        if (!iface.shouldIanswer(level.level, rtrID)) {
            return;
        }
        int i = pck.msbGetW(0) - 8; // pdu length
        if (pck.dataSize() < i) {
            logger.info("got truncated from l" + level.level + " " + ethAddr);
            return;
        }
        pck.setDataSize(i);
        // pck.getAddr(adr, 2); // source id
        // int circ = pck.getByte(8);
        pck.getSkip(9);
        tabGen<rtrIsisLsp> l1 = readLspList(pck);
        tabGen<rtrIsisLsp> l2 = new tabGen<rtrIsisLsp>();
        for (i = 0; i < l1.size(); i++) {
            rtrIsisLsp ntry = l1.get(i);
            ntry = level.lsps.find(ntry);
            if (ntry == null) {
                continue;
            }
            l2.add(ntry);
        }
        doLspList(l1, l2);
        doRetrans();
    }

    private void recvCsnp(packHolder pck) {
        if (peerAdjState != statUp) {
            return;
        }
        if (!iface.shouldIanswer(level.level, rtrID)) {
            return;
        }
        int i = pck.msbGetW(0) - 8; // pdu length
        if (pck.dataSize() < i) {
            logger.info("got truncated from l" + level.level + " " + ethAddr);
            return;
        }
        pck.setDataSize(i);
        // pck.getAddr(adr, 2); // source id
        // int circ = pck.getByte(8);
        rtrIsisLsp frst = new rtrIsisLsp();
        rtrIsisLsp last = new rtrIsisLsp();
        frst.readId(pck, 9); // first lsp
        last.readId(pck, 17); // last lsp
        pck.getSkip(25);
        tabGen<rtrIsisLsp> l1 = readLspList(pck);
        tabGen<rtrIsisLsp> l2 = new tabGen<rtrIsisLsp>();
        for (i = 0; i < level.lsps.size(); i++) {
            rtrIsisLsp ntry = level.lsps.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.compare(ntry, frst) < 0) {
                continue;
            }
            if (ntry.compare(ntry, last) > 0) {
                continue;
            }
            l2.add(ntry);
        }
        doLspList(l1, l2);
        doRetrans();
    }

    private void doLspList(tabGen<rtrIsisLsp> rem, tabGen<rtrIsisLsp> loc) {
        for (int i = 0; i < rem.size(); i++) {
            rtrIsisLsp lsp = rem.get(i);
            if (lsp == null) {
                continue;
            }
            rtrIsisLsp old = level.lsps.find(lsp);
            if (old == null) {
                if (lsp.getTimeRemain(true) < 1) {
                    continue;
                }
                request.add(lsp.copyBytes(false));
                continue;
            }
            if (old.otherNewer(lsp)) {
                request.add(lsp.copyBytes(false));
            } else {
                request.del(lsp);
            }
        }
        for (int i = 0; i < loc.size(); i++) {
            rtrIsisLsp lsp = loc.get(i);
            if (lsp == null) {
                continue;
            }
            rtrIsisLsp old = rem.find(lsp);
            if (old == null) {
                if (lsp.getTimeRemain(true) < 1) {
                    continue;
                }
                advert.del(lsp);
                continue;
            }
            if (old.otherNewer(lsp)) {
                advert.del(lsp);
            } else {
                advert.add(lsp.copyBytes(false));
            }
        }
    }

    /**
     * do retransmit work
     */
    protected synchronized void doRetrans() {
        if ((bits.getTime() - lastHeard) > holdTime) {
            stopNow();
            level.schedWork(7);
            return;
        }
        if (peerAdjState != statUp) {
            return;
        }
        int i = request.size();
        if (i > 0) {
            i = bits.random(0, i);
            rtrIsisLsp lsp = request.get(i);
            if (lsp != null) {
                lsp.sequence = 0;
                iface.sendPsnpPack(lsp, level.level);
            }
        }
        for (i = advert.size(); i >= 0; i--) {
            rtrIsisLsp lsp = advert.get(i);
            if (lsp == null) {
                continue;
            }
            rtrIsisLsp old = level.lsps.find(lsp);
            if (old == null) {
                advert.del(lsp);
                continue;
            }
            if (!lsp.otherNewer(old)) {
                continue;
            }
            advert.del(old);
        }
        for (i = 0; i < level.lsps.size(); i++) {
            updPos = (updPos + 1) % level.lsps.size();
            rtrIsisLsp lsp = level.lsps.get(updPos);
            if (lsp == null) {
                continue;
            }
            if (advert.find(lsp) != null) {
                continue;
            }
            iface.sendLspPack(lsp, level.level);
            if (debugger.rtrIsisTraf) {
                logger.debug("lsp " + lsp);
            }
            if (iface.amIdis(level.level)) {
                advert.add(lsp.copyBytes(false));
            }
            break;
        }
    }

}

class rtrIsisNeighRetrans extends TimerTask {

    private rtrIsisNeigh lower;

    public rtrIsisNeighRetrans(rtrIsisNeigh parent) {
        lower = parent;
    }

    public void run() {
        try {
            lower.doRetrans();
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}
