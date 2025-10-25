package org.freertr.rtr;

import org.freertr.addr.addrClns;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIsis;
import org.freertr.addr.addrMac;
import org.freertr.clnt.clntEcho;
import org.freertr.clnt.clntPing;
import org.freertr.clnt.clntTwamp;
import org.freertr.ip.ipMpls;
import org.freertr.pack.packHolder;
import org.freertr.tab.tabAverage;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabLabel;
import org.freertr.tab.tabLabelEntry;
import org.freertr.util.bits;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.notifier;
import org.freertr.enc.encTlv;
import org.freertr.prt.prtIsoip;
import org.freertr.sec.secInfoCls;
import org.freertr.sec.secInfoWrk;

/**
 * isis neighbor
 *
 * @author matecsaba
 */
public class rtrIsisNeigh implements Runnable, rtrBfdClnt, Comparable<rtrIsisNeigh> {

    /**
     * ipinfo result
     */
    public secInfoWrk ipInfoRes;

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
     * peer interface other address
     */
    protected addrIP ofcAddr;

    /**
     * time echo sent
     */
    protected long echoTime;

    /**
     * calculated echo
     */
    protected tabAverage echoCalc = new tabAverage(1, 1);

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
     * notifier
     */
    protected notifier notif = new notifier();

    /**
     * segment routing label
     */
    protected tabLabelEntry segrouLab;

    /**
     * segment routing other label
     */
    protected tabLabelEntry segrouOth;

    /**
     * the level
     */
    protected final rtrIsisLevel level;

    private int updPos;

    private final rtrIsis lower;

    private final rtrIsisIface iface;

    private long lastHeard;

    private boolean need2run = true;

    private final tabGen<rtrIsisLsp> advert;

    private final tabGen<rtrIsisLsp> request;

    private final tabGen<rtrIsisLsp> pending;

    /**
     * level2 hello pdu
     */
    public final static int msgTypL1hello = 0x0f;

    /**
     * level1 hello pdu
     */
    public final static int msgTypL2hello = 0x10;

    /**
     * p2p hello pdu
     */
    public final static int msgTypP2Phello = 0x11;

    /**
     * level1 link state pdu
     */
    public final static int msgTypL1lsp = 0x12;

    /**
     * level2 link state pdu
     */
    public final static int msgTypL2lsp = 0x14;

    /**
     * level1 complete sequence numbers pdu
     */
    public final static int msgTypL1csnp = 0x18;

    /**
     * level2 complete sequence numbers pdu
     */
    public final static int msgTypL2csnp = 0x19;

    /**
     * level1 partial sequence numbers pdu
     */
    public final static int msgTypL1psnp = 0x1a;

    /**
     * level2 partial sequence numbers pdu
     */
    public final static int msgTypL2psnp = 0x1b;

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
     * convert status to string
     *
     * @return string
     */
    public String status2string() {
        if (!iface.shouldIanswer(level.level, rtrID)) {
            return "unneeded";
        }
        switch (peerAdjState) {
            case statDown:
                return "down";
            case statInit:
                return "init";
            case statUp:
                return "up";
            default:
                return "unknown=" + peerAdjState;
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
        ifcAddr = new addrIP();
        ofcAddr = new addrIP();
        peerDisA = new addrIsis();
        peerAdjState = statDown;
        lastHeard = bits.getTime();
        advert = new tabGen<rtrIsisLsp>();
        request = new tabGen<rtrIsisLsp>();
        pending = new tabGen<rtrIsisLsp>();
    }

    public int compareTo(rtrIsisNeigh o) {
        if (level.level < o.level.level) {
            return -1;
        }
        if (level.level > o.level.level) {
            return +1;
        }
        return ethAddr.compareTo(o.ethAddr);
    }

    public String toString() {
        return "isis with l" + level.level + " " + ethAddr;
    }

    /**
     * start this neighbor
     */
    protected void startNow() {
        if (debugger.rtrIsisEvnt) {
            logger.debug("starting neighbor l" + level.level + " " + ethAddr);
        }
        new Thread(this).start();
        upTime = bits.getTime();
    }

    /**
     * stow this neighbor
     */
    protected void stopNow() {
        logger.error("neighbor level" + level.level + " " + ifcAddr + " down");
        iface.iface.bfdDel(ifcAddr, this);
        if (iface.oface != null) {
            iface.oface.bfdDel(ofcAddr, this);
        }
        peerAdjState = statDown;
        tabLabel.release(segrouLab, tabLabelEntry.owner.isisAdj);
        tabLabel.release(segrouOth, tabLabelEntry.owner.isisAdj);
        level.schedWork(7);
        need2run = false;
        iface.neighs.del(this);
        notif.wakeup();
        iface.doRetrans();
    }

    /**
     * get peer metric
     *
     * @return metric
     */
    public int getMetric() {
        if (iface.ldpSync) {
            if (lower.fwdCore.ldpNeighFind(ifcAddr, false) == null) {
                return 0xffffff;
            }
        }
        int met = iface.metric;
        if (iface.dynamicMetric < 1) {
            return met;
        }
        return echoCalc.getResult(met);
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
                recvLsp(typ, pck);
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
        readHelloTlvs(pck, msgTypP2Phello);
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
        readHelloTlvs(pck, msgTypL2hello);
    }

    private void readHelloTlvs(packHolder pck, int typ) {
        int hdrSiz = msgTyp2headSiz(typ) - 8;
        encTlv tlv = rtrIsis.getTlv();
        int oldAdjSt = peerAdjState;
        peerAdjState = statDown;
        foreignArea = false;
        peerExtCirc = 0;
        ifcAddr = new addrIP();
        ofcAddr = new addrIP();
        boolean protoSupp = false;
        boolean otherSupp = false;
        boolean areaAddr = false;
        boolean seenOwn = false;
        byte[] remAuth = null;
        int authOfs = 0;
        int packSiz = pck.dataSize();
        for (;;) {
            int ofs = pck.dataSize();
            if (tlv.getBytes(pck)) {
                break;
            }
            switch (tlv.valTyp) {
                case rtrIsisLsp.tlvHandshake:
                    peerAdjState = bits.getByte(tlv.valDat, 0); // adjacency state
                    peerExtCirc = bits.msbGetD(tlv.valDat, 1); // circuit id
                    break;
                case rtrIsisLsp.tlvProtSupp:
                    int protoId = lower.getNLPIDval(false);
                    int otherId = lower.getNLPIDval(true);
                    for (int i = 0; i < tlv.valSiz; i++) {
                        int currId = bits.getByte(tlv.valDat, i);
                        protoSupp |= currId == protoId;
                        otherSupp |= currId == otherId;
                    }
                    break;
                case rtrIsisLsp.tlvAuthen:
                    remAuth = tlv.copyBytes();
                    authOfs = packSiz - ofs;
                    break;
                case rtrIsisLsp.tlvAreaAddr:
                    for (int i = 0; i < tlv.valSiz;) {
                        addrClns cl = new addrClns();
                        cl.fromBuf(tlv.valDat, i);
                        i += cl.getAddrLen();
                        cl.fillUnunsed();
                        int o = cl.compareTo(lower.areaID);
                        areaAddr |= o == 0;
                        foreignArea |= o != 0;
                    }
                    break;
                case rtrIsisLsp.tlvIpv4addr:
                case rtrIsisLsp.tlvIpv6addr:
                    lower.getAddrIface(false, tlv, ifcAddr);
                    lower.getAddrIface(true, tlv, ofcAddr);
                    break;
                case rtrIsisLsp.tlvLanNeigh:
                    for (int i = 0; i < tlv.valSiz;) {
                        addrMac mc = new addrMac();
                        mc.fromBuf(tlv.valDat, i);
                        i += addrMac.size;
                        seenOwn |= mc.compareTo(iface.hwaddr) == 0;
                    }
                    break;
            }
        }
        pck.setBytesLeft(packSiz);
        pck.getSkip(-hdrSiz);
        if (!iface.netPnt2pnt) {
            if (seenOwn) {
                peerAdjState = statUp;
            } else {
                peerAdjState = statInit;
            }
        }
        int i = 1;
        byte[] locAuth = iface.getAuthData(pck, typ, authOfs + hdrSiz);
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
            logger.info("got no protocol from l" + level.level + " " + ethAddr);
            iface.cntr.drop(pck, counter.reasons.badProto);
            return;
        }
        if (ifcAddr.isEmpty()) {
            peerAdjState = statDown;
            logger.info("got no address from l" + level.level + " " + ethAddr);
            iface.cntr.drop(pck, counter.reasons.badAddr);
            return;
        }
        if ((iface.connectedCheck) && (!iface.iface.network.matches(ifcAddr))) {
            logger.info("got from out of subnet peer " + ifcAddr);
            return;
        }
        if (iface.otherEna) {
            if (!otherSupp) {
                peerAdjState = statDown;
                logger.info("got no other protocol from l" + level.level + " " + ethAddr);
                iface.cntr.drop(pck, counter.reasons.badProto);
                return;
            }
            if (ofcAddr.isEmpty()) {
                peerAdjState = statDown;
                logger.info("got no other address from l" + level.level + " " + ethAddr);
                iface.cntr.drop(pck, counter.reasons.badAddr);
                return;
            }
            if ((iface.connectedCheck) && (!iface.oface.network.matches(ofcAddr))) {
                logger.info("got from out of subnet peer " + ofcAddr);
                return;
            }
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
            logger.error("neighbor level" + level.level + " " + ifcAddr + " forgot us");
            iface.iface.bfdDel(ifcAddr, this);
            if (iface.oface != null) {
                iface.oface.bfdDel(ofcAddr, this);
            }
            tabLabel.release(segrouLab, tabLabelEntry.owner.isisAdj);
            tabLabel.release(segrouOth, tabLabelEntry.owner.isisAdj);
            peerAdjState = statDown;
            level.schedWork(7);
            return;
        }
        iface.iface.lower.createETHheader(new packHolder(true, true), ifcAddr, 0);
        if (iface.otherEna) {
            iface.oface.lower.createETHheader(new packHolder(true, true), ofcAddr, 0);
        }
        if (iface.ipInfoCfg != null) {
            secInfoCls cls = new secInfoCls(null, null, null, lower.fwdCore, ifcAddr, prtIsoip.proto, iface.iface.addr);
            ipInfoRes = new secInfoWrk(iface.ipInfoCfg, cls);
            ipInfoRes.doWork(true);
        }
        logger.warn("neighbor level" + level.level + " " + ifcAddr + " up");
        advert.clear();
        if (lower.segrouLab != null) {
            segrouLab = tabLabel.allocate(tabLabelEntry.owner.isisAdj);
            segrouLab.setFwdMpls(tabLabelEntry.owner.isisAdj, lower.fwdCore, iface.iface, ifcAddr, tabLabel.int2labels(ipMpls.labelImp));
            if (iface.otherEna) {
                segrouOth = tabLabel.allocate(tabLabelEntry.owner.isisAdj);
                segrouOth.setFwdMpls(tabLabelEntry.owner.isisAdj, lower.other.fwd, iface.oface, ofcAddr, tabLabel.int2labels(ipMpls.labelImp));
            }
        }
        level.schedWork(7);
        if (iface.bfdTrigger) {
            iface.iface.bfdAdd(ifcAddr, this, "isis");
            if (iface.otherEna) {
                iface.oface.bfdAdd(ofcAddr, this, "isis");
            }
        }
    }

    private tabGen<rtrIsisLsp> readLspList(packHolder pck) {
        tabGen<rtrIsisLsp> l = new tabGen<rtrIsisLsp>();
        encTlv tlv = rtrIsis.getTlv();
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

    private void recvLsp(int typ, packHolder pck) {
        if (peerAdjState != statUp) {
            return;
        }
        rtrIsisLsp lsp = new rtrIsisLsp();
        if (lsp.readData(pck, 0) < 0) {
            logger.info("got bad lsp from l" + level.level + " " + ifcAddr);
            iface.cntr.drop(pck, counter.reasons.badSum);
            return;
        }
        byte[] buf = level.getAuthen(new packHolder(true, true), 0, 0);
        if (buf != null) {
            byte[] got = new byte[0];
            encTlv tlv = rtrIsis.getTlv();
            packHolder p = new packHolder(true, true);
            int siz = lsp.writeData(p, 0);
            p.msbPutW(2, 0); // lifetime
            p.msbPutW(16, 0); // checksum
            p.putSkip(siz);
            p.merge2end();
            p.getSkip(rtrIsisLsp.headSize);
            int pos = rtrIsisLsp.headSize;
            for (;;) {
                int ofs = p.dataSize();
                if (tlv.getBytes(p)) {
                    break;
                }
                if (tlv.valTyp != rtrIsisLsp.tlvAuthen) {
                    continue;
                }
                if (tlv.valSiz != buf.length) {
                    continue;
                }
                pos = siz - ofs;
                got = new byte[tlv.valSiz];
                bits.byteCopy(tlv.valDat, 0, got, 0, got.length);
            }
            p.setBytesLeft(siz);
            buf = level.getAuthen(p, typ, pos);
            boolean authed = got.length == buf.length;
            if (authed) {
                authed = bits.byteComp(got, 0, buf, 0, buf.length) == 0;
            }
            if (!authed) {
                logger.info("got bad authentication from l" + level.level + " " + ethAddr + " on " + lsp);
                return;
            }
        }
        if (debugger.rtrIsisTraf) {
            logger.debug("lsp " + lsp);
        }
        request.del(lsp);
        pending.del(lsp);
        if (iface.shouldIanswer(level.level, rtrID)) {
            iface.sendPsnpPack(lsp, level);
        }
        rtrIsisLsp old = level.lsps.add(lsp);
        if (old == null) {
            advert.put(lsp.copyBytes(false));
            if (lower.routerID.compareTo(lsp.srcID) == 0) {
                level.schedWork(1);
            }
            level.schedWork(6);
            doRetrans();
            level.wakeNeighs();
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
        if (lower.routerID.compareTo(lsp.srcID) == 0) {
            level.schedWork(1);
        }
        level.schedWork(6);
        doRetrans();
        level.wakeNeighs();
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
            if (ntry.compareTo(frst) < 0) {
                continue;
            }
            if (ntry.compareTo(last) > 0) {
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
            pending.del(lsp);
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
        long tim = bits.getTime();
        if ((tim - lastHeard) > holdTime) {
            stopNow();
            level.schedWork(7);
            return;
        }
        if (ipInfoRes != null) {
            if (ipInfoRes.need2drop()) {
                stopNow();
                level.schedWork(7);
                return;
            }
        }
        if (peerAdjState != statUp) {
            return;
        }
        if ((echoTime + iface.echoTimer) < tim) {
            echoCalc.updateFrom(iface.echoParam);
            switch (iface.dynamicMetric) {
                case 1:
                    clntPing png = new clntPing();
                    png.meas = echoCalc;
                    png.fwd = lower.fwdCore;
                    png.src = iface.iface;
                    png.trg = ifcAddr;
                    png.doWork();
                    break;
                case 2:
                    clntEcho ech = new clntEcho();
                    ech.meas = echoCalc;
                    ech.udp = lower.udpCore;
                    ech.src = iface.iface;
                    ech.trg = ifcAddr;
                    ech.doWork();
                    break;
                case 3:
                    clntTwamp twm = new clntTwamp();
                    twm.meas = echoCalc;
                    twm.udp = lower.udpCore;
                    twm.src = iface.iface;
                    twm.trg = ifcAddr;
                    twm.doWork();
                    break;
            }
            echoTime = tim - 1;
        }
        int i = request.size();
        if (i > 0) {
            i = bits.random(0, i);
            rtrIsisLsp lsp = request.get(i);
            if (lsp != null) {
                if (pending.find(lsp) == null) {
                    pending.add(lsp.copyBytes(false));
                    lsp.sequence = 0;
                    iface.sendPsnpPack(lsp, level);
                }
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
            if (pending.find(lsp) != null) {
                continue;
            }
            pending.add(lsp.copyBytes(false));
            iface.sendLspPack(lsp, level);
            if (debugger.rtrIsisTraf) {
                logger.debug("lsp " + lsp);
            }
            if (iface.amIdis(level.level)) {
                advert.add(lsp.copyBytes(false));
            }
            break;
        }
    }

    public void run() {
        long last = 0;
        for (;;) {
            if (!need2run) {
                break;
            }
            notif.misleep(iface.retransTimer);
            long tim = bits.getTime();
            if ((tim - last) >= iface.retransTimer) {
                pending.clear();
                last = tim;
            }
            try {
                doRetrans();
            } catch (Exception e) {
                logger.traceback(e);
            }
        }
    }

}
