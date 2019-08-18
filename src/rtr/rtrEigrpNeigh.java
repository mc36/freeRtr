package rtr;

import addr.addrIP;
import addr.addrIPv4;
import addr.addrIPv6;
import java.util.Comparator;
import pack.packHolder;
import tab.tabRoute;
import tab.tabRouteEntry;
import util.bits;
import util.counter;
import util.debugger;
import util.logger;
import util.notifier;
import util.typLenVal;

/**
 * eigrp neighbor
 *
 * @author matecsaba
 */
public class rtrEigrpNeigh implements Runnable, rtrBfdClnt, Comparator<rtrEigrpNeigh> {

    /**
     * update
     */
    public static final int opcUpdate = 1;

    /**
     * request
     */
    public static final int opcRequest = 2;

    /**
     * query
     */
    public static final int opcQuery = 3;

    /**
     * reply
     */
    public static final int opcReply = 4;

    /**
     * hello
     */
    public static final int opcHello = 5;

    /**
     * probe
     */
    public static final int opcProbe = 7;

    /**
     * sia query
     */
    public static final int opcSiaQuery = 10;

    /**
     * sia reply
     */
    public static final int opcSiaReply = 11;

    /**
     * initialize
     */
    public static final int flagInit = 1;

    /**
     * conditional receive
     */
    public static final int flagCR = 2;

    /**
     * restart
     */
    public static final int flagRst = 4;

    /**
     * end of table
     */
    public static final int flagEot = 8;

    /**
     * convert opcode to string
     *
     * @param i opcode
     * @return string
     */
    public static String opcode2string(int i) {
        switch (i) {
            case opcUpdate:
                return "update";
            case opcRequest:
                return "request";
            case opcQuery:
                return "query";
            case opcReply:
                return "reply";
            case opcHello:
                return "hello";
            case opcProbe:
                return "probe";
            case opcSiaQuery:
                return "siaQuery";
            case opcSiaReply:
                return "siaReply";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * transport address of peer
     */
    public addrIP peer;

    /**
     * learned routes
     */
    public tabRoute<addrIP> learned = new tabRoute<addrIP>("lrn");

    /**
     * advertised routes
     */
    public tabRoute<addrIP> adverted = new tabRoute<addrIP>("adv");

    /**
     * queryed routes
     */
    public tabRoute<addrIP> queryed = new tabRoute<addrIP>("que");

    /**
     * time last heard
     */
    public long lastHeard;

    /**
     * notified on route change
     */
    protected notifier notif = new notifier();

    /**
     * protocol handler
     */
    protected final rtrEigrp lower;

    /**
     * interface handler
     */
    protected final rtrEigrpIface iface;

    /**
     * uptime
     */
    protected long upTime;

    private boolean need2run;

    private int rxSeq;

    private int txSeq;

    private int txFlg;

    private int txOpc;

    private packHolder txBuf;

    /**
     * start one peer
     *
     * @param parent protocol handler
     * @param ifc interface handler
     * @param per transport address
     */
    public rtrEigrpNeigh(rtrEigrp parent, rtrEigrpIface ifc, addrIP per) {
        lower = parent;
        iface = ifc;
        peer = per.copyBytes();
        lastHeard = bits.getTime();
    }

    public int compare(rtrEigrpNeigh o1, rtrEigrpNeigh o2) {
        return o1.peer.compare(o1.peer, o2.peer);
    }

    public String toString() {
        return "eigrp with " + peer;
    }

    /**
     * stop work
     */
    public void bfdPeerDown() {
        stopWork();
    }

    /**
     * send one packet on this neighbor
     *
     * @param pck packet to send
     */
    protected void packSend(packHolder pck) {
        pck.merge2beg();
        pck.IPdf = false;
        pck.IPttl = 255;
        pck.IPtos = 0;
        pck.IPprt = rtrEigrp.protoNum;
        pck.IPsrc.setAddr(iface.iface.addr);
        pck.IPtrg.setAddr(peer);
        lower.fwdCore.protoPack(iface.iface, pck);
    }

    /**
     * send acknowledgement
     */
    protected void packAck() {
        packHolder pck = new packHolder(true, true);
        iface.makeHead(pck, opcHello, 0, 0, rxSeq);
        packSend(pck);
    }

    /**
     * received one packet
     *
     * @param pck packet received
     */
    protected void recvPack(packHolder pck) {
        if (pck.dataSize() < rtrEigrp.sizeHead) {
            logger.info("got too small from " + peer);
            iface.cntr.drop(pck, counter.reasons.tooSmall);
            return;
        }
        if (pck.getByte(0) != rtrEigrp.verNum) { // version
            logger.info("got bad version from " + peer);
            iface.cntr.drop(pck, counter.reasons.badVer);
            return;
        }
        if (pck.getIPsum(0, pck.dataSize(), 0) != 0xffff) { // checksum
            logger.info("got bad checksum from " + peer);
            iface.cntr.drop(pck, counter.reasons.badSum);
            return;
        }
        if (pck.msbGetD(16) != lower.as) {
            logger.info("got invalid as from " + peer);
            iface.cntr.drop(pck, counter.reasons.badVal);
            return;
        }
        int opc = pck.getByte(1);
        int flg = pck.msbGetD(4);
        int seq = pck.msbGetD(8);
        int ack = pck.msbGetD(12);
        pck.getSkip(rtrEigrp.sizeHead);
        if (debugger.rtrEigrpTraf) {
            logger.debug("got op=" + opcode2string(opc) + " flg=" + flg + " ack=" + ack + " seq=" + seq + " from " + peer);
        }
        lastHeard = bits.getTime();
        if ((ack == txSeq) && (txBuf != null)) {
            txBuf = null;
            txFlg = 0;
            txOpc = 0;
            txSeq++;
            notif.wakeup();
        }
        if ((flg & flagInit) != 0) {
            rxSeq = seq - 1;
            adverted.clear();
            learned.clear();
        }
        switch (opc) {
            case opcUpdate:
            case opcQuery:
            case opcReply:
            case opcSiaQuery:
            case opcSiaReply:
                break;
            case opcHello:
                return;
            default:
                logger.info("got invalid opcode from " + peer);
                return;
        }
        if (seq != (rxSeq + 1)) {
            logger.info("got old sequence from " + peer);
            packAck();
            return;
        }
        rxSeq++;
        typLenVal tlv = rtrEigrp.getTlv();
        int cnt = 0;
        for (;;) {
            if (tlv.getBytes(pck)) {
                break;
            }
            packHolder pck2 = new packHolder(true, true);
            byte[] buf = tlv.copyBytes();
            pck2.putCopy(buf, 0, 0, buf.length);
            pck2.putSkip(buf.length);
            pck2.merge2beg();
            tabRouteEntry<addrIP> ntry = null;
            boolean reach = false;
            switch (tlv.valTyp) {
                case 0x102: // ipv4 int
                    pck2.getSkip(20);
                    ntry = rtrBgpUtil.readPrefix(rtrBgpUtil.afiIpv4, false, pck2);
                    if (ntry == null) {
                        continue;
                    }
                    reach = readMetric(ntry, tlv, 4);
                    break;
                case 0x103: // ipv4 ext
                    pck2.getSkip(40);
                    ntry = rtrBgpUtil.readPrefix(rtrBgpUtil.afiIpv4, false, pck2);
                    if (ntry == null) {
                        continue;
                    }
                    readExtern(ntry, tlv, 4);
                    reach = readMetric(ntry, tlv, 24);
                    break;
                case 0x402: // ipv6 int
                    pck2.getSkip(32);
                    ntry = rtrBgpUtil.readPrefix(rtrBgpUtil.afiIpv6, false, pck2);
                    if (ntry == null) {
                        continue;
                    }
                    reach = readMetric(ntry, tlv, 16);
                    break;
                case 0x403: // ipv6 ext
                    pck2.getSkip(52);
                    ntry = rtrBgpUtil.readPrefix(rtrBgpUtil.afiIpv6, false, pck2);
                    if (ntry == null) {
                        continue;
                    }
                    readExtern(ntry, tlv, 16);
                    reach = readMetric(ntry, tlv, 36);
                    break;
                default:
                    continue;
            }
            ntry.nextHop = peer.copyBytes();
            ntry.distance = iface.distance;
            ntry.iface = iface.iface;
            ntry.srcRtr = peer.copyBytes();
            if (debugger.rtrEigrpTraf) {
                logger.debug("prefix " + reach + " " + ntry);
            }
            switch (opc) {
                case opcUpdate:
                case opcReply:
                case opcSiaReply:
                    if (reach) {
                        cnt += tabRoute.addUpdatedEntry(tabRoute.addType.always, learned, rtrBgpUtil.safiUnicast, ntry, iface.roumapIn, iface.roupolIn, iface.prflstIn);
                    } else {
                        cnt += tabRoute.delUpdatedEntry(learned, rtrBgpUtil.safiUnicast, ntry, iface.roumapIn, iface.roupolIn, iface.prflstIn);
                    }
                    break;
                case opcQuery:
                case opcSiaQuery:
                    queryed.add(tabRoute.addType.always, ntry, true, true);
                    adverted.del(ntry.prefix);
                    cnt += tabRoute.delUpdatedEntry(learned, rtrBgpUtil.safiUnicast, ntry, iface.roumapIn, iface.roupolIn, iface.prflstIn);
                    break;
                default:
                    break;
            }
        }
        packAck();
        if (cnt > 0) {
            lower.notif.wakeup();
        }
    }

    /**
     * get show neighbor string
     *
     * @return show line
     */
    public String getShNeigh() {
        return learned.size() + "|" + adverted.size() + "|" + peer;
    }

    /**
     * start work
     */
    public void startWork() {
        if (debugger.rtrEigrpEvnt) {
            logger.debug("starting peer " + peer);
        }
        lastHeard = bits.getTime();
        need2run = true;
        upTime = bits.getTime();
        new Thread(this).start();
        txSeq = bits.randomW() + 1;
        txBuf = new packHolder(true, true);
        txFlg = flagInit;
        txOpc = opcUpdate;
    }

    /**
     * stop work
     */
    public void stopWork() {
        if (debugger.rtrEigrpEvnt) {
            logger.debug("stopping peer " + peer);
        }
        need2run = false;
        adverted.clear();
        learned.clear();
        iface.neighs.del(this);
        lower.notif.wakeup();
        iface.iface.bfdDel(peer, this);
        notif.wakeup();
    }

    public void run() {
        logger.warn("neighbor " + peer + " up");
        if (iface.bfdTrigger) {
            iface.iface.bfdAdd(peer, this, "eigrp");
        }
        try {
            for (;;) {
                if (!need2run) {
                    break;
                }
                doAdvert();
                doTransmit();
                notif.sleep(iface.deadTimer);
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        logger.error("neighbor " + peer + " down");
        stopWork();
    }

    private void doTransmit() {
        packHolder pck;
        try {
            pck = txBuf.copyBytes(true, true);
        } catch (Exception e) {
            return;
        }
        iface.makeHead(pck, txOpc, txFlg, txSeq, rxSeq);
        packSend(pck);
    }

    private void doAdvert() {
        if (txBuf != null) {
            return;
        }
        if (queryed.size() > 0) {
            tabRouteEntry<addrIP> ntry = queryed.get(0);
            queryed.del(ntry);
            writeMetric(ntry, false);
            txBuf = createEntry(ntry);
            txFlg = 0;
            txOpc = opcReply;
            return;
        }
        for (int i = 0; i < adverted.size(); i++) {
            tabRouteEntry<addrIP> ntry = adverted.get(i);
            if (ntry == null) {
                continue;
            }
            if (iface.need2adv.find(ntry) != null) {
                continue;
            }
            adverted.del(ntry);
            writeMetric(ntry, false);
            txBuf = createEntry(ntry);
            txFlg = 0;
            txOpc = opcQuery;
            return;
        }
        for (int i = 0; i < iface.need2adv.size(); i++) {
            tabRouteEntry<addrIP> ntry = iface.need2adv.get(i);
            if (ntry == null) {
                continue;
            }
            if (!ntry.differs(adverted.find(ntry))) {
                continue;
            }
            ntry = ntry.copyBytes();
            adverted.add(tabRoute.addType.always, ntry, true, true);
            if (ntry.originator == null) {
                writeMetric(ntry, true);
            }
            txBuf = createEntry(ntry);
            txFlg = 0;
            txOpc = opcUpdate;
            return;
        }
    }

    private void readExtern(tabRouteEntry<addrIP> ntry, typLenVal tlv, int ofs) {
        addrIPv4 a4 = new addrIPv4();
        a4.fromBuf(tlv.valDat, ofs);
        ntry.aggrRtr = new addrIP();
        ntry.aggrRtr.fromIPv4addr(a4);
        ntry.aggrAs = bits.msbGetD(tlv.valDat, ofs + 4);
        ntry.tag = bits.msbGetD(tlv.valDat, ofs + 8);
    }

    private boolean readMetric(tabRouteEntry<addrIP> ntry, typLenVal tlv, int ofs) {
        int dly = bits.msbGetD(tlv.valDat, ofs + 0);
        int bwd = bits.msbGetD(tlv.valDat, ofs + 4);
        if (dly != -1) {
            dly += iface.delayIn;
            tlv.valDat[ofs + 11]++; // hop count
            bits.msbPutD(tlv.valDat, ofs + 0, dly);
        }
        if (iface.iface.bandwidth < bwd) {
            bwd = (int) iface.iface.bandwidth;
        }
        int i = lower.k1 * bwd;
        i += (lower.k2 * bwd) / (256 - (tlv.valDat[ofs + 13] & 0xff));
        i += lower.k3 * dly;
        if (lower.k5 != 0) {
            i = (i * lower.k5) / (lower.k4 + (tlv.valDat[ofs + 12] & 0xff));
        }
        ntry.metric = i;
        ntry.accIgp = dly;
        ntry.bandwidth = bwd;
        ntry.originator = new addrIP();
        ntry.originator.fromBuf(tlv.valDat, ofs);
        return dly != -1;
    }

    private void writeMetric(tabRouteEntry<addrIP> ntry, boolean reach) {
        byte[] buf = new byte[16];
        if (!reach) {
            bits.msbPutD(buf, 0, -1); // delay
        } else {
            bits.msbPutD(buf, 0, ntry.accIgp + iface.delayOut); // delay
        }
        bits.msbPutD(buf, 4, ntry.bandwidth); // bw
        bits.msbPutW(buf, 9, 1500); // mtu
        buf[12] = (byte) 255; // reliability
        buf[13] = (byte) 1; // load
        ntry.originator = new addrIP();
        ntry.originator.fromBuf(buf, 0);
    }

    private packHolder createEntry(tabRouteEntry<addrIP> ntry) {
        if (debugger.rtrEigrpTraf) {
            logger.debug("prefix " + ntry);
        }
        typLenVal tlv = rtrEigrp.getTlv();
        packHolder pck = new packHolder(true, true);
        boolean ipv4 = ntry.prefix.network.isIPv4();
        if (ntry.aggrRtr != null) {
            tlv.valTyp = 3; // external
            pck.putAddr(0, ntry.aggrRtr.toIPv4()); // rtr
            pck.msbPutD(4, ntry.aggrAs); // as
            pck.msbPutD(8, ntry.tag); // tag
            pck.msbPutD(12, 0); // metric
            pck.msbPutD(17, 0xb00); // type
            pck.putSkip(20);
        } else {
            tlv.valTyp = 2; // internal
        }
        if (ipv4) {
            tlv.valTyp |= 0x100;
            pck.putFill(0, addrIPv4.size, 0);
            pck.putSkip(addrIPv4.size);
        } else {
            tlv.valTyp |= 0x400;
            pck.putFill(0, addrIPv6.size, 0);
            pck.putSkip(addrIPv6.size);
        }
        pck.putAddr(0, ntry.originator);
        pck.putSkip(ntry.originator.getSize());
        if (ipv4) {
            rtrBgpUtil.writePrefix(rtrBgpUtil.afiIpv4, pck, ntry);
        } else {
            rtrBgpUtil.writePrefix(rtrBgpUtil.afiIpv6, pck, ntry);
        }
        pck.merge2end();
        byte[] buf = pck.getCopy();
        pck.clear();
        tlv.putBytes(pck, tlv.valTyp, buf);
        return pck;
    }

}
