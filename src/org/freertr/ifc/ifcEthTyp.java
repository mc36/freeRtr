package org.freertr.ifc;

import java.io.File;
import java.io.RandomAccessFile;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrMac;
import org.freertr.addr.addrType;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgIfc;
import org.freertr.ip.ipCor;
import org.freertr.ip.ipIfc4;
import org.freertr.ip.ipIfc6;
import org.freertr.pack.packHolder;
import org.freertr.tab.tabAceslstN;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabListing;
import org.freertr.tab.tabQos;
import org.freertr.tab.tabRateLimit;
import org.freertr.user.userFormat;
import org.freertr.util.bits;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.history;
import org.freertr.util.logger;
import org.freertr.util.notifier;
import org.freertr.util.state;

/**
 * ethertype multiplexer
 *
 * @author matecsaba
 */
public class ifcEthTyp implements Runnable, ifcUp {

    /**
     * send in clear
     */
    public ifcDn sendClear;

    /**
     * strict mac check ingress
     */
    public addrMac macCheckRx = null;

    /**
     * strict mac check egress
     */
    public addrMac macCheckTx = null;

    /**
     * strict mtu check ingress
     */
    public boolean mtuCheckRx = false;

    /**
     * strict mtu check egress
     */
    public boolean mtuCheckTx = false;

    /**
     * forced mtu size
     */
    public int forcedMTU = 0;

    /**
     * forced bandwidth
     */
    public long forcedBW = 0;

    /**
     * forced down
     */
    public int forcedDN = 0;

    /**
     * forced up
     */
    public boolean forcedUP = false;
    /**
     * forced mac address
     */
    public addrMac forcedMac;

    /**
     * log state change
     */
    public boolean logStateChg = false;

    /**
     * padding minimum
     */
    public int padupMin = 0;

    /**
     * padding modulo
     */
    public int padupMod = 0;

    /**
     * ingress rate
     */
    public tabRateLimit rateIn;

    /**
     * egress rate
     */
    public tabRateLimit rateOut;

    /**
     * ingress qos
     */
    public tabQos qosIn;

    /**
     * egress qos
     */
    public tabQos qosOut;

    /**
     * mac security
     */
    public ifcMacSec macSec;

    /**
     * loss detector
     */
    public ifcLossDet lossDet;

    /**
     * sgt handler
     */
    public ifcSgt sgtHnd;

    /**
     * sgt handler
     */
    public int sgtSet = -1;

    /**
     * service chaining
     */
    public ifcNshFwd nshFwd;

    /**
     * monitor filter
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> monFlt;

    /**
     * monitor direction, 1=rx, 2=tx
     */
    public int monDir = 3;

    /**
     * monitor truncate
     */
    public int monTrnc = 0;

    /**
     * monitor sampling need
     */
    public int monSmpN = 0;

    /**
     * monitor unsampled packets
     */
    private int monSmpP = 0;

    /**
     * monitor session target
     */
    public ifcEthTyp monSes = null;

    /**
     * monitor session headers
     */
    public boolean monHdr;

    /**
     * monitor buffer data
     */
    public byte[] monBufD = null;

    /**
     * monitor buffer pointer
     */
    public int monBufP;

    /**
     * snap code
     */
    public final static int snap = 0xaaaa;

    /**
     * control code
     */
    public final static int ctrl = 0x03;

    /**
     * ieee code
     */
    public final static int ieee = 0x80c2;

    /**
     * ieee ethernet code
     */
    public final static int ieth = 0x7;

    /**
     * ipv4 core
     */
    public ipCor ip4cor;

    /**
     * ipv6 core
     */
    public ipCor ip6cor;

    private final notifier notif;

    private int need2run;

    private long lastSync;

    private state.states lastState;

    private ifcDn lower;

    private ifcEthTypET defUpper;

    private final tabGen<ifcEthTypET> etTyps;

    private final tabGen<ifcEthTypLLC> llcTyps;

    private final tabGen<ifcEthTypSNAP> snapTyps;

    private boolean promiscous;

    private counter totCntr;

    private final counter cntr;

    private final counter[] sizes;

    private final counter[] protos;

    private final counter[] clsCos;

    private final counter[] clsExp;

    private final counter[] clsPrc;

    private final counter[] ttlMpl;

    private final counter[] ttlNsh;

    private final counter[] ttlPrt;

    private final history hstry;

    /**
     * hardware counters
     */
    public counter hwCntr;

    /**
     * hardware substract
     */
    public counter hwSub;

    /**
     * hardware history
     */
    public history hwHstry;

    private final String name;

    private final cfgIfc cfger;

    private RandomAccessFile logFile = null;

    /**
     * looped packets dropped
     */
    public static int loopDrops;

    /**
     * looped packets depth
     */
    public final static int loopMax = 64;

    /**
     * get history handler
     *
     * @return history handler
     */
    public history getHistory() {
        return hstry;
    }

    public counter getCounter() {
        return cntr;
    }

    /**
     * get promiscous state
     *
     * @return state
     */
    public boolean getPromisc() {
        return promiscous;
    }

    /**
     * get macsec state
     *
     * @return state
     */
    public boolean getMacsec() {
        return macSec != null;
    }

    /**
     * get lossdet state
     *
     * @return state
     */
    public boolean getLossdet() {
        return lossDet != null;
    }

    /**
     * get sgt state
     *
     * @return state
     */
    public boolean getSgt() {
        return sgtHnd != null;
    }

    /**
     * get hardware address
     *
     * @return hw address
     */
    public addrType getHwAddr() {
        if (forcedMac != null) {
            return forcedMac;
        }
        return lower.getHwAddr();
    }

    public String toString() {
        return name;
    }

    /**
     * set worker interface
     *
     * @param parent worker interface
     */
    public void setParent(ifcDn parent) {
        lower = parent;
        lastState = lower.getState();
        propagateState();
    }

    /**
     * get state of interface
     *
     * @return state of line protocol
     */
    public state.states getState() {
        if (forcedDN != 0) {
            return state.states.admin;
        }
        if (forcedUP) {
            return state.states.up;
        }
        return lastState;
    }

    public void setState(state.states stat) {
        stat = state.toUsable(stat);
        if (lastState == stat) {
            return;
        }
        lastState = stat;
        if (forcedDN != 0) {
            return;
        }
        if (forcedUP) {
            return;
        }
        propagateState();
    }

    /**
     * propagate current state;
     */
    public void propagateState() {
        state.states s = lastState;
        if (forcedUP) {
            s = state.states.up;
        }
        if (forcedDN != 0) {
            s = state.states.admin;
        }
        cntr.stateChange(s);
        if (logStateChg) {
            logger.warn("interface " + name + " change to " + state.conv2string(s));
        }
        for (int i = 0; i < etTyps.size(); i++) {
            etTyps.get(i).upper.setState(s);
        }
        for (int i = 0; i < llcTyps.size(); i++) {
            llcTyps.get(i).upper.setState(s);
        }
        for (int i = 0; i < snapTyps.size(); i++) {
            snapTyps.get(i).upper.setState(s);
        }
        if (defUpper.upper != null) {
            defUpper.upper.setState(s);
        }
        cfger.stateChanged(s);
    }

    /**
     * close this interface
     */
    public void closeUp() {
        lastState = state.states.close;
        qosIn = null;
        qosOut = null;
        timerUpdate();
        for (int i = 0; i < etTyps.size(); i++) {
            try {
                etTyps.get(i).upper.closeUp();
            } catch (Exception e) {
            }
        }
        for (int i = 0; i < llcTyps.size(); i++) {
            try {
                llcTyps.get(i).upper.closeUp();
            } catch (Exception e) {
            }
        }
        for (int i = 0; i < snapTyps.size(); i++) {
            try {
                snapTyps.get(i).upper.closeUp();
            } catch (Exception e) {
            }
        }
        try {
            defUpper.upper.closeUp();
        } catch (Exception e) {
        }
    }

    /**
     * update timer thread
     */
    public synchronized void timerUpdate() {
        if ((qosIn != null) || (qosOut != null) || (macSec != null) || (lossDet != null)) {
            need2run |= 1;
        } else {
            need2run &= 0xfe;
        }
        if ((need2run & 2) != 0) {
            return;
        }
        if (debugger.ifcEthTypTraf) {
            logger.debug("starting");
        }
        need2run |= 2;
        new Thread(this).start();
    }

    /**
     * trigger timer thread
     */
    public void triggerSync() {
        lastSync = 0;
        notif.wakeup();
    }

    public void run() {
        for (;;) {
            if ((need2run & 1) == 0) {
                break;
            }
            long lst = 0;
            long tim = bits.getTime();
            if (qosOut != null) {
                for (;;) {
                    packHolder pck = qosOut.dequeuePack(tim);
                    if (pck == null) {
                        break;
                    }
                    if (doOutProcess(pck)) {
                        break;
                    }
                    pktAccountTx(pck);
                    lower.sendPack(pck);
                }
                if (lst < qosOut.lastLeft) {
                    lst = qosOut.lastLeft;
                }
            }
            if (qosIn != null) {
                for (;;) {
                    packHolder pck = qosIn.dequeuePack(tim);
                    if (pck == null) {
                        break;
                    }
                    doRxPack(pck);
                }
                if (lst < qosIn.lastLeft) {
                    lst = qosIn.lastLeft;
                }
            }
            if ((lst < 1) || (lst > 10000)) {
                lst = 10000;
            }
            notif.misleep((int) lst);
            if ((tim - lastSync) < 5000) {
                continue;
            }
            lastSync = tim;
            if (lossDet != null) {
                packHolder pck = lossDet.doSync();
                if (pck != null) {
                    pktAccountTx(pck);
                    if (macSec != null) {
                        macSec.doEncrypt(pck);
                    }
                    lower.sendPack(pck);
                }
            }
            if (macSec != null) {
                packHolder pck = macSec.doSync();
                if (pck != null) {
                    pktAccountTx(pck);
                    lower.sendPack(pck);
                }
            }
        }
        need2run &= 0xfd;
        if (debugger.ifcEthTypTraf) {
            logger.debug("stopped");
        }
    }

    /**
     * set filter criteria
     *
     * @param promisc need all packet (promiscous mode)
     */
    public void setFilter(boolean promisc) {
        promisc |= defUpper.promiscous;
        for (int i = 0; i < etTyps.size(); i++) {
            promisc |= etTyps.get(i).promiscous;
        }
        for (int i = 0; i < llcTyps.size(); i++) {
            promisc |= llcTyps.get(i).promiscous;
        }
        for (int i = 0; i < snapTyps.size(); i++) {
            promisc |= snapTyps.get(i).promiscous;
        }
        if (promiscous == promisc) {
            return;
        }
        promiscous = promisc;
        lower.setFilter(promisc);
        if (debugger.ifcEthTypTraf) {
            logger.debug("set filter to " + promisc);
        }
    }

    /**
     * create new multiplexer
     *
     * @param nam name of interface
     * @param ifc config interface
     */
    public ifcEthTyp(String nam, cfgIfc ifc) {
        if (debugger.ifcEthTypTraf) {
            logger.debug("started");
        }
        cfger = ifc;
        name = "" + nam;
        notif = new notifier();
        lower = new ifcNull();
        totCntr = new counter();
        cntr = new counter();
        sizes = new counter[8];
        clsCos = new counter[8];
        clsExp = new counter[8];
        clsPrc = new counter[8];
        protos = new counter[256];
        ttlPrt = new counter[256];
        ttlNsh = new counter[64];
        ttlMpl = new counter[256];
        for (int i = 0; i < sizes.length; i++) {
            sizes[i] = new counter();
        }
        for (int i = 0; i < clsCos.length; i++) {
            clsCos[i] = new counter();
        }
        for (int i = 0; i < clsExp.length; i++) {
            clsExp[i] = new counter();
        }
        for (int i = 0; i < clsPrc.length; i++) {
            clsPrc[i] = new counter();
        }
        for (int i = 0; i < protos.length; i++) {
            protos[i] = new counter();
        }
        for (int i = 0; i < ttlPrt.length; i++) {
            ttlPrt[i] = new counter();
        }
        for (int i = 0; i < ttlNsh.length; i++) {
            ttlNsh[i] = new counter();
        }
        for (int i = 0; i < ttlMpl.length; i++) {
            ttlMpl[i] = new counter();
        }
        hstry = new history();
        defUpper = new ifcEthTypET(this, null);
        promiscous = false;
        lastState = state.states.up;
        etTyps = new tabGen<ifcEthTypET>();
        llcTyps = new tabGen<ifcEthTypLLC>();
        snapTyps = new tabGen<ifcEthTypSNAP>();
    }

    private packHolder applyMonitor(packHolder pck, int dir, boolean copy) {
        if ((dir & monDir) == 0) {
            return null;
        }
        if (monSmpN > 0) {
            if ((monSmpP++ % monSmpN) != 0) {
                return null;
            }
        }
        if (monFlt != null) {
            pck.ETHtype = pck.msbGetW(0);
            pck.getSkip(2);
            boolean b;
            switch (pck.ETHtype) {
                case ipIfc4.type:
                    b = ip4cor.parseIPheader(pck, true);
                    break;
                case ipIfc6.type:
                    b = ip6cor.parseIPheader(pck, true);
                    break;
                default:
                    b = true;
                    break;
            }
            if (b) {
                pck.getSkip(-2);
                return null;
            }
            b = monFlt.matches(false, true, pck);
            pck.getSkip(-2);
            if (!b) {
                return null;
            }
        }
        if (copy) {
            pck = pck.copyBytes(true, true);
        }
        if (monTrnc < 1) {
            return pck;
        }
        if (pck.dataSize() < monTrnc) {
            return pck;
        }
        if (!copy) {
            pck = pck.copyBytes(true, true);
        }
        pck.setDataSize(monTrnc);
        return pck;
    }

    private void doRxPack(packHolder pck) {
        int typ = pck.msbGetW(0); // ether type
        pck.ETHtype = typ;
        if (debugger.ifcEthTypTraf) {
            logger.debug("rx type=" + bits.toHexW(typ));
        }
        ifcEthTypET et = new ifcEthTypET(this, null);
        et.ethTyp = typ;
        et = etTyps.find(et);
        if (et != null) {
            et.doRxPack(pck);
            return;
        }
        if (typ > 1500) {
            defUpper.doRxPack(pck);
            return;
        }
        if ((llcTyps.size() + snapTyps.size()) < 1) {
            defUpper.doRxPack(pck);
            return;
        }
        typ += 2;
        if (typ > pck.dataSize()) {
            cntr.drop(pck, counter.reasons.tooSmall);
            return;
        }
        pck.setDataSize(typ);
        typ = pck.msbGetW(2); // llc type
        pck.ETHtype = typ;
        if (debugger.ifcEthTypTraf) {
            logger.debug("rx llc=" + bits.toHexW(typ));
        }
        ifcEthTypLLC llc = new ifcEthTypLLC(null, null);
        llc.llcTyp = typ;
        llc = llcTyps.find(llc);
        if (llc != null) {
            llc.doRxPack(pck);
            return;
        }
        if (typ != snap) {
            defUpper.doRxPack(pck);
            return;
        }
        typ = pck.msbGetD(5) >>> 8; // organization id
        pck.ETHtype = typ;
        if (debugger.ifcEthTypTraf) {
            logger.debug("rx snap=" + bits.toHexD(typ));
        }
        ifcEthTypSNAP snp = new ifcEthTypSNAP(null, null);
        snp.snapTyp = typ;
        snp = snapTyps.find(snp);
        if (snp != null) {
            snp.doRxPack(pck);
            return;
        }
        if (typ != 0) {
            defUpper.doRxPack(pck);
            return;
        }
        pck.getSkip(8);
        typ = pck.msbGetW(0); // ether type
        pck.ETHtype = typ;
        if (debugger.ifcEthTypTraf) {
            logger.debug("rx (embedded)type=" + bits.toHexW(typ));
        }
        et = new ifcEthTypET(this, null);
        et.ethTyp = typ;
        et = etTyps.find(et);
        if (et != null) {
            et.doRxPack(pck);
        } else {
            defUpper.doRxPack(pck);
        }
    }

    /**
     * send one packet to the network
     *
     * @param pck packet to send
     */
    public void doTxPack(packHolder pck) {
        if (padupMin > 0) {
            int i = padupMin - pck.dataSize();
            if (i > 0) {
                pck.putFill(0, i, 0);
                pck.putSkip(i);
                pck.merge2end();
            }
        }
        if (padupMod > 1) {
            int i = padupMod - (pck.dataSize() % padupMod);
            pck.putFill(0, i, 0);
            pck.putSkip(i);
            pck.merge2end();
        }
        if (mtuCheckTx) {
            if ((pck.dataSize() - 2) > getMTUsize()) {
                cntr.drop(pck, counter.reasons.badLen);
                return;
            }
        }
        if (macCheckTx != null) {
            boolean ok = promiscous || pck.ETHtrg.isBroadcast() || pck.ETHtrg.isMulticast() || (macCheckTx.compareTo(pck.ETHsrc) == 0);
            if (!ok) {
                cntr.drop(pck, counter.reasons.badAddr);
                return;
            }
        }
        if (!forcedUP) {
            if (lastState != state.states.up) {
                cntr.drop(pck, counter.reasons.notUp);
                return;
            }
            if (forcedDN != 0) {
                cntr.drop(pck, counter.reasons.notUp);
                return;
            }
        }
        if (rateOut != null) {
            if (rateOut.check(pck.dataSize())) {
                cntr.drop(pck, counter.reasons.noBuffer);
                return;
            }
        }
        pck.INTsent++;
        if (pck.INTsent > loopMax) {
            loopDrops++;
            cntr.drop(pck, counter.reasons.tooLong);
            return;
        }
        if (qosOut != null) {
            qosOut.classifyPack(pck);
            qosOut.enqueuePack(pck);
            notif.wakeup();
            return;
        }
        if (doOutProcess(pck)) {
            return;
        }
        pktAccountTx(pck);
        lower.sendPack(pck);
    }

    private boolean doOutProcess(packHolder pck) {
        if (logFile != null) {
            packHolder mon = applyMonitor(pck, 2, false);
            if (mon != null) {
                try {
                    logFile.write(mon.convertToPcap(bits.getTime() + cfgAll.timeServerOffset, true));
                } catch (Exception e) {
                }
            }
        }
        if (monBufD != null) {
            packHolder mon = applyMonitor(pck, 2, false);
            if (mon != null) {
                putMonBufPck(mon.convertToPcap(bits.getTime() + cfgAll.timeServerOffset, true));
            }
        }
        if (monSes != null) {
            packHolder mon = applyMonitor(pck, 2, true);
            if (mon != null) {
                if (monHdr) {
                    ifcEther.createETHheader(mon, false);
                }
                monSes.doTxPack(mon);
            }
        }
        if (sendClear != null) {
            pktAccountTx(pck);
            sendClear.sendPack(pck);
            return true;
        }
        if (sgtHnd != null) {
            if (sgtHnd.doEncode(pck)) {
                return true;
            }
        }
        if (lossDet != null) {
            if (lossDet.doEncode(pck)) {
                return true;
            }
        }
        if (macSec != null) {
            if (macSec.doEncrypt(pck)) {
                return true;
            }
        }
        return false;
    }

    public void recvPack(packHolder pck) {
        doRxWork(pck, false);
    }

    /**
     * got packet from dataplane
     *
     * @param pck packet
     */
    public void gotFromDataplane(packHolder pck) {
        doRxWork(pck, true);
    }

    private void doRxWork(packHolder pck, boolean fromDp) {
        pktAccountRx1(pck);
        if (mtuCheckRx) {
            if ((pck.dataSize() - 2) > getMTUsize()) {
                cntr.drop(pck, counter.reasons.badLen);
                return;
            }
        }
        if (macCheckRx != null) {
            boolean ok = promiscous || pck.ETHtrg.isBroadcast() || pck.ETHtrg.isMulticast() || (macCheckRx.compareTo(pck.ETHtrg) == 0);
            if (!ok) {
                cntr.drop(pck, counter.reasons.badAddr);
                return;
            }
        }
        if (!forcedUP) {
            if (lastState != state.states.up) {
                cntr.drop(pck, counter.reasons.notUp);
                return;
            }
            if (forcedDN != 0) {
                cntr.drop(pck, counter.reasons.notUp);
                return;
            }
        }
        if (rateIn != null) {
            if (rateIn.check(pck.dataSize())) {
                cntr.drop(pck, counter.reasons.noBuffer);
                return;
            }
        }
        if (macSec != null) {
            if (macSec.doDecrypt(pck, fromDp)) {
                cntr.drop(pck, counter.reasons.badSum);
                return;
            }
        }
        if (lossDet != null) {
            if (lossDet.doDecode(pck)) {
                cntr.drop(pck, counter.reasons.badSum);
                return;
            }
        }
        if (sgtHnd != null) {
            if (sgtHnd.doDecode(pck, fromDp)) {
                cntr.drop(pck, counter.reasons.badSum);
                return;
            }
        }
        if (sgtSet >= 0) {
            pck.SGTid = sgtSet;
        }
        if (logFile != null) {
            packHolder mon = applyMonitor(pck, 1, false);
            if (mon != null) {
                try {
                    logFile.write(mon.convertToPcap(bits.getTime() + cfgAll.timeServerOffset, true));
                } catch (Exception e) {
                }
            }
        }
        if (monBufD != null) {
            packHolder mon = applyMonitor(pck, 1, false);
            if (mon != null) {
                putMonBufPck(mon.convertToPcap(bits.getTime() + cfgAll.timeServerOffset, true));
            }
        }
        if (monSes != null) {
            packHolder mon = applyMonitor(pck, 1, true);
            if (mon != null) {
                if (monHdr) {
                    ifcEther.createETHheader(mon, false);
                }
                monSes.doTxPack(mon);
            }
        }
        if (qosIn == null) {
            pktAccountRx2(pck);
            doRxPack(pck);
            return;
        }
        qosIn.classifyPack(pck);
        pktAccountRx2(pck);
        qosIn.enqueuePack(pck);
        notif.wakeup();
    }

    /**
     * add one ethertype handler
     *
     * @param typ ethertype value that will served (-1=default)
     * @param name name of the protocol
     * @param ifc interface handler that should be notified when packet arrives
     * @return ethertype handler
     */
    public ifcEthTypET addET(int typ, String name, ifcUp ifc) {
        if (debugger.ifcEthTypTraf) {
            logger.debug("add ethertype=" + bits.toHexW(typ));
        }
        ifcEthTypET ntry = new ifcEthTypET(this, ifc);
        ntry.ethTyp = typ;
        ntry.name = name;
        ifc.setParent(ntry);
        if (typ == -1) {
            defUpper = ntry;
            setFilter(false);
            return ntry;
        }
        ifcEthTypET old = etTyps.add(ntry);
        if (old != null) {
            return old;
        }
        setFilter(false);
        return ntry;
    }

    /**
     * update one ethertype handler
     *
     * @param typ ethertype value that will served (-1=default)
     * @param ifc interface handler that should be notified when packet arrives
     * @return ethertype handler
     */
    public ifcEthTypET updateET(int typ, ifcUp ifc) {
        if (debugger.ifcEthTypTraf) {
            logger.debug("update ethertype=" + bits.toHexW(typ));
        }
        if (typ == -1) {
            defUpper.ethTyp = typ;
            ifc.setParent(defUpper);
            return defUpper;
        }
        ifcEthTypET ntry = new ifcEthTypET(this, ifc);
        ntry.ethTyp = typ;
        ntry = etTyps.find(ntry);
        if (ntry == null) {
            return null;
        }
        ntry.upper = ifc;
        ifc.setParent(ntry);
        return ntry;
    }

    /**
     * remove one ethertype handler
     *
     * @param typ ethertype value that should deleted (-1=default)
     * @return true interface handler that was used (null=error)
     */
    public ifcUp delET(int typ) {
        if (debugger.ifcEthTypTraf) {
            logger.debug("del ethertype=" + bits.toHexW(typ));
        }
        ifcEthTypET ntry = new ifcEthTypET(this, null);
        if (typ == -1) {
            ntry = defUpper;
            defUpper = new ifcEthTypET(this, null);
        } else {
            ntry.ethTyp = typ;
            ntry = etTyps.del(ntry);
            if (ntry == null) {
                return null;
            }
        }
        try {
            ntry.upper.closeUp();
        } catch (Exception e) {
        }
        setFilter(false);
        return ntry.upper;
    }

    /**
     * add one llc handler
     *
     * @param typ llc value that will served
     * @param name name of the protocol
     * @param ifc interface handler that should be notified when packet arrives
     * @return llc handler
     */
    public ifcEthTypLLC addLLC(int typ, String name, ifcUp ifc) {
        if (debugger.ifcEthTypTraf) {
            logger.debug("add llc=" + bits.toHexW(typ));
        }
        ifcEthTypLLC ntry = new ifcEthTypLLC(this, ifc);
        ntry.llcTyp = typ;
        ntry.name = name;
        ifc.setParent(ntry);
        ifcEthTypLLC old = llcTyps.add(ntry);
        if (old != null) {
            return old;
        }
        setFilter(false);
        return ntry;
    }

    /**
     * update one llc handler
     *
     * @param typ llc value that will served
     * @param ifc interface handler that should be notified when packet arrives
     * @return llc handler
     */
    public ifcEthTypLLC updateLLC(int typ, ifcUp ifc) {
        if (debugger.ifcEthTypTraf) {
            logger.debug("update llc=" + bits.toHexW(typ));
        }
        ifcEthTypLLC ntry = new ifcEthTypLLC(this, ifc);
        ntry.llcTyp = typ;
        ntry = llcTyps.find(ntry);
        if (ntry == null) {
            return null;
        }
        ntry.upper = ifc;
        ifc.setParent(ntry);
        return ntry;
    }

    /**
     * remove one llc handler
     *
     * @param typ llc value that should deleted
     * @return true interface handler that was used (null=error)
     */
    public ifcUp delLLC(int typ) {
        if (debugger.ifcEthTypTraf) {
            logger.debug("del llc=" + bits.toHexW(typ));
        }
        ifcEthTypLLC ntry = new ifcEthTypLLC(null, null);
        ntry.llcTyp = typ;
        ntry = llcTyps.del(ntry);
        if (ntry == null) {
            return null;
        }
        try {
            ntry.upper.closeUp();
        } catch (Exception e) {
        }
        setFilter(false);
        return ntry.upper;
    }

    /**
     * add one snap handler
     *
     * @param typ snap value that will served
     * @param name name of the protocol
     * @param ifc interface handler that should be notified when packet arrives
     * @return snap handler
     */
    public ifcEthTypSNAP addSNAP(int typ, String name, ifcUp ifc) {
        if (debugger.ifcEthTypTraf) {
            logger.debug("add snap=" + bits.toHexD(typ));
        }
        ifcEthTypSNAP ntry = new ifcEthTypSNAP(this, ifc);
        ntry.snapTyp = typ;
        ntry.name = name;
        ifc.setParent(ntry);
        ifcEthTypSNAP old = snapTyps.add(ntry);
        if (old != null) {
            return old;
        }
        setFilter(false);
        return ntry;
    }

    /**
     * update one snap handler
     *
     * @param typ snap value that will served
     * @param ifc interface handler that should be notified when packet arrives
     * @return snap handler
     */
    public ifcEthTypSNAP updateSNAP(int typ, ifcUp ifc) {
        if (debugger.ifcEthTypTraf) {
            logger.debug("update snap=" + bits.toHexD(typ));
        }
        ifcEthTypSNAP ntry = new ifcEthTypSNAP(this, ifc);
        ntry.snapTyp = typ;
        ntry = snapTyps.find(ntry);
        if (ntry == null) {
            return null;
        }
        ntry.upper = ifc;
        ifc.setParent(ntry);
        return ntry;
    }

    /**
     * remove one snap handler
     *
     * @param typ snap value that should deleted
     * @return true interface handler that was used (null=error)
     */
    public ifcUp delSNAP(int typ) {
        if (debugger.ifcEthTypTraf) {
            logger.debug("del snap=" + bits.toHexD(typ));
        }
        ifcEthTypSNAP ntry = new ifcEthTypSNAP(null, null);
        ntry.snapTyp = typ;
        ntry = snapTyps.del(ntry);
        if (ntry == null) {
            return null;
        }
        try {
            ntry.upper.closeUp();
        } catch (Exception e) {
        }
        setFilter(false);
        return ntry.upper;
    }

    /**
     * get size of mtu
     *
     * @return mtu size
     */
    public int getMTUsize() {
        if (forcedMTU > 0) {
            return forcedMTU;
        }
        return lower.getMTUsize();
    }

    /**
     * get interface bandwidth
     *
     * @return bandwidth
     */
    public long getBandwidth() {
        if (forcedBW > 0) {
            return forcedBW;
        }
        return lower.getBandwidth();
    }

    private int pktsiz2bucket(int siz) {
        int i = (siz & 0xffffff) >>> 8;
        if (i > 7) {
            return 7;
        }
        return i;
    }

    private void pktAccountTx(packHolder pck) {
        cntr.tx(pck);
        sizes[pktsiz2bucket(pck.dataSize())].tx(pck);
        clsCos[pck.ETHcos & 7].tx(pck);
        clsExp[pck.MPLSexp & 7].tx(pck);
        clsPrc[(pck.IPtos >>> 5) & 7].tx(pck);
        protos[pck.IPprt & 0xff].tx(pck);
        ttlPrt[pck.IPttl & 0xff].tx(pck);
        ttlNsh[pck.NSHttl & 0x3f].tx(pck);
        ttlMpl[pck.MPLSttl & 0xff].tx(pck);
    }

    private void pktAccountRx1(packHolder pck) {
        cntr.rx(pck);
        sizes[pktsiz2bucket(pck.dataSize())].rx(pck);
    }

    private void pktAccountRx2(packHolder pck) {
        clsCos[pck.ETHcos & 7].rx(pck);
        clsExp[pck.MPLSexp & 7].rx(pck);
        clsPrc[(pck.IPtos >>> 5) & 7].rx(pck);
        protos[pck.IPprt & 0xff].rx(pck);
        ttlPrt[pck.IPttl & 0xff].rx(pck);
        ttlNsh[pck.NSHttl & 0x3f].rx(pck);
        ttlMpl[pck.MPLSttl & 0xff].rx(pck);
    }

    /**
     * get show results
     *
     * @return table
     */
    public userFormat getShSizes() {
        userFormat l = new userFormat("|", "size|tx|rx|drop|tx|rx|drop", "1|3packet|3byte");
        String[] heds = {"0-255", "256-511", "512-767", "768-1023", "1024-1279", "1280-1535", "1536-1791", "1792-65535",};
        for (int i = 0; i < sizes.length; i++) {
            l.add(heds[i] + "|" + sizes[i].getShPsum() + "|" + sizes[i].getShBsum());
        }
        return l;
    }

    /**
     * get show results
     *
     * @return table
     */
    public userFormat getShClasses() {
        userFormat l = new userFormat("|", "class|rx|tx|rx|tx|rx|tx|rx|tx|rx|tx", "1|2cos pack|2cos byte|2exp pack|2exp byte|2prec pack|2prec byte");
        for (int i = 0; i < clsPrc.length; i++) {
            l.add(i + "|" + getShClasses(i));
        }
        return l;
    }

    private String getShClasses(int i) {
        counter cos = clsCos[i];
        counter exp = clsExp[i];
        counter prc = clsPrc[i];
        return cos.packRx + "|" + cos.packTx + "|" + cos.byteRx + "|" + cos.byteTx + "|"
                + exp.packRx + "|" + exp.packTx + "|" + exp.byteRx + "|" + exp.byteTx + "|"
                + prc.packRx + "|" + prc.packTx + "|" + prc.byteRx + "|" + prc.byteTx;
    }

    /**
     * get show results
     *
     * @return table
     */
    public userFormat getShTimes() {
        userFormat l = new userFormat("|", "ttl|rx|tx|rx|tx|rx|tx|rx|tx|rx|tx", "1|2nsh pack|2nshs byte|2mpls pack|2mpls byte|2ip pack|2ip byte");
        for (int i = 0; i < ttlPrt.length; i++) {
            counter nsh;
            l.add(i + "|" + getShTimes(i));
        }
        return l;
    }

    private String getShTimes(int i) {
        counter nsh;
        if (i < ttlNsh.length) {
            nsh = ttlNsh[i];
        } else {
            nsh = new counter();
        }
        counter mpl = ttlMpl[i];
        counter prt = ttlPrt[i];
        return nsh.packRx + "|" + nsh.packTx + "|" + nsh.byteRx + "|" + nsh.byteTx + "|"
                + mpl.packRx + "|" + mpl.packTx + "|" + mpl.byteRx + "|" + mpl.byteTx + "|"
                + prt.packRx + "|" + prt.packTx + "|" + prt.byteRx + "|" + prt.byteTx;
    }

    /**
     * get show results
     *
     * @return table
     */
    public userFormat getShProtos() {
        userFormat l = new userFormat("|", "proto|pack|byte|pack|byte", "1|2tx|2rx");
        for (int i = 0; i < protos.length; i++) {
            l.add(i + "|" + protos[i].packTx + "|" + protos[i].byteTx + "|" + protos[i].packRx + "|" + protos[i].byteRx);
        }
        return l;
    }

    /**
     * get show results
     *
     * @return table
     */
    public userFormat getShLoss() {
        userFormat l = new userFormat("|", "who|tx|rx");
        if (lossDet == null) {
            return l;
        }
        lossDet.getShow(l);
        return l;
    }

    /**
     * get show results
     *
     * @return table
     */
    public userFormat getShTypes() {
        userFormat l = new userFormat("|", "type|value|handler|tx|rx|drop|tx|rx|drop", "3|3packet|3byte");
        if (defUpper.upper != null) {
            l.add(defUpper.dump());
        }
        for (int i = 0; i < etTyps.size(); i++) {
            l.add(etTyps.get(i).dump());
        }
        for (int i = 0; i < llcTyps.size(); i++) {
            l.add(llcTyps.get(i).dump());
        }
        for (int i = 0; i < snapTyps.size(); i++) {
            l.add(snapTyps.get(i).dump());
        }
        return l;
    }

    /**
     * get short results
     *
     * @return text
     */
    public String getShHeads() {
        String s = "";
        if (promiscous) {
            s = ", promisc";
        }
        return name + " is " + state.conv2string(getState()) + s;
    }

    /**
     * reinit file logger
     *
     * @param s name of capture file
     * @return false if successful, true if error happened
     */
    public boolean initLog(String s) {
        try {
            logFile.close();
        } catch (Exception e) {
        }
        if (logFile != null) {
            logFile = null;
            return true;
        }
        try {
            RandomAccessFile f = new RandomAccessFile(new File(s), "rw");
            f.setLength(0);
            f.write(packHolder.getPcapHeader(1));
            logFile = f;
        } catch (Exception e) {
        }
        return logFile == null;
    }

    /**
     * get monitor buffer size
     *
     * @return bytes
     */
    public int getMonBufSize() {
        if (monBufD == null) {
            return -1;
        }
        return monBufD.length;
    }

    private synchronized void putMonBufPck(byte[] pck) {
        byte[] trg = monBufD;
        if (trg == null) {
            return;
        }
        for (int i = 0; i < pck.length; i++) {
            trg[monBufP] = pck[i];
            monBufP = (monBufP + 1) % trg.length;
        }
    }

    /**
     * clear hardware counters
     */
    public void clearHwCounter() {
        if (hwCntr == null) {
            return;
        }
        if (hwSub == null) {
            hwSub = hwCntr.copyBytes();
            return;
        }
        hwSub = hwSub.plus(hwCntr);
    }

    /**
     * clear counters
     */
    public void clearSwCounter() {
        for (int i = 0; i < etTyps.size(); i++) {
            etTyps.get(i).cntr.clear();
        }
        for (int i = 0; i < llcTyps.size(); i++) {
            llcTyps.get(i).cntr.clear();
        }
        for (int i = 0; i < snapTyps.size(); i++) {
            snapTyps.get(i).cntr.clear();
        }
        for (int i = 0; i < sizes.length; i++) {
            sizes[i].clear();
        }
        for (int i = 0; i < protos.length; i++) {
            protos[i].clear();
        }
        for (int i = 0; i < clsCos.length; i++) {
            clsCos[i].clear();
        }
        for (int i = 0; i < clsExp.length; i++) {
            clsExp[i].clear();
        }
        for (int i = 0; i < clsPrc.length; i++) {
            clsPrc[i].clear();
        }
        totCntr = totCntr.plus(cntr);
        cntr.clear();
    }

    /**
     * get total counters
     *
     * @return total counters
     */
    public counter getTotalCntr() {
        return totCntr.plus(cntr);
    }

    /**
     * get hw total counters
     *
     * @return total counters
     */
    public counter getHwTotalCntr() {
        if (hwCntr == null) {
            return null;
        }
        if (hwSub == null) {
            return hwCntr;
        }
        return hwCntr.plus(hwSub);
    }

    /**
     * put hw ethertype counters
     *
     * @param typ ethertype
     * @param cnt counter
     * @return false on success, true on error
     */
    public boolean putHwEthTyp(int typ, counter cnt) {
        ifcEthTypET et = new ifcEthTypET(this, null);
        et.ethTyp = typ;
        et = etTyps.find(et);
        if (et == null) {
            return true;
        }
        et.hwCntr = cnt;
        return false;
    }

}

class ifcEthTypET implements ifcDn, Comparable<ifcEthTypET> {

    public int ethTyp;

    public String name;

    public ifcUp upper;

    public boolean promiscous;

    public counter cntr = new counter();

    public counter hwCntr;

    private ifcEthTyp lower;

    public ifcEthTypET(ifcEthTyp parent, ifcUp server) {
        lower = parent;
        if (server != null) {
            upper = server;
            return;
        }
        ifcNull nul = new ifcNull();
        nul.getCounter().dropper = parent.getCounter();
        upper = nul;
    }

    public int compareTo(ifcEthTypET o) {
        if (ethTyp < o.ethTyp) {
            return -1;
        }
        if (ethTyp > o.ethTyp) {
            return +1;
        }
        return 0;
    }

    public String dump() {
        return "ethtyp|" + bits.toHexW(ethTyp) + "|" + name + "|" + cntr.getShHwPsum(hwCntr) + "|" + cntr.getShHwBsum(hwCntr);
    }

    public String toString() {
        return "" + lower;
    }

    public counter getCounter() {
        return cntr;
    }

    public addrType getHwAddr() {
        return lower.getHwAddr();
    }

    public state.states getState() {
        return lower.getState();
    }

    public void flapped() {
    }

    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
    }

    public void setFilter(boolean promisc) {
        promiscous = promisc;
        lower.setFilter(promisc);
    }

    public long getBandwidth() {
        return lower.getBandwidth();
    }

    public int getMTUsize() {
        return lower.getMTUsize();
    }

    public void closeDn() {
        lower.delET(ethTyp);
    }

    public void sendPack(packHolder pck) {
        if (debugger.ifcEthTypTraf) {
            logger.debug("tx type=" + bits.toHexW(ethTyp));
        }
        cntr.tx(pck);
        pck.ETHtype = ethTyp;
        lower.doTxPack(pck);
    }

    public void doRxPack(packHolder pck) {
        cntr.rx(pck);
        upper.recvPack(pck);
    }

}

class ifcEthTypLLC implements ifcDn, Comparable<ifcEthTypLLC> {

    public int llcTyp;

    public String name;

    public ifcUp upper = new ifcNull();

    public boolean promiscous;

    public counter cntr = new counter();

    private ifcEthTyp lower;

    public ifcEthTypLLC(ifcEthTyp parent, ifcUp server) {
        lower = parent;
        upper = server;
    }

    public int compareTo(ifcEthTypLLC o) {
        if (llcTyp < o.llcTyp) {
            return -1;
        }
        if (llcTyp > o.llcTyp) {
            return +1;
        }
        return 0;
    }

    public String dump() {
        return "llc|" + bits.toHexW(llcTyp) + "|" + name + "|" + cntr.getShPsum() + "|" + cntr.getShBsum();
    }

    public String toString() {
        return "" + lower;
    }

    public counter getCounter() {
        return cntr;
    }

    public addrType getHwAddr() {
        return lower.getHwAddr();
    }

    public state.states getState() {
        return lower.getState();
    }

    public void flapped() {
    }

    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
    }

    public void setFilter(boolean promisc) {
        promiscous = promisc;
        lower.setFilter(promisc);
    }

    public long getBandwidth() {
        return lower.getBandwidth();
    }

    public int getMTUsize() {
        int i = lower.getMTUsize();
        if (i > 1497) {
            i = 1497;
        }
        return i;
    }

    public void closeDn() {
        lower.delLLC(llcTyp);
    }

    public void sendPack(packHolder pck) {
        if (debugger.ifcEthTypTraf) {
            logger.debug("tx llc=" + bits.toHexW(llcTyp));
        }
        pck.getSkip(2);
        cntr.tx(pck);
        pck.msbPutW(0, pck.dataSize() + 3); // size of packet
        pck.msbPutW(2, llcTyp); // llc type
        pck.putByte(4, ifcEthTyp.ctrl); // control flags
        pck.putSkip(5);
        pck.merge2beg();
        pck.ETHtype = llcTyp;
        lower.doTxPack(pck);
    }

    public void doRxPack(packHolder pck) {
        pck.getSkip(5);
        cntr.rx(pck);
        if (upper == null) {
            cntr.drop(pck, counter.reasons.noIface);
            return;
        }
        pck.msbPutW(0, llcTyp);
        pck.putSkip(2);
        pck.merge2beg();
        upper.recvPack(pck);
    }

}

class ifcEthTypSNAP implements ifcDn, Comparable<ifcEthTypSNAP> {

    public int snapTyp;

    public String name;

    public ifcUp upper = new ifcNull();

    public boolean promiscous;

    public counter cntr = new counter();

    private ifcEthTyp lower;

    public ifcEthTypSNAP(ifcEthTyp parent, ifcUp server) {
        lower = parent;
        upper = server;
    }

    public int compareTo(ifcEthTypSNAP o) {
        if (snapTyp < o.snapTyp) {
            return -1;
        }
        if (snapTyp > o.snapTyp) {
            return +1;
        }
        return 0;
    }

    public String dump() {
        return "snap|" + bits.toHexD(snapTyp) + "|" + name + "|" + cntr.getShPsum() + "|" + cntr.getShBsum();
    }

    public String toString() {
        return "" + lower;
    }

    public counter getCounter() {
        return cntr;
    }

    public addrType getHwAddr() {
        return lower.getHwAddr();
    }

    public state.states getState() {
        return lower.getState();
    }

    public void flapped() {
    }

    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
    }

    public void setFilter(boolean promisc) {
        promiscous = promisc;
        lower.setFilter(promisc);
    }

    public long getBandwidth() {
        return lower.getBandwidth();
    }

    public int getMTUsize() {
        int i = lower.getMTUsize();
        if (i > 1494) {
            i = 1494;
        }
        return i;
    }

    public void closeDn() {
        lower.delSNAP(snapTyp);
    }

    public void sendPack(packHolder pck) {
        if (debugger.ifcEthTypTraf) {
            logger.debug("tx snap=" + bits.toHexD(snapTyp));
        }
        cntr.tx(pck);
        pck.msbPutW(0, pck.dataSize() + 6); // size of packet
        pck.msbPutW(2, ifcEthTyp.snap); // llc type
        pck.putByte(4, ifcEthTyp.ctrl); // control flags
        pck.msbPutD(5, snapTyp << 8); // organization code
        pck.putSkip(8);
        pck.merge2beg();
        pck.ETHtype = snapTyp;
        lower.doTxPack(pck);
    }

    public void doRxPack(packHolder pck) {
        pck.getSkip(8);
        cntr.rx(pck);
        if (upper == null) {
            cntr.drop(pck, counter.reasons.noIface);
            return;
        }
        upper.recvPack(pck);
    }

}
