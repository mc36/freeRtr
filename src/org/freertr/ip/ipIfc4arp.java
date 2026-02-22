package org.freertr.ip;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrMac;
import org.freertr.addr.addrPrefix;
import org.freertr.addr.addrType;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcUp;
import org.freertr.pack.packHolder;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabRateLimit;
import org.freertr.user.userFormat;
import org.freertr.util.bits;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.state;

/**
 * implements arp (rfc826) cache
 *
 * @author matecsaba
 */
public class ipIfc4arp implements ifcUp {

    /**
     * ethertype of my packets
     */
    public final static int type = 0x806;

    /**
     * size of my packets
     */
    public final static int size = 28;

    /**
     * arp cache dynamic
     */
    public boolean arpCacheDynamic = true;

    /**
     * arp cache timeout
     */
    public int arpCacheTimeout = ipIfcLoop.defaultCacheTime;

    /**
     * arp cache retry
     */
    public int arpCacheRetry = ipIfcLoop.defaultRetryTime;

    /**
     * arp query rate
     */
    public tabRateLimit arpQueryRate;

    /**
     * timer
     */
    protected ipIfc4arpTimer timer;

    private ifcDn lower = new ifcNull();

    private ipIfc4 upper;

    private addrMac hwaddr = new addrMac();

    private addrIPv4 ipaddr = new addrIPv4();

    private addrPrefix<addrIPv4> network = new addrPrefix<addrIPv4>(new addrIPv4(), 0);

    private tabGen<ipIfc4arpEntry> cache = new tabGen<ipIfc4arpEntry>();

    private long currTim = bits.getTime();

    private counter cntr = new counter();

    private int gratoitous;

    private final static int opcodeARPreq = 1;

    private final static int opcodeARPrep = 2;

    private final static int opcodeRARPreq = 3;

    private final static int opcodeRARPrep = 4;

    private final static int opcodeDRARPreq = 5;

    private final static int opcodeDRARPrep = 6;

    private final static int opcodeInARPreq = 8;

    private final static int opcodeInARPrep = 9;

    /**
     * create arp handler
     *
     * @param parent parent of me
     */
    public ipIfc4arp(ipIfc4 parent) {
        upper = parent;
    }

    /**
     * get counter
     *
     * @return counter
     */
    public counter getCounter() {
        return cntr;
    }

    private static String opcode2string(int i) {
        String s;
        switch (i) {
            case opcodeARPreq:
                s = "arpReq";
                break;
            case opcodeARPrep:
                s = "arpRep";
                break;
            case opcodeRARPreq:
                s = "rarpReq";
                break;
            case opcodeRARPrep:
                s = "rarpRep";
                break;
            case opcodeDRARPreq:
                s = "drarpReq";
                break;
            case opcodeDRARPrep:
                s = "drarpRep";
                break;
            case opcodeInARPreq:
                s = "inarpReq";
                break;
            case opcodeInARPrep:
                s = "inarpRep";
                break;
            default:
                s = "unknown=" + i;
                break;
        }
        return s;
    }

    /**
     * set ip network
     *
     * @param addr address
     * @param mask mask
     */
    public void setIPv4addr(addrIPv4 addr, int mask) {
        cache.clear();
        ipaddr = addr.copyBytes();
        network = new addrPrefix<addrIPv4>(addr, mask);
    }

    private synchronized void resetTimer(boolean needRun) {
        timer = null;
        if (!needRun) {
            return;
        }
        timer = new ipIfc4arpTimer(this);
        timer.start();
    }

    /**
     * set worker interface
     *
     * @param parent worker interface
     */
    public void setParent(ifcDn parent) {
        lower = parent;
        hwaddr = (addrMac) lower.getHwAddr();
    }

    /**
     * set state of interface
     *
     * @param stat new state
     */
    public void setState(state.states stat) {
        cntr.stateChange(stat);
        resetTimer(state.toUsable(stat) == state.states.up);
    }

    /**
     * close this interface
     */
    public void closeUp() {
        resetTimer(false);
    }

    private void sendArpPack(packHolder pck, int opcode, addrMac trgH,
            addrIPv4 trgP, addrMac srcH, addrIPv4 srcP) {
        pck.clear();
        putHeader(pck, trgH, srcH);
        pck.msbPutW(0, type);
        pck.putSkip(2);
        if (trgH.isBroadcast()) {
            trgH.setNot(trgH);
        }
        if (debugger.ipIfc4arpTraf) {
            logger.debug("tx op=" + opcode2string(opcode) + " th=" + trgH
                    + " tp=" + trgP);
        }
        pck.msbPutW(0, 1); // hardware address space
        pck.msbPutW(2, ipIfc4.type); // protocol address space
        pck.putByte(4, addrMac.size); // byte length of each hardware address
        pck.putByte(5, addrIPv4.size); // byte length of each protocol address
        pck.msbPutW(6, opcode); // opcode
        pck.putAddr(8, srcH); // hardware address of sender
        pck.putAddr(14, srcP); // protocol address of sender
        pck.putAddr(18, trgH); // hardware address of target
        pck.putAddr(24, trgP); // protocol address of target
        pck.putSkip(size); // size of arp packet
        pck.merge2beg();
        lower.sendPack(pck);
    }

    /**
     * is proxy arp enabled
     *
     * @return not enabled
     */
    public boolean getProxyArp() {
        return false;
    }

    /**
     * this interface got a packet for processing
     *
     * @param pck packet needs to parsed
     */
    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        if (pck.msbGetW(0) != type) { // ethertype
            cntr.drop(pck, counter.reasons.badEthTyp);
            return;
        }
        pck.getSkip(2);
        if (pck.dataSize() < size) {
            cntr.drop(pck, counter.reasons.tooSmall);
            return;
        }
        if (pck.msbGetW(0) != 1) { // hardware address space
            cntr.drop(pck, counter.reasons.badTyp);
            return;
        }
        if (pck.msbGetW(2) != ipIfc4.type) { // protocol address space
            cntr.drop(pck, counter.reasons.badCod);
            return;
        }
        if (pck.getByte(4) != addrMac.size) { // length of hardware address
            cntr.drop(pck, counter.reasons.badLen);
            return;
        }
        if (pck.getByte(5) != addrIPv4.size) { // length of protocol address
            cntr.drop(pck, counter.reasons.badLen);
            return;
        }
        int gotOP = pck.msbGetW(6); // opcode
        addrMac gotSH = new addrMac();
        addrIPv4 gotSP = new addrIPv4();
        addrMac gotTH = new addrMac();
        addrIPv4 gotTP = new addrIPv4();
        pck.getAddr(gotSH, 8); // hardware address of sender
        pck.getAddr(gotSP, 14); // protocol address of sender
        pck.getAddr(gotTH, 18); // hardware address of target
        pck.getAddr(gotTP, 24); // protocol address of target
        if (debugger.ipIfc4arpTraf) {
            logger.debug("rx op=" + opcode2string(gotOP) + " sh=" + gotSH
                    + " sp=" + gotSP + " th=" + gotTH + " tp=" + gotTP);
        }
        if (gotSP.compareTo(ipaddr) == 0) {
            cntr.drop(pck, counter.reasons.badSrcAddr);
            logger.info("ipv4 address conflict with " + gotSH + " at " + lower);
            return;
        }
        if (arpCacheDynamic) {
            ipIfc4arpEntry ntry = new ipIfc4arpEntry();
            ntry.ip = gotSP;
            ntry.mac = gotSH;
            ntry.time = currTim;
            addEntry(ntry);
        }
        if (gotOP != opcodeARPreq) {
            cntr.drop(pck, counter.reasons.badCod);
            return;
        }
        addrIP adr = new addrIP();
        adr.fromIPv4addr(gotTP);
        addrType mac = upper.checkMyAlias(adr);
        if (mac != null) {
            sendArpPack(pck, opcodeARPrep, gotSH, gotSP, (addrMac) mac, gotTP);
            return;
        }
        boolean rep = gotTP.compareTo(ipaddr) == 0;
        if (network.matches(gotTP)) {
            rep |= upper.ifcHdr.answerNetReqs;
        } else {
            rep |= upper.ifcHdr.answerDefReqs;
        }
        if (!rep) {
            return;
        }
        if (upper.ifcHdr.answerFilter != null) {
            packHolder flt = new packHolder(true, true);
            adr = new addrIP();
            adr.fromIPv4addr(gotSP);
            flt.IPsrc.setAddr(adr);
            adr = new addrIP();
            adr.fromIPv4addr(gotTP);
            flt.IPtrg.setAddr(adr);
            if (!upper.ifcHdr.answerFilter.matches(false, false, flt)) {
                return;
            }
        }
        sendArpPack(pck, opcodeARPrep, gotSH, gotSP, hwaddr, gotTP);
    }

    private void addEntry(ipIfc4arpEntry ntry) {
        if (debugger.ipIfc4arpEvnt) {
            logger.debug("add " + ntry);
        }
        cache.put(ntry);
        upper.upper.tableChanger();
    }

    private void delEntry(ipIfc4arpEntry ntry) {
        if (debugger.ipIfc4arpEvnt) {
            logger.debug("del " + ntry);
        }
        cache.del(ntry);
        upper.upper.tableChanger();
    }

    private void putHeader(packHolder pck, addrMac trg, addrMac src) {
        pck.putStart();
        pck.ETHtrg.setAddr(trg);
        pck.ETHsrc.setAddr(src);
    }

    /**
     * put dst,src mac address to packet
     *
     * @param pck packet to update
     * @param adr next hop ip address
     * @return true not found, false if updated successfully
     */
    public boolean readMACheader(packHolder pck, addrIPv4 adr) {
        ipIfc4arpEntry ntry = new ipIfc4arpEntry();
        ntry.ip = adr;
        ntry = cache.find(ntry);
        if (ntry != null) {
            ntry.time = currTim;
            putHeader(pck, ntry.mac, hwaddr);
            return false;
        }
        if (adr.isMulticast()) {
            if (upper.ifcHdr.mcastAsBcast) {
                putHeader(pck, addrMac.getBroadcast(), hwaddr);
                return false;
            }
            if (!upper.ifcHdr.mcastAsUcast) {
                putHeader(pck, adr.conv2multiMac(), hwaddr);
                return false;
            }
            ntry = cache.get(0);
            if (ntry == null) {
                putHeader(pck, adr.conv2multiMac(), hwaddr);
                return false;
            }
            putHeader(pck, ntry.mac, hwaddr);
            return false;
        }
        if (adr.isBroadcast()) {
            putHeader(pck, addrMac.getBroadcast(), hwaddr);
            return false;
        }
        if (adr.compareTo(network.network) < 0) {
            putHeader(pck, addrMac.getBroadcast(), hwaddr);
            return false;
        }
        if (adr.compareTo(network.broadcast) > 0) {
            putHeader(pck, addrMac.getBroadcast(), hwaddr);
            return false;
        }
        if (!network.matches(adr)) {
            cntr.drop(pck, counter.reasons.badTrgAddr);
            return true;
        }
        cntr.drop(pck, counter.reasons.notInTab);
        if (arpQueryRate != null) {
            if (arpQueryRate.check(1)) {
                return true;
            }
        }
        sendArpPack(pck, opcodeARPreq, addrMac.getBroadcast(), adr, hwaddr, ipaddr);
        return true;
    }

    public String toString() {
        return "arp on " + lower;
    }

    /**
     * send arp packet
     *
     * @param mac source mac
     * @param adr source ip
     */
    public void sendARPheader(addrMac mac, addrIPv4 adr) {
        sendArpPack(new packHolder(true, true), opcodeARPreq,
                addrMac.getBroadcast(), adr, mac, adr);
    }

    /**
     * do arp cache cleanup round
     */
    public void doCachePurge() {
        if (debugger.ipIfc4arpEvnt) {
            logger.debug("purge");
        }
        gratoitous++;
        if (gratoitous > 2) {
            sendArpPack(new packHolder(true, true), opcodeARPreq,
                    addrMac.getBroadcast(), ipaddr, hwaddr, ipaddr);
            if (upper.ifcHdr != null) {
                for (int i = 0;; i++) {
                    addrIP adr = upper.ifcHdr.adrGetIp(i);
                    if (adr == null) {
                        break;
                    }
                    addrMac mac = upper.ifcHdr.adrGetMac(i);
                    if (mac == null) {
                        break;
                    }
                    sendARPheader(mac, adr.toIPv4());
                }
            }
            gratoitous = 0;
        }
        currTim = bits.getTime();
        ipIfc4arpEntry ntry = new ipIfc4arpEntry();
        packHolder pck = new packHolder(true, true);
        for (int i = cache.size() - 1; i >= 0; i--) {
            ntry = cache.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.stat) {
                continue;
            }
            if ((currTim - ntry.time) > arpCacheRetry) {
                sendArpPack(pck, opcodeARPreq, ntry.mac, ntry.ip, hwaddr,
                        ipaddr);
            }
            if ((currTim - ntry.time) < arpCacheTimeout) {
                continue;
            }
            delEntry(ntry);
        }
    }

    /**
     * update mac header
     *
     * @param mod mode
     * @param mac address to set
     * @param adr address to set
     */
    public void updateMACheader(int mod, addrMac mac, addrIPv4 adr) {
        ipIfc4arpEntry ntry = new ipIfc4arpEntry();
        ntry.ip = adr.copyBytes();
        ntry.mac = mac.copyBytes();
        ntry.time = currTim;
        switch (mod) {
            case 0:
                addEntry(ntry);
                break;
            case 1:
                ntry.stat = true;
                addEntry(ntry);
                break;
            case 2:
                ntry.stat = true;
                delEntry(ntry);
                break;
        }
    }

    /**
     * get mac address
     *
     * @param adr ip address
     * @return mac address, null if not found
     */
    public addrMac getMACaddr(addrIPv4 adr) {
        ipIfc4arpEntry ntry = new ipIfc4arpEntry();
        ntry.ip = adr;
        ntry = cache.find(ntry);
        if (ntry == null) {
            return null;
        }
        return ntry.mac;
    }

    /**
     * get mac and ip address
     *
     * @param seq sequence to read
     * @param adr ip address
     * @param mac mac address
     * @return false on success, true on error
     */
    public boolean getMACaddr(int seq, addrIP adr, addrMac mac) {
        ipIfc4arpEntry ntry = cache.get(seq);
        if (ntry == null) {
            return true;
        }
        adr.fromIPv4addr(ntry.ip);
        mac.setAddr(ntry.mac);
        return false;
    }

    /**
     * get static mac and ip addresses
     *
     * @param lst list to append
     * @param beg beginning
     */
    public void getMACaddr(List<String> lst, String beg) {
        for (int i = 0; i < cache.size(); i++) {
            ipIfc4arpEntry ntry = cache.get(i);
            if (ntry == null) {
                continue;
            }
            if (!ntry.stat) {
                continue;
            }
            lst.add(beg + ntry.ip + " " + ntry.mac);
        }
    }

    /**
     * get list of neighbors
     *
     * @return list of entries
     */
    public userFormat getShCache() {
        userFormat lst = new userFormat("|", "mac|address|time|static");
        for (int i = 0; i < cache.size(); i++) {
            ipIfc4arpEntry ntry = cache.get(i);
            if (ntry == null) {
                continue;
            }
            lst.add(ntry.mac + "|" + ntry.ip + "|" + bits.timePast(ntry.time) + "|" + ntry.stat);
        }
        return lst;
    }

    /**
     * get local mac address
     *
     * @return mac address
     */
    public addrMac getLocalMac() {
        return hwaddr.copyBytes();
    }

}

class ipIfc4arpTimer implements Runnable {

    private ipIfc4arp parent;

    public ipIfc4arpTimer(ipIfc4arp prnt) {
        parent = prnt;
    }

    public void start() {
        logger.startThread(this);
    }

    public void run() {
        try {
            for (;;) {
                bits.sleep(60000);
                if (parent.timer != this) {
                    break;
                }
                parent.doCachePurge();
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}

class ipIfc4arpEntry implements Comparable<ipIfc4arpEntry> {

    public addrMac mac;

    public addrIPv4 ip;

    public long time;

    public boolean stat;

    public String toString() {
        return mac + " " + ip + " " + bits.timePast(time);
    }

    public int compareTo(ipIfc4arpEntry o) {
        return ip.compareTo(o.ip);
    }

}
