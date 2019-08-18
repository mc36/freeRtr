package ip;

import addr.addrIP;
import addr.addrIPv4;
import addr.addrMac;
import addr.addrPrefix;
import addr.addrType;
import ifc.ifcDn;
import ifc.ifcNull;
import ifc.ifcUp;
import java.util.Comparator;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
import pack.packHolder;
import tab.tabGen;
import user.userFormat;
import util.bits;
import util.counter;
import util.debugger;
import util.logger;
import util.state;

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
     * arp cache timeout
     */
    public int arpCacheTimeout = ipIfcLoop.defaultCacheTime;

    private ifcDn lower = new ifcNull();

    private ipIfc4 upper;

    private addrMac hwaddr;

    private addrIPv4 ipaddr = new addrIPv4();

    private addrPrefix<addrIPv4> network = new addrPrefix<addrIPv4>(
            new addrIPv4(), 0);

    private tabGen<ipIfc4arpEntry> cache = new tabGen<ipIfc4arpEntry>();

    private Timer timer;

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
        resetTimer(false);
        cache.clear();
        ipaddr = addr.copyBytes();
        network = new addrPrefix<addrIPv4>(addr, mask);
        resetTimer(true);
    }

    private void resetTimer(boolean needRun) {
        try {
            timer.cancel();
        } catch (Exception e) {
        }
        if (!needRun) {
            return;
        }
        timer = new Timer();
        ipIfc4arpTimer task = new ipIfc4arpTimer(this);
        timer.schedule(task, 500, 60000);
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
        pck.msbPutW(2, 0x800); // protocol address space
        pck.putByte(4, addrMac.size); // byte length of each hardware address
        pck.putByte(5, addrIPv4.size); // byte length of each protocol address
        pck.msbPutW(6, opcode); // opcode
        pck.putAddr(8, srcH); // hardware address of sender
        pck.putAddr(14, srcP); // protocol address of sender
        pck.putAddr(18, trgH); // hardware address of target
        pck.putAddr(24, trgP); // protocol address of target
        pck.putSkip(28); // size of arp packet
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
        if (pck.msbGetW(0) != 1) { // hardware address space
            cntr.drop(pck, counter.reasons.badTyp);
            return;
        }
        if (pck.msbGetW(2) != 0x800) { // protocol address space
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
        if (ipaddr.compare(gotSP, ipaddr) == 0) {
            cntr.drop(pck, counter.reasons.badSrcAddr);
            logger.info("ipv4 address conflict with " + gotSH + " at " + lower);
            return;
        }
        ipIfc4arpEntry ntry = new ipIfc4arpEntry();
        ntry.ip = gotSP;
        ntry.mac = gotSH;
        ntry.time = currTim;
        addEntry(ntry);
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
        boolean rep = (gotTP.compare(gotTP, ipaddr) == 0);
        if (network.matches(gotTP)) {
            rep |= upper.ifcHdr.answerNetReqs;
        } else {
            rep |= upper.ifcHdr.answerDefReqs;
        }
        if (rep) {
            sendArpPack(pck, opcodeARPrep, gotSH, gotSP, hwaddr, gotTP);
        }
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
        if (adr.isMulticast()) {
            if (upper.ifcHdr.mcastAsBcast) {
                putHeader(pck, addrMac.getBroadcast(), hwaddr);
            } else {
                putHeader(pck, adr.conv2multiMac(), hwaddr);
            }
            return false;
        }
        if (adr.isBroadcast()) {
            putHeader(pck, addrMac.getBroadcast(), hwaddr);
            return false;
        }
        if (adr.compare(adr, network.network) <= 0) {
            putHeader(pck, addrMac.getBroadcast(), hwaddr);
            return false;
        }
        if (adr.compare(adr, network.broadcast) >= 0) {
            putHeader(pck, addrMac.getBroadcast(), hwaddr);
            return false;
        }
        ipIfc4arpEntry ntry = new ipIfc4arpEntry();
        ntry.ip = adr;
        ntry = cache.find(ntry);
        if (ntry != null) {
            ntry.time = currTim;
            putHeader(pck, ntry.mac, hwaddr);
            return false;
        }
        if (!network.matches(adr)) {
            cntr.drop(pck, counter.reasons.badTrgAddr);
            return true;
        }
        cntr.drop(pck, counter.reasons.notInTab);
        sendArpPack(pck, opcodeARPreq, addrMac.getBroadcast(), adr, hwaddr,
                ipaddr);
        return true;
    }

    public String toString() {
        return "arp on " + lower;
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
            gratoitous = 0;
        }
        currTim = bits.getTime();
        ipIfc4arpEntry ntry = new ipIfc4arpEntry();
        for (int i = cache.size() - 1; i >= 0; i--) {
            ntry = cache.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.stat) {
                continue;
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
        ntry.time = bits.getTime();
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

}

class ipIfc4arpTimer extends TimerTask {

    private ipIfc4arp parent;

    public ipIfc4arpTimer(ipIfc4arp prnt) {
        parent = prnt;
    }

    public void run() {
        try {
            parent.doCachePurge();
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}

class ipIfc4arpEntry implements Comparator<ipIfc4arpEntry> {

    public addrMac mac;

    public addrIPv4 ip;

    public long time;

    public boolean stat;

    public String toString() {
        return mac + " " + ip + " " + bits.timePast(time);
    }

    public int compare(ipIfc4arpEntry o1, ipIfc4arpEntry o2) {
        return o1.ip.compare(o1.ip, o2.ip);
    }

}
