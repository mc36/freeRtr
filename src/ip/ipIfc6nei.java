package ip;

import addr.addrIP;
import addr.addrIPv6;
import addr.addrMac;
import addr.addrPrefix;
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
import util.typLenVal;

/**
 * implements neighbor (rfc2461) cache
 *
 * @author matecsaba
 */
public class ipIfc6nei implements ifcUp {

    /**
     * neighbor cache timeout
     */
    public int neiCacheTimeout = ipIfcLoop.defaultCacheTime;

    private addrMac hwaddr;

    private addrIPv6 ipaddr = new addrIPv6();

    private addrIPv6 lladdr = new addrIPv6();

    private addrPrefix<addrIPv6> network = new addrPrefix<addrIPv6>(
            new addrIPv6(), 0);

    private tabGen<ipIfc6neiEntry> cache = new tabGen<ipIfc6neiEntry>();

    private ifcDn lower = new ifcNull();

    private Timer timer;

    private ipIfc6 upper;

    private counter cntr = new counter();

    private long currTim = bits.getTime();

    private ipCor6 ipc = new ipCor6();

    private ipIcmp6 icc = new ipIcmp6();

    private typLenVal tlv = ipIcmp6.getTLVreader();

    /**
     * get counter
     *
     * @return counter
     */
    public counter getCounter() {
        return cntr;
    }

    /**
     * create nei handler
     *
     * @param parent parent of me
     */
    public ipIfc6nei(ipIfc6 parent) {
        upper = parent;
    }

    /**
     * set ip network
     *
     * @param addr address
     * @param mask mask
     */
    public void setIPv6addr(addrIPv6 addr, int mask) {
        resetTimer(false);
        cache.clear();
        ipaddr = addr.copyBytes();
        network = new addrPrefix<addrIPv6>(addr, mask);
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
        ipIfc6neiTimer task = new ipIfc6neiTimer(this);
        timer.schedule(task, 500, 60000);
    }

    /**
     * set parent
     *
     * @param parent parent
     */
    public void setParent(ifcDn parent) {
        lower = parent;
        hwaddr = (addrMac) lower.getHwAddr();
        lladdr = addrIPv6.genLinkLocal(hwaddr);
    }

    /**
     * set state
     *
     * @param stat state
     */
    public void setState(state.states stat) {
        cntr.stateChange(stat);
        resetTimer(state.toUsable(stat) == state.states.up);
    }

    /**
     * close interface
     */
    public void closeUp() {
        resetTimer(false);
    }

    /**
     * is proxy nei enabled
     *
     * @return not enabled
     */
    public boolean getProxyArp() {
        return false;
    }

    /**
     * got icmp packet
     *
     * @param pck packet parsed
     * @return false if replied successfully, true if error happened
     */
    public boolean gotIcmpPack(packHolder pck) {
        if (debugger.ipIfc6neiTraf) {
            logger.debug("rx op=" + icc.icmp2string(pck.ICMPtc));
        }
        int siz;
        int ned = -1;
        addrIPv6 adrI = pck.IPsrc.toIPv6();
        switch (pck.ICMPtc) {
            case ipIcmp6.icmpRtrSol:
                siz = 8;
                ned = 1; // source address
                break;
            case ipIcmp6.icmpRtrAdv:
                siz = 16;
                ned = 1; // source address
                break;
            case ipIcmp6.icmpNeiSol:
                siz = 24;
                ned = 1; // source address
                break;
            case ipIcmp6.icmpNeiAdv:
                siz = 24;
                ned = 2; // target address
                pck.getAddr(adrI, 8);
                break;
            default:
                return true;
        }
        pck.getSkip(siz);
        for (;;) {
            if (tlv.getBytes(pck)) {
                break;
            }
            if ((tlv.valTyp != ned)) {
                continue;
            }
            if (tlv.valSiz < addrMac.size) {
                continue;
            }
            ipIfc6neiEntry ntry = new ipIfc6neiEntry();
            addrMac adrM = new addrMac();
            adrM.fromBuf(tlv.valDat, 0);
            ntry.ip = adrI.copyBytes();
            ntry.mac = adrM;
            ntry.time = currTim;
            if (pck.ICMPtc == ipIcmp6.icmpRtrAdv) {
                ntry.router = true;
            }
            addEntry(ntry);
        }
        return false;
    }

    private void addEntry(ipIfc6neiEntry ntry) {
        if (debugger.ipIfc6neiEvnt) {
            logger.debug("add " + ntry);
        }
        cache.put(ntry);
        upper.upper.tableChanger();
    }

    private void delEntry(ipIfc6neiEntry ntry) {
        if (debugger.ipIfc6neiEvnt) {
            logger.debug("del " + ntry);
        }
        cache.del(ntry);
        upper.upper.tableChanger();
    }

    /**
     * received packet
     *
     * @param pck packet
     */
    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        if (pck.msbGetW(0) != ipIfc6.type) {// ethertype
            return;
        }
        pck.getSkip(2);
        if (ipc.parseIPheader(pck, true)) {
            return;
        }
        if (pck.IPprt != ipIcmp6.protoNum) {
            return;
        }
        pck.getSkip(pck.IPsiz);
        if (icc.parseICMPheader(pck)) {
            return;
        }
        gotIcmpPack(pck);
    }

    private void putHeader(packHolder pck, addrMac adr) {
        pck.putStart();
        pck.ETHtrg.setAddr(adr);
        pck.ETHsrc.setAddr(hwaddr);
    }

    /**
     * put dst,src mac address to packet
     *
     * @param pck packet to update
     * @param adr next hop ip address
     * @return true not found, false if updated successfully
     */
    public boolean readMACheader(packHolder pck, addrIPv6 adr) {
        if (adr.isMulticast()) {
            if (upper.ifcHdr.mcastAsBcast) {
                putHeader(pck, addrMac.getBroadcast());
            } else {
                putHeader(pck, adr.conv2multiMac());
            }
            return false;
        }
        if (!adr.isLinkLocal()) {
            if (adr.compare(adr, network.network) <= 0) {
                putHeader(pck, addrMac.getBroadcast());
                return false;
            }
            if (adr.compare(adr, network.broadcast) >= 0) {
                putHeader(pck, addrMac.getBroadcast());
                return false;
            }
        }
        ipIfc6neiEntry ntry = new ipIfc6neiEntry();
        ntry.ip = adr;
        ntry = cache.find(ntry);
        if (ntry != null) {
            ntry.time = currTim;
            putHeader(pck, ntry.mac);
            return false;
        }
        if ((!network.matches(adr)) && (!adr.isLinkLocal())) {
            cntr.drop(pck, counter.reasons.badTrgAddr);
            return true;
        }
        cntr.drop(pck, counter.reasons.notInTab);
        icc.createNeighSol(hwaddr, pck, adr, lladdr);
        pck.msbPutW(0, ipIfc6.type); // ethertype
        pck.putSkip(2);
        pck.merge2beg();
        if (upper.ifcHdr.mcastAsBcast) {
            putHeader(pck, addrMac.getBroadcast());
        } else {
            adr = pck.IPtrg.toIPv6();
            putHeader(pck, adr.conv2solicited().conv2multiMac());
        }
        lower.sendPack(pck);
        return true;
    }

    /**
     * do neighbor cache cleanup round
     */
    public void doCachePurge() {
        if (debugger.ipIfc6neiEvnt) {
            logger.debug("purge");
        }
        currTim = bits.getTime();
        ipIfc6neiEntry ntry = new ipIfc6neiEntry();
        for (int i = cache.size() - 1; i >= 0; i--) {
            ntry = cache.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.stat) {
                continue;
            }
            if ((currTim - ntry.time) < neiCacheTimeout) {
                continue;
            }
            delEntry(ntry);
        }
    }

    /**
     * get mac address
     *
     * @param adr ip address
     * @return mac address, null if not found
     */
    public addrMac getMACaddr(addrIPv6 adr) {
        ipIfc6neiEntry ntry = new ipIfc6neiEntry();
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
        ipIfc6neiEntry ntry = cache.get(seq);
        if (ntry == null) {
            return true;
        }
        adr.fromIPv6addr(ntry.ip);
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
            ipIfc6neiEntry ntry = cache.get(i);
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
     * update packet header
     *
     * @param mod mode
     * @param mac address to set
     * @param adr address to set
     */
    public void updateMACheader(int mod, addrMac mac, addrIPv6 adr) {
        ipIfc6neiEntry ntry = new ipIfc6neiEntry();
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
     * get list of neighbors
     *
     * @return list of entries
     */
    public userFormat getShCache() {
        userFormat lst = new userFormat("|", "mac|address|time|static|router");
        for (int i = 0; i < cache.size(); i++) {
            ipIfc6neiEntry ntry = cache.get(i);
            if (ntry == null) {
                continue;
            }
            lst.add(ntry.mac + "|" + ntry.ip + "|" + bits.timePast(ntry.time) + "|" + ntry.stat + "|" + ntry.router);
        }
        return lst;
    }

}

class ipIfc6neiTimer extends TimerTask {

    ipIfc6nei parent;

    public ipIfc6neiTimer(ipIfc6nei prnt) {
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

class ipIfc6neiEntry implements Comparator<ipIfc6neiEntry> {

    public addrMac mac;

    public addrIPv6 ip;

    public long time;

    public boolean stat;

    public boolean router;

    public String toString() {
        return mac + " " + ip + " " + bits.timePast(time);
    }

    public int compare(ipIfc6neiEntry o1, ipIfc6neiEntry o2) {
        return o1.ip.compare(o1.ip, o2.ip);
    }

}
