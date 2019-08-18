package rtr;

import addr.addrIP;
import addr.addrMac;
import cfg.cfgAll;
import cfg.cfgTrack;
import ip.ipFwd;
import ip.ipFwdIface;
import ip.ipPrt;
import java.util.Comparator;
import java.util.Timer;
import java.util.TimerTask;
import pack.packHolder;
import pack.packVrrp;
import tab.tabGen;
import user.userFormat;
import util.bits;
import util.counter;
import util.debugger;
import util.logger;
import util.state;

/**
 * virtual router redundancy protocol (rfc5798) interface
 *
 * @author matecsaba
 */
public class rtrVrrpIface implements ipPrt {

    /**
     * virtual ip address
     */
    public addrIP ip = new addrIP();

    /**
     * virtual mac address
     */
    public addrMac mac = new addrMac();

    /**
     * group number
     */
    public int group = 0;

    /**
     * version number
     */
    public int version = 3;

    /**
     * hello interval
     */
    public int hello = 1000;

    /**
     * hold interval
     */
    public int hold = 3000;

    /**
     * priority
     */
    public int priority = 100;

    /**
     * tracker to watch
     */
    public String trackR;

    /**
     * decrement on down
     */
    public int trackD;

    /**
     * used ip version
     */
    public final boolean ipv4;

    /**
     * list of neighbors
     */
    protected final tabGen<rtrVrrpNeigh> neighs;

    /**
     * last state sent
     */
    protected int lastStat;

    private final static int stLstn = 1;

    private final static int stMstr = 2;

    private final static int stBckp = 3;

    private long started;

    private ipFwd fwdCore;

    private ipFwdIface fwdIfc;

    private counter cntr = new counter();

    private Timer keepTimer;

    /**
     * create one instance
     *
     * @param fwdr forwarder to use
     * @param iface interface to use
     */
    public rtrVrrpIface(ipFwd fwdr, ipFwdIface iface) {
        fwdCore = fwdr;
        fwdIfc = iface;
        ipv4 = iface.addr.isIPv4();
        neighs = new tabGen<rtrVrrpNeigh>();
        started = bits.getTime();
    }

    private static String state2string(int i) {
        switch (i) {
            case stLstn:
                return "listen";
            case stMstr:
                return "master";
            case stBckp:
                return "backup";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * generate packet holder
     *
     * @return entry
     */
    public packVrrp genPackHolder() {
        packVrrp pck = new packVrrp();
        pck.group = group;
        pck.ipv4 = ipv4;
        pck.version = version;
        return pck;
    }

    /**
     * generate local as neighbor
     *
     * @return entry
     */
    public rtrVrrpNeigh genLocalNeigh() {
        rtrVrrpNeigh ntry = new rtrVrrpNeigh(fwdIfc.addr);
        ntry.priority = priority;
        if (trackD < 1) {
            return ntry;
        }
        cfgTrack res = cfgAll.trackFind(trackR, false);
        if (res == null) {
            return ntry;
        }
        if (res.worker.getStatus()) {
            return ntry;
        }
        ntry.priority -= trackD;
        return ntry;
    }

    /**
     * reset state
     */
    public void resetState() {
        fwdIfc.adrDel(ip);
        started = bits.getTime();
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
        if (hello < 1) {
            return;
        }
        keepTimer = new Timer();
        rtrVrrpIfaceHello task = new rtrVrrpIfaceHello(this);
        keepTimer.schedule(task, 500, hello);
    }

    /**
     * register protocol
     */
    public void register2ip() {
        if (debugger.rtrVrrpEvnt) {
            logger.debug("reg to " + fwdIfc);
        }
        fwdCore.protoAdd(this, fwdIfc, null);
    }

    /**
     * unregister protocol
     */
    public void unregister2ip() {
        if (debugger.rtrVrrpEvnt) {
            logger.debug("unreg from " + fwdIfc);
        }
        fwdIfc.adrDel(ip);
        fwdCore.protoDel(this, fwdIfc, null);
    }

    /**
     * list neighbors
     *
     * @param l list to append
     */
    public void getShNeighs(userFormat l) {
        l.add(fwdIfc + "|" + genLocalNeigh().getShSum());
        for (int i = 0; i < neighs.size(); i++) {
            rtrVrrpNeigh nei = neighs.get(i);
            if (nei == null) {
                continue;
            }
            l.add(fwdIfc + "|" + nei.getShSum());
        }
    }

    /**
     * get protocol number
     *
     * @return number
     */
    public int getProtoNum() {
        return packVrrp.proto;
    }

    public String toString() {
        return "vrrp";
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
     * @param pck packet
     */
    public void recvPack(ipFwdIface rxIfc, packHolder pck) {
        packVrrp hsr = genPackHolder();
        if (hsr.parsePacket(pck)) {
            return;
        }
        if (debugger.rtrVrrpTraf) {
            logger.debug("rx from " + pck.IPsrc + " " + hsr);
        }
        if (hsr.group != group) {
            return;
        }
        rtrVrrpNeigh nei = new rtrVrrpNeigh(pck.IPsrc);
        nei.time = bits.getTime();
        nei.priority = hsr.priority;
        neighs.put(nei);
        return;
    }

    /**
     * alert packet
     *
     * @param rxIfc interface
     * @param pck packet
     * @return false on success, true on error
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

    private int getCurrStat() {
        long tim = bits.getTime();
        rtrVrrpNeigh actv = genLocalNeigh();
        for (int i = neighs.size() - 1; i >= 0; i--) {
            rtrVrrpNeigh ntry = neighs.get(i);
            if ((tim - ntry.time) > hold) {
                neighs.del(ntry);
                continue;
            }
            if (ntry.isWinner(actv) < 0) {
                continue;
            }
            actv = ntry;
        }
        if (debugger.rtrVrrpEvnt) {
            logger.debug("found: master=" + actv);
        }
        if ((tim - started) < hold) {
            return stLstn;
        }
        if (actv.isWinner(genLocalNeigh()) == 0) {
            return stMstr;
        } else {
            return stBckp;
        }
    }

    /**
     * send advertisement
     */
    protected void sendHello() {
        int currStat = getCurrStat();
        if (currStat != lastStat) {
            logger.warn("vrrp " + ip + " changed to " + state2string(currStat));
            if (currStat == stMstr) {
                fwdIfc.adrAdd(ip, mac);
            }
            if (lastStat == stMstr) {
                fwdIfc.adrDel(ip);
            }
        }
        lastStat = currStat;
        if (currStat == stLstn) {
            return;
        }
        packVrrp pckH = genPackHolder();
        pckH.virtual = ip.copyBytes();
        pckH.hello = hello;
        pckH.priority = priority;
        pckH.virtual = ip.copyBytes();
        pckH.type = packVrrp.typAdvert;
        packHolder pckB = new packHolder(true, true);
        pckH.createPacket(pckB, fwdIfc);
        pckB.merge2beg();
        fwdCore.protoPack(fwdIfc, pckB);
        if (debugger.rtrVrrpTraf) {
            logger.debug("tx " + pckH);
        }
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

class rtrVrrpNeigh implements Comparator<rtrVrrpNeigh> {

    /**
     * address of peer
     */
    public final addrIP peer;

    /**
     * last priority
     */
    public int priority;

    /**
     * time of activity
     */
    public long time;

    /**
     * create one neighbor
     *
     * @param adr address of peer
     */
    public rtrVrrpNeigh(addrIP adr) {
        peer = adr.copyBytes();
    }

    public String toString() {
        return "vrrp with " + peer;
    }

    public int compare(rtrVrrpNeigh o1, rtrVrrpNeigh o2) {
        return o1.peer.compare(o1.peer, o2.peer);
    }

    /**
     * check if winner against
     *
     * @param cmp competitor
     * @return +1 if winner, 0 if neutral, -1 if loser
     */
    public int isWinner(rtrVrrpNeigh cmp) {
        if (priority > cmp.priority) {
            return +1;
        }
        if (priority < cmp.priority) {
            return -1;
        }
        return peer.compare(peer, cmp.peer);
    }

    /**
     * get show result
     *
     * @return result
     */
    public String getShSum() {
        return peer + "|" + priority;
    }

}

class rtrVrrpIfaceHello extends TimerTask {

    private rtrVrrpIface lower;

    public rtrVrrpIfaceHello(rtrVrrpIface parent) {
        lower = parent;
    }

    public void run() {
        try {
            lower.sendHello();
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}
