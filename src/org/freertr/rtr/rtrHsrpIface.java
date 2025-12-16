package org.freertr.rtr;

import org.freertr.addr.addrIP;
import org.freertr.addr.addrMac;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgTrack;
import org.freertr.ip.ipFwdIface;
import org.freertr.pack.packHolder;
import org.freertr.pack.packHsrp;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServP;
import org.freertr.prt.prtUdp;
import org.freertr.tab.tabGen;
import org.freertr.user.userFormat;
import org.freertr.util.bits;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.state;

/**
 * hot standby router protocol (rfc2281) interface
 *
 * @author matecsaba
 */
public class rtrHsrpIface implements prtServP {

    /**
     * udp handler
     */
    protected prtUdp udp;

    /**
     * ip interface
     */
    protected ipFwdIface ifc;

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
    public int version = 2;

    /**
     * hello interval
     */
    public int hello = 3000;

    /**
     * hold interval
     */
    public int hold = 10000;

    /**
     * priority
     */
    public int priority = 100;

    /**
     * authentication data
     */
    public String authen = "cisco";

    /**
     * preemption
     */
    public boolean preempt;

    /**
     * tracker to watch
     */
    public String trackR;

    /**
     * decrement on down
     */
    public int trackD;

    /**
     * bfd enabled
     */
    public boolean bfdTrigger;

    /**
     * used ip version
     */
    public final boolean ipv4;

    /**
     * list of neighbors
     */
    protected final tabGen<rtrHsrpNeigh> neighs;

    /**
     * last state sent
     */
    protected int lastStat;

    /**
     * last opcode sent
     */
    protected int lastOpcod;

    private prtGenConn conn;

    private addrMac ident;

    private long started;

    /**
     * create one instance
     *
     * @param dgrm udp handler
     * @param iface the ip interface to work on
     */
    public rtrHsrpIface(prtUdp dgrm, ipFwdIface iface) {
        udp = dgrm;
        ifc = iface;
        ipv4 = iface.addr.isIPv4();
        neighs = new tabGen<rtrHsrpNeigh>();
        ident = addrMac.getRandom();
        started = bits.getTime();
    }

    public String toString() {
        return "hsrp on " + ifc;
    }

    /**
     * generate packet holder
     *
     * @return entry
     */
    public packHsrp genPackHolder() {
        packHsrp pck = new packHsrp();
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
    public rtrHsrpNeigh genLocalNeigh() {
        rtrHsrpNeigh ntry = new rtrHsrpNeigh(this, ifc.addr);
        ntry.priority = priority;
        ntry.state = lastStat;
        ntry.opcode = lastOpcod;
        ntry.upTime = started;
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
        ifc.adrDel(ip);
        started = bits.getTime();
    }

    /**
     * register to udp
     */
    public void register2udp() {
        if (debugger.rtrHsrpEvnt) {
            logger.debug("starting on " + ifc);
        }
        packHsrp pck = genPackHolder();
        int prt = pck.getPortNum();
        conn = udp.packetConnect(this, ifc, prt, pck.genIpAddr(), prt, "hsrp", -1, null, -1, -1);
        if (conn == null) {
            return;
        }
        conn.timeout = 0;
        conn.workInterval = hello;
        udp.packetListen(this, ifc, genPackHolder().getPortNum(), null, 0, "hsrp", -1, null, -1, -1);
        resetState();
    }

    /**
     * unregister from udp
     */
    public void unregister2udp() {
        if (debugger.rtrHsrpEvnt) {
            logger.debug("stopping on " + ifc);
        }
        ifc.adrDel(ip);
        udp.listenStop(ifc, genPackHolder().getPortNum(), null, 0);
        conn.setClosing();
    }

    /**
     * list neighbors
     *
     * @param l list to append
     */
    public void getShNeighs(userFormat l) {
        l.add(ifc + "|" + genLocalNeigh().getShSum());
        for (int i = 0; i < neighs.size(); i++) {
            rtrHsrpNeigh nei = neighs.get(i);
            if (nei == null) {
                continue;
            }
            l.add(ifc + "|" + nei.getShSum());
        }
    }

    /**
     * close interface
     *
     * @param ifc interface
     */
    public void closedInterface(ipFwdIface ifc) {
    }

    /**
     * start connection
     *
     * @param id connection
     * @return false if success, true if error
     */
    public boolean datagramAccept(prtGenConn id) {
        id.timeout = hello;
        id.workInterval = hello;
        return false;
    }

    /**
     * connection ready
     *
     * @param id connection
     */
    public void datagramReady(prtGenConn id) {
    }

    /**
     * stop connection
     *
     * @param id connection
     */
    public void datagramClosed(prtGenConn id) {
    }

    /**
     * work connection
     *
     * @param id connection
     */
    public void datagramWork(prtGenConn id) {
        if (conn == null) {
            return;
        }
        if (id.compareTo(conn) != 0) {
            return;
        }
        sendHello();
    }

    /**
     * received error
     *
     * @param id connection
     * @param pck packet
     * @param rtr reporting router
     * @param err error happened
     * @param lab error label
     * @return false on success, true on error
     */
    public boolean datagramError(prtGenConn id, packHolder pck, addrIP rtr, counter.reasons err, int lab) {
        return false;
    }

    /**
     * notified that state changed
     *
     * @param id id number to reference connection
     * @param stat state
     * @return return false if successful, true if error happened
     */
    public boolean datagramState(prtGenConn id, state.states stat) {
        return false;
    }

    /**
     * received packet
     *
     * @param id connection
     * @param pck packet
     * @return false if success, true if error
     */
    public boolean datagramRecv(prtGenConn id, packHolder pck) {
        id.setClosing();
        if (!ifc.network.matches(id.peerAddr)) {
            logger.info("got from out of subnet peer " + id);
            return true;
        }
        packHsrp hsr = genPackHolder();
        if (hsr.parsePacket(pck)) {
            return false;
        }
        if (debugger.rtrHsrpTraf) {
            logger.debug("rx from " + id.peerAddr + " " + hsr);
        }
        if (hsr.group != group) {
            return false;
        }
        if (!authen.equals(hsr.authen)) {
            return false;
        }
        rtrHsrpNeigh nei = new rtrHsrpNeigh(this, id.peerAddr);
        rtrHsrpNeigh old = neighs.add(nei);
        if (old != null) {
            nei = old;
        } else {
            nei.upTime = bits.getTime();
            logger.warn("neighbor " + nei.peer + " up");
            if (bfdTrigger) {
                ifc.bfdAdd(id.peerAddr, nei, "hsrp");
            }
        }
        nei.time = bits.getTime();
        nei.state = hsr.state;
        nei.opcode = hsr.opcod;
        nei.priority = hsr.priority;
        return false;
    }

    /**
     * send advertisement
     */
    protected synchronized void sendHello() {
        int currStat = getCurrStat();
        lastOpcod = packHsrp.opHello;
        if (currStat != lastStat) {
            logger.warn("hsrp " + ip + " changed to " + packHsrp.state2string(currStat));
            if (currStat == packHsrp.staActv) {
                lastOpcod = packHsrp.opCoup;
                ifc.adrAdd(ip, mac, false);
            }
            if (lastStat == packHsrp.staActv) {
                lastOpcod = packHsrp.opResign;
                ifc.adrDel(ip);
            }
        }
        lastStat = currStat;
        conn.workInterval = hello;
        packHsrp pckH = genPackHolder();
        pckH.virtual = ip.copyBytes();
        pckH.authen = "" + authen;
        pckH.hello = hello;
        pckH.hold = hold;
        pckH.priority = priority;
        pckH.ident = ident.copyBytes();
        pckH.virtual = ip.copyBytes();
        pckH.state = lastStat;
        pckH.opcod = lastOpcod;
        packHolder pckB = new packHolder(true, true);
        pckH.createPacket(pckB);
        pckB.merge2beg();
        conn.send2net(pckB);
        if (debugger.rtrHsrpTraf) {
            logger.debug("tx " + pckH);
        }
    }

    private int getCurrStat() {
        long tim = bits.getTime();
        rtrHsrpNeigh actv = null;
        rtrHsrpNeigh stby = null;
        for (int i = neighs.size() - 1; i >= 0; i--) {
            rtrHsrpNeigh ntry = neighs.get(i);
            if ((tim - ntry.time) > hold) {
                logger.error("neighbor " + ntry.peer + " down");
                neighs.del(ntry);
                ifc.bfdDel(ntry.peer, ntry);
                continue;
            }
            if (ntry.state == packHsrp.staActv) {
                actv = ntry;
            }
            if (ntry.state == packHsrp.staStby) {
                stby = ntry;
            }
        }
        if (debugger.rtrHsrpEvnt) {
            logger.debug("found: active=" + actv + " standby=" + stby);
        }
        if ((tim - started) < hold) {
            return packHsrp.staSpk;
        }
        int stat = packHsrp.staSpk;
        if (stby == null) {
            stat = packHsrp.staStby;
        } else if (stby.isWinner(genLocalNeigh()) < 0) {
            stat = packHsrp.staStby;
        }
        if (actv == null) {
            if (lastStat == packHsrp.staStby) {
                stat = packHsrp.staActv;
            }
            if (lastStat == packHsrp.staActv) {
                stat = packHsrp.staActv;
            }
        } else if (preempt && (actv.isWinner(genLocalNeigh()) < 0)) {
            stat = packHsrp.staActv;
        }
        return stat;
    }

}

class rtrHsrpNeigh implements Comparable<rtrHsrpNeigh>, rtrBfdClnt {

    /**
     * parent
     */
    public final rtrHsrpIface lower;

    /**
     * address of peer
     */
    public final addrIP peer;

    /**
     * last state
     */
    public int state;

    /**
     * last opcode
     */
    public int opcode;

    /**
     * last priority
     */
    public int priority;

    /**
     * time of activity
     */
    public long time;

    /**
     * uptime
     */
    public long upTime;

    /**
     * create one neighbor
     *
     * @param adr address of peer
     */
    public rtrHsrpNeigh(rtrHsrpIface parent, addrIP adr) {
        lower = parent;
        peer = adr.copyBytes();
    }

    public String toString() {
        return "hsrp with " + peer;
    }

    public int compareTo(rtrHsrpNeigh o) {
        return peer.compareTo(o.peer);
    }

    /**
     * check if winner against
     *
     * @param cmp competitor
     * @return +1 if winner, 0 if neutral, -1 if loser
     */
    public int isWinner(rtrHsrpNeigh cmp) {
        if (priority > cmp.priority) {
            return +1;
        }
        if (priority < cmp.priority) {
            return -1;
        }
        return peer.compareTo(cmp.peer);
    }

    /**
     * get show result
     *
     * @return result
     */
    public String getShSum() {
        return peer + "|" + packHsrp.state2string(state) + "|" + priority + "|" + bits.timePast(upTime);
    }

    public void bfdPeerDown() {
        time = 0;
        lower.sendHello();
    }

}
