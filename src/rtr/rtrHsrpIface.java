package rtr;

import addr.addrIP;
import addr.addrMac;
import cfg.cfgAll;
import cfg.cfgTrack;
import ip.ipFwdIface;
import java.util.Comparator;
import pack.packHolder;
import pack.packHsrp;
import prt.prtGenConn;
import prt.prtServP;
import prt.prtUdp;
import tab.tabGen;
import user.userFormat;
import util.bits;
import util.debugger;
import util.logger;

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
        rtrHsrpNeigh ntry = new rtrHsrpNeigh(ifc.addr);
        ntry.priority = priority;
        ntry.state = lastStat;
        ntry.opcode = lastOpcod;
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
        conn = udp.packetConnect(this, ifc, prt, pck.genIpAddr(), prt, "hsrp", null, -1);
        if (conn == null) {
            return;
        }
        conn.timeout = 0;
        conn.workInterval = hello;
        udp.packetListen(this, ifc, genPackHolder().getPortNum(), null, 0, 0, "hsrp", null, -1);
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
        udp.listenStop(ifc, genPackHolder().getPortNum(), null, 0, 0);
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
        if (id.compare(id, conn) != 0) {
            return;
        }
        int currStat = getCurrStat();
        lastOpcod = packHsrp.opHello;
        if (currStat != lastStat) {
            logger.warn("hsrp " + ip + " changed to " + packHsrp.state2string(currStat));
            if (currStat == packHsrp.staActv) {
                lastOpcod = packHsrp.opCoup;
                ifc.adrAdd(ip, mac);
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

    /**
     * received packet
     *
     * @param id connection
     * @param pck packet
     * @return false if success, true if error
     */
    public boolean datagramRecv(prtGenConn id, packHolder pck) {
        id.setClosing();
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
        rtrHsrpNeigh nei = new rtrHsrpNeigh(id.peerAddr);
        nei.time = bits.getTime();
        nei.state = hsr.state;
        nei.opcode = hsr.opcod;
        nei.priority = hsr.priority;
        neighs.put(nei);
        return false;
    }

    private int getCurrStat() {
        long tim = bits.getTime();
        rtrHsrpNeigh actv = null;
        rtrHsrpNeigh stby = null;
        for (int i = neighs.size() - 1; i >= 0; i--) {
            rtrHsrpNeigh ntry = neighs.get(i);
            if ((tim - ntry.time) > hold) {
                neighs.del(ntry);
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

class rtrHsrpNeigh implements Comparator<rtrHsrpNeigh> {

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
     * create one neighbor
     *
     * @param adr address of peer
     */
    public rtrHsrpNeigh(addrIP adr) {
        peer = adr.copyBytes();
    }

    public String toString() {
        return "hsrp with " + peer;
    }

    public int compare(rtrHsrpNeigh o1, rtrHsrpNeigh o2) {
        return o1.peer.compare(o1.peer, o2.peer);
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
        return peer.compare(peer, cmp.peer);
    }

    /**
     * get show result
     *
     * @return result
     */
    public String getShSum() {
        return peer + "|" + packHsrp.state2string(state) + "|" + priority;
    }

}
