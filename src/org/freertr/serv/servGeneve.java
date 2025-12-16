package org.freertr.serv;

import java.util.List;
import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrType;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgBrdg;
import org.freertr.ifc.ifcBridgeIfc;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcUp;
import org.freertr.pack.packGeneve;
import org.freertr.pack.packHolder;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServP;
import org.freertr.tab.tabGen;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.counter;
import org.freertr.util.state;

/**
 * generic network virtualization encapsulation (rfc8926) client
 *
 * @author matecsaba
 */
public class servGeneve extends servGeneric implements prtServP {

    /**
     * create instance
     */
    public servGeneve() {
    }

    /**
     * interface to use
     */
    public cfgBrdg brdgIfc;

    /**
     * sending ttl value, -1 means maps out
     */
    public int sendingTTL = 255;

    /**
     * sending tos value, -1 means maps out
     */
    public int sendingTOS = -1;

    /**
     * sending df value, -1 means maps out
     */
    public int sendingDFN = -1;

    /**
     * sending flow value, -1 means maps out
     */
    public int sendingFLW = -1;

    /**
     * timeout
     */
    public int timeout = 120000;

    /**
     * physical interface
     */
    public boolean physInt = false;

    /**
     * virtual network id
     */
    public int vni;

    /**
     * list of connections
     */
    public tabGen<servGeneveConn> conns = new tabGen<servGeneveConn>();

    /**
     * counter
     */
    public counter cntr;

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("server geneve .*", cmds.tabulator + "port " + packGeneve.port, null),
        new userFilter("server geneve .*", cmds.tabulator + "protocol " + proto2string(protoAllDgrm), null),
        new userFilter("server geneve .*", cmds.tabulator + cmds.negated + cmds.tabulator + "physical-interface", null)
    };

    public userFilter[] srvDefFlt() {
        return defaultF;
    }

    public String toString() {
        return "geneve";
    }

    /**
     * find one connection
     *
     * @param id connection id
     * @param create set true to create if none found
     * @return connection entry
     */
    public servGeneveConn connFind(prtGenConn id, boolean create) {
        servGeneveConn ntry = new servGeneveConn(id, this);
        if (!create) {
            return conns.find(ntry);
        }
        servGeneveConn old = conns.add(ntry);
        if (old != null) {
            return old;
        }
        ntry.brdgIfc = brdgIfc.bridgeHed.newIface(physInt, true, false);
        ntry.setUpper(ntry.brdgIfc);
        return ntry;
    }

    /**
     * delete one connection
     *
     * @param id connection id
     * @return connection entry
     */
    public servGeneveConn connDel(prtGenConn id) {
        servGeneveConn ntry = new servGeneveConn(id, this);
        return conns.del(ntry);
    }

    public void srvShRun(String beg, List<String> l, int filter) {
        if (brdgIfc == null) {
            l.add(beg + "no bridge");
        } else {
            l.add(beg + "bridge " + brdgIfc.number);
        }
        cmds.cfgLine(l, !physInt, beg, "physical-interface", "");
        l.add(beg + "vni " + vni);
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("bridge")) {
            brdgIfc = cfgAll.brdgFind(cmd.word(), false);
            if (brdgIfc == null) {
                cmd.error("no such bridge group");
                return false;
            }
            return false;
        }
        if (s.equals("vni")) {
            vni = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("physical-interface")) {
            physInt = true;
            return false;
        }
        if (!s.equals(cmds.negated)) {
            return true;
        }
        s = cmd.word();
        if (s.equals("bridge")) {
            brdgIfc = null;
            return false;
        }
        if (s.equals("physical-interface")) {
            physInt = false;
            return false;
        }
        return true;
    }

    public void srvHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "bridge", "set interface to clone");
        l.add(null, false, 2, new int[]{-1}, "<num>", "number of bridge");
        l.add(null, false, 1, new int[]{2}, "vni", "set virtual network id");
        l.add(null, false, 2, new int[]{-1}, "<num>", "net id");
        l.add(null, false, 1, new int[]{-1}, "physical-interface", "adding as physical to bridge");
    }

    public String srvName() {
        return "geneve";
    }

    public int srvPort() {
        return packGeneve.port;
    }

    public int srvProto() {
        return protoAllDgrm;
    }

    public boolean srvInit() {
        return genDgrmStart(this, 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        id.timeout = timeout;
        id.sendTTL = sendingTTL;
        id.sendTOS = sendingTOS;
        id.sendDFN = sendingDFN;
        id.sendFLW = sendingFLW;
        connFind(id, true);
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
        servGeneveConn ntry = connDel(id);
        if (ntry == null) {
            return;
        }
        ntry.closeDn();
    }

    /**
     * work connection
     *
     * @param id connection
     */
    public void datagramWork(prtGenConn id) {
        servGeneveConn ntry = connFind(id, false);
        if (ntry == null) {
            id.setClosing();
            return;
        }
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
     * @return false on success, true on error
     */
    public boolean datagramRecv(prtGenConn id, packHolder pck) {
        servGeneveConn ntry = connFind(id, false);
        if (ntry == null) {
            id.setClosing();
            return false;
        }
        packGeneve gnv = new packGeneve();
        if (gnv.parseHeader(pck)) {
            return false;
        }
        if (gnv.vni != vni) {
            return false;
        }
        ntry.upper.recvPack(pck);
        return false;
    }

}

class servGeneveConn implements ifcDn, Comparable<servGeneveConn> {

    public prtGenConn conn;

    public servGeneve lower;

    public ifcBridgeIfc brdgIfc;

    public ifcUp upper = new ifcNull();

    public counter cntr = new counter();

    public int compareTo(servGeneveConn o) {
        return conn.compareTo(o.conn);
    }

    public String toString() {
        return lower + " with " + conn.peerAddr;
    }

    public servGeneveConn(prtGenConn id, servGeneve parent) {
        conn = id;
        lower = parent;
    }

    public addrType getHwAddr() {
        return new addrEmpty();
    }

    public void setFilter(boolean promisc) {
    }

    public state.states getState() {
        return state.states.up;
    }

    public void closeDn() {
        lower.connDel(conn);
        if (upper != null) {
            upper.closeUp();
            upper = null;
        }
        if (brdgIfc != null) {
            brdgIfc.closeUp();
            brdgIfc = null;
        }
    }

    public void flapped() {
        closeDn();
    }

    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
    }

    public counter getCounter() {
        return cntr;
    }

    public int getMTUsize() {
        return 1400;
    }

    public long getBandwidth() {
        return 8000000;
    }

    public void sendPack(packHolder pck) {
        packGeneve gnv = new packGeneve();
        gnv.vni = lower.vni;
        gnv.createHeader(pck);
        pck.merge2beg();
        pck.putDefaults();
        conn.send2net(pck);
    }

}
