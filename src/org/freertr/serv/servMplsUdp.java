package org.freertr.serv;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrMac;
import org.freertr.addr.addrType;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgIfc;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcUp;
import org.freertr.pack.packHolder;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtMplsIp;
import org.freertr.prt.prtServP;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabRouteIface;
import org.freertr.user.userFilter;
import org.freertr.user.userFormat;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.counter;
import org.freertr.util.state;

/**
 * mpls in udp (rfc7510) server
 *
 * @author matecsaba
 */
public class servMplsUdp extends servGeneric implements prtServP {

    /**
     * create instance
     */
    public servMplsUdp() {
    }

    /**
     * port number
     */
    public final static int portNum = 6635;

    /**
     * interface to use
     */
    public cfgIfc tempIfc;

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
     * list of connections
     */
    public tabGen<servMplsUdpConn> conns = new tabGen<servMplsUdpConn>();

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("server mplsudp .*", cmds.tabulator + "port " + portNum, null),
        new userFilter("server mplsudp .*", cmds.tabulator + "protocol " + proto2string(protoAllDgrm), null),
        new userFilter("server mplsudp .*", cmds.tabulator + "timeout 60000", null)
    };

    public userFilter[] srvDefFlt() {
        return defaultF;
    }

    /**
     * find one connection
     *
     * @param id connection id
     * @param create set true to create if none found
     * @return connection entry
     */
    public servMplsUdpConn connFind(prtGenConn id, boolean create) {
        servMplsUdpConn ntry = new servMplsUdpConn(id, this);
        if (!create) {
            return conns.find(ntry);
        }
        if (tempIfc == null) {
            return null;
        }
        servMplsUdpConn old = conns.add(ntry);
        if (old != null) {
            return old;
        }
        ntry.doStartup();
        return ntry;
    }

    /**
     * delete one connection
     *
     * @param id connection id
     * @return connection entry
     */
    public servMplsUdpConn connDel(prtGenConn id) {
        servMplsUdpConn ntry = new servMplsUdpConn(id, this);
        return conns.del(ntry);
    }

    public void srvShRun(String beg, List<String> l, int filter) {
        if (tempIfc == null) {
            l.add(beg + "no clone");
        } else {
            l.add(beg + "clone " + tempIfc.name);
        }
        l.add(beg + "timeout " + timeout);
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("timeout")) {
            timeout = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("clone")) {
            tempIfc = cfgAll.ifcFind(cmd.word(), 0);
            if (tempIfc == null) {
                cmd.error("no such interface");
                return false;
            }
            if (tempIfc.type != tabRouteIface.ifaceType.template) {
                cmd.error("not template interface");
                tempIfc = null;
                return false;
            }
            return false;
        }
        if (!s.equals(cmds.negated)) {
            return true;
        }
        s = cmd.word();
        if (s.equals("clone")) {
            tempIfc = null;
            return false;
        }
        return true;
    }

    public void srvHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "clone", "set interface to clone");
        l.add(null, false, 2, new int[]{-1}, "<name:ifc>", "name of interface");
        l.add(null, false, 1, new int[]{2}, "timeout", "timeout of client");
        l.add(null, false, 2, new int[]{-1}, "<num>", "milliseconds");
    }

    public String srvName() {
        return "mplsudp";
    }

    public int srvPort() {
        return portNum;
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
        servMplsUdpConn ntry = connDel(id);
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
        servMplsUdpConn ntry = connFind(id, false);
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
        servMplsUdpConn ntry = connFind(id, false);
        if (ntry == null) {
            id.setClosing();
            return false;
        }
        ntry.doRecv(pck);
        return false;
    }

    /**
     * get show
     *
     * @return result
     */
    public userFormat getShow() {
        userFormat res = new userFormat("|", "addr|port|iface|for|since");
        for (int i = 0; i < conns.size(); i++) {
            servMplsUdpConn ntry = conns.get(i);
            if (ntry == null) {
                continue;
            }
            res.add(ntry.conn.peerAddr + "|" + ntry.conn.portRem + "|" + ntry.acesIfc.name + "|" + bits.timePast(ntry.created) + "|" + bits.time2str(cfgAll.timeZoneName, ntry.created + cfgAll.timeServerOffset, 3));
        }
        return res;
    }

}

class servMplsUdpConn implements ifcDn, Comparable<servMplsUdpConn> {

    public prtGenConn conn;

    public servMplsUdp lower;

    public counter cntr = new counter();

    public ifcUp upper = new ifcNull();

    public cfgIfc acesIfc;

    public long created;

    public int compareTo(servMplsUdpConn o) {
        return conn.compareTo(o.conn);
    }

    public servMplsUdpConn(prtGenConn id, servMplsUdp parent) {
        conn = id;
        lower = parent;
    }

    public String toString() {
        return "mplsudp with " + conn.peerAddr;
    }

    public void doRecv(packHolder pck) {
        cntr.rx(pck);
        if (prtMplsIp.mpls2ethtyp(pck)) {
            return;
        }
        cntr.rx(pck);
        upper.recvPack(pck);
    }

    public void doStartup() {
        acesIfc = lower.tempIfc.cloneStart(this);
        setUpper(acesIfc.ethtyp);
        acesIfc.ethtyp.setState(state.states.up);
        created = bits.getTime();
    }

    public void sendPack(packHolder pck) {
        pck.merge2beg();
        if (prtMplsIp.ethtyp2mpls(pck)) {
            return;
        }
        cntr.tx(pck);
        pck.putDefaults();
        conn.send2net(pck);
    }

    public addrType getHwAddr() {
        return addrMac.getRandom();
    }

    public void setFilter(boolean promisc) {
    }

    public state.states getState() {
        return state.states.up;
    }

    public void closeDn() {
        acesIfc.cloneStop();
        lower.connDel(conn);
        conn.setClosing();
    }

    public void flapped() {
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

}
