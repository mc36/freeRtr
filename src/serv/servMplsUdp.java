package serv;

import addr.addrMac;
import addr.addrType;
import cfg.cfgAll;
import cfg.cfgIfc;
import ifc.ifcDn;
import ifc.ifcNull;
import ifc.ifcUp;
import java.util.Comparator;
import java.util.List;
import pack.packHolder;
import pipe.pipeSide;
import prt.prtGenConn;
import prt.prtMplsIp;
import prt.prtServP;
import tab.tabGen;
import user.userFilter;
import user.userHelping;
import util.bits;
import util.cmds;
import util.counter;
import util.state;

/**
 * mpls in udp (rfc7510) server
 *
 * @author matecsaba
 */
public class servMplsUdp extends servGeneric implements prtServP {

    /**
     * port number
     */
    public static final int portNum = 6635;

    /**
     * interface to use
     */
    public cfgIfc tempIfc;

    /**
     * timeout
     */
    public int timeout = 60000;

    /**
     * list of connections
     */
    public tabGen<servMplsUdpConn> conns = new tabGen<servMplsUdpConn>();

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "server mplsudp .*! port " + portNum,
        "server mplsudp .*! protocol " + proto2string(protoAllDgrm),
        "server mplsudp .*! timeout 60000"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
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

    public void srvShRun(String beg, List<String> l) {
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
            tempIfc = cfgAll.ifcFind(cmd.word(), false);
            if (tempIfc == null) {
                cmd.error("no such interface");
                return false;
            }
            if (tempIfc.type != cfgIfc.ifaceType.template) {
                cmd.error("not template interface");
                tempIfc = null;
                return false;
            }
            return false;
        }
        if (!s.equals("no")) {
            return true;
        }
        s = cmd.word();
        if (s.equals("clone")) {
            tempIfc = null;
            return false;
        }
        return true;
    }

    public void srvHelp(userHelping l) {
        l.add("1 2  clone                        set interface to clone");
        l.add("2 .    <name>                     name of interface");
        l.add("1 2  timeout                      timeout of client");
        l.add("2 .    <num>                      milliseconds");
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

}

class servMplsUdpConn implements ifcDn, Comparator<servMplsUdpConn> {

    public prtGenConn conn;

    public servMplsUdp lower;

    public counter cntr = new counter();

    public ifcUp upper = new ifcNull();

    public cfgIfc acesIfc;

    public int compare(servMplsUdpConn o1, servMplsUdpConn o2) {
        return o1.conn.compare(o1.conn, o2.conn);
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
