package serv;

import addr.addrEmpty;
import addr.addrType;
import cfg.cfgAll;
import cfg.cfgBrdg;
import cfg.cfgIfc;
import ifc.ifcBridgeIfc;
import ifc.ifcDn;
import ifc.ifcNull;
import ifc.ifcUp;
import java.util.Comparator;
import java.util.List;
import pack.packHolder;
import pipe.pipeSide;
import prt.prtGenConn;
import prt.prtServP;
import tab.tabGen;
import user.userFilter;
import user.userHelping;
import util.cmds;
import util.counter;
import util.state;

/**
 * packet over udp encapsulation server
 *
 * @author matecsaba
 */
public class servPckOudp extends servGeneric implements prtServP {

    /**
     * port number
     */
    public static final int port = 2554;

    /**
     * interface to use
     */
    public cfgIfc dialIfc;

    /**
     * interface to use
     */
    public cfgBrdg brdgIfc;

    /**
     * list of connections
     */
    public tabGen<servPckOudpConn> conns = new tabGen<servPckOudpConn>();

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "server pckoudp .*! port " + port,
        "server pckoudp .*! protocol " + proto2string(protoAllDgrm),};

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
    public servPckOudpConn connFind(prtGenConn id, boolean create) {
        servPckOudpConn ntry = new servPckOudpConn(id, this);
        if (!create) {
            return conns.find(ntry);
        }
        servPckOudpConn old = conns.add(ntry);
        if (old != null) {
            return old;
        }
        if (dialIfc != null) {
            ntry.dialIfc = dialIfc.cloneStart(ntry);
            return ntry;
        }
        if (brdgIfc != null) {
            ntry.brdgIfc = brdgIfc.bridgeHed.newIface(false, true, false);
            ntry.setUpper(ntry.brdgIfc);
            return ntry;
        }
        conns.del(ntry);
        return null;
    }

    /**
     * delete one connection
     *
     * @param id connection id
     * @return connection entry
     */
    public servPckOudpConn connDel(prtGenConn id) {
        return conns.del(new servPckOudpConn(id, this));
    }

    public void srvShRun(String beg, List<String> l) {
        if (dialIfc == null) {
            l.add(beg + "no clone");
        } else {
            l.add(beg + "clone " + dialIfc.name);
        }
        if (brdgIfc == null) {
            l.add(beg + "no bridge");
        } else {
            l.add(beg + "bridge " + brdgIfc.name);
        }
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("clone")) {
            dialIfc = cfgAll.ifcFind(cmd.word(), false);
            if (dialIfc == null) {
                cmd.error("no such interface");
                return false;
            }
            if (dialIfc.type != cfgIfc.ifaceType.dialer) {
                cmd.error("not dialer interface");
                dialIfc = null;
                return false;
            }
            return false;
        }
        if (s.equals("bridge")) {
            brdgIfc = cfgAll.brdgFind(cmd.word(), false);
            if (brdgIfc == null) {
                cmd.error("no such bridge group");
                return false;
            }
            return false;
        }
        if (!s.equals("no")) {
            return true;
        }
        s = cmd.word();
        if (s.equals("clone")) {
            dialIfc = null;
            return false;
        }
        if (s.equals("bridge")) {
            brdgIfc = null;
            return false;
        }
        return true;
    }

    public void srvHelp(userHelping l) {
        l.add("1 2  clone                        set interface to clone");
        l.add("2 .    <name>                     name of interface");
        l.add("1 2  bridge                       set interface to bridge");
        l.add("2 .    <name>                     name of interface");
    }

    public String srvName() {
        return "pckoudp";
    }

    public int srvPort() {
        return port;
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
        id.timeout = 120000;
        return connFind(id, true) == null;
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
        servPckOudpConn ntry = connDel(id);
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
        servPckOudpConn ntry = connFind(id, false);
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
        servPckOudpConn ntry = connFind(id, false);
        if (ntry == null) {
            id.setClosing();
            return false;
        }
        ntry.cntr.rx(pck);
        ntry.upper.recvPack(pck);
        return false;
    }

}

class servPckOudpConn implements ifcDn, Comparator<servPckOudpConn> {

    public prtGenConn conn;

    public servPckOudp lower;

    public counter cntr = new counter();

    public ifcUp upper = new ifcNull();

    public cfgIfc dialIfc;

    public ifcBridgeIfc brdgIfc;

    public String toString() {
        return "pckoudp with " + conn.peerAddr;
    }

    public servPckOudpConn(prtGenConn id, servPckOudp parent) {
        conn = id;
        lower = parent;
    }

    public int compare(servPckOudpConn o1, servPckOudpConn o2) {
        return o1.conn.compare(o1.conn, o2.conn);
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
        upper.closeUp();
        conn.setClosing();
        if (dialIfc != null) {
            dialIfc.cloneStop();
        }
        if (brdgIfc != null) {
            brdgIfc.closeUp();
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
        pck.merge2beg();
        cntr.tx(pck);
        pck.putDefaults();
        conn.send2net(pck);
    }

}
