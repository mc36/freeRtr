package serv;

import addr.addrEmpty;
import addr.addrType;
import cfg.cfgAll;
import cfg.cfgBrdg;
import ifc.ifcBridgeIfc;
import ifc.ifcDn;
import ifc.ifcNull;
import ifc.ifcUp;
import java.util.Comparator;
import java.util.List;
import pack.packHolder;
import pack.packVxlan;
import pipe.pipeSide;
import prt.prtGenConn;
import prt.prtServP;
import tab.tabGen;
import user.userFilter;
import user.userHelping;
import util.bits;
import util.cmds;
import util.counter;
import util.state;

/**
 * vxlan (rfc7348) server
 *
 * @author matecsaba
 */
public class servVxlan extends servGeneric implements prtServP {

    /**
     * interface to use
     */
    public cfgBrdg brdgIfc;

    /**
     * instance id
     */
    public int inst;

    /**
     * list of connections
     */
    public tabGen<servVxlanConn> conns = new tabGen<servVxlanConn>();

    /**
     * counter
     */
    public counter cntr;

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "server vxlan .*! port " + packVxlan.port,
        "server vxlan .*! protocol " + proto2string(protoAllDgrm),};

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public String toString() {
        return "vxlan";
    }

    /**
     * find one connection
     *
     * @param id connection id
     * @param create set true to create if none found
     * @return connection entry
     */
    public servVxlanConn connFind(prtGenConn id, boolean create) {
        servVxlanConn ntry = new servVxlanConn(id, this);
        if (!create) {
            return conns.find(ntry);
        }
        servVxlanConn old = conns.add(ntry);
        if (old != null) {
            return old;
        }
        ntry.brdgIfc = brdgIfc.bridgeHed.newIface(false, true, false);
        ntry.setUpper(ntry.brdgIfc);
        return ntry;
    }

    /**
     * delete one connection
     *
     * @param id connection id
     * @return connection entry
     */
    public servVxlanConn connDel(prtGenConn id) {
        servVxlanConn ntry = new servVxlanConn(id, this);
        return conns.del(ntry);
    }

    public void srvShRun(String beg, List<String> l) {
        if (brdgIfc == null) {
            l.add(beg + "no bridge");
        } else {
            l.add(beg + "bridge " + brdgIfc.name);
        }
        l.add(cmds.tabulator + "instance " + inst);
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
        if (s.equals("instance")) {
            inst = bits.str2num(cmd.word());
            return false;
        }
        if (!s.equals("no")) {
            return true;
        }
        s = cmd.word();
        if (s.equals("bridge")) {
            brdgIfc = null;
            return false;
        }
        return true;
    }

    public void srvHelp(userHelping l) {
        l.add("1 2  bridge                       set interface to clone");
        l.add("2 .    <name>                     name of interface");
        l.add("1 2  instance                     set instance id");
        l.add("2 .    <num>                      instance id");
    }

    public String srvName() {
        return "vxlan";
    }

    public int srvPort() {
        return packVxlan.port;
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
        servVxlanConn ntry = connDel(id);
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
        servVxlanConn ntry = connFind(id, false);
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
        servVxlanConn ntry = connFind(id, false);
        if (ntry == null) {
            id.setClosing();
            return false;
        }
        packVxlan vxl = new packVxlan();
        if (vxl.parseHeader(pck)) {
            return false;
        }
        if (vxl.instance != inst) {
            return false;
        }
        ntry.upper.recvPack(pck);
        return false;
    }

}

class servVxlanConn implements ifcDn, Comparator<servVxlanConn> {

    public prtGenConn conn;

    public servVxlan lower;

    public ifcBridgeIfc brdgIfc;

    public ifcUp upper = new ifcNull();

    public counter cntr = new counter();

    public int compare(servVxlanConn o1, servVxlanConn o2) {
        return o1.conn.compare(o1.conn, o2.conn);
    }

    public String toString() {
        return lower + " with " + conn.peerAddr;
    }

    public servVxlanConn(prtGenConn id, servVxlan parent) {
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
        packVxlan vxl = new packVxlan();
        vxl.instance = lower.inst;
        vxl.createHeader(pck);
        pck.merge2beg();
        pck.putDefaults();
        conn.send2net(pck);
    }

}
