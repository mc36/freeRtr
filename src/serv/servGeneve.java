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
import pack.packGeneve;
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
 * generic network virtualization encapsulation (rfcXXXX) client
 *
 * @author matecsaba
 */
public class servGeneve extends servGeneric implements prtServP {

    /**
     * interface to use
     */
    public cfgBrdg brdgIfc;

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
    public final static String defaultL[] = {
        "server geneve .*! port " + packGeneve.port,
        "server geneve .*! protocol " + proto2string(protoAllDgrm),};

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
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
    public servGeneveConn connDel(prtGenConn id) {
        servGeneveConn ntry = new servGeneveConn(id, this);
        return conns.del(ntry);
    }

    public void srvShRun(String beg, List<String> l) {
        if (brdgIfc == null) {
            l.add(beg + "no bridge");
        } else {
            l.add(beg + "bridge " + brdgIfc.name);
        }
        l.add(cmds.tabulator + "vni " + vni);
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
        l.add("1 2  vni                          set virtual network id");
        l.add("2 .    <num>                      net id");
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

class servGeneveConn implements ifcDn, Comparator<servGeneveConn> {

    public prtGenConn conn;

    public servGeneve lower;

    public ifcBridgeIfc brdgIfc;

    public ifcUp upper = new ifcNull();

    public counter cntr = new counter();

    public int compare(servGeneveConn o1, servGeneveConn o2) {
        return o1.conn.compare(o1.conn, o2.conn);
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
