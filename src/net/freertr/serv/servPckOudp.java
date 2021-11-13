package net.freertr.serv;

import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgBrdg;
import net.freertr.cfg.cfgIfc;
import net.freertr.pack.packHolder;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServP;
import net.freertr.tab.tabGen;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
import net.freertr.util.cmds;
import net.freertr.util.counter;
import net.freertr.util.state;

/**
 * packet over udp encapsulation server
 *
 * @author matecsaba
 */
public class servPckOudp extends servGeneric implements prtServP {

    /**
     * create instance
     */
    public servPckOudp() {
    }

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
     * physical interface
     */
    public boolean physInt = false;

    /**
     * list of connections
     */
    public tabGen<servPckOudpConn> conns = new tabGen<servPckOudpConn>();

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        "server pckoudp .*! port " + port,
        "server pckoudp .*! protocol " + proto2string(protoAllDgrm),
        "server pckoudp .*! no physical-interface"
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
            ntry.brdgIfc = brdgIfc.bridgeHed.newIface(physInt, true, false);
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

    public void srvShRun(String beg, List<String> l, int filter) {
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
        cmds.cfgLine(l, !physInt, beg, "physical-interface", "");
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
        if (s.equals("physical-interface")) {
            physInt = true;
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
        if (s.equals("physical-interface")) {
            physInt = false;
            return false;
        }
        return true;
    }

    public void srvHelp(userHelping l) {
        l.add(null, "1 2  clone                        set interface to clone");
        l.add(null, "2 .    <name:ifc>                 name of interface");
        l.add(null, "1 2  bridge                       set interface to bridge");
        l.add(null, "2 .    <num>                      number of bridge");
        l.add(null, "1 .  physical-interface           adding as physical to bridge");
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
