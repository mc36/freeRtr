package net.freertr.serv;

import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.auth.authLocal;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgIfc;
import net.freertr.pack.packHolder;
import net.freertr.pack.packL2tp2;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServP;
import net.freertr.tab.tabGen;
import net.freertr.user.userFilter;
import net.freertr.user.userFormat;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.counter;
import net.freertr.util.state;

/**
 * layer two tunneling protocol (rfc2661) server
 *
 * @author matecsaba
 */
public class servL2tp2 extends servGeneric implements prtServP {

    /**
     * create instance
     */
    public servL2tp2() {
    }

    /**
     * interface to use
     */
    public cfgIfc clnIfc;

    /**
     * password
     */
    public String password;

    /**
     * list of connections
     */
    public tabGen<servL2tp2conn> conns = new tabGen<servL2tp2conn>();

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        "server l2tp2 .*! port " + packL2tp2.port,
        "server l2tp2 .*! protocol " + proto2string(protoAllDgrm),
        "server l2tp2 .*! no password"
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
    public servL2tp2conn connFind(prtGenConn id, boolean create) {
        servL2tp2conn ntry = new servL2tp2conn(id, this);
        if (!create) {
            return conns.find(ntry);
        }
        servL2tp2conn old = conns.add(ntry);
        if (old != null) {
            return old;
        }
        ntry.created = bits.getTime();
        return ntry;
    }

    /**
     * delete one connection
     *
     * @param id connection id
     * @return connection entry
     */
    public servL2tp2conn connDel(prtGenConn id) {
        servL2tp2conn ntry = new servL2tp2conn(id, this);
        return conns.del(ntry);
    }

    public void srvShRun(String beg, List<String> l, int filter) {
        if (clnIfc == null) {
            l.add(beg + "no clone");
        } else {
            l.add(beg + "clone " + clnIfc.name);
        }
        cmds.cfgLine(l, password == null, cmds.tabulator, "password", authLocal.passwdEncode(password, (filter & 2) != 0));
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("clone")) {
            clnIfc = cfgAll.ifcFind(cmd.word(), false);
            if (clnIfc == null) {
                cmd.error("no such interface");
                return false;
            }
            if (clnIfc.type != cfgIfc.ifaceType.dialer) {
                cmd.error("not dialer interface");
                clnIfc = null;
                return false;
            }
            return false;
        }
        if (s.equals("password")) {
            password = authLocal.passwdDecode(cmd.getRemaining());
            return false;
        }
        if (!s.equals("no")) {
            return true;
        }
        s = cmd.word();
        if (s.equals("clone")) {
            clnIfc = null;
            return false;
        }
        if (s.equals("password")) {
            password = null;
            return false;
        }
        return true;
    }

    public void srvHelp(userHelping l) {
        l.add(null, "1 2  clone                        set interface to clone");
        l.add(null, "2 .    <name:ifc>                 name of interface");
        l.add(null, "1 2  password                     set password");
        l.add(null, "2 .    <str>                      password");
    }

    public String srvName() {
        return "l2tp2";
    }

    public int srvPort() {
        return packL2tp2.port;
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
        servL2tp2conn ntry = connDel(id);
        if (ntry == null) {
            return;
        }
        ntry.setClosed();
    }

    /**
     * work connection
     *
     * @param id connection
     */
    public void datagramWork(prtGenConn id) {
        servL2tp2conn ntry = connFind(id, false);
        if (ntry == null) {
            id.setClosing();
            return;
        }
        ntry.doWork();
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
        servL2tp2conn ntry = connFind(id, false);
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
        userFormat res = new userFormat("|", "addr|port|tunloc|tunrem|sess|for|since");
        for (int i = 0; i < conns.size(); i++) {
            servL2tp2conn ntry = conns.get(i);
            if (ntry == null) {
                continue;
            }
            res.add(ntry.conn.peerAddr + "|" + ntry.conn.portRem + "|" + ntry.tunLoc + "|" + ntry.tunRem + "|" + ntry.session.size() + "|" + bits.timePast(ntry.created) + "|" + bits.time2str(cfgAll.timeZoneName, ntry.created + cfgAll.timeServerOffset, 3));
        }
        return res;
    }

}
