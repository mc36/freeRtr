package net.freertr.serv;

import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgIfc;
import net.freertr.pack.packGtp;
import net.freertr.pack.packHolder;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServP;
import net.freertr.tab.tabGen;
import net.freertr.user.userFilter;
import net.freertr.user.userFormat;
import net.freertr.user.userHelping;
import net.freertr.util.cmds;
import net.freertr.util.counter;
import net.freertr.util.state;

/**
 * gprs tunneling protocol (3gpp29060) server
 *
 * @author matecsaba
 */
public class servGtp extends servGeneric implements prtServP {

    /**
     * create instance
     */
    public servGtp() {
    }

    /**
     * interface to use
     */
    public cfgIfc clnIfc;

    /**
     * list of connections
     */
    public tabGen<servGtpConn> conns = new tabGen<servGtpConn>();

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        "server gtp .*! port " + packGtp.portCtrl,
        "server gtp .*! protocol " + proto2string(protoAllDgrm)
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
     * @param id peer id
     * @param create set true to create if none found
     * @return connection entry
     */
    public servGtpConn connFind(addrIP id, boolean create) {
        servGtpConn ntry = new servGtpConn(id, this);
        if (!create) {
            return conns.find(ntry);
        }
        servGtpConn old = conns.add(ntry);
        if (old != null) {
            ntry = old;
        }
        return ntry;
    }

    /**
     * delete one connection
     *
     * @param id peer id
     * @return connection entry
     */
    public servGtpConn connDel(addrIP id) {
        servGtpConn ntry = new servGtpConn(id, this);
        return conns.del(ntry);
    }

    public void srvShRun(String beg, List<String> l, int filter) {
        if (clnIfc == null) {
            l.add(beg + "no clone");
        } else {
            l.add(beg + "clone " + clnIfc.name);
        }
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
        if (!s.equals("no")) {
            return true;
        }
        s = cmd.word();
        if (s.equals("clone")) {
            clnIfc = null;
            return false;
        }
        return true;
    }

    public void srvHelp(userHelping l) {
        l.add(null, "1 2  clone                        set interface to clone");
        l.add(null, "2 .    <name:ifc>                 name of interface");
    }

    public String srvName() {
        return "gtp";
    }

    public int srvPort() {
        return packGtp.portCtrl;
    }

    public int srvProto() {
        return protoAllDgrm;
    }

    public boolean srvInit() {
        if (genDgrmStart(this, srvPort + (packGtp.portData - packGtp.portCtrl))) {
            return true;
        }
        return genDgrmStart(this, 0);
    }

    public boolean srvDeinit() {
        if (genericStop(srvPort + (packGtp.portData - packGtp.portCtrl))) {
            return true;
        }
        return genericStop(0);
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        id.timeout = 120000;
        boolean ctrl = id.portLoc == srvPort;
        servGtpConn ntry = connFind(id.peerAddr, ctrl);
        if (ntry == null) {
            return true;
        }
        if (ctrl) {
            if (ntry.connC != null) {
                ntry.connC.setClosing();
            }
            ntry.connC = id;
        } else {
            if (ntry.connD != null) {
                ntry.connD.setClosing();
            }
            ntry.connD = id;
        }
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
        servGtpConn ntry = connDel(id.peerAddr);
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
        servGtpConn ntry = connFind(id.peerAddr, false);
        if (ntry == null) {
            id.setClosing();
            return;
        }
        ntry.doWork(id);
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
        servGtpConn ntry = connFind(id.peerAddr, false);
        if (ntry == null) {
            id.setClosing();
            return false;
        }
        int ctrl = -1;
        if (ntry.connC != null) {
            if (id.compare(id, ntry.connC) == 0) {
                ctrl = 1;
            }
        }
        if (ntry.connD != null) {
            if (id.compare(id, ntry.connD) == 0) {
                ctrl = 2;
            }
        }
        if (ctrl < 0) {
            id.setClosing();
            return true;
        }
        ntry.doRecv(pck, ctrl == 1);
        return false;
    }

    /**
     * get show
     *
     * @return result
     */
    public userFormat getShow() {
        userFormat res = new userFormat("|", "addr|sess");
        for (int i = 0; i < conns.size(); i++) {
            servGtpConn ntry = conns.get(i);
            if (ntry == null) {
                continue;
            }
            res.add(ntry.peer + "|" + ntry.session.size());
        }
        return res;
    }

}
