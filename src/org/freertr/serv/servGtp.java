package org.freertr.serv;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgIfc;
import org.freertr.pack.packGtp;
import org.freertr.pack.packHolder;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
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
    public tabGen<servGtpConn> conns = new tabGen<servGtpConn>();

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("server gtp .*", cmds.tabulator + "port " + packGtp.portCtrl, null),
        new userFilter("server gtp .*", cmds.tabulator + "protocol " + proto2string(protoAllDgrm), null)
    };

    public userFilter[] srvDefFlt() {
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
            return old;
        }
        ntry.created = bits.getTime();
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
            clnIfc = cfgAll.ifcFind(cmd.word(), 0);
            if (clnIfc == null) {
                cmd.error("no such interface");
                return false;
            }
            if (clnIfc.type != tabRouteIface.ifaceType.dialer) {
                cmd.error("not dialer interface");
                clnIfc = null;
                return false;
            }
            return false;
        }
        if (!s.equals(cmds.negated)) {
            return true;
        }
        s = cmd.word();
        if (s.equals("clone")) {
            clnIfc = null;
            return false;
        }
        return true;
    }

    public void srvHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "clone", "set interface to clone");
        l.add(null, false, 2, new int[]{-1}, "<name:ifc>", "name of interface");
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

    private int getDataPort() {
        return srvPort + packGtp.portData - packGtp.portCtrl;
    }

    public boolean srvInit() {
        if (genDgrmStart(this, getDataPort())) {
            return true;
        }
        return genDgrmStart(this, 0);
    }

    public boolean srvDeinit() {
        if (genericStop(getDataPort())) {
            return true;
        }
        return genericStop(0);
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        id.timeout = timeout;
        id.sendTTL = sendingTTL;
        id.sendTOS = sendingTOS;
        id.sendDFN = sendingDFN;
        id.sendFLW = sendingFLW;
        boolean ctrl = id.portLoc == srvPort;
        servGtpConn ntry = connFind(id.peerAddr, ctrl);
        if (ntry == null) {
            return true;
        }
        if (!ctrl) {
            if (ntry.connD != null) {
                ntry.connD.setClosing();
            }
            ntry.connD = id;
            return false;
        }
        if (ntry.connC != null) {
            ntry.connC.setClosing();
        }
        ntry.connC = id;
        if (ntry.connD != null) {
            return false;
        }
        id = srvVrf.getUdp(id.peerAddr).packetConnect(this, id.iface, getDataPort(), id.peerAddr, getDataPort(), srvName(), -1, null, -1, -1);
        if (id == null) {
            return false;
        }
        id.timeout = timeout;
        id.sendTTL = sendingTTL;
        id.sendTOS = sendingTOS;
        id.sendDFN = sendingDFN;
        id.sendFLW = sendingFLW;
        ntry.connD = id;
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
            if (id.compareTo(ntry.connC) == 0) {
                ctrl = 1;
            }
        }
        if (ntry.connD != null) {
            if (id.compareTo(ntry.connD) == 0) {
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
        userFormat res = new userFormat("|", "addr|sess|for|since");
        for (int i = 0; i < conns.size(); i++) {
            servGtpConn ntry = conns.get(i);
            if (ntry == null) {
                continue;
            }
            res.add(ntry.peer + "|" + ntry.session.size() + "|" + bits.timePast(ntry.created) + "|" + bits.time2str(cfgAll.timeZoneName, ntry.created + cfgAll.timeServerOffset, 3));
        }
        return res;
    }

    /**
     * do clear
     *
     * @param peer peer ip
     */
    public void doClear(addrIP peer) {
        for (int i = 0; i < conns.size(); i++) {
            servGtpConn ntry = conns.get(i);
            if (ntry == null) {
                continue;
            }
            if (peer.compareTo(ntry.peer) != 0) {
                continue;
            }
            ntry.setClosed();
        }
    }

}
