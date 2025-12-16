package org.freertr.serv;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgBrdg;
import org.freertr.pack.packHolder;
import org.freertr.pack.packVxlan;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServP;
import org.freertr.tab.tabGen;
import org.freertr.user.userFilter;
import org.freertr.user.userFormat;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.counter;
import org.freertr.util.state;

/**
 * vxlan (rfc7348) server
 *
 * @author matecsaba
 */
public class servVxlan extends servGeneric implements prtServP {

    /**
     * create instance
     */
    public servVxlan() {
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
    public final static userFilter[] defaultF = {
        new userFilter("server vxlan .*", cmds.tabulator + "port " + packVxlan.port, null),
        new userFilter("server vxlan .*", cmds.tabulator + "protocol " + proto2string(protoAllDgrm), null),
        new userFilter("server vxlan .*", cmds.tabulator + cmds.negated + cmds.tabulator + "physical-interface", null)
    };

    public userFilter[] srvDefFlt() {
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
        ntry.brdgIfc = brdgIfc.bridgeHed.newIface(physInt, true, false);
        ntry.setUpper(ntry.brdgIfc);
        ntry.created = bits.getTime();
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

    public void srvShRun(String beg, List<String> l, int filter) {
        if (brdgIfc == null) {
            l.add(beg + "no bridge");
        } else {
            l.add(beg + "bridge " + brdgIfc.number);
        }
        cmds.cfgLine(l, !physInt, beg, "physical-interface", "");
        l.add(beg + "instance " + inst);
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
        l.add(null, false, 1, new int[]{2}, "instance", "set instance id");
        l.add(null, false, 2, new int[]{-1}, "<num>", "instance id");
        l.add(null, false, 1, new int[]{-1}, "physical-interface", "adding as physical to bridge");
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
        ntry.cntr.rx(pck);
        ntry.upper.recvPack(pck);
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
            servVxlanConn ntry = conns.get(i);
            if (ntry == null) {
                continue;
            }
            res.add(ntry.conn.peerAddr + "|" + ntry.conn.portRem + "|" + ntry.brdgIfc.getIfcName() + "|" + bits.timePast(ntry.created) + "|" + bits.time2str(cfgAll.timeZoneName, ntry.created + cfgAll.timeServerOffset, 3));
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
            servVxlanConn ntry = conns.get(i);
            if (ntry == null) {
                continue;
            }
            if (peer.compareTo(ntry.conn.peerAddr) != 0) {
                continue;
            }
            ntry.closeDn();
        }
    }

}
