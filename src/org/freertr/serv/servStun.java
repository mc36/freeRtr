package org.freertr.serv;

import java.util.List;
import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrType;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgIfc;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcUp;
import org.freertr.pack.packHolder;
import org.freertr.pack.packStun;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServS;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabRouteIface;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.counter;
import org.freertr.util.logger;
import org.freertr.util.state;

/**
 * serial tunneling
 *
 * @author matecsaba
 */
public class servStun extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servStun() {
    }

    /**
     * group number
     */
    public int grpNum;

    /**
     * interface to use
     */
    public cfgIfc clnIfc;

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("server stun .*", cmds.tabulator + "port " + packStun.port, null),
        new userFilter("server stun .*", cmds.tabulator + "protocol " + proto2string(protoAllStrm), null)
    };

    public userFilter[] srvDefFlt() {
        return defaultF;
    }

    public void srvShRun(String beg, List<String> l, int filter) {
        l.add(beg + "group " + grpNum);
        if (clnIfc == null) {
            l.add(beg + "no clone");
        } else {
            l.add(beg + "clone " + clnIfc.name);
        }
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("group")) {
            grpNum = bits.str2num(cmd.word());
            return false;
        }
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
        if (s.equals("group")) {
            grpNum = 0;
            return false;
        }
        if (s.equals("clone")) {
            clnIfc = null;
            return false;
        }
        return true;
    }

    public void srvHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "group", "set group number");
        l.add(null, false, 2, new int[]{-1}, "<num>", "group number");
        l.add(null, false, 1, new int[]{2}, "clone", "set interface to clone");
        l.add(null, false, 2, new int[]{-1}, "<name:ifc>", "name of interface");
    }

    public String srvName() {
        return "stun";
    }

    public int srvPort() {
        return packStun.port;
    }

    public int srvProto() {
        return protoAllStrm;
    }

    public boolean srvInit() {
        return genStrmStart(this, new pipeLine(32768, false), 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        new servStunConn(this, pipe, grpNum);
        return false;
    }

}

class servStunConn implements Runnable, ifcDn {

    /**
     * counter of this interface
     */
    public counter cntr = new counter();

    private cfgIfc ifc;

    private ifcUp upper = new ifcNull();

    private packStun lower;

    public servStunConn(servStun parent, pipeSide conn, int grp) {
        if (parent.clnIfc == null) {
            conn.setClose();
            return;
        }
        lower = new packStun(conn, grp);
        ifc = parent.clnIfc.cloneStart(this);
        logger.startThread(this);
    }

    public void run() {
        try {
            doRun();
        } catch (Exception e) {
            logger.traceback(e);
        }
        lower.setClose();
        ifc.cloneStop();
    }

    private void doRun() {
        lower.setOpening();
        for (;;) {
            packHolder pck = lower.recvPack();
            if (pck == null) {
                break;
            }
            upper.recvPack(pck);
        }
    }

    public counter getCounter() {
        return cntr;
    }

    public void closeDn() {
        lower.setClose();
    }

    public void flapped() {
        lower.setClose();
    }

    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
    }

    public state.states getState() {
        return state.states.up;
    }

    public void setFilter(boolean promisc) {
    }

    public addrType getHwAddr() {
        return new addrEmpty();
    }

    public int getMTUsize() {
        return 1504;
    }

    public long getBandwidth() {
        return 8000000;
    }

    public String toString() {
        return "stun";
    }

    public void sendPack(packHolder pck) {
        pck.putDefaults();
        lower.sendPack(pck);
    }

}
