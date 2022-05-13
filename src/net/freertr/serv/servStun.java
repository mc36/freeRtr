package net.freertr.serv;

import java.util.List;
import net.freertr.addr.addrEmpty;
import net.freertr.addr.addrType;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgIfc;
import net.freertr.ifc.ifcDn;
import net.freertr.ifc.ifcNull;
import net.freertr.ifc.ifcUp;
import net.freertr.pack.packHolder;
import net.freertr.pack.packStun;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServS;
import net.freertr.tab.tabGen;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.counter;
import net.freertr.util.logger;
import net.freertr.util.state;

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
    public final static String[] defaultL = {
        "server stun .*! port " + packStun.port,
        "server stun .*! protocol " + proto2string(protoAllStrm)
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
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

    public void srvHelp(userHelping l) {
        l.add(null, "1 2  group                        set group number");
        l.add(null, "2 .    <num>                      group number");
        l.add(null, "1 2  clone                        set interface to clone");
        l.add(null, "2 .    <name:ifc>                 name of interface");
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
        new Thread(this).start();
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
