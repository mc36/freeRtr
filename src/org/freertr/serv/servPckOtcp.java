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
import org.freertr.pack.packSize;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServS;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabRouteIface;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.util.cmds;
import org.freertr.util.counter;
import org.freertr.util.logger;
import org.freertr.util.state;

/**
 * packet over dtls encapsulation server
 *
 * @author matecsaba
 */
public class servPckOtcp extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servPckOtcp() {
    }

    /**
     * port number
     */
    public final static int port = 2554;

    /**
     * interface to use
     */
    public cfgIfc clnIfc;

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("server pckotcp .*", cmds.tabulator + "port " + port, null),
        new userFilter("server pckotcp .*", cmds.tabulator + "protocol " + proto2string(protoAllStrm), null)
    };

    public userFilter[] srvDefFlt() {
        return defaultF;
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
        return "pckotcp";
    }

    public int srvPort() {
        return port;
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
        id.timeout = 120000;
        new servPckOtcpConn(pipe, this);
        return false;
    }

}

class servPckOtcpConn implements Runnable, ifcDn {

    public pipeSide pipe;

    private packSize pips = null;

    public servPckOtcp parent;

    public cfgIfc ifc;

    public counter cntr = new counter();

    public ifcUp upper = new ifcNull();

    public servPckOtcpConn(pipeSide pip, servPckOtcp lower) {
        pipe = pip;
        parent = lower;
        pips = new packSize(pipe, 2, true, 1, 0);
        ifc = lower.clnIfc.cloneStart(this);
        new Thread(this).start();
    }

    public void run() {
        try {
            packHolder pck = new packHolder(true, true);
            for (;;) {
                if (pipe.isClosed() != 0) {
                    break;
                }
                pck.clear();
                if (pips.recvPacket(pck)) {
                    break;
                }
                upper.recvPack(pck);
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        closeDn();
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
        upper.closeUp();
        pipe.setClose();
        ifc.cloneStop();
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
        pck.putDefaults();
        pips.sendPacket(pck);
    }

}
