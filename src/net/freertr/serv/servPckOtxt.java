package net.freertr.serv;

import java.util.List;
import net.freertr.addr.addrEmpty;
import net.freertr.addr.addrType;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgIfc;
import net.freertr.enc.encBase64;
import net.freertr.ifc.ifcDn;
import net.freertr.ifc.ifcNull;
import net.freertr.ifc.ifcUp;
import net.freertr.pack.packHolder;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServS;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabRouteIface;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
import net.freertr.util.cmds;
import net.freertr.util.counter;
import net.freertr.util.logger;
import net.freertr.util.state;

/**
 * packet over dtls encapsulation server
 *
 * @author matecsaba
 */
public class servPckOtxt extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servPckOtxt() {
    }

    /**
     * port number
     */
    public static final int port = 2554;

    /**
     * interface to use
     */
    public cfgIfc clnIfc;

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        "server pckotxt .*! port " + port,
        "server pckotxt .*! protocol " + proto2string(protoAllStrm)
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
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
        return "pckotxt";
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
        new servPckOtxtConn(pipe, this);
        return false;
    }

}

class servPckOtxtConn implements Runnable, ifcDn {

    public pipeSide pipe;

    public servPckOtxt parent;

    public cfgIfc ifc;

    public counter cntr = new counter();

    public ifcUp upper = new ifcNull();

    public servPckOtxtConn(pipeSide pip, servPckOtxt lower) {
        pipe = pip;
        parent = lower;
        ifc = lower.clnIfc.cloneStart(this);
        pipe.lineRx = pipeSide.modTyp.modeCRorLF;
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        new Thread(this).start();
    }

    public void run() {
        try {
            packHolder pck = new packHolder(true, true);
            for (;;) {
                if (pipe.isClosed() != 0) {
                    break;
                }
                String s = pipe.lineGet(0x11);
                if (s == null) {
                    break;
                }
                if (s.length() < 1) {
                    continue;
                }
                byte[] buf = encBase64.decodeBytes(s);
                if (buf == null) {
                    continue;
                }
                pck.clear();
                pck.putCopy(buf, 0, 0, buf.length);
                pck.putSkip(buf.length);
                pck.merge2beg();
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
        pipe.linePut(encBase64.encodeBytes(pck.getCopy()));
    }

}
