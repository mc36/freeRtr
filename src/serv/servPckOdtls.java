package serv;

import addr.addrEmpty;
import addr.addrType;
import cfg.cfgAll;
import cfg.cfgIfc;
import ifc.ifcDn;
import ifc.ifcNull;
import ifc.ifcUp;
import java.util.List;
import pack.packHolder;
import pipe.pipeLine;
import pipe.pipeSide;
import prt.prtGenConn;
import prt.prtServS;
import tab.tabGen;
import user.userFilter;
import user.userHelping;
import util.cmds;
import util.counter;
import util.logger;
import util.state;

/**
 * packet over dtls encapsulation server
 *
 * @author matecsaba
 */
public class servPckOdtls extends servGeneric implements prtServS {

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
    public final static String defaultL[] = {
        "server pckodtls .*! port " + port,
        "server pckodtls .*! protocol " + proto2string(protoAllDgrm)
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public void srvShRun(String beg, List<String> l) {
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
        l.add("1 2  clone                        set interface to clone");
        l.add("2 .    <name>                     name of interface");
    }

    public String srvName() {
        return "pckodtls";
    }

    public int srvPort() {
        return port;
    }

    public int srvProto() {
        return protoAllDgrm;
    }

    public boolean srvInit() {
        return genStrmStart(this, new pipeLine(32768, true), 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        id.timeout = 120000;
        new servPckOdtlsConn(pipe, this);
        return false;
    }

}

class servPckOdtlsConn implements Runnable, ifcDn {

    public pipeSide pipe;

    public servPckOdtls parent;

    public cfgIfc ifc;

    public counter cntr = new counter();

    public ifcUp upper = new ifcNull();

    public servPckOdtlsConn(pipeSide pip, servPckOdtls lower) {
        pipe = pip;
        parent = lower;
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
                if (pck.pipeRecv(pipe, 0, 8192, 143) < 1) {
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
        pck.pipeSend(pipe, 0, pck.dataSize(), 2);
    }

}
