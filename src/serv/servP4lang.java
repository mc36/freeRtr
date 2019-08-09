package serv;

import java.util.Comparator;
import addr.addrEmpty;
import addr.addrType;
import addr.addrIP;
import addr.addrPrefix;
import cfg.cfgAll;
import cfg.cfgIfc;
import cfg.cfgVrf;
import ifc.ifcUp;
import ifc.ifcDn;
import ifc.ifcEther;
import ifc.ifcNull;
import java.util.List;
import pack.packHolder;
import pipe.pipeLine;
import pipe.pipeSide;
import prt.prtGenConn;
import prt.prtServS;
import tab.tabGen;
import tab.tabRoute;
import tab.tabRouteEntry;
import user.userFilter;
import user.userHelping;
import util.cmds;
import util.counter;
import util.debugger;
import util.logger;
import util.state;
import util.bits;

/**
 * p4lang
 *
 * @author matecsaba
 */
public class servP4lang extends servGeneric implements prtServS {

    /**
     * port
     */
    public final static int port = 9090;

    /**
     * exported vrf
     */
    public cfgVrf expVrf;

    /**
     * exported interfaces
     */
    public tabGen<servP4langIfc1> expIfc = new tabGen<servP4langIfc1>();

    /**
     * last connection
     */
    protected servP4langConn conn;

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "server p4lang .*! port " + port,
        "server p4lang .*! protocol " + proto2string(protoAllStrm),};

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public void srvShRun(String beg, List<String> l) {
        if (expVrf == null) {
            l.add(beg + "no export-vrf");
        } else {
            l.add(beg + "export-vrf " + expVrf.name);
        }
        for (int i = 0; i < expIfc.size(); i++) {
            servP4langIfc1 ntry = expIfc.get(i);
            l.add(beg + "export-port " + ntry.ifc.name + " " + ntry.id);
        }
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("export-vrf")) {
            expVrf = cfgAll.vrfFind(cmd.word(), false);
            if (expVrf == null) {
                cmd.error("no such vrf");
                return false;
            }
            return false;
        }
        if (s.equals("export-port")) {
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
            if (ifc == null) {
                cmd.error("no such interface");
                return false;
            }
            if (ifc.type != cfgIfc.ifaceType.sdn) {
                cmd.error("not openflow interface");
                return false;
            }
            servP4langIfc1 ntry = new servP4langIfc1();
            ntry.id = bits.str2num(cmd.word());
            ntry.ifc = ifc;
            ntry.lower = this;
            expIfc.put(ntry);
            ntry.setUpper(ifc.ethtyp);
            return false;
        }
        if (!s.equals("no")) {
            return true;
        }
        s = cmd.word();
        if (s.equals("export-vrf")) {
            expVrf = null;
            return false;
        }
        if (s.equals("export-port")) {
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
            if (ifc == null) {
                cmd.error("no such interface");
                return false;
            }
            servP4langIfc1 ntry = new servP4langIfc1();
            ntry.id = bits.str2num(cmd.word());
            ntry.ifc = ifc;
            ntry = expIfc.del(ntry);
            ifcNull nul = new ifcNull();
            nul.setUpper(ifc.ethtyp);
            return false;
        }
        return true;
    }

    public void srvHelp(userHelping l) {
        l.add("1 2  export-vrf                specify vrf to export");
        l.add("2 .    <name>                  vrf name");
        l.add("1 2  export-port               specify port to export");
        l.add("2 3    <name>                  interface name");
        l.add("3 .      <num>                 openflow port number");
    }

    public String srvName() {
        return "p4lang";
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
        pipe.lineRx = pipeSide.modTyp.modeLF;
        pipe.lineTx = pipeSide.modTyp.modeLF;
        conn = new servP4langConn(pipe, this);
        return false;
    }

    /**
     * send line
     *
     * @param a line
     */
    protected synchronized void sendLine(String a) {
        if (debugger.servP4langTraf) {
            logger.debug(a);
        }
        conn.pipe.linePut(a);
    }

    /**
     * send packet
     *
     * @param id id
     * @param pckB binary
     */
    protected void sendPack(int id, packHolder pckB) {
        String a = "packet " + id + " ";
        byte[] data = pckB.getCopy();
        for (int i = 0; i < data.length; i++) {
            a += bits.toHexB(data[i]);
        }
        sendLine(a);
    }

}

class servP4langIfc1 implements ifcDn, Comparator<servP4langIfc1> {

    public servP4lang lower;

    public int id;

    public cfgIfc ifc;

    public ifcUp upper;

    public counter cntr = new counter();

    public int compare(servP4langIfc1 o1, servP4langIfc1 o2) {
        if (o1.id < o2.id) {
            return -1;
        }
        if (o1.id > o2.id) {
            return +1;
        }
        return 0;
    }

    public String toString() {
        return "p4lang port " + id;
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
    }

    public void flapped() {
    }

    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
    }

    public counter getCounter() {
        return cntr;
    }

    public int getMTUsize() {
        return 1500;
    }

    public long getBandwidth() {
        return 8000000;
    }

    public void sendPack(packHolder pck) {
        if (lower.conn == null) {
            return;
        }
        ifcEther.createETHheader(pck, false);
        lower.sendPack(id, pck);
    }

}

class servP4langConn implements Runnable {

    public pipeSide pipe;

    public servP4lang lower;

    public int keepalive;

    public tabRoute<addrIP> sent4 = new tabRoute<addrIP>("sent");

    public tabRoute<addrIP> sent6 = new tabRoute<addrIP>("sent");

    public servP4langConn(pipeSide pip, servP4lang upper) {
        pipe = pip;
        lower = upper;
        new Thread(this).start();
    }

    public void run() {
        try {
            for (;;) {
                if (doRound()) {
                    break;
                }
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

    private boolean doRound() {
        if (pipe.isClosed() != 0) {
            return true;
        }
        keepalive++;
        if (keepalive > 30) {
            String a = "keepalive";
            lower.sendLine(a);
            keepalive = 0;
        }
        if (lower.expVrf == null) {
            return false;
        }
        if (pipe.ready2rx() > 0) {
            String s = pipe.lineGet(0x11);
            if (debugger.servP4langTraf) {
                logger.debug(s);
            }
            cmds cmd = new cmds("p4lang", s);
            s = cmd.word();
            if (!s.equals("packet")) {
                return false;
            }
            servP4langIfc1 ntry = new servP4langIfc1();
            ntry.id = bits.str2num(cmd.word());
            ntry = lower.expIfc.find(ntry);
            if (ntry == null) {
                return false;
            }
            packHolder pck = new packHolder(true, true);
            s = cmd.getRemaining();
            for (;;) {
                if (s.length() < 2) {
                    break;
                }
                pck.putByte(0, bits.fromHex(s.substring(0, 2)));
                s = s.substring(2, s.length());
                pck.putSkip(1);
                pck.merge2end();
            }
            ifcEther.parseETHheader(pck, false);
            ntry.upper.recvPack(pck);
            return false;
        }
        doTable(true, lower.expVrf.fwd4.actualU, sent4);
        doTable(false, lower.expVrf.fwd6.actualU, sent6);
        bits.sleep(1000);
        return false;
    }

    private void doTable(boolean ipv4, tabRoute<addrIP> need, tabRoute<addrIP> done) {
        for (int i = 0; i < need.size(); i++) {
            tabRouteEntry<addrIP> ntry = need.get(i);
            if (ntry.nextHop == null) {
                continue;
            }
            tabRouteEntry<addrIP> old = done.find(ntry);
            if (old != null) {
                if (!ntry.differs(old)) {
                    continue;
                }
            }
            done.add(tabRoute.addType.notyet, ntry, true, true);
            String a;
            if (ipv4) {
                a = "" + addrPrefix.ip2ip4(ntry.prefix);
            } else {
                a = "" + addrPrefix.ip2ip6(ntry.prefix);
            }
            lower.sendLine("route_add " + a + " " + ntry.nextHop);
        }
        for (int i = 0; i < done.size(); i++) {
            tabRouteEntry<addrIP> ntry = done.get(i);
            if (need.find(ntry) != null) {
                continue;
            }
            done.del(ntry);
            String a;
            if (ipv4) {
                a = "" + addrPrefix.ip2ip4(ntry.prefix);
            } else {
                a = "" + addrPrefix.ip2ip6(ntry.prefix);
            }
            lower.sendLine("route_del " + a + " " + ntry.nextHop);
        }
    }

}
