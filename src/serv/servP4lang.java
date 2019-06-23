package serv;

import java.util.Comparator;
import addr.addrIP;
import addr.addrPrefix;
import cfg.cfgAll;
import cfg.cfgIfc;
import cfg.cfgVrf;
import java.util.List;
import pipe.pipeDiscard;
import pipe.pipeShell;
import pipe.pipeSide;
import prt.prtGenConn;
import prt.prtServS;
import tab.tabGen;
import tab.tabRoute;
import tab.tabRouteEntry;
import user.userFilter;
import user.userHelping;
import util.cmds;
import util.debugger;
import util.logger;
import util.notifier;
import util.bits;

/**
 * p4lang
 *
 * @author matecsaba
 */
public class servP4lang extends servGeneric implements prtServS, Runnable {

    /**
     * port
     */
    public final static int port = 9090;

    /**
     * tx notifier
     */
    protected notifier notif = new notifier();

    /**
     * exported vrf
     */
    public cfgVrf expVrf;

    /**
     * exported interfaces
     */
    public tabGen<servP4langIfc1> expIfc = new tabGen<servP4langIfc1>();

    private boolean need2run;

    private tabRoute<addrIP> sent4 = new tabRoute<addrIP>("sent");

    private tabRoute<addrIP> sent6 = new tabRoute<addrIP>("sent");

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
            expVrf.fwd4.tableChanged = notif;
            expVrf.fwd6.tableChanged = notif;
            notif.wakeup();
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
            expIfc.put(ntry);
            notif.wakeup();
            return false;
        }
        if (!s.equals("no")) {
            return true;
        }
        s = cmd.word();
        if (s.equals("export-vrf")) {
            expVrf = null;
            notif.wakeup();
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
            notif.wakeup();
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
        need2run = true;
        new Thread(this).start();
        return false;
    }

    public boolean srvDeinit() {
        need2run = false;
        return false;
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        return true;
    }

    public void run() {
        try {
            for (;;) {
                if (!need2run) {
                    break;
                }
                notif.sleep(1000);
                doRound();
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

    private void doRound() {
        if (expVrf == null) {
            return;
        }
        doTable(true, expVrf.fwd4.actualU, sent4, "tbl_ipv4_fib_host", "act_ipv4_fib_hit");
        doTable(false, expVrf.fwd6.actualU, sent6, "tbl_ipv6_fib_host", "act_ipv6_fib_hit");
    }

    private void doTable(boolean ipv4, tabRoute<addrIP> need, tabRoute<addrIP> done, String tab, String act) {
        for (int i = 0; i < need.size(); i++) {
            tabRouteEntry<addrIP> ntry = need.get(i);
            if (done.find(ntry) != null) {
                continue;
            }
            done.add(tabRoute.addType.notyet, ntry, true, true);
            String a;
            if (ipv4) {
                a = "" + addrPrefix.ip2ip4(ntry.prefix);
            } else {
                a = "" + addrPrefix.ip2ip6(ntry.prefix);
            }
            a = "simple_switch_CLI --thrift-port 9090 table_add " + tab + " " + act + " " + a + " => " + ntry.nextHop;
            if (debugger.servP4langTraf) {
                logger.debug(a);
            }
            pipeShell.exec(pipeDiscard.needAny(null), a, "", true, true);
        }
    }

}

class servP4langIfc1 implements Comparator<servP4langIfc1> {

    public int id;

    public cfgIfc ifc;

    public int compare(servP4langIfc1 o1, servP4langIfc1 o2) {
        if (o1.id < o2.id) {
            return -1;
        }
        if (o1.id > o2.id) {
            return +1;
        }
        return 0;
    }

}
