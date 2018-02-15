package serv;

import addr.addrIP;
import cfg.cfgAll;
import cfg.cfgIfc;
import ip.ipFwdIface;
import java.util.Comparator;
import java.util.List;
import pipe.pipeConnect;
import pipe.pipeLine;
import pipe.pipeSide;
import prt.prtGen;
import prt.prtGenConn;
import prt.prtServS;
import tab.tabGen;
import user.userFilter;
import user.userHelping;
import util.bits;
import util.cmds;
import util.logger;

/**
 * load balancer
 *
 * @author matecsaba
 */
public class servLoadBalancer extends servGeneric implements prtServS {

    /**
     * port number
     */
    public static final int port = 1;

    /**
     * source interface
     */
    public cfgIfc originate;

    /**
     * timeout on connection
     */
    public int timeOut = 60 * 1000;

    /**
     * buffer size
     */
    public int bufSiz = 65536;

    /**
     * logging
     */
    public boolean logging = false;

    /**
     * list of servers
     */
    public tabGen<servLoadBalancerEntry> servLst = new tabGen<servLoadBalancerEntry>();

    /**
     * next server
     */
    public int servNxt = 0;

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "server loadbalancer .*! port " + port,
        "server loadbalancer .*! protocol " + proto2string(protoAllStrm),
        "server loadbalancer .*! no source",
        "server loadbalancer .*! timeout 60000",
        "server loadbalancer .*! buffer 65536",
        "server loadbalancer .*! no logging",};

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public void srvShRun(String beg, List<String> l) {
        cmds.cfgLine(l, !logging, beg, "logging", "");
        if (originate == null) {
            l.add(beg + "no source");
        } else {
            l.add(beg + "source " + originate.name);
        }
        for (int i = 0; i < servLst.size(); i++) {
            l.add(beg + "server " + servLst.get(i));
        }
        l.add(beg + "timeout " + timeOut);
        l.add(beg + "buffer " + bufSiz);
    }

    public boolean srvCfgStr(cmds cmd) {
        String a = cmd.word();
        if (a.equals("logging")) {
            logging = true;
            return false;
        }
        if (a.equals("timeout")) {
            timeOut = bits.str2num(cmd.word());
            return false;
        }
        if (a.equals("buffer")) {
            bufSiz = bits.str2num(cmd.word());
            return false;
        }
        if (a.equals("source")) {
            cfgIfc i = cfgAll.ifcFind(cmd.word(), false);
            if (i == null) {
                cmd.error("no such interface");
                return false;
            }
            originate = i;
            return false;
        }
        if (a.equals("server")) {
            servLoadBalancerEntry ntry = new servLoadBalancerEntry();
            ntry.num = bits.str2num(cmd.word());
            if (ntry.addr.fromString(cmd.word())) {
                return true;
            }
            ntry.port = bits.str2num(cmd.word());
            servLst.put(ntry);
            return false;
        }
        if (!a.equals("no")) {
            return true;
        }
        a = cmd.word();
        if (a.equals("logging")) {
            logging = false;
            return false;
        }
        if (a.equals("source")) {
            originate = null;
            return false;
        }
        if (a.equals("server")) {
            servLoadBalancerEntry ntry = new servLoadBalancerEntry();
            ntry.num = bits.str2num(cmd.word());
            servLst.del(ntry);
            return false;
        }
        return false;
    }

    public void srvHelp(userHelping l) {
        l.add("1 .  logging                      set logging");
        l.add("1 2  timeout                      set timeout on connection");
        l.add("2 .    <num>                      timeout in ms");
        l.add("1 2  buffer                       set buffer size on connection");
        l.add("2 .    <num>                      buffer in bytes");
        l.add("1 2  source                       set source interface");
        l.add("2 .    <name>                     name of interface");
        l.add("1 2  server                       name of server");
        l.add("2 3    <num>                      number of server");
        l.add("3 4      <addr>                   address of server");
        l.add("4 .        <port>                 port on server");
    }

    public String srvName() {
        return "loadbalancer";
    }

    public int srvPort() {
        return port;
    }

    public int srvProto() {
        return protoAllStrm;
    }

    public boolean srvInit() {
        dynBlckMod = true;
        return genStrmStart(this, new pipeLine(bufSiz, false), 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        if (logging) {
            logger.info("connection from " + id.peerAddr);
        }
        pipe.timeout = timeOut;
        new servLoadBalancerDoer(this, pipe);
        return false;
    }

    /**
     * start one connection
     *
     * @param con1 incoming connection
     * @return false on success, true on error
     */
    public boolean doConnStart(pipeSide con1) {
        con1.timeout = timeOut;
        con1.wait4ready(timeOut);
        servLoadBalancerEntry ntry = null;
        int o = servLst.size();
        if (o < 1) {
            return true;
        }
        long p = bits.getTime();
        for (int i = 0; i < o; i++) {
            servNxt = (servNxt + 1) % o;
            ntry = servLst.get(servNxt);
            if (ntry.bad == 0) {
                break;
            }
            if ((p - ntry.bad) < timeOut) {
                ntry = null;
                continue;
            }
            ntry.bad = 0;
            break;
        }
        if (ntry == null) {
            return true;
        }
        prtGen prt = getProtocol(srvVrf, srvProto, ntry.addr);
        if (prt == null) {
            return true;
        }
        ipFwdIface ifc = null;
        if (originate != null) {
            ifc = originate.getFwdIfc(ntry.addr);
        }
        pipeSide con2 = prt.streamConnect(new pipeLine(bufSiz, con1.isBlockMode()), ifc, 0, ntry.addr, ntry.port, srvName(), null, -1);
        if (con2 == null) {
            ntry.bad = bits.getTime();
            return true;
        }
        con2.timeout = timeOut;
        if (con2.wait4ready(timeOut)) {
            ntry.bad = bits.getTime();
            return true;
        }
        pipeConnect.connect(con1, con2, true);
        return false;
    }

}

class servLoadBalancerEntry implements Comparator<servLoadBalancerEntry> {

    public int num;

    public addrIP addr = new addrIP();

    public int port;

    public long bad = 0;

    public String toString() {
        return num + " " + addr + " " + port;
    }

    public int compare(servLoadBalancerEntry o1, servLoadBalancerEntry o2) {
        if (o1.num < o2.num) {
            return -1;
        }
        if (o1.num > o2.num) {
            return +1;
        }
        return 0;
    }

}

class servLoadBalancerDoer implements Runnable {

    private pipeSide pipe;

    private servLoadBalancer parent;

    public servLoadBalancerDoer(servLoadBalancer prnt, pipeSide stream) {
        parent = prnt;
        pipe = stream;
        new Thread(this).start();
    }

    public void run() {
        try {
            if (parent.doConnStart(pipe)) {
                pipe.setClose();
            }
        } catch (Exception e) {
            pipe.setClose();
            logger.traceback(e);
        }
    }

}
