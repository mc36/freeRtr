package net.freertr.serv;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgIfc;
import net.freertr.cfg.cfgVrf;
import net.freertr.ip.ipFwdIface;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGen;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServS;
import net.freertr.tab.tabGen;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.logger;

/**
 * traffic multiplexer
 *
 * @author matecsaba
 */
public class servMultiplexer extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servMultiplexer() {
    }

    /**
     * port number
     */
    public static final int port = 1;

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
     * connections
     */
    protected final List<servMultiplexerConn> conns = new ArrayList<servMultiplexerConn>();

    /**
     * targets
     */
    protected final tabGen<servMultiplexerTrgt> targets = new tabGen<servMultiplexerTrgt>();

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        "server multiplexer .*! port " + port,
        "server multiplexer .*! protocol " + proto2string(protoAllStrm),
        "server multiplexer .*! timeout 60000",
        "server multiplexer .*! buffer 65536",
        "server multiplexer .*! no logging",};

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public void srvShRun(String beg, List<String> l, int filter) {
        cmds.cfgLine(l, !logging, beg, "logging", "");
        l.add(beg + "timeout " + timeOut);
        l.add(beg + "buffer " + bufSiz);
        for (int i = 0; i < targets.size(); i++) {
            l.add(beg + "target " + targets.get(i));
        }
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
        if (a.equals("target")) {
            servMultiplexerTrgt ntry = new servMultiplexerTrgt();
            ntry.lower = this;
            ntry.num = bits.str2num(cmd.word());
            ntry.vrf = cfgAll.vrfFind(cmd.word(), false);
            if (ntry.vrf == null) {
                return true;
            }
            ntry.iface = cfgAll.ifcFind(cmd.word(), 0);
            if (ntry.iface == null) {
                return true;
            }
            if (ntry.addr.fromString(cmd.word())) {
                return true;
            }
            ntry.port = bits.str2num(cmd.word());
            for (;;) {
                a = cmd.word();
                if (a.length() < 1) {
                    break;
                }
                if (a.equals("logging")) {
                    ntry.logging = true;
                    continue;
                }
                if (a.equals("clear")) {
                    ntry.clear = true;
                    continue;
                }
                if (a.equals("nowait")) {
                    ntry.nowait = true;
                    continue;
                }
                if (a.equals("rx")) {
                    ntry.limit = 1;
                    continue;
                }
                if (a.equals("tx")) {
                    ntry.limit = 2;
                    continue;
                }
            }
            new Thread(ntry).start();
            servMultiplexerTrgt old = targets.put(ntry);
            if (old == null) {
                return false;
            }
            old.stopWork();
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
        if (a.equals("target")) {
            servMultiplexerTrgt ntry = new servMultiplexerTrgt();
            ntry.num = bits.str2num(cmd.word());
            servMultiplexerTrgt old = targets.del(ntry);
            if (old == null) {
                return false;
            }
            old.stopWork();
            return false;
        }
        return true;
    }

    public void srvHelp(userHelping l) {
        l.add(null, "1 .  logging                      set logging");
        l.add(null, "1 2  timeout                      set timeout on connection");
        l.add(null, "2 .    <num>                      timeout in ms");
        l.add(null, "1 2  buffer                       set buffer size on connection");
        l.add(null, "2 .    <num>                      buffer in bytes");
        l.add(null, "1 2  target                       name of server");
        l.add(null, "2 3    <num>                      number of target");
        l.add(null, "3 4      <name:vrf>               name of vrf");
        l.add(null, "4 5        <name:ifc>             name of interface");
        l.add(null, "5 6          <addr>               address of target");
        l.add(null, "6 7,.          <port>             port on target");
        l.add(null, "7 7,.            rx               only rx");
        l.add(null, "7 7,.            tx               only tx");
        l.add(null, "7 7,.            logging          set logging");
        l.add(null, "7 7,.            clear            clear clients on disconnect");
        l.add(null, "7 7,.            nowait           use nonblocking send");
    }

    public String srvName() {
        return "multiplexer";
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
        pipe.setTime(timeOut);
        servMultiplexerConn ntry = new servMultiplexerConn(this, id.peerAddr, pipe);
        new Thread(ntry).start();
        conns.add(ntry);
        return false;
    }

    /**
     * connection got data
     *
     * @param buf buffer
     * @param siz size
     */
    protected void connData(byte[] buf, int siz) {
        for (int i = 0; i < targets.size(); i++) {
            servMultiplexerTrgt ntry = targets.get(i);
            if (ntry == null) {
                continue;
            }
            if (!ntry.ready) {
                continue;
            }
            if (ntry.limit == 1) {
                continue;
            }
            if (ntry.nowait) {
                ntry.conn.nonBlockPut(buf, 0, siz);
            } else {
                ntry.conn.blockingPut(buf, 0, siz);
            }
        }
    }

    /**
     * connection got data
     *
     * @param buf buffer
     * @param siz size
     */
    protected void trgtData(byte[] buf, int siz) {
        for (int i = 0; i < conns.size(); i++) {
            servMultiplexerConn ntry = conns.get(i);
            if (ntry == null) {
                continue;
            }
            ntry.conn.blockingPut(buf, 0, siz);
        }
    }

    /**
     * close client connections
     */
    protected void clearClients() {
        for (int i = conns.size() - 1; i >= 0; i--) {
            servMultiplexerConn ntry = conns.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.conn == null) {
                continue;
            }
            ntry.conn.setClose();
        }
    }

}

class servMultiplexerConn implements Runnable {

    public servMultiplexer lower;

    public pipeSide conn;

    public addrIP peer;

    public servMultiplexerConn(servMultiplexer parent, addrIP addr, pipeSide pipe) {
        lower = parent;
        conn = pipe;
        peer = addr;
    }

    public void run() {
        if (lower.logging) {
            logger.info("connection from " + peer + " up");
        }
        try {
            byte[] buf = new byte[1024];
            for (;;) {
                if (conn.isClosed() != 0) {
                    break;
                }
                int siz = conn.ready2rx();
                if (siz < 1) {
                    siz = 1;
                }
                if (siz > buf.length) {
                    siz = buf.length;
                }
                if (conn.blockingGet(buf, 0, siz) != siz) {
                    break;
                }
                lower.connData(buf, siz);
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        lower.conns.remove(this);
        conn.setClose();
        if (lower.logging) {
            logger.info("connection from " + peer + " down");
        }
    }

}

class servMultiplexerTrgt implements Comparator<servMultiplexerTrgt>, Runnable {

    public servMultiplexer lower;

    public int num;

    public cfgVrf vrf;

    public cfgIfc iface;

    public addrIP addr = new addrIP();

    public int port;

    public boolean ready;

    public pipeSide conn;

    public boolean logging;

    public boolean clear;

    public boolean nowait;

    public int limit;

    public boolean need2run = true;

    public String toString() {
        String a = "";
        if (logging) {
            a += " logging";
        }
        if (clear) {
            a += " clear";
        }
        if (nowait) {
            a += " nowait";
        }
        switch (limit) {
            case 1:
                a += " rx";
                break;
            case 2:
                a += " tx";
                break;
        }
        return num + " " + vrf.name + " " + iface.name + " " + addr + " " + port + a;
    }

    public int compare(servMultiplexerTrgt o1, servMultiplexerTrgt o2) {
        if (o1.num < o2.num) {
            return -1;
        }
        if (o1.num > o2.num) {
            return +1;
        }
        return 0;
    }

    public void run() {
        try {
            for (;;) {
                if (doWork()) {
                    break;
                }
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

    public boolean doWork() {
        bits.sleep(1000);
        ready = false;
        if (!need2run) {
            return true;
        }
        prtGen prt = servMultiplexer.getProtocol(vrf, lower.srvProto, addr);
        if (prt == null) {
            return false;
        }
        ipFwdIface ifc = null;
        if (iface != null) {
            ifc = iface.getFwdIfc(addr);
        }
        conn = prt.streamConnect(new pipeLine(lower.bufSiz, false), ifc, 0, addr, port, lower.srvName(), null, -1);
        if (conn == null) {
            return false;
        }
        conn.setTime(lower.timeOut);
        if (conn.wait4ready(lower.timeOut)) {
            return false;
        }
        ready = true;
        if (logging) {
            logger.info("connection to " + addr + " up");
        }
        byte[] buf = new byte[1024];
        for (;;) {
            if (!need2run) {
                return true;
            }
            if (conn.isClosed() != 0) {
                break;
            }
            int siz = conn.ready2rx();
            if (siz < 1) {
                siz = 1;
            }
            if (siz > buf.length) {
                siz = buf.length;
            }
            if (conn.blockingGet(buf, 0, siz) != siz) {
                break;
            }
            if (limit == 2) {
                continue;
            }
            lower.trgtData(buf, siz);
        }
        ready = false;
        if (logging) {
            logger.info("connection to " + addr + " down");
        }
        if (!clear) {
            return false;
        }
        lower.clearClients();
        return false;
    }

    public void stopWork() {
        need2run = false;
        if (conn == null) {
            return;
        }
        conn.setClose();
    }

}
