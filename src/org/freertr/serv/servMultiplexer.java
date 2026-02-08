package org.freertr.serv;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgProxy;
import org.freertr.cfg.cfgVrf;
import org.freertr.clnt.clntProxy;
import org.freertr.ip.ipFwdIface;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGen;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServS;
import org.freertr.tab.tabGen;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.logger;

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
    public final static int port = 1;

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
    public final static userFilter[] defaultF = {
        new userFilter("server multiplexer .*", cmds.tabulator + "port " + port, null),
        new userFilter("server multiplexer .*", cmds.tabulator + "protocol " + proto2string(protoAllStrm), null),
        new userFilter("server multiplexer .*", cmds.tabulator + "timeout 60000", null),
        new userFilter("server multiplexer .*", cmds.tabulator + "buffer 65536", null),
        new userFilter("server multiplexer .*", cmds.tabulator + cmds.negated + cmds.tabulator + "logging", null)
    };

    public userFilter[] srvDefFlt() {
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
            if (ntry.addr.fromString(cmd.word())) {
                return true;
            }
            ntry.port = bits.str2num(cmd.word());
            for (;;) {
                a = cmd.word();
                if (a.length() < 1) {
                    break;
                }
                if (a.equals("vrf")) {
                    ntry.vrf = cfgAll.vrfFind(cmd.word(), false);
                    continue;
                }
                if (a.equals("iface")) {
                    ntry.iface = cfgAll.ifcFind(cmd.word(), 0);
                    continue;
                }
                if (a.equals("proxy")) {
                    cfgProxy p = cfgAll.proxyFind(cmd.word(), false);
                    if (p == null) {
                        continue;
                    }
                    ntry.proxy = p.proxy;
                    continue;
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
            ntry.start();
            servMultiplexerTrgt old = targets.put(ntry);
            if (old == null) {
                return false;
            }
            old.stopWork();
            return false;
        }
        if (!a.equals(cmds.negated)) {
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

    public void srvHelp(userHelp l) {
        l.add(null, false, 1, new int[]{-1}, "logging", "set logging");
        l.add(null, false, 1, new int[]{2}, "timeout", "set timeout on connection");
        l.add(null, false, 2, new int[]{-1}, "<num>", "timeout in ms");
        l.add(null, false, 1, new int[]{2}, "buffer", "set buffer size on connection");
        l.add(null, false, 2, new int[]{-1}, "<num>", "buffer in bytes");
        l.add(null, false, 1, new int[]{2}, "target", "name of server");
        l.add(null, false, 2, new int[]{3}, "<num>", "number of target");
        l.add(null, false, 3, new int[]{4}, "<addr>", "address of target");
        l.add(null, false, 4, new int[]{5, -1}, "<port>", "port on target");
        l.add(null, false, 5, new int[]{5, -1}, "rx", "only rx");
        l.add(null, false, 5, new int[]{5, -1}, "tx", "only tx");
        l.add(null, false, 5, new int[]{5, -1}, "logging", "set logging");
        l.add(null, false, 5, new int[]{5, -1}, "clear", "clear clients on disconnect");
        l.add(null, false, 5, new int[]{5, -1}, "nowait", "use nonblocking send");
        l.add(null, false, 5, new int[]{6}, "vrf", "specify vrf to use");
        l.add(null, false, 6, new int[]{5, -1}, "<name:vrf>", "name of vrf");
        l.add(null, false, 5, new int[]{6}, "iface", "specify interface to use");
        l.add(null, false, 6, new int[]{5, -1}, "<name:ifc>", "name of interface");
        l.add(null, false, 5, new int[]{6}, "proxy", "specify proxy to use");
        l.add(null, false, 6, new int[]{5, -1}, "<name:prx>", "name of proxy");
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
        ntry.start();
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

    public void start() {
        new Thread(this).start();
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

class servMultiplexerTrgt implements Comparable<servMultiplexerTrgt>, Runnable {

    public servMultiplexer lower;

    public int num;

    public clntProxy proxy;

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
        if (vrf != null) {
            a += " vrf " + vrf.name;
        }
        if (iface != null) {
            a += " iface " + iface.name;
        }
        if (proxy != null) {
            a += " proxy " + proxy.name;
        }
        return num + " " + addr + " " + port + a;
    }

    public int compareTo(servMultiplexerTrgt o) {
        if (num < o.num) {
            return -1;
        }
        if (num > o.num) {
            return +1;
        }
        return 0;
    }

    public void start() {
        new Thread(this).start();
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
        conn = null;
        if (proxy != null) {
            conn = proxy.doConnect(lower.srvProto, addr, port, lower.srvName());
        } else {
            if (vrf == null) {
                return false;
            }
            prtGen prt = servGeneric.getProtocol(vrf, lower.srvProto, addr);
            if (prt == null) {
                return false;
            }
            ipFwdIface ifc = null;
            if (iface != null) {
                ifc = iface.getFwdIfc(addr);
            }
            conn = prt.streamConnect(new pipeLine(lower.bufSiz, false), ifc, 0, addr, port, lower.srvName(), -1, null, -1, -1);
        }
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
