package org.freertr.serv;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgInit;
import org.freertr.cfg.cfgVrf;
import org.freertr.clnt.clntDns;
import org.freertr.enc.encUrl;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtRedun;
import org.freertr.prt.prtRedunClnt;
import org.freertr.prt.prtServS;
import org.freertr.tab.tabGen;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.logger;

/**
 * bgp peer proxy
 *
 * @author matecsaba
 */
public class servBgproxy extends servGeneric implements prtServS, prtRedunClnt {

    /**
     * create instance
     */
    public servBgproxy() {
    }

    /**
     * port number
     */
    public final static int port = 17980;

    /**
     * group number
     */
    public cfgVrf trgVrf;

    /**
     * ha mode
     */
    public boolean haMode;

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
     * rewrite nexthop
     */
    public boolean nexthopIn = true;

    /**
     * rewrite nexthop
     */
    public boolean nexthopOut = true;

    /**
     * neighbors
     */
    protected tabGen<servBgproxyNei> neighs = new tabGen<servBgproxyNei>();

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("server bgproxy .*", cmds.tabulator + "port " + port, null),
        new userFilter("server bgproxy .*", cmds.tabulator + "protocol " + proto2string(protoAllStrm), null),
        new userFilter("server bgproxy .*", cmds.tabulator + cmds.negated + cmds.tabulator + "ha-mode", null),
        new userFilter("server bgproxy .*", cmds.tabulator + cmds.negated + cmds.tabulator + "target", null),
        new userFilter("server bgproxy .*", cmds.tabulator + "nexthop-in", null),
        new userFilter("server bgproxy .*", cmds.tabulator + "nexthop-out", null),
        new userFilter("server bgproxy .*", cmds.tabulator + "timeout 60000", null),
        new userFilter("server bgproxy .*", cmds.tabulator + "buffer 65536", null),
        new userFilter("server bgproxy .*", cmds.tabulator + cmds.negated + cmds.tabulator + "logging", null)
    };

    public userFilter[] srvDefFlt() {
        return defaultF;
    }

    public String srvName() {
        return "bgproxy";
    }

    public int srvPort() {
        return port;
    }

    public int srvProto() {
        return protoAllStrm;
    }

    public boolean srvInit() {
        return genStrmStart(this, new pipeLine(bufSiz, false), 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

    public void srvShRun(String beg, List<String> lst, int filter) {
        cmds.cfgLine(lst, !logging, beg, "logging", "");
        cmds.cfgLine(lst, !haMode, beg, "ha-mode", "");
        cmds.cfgLine(lst, !nexthopIn, beg, "nexthop-in", "");
        cmds.cfgLine(lst, !nexthopOut, beg, "nexthop-out", "");
        if (trgVrf == null) {
            lst.add(beg + "no target");
        } else {
            lst.add(beg + "target " + trgVrf.name);
        }
        lst.add(beg + "timeout " + timeOut);
        lst.add(beg + "buffer " + bufSiz);
    }

    public boolean srvCfgStr(cmds cmd) {
        String a = cmd.word();
        boolean neg = a.equals(cmds.negated);
        if (neg) {
            a = cmd.word();
        }
        if (a.equals("logging")) {
            logging = !neg;
            return false;
        }
        if (a.equals("ha-mode")) {
            haMode = !neg;
            if (haMode) {
                prtRedun.clientAdd(this, srvName() + " " + srvName);
            } else {
                prtRedun.clientDel(this);
            }
            return false;
        }
        if (a.equals("nexthop-in")) {
            nexthopIn = !neg;
            return false;
        }
        if (a.equals("nexthop-out")) {
            nexthopOut = !neg;
            return false;
        }
        if (a.equals("target")) {
            if (neg) {
                trgVrf = null;
                return false;
            }
            trgVrf = cfgAll.vrfFind(cmd.word(), false);
            if (trgVrf == null) {
                cmd.error("no such vrf");
                return false;
            }
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
        return true;
    }

    public void srvHelp(userHelp l) {
        l.add(null, false, 1, new int[]{-1}, "logging", "log the hits");
        l.add(null, false, 1, new int[]{-1}, "ha-mode", "save state");
        l.add(null, false, 1, new int[]{2}, "timeout", "set timeout on connection");
        l.add(null, false, 2, new int[]{-1}, "<num>", "timeout in ms");
        l.add(null, false, 1, new int[]{2}, "buffer", "set buffer size on connection");
        l.add(null, false, 2, new int[]{-1}, "<num>", "buffer in bytes");
        l.add(null, false, 1, new int[]{2}, "target", "set vrf to use");
        l.add(null, false, 2, new int[]{-1}, "<name:vrf>", "name of vrf");
        l.add(null, false, 1, new int[]{2}, "nexthop-in", "rewrite nexthop toward inside");
        l.add(null, false, 1, new int[]{2}, "nexthop-out", "rewrite nexthop toward outside");
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        if (logging) {
            logger.info("connection from " + id.peerAddr);
        }
        pipe.setTime(timeOut);
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        pipe.lineRx = pipeSide.modTyp.modeCRtryLF;
        pipe.setReady();
        new servBgproxyClnt(this, pipe);
        return false;
    }

    /**
     * get state information
     *
     * @param lst list to append
     */
    public void redunStateGet(List<String> lst) {
        if (!haMode) {
            return;
        }
    }

    /**
     * set state information
     *
     * @param cmd string to append
     * @return true on error, false on success
     */
    public boolean redunStateSet(cmds cmd) {
        return false;
    }

}

class servBgproxyClnt implements Runnable {

    private final servBgproxy parent;

    private final pipeSide pipe;

    public servBgproxyClnt(servBgproxy lower, pipeSide conn) {
        parent = lower;
        pipe = conn;
        logger.startThread(this);
    }

    public void run() {
        try {
            if (doWork()) {
                return;
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        pipe.setClose();
    }

    public boolean doWork() {
        String a = pipe.lineGet(1);
        cmds cmd = new cmds("api", a);
        for (;;) {
            a = pipe.lineGet(1);
            if (a.length() < 1) {
                break;
            }
        }
        a = cmd.word().toLowerCase();
        if (!a.equals("connect")) {
            return false;
        }
        a = cmd.word();
        encUrl gotUrl = new encUrl();
        gotUrl.fromString("tcp://" + a);
        addrIP adr = clntDns.justResolv(gotUrl.server, 0);
        if (adr == null) {
            return false;
        }
        if (parent.logging) {
            logger.info("connection for " + adr);
        }
        servBgproxyNei nei = new servBgproxyNei(parent, adr);
        servBgproxyNei old = parent.neighs.add(nei);
        if (old != null) {
            old.client.setClose();
            nei = old;
        } else {
            nei.startWork();
        }
        nei.client = pipe;
        nei.newly = true;
        pipe.linePut("HTTP/1.1 200 connected");
        pipe.linePut("Server: " + cfgInit.versionAgent);
        pipe.linePut("");
        return true;
    }

}

class servBgproxyNei implements Runnable, Comparable<servBgproxyNei> {

    private final servBgproxy parent;

    private final addrIP peer;

    protected pipeSide client;

    protected boolean newly;

    private pipeSide remote;

    private byte[] openCln;

    private byte[] openRem;

    public servBgproxyNei(servBgproxy lower, addrIP adr) {
        parent = lower;
        peer = adr.copyBytes();
    }

    public int compareTo(servBgproxyNei o) {
        return peer.compareTo(o.peer);
    }

    public void startWork() {
        if (parent.logging) {
            logger.info("starting for " + peer);
        }
        logger.startThread(this);
    }

    public void run() {
    }

}
