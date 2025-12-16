package org.freertr.serv;

import java.util.List;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgInit;
import org.freertr.cfg.cfgProxy;
import org.freertr.clnt.clntProxy;
import org.freertr.clnt.clntWhois;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServS;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabIntMatcher;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.logger;

/**
 * whois server
 *
 * @author matecsaba
 */
public class servWhois extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servWhois() {
    }

    /**
     * port number
     */
    public final static int port = clntWhois.port;

    /**
     * remote servers
     */
    public tabGen<servWhoisRem> remotes = new tabGen<servWhoisRem>();

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("server whois .*", cmds.tabulator + "port " + port, null),
        new userFilter("server whois .*", cmds.tabulator + "protocol " + proto2string(protoAllStrm), null)
    };

    public userFilter[] srvDefFlt() {
        return defaultF;
    }

    public String srvName() {
        return "whois";
    }

    public int srvPort() {
        return port;
    }

    public int srvProto() {
        return protoAllStrm;
    }

    public boolean srvInit() {
        return genStrmStart(this, new pipeLine(65536, false), 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

    public void srvShRun(String beg, List<String> lst, int filter) {
        for (int i = 0; i < remotes.size(); i++) {
            servWhoisRem ntry = remotes.get(i);
            lst.add(beg + ntry);
        }
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("local")) {
            servWhoisRem ntry = new servWhoisRem();
            if (ntry.rng.fromString(cmd.word())) {
                cmd.error("bad range");
                return false;
            }
            ntry.opt = cmd.getRemaining();
            remotes.put(ntry);
            return false;
        }
        if (s.equals("remote")) {
            servWhoisRem ntry = new servWhoisRem();
            if (ntry.rng.fromString(cmd.word())) {
                cmd.error("bad range");
                return false;
            }
            cfgProxy prx = cfgAll.proxyFind(cmd.word(), false);
            if (prx == null) {
                cmd.error("no such proxy profile");
                return false;
            }
            ntry.prx = prx.proxy;
            ntry.srv = cmd.word();
            ntry.opt = cmd.getRemaining();
            if (ntry.opt.length() < 1) {
                ntry.opt = null;
            }
            remotes.put(ntry);
            return false;
        }
        if (!s.equals(cmds.negated)) {
            return true;
        }
        s = cmd.word();
        if (s.equals("local")) {
            servWhoisRem ntry = new servWhoisRem();
            if (ntry.rng.fromString(cmd.word())) {
                cmd.error("bad range");
                return false;
            }
            remotes.del(ntry);
            return false;
        }
        if (s.equals("remote")) {
            servWhoisRem ntry = new servWhoisRem();
            if (ntry.rng.fromString(cmd.word())) {
                cmd.error("bad range");
                return false;
            }
            remotes.del(ntry);
            return false;
        }
        return true;
    }

    public void srvHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "local", "select local definition");
        l.add(null, false, 2, new int[]{3}, "<num>", "as number range");
        l.add(null, false, 3, new int[]{-1}, "<str>", "name of asn");
        l.add(null, false, 1, new int[]{2}, "remote", "select remote resolver");
        l.add(null, false, 2, new int[]{3}, "<num>", "as number range");
        l.add(null, false, 3, new int[]{4}, "<name:prx>", "proxy profile to use");
        l.add(null, false, 4, new int[]{5, -1}, "<str>", "remote server name");
        l.add(null, false, 5, new int[]{-1}, "[str]", "server option");
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.setTime(60000);
        pipe.setReady();
        servWhoisConn ntry = new servWhoisConn(this, pipe);
        ntry.doStart();
        return false;
    }

}

class servWhoisRem implements Comparable<servWhoisRem> {

    public final tabIntMatcher rng = new tabIntMatcher();

    public clntProxy prx;

    public String srv;

    public String opt;

    public String toString() {
        if (srv == null) {
            return "local " + rng + " " + opt;
        }
        String a = "remote " + rng + " " + prx.name + " " + srv;
        if (opt == null) {
            return a;
        }
        return a + " " + opt;
    }

    public int compareTo(servWhoisRem o) {
        if (rng.rangeMin < o.rng.rangeMin) {
            return -1;
        }
        if (rng.rangeMin > o.rng.rangeMin) {
            return +1;
        }
        if (rng.rangeMax < o.rng.rangeMax) {
            return -1;
        }
        if (rng.rangeMax > o.rng.rangeMax) {
            return +1;
        }
        return 0;
    }

}

class servWhoisConn implements Runnable {

    public final servWhois prnt;

    public final pipeSide pipe;

    /**
     * create one connection
     *
     * @param parent lower
     * @param conn pipe
     */
    public servWhoisConn(servWhois parent, pipeSide conn) {
        prnt = parent;
        pipe = conn;
    }

    /**
     * do startup
     */
    public void doStart() {
        new Thread(this).start();
    }

    public void run() {
        try {
            pipe.lineTx = pipeSide.modTyp.modeCRLF;
            pipe.lineRx = pipeSide.modTyp.modeCRorLF;
            pipe.linePut("% caching whois ready");
            pipe.linePut("% " + cfgInit.versionFull);
            pipe.linePut("");
            String a = pipe.lineGet(1);
            a = a.trim().toLowerCase();
            if (!a.startsWith("as")) {
                pipe.linePut("% bad request");
                pipe.setClose();
                return;
            }
            a = a.substring(2, a.length());
            int n = bits.str2num(a);
            String m = null;
            for (int i = 0; i < prnt.remotes.size(); i++) {
                servWhoisRem ntry = prnt.remotes.get(i);
                if (!ntry.rng.matches(n)) {
                    continue;
                }
                if (ntry.srv == null) {
                    m = "" + ntry.opt;
                    break;
                }
                clntWhois w = new clntWhois(pipe, ntry.prx, ntry.srv, ntry.opt);
                m = w.doQuery(n);
                break;
            }
            if (m == null) {
                m = clntWhois.asn2name(n, true);
            }
            a = bits.num2str(n);
            pipe.linePut("% information for asn " + a + " is " + m);
            pipe.linePut("as-block:       AS" + a + " - AS" + a);
            pipe.linePut("aut-num:        AS" + a);
            pipe.linePut("descr:          cached at " + cfgAll.hostName);
            pipe.linePut("as-name:        " + m);
            pipe.linePut("% list of other information");
            List<String> lst = clntWhois.asn2infos(n);
            for (int i = 0; i < lst.size(); i++) {
                pipe.linePut("remark:         info " + lst.get(i));
            }
            pipe.linePut("% end of listing");
            pipe.setClose();
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}
