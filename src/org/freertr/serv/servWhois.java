package org.freertr.serv;

import java.util.List;
import org.freertr.cfg.cfgAll;
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
import org.freertr.user.userHelping;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.logger;
import org.freertr.util.version;

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
     * defaults text
     */
    public final static String[] defaultL = {
        "server whois .*!" + cmds.tabulator + "port " + port,
        "server whois .*!" + cmds.tabulator + "protocol " + proto2string(protoAllStrm)
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    /**
     * defaults filter
     */
    public tabGen<servWhoisRem> remotes = new tabGen<servWhoisRem>();

    public tabGen<userFilter> srvDefFlt() {
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
            lst.add(beg + "remote " + ntry);
        }
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
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
            ntry.srv = cmd.getRemaining();
            remotes.put(ntry);
            return false;
        }
        if (!s.equals(cmds.negated)) {
            return true;
        }
        s = cmd.word();
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

    public void srvHelp(userHelping l) {
        l.add(null, "1 2  remote                       select remote resolver");
        l.add(null, "2 3    <num>                      as number range");
        l.add(null, "3 4      <name:prx>               proxy profile to use");
        l.add(null, "4 .        <str>                  remote server name");
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

    public String toString() {
        return rng + " " + prx.name + " " + srv;
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
            pipe.linePut("% " + version.headLine);
            pipe.linePut("");
            String a = pipe.lineGet(1);
            a = a.trim();
            int i = a.length();
            if (i > 2) {
                a = a.substring(2, i);
            }
            int n = bits.str2num(a);
            String m = null;
            for (i = 0; i < prnt.remotes.size(); i++) {
                servWhoisRem ntry = prnt.remotes.get(i);
                if (!ntry.rng.matches(n)) {
                    continue;
                }
                clntWhois w = new clntWhois(pipe, ntry.prx, ntry.srv);
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
            for (i = 0; i < lst.size(); i++) {
                pipe.linePut("remark:         info " + lst.get(i));
            }
            pipe.linePut("% end of listing");
            pipe.setClose();
        } catch (Exception e) {
            logger.traceback(e);
        }
    }
}
