package net.freertr.serv;

import java.util.List;
import net.freertr.cfg.cfgAll;
import net.freertr.clnt.clntWhois;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServS;
import net.freertr.tab.tabGen;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.logger;
import net.freertr.util.version;

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
        "server whois .*! port " + port,
        "server whois .*! protocol " + proto2string(protoAllStrm)
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

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
    }

    public boolean srvCfgStr(cmds cmd) {
        return false;
    }

    public void srvHelp(userHelping l) {
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.setTime(60000);
        pipe.setReady();
        servWhoisConn ntry = new servWhoisConn(this, pipe);
        ntry.doStart();
        return false;
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
            pipe.linePut("as-block:       AS" + n + " - AS" + n);
            pipe.linePut("descr:          cached at " + cfgAll.hostName);
            pipe.linePut("as-name:        " + clntWhois.asn2name(n, true));
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
