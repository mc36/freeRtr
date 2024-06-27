package org.freertr.serv;

import java.util.List;
import org.freertr.pack.packHolder;
import org.freertr.pack.packNetflow;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServS;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabSession;
import org.freertr.tab.tabSessionEntry;
import org.freertr.user.userFilter;
import org.freertr.user.userHelping;
import org.freertr.util.cmds;
import org.freertr.util.logger;

/**
 * netflow (rfc3954) server
 *
 * @author matecsaba
 */
public class servNetflow extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servNetflow() {
    }

    /**
     * sessions
     */
    public tabSession connects = new tabSession(true, 60000);

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        "server netflow .*!" + cmds.tabulator + "port " + packNetflow.port,
        "server netflow .*!" + cmds.tabulator + "protocol " + proto2string(protoAllDgrm),
        "server netflow .*!" + cmds.tabulator + cmds.negated + cmds.tabulator + "timeout",
        "server netflow .*!" + cmds.tabulator + cmds.negated + cmds.tabulator + "sessions",
        "server netflow .*!" + cmds.tabulator + cmds.negated + cmds.tabulator + "rate",
        "server netflow .*!" + cmds.tabulator + cmds.negated + cmds.tabulator + "mac",
        "server netflow .*!" + cmds.tabulator + cmds.negated + cmds.tabulator + "before",
        "server netflow .*!" + cmds.tabulator + cmds.negated + cmds.tabulator + "after",
        "server netflow .*!" + cmds.tabulator + cmds.negated + cmds.tabulator + "dropped",
        "server netflow .*!" + cmds.tabulator + cmds.negated + cmds.tabulator + "allow-routing",
        "server netflow .*!" + cmds.tabulator + cmds.negated + cmds.tabulator + "allow-sending",
        "server netflow .*!" + cmds.tabulator + cmds.negated + cmds.tabulator + "allow-linklocal",
        "server netflow .*!" + cmds.tabulator + cmds.negated + cmds.tabulator + "allow-multicast",
        "server netflow .*!" + cmds.tabulator + cmds.negated + cmds.tabulator + "allow-broadcast",
        "server netflow .*!" + cmds.tabulator + cmds.negated + cmds.tabulator + "allow-list",
        "server netflow .*!" + cmds.tabulator + cmds.negated + cmds.tabulator + "allow-url",
        "server netflow .*!" + cmds.tabulator + cmds.negated + cmds.tabulator + "drop-rx",
        "server netflow .*!" + cmds.tabulator + cmds.negated + cmds.tabulator + "drop-tx",
        "server netflow .*!" + cmds.tabulator + cmds.negated + cmds.tabulator + "drop-frg",
        "server netflow .*!" + cmds.tabulator + cmds.negated + cmds.tabulator + "member"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    /**
     * get defaults filter
     *
     * @return filter
     */
    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    /**
     * get name
     *
     * @return name
     */
    public String srvName() {
        return "netflow";
    }

    /**
     * get port
     *
     * @return port
     */
    public int srvPort() {
        return packNetflow.port;
    }

    /**
     * get protocol
     *
     * @return protocol
     */
    public int srvProto() {
        return protoAllDgrm;
    }

    /**
     * initialize
     *
     * @return false on success, true on error
     */
    public boolean srvInit() {
        connects.startTimer();
        return genStrmStart(this, new pipeLine(32768, true), 0);
    }

    /**
     * deinitialize
     *
     * @return false on success, true on error
     */
    public boolean srvDeinit() {
        connects.stopTimer();
        return genericStop(0);
    }

    /**
     * get configuration
     *
     * @param beg beginning
     * @param lst list
     */
    public void srvShRun(String beg, List<String> lst, int filter) {
        connects.getConfig(lst, beg);
    }

    /**
     * configure
     *
     * @param cmd command
     * @return false on success, true on error
     */
    public boolean srvCfgStr(cmds cmd) {
        connects.fromString(cmd);
        return false;
    }

    /**
     * get help
     *
     * @param l help
     */
    public void srvHelp(userHelping l) {
        l.add(null, "1 2  timeout                      set timeout");
        l.add(null, "2 .    <num>                      timeout in ms");
        l.add(null, "1 2  sessions                     set session limit");
        l.add(null, "2 .    <num>                      number of sessions");
        l.add(null, "1 2  rate                         specify translation rate");
        l.add(null, "2 .    <nam:pm>                   name of policy map");
        l.add(null, "1 .  before                       log on session start");
        l.add(null, "1 .  after                        log on session stop");
    }

    /**
     * start connection
     *
     * @param pipe pipeline
     * @param id connection
     * @return false on success, true on error
     */
    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.setTime(120000);
        pipe.lineRx = pipeSide.modTyp.modeCRLF;
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        new servNetflowConn(this, pipe);
        return false;
    }

    /**
     * got message
     *
     * @param pckB packet to read
     * @param pckF netflow parser
     */
    protected void gotPack(packHolder pckB, packNetflow pckF) {
        List<tabSessionEntry> lst = pckF.parsePacket(pckB);
        for (int i = 0; i < lst.size(); i++) {
            tabSessionEntry ntry = lst.get(i);
            tabSessionEntry old = connects.doSess(ntry, null, true);
            if (old == null) {
                continue;
            }
            if (ntry.dir != old.dir) {
                ntry = ntry.reverseCounts();
            }
            old.addCounts(ntry);
        }
    }

}

class servNetflowConn implements Runnable {

    private servNetflow parent;

    private pipeSide pipe;

    public servNetflowConn(servNetflow prnt, pipeSide pip) {
        parent = prnt;
        pipe = pip;
        new Thread(this).start();
    }

    public void run() {
        packHolder pckB = new packHolder(true, true);
        packNetflow pckF = new packNetflow();
        try {
            pipe.wait4ready(120000);
            for (;;) {
                pckB.clear();
                pckB = pipe.readPacket(pckB, 0, true);
                if (pckB == null) {
                    break;
                }
                parent.gotPack(pckB, pckF);
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        pipe.setClose();
    }

}
