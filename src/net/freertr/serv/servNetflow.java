package net.freertr.serv;

import java.util.List;
import net.freertr.pack.packHolder;
import net.freertr.pack.packNetflow;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServS;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabSession;
import net.freertr.tab.tabSessionEntry;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
import net.freertr.util.cmds;
import net.freertr.util.logger;

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
        "server netflow .*! port " + packNetflow.port,
        "server netflow .*! protocol " + proto2string(protoAllDgrm),
        "server netflow .*! no timeout",
        "server netflow .*! no mac",
        "server netflow .*! no before",
        "server netflow .*! no after",
        "server netflow .*! no dropped",
        "server netflow .*! no allow-routing",
        "server netflow .*! no allow-linklocal",
        "server netflow .*! no allow-multicast",
        "server netflow .*! no allow-broadcast",
        "server netflow .*! no allow-list",
        "server netflow .*! no drop-rx",
        "server netflow .*! no drop-tx",
        "server netflow .*! no drop-frg",
        "server netflow .*! no member"
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
