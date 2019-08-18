package serv;

import cfg.cfgAll;
import java.util.ArrayList;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
import pack.packHolder;
import pack.packNetflow;
import pipe.pipeLine;
import pipe.pipeSide;
import prt.prtGenConn;
import prt.prtServS;
import tab.tabGen;
import tab.tabSessionEntry;
import user.userFilter;
import user.userHelping;
import util.bits;
import util.cmds;
import util.logger;

/**
 * netflow (rfc3954) server
 *
 * @author matecsaba
 */
public class servNetflow extends servGeneric implements prtServS {

    /**
     * log to local logging
     */
    public boolean log2local = false;

    /**
     * log to local file
     */
    public String log2file;

    /**
     * timeout value
     */
    public int timeout = 60000;

    /**
     * sessions
     */
    public tabGen<tabSessionEntry> connects = new tabGen<tabSessionEntry>();

    private Timer purgeTimer;

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "server netflow .*! port " + packNetflow.port,
        "server netflow .*! protocol " + proto2string(protoAllDgrm),
        "server netflow .*! timeout 60000",
        "server netflow .*! no local",
        "server netflow .*! no file"
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
        restartTimer(false);
        return genStrmStart(this, new pipeLine(32768, true), 0);
    }

    /**
     * deinitialize
     *
     * @return false on success, true on error
     */
    public boolean srvDeinit() {
        restartTimer(true);
        return genericStop(0);
    }

    private void restartTimer(boolean shutdown) {
        try {
            purgeTimer.cancel();
        } catch (Exception e) {
        }
        purgeTimer = null;
        if (shutdown) {
            return;
        }
        purgeTimer = new Timer();
        servNetflowKeep task = new servNetflowKeep(this);
        purgeTimer.schedule(task, 1000, timeout / 4);
    }

    /**
     * get configuration
     *
     * @param beg beginning
     * @param lst list
     */
    public void srvShRun(String beg, List<String> lst) {
        lst.add(beg + "timeout " + timeout);
        cmds.cfgLine(lst, !log2local, beg, "local", "");
        cmds.cfgLine(lst, log2file == null, beg, "file", log2file);
    }

    /**
     * configure
     *
     * @param cmd command
     * @return false on success, true on error
     */
    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("file")) {
            log2file = cmd.word();
            return false;
        }
        if (s.equals("local")) {
            log2local = true;
            return false;
        }
        if (s.equals("timeout")) {
            timeout = bits.str2num(cmd.word());
            return false;
        }
        if (!s.equals("no")) {
            return true;
        }
        s = cmd.word();
        if (s.equals("file")) {
            log2file = null;
            return false;
        }
        if (s.equals("local")) {
            log2local = false;
            return false;
        }
        return true;
    }

    /**
     * get help
     *
     * @param l help
     */
    public void srvHelp(userHelping l) {
        l.add("1 2  file                         set log file");
        l.add("2 .    <file>                     log file");
        l.add("1 2  timeout                      set timeout");
        l.add("2 .    <num>                      timeout in ms");
        l.add("1 .  local                        set local logging");
    }

    /**
     * start connection
     *
     * @param pipe pipeline
     * @param id connection
     * @return false on success, true on error
     */
    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.timeout = 120000;
        pipe.lineRx = pipeSide.modTyp.modeCRLF;
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        new servNetflowConn(this, pipe);
        return false;
    }

    private void logMsg(String msg) {
        if (log2local) {
            logger.info(msg);
        }
        if (log2file != null) {
            List<String> l = new ArrayList<String>();
            l.add(bits.time2str(cfgAll.timeZoneName, bits.getTime() + cfgAll.timeServerOffset, 3) + " " + msg);
            bits.buf2txt(false, l, log2file);
        }
    }

    /**
     * got message
     *
     * @param pckB packet to read
     * @param pckF netflow parser
     */
    protected void gotPack(packHolder pckB, packNetflow pckF) {
        List<tabSessionEntry> lst = pckF.parsePacket(pckB);
        long tim = bits.getTime();
        for (int i = 0; i < lst.size(); i++) {
            tabSessionEntry ntry = lst.get(i);
            tabSessionEntry old = connects.find(ntry);
            if (old != null) {
                old.lastTime = tim;
                old.addCounts(ntry);
                continue;
            }
            ntry.lastTime = tim;
            ntry.startTime = tim;
            connects.put(ntry);
            logMsg("started " + ntry);
        }
    }

    /**
     * purge table
     */
    protected void doKeep() {
        long tim = bits.getTime();
        for (int i = connects.size() - 1; i >= 0; i--) {
            tabSessionEntry cur = connects.get(i);
            if (cur == null) {
                continue;
            }
            if ((tim - cur.lastTime) < timeout) {
                continue;
            }
            connects.del(cur);
            logMsg("finished " + cur);
        }
    }

}

class servNetflowKeep extends TimerTask {

    private servNetflow parent;

    public servNetflowKeep(servNetflow prnt) {
        parent = prnt;
    }

    public void run() {
        try {
            parent.doKeep();
        } catch (Exception e) {
            logger.traceback(e);
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
            pipe.wait4ready(0);
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
