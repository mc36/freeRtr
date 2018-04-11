package serv;

import cfg.cfgAll;
import java.util.ArrayList;
import java.util.List;
import pack.packHolder;
import pipe.pipeLine;
import pipe.pipeSide;
import prt.prtGenConn;
import prt.prtServS;
import tab.tabGen;
import user.userFilter;
import user.userHelping;
import util.bits;
import util.cmds;
import util.logger;

/**
 * syslog protocol (rfc3164) server
 *
 * @author matecsaba
 */
public class servSyslog extends servGeneric implements prtServS {

    /**
     * port number
     */
    public static final int port = 514;

    /**
     * log to local logging
     */
    protected boolean log2local = false;

    /**
     * log to local file
     */
    protected String log2file;

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "server syslog .*! port " + port,
        "server syslog .*! protocol " + proto2string(protoAllDgrm),
        "server syslog .*! no local",
        "server syslog .*! no file"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    /**
     * facilities
     */
    public final static String facilities[] = {
        "kernel", "user", "mail", "system", "security1", "syslogd",
        "lpd", "news", "uucp", "clock1", "security2", "ftp", "ntp",
        "logaudit", "logalert", "clock2", "local0", "local1", "local2",
        "local3", "local4", "local5", "local6", "local7"};

    /**
     * facility number to string
     *
     * @param i number
     * @return string
     */
    public static final String num2facility(int i) {
        if (i < 0) {
            return null;
        }
        if (i >= facilities.length) {
            return null;
        }
        return facilities[i];
    }

    /**
     * facility string to number
     *
     * @param s string
     * @return number, -1 if not found
     */
    public static final int facility2num(String s) {
        for (int i = 0; i < facilities.length; i++) {
            if (s.equals(facilities[i])) {
                return i;
            }
        }
        return -1;
    }

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public String srvName() {
        return "syslog";
    }

    public int srvPort() {
        return port;
    }

    public int srvProto() {
        return protoAllDgrm;
    }

    public boolean srvInit() {
        return genStrmStart(this, new pipeLine(32768, false), 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

    public void srvShRun(String beg, List<String> lst) {
        cmds.cfgLine(lst, !log2local, beg, "local", "");
        cmds.cfgLine(lst, log2file == null, beg, "file", log2file);
    }

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

    public void srvHelp(userHelping l) {
        l.add("1 2  file                         set log file");
        l.add("2 .    <file>                     log file");
        l.add("1 .  local                        set local logging");
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.timeout = 10000;
        pipe.lineRx = pipeSide.modTyp.modeCRLF;
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        new servSyslogDoer(this, pipe, id);
        return false;
    }

    /**
     * got message
     *
     * @param peer source of message
     * @param msg message string
     */
    protected void gotMsg(String peer, String msg) {
        if (log2local) {
            logger.info("syslog " + peer + ": " + msg);
        }
        if (log2file != null) {
            List<String> l = new ArrayList<String>();
            l.add(bits.time2str(cfgAll.timeZoneName, bits.getTime() + cfgAll.timeServerOffset, 3) + " " + peer + " " + msg);
            bits.buf2txt(false, l, log2file);

        }
    }

}

class servSyslogDoer implements Runnable {

    private servSyslog lower;

    private pipeSide pipe;

    private prtGenConn sck;

    public servSyslogDoer(servSyslog parent, pipeSide conn, prtGenConn id) {
        lower = parent;
        pipe = conn;
        sck = id;
        new Thread(this).start();
    }

    public void run() {
        try {
            for (;;) {
                packHolder pck = pipe.readPacket(true);
                if (pck == null) {
                    break;
                }
                int i = pck.dataSize();
                if (i < 1) {
                    continue;
                }
                lower.gotMsg("" + sck.peerAddr, pck.getAsciiZ(0, i, -1).replaceAll("\r", " ").replaceAll("\n", " "));
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        sck.setClosing();
        pipe.setClose();
    }

}
