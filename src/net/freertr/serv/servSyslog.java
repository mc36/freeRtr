package net.freertr.serv;

import java.util.List;
import net.freertr.pack.packHolder;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServS;
import net.freertr.tab.tabGen;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.logFil;
import net.freertr.util.logger;

/**
 * syslog protocol (rfc3164) server
 *
 * @author matecsaba
 */
public class servSyslog extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servSyslog() {
    }

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
    protected logFil log2file;

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        "server syslog .*! port " + port,
        "server syslog .*! protocol " + proto2string(protoAllDgrm),
        "server syslog .*! no local",
        "server syslog .*! no file",
        "server syslog .*! no rotate"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    /**
     * facilities
     */
    public final static String[] facilities = {
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

    public void srvShRun(String beg, List<String> lst, int filter) {
        cmds.cfgLine(lst, !log2local, beg, "local", "");
        cmds.cfgLine(lst, log2file == null, beg, "file", "" + log2file);
        if (log2file != null) {
            String a = log2file.rotate1();
            cmds.cfgLine(lst, a == null, beg, "rotate", a);
        }
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("file")) {
            try {
                log2file.close();
            } catch (Exception e) {
            }
            log2file = new logFil(cmd.word());
            log2file.open(false);
            return false;
        }
        if (s.equals("rotate")) {
            if (log2file == null) {
                return false;
            }
            int siz = bits.str2num(cmd.word());
            s = cmd.word();
            int tim = bits.str2num(cmd.word());
            log2file.rotate(s, siz, tim, 0);
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
            try {
                log2file.close();
            } catch (Exception e) {
            }
            log2file = null;
            return false;
        }
        if (s.equals("rotate")) {
            if (log2file == null) {
                return false;
            }
            log2file.rotate(null, 0, 0, 0);
            return false;
        }
        if (s.equals("local")) {
            log2local = false;
            return false;
        }
        return true;
    }

    public void srvHelp(userHelping l) {
        l.add(null, "1 2  file                         set log file");
        l.add(null, "2 .    <file>                     log file");
        l.add(null, "1 2  rotate                       log file rotation");
        l.add(null, "2 3    <num>                      maximum file size");
        l.add(null, "3 4,.    <str>                    name of second file");
        l.add(null, "4 .        <num>                  ms between backup");
        l.add(null, "1 .  local                        set local logging");
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.setTime(10000);
        pipe.lineRx = pipeSide.modTyp.modeCRLF;
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        new servSyslogDoer(this, pipe, id);
        return false;
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
                String msg = pck.getAsciiZ(0, i, -1).replaceAll("\r", " ").replaceAll("\n", " ");
                if (lower.log2local) {
                    logger.info("syslog " + sck.peerAddr + ": " + msg);
                }
                if (lower.log2file != null) {
                    lower.log2file.add(logger.getTimestamp() + " " + sck.peerAddr + " " + msg);
                }
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        sck.setClosing();
        pipe.setClose();
    }

}
