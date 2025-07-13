package org.freertr.serv;

import java.util.List;
import org.freertr.pack.packHolder;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServS;
import org.freertr.tab.tabGen;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.logFil;
import org.freertr.util.logger;

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
    public final static int port = 514;

    /**
     * log to local logging
     */
    protected boolean log2local = false;

    /**
     * log to local file
     */
    protected logFil log2file;

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
    public final static String num2facility(int i) {
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
    public final static int facility2num(String s) {
        for (int i = 0; i < facilities.length; i++) {
            if (s.equals(facilities[i])) {
                return i;
            }
        }
        return -1;
    }

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("server syslog .*", cmds.tabulator + "port " + port, null),
        new userFilter("server syslog .*", cmds.tabulator + "protocol " + proto2string(protoAllDgrm), null),
        new userFilter("server syslog .*", cmds.tabulator + cmds.negated + cmds.tabulator + "local", null),
        new userFilter("server syslog .*", cmds.tabulator + cmds.negated + cmds.tabulator + "file", null),
        new userFilter("server syslog .*", cmds.tabulator + cmds.negated + cmds.tabulator + "rotate", null)
    };

    public userFilter[] srvDefFlt() {
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
        if (!s.equals(cmds.negated)) {
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

    public void srvHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "file", "set log file");
        l.add(null, false, 2, new int[]{-1}, "<file>", "log file");
        l.add(null, false, 1, new int[]{2}, "rotate", "log file rotation");
        l.add(null, false, 2, new int[]{3}, "<num>", "maximum file size");
        l.add(null, false, 3, new int[]{4, -1}, "<str>", "name of second file");
        l.add(null, false, 4, new int[]{-1}, "<num>", "ms between backup");
        l.add(null, false, 1, new int[]{-1}, "local", "set local logging");
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
