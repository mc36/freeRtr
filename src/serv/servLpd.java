package serv;

import java.util.List;
import pipe.pipeDiscard;
import pipe.pipeLine;
import pipe.pipeShell;
import pipe.pipeSide;
import prt.prtGenConn;
import prt.prtServS;
import tab.tabGen;
import user.userFilter;
import user.userFlash;
import user.userHelping;
import util.bits;
import util.cmds;
import util.debugger;
import util.logger;
import util.uniResLoc;

/**
 * line printer daemon protocol (rfc1179) server
 *
 * @author matecsaba
 */
public class servLpd extends servGeneric implements prtServS {

    /**
     * port number
     */
    public static final int port = 515;

    /**
     * spool path
     */
    public String spoolPath = "/";

    /**
     * print script
     */
    public String printScript = null;

    /**
     * keep after printing
     */
    public boolean keepFiles = false;

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "server lpd .*! port " + port,
        "server lpd .*! protocol " + proto2string(protoAllStrm),
        "server lpd .*! no script",
        "server lpd .*! no keep"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public String srvName() {
        return "lpd";
    }

    public int srvPort() {
        return port;
    }

    public int srvProto() {
        return protoAllStrm;
    }

    public boolean srvInit() {
        return genStrmStart(this, new pipeLine(32768, false), 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

    public void srvShRun(String beg, List<String> lst) {
        lst.add(beg + "path " + spoolPath);
        cmds.cfgLine(lst, printScript == null, beg, "script", printScript);
        cmds.cfgLine(lst, !keepFiles, beg, "keep", "");
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("path")) {
            spoolPath = "/" + uniResLoc.normalizePath(cmd.word() + "/");
            return false;
        }
        if (s.equals("script")) {
            printScript = "/" + uniResLoc.normalizePath(cmd.word());
            return false;
        }
        if (s.equals("keep")) {
            keepFiles = true;
            return false;
        }
        if (!s.equals("no")) {
            return true;
        }
        s = cmd.word();
        if (s.equals("path")) {
            spoolPath = "/";
            return false;
        }
        if (s.equals("script")) {
            printScript = null;
            return false;
        }
        if (s.equals("keep")) {
            keepFiles = false;
            return false;
        }
        return true;
    }

    public void srvHelp(userHelping l) {
        l.add("1 2  path                         set spool folder");
        l.add("2 .    <path>                     name of spool folder");
        l.add("1 2  script                       set printer script");
        l.add("2 .    <path>                     name of printer script");
        l.add("1 .  keep                         keep print files");
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.timeout = 120000;
        pipe.lineRx = pipeSide.modTyp.modeLF;
        pipe.lineTx = pipeSide.modTyp.modeLF;
        new servLpdDoer(this, pipe);
        return false;
    }

}

class servLpdDoer implements Runnable {

    private servLpd lower;

    private pipeSide pipe;

    private int mode;

    private String filBeg;

    private int ctrlNum;

    private int dataNum;

    public servLpdDoer(servLpd parent, pipeSide conn) {
        lower = parent;
        pipe = conn;
        new Thread(this).start();
    }

    public void run() {
        try {
            mode = 1000;
            filBeg = lower.spoolPath + bits.getTime() + "-";
            ctrlNum = 0;
            dataNum = 0;
            for (;;) {
                if (doCmd()) {
                    break;
                }
            }
            doPrnt();
            doDel();
        } catch (Exception e) {
            logger.traceback(e);
        }
        pipe.setClose();
    }

    private boolean sendByte(int i) {
        if (debugger.servLpdTraf) {
            logger.debug("tx mode=" + mode + " res=" + i);
        }
        byte[] buf = new byte[1];
        buf[0] = (byte) i;
        return pipe.morePut(buf, 0, buf.length) != buf.length;
    }

    private boolean doCmd() {
        byte[] buf = new byte[1];
        if (pipe.moreGet(buf, 0, buf.length) != buf.length) {
            return true;
        }
        int i = buf[0];
        if (i == 0) {
            return false;
        }
        String s = pipe.lineGet(1);
        if (s == null) {
            return true;
        }
        if (debugger.servLpdTraf) {
            logger.debug("rx mode=" + mode + " cmd=" + i + " line=" + s);
        }
        cmds cmd = new cmds("", s);
        switch (mode + i) {
            case 1001: // print documents
            case 1003: // send short queue state
            case 1004: // send long queue state
            case 1005: // remove document
                return true;
            case 1002: // receive job
                mode = 2000;
                sendByte(0);
                return false;
            case 2001: // abort job
                mode = 3000;
                sendByte(0);
                return false;
            case 2002: // control file
                sendByte(0);
                i = bits.str2num(cmd.word());
                buf = new byte[i];
                if (pipe.moreGet(buf, 0, i) != i) {
                    return true;
                }
                sendByte(0);
                bits.byteSave(true, buf, filBeg + ctrlNum + ".txt");
                ctrlNum++;
                return false;
            case 2003: // data file
                sendByte(0);
                i = bits.str2num(cmd.word());
                buf = new byte[i];
                if (pipe.moreGet(buf, 0, i) != i) {
                    return true;
                }
                sendByte(0);
                bits.byteSave(true, buf, filBeg + dataNum + ".ps");
                dataNum++;
                return false;
            default:
                return true;
        }
    }

    private void doDel() {
        if (lower.keepFiles) {
            return;
        }
        for (int i = 0; i < dataNum; i++) {
            userFlash.delete(filBeg + i + ".ps");
        }
        for (int i = 0; i < ctrlNum; i++) {
            userFlash.delete(filBeg + i + ".txt");
        }
        if (debugger.servLpdTraf) {
            logger.debug("deleted files");
        }
    }

    private void doPrnt() {
        for (int i = 0; i < dataNum; i++) {
            doPrnt(filBeg + i + ".ps");
        }
    }

    protected void doPrnt(String fn) {
        if (lower.printScript == null) {
            return;
        }
        if (debugger.servLpdTraf) {
            logger.debug("printing " + fn + ".");
        }
        pipeLine pl = new pipeLine(32768, false);
        pipeSide p1 = pl.getSide();
        pipeSide p2 = pl.getSide();
        pipeDiscard.discard(p1);
        pipeShell sh = pipeShell.exec(p2, lower.printScript + " " + fn, null, true, true);
        sh.waitFor();
    }

}
