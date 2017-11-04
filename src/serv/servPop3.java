package serv;

import auth.authGeneric;
import auth.authResult;
import cfg.cfgAll;
import cfg.cfgAuther;
import cry.cryHashMd5;
import cry.cryUtils;
import java.io.File;
import java.util.Comparator;
import java.util.List;
import pack.packText;
import pipe.pipeLine;
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
 * post office protocol 3 (rfc1939) server
 *
 * @author matecsaba
 */
public class servPop3 extends servGeneric implements prtServS {

    /**
     * port number
     */
    public static final int port = 110;

    /**
     * mail folders
     */
    public String mailFolders = "/";

    /**
     * authentication list
     */
    public authGeneric authenticList;

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "server pop3 .*! port " + port,
        "server pop3 .*! protocol " + proto2string(protoAllStrm)
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    /**
     * calculate apop string
     *
     * @param cookie cookie
     * @param pass password
     * @return result
     */
    public static String calcApop(String cookie, String pass) {
        cryHashMd5 md5 = new cryHashMd5();
        md5.init();
        md5.update(cookie.getBytes());
        md5.update(pass.getBytes());
        return cryUtils.hash2hex(md5);
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.timeout = 120000;
        pipe.lineRx = pipeSide.modTyp.modeCRtryLF;
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        new servPop3doer(this, pipe, id);
        return false;
    }

    public void srvShRun(String beg, List<String> l) {
        if (authenticList == null) {
            l.add(beg + "no authentication");
        } else {
            l.add(beg + "authentication " + authenticList.autName);
        }
        l.add(beg + "path " + mailFolders);
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("authentication")) {
            cfgAuther lst = cfgAll.autherFind(cmd.word(), null);
            if (lst == null) {
                cmd.error("no such auth list");
                return false;
            }
            authenticList = lst.getAuther();
            return false;
        }
        if (s.equals("path")) {
            mailFolders = "/" + uniResLoc.normalizePath(cmd.word() + "/");
            return false;
        }
        if (!s.equals("no")) {
            return true;
        }
        s = cmd.word();
        if (s.equals("authentication")) {
            authenticList = null;
            return false;
        }
        if (s.equals("path")) {
            mailFolders = "/";
            return false;
        }
        return true;
    }

    public void srvHelp(userHelping l) {
        l.add("1 2  authentication               set authentication");
        l.add("2 .    <name>                     name of authentication list");
        l.add("1 2  path                         set root folder");
        l.add("2 .    <path>                     name of root folder");
    }

    public String srvName() {
        return "pop3";
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

}

class servPop3msg implements Comparator<servPop3msg> {

    public String name;

    public long size;

    public int compare(servPop3msg o1, servPop3msg o2) {
        return o1.name.compareTo(o2.name);
    }

}

class servPop3doer implements Runnable {

    private servPop3 lower;

    private pipeSide pipe;

    private prtGenConn conn;

    private String userN = "";

    private boolean authed = false;

    private tabGen<servPop3msg> deled = null;

    private int cookie;

    public servPop3doer(servPop3 parent, pipeSide stream, prtGenConn id) {
        lower = parent;
        pipe = stream;
        conn = id;
        cookie = bits.randomD();
        new Thread(this).start();
    }

    public String getCookie() {
        return "<" + cookie + "." + conn.peerAddr + "@" + conn.iface.addr + ">";
    }

    public tabGen<servPop3msg> getMsgList() {
        File f;
        File fl[] = null;
        try {
            f = new File(lower.mailFolders + userN);
            fl = f.listFiles();
        } catch (Exception e) {
            return null;
        }
        if (fl == null) {
            return null;
        }
        tabGen<servPop3msg> lst = new tabGen<servPop3msg>();
        for (int i = 0; i < fl.length; i++) {
            f = fl[i];
            servPop3msg n = new servPop3msg();
            n.name = f.getName();
            n.size = f.length();
            lst.put(n);
        }
        return lst;
    }

    public String getMsgName(servPop3msg msg) {
        return lower.mailFolders + userN + "/" + msg.name;
    }

    public servPop3msg getByNum(String s) {
        tabGen<servPop3msg> lst = getMsgList();
        if (lst == null) {
            return null;
        }
        int i = bits.str2num(s) - 1;
        if ((i < 0) || (i >= lst.size())) {
            doLine("-ERR no such message");
            return null;
        }
        return lst.get(i);
    }

    public int headSize(List<String> txt) {
        if (txt == null) {
            return 0;
        }
        for (int i = 0; i < txt.size(); i++) {
            if (txt.get(i).length() < 1) {
                return i;
            }
        }
        return txt.size();
    }

    public void doLine(String s) {
        if (debugger.servPop3traf) {
            logger.debug("tx: " + s);
        }
        pipe.linePut(s);
    }

    public boolean doOne() {
        String s = pipe.lineGet(1).trim();
        if (debugger.servPop3traf) {
            logger.debug("rx: " + s);
        }
        cmds cmd = new cmds("", s);
        String a = cmd.word().toLowerCase();
        if (a.length() < 1) {
            return pipe.isClosed() != 0;
        }
        if (a.equals("quit")) {
            if (deled != null) {
                for (int i = 0; i < deled.size(); i++) {
                    userFlash.delete(getMsgName(deled.get(i)));
                }
            }
            doLine("+OK goodbye");
            return true;
        }
        if (a.equals("user")) {
            authed = false;
            userN = cmd.word();
            doLine("+OK password please");
            return false;
        }
        if (a.equals("pass")) {
            authed = false;
            authResult res = lower.authenticList.authUserPass(userN, cmd.word());
            if (res == null) {
                doLine("-ERR error");
                return true;
            }
            if (res.result != authResult.authSuccessful) {
                doLine("-ERR failed");
                return true;
            }
            doLine("+OK welcome");
            deled = new tabGen<servPop3msg>();
            authed = true;
            return false;
        }
        if (a.equals("stls")) {
            if (lower.noneSecKeys()) {
                doLine("-ERR not allowed");
                return false;
            }
            doLine("+OK do it");
            pipeSide res = lower.negoSecSess(pipe, servGeneric.protoTls, new pipeLine(65536, false), null);
            if (res == null) {
                return true;
            }
            res.lineRx = pipeSide.modTyp.modeCRtryLF;
            res.lineTx = pipeSide.modTyp.modeCRLF;
            pipe = res;
            return false;
        }
        if (a.equals("apop")) {
            authed = false;
            userN = cmd.word();
            authResult res = lower.authenticList.authUserApop(getCookie(), userN, cmd.word());
            if (res == null) {
                doLine("-ERR error");
                return true;
            }
            if (res.result != authResult.authSuccessful) {
                doLine("-ERR failed");
                return true;
            }
            doLine("+OK welcome");
            deled = new tabGen<servPop3msg>();
            authed = true;
            return false;
        }
        if (a.equals("capa")) {
            doLine("+OK capability list follows");
            doLine("RESP-CODES");
            doLine("LOGIN-DELAY 600");
            doLine("EXPIRE 0");
            doLine("USER");
            doLine("APOP");
            if (!lower.noneSecKeys()) {
                doLine("STLS");
            }
            doLine("UIDL");
            doLine("TOP");
            doLine(".");
            return false;
        }
        if (a.equals("help")) {
            doLine("+OK you: " + conn.peerAddr + " " + conn.portRem);
            doLine("commands: apop, user, pass, quit, help, capa, noop, top, stat, list, uidl, retr, dele, rset");
            doLine(".");
            return false;
        }
        if (a.equals("noop")) {
            doLine("+OK done");
            return false;
        }
        if (!authed) {
            doLine("-ERR please authenticate");
            return false;
        }
        if (getMsgList() == null) {
            doLine("-ERR invalid credentinals");
            authed = false;
            return true;
        }
        if (a.equals("stat")) {
            tabGen<servPop3msg> lst = getMsgList();
            long o = 0;
            for (int i = 0; i < lst.size(); i++) {
                o += lst.get(i).size;
            }
            doLine("+OK " + lst.size() + " " + o);
            return false;
        }
        if (a.equals("list")) {
            tabGen<servPop3msg> lst = getMsgList();
            doLine("+OK list of messages");
            for (int i = 0; i < lst.size(); i++) {
                doLine((i + 1) + " " + lst.get(i).size);
            }
            doLine(".");
            return false;
        }
        if (a.equals("uidl")) {
            tabGen<servPop3msg> lst = getMsgList();
            doLine("+OK list of messages");
            for (int i = 0; i < lst.size(); i++) {
                doLine((i + 1) + " " + lst.get(i).name);
            }
            doLine(".");
            return false;
        }
        if (a.equals("retr")) {
            servPop3msg msg = getByNum(cmd.word());
            if (msg == null) {
                return false;
            }
            List<String> txt = bits.txt2buf(getMsgName(msg));
            if (txt == null) {
                doLine("-ERR read error");
                return false;
            }
            packText pt = new packText(pipe);
            doLine("+OK " + txt.size() + " lines");
            for (int i = 0; i < txt.size(); i++) {
                pt.dottedSend(txt.get(i));
            }
            doLine(".");
            return false;
        }
        if (a.equals("dele")) {
            servPop3msg msg = getByNum(cmd.word());
            if (msg == null) {
                return false;
            }
            if (deled.add(msg) == null) {
                doLine("+OK marked for deletion");
            } else {
                doLine("-ERR already deleted");
            }
            return false;
        }
        if (a.equals("rset")) {
            deled.clear();
            doLine("+OK deletion list cleared");
            return false;
        }
        if (a.equals("top")) {
            servPop3msg msg = getByNum(cmd.word());
            if (msg == null) {
                return false;
            }
            List<String> txt = bits.txt2buf(getMsgName(msg));
            if (txt == null) {
                doLine("-ERR read error");
                return false;
            }
            packText pt = new packText(pipe);
            doLine("+OK " + txt.size() + " lines");
            int o = headSize(txt) + 1 + bits.str2num(cmd.word());
            for (int i = 0; i < o; i++) {
                pt.dottedSend(txt.get(i));
            }
            doLine(".");
            return false;
        }
        doLine("-ERR bad command");
        return false;
    }

    public void run() {
        try {
            doLine("+OK server ready " + getCookie());
            for (;;) {
                if (doOne()) {
                    break;
                }
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        pipe.setClose();
    }

}
