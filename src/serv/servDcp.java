package serv;

import cfg.cfgAll;
import java.util.Comparator;
import java.util.List;
import pipe.pipeLine;
import pipe.pipeSide;
import prt.prtGenConn;
import prt.prtServS;
import tab.tabGen;
import user.userFilter;
import user.userHelping;
import util.bits;
import util.cmds;
import util.debugger;
import util.logger;

/**
 * direct connect protocol server
 *
 * @author matecsaba
 */
public class servDcp extends servGeneric implements prtServS {

    /**
     * port number
     */
    public static final int port = 411;

    /**
     * logging
     */
    public boolean logging;

    /**
     * list of users
     */
    protected tabGen<servDcpDoer> users = new tabGen<servDcpDoer>();

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "server dcp .*! port " + port,
        "server dcp .*! protocol " + proto2string(protoAllStrm),
        "server dcp .*! no logging"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public String srvName() {
        return "dcp";
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

    public void srvShRun(String beg, List<String> l) {
        cmds.cfgLine(l, !logging, beg, "logging", "");
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("logging")) {
            logging = true;
            return false;
        }
        if (!s.equals("no")) {
            return true;
        }
        s = cmd.word();
        if (s.equals("logging")) {
            logging = false;
            return false;
        }
        return true;
    }

    public void srvHelp(userHelping l) {
        l.add("1 .  logging                   log user communication");
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.timeout = 120000;
        pipe.lineRx = pipeSide.modTyp.modeCR;
        pipe.lineTx = pipeSide.modTyp.modeCR;
        new servDcpDoer(this, pipe);
        return false;
    }

    /**
     * send line to everyone
     *
     * @param s line to send
     */
    protected void sendLn(String s) {
        for (int i = 0; i < users.size(); i++) {
            users.get(i).sendLn(s);
        }
    }

    /**
     * convert lock to key
     *
     * @param lock lock to convert
     * @return converted key
     */
    public static byte[] lock2key(byte[] lock) {
        byte[] key = new byte[lock.length];
        for (int i = 1; i < lock.length; i++) {
            key[i] = (byte) (lock[i] ^ lock[i - 1]);
        }
        key[0] = (byte) (lock[0] ^ lock[lock.length - 1] ^ lock[lock.length - 2] ^ 5);
        for (int i = 0; i < key.length; i++) {
            key[i] = (byte) ((key[i] << 4) | (key[i] >>> 4));
        }
        return key;

    }

}

class servDcpDoer implements Runnable, Comparator<servDcpDoer> {

    public String name;

    public String info;

    private servDcp lower;

    private pipeSide pipe;

    public servDcpDoer(String nam) {
        name = nam;
    }

    public servDcpDoer(servDcp parent, pipeSide conn) {
        lower = parent;
        pipe = conn;
        new Thread(this).start();
    }

    public int compare(servDcpDoer o1, servDcpDoer o2) {
        return o1.name.compareTo(o2.name);
    }

    public void sendLn(String s) {
        if (debugger.servDcpTraf) {
            logger.debug(name + " tx " + s);
        }
        byte[] buf = (s + "|").getBytes();
        pipe.blockingPut(buf, 0, buf.length);
    }

    public String readLn() {
        String s = "";
        for (;;) {
            byte[] buf = new byte[1];
            if (pipe.blockingGet(buf, 0, buf.length) != buf.length) {
                break;
            }
            if (buf[0] == 124) {
                break;
            }
            s += new String(buf);
        }
        if (debugger.servDcpTraf) {
            logger.debug(name + " rx " + s);
        }
        return s;
    }

    public void run() {
        try {
            String lck = "";
            for (int i = 0; i < 16; i++) {
                lck += bits.toHexB(bits.randomB());
            }
            sendLn("$Lock " + lck + " Pk=version1.0.0");
            sendLn("$HubName " + cfgAll.hostName);
            for (;;) {
                if (pipe.isClosed() != 0) {
                    return;
                }
                cmds cmd = new cmds("cmd", readLn());
                String a = cmd.word().toLowerCase();
                if (!a.equals("$validatenick")) {
                    continue;
                }
                a = cmd.word();
                name = a;
                if (lower.users.find(this) != null) {
                    sendLn("$ValidateDenide");
                    continue;
                }
                break;
            }
            info = "$$$$0$";
            lower.users.add(this);
            sendLn("$Hello " + name);
            for (;;) {
                if (pipe.isClosed() != 0) {
                    break;
                }
                cmds cmd = new cmds("cmd", readLn());
                String a = cmd.word().toLowerCase();
                if (a.equals("$quit")) {
                    break;
                }
                if (a.equals("$myinfo")) {
                    cmd.word();
                    cmd.word();
                    info = cmd.getRemaining();
                    lower.sendLn("$MyINFO $ALL " + name + " " + info);
                    continue;
                }
                if (a.equals("$getnicklist")) {
                    for (int i = 0; i < lower.users.size(); i++) {
                        servDcpDoer usr = lower.users.get(i);
                        sendLn("$MyINFO $ALL " + usr.name + " " + usr.info);
                    }
                    continue;
                }
                if (a.equals("$getinfo")) {
                    servDcpDoer usr = lower.users.find(new servDcpDoer(cmd.word()));
                    if (usr == null) {
                        continue;
                    }
                    sendLn("$MyINFO $ALL " + usr.name + " " + usr.info);
                    continue;
                }
                if (a.equals("$connecttome")) {
                    servDcpDoer usr = lower.users.find(new servDcpDoer(cmd.word()));
                    if (usr == null) {
                        continue;
                    }
                    usr.sendLn("$ConnectToMe " + usr.name + " " + cmd.getRemaining());
                    continue;
                }
                if (a.equals("$revconnecttome")) {
                    cmd.word();
                    servDcpDoer usr = lower.users.find(new servDcpDoer(cmd.word()));
                    if (usr == null) {
                        continue;
                    }
                    usr.sendLn("$RevConnectToMe " + name + " " + usr.name);
                    continue;
                }
                if (a.equals("$to:")) {
                    a = cmd.word();
                    servDcpDoer usr = lower.users.find(new servDcpDoer(a));
                    if (usr == null) {
                        continue;
                    }
                    cmd.word();
                    cmd.word();
                    cmd.word();
                    if (lower.logging) {
                        logger.info(name + " " + a + " " + cmd.getRemaining());
                    }
                    usr.sendLn("$To: " + usr.name + " $From: " + name + " $<" + name + "> " + cmd.getRemaining());
                    continue;
                }
                if (!a.startsWith("<")) {
                    continue;
                }
                if (lower.logging) {
                    logger.info(name + " " + cmd.getRemaining());
                }
                lower.sendLn("<" + name + "> " + cmd.getRemaining());
            }
            lower.users.del(this);
            lower.sendLn("$Quit " + name);
        } catch (Exception e) {
            logger.traceback(e);
        }
        pipe.setClose();
    }

}
