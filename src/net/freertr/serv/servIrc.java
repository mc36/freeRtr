package net.freertr.serv;

import java.util.Comparator;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.cfg.cfgAll;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServS;
import net.freertr.tab.tabGen;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.debugger;
import net.freertr.util.logger;
import net.freertr.util.version;

/**
 * internet relay chat (rfc2812) server
 *
 * @author matecsaba
 */
public class servIrc extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servIrc() {
    }

    /**
     * port number
     */
    public static final int port = 6667;

    /**
     * channels
     */
    public tabGen<servIrcChan> chans = new tabGen<servIrcChan>();

    /**
     * logging
     */
    public boolean logging;

    /**
     * peers
     */
    public tabGen<servIrcConn> peers = new tabGen<servIrcConn>();

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        "server irc .*! port " + port,
        "server irc .*! protocol " + proto2string(protoAllStrm),
        "server irc .*! no logging"
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
     * get config
     *
     * @param beg beginning
     * @param l list
     */
    public void srvShRun(String beg, List<String> l, int filter) {
        cmds.cfgLine(l, !logging, beg, "logging", "");
    }

    /**
     * configure
     *
     * @param cmd command
     * @return false on success, true on error
     */
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

    /**
     * get help
     *
     * @param l help
     */
    public void srvHelp(userHelping l) {
        l.add(null, "1 .  logging                   log user communication");
    }

    /**
     * get name
     *
     * @return name
     */
    public String srvName() {
        return "irc";
    }

    /**
     * get port
     *
     * @return port
     */
    public int srvPort() {
        return port;
    }

    /**
     * get protocol
     *
     * @return protocol
     */
    public int srvProto() {
        return protoAllStrm;
    }

    /**
     * initialize
     *
     * @return false on success, true on error
     */
    public boolean srvInit() {
        return genStrmStart(this, new pipeLine(32768, false), 0);
    }

    /**
     * deinitialize
     *
     * @return false on success, true on error
     */
    public boolean srvDeinit() {
        return genericStop(0);
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
        pipe.lineRx = pipeSide.modTyp.modeCRorLF;
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        peers.add(new servIrcConn(this, pipe, id.peerAddr.copyBytes(), id.portRem));
        return false;
    }

    /**
     * delete one user
     *
     * @param peer peer to delete
     */
    public synchronized void delUser(servIrcConn peer) {
        peers.del(peer);
        for (int i = chans.size() - 1; i >= 0; i--) {
            servIrcChan chn = chans.get(i);
            if (chn == null) {
                continue;
            }
            chn.peers.del(peer);
            if (chn.peers.size() > 0) {
                continue;
            }
            chans.del(chn);
        }
    }

    /**
     * find one user
     *
     * @param s username to find
     * @return user, null if nothing
     */
    public servIrcConn findUser(String s) {
        s = s.trim().toLowerCase();
        for (int i = peers.size() - 1; i >= 0; i--) {
            servIrcConn usr = peers.get(i);
            if (usr == null) {
                continue;
            }
            if (s.equals(usr.nick.toLowerCase())) {
                return usr;
            }
        }
        return null;
    }

    /**
     * find one channel
     *
     * @param s channel to find or create
     * @param create true to create if not exists
     * @return channel, null if not found
     */
    public servIrcChan findChan(String s, boolean create) {
        String a = s.trim().toLowerCase();
        for (int i = chans.size() - 1; i >= 0; i--) {
            servIrcChan chn = chans.get(i);
            if (chn == null) {
                continue;
            }
            if (a.equals(chn.name.toLowerCase())) {
                return chn;
            }
        }
        if (!create) {
            return null;
        }
        servIrcChan chn = new servIrcChan(a);
        chans.add(chn);
        return chn;
    }

}

class servIrcChan implements Comparator<servIrcChan> {

    public final String name;

    public tabGen<servIrcConn> peers = new tabGen<servIrcConn>();

    public servIrcChan(String nam) {
        name = nam;
    }

    public int compare(servIrcChan o1, servIrcChan o2) {
        return o1.name.compareTo(o2.name);
    }

    public void rawTx(String s, servIrcConn f) {
        if (debugger.servIrcTraf) {
            logger.debug(name + ": " + s);
        }
        for (int i = peers.size() - 1; i >= 0; i--) {
            servIrcConn usr = peers.get(i);
            if (usr == null) {
                continue;
            }
            if (f != null) {
                if (f.nick.equals(usr.nick)) {
                    continue;
                }
            }
            usr.rawTx(s);
        }
    }

    public String listNames() {
        String s = "";
        for (int i = peers.size() - 1; i >= 0; i--) {
            servIrcConn usr = peers.get(i);
            if (usr == null) {
                continue;
            }
            s += " " + usr.nick;
        }
        return s.trim();
    }

}

class servIrcConn implements Comparator<servIrcConn>, Runnable {

    public final pipeSide conn;

    public final servIrc lower;

    public String nick;

    public final addrIP peer;

    public final int port;

    public boolean need2run;

    public servIrcConn(servIrc parent, pipeSide pipe, addrIP host, int prt) {
        need2run = true;
        lower = parent;
        conn = pipe;
        peer = host.copyBytes();
        port = prt;
        nick = "peer" + peer;
        new Thread(this).start();
        new servIrcKeep(this);
    }

    public int compare(servIrcConn o1, servIrcConn o2) {
        if (o1.port < o2.port) {
            return -1;
        }
        if (o1.port > o2.port) {
            return +1;
        }
        return o1.peer.compare(o1.peer, o2.peer);
    }

    public void run() {
        try {
            for (;;) {
                if (doOne()) {
                    break;
                }
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        conn.setClose();
        need2run = false;
        lower.delUser(this);
    }

    public void rawTx(String s) {
        if (debugger.servIrcTraf) {
            logger.debug("tx: " + s);
        }
        conn.linePut(s);
    }

    public void srvTx(String s) {
        rawTx(":" + cfgAll.hostName + " " + s);
    }

    public void numTx(String n, String s) {
        srvTx(n + " " + nick + " " + s);
    }

    public void namesTx(servIrcChan ch) {
        numTx("353", "= " + ch.name + " :" + ch.listNames());
        numTx("366", ch.name + " :end of names");
    }

    private boolean doOne() {
        String s = conn.lineGet(1).trim();
        if (debugger.servIrcTraf) {
            logger.debug("rx: " + s);
        }
        cmds cmd = new cmds("", s);
        s = cmd.word().toLowerCase();
        if (s.startsWith(":")) {
            s = cmd.word().toLowerCase();
        }
        if (s.length() < 1) {
            return conn.isClosed() != 0;
        }
        if (s.equals("cap")) {
            s = cmd.word().toLowerCase();
            if (s.equals("ls")) {
                srvTx("CAP LS * :");
                return false;
            }
            if (s.equals("req")) {
                srvTx("CAP NAK " + cmd.getRemaining());
                return false;
            }
            if (s.equals("clear")) {
                srvTx("CAP ACK " + cmd.getRemaining());
                return false;
            }
            return false;
        }
        if (s.equals("user")) {
            return false;
        }
        if (s.equals("nick")) {
            nick = cmd.word().trim();
            numTx("001", ":welcome to irc!");
            int i = lower.chans.size();
            numTx("254", i + " :" + i + " channels");
            i = lower.peers.size();
            numTx("254", i + " 0 :" + i + " users");
            return false;
        }
        if (s.equals("quit")) {
            return true;
        }
        if (s.equals("pong")) {
            return false;
        }
        if (s.equals("ping")) {
            rawTx("PONG " + cmd.getRemaining());
            return false;
        }
        if (s.equals("version")) {
            numTx("351", version.namVer + " :");
            return false;
        }
        if (s.equals("time")) {
            numTx("391", cfgAll.hostName + " :" + bits.time2str(cfgAll.timeZoneName, bits.getTime() + cfgAll.timeServerOffset, 4));
            return false;
        }
        if (s.equals("names")) {
            s = cmd.word();
            servIrcChan chn = lower.findChan(s, false);
            if (chn == null) {
                return false;
            }
            namesTx(chn);
            return false;
        }
        if (s.equals("list")) {
            for (int i = lower.chans.size() - 1; i >= 0; i--) {
                servIrcChan chn = lower.chans.get(i);
                if (chn == null) {
                    continue;
                }
                numTx("322", chn.name + " " + chn.peers.size() + " :");
            }
            numTx("323", ":end of list");
            return false;
        }
        if (s.equals("userhost")) {
            servIrcConn usr = lower.findUser(cmd.word());
            if (usr == null) {
                return false;
            }
            numTx("302", ":" + usr.nick + "=+" + usr.nick + "@" + usr.peer);
            return false;
        }
        if (s.equals("join")) {
            s = cmd.word();
            if (!s.startsWith("#")) {
                s = "#" + s;
            }
            servIrcChan chn = lower.findChan(s, true);
            chn.peers.add(this);
            chn.rawTx(":" + nick + "!" + nick + " JOIN " + chn.name, null);
            namesTx(chn);
            return false;
        }
        if (s.equals("part")) {
            s = cmd.word();
            servIrcChan chn = lower.findChan(s, false);
            if (chn == null) {
                return false;
            }
            chn.rawTx(":" + nick + "!" + nick + " PART " + chn.name, null);
            chn.peers.del(this);
            if (chn.peers.size() > 0) {
                return false;
            }
            lower.chans.del(chn);
            return false;
        }
        if (s.equals("privmsg")) {
            s = cmd.word();
            String a = cmd.getRemaining();
            if (lower.logging) {
                logger.info(nick + " " + s + " " + a);
            }
            if (s.startsWith("#")) {
                servIrcChan chn = lower.findChan(s, false);
                if (chn == null) {
                    return false;
                }
                chn.rawTx(":" + nick + "!" + nick + " PRIVMSG " + chn.name + " " + a, this);
            } else {
                servIrcConn usr = lower.findUser(s);
                if (usr == null) {
                    return false;
                }
                usr.rawTx(":" + nick + "!" + nick + " PRIVMSG " + usr.nick + " " + a);
            }
            return false;
        }
        rawTx("ERROR bad command " + s + " " + cmd.getRemaining());
        return false;
    }

}

class servIrcKeep implements Runnable {

    private final servIrcConn lower;

    public servIrcKeep(servIrcConn conn) {
        lower = conn;
        new Thread(this).start();
    }

    public void run() {
        try {
            for (;;) {
                if (!lower.need2run) {
                    return;
                }
                lower.rawTx("PING " + bits.getTime());
                bits.sleep(30000);
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}
