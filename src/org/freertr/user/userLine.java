package org.freertr.user;

import java.util.ArrayList;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
import org.freertr.auth.authGeneric;
import org.freertr.auth.authResult;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgAuther;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgInit;
import org.freertr.enc.enc7bit;
import org.freertr.pipe.pipeSetting;
import org.freertr.pipe.pipeSide;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabRouteIface;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.logger;

/**
 * one line descriptor
 *
 * @author matecsaba
 */
public class userLine {

    /**
     * create instance
     */
    public userLine() {
    }

    /**
     * interface to use
     */
    public cfgIfc execIface;

    /**
     * exec timeout
     */
    public int execTimeOut = 5 * 60 * 1000;

    /**
     * width of terminal screen
     */
    public int execWidth = 79;

    /**
     * height of terminal screen
     */
    public int execHeight = 24;

    /**
     * history size
     */
    public int execHistory = 64;

    /**
     * rib lines
     */
    public int execRibLines = 8192;

    /**
     * show timestamps
     */
    public boolean execTimes;

    /**
     * colorize
     */
    public userFormat.colorMode execColor = userFormat.colorMode.normal;

    /**
     * boxer
     */
    public userFormat.boxerMode execBoxer = userFormat.boxerMode.normal;

    /**
     * normal color
     */
    public int execColNrm = userScreen.colWhite;

    /**
     * prompt color
     */
    public int execColPrm = userScreen.colBrGreen;

    /**
     * header color
     */
    public int execColHdr = userScreen.colBrYellow;

    /**
     * colorize
     */
    public userScreen.ansiMode ansiMode = userScreen.ansiMode.normal;

    /**
     * space as tab
     */
    public boolean execSpace;

    /**
     * caps lock
     */
    public boolean execCaps;

    /**
     * term bell
     */
    public boolean execBells;

    /**
     * table mode
     */
    public userFormat.tableMode execTables = userFormat.tableMode.normal;

    /**
     * log exec events
     */
    public boolean execLogging = false;

    /**
     * log login events
     */
    public boolean loginLogging = false;

    /**
     * display last login, 0=none, 1=global, 2=local
     */
    public int loginLast = 0;

    /**
     * password stars
     */
    public boolean passStars = false;

    /**
     * authentication list
     */
    public authGeneric authenticList;

    /**
     * authorization list
     */
    public authGeneric authorizeList;

    /**
     * command to use on login
     */
    public String autoCommand = "";

    /**
     * terminate session after command
     */
    public boolean autoHangup = false;

    /**
     * display title
     */
    public boolean title = true;

    /**
     * display banner
     */
    public boolean banner = true;

    /**
     * detect terminals
     */
    public boolean detect = false;

    /**
     * disconnect warning
     */
    public boolean expirity = false;

    /**
     * monitor from start
     */
    public boolean monitor = false;

    /**
     * username prompt
     */
    public String promptUser = "username:";

    /**
     * password prompt
     */
    public String promptPass = "password:";

    /**
     * failed prompt
     */
    public String promptFailed = "authentication failed";

    /**
     * welcome prompt
     */
    public String promptWelcome = "welcome";

    /**
     * success prompt
     */
    public String promptSuccess = "line ready";

    /**
     * last prompt
     */
    public String promptLast = "before:";

    /**
     * goodbye prompt
     */
    public String promptGoodbye = "see you later";

    /**
     * prompt retry
     */
    public int promptRetry = 3;

    /**
     * activation character
     */
    public int promptActivate = 13;

    /**
     * escape character
     */
    public int promptEscape = 3;

    /**
     * deactivation character
     */
    public int promptDeActive = 65536;

    /**
     * prompt delay
     */
    public int promptDelay = 3000;

    /**
     * prompt timeout
     */
    public int promptTimeout = 60 * 1000;

    /**
     * prompt privilege
     */
    public int promptPrivilege = 15;

    /**
     * list of users
     */
    protected final static tabGen<userLineHandler> loggedUsers = new tabGen<userLineHandler>();

    /**
     * previous configure
     */
    public static String prevConfiger = "<bootup>";

    /**
     * previous user
     */
    protected static String prevUserGlb = "you are the first on this box";

    /**
     * previous user
     */
    protected String prevUserLoc = "you are the first on this line";

    private static void convLine(List<Integer> bts, String a) {
        byte[] b = a.getBytes();
        for (int i = 0; i < b.length; i++) {
            int o = b[i];
            bts.add(o);
        }
        bts.add(13);
        bts.add(10);
    }

    /**
     * show logger in users
     *
     * @return list
     */
    public static userFormat listLoggedIns() {
        userFormat res = new userFormat("|", "user|state|idle|from|since");
        for (int i = 0; i < loggedUsers.size(); i++) {
            userLineHandler cur = loggedUsers.get(i);
            if (cur == null) {
                continue;
            }
            res.add("" + cur);
        }
        return res;
    }

    /**
     * send broadcast message
     *
     * @param a source of message
     * @param t message to be sent
     * @return messages sent successfully
     */
    protected static int sendBcastMsg(String a, List<String> t) {
        a = enc7bit.decodeExtStr("" + a);
        List<Integer> bts = new ArrayList<Integer>();
        convLine(bts, "");
        convLine(bts, "");
        convLine(bts, "message from " + a + ":");
        for (int i = 0; i < t.size(); i++) {
            a = t.get(i);
            a = enc7bit.decodeExtStr(a);
            convLine(bts, a);
        }
        byte[] buf = new byte[bts.size()];
        for (int i = 0; i < buf.length; i++) {
            int o = bts.get(i);
            buf[i] = (byte) o;
        }
        int o = 0;
        for (int i = 0; i < loggedUsers.size(); i++) {
            userLineHandler u = loggedUsers.get(i);
            if (u == null) {
                continue;
            }
            if (u.sendBcastMsg(buf)) {
                continue;
            }
            o++;
        }
        return o;
    }

    /**
     * do commands
     *
     * @param exe exec handler
     * @param cfg config handler
     */
    public static void doCommands(userExec exe, userConfig cfg) {
        for (;;) {
            exe.last = bits.getTime();
            userExec.cmdRes i = exe.doCommand();
            if (i == userExec.cmdRes.command) {
                continue;
            }
            if (i == userExec.cmdRes.logout) {
                return;
            }
            if (i != userExec.cmdRes.config) {
                continue;
            }
            if (cfgAll.configExclusive > 0) {
                cfgAll.configExclusive++;
            }
            String remote = exe.pipe.settingsGet(pipeSetting.origin, "?");
            logger.warn(exe.username + " configuring from " + remote);
            cfg.resetMode();
            List<String> sesStart = null;
            if (exe.rollback) {
                logger.info("configuration checkpoint frozen!");
                sesStart = cfgAll.getShRun(1);
            }
            for (;;) {
                exe.last = bits.getTime();
                if (cfg.doCommand()) {
                    break;
                }
            }
            if (exe.pipe.isClosed() == 0) {
                if (sesStart != null) {
                    logger.info("configuration checkpoint released!");
                }
                sesStart = null;
            }
            if (sesStart != null) {
                sesStart = userFilter.getDiffs(cfgAll.getShRun(1), sesStart);
                int res = cfgInit.executeSWcommands(sesStart, false);
                logger.info("configuration reverted to frozen checkpoint with " + res + " errors.");
            }
            if (cfgAll.configExclusive > 1) {
                cfgAll.configExclusive--;
            }
            if (cfgAll.configAsave) {
                exe.executeCommand("write memory");
            }
            logger.warn(exe.username + " configured from " + remote);
        }
    }

    /**
     * get running configuration
     *
     * @param beg beginning string
     * @param lst list to append
     * @param filter filter defaults
     */
    public void getShRun(String beg, List<String> lst, int filter) {
        if (execIface == null) {
            lst.add(beg + "no exec interface");
        } else {
            lst.add(beg + "exec interface " + execIface.name);
        }
        lst.add(beg + "exec timeout " + execTimeOut);
        lst.add(beg + "exec width " + execWidth);
        lst.add(beg + "exec height " + execHeight);
        lst.add(beg + "exec history " + execHistory);
        lst.add(beg + "exec riblines " + execRibLines);
        cmds.cfgLine(lst, !execTimes, beg, "exec timestamp", "");
        cmds.cfgLine(lst, !execSpace, beg, "exec spacetab", "");
        cmds.cfgLine(lst, !execCaps, beg, "exec capslock", "");
        cmds.cfgLine(lst, !execBells, beg, "exec bells", "");
        lst.add(beg + "exec background " + userScreen.color2string(execColNrm >>> 16));
        lst.add(beg + "exec foreground " + userScreen.color2string(execColNrm));
        lst.add(beg + "exec prompt " + userScreen.color2string(execColPrm));
        lst.add(beg + "exec header " + userScreen.color2string(execColHdr));
        lst.add(beg + "exec colorize " + userFormat.colmod2str(execColor));
        lst.add(beg + "exec boxer " + userFormat.boxmod2str(execBoxer));
        lst.add(beg + "exec ansimode " + userScreen.ansimod2str(ansiMode));
        lst.add(beg + "exec tablemode " + userFormat.tabmod2str(execTables));
        lst.add(beg + "exec welcome " + promptWelcome);
        lst.add(beg + "exec ready " + promptSuccess);
        lst.add(beg + "exec before " + promptLast);
        lst.add(beg + "exec bye " + promptGoodbye);
        cmds.cfgLine(lst, !execLogging, beg, "exec logging", "");
        lst.add(beg + "exec autocommand " + autoCommand);
        cmds.cfgLine(lst, !autoHangup, beg, "exec autohangup", "");
        cmds.cfgLine(lst, !banner, beg, "exec banner", "");
        cmds.cfgLine(lst, !detect, beg, "exec detect", "");
        cmds.cfgLine(lst, !title, beg, "exec title", "");
        cmds.cfgLine(lst, !expirity, beg, "exec expirity", "");
        cmds.cfgLine(lst, !monitor, beg, "exec monitor", "");
        lst.add(beg + "exec privilege " + promptPrivilege);
        if (authorizeList == null) {
            lst.add(beg + "no exec authorization");
        } else {
            lst.add(beg + "exec authorization " + authorizeList.autName);
        }
        if (authenticList == null) {
            lst.add(beg + "no login authentication");
        } else {
            lst.add(beg + "login authentication " + authenticList.autName);
        }
        lst.add(beg + "login escape " + promptEscape);
        lst.add(beg + "login activate " + promptActivate);
        lst.add(beg + "login deactivate " + promptDeActive);
        lst.add(beg + "login timeout " + promptTimeout);
        lst.add(beg + "login retry " + promptRetry);
        lst.add(beg + "login delay " + promptDelay);
        lst.add(beg + "login user " + promptUser);
        lst.add(beg + "login pass " + promptPass);
        lst.add(beg + "login fail " + promptFailed);
        cmds.cfgLine(lst, !passStars, beg, "login stars", "");
        cmds.cfgLine(lst, !loginLogging, beg, "login logging", "");
        String a;
        switch (loginLast) {
            case 0:
                a = "none";
                break;
            case 1:
                a = "global";
                break;
            case 2:
                a = "local";
                break;
            case 3:
                a = "both";
                break;
            default:
                a = "unknown:" + loginLast;
                break;
        }
        lst.add(beg + "login last " + a);
        if ((filter & 1) == 0) {
            return;
        }
        List<String> res = userFilter.filterText(lst, userRead.linedefF);
        lst.clear();
        lst.addAll(res);
    }

    /**
     * parse commands
     *
     * @param cmd commands
     * @return true if error happened
     */
    public boolean doCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("exec")) {
            s = cmd.word();
            if (s.equals("interface")) {
                execIface = cfgAll.ifcFind(cmd.word(), 0);
                if (execIface == null) {
                    return false;
                }
                if (execIface.type != tabRouteIface.ifaceType.dialer) {
                    execIface = null;
                    return false;
                }
                return false;
            }
            if (s.equals("logging")) {
                execLogging = true;
                return false;
            }
            if (s.equals("timeout")) {
                execTimeOut = bits.str2num(cmd.word());
                return false;
            }
            if (s.equals("width")) {
                execWidth = bits.str2num(cmd.word());
                return false;
            }
            if (s.equals("height")) {
                execHeight = bits.str2num(cmd.word());
                return false;
            }
            if (s.equals("history")) {
                execHistory = bits.str2num(cmd.word());
                return false;
            }
            if (s.equals("riblines")) {
                execRibLines = bits.str2num(cmd.word());
                return false;
            }
            if (s.equals("timestamp")) {
                execTimes = true;
                return false;
            }
            if (s.equals("background")) {
                int i = userScreen.string2color(cmd.word());
                if (i < 0) {
                    return false;
                }
                execColNrm = userScreen.setBackground(execColNrm, i);
                execColHdr = userScreen.setBackground(execColHdr, i);
                execColPrm = userScreen.setBackground(execColPrm, i);
                return false;
            }
            if (s.equals("foreground")) {
                int i = userScreen.string2color(cmd.word());
                if (i < 0) {
                    return false;
                }
                execColNrm = userScreen.setForeground(execColNrm, i);
                return false;
            }
            if (s.equals("header")) {
                int i = userScreen.string2color(cmd.word());
                if (i < 0) {
                    return false;
                }
                execColHdr = userScreen.setForeground(execColHdr, i);
                return false;
            }
            if (s.equals("prompt")) {
                int i = userScreen.string2color(cmd.word());
                if (i < 0) {
                    return false;
                }
                execColPrm = userScreen.setForeground(execColPrm, i);
                return false;
            }
            if (s.equals("colorize")) {
                execColor = userFormat.str2colmod(cmd.word());
                return false;
            }
            if (s.equals("boxer")) {
                execBoxer = userFormat.str2boxmod(cmd.word());
                return false;
            }
            if (s.equals("spacetab")) {
                execSpace = true;
                return false;
            }
            if (s.equals("capslock")) {
                execCaps = true;
                return false;
            }
            if (s.equals("bells")) {
                execBells = true;
                return false;
            }
            if (s.equals("tablemode")) {
                execTables = userFormat.str2tabmod(cmd.word());
                return false;
            }
            if (s.equals("before")) {
                promptLast = cmd.getRemaining();
                return false;
            }
            if (s.equals("ready")) {
                promptSuccess = cmd.getRemaining();
                return false;
            }
            if (s.equals("welcome")) {
                promptWelcome = cmd.getRemaining();
                return false;
            }
            if (s.equals("bye")) {
                promptGoodbye = cmd.getRemaining();
                return false;
            }
            if (s.equals("banner")) {
                banner = true;
                return false;
            }
            if (s.equals("detect")) {
                detect = true;
                return false;
            }
            if (s.equals("title")) {
                title = true;
                return false;
            }
            if (s.equals("expirity")) {
                expirity = true;
                return false;
            }
            if (s.equals("monitor")) {
                monitor = true;
                return false;
            }
            if (s.equals("autohangup")) {
                autoHangup = true;
                return false;
            }
            if (s.equals("autocommand")) {
                autoCommand = cmd.getRemaining();
                return false;
            }
            if (s.equals("privilege")) {
                promptPrivilege = bits.str2num(cmd.word()) & 0xf;
                return false;
            }
            if (s.equals("authorization")) {
                cfgAuther lst = cfgAll.autherFind(cmd.word(), null);
                if (lst == null) {
                    cmd.error("no such auth list");
                    return false;
                }
                authorizeList = lst.getAuther();
                return false;
            }
            return true;
        }
        if (s.equals("login")) {
            s = cmd.word();
            if (s.equals("last")) {
                loginLast = 1;
                for (;;) {
                    s = cmd.word();
                    if (s.length() < 1) {
                        break;
                    }
                    if (s.equals("global")) {
                        loginLast = 1;
                        continue;
                    }
                    if (s.equals("local")) {
                        loginLast = 2;
                        continue;
                    }
                    if (s.equals("both")) {
                        loginLast = 3;
                        continue;
                    }
                    if (s.equals("none")) {
                        loginLast = 0;
                        continue;
                    }
                }
                return false;
            }
            if (s.equals("logging")) {
                loginLogging = true;
                return false;
            }
            if (s.equals("authentication")) {
                cfgAuther lst = cfgAll.autherFind(cmd.word(), null);
                if (lst == null) {
                    cmd.error("no such auth list");
                    return false;
                }
                authenticList = lst.getAuther();
                return false;
            }
            if (s.equals("stars")) {
                passStars = true;
                return false;
            }
            if (s.equals("escape")) {
                promptEscape = bits.str2num(cmd.word());
                return false;
            }
            if (s.equals("activate")) {
                promptActivate = bits.str2num(cmd.word());
                return false;
            }
            if (s.equals("deactivate")) {
                promptDeActive = bits.str2num(cmd.word());
                return false;
            }
            if (s.equals("retry")) {
                promptRetry = bits.str2num(cmd.word());
                return false;
            }
            if (s.equals("delay")) {
                promptDelay = bits.str2num(cmd.word());
                return false;
            }
            if (s.equals("timeout")) {
                promptTimeout = bits.str2num(cmd.word());
                return false;
            }
            if (s.equals("user")) {
                promptUser = cmd.getRemaining();
                return false;
            }
            if (s.equals("pass")) {
                promptPass = cmd.getRemaining();
                return false;
            }
            if (s.equals("fail")) {
                promptFailed = cmd.getRemaining();
                return false;
            }
            return true;
        }
        if (!s.equals(cmds.negated)) {
            return true;
        }
        s = cmd.word();
        if (s.equals("exec")) {
            s = cmd.word();
            if (s.equals("timestamp")) {
                execTimes = false;
                return false;
            }
            if (s.equals("spacetab")) {
                execSpace = false;
                return false;
            }
            if (s.equals("capslock")) {
                execCaps = false;
                return false;
            }
            if (s.equals("bells")) {
                execBells = false;
                return false;
            }
            if (s.equals("interface")) {
                execIface = null;
                return false;
            }
            if (s.equals("logging")) {
                execLogging = false;
                return false;
            }
            if (s.equals("banner")) {
                banner = false;
                return false;
            }
            if (s.equals("detect")) {
                detect = false;
                return false;
            }
            if (s.equals("title")) {
                title = false;
                return false;
            }
            if (s.equals("expirity")) {
                expirity = false;
                return false;
            }
            if (s.equals("monitor")) {
                monitor = false;
                return false;
            }
            if (s.equals("autohangup")) {
                autoHangup = false;
                return false;
            }
            if (s.equals("autocommand")) {
                autoCommand = "";
                return false;
            }
            if (s.equals("authorization")) {
                authorizeList = null;
                return false;
            }
            return true;
        }
        if (s.equals("login")) {
            s = cmd.word();
            if (s.equals("last")) {
                loginLast = 0;
                return false;
            }
            if (s.equals("logging")) {
                loginLogging = false;
                return false;
            }
            if (s.equals("authentication")) {
                authenticList = null;
                return false;
            }
            if (s.equals("stars")) {
                passStars = false;
                return false;
            }
            return true;
        }
        return true;
    }

    /**
     * get help text
     *
     * @param l list of commands
     */
    public void getHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "exec", "set executable parameters");
        l.add(null, false, 2, new int[]{3}, "interface", "set interface to use for framing");
        l.add(null, false, 3, new int[]{-1}, "<name:ifc>", "name of interface");
        l.add(null, false, 2, new int[]{-1}, "logging", "enable logging");
        l.add(null, false, 2, new int[]{-1}, "timestamp", "enable timestamps");
        l.add(null, false, 2, new int[]{3, -1}, "boxer", "make shows cute");
        l.add(null, false, 3, new int[]{-1}, "normal", "select normal mode");
        l.add(null, false, 3, new int[]{-1}, "simple", "select simple mode");
        l.add(null, false, 3, new int[]{-1}, "cowsay", "select coway mode");
        l.add(null, false, 2, new int[]{3, -1}, "colorize", "enable colorization");
        l.add(null, false, 3, new int[]{-1}, "normal", "select normal mode");
        l.add(null, false, 3, new int[]{-1}, "header", "select header mode");
        l.add(null, false, 3, new int[]{-1}, "rainbow", "select rainbow mode");
        l.add(null, false, 3, new int[]{-1}, "zeroes", "select zeroes mode");
        l.add(null, false, 3, new int[]{-1}, "prompt", "select prompt mode");
        l.add(null, false, 2, new int[]{-1}, "spacetab", "enable space as tab");
        l.add(null, false, 2, new int[]{-1}, "capslock", "enable caps lock");
        l.add(null, false, 2, new int[]{-1}, "bells", "enable beep boops");
        l.add(null, false, 2, new int[]{3}, "tablemode", "set table mode");
        userFormat.listTableModes(l, 3);
        l.add(null, false, 2, new int[]{3}, "ansimode", "select ansi coloring mode");
        l.add(null, false, 3, new int[]{-1}, "none", "select black and white mode");
        l.add(null, false, 3, new int[]{-1}, "original", "select 8 colors mode");
        l.add(null, false, 3, new int[]{-1}, "normal", "select 16 colors mode");
        l.add(null, false, 3, new int[]{-1}, "indexed", "select 256 colors mode");
        l.add(null, false, 3, new int[]{-1}, "palette", "select 16m colors mode");
        l.add(null, false, 2, new int[]{3}, "background", "select background color");
        l.add(null, false, 2, new int[]{3}, "foreground", "select foreground color");
        l.add(null, false, 2, new int[]{3}, "prompt", "select prompt color");
        l.add(null, false, 2, new int[]{3}, "header", "select header color");
        l.add(null, false, 3, new int[]{-1}, "black", "select color");
        l.add(null, false, 3, new int[]{-1}, "red", "select color");
        l.add(null, false, 3, new int[]{-1}, "green", "select color");
        l.add(null, false, 3, new int[]{-1}, "yellow", "select color");
        l.add(null, false, 3, new int[]{-1}, "blue", "select color");
        l.add(null, false, 3, new int[]{-1}, "magenta", "select color");
        l.add(null, false, 3, new int[]{-1}, "cyan", "select color");
        l.add(null, false, 3, new int[]{-1}, "white", "select color");
        l.add(null, false, 3, new int[]{-1}, "bright-black", "select color");
        l.add(null, false, 3, new int[]{-1}, "bright-red", "select color");
        l.add(null, false, 3, new int[]{-1}, "bright-green", "select color");
        l.add(null, false, 3, new int[]{-1}, "bright-yellow", "select color");
        l.add(null, false, 3, new int[]{-1}, "bright-blue", "select color");
        l.add(null, false, 3, new int[]{-1}, "bright-magenta", "select color");
        l.add(null, false, 3, new int[]{-1}, "bright-cyan", "select color");
        l.add(null, false, 3, new int[]{-1}, "bright-white", "select color");
        l.add(null, false, 2, new int[]{3}, "timeout", "set timeout value");
        l.add(null, false, 3, new int[]{-1}, "<num>", "timeout in milliseconds");
        l.add(null, false, 2, new int[]{3}, "width", "number of columns");
        l.add(null, false, 3, new int[]{-1}, "<num>", "width of terminal");
        l.add(null, false, 2, new int[]{3}, "height", "set height of terminal");
        l.add(null, false, 3, new int[]{-1}, "<num>", "number of lines");
        l.add(null, false, 2, new int[]{3}, "history", "set history size");
        l.add(null, false, 3, new int[]{-1}, "<num>", "number of lines");
        l.add(null, false, 2, new int[]{3}, "riblines", "set rib buffer size");
        l.add(null, false, 3, new int[]{-1}, "<num>", "number of lines");
        l.add(null, false, 2, new int[]{3}, "ready", "set ready message");
        l.add(null, false, 3, new int[]{3, -1}, "<text>", "text to display");
        l.add(null, false, 2, new int[]{3}, "bye", "set goodbye message");
        l.add(null, false, 3, new int[]{3, -1}, "<text>", "text to display");
        l.add(null, false, 2, new int[]{3}, "welcome", "set welcome message");
        l.add(null, false, 3, new int[]{3, -1}, "<text>", "text to display");
        l.add(null, false, 2, new int[]{3}, "before", "set previous user message");
        l.add(null, false, 3, new int[]{3, -1}, "<text>", "text to display");
        l.add(null, false, 2, new int[]{3}, "autocommand", "set automatic command");
        l.add(null, false, 3, new int[]{3, -1}, "<text>", "autocommand of user");
        l.add(null, false, 2, new int[]{-1}, "banner", "display banner");
        l.add(null, false, 2, new int[]{-1}, "detect", "detect terminal size");
        l.add(null, false, 2, new int[]{-1}, "title", "send hostname");
        l.add(null, false, 2, new int[]{-1}, "expirity", "display expirity warnings");
        l.add(null, false, 2, new int[]{-1}, "monitor", "display logging information");
        l.add(null, false, 2, new int[]{-1}, "autohangup", "disconnect user after autocommand");
        l.add(null, false, 2, new int[]{3}, "privilege", "set default privilege");
        l.add(null, false, 3, new int[]{-1}, "<num>", "privilege of terminal");
        l.add(null, false, 2, new int[]{3}, "authorization", "set authorization");
        l.add(null, false, 3, new int[]{-1}, "<name:aaa>", "name of authentication list");
        l.add(null, false, 1, new int[]{2}, "login", "set login parameters");
        l.add(null, false, 2, new int[]{3, -1}, "last", "display last login line");
        l.add(null, false, 3, new int[]{-1}, "none", "nothing");
        l.add(null, false, 3, new int[]{-1}, "global", "globally");
        l.add(null, false, 3, new int[]{-1}, "local", "locally");
        l.add(null, false, 3, new int[]{-1}, "both", "everything");
        l.add(null, false, 2, new int[]{-1}, "logging", "enable logging");
        l.add(null, false, 2, new int[]{-1}, "stars", "use stars in password prompt");
        l.add(null, false, 2, new int[]{3}, "authentication", "set authentication");
        l.add(null, false, 3, new int[]{-1}, "<name:aaa>", "name of authentication list");
        l.add(null, false, 2, new int[]{3}, "escape", "set escape character");
        l.add(null, false, 3, new int[]{-1}, "<num>", "ascii number");
        l.add(null, false, 2, new int[]{3}, "activate", "set activation character");
        l.add(null, false, 3, new int[]{-1}, "<num>", "ascii number");
        l.add(null, false, 2, new int[]{3}, "deactivate", "set deactivation character");
        l.add(null, false, 3, new int[]{-1}, "<num>", "ascii number");
        l.add(null, false, 2, new int[]{3}, "timeout", "set timeout value");
        l.add(null, false, 3, new int[]{-1}, "<num>", "timeout in milliseconds");
        l.add(null, false, 2, new int[]{3}, "retry", "set retry value");
        l.add(null, false, 3, new int[]{-1}, "<num>", "number of tries");
        l.add(null, false, 2, new int[]{3}, "delay", "set delay value");
        l.add(null, false, 3, new int[]{-1}, "<num>", "timeout in milliseconds");
        l.add(null, false, 2, new int[]{3}, "user", "set username prompt");
        l.add(null, false, 3, new int[]{3, -1}, "<text>", "text to display");
        l.add(null, false, 2, new int[]{3}, "pass", "set password prompt");
        l.add(null, false, 3, new int[]{3, -1}, "<text>", "text to display");
        l.add(null, false, 2, new int[]{3}, "fail", "set failed message");
        l.add(null, false, 3, new int[]{3, -1}, "<text>", "text to display");
    }

    /**
     * create new cli handler
     *
     * @param pip pipeline to work on
     * @param nam name of remote
     * @param phys physical lines, 0=no, 1=yes, 2=console
     */
    public void createHandler(pipeSide pip, String nam, int phys) {
        new userLineHandler(this, pip, nam, phys);
    }

}

class userLineHandler implements Runnable, Comparable<userLineHandler> {

    public final int hsh;

    public final pipeSide pipe;

    public final userLine parent;

    public final String remote;

    public final int physical;

    public final long since;

    public authResult user;

    public boolean preauthed;

    public Timer expTim;

    public userExec exe;

    public userConfig cfg;

    public userLineHandler(userLine prnt, pipeSide pip, String rem, int phys) {
        parent = prnt;
        pipe = pip;
        remote = rem;
        physical = phys;
        pipe.setTime(parent.execTimeOut);
        pipe.lineRx = pipeSide.modTyp.modeCRtorLF;
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        hsh = pip.hashCode();
        since = bits.getTime();
        new Thread(this).start();
    }

    public boolean sendBcastMsg(byte[] buf) {
        if (isClosed()) {
            return false;
        }
        return pipe.nonBlockPut(buf, 0, buf.length) != buf.length;
    }

    public boolean isClosed() {
        if (pipe == null) {
            return true;
        }
        if (pipe.isClosed() == 0) {
            return false;
        }
        userLine.loggedUsers.del(this);
        return true;
    }

    public long getIdle() {
        if (exe == null) {
            return 0;
        }
        return exe.last;
    }

    public int getState() {
        if (pipe == null) {
            return 1;
        }
        if (isClosed()) {
            return 2;
        }
        boolean mon;
        mon = pipe.settingsGet(pipeSetting.logging, false);
        if (mon) {
            return 3;
        } else {
            return 4;
        }
    }

    public final static String state2str(int i) {
        switch (i) {
            case 1:
                return "nulled";
            case 2:
                return "disconn";
            case 3:
                return "monitor";
            case 4:
                return "regular";
            default:
                return "unknown#" + i;
        }
    }

    public String toString() {
        return user.user + "|" + state2str(getState()) + "|" + bits.timePast(getIdle()) + "|" + remote + "|" + bits.timePast(since);
    }

    /**
     * return user readable string
     *
     * @return
     */
    public String toUserStr() {
        String s = null;
        if (user != null) {
            s = "" + user.user;
        }
        return "user=" + s + " rem=" + remote + " after " + bits.timePast(since);
    }

    private void doInit() {
        user = new authResult();
        user.privilege = parent.promptPrivilege;
        pipe.settingsAdd(pipeSetting.authed, user);
        pipe.wait4ready(0);
        authResult mine = user;
        user = pipe.settingsGet(pipeSetting.authed, user);
        preauthed = mine != user;
    }

    private void doAuth() {
        if (pipe.isClosed() != 0) {
            return;
        }
        pipe.setTime(parent.promptTimeout);
        if (parent.title) {
            userScreen.sendTit(pipe, cfgAll.hostName);
        }
        if (parent.detect) {
            userScreen.updtSiz(pipe);
        }
        if (parent.banner) {
            pipe.blockingPut(cfgAll.banner, 0, cfgAll.banner.length);
        }
        if (parent.banner) {
            pipe.linePut(parent.promptWelcome);
        }
        if (preauthed) {
            return;
        }
        if (parent.authenticList == null) {
            return;
        }
        for (int cnt = 0;; cnt++) {
            String usr;
            for (;;) {
                pipe.strPut(parent.promptUser);
                usr = pipe.lineGet(0x32);
                if (usr.length() > 0) {
                    break;
                }
                if (pipe.isClosed() != 0) {
                    break;
                }
            }
            pipe.strPut(parent.promptPass);
            int i;
            if (parent.passStars) {
                i = 0x33;
            } else {
                i = 0x31;
            }
            String pwd = pipe.lineGet(i);
            user = parent.authenticList.authUserPass(usr, pwd);
            if (user.result == authResult.authSuccessful) {
                break;
            }
            if (parent.loginLogging) {
                logger.info("login failed (" + usr + ") from " + remote);
            }
            bits.sleep(parent.promptDelay);
            pipe.linePut(parent.promptFailed);
            if (cnt < parent.promptRetry) {
                continue;
            }
            pipe.setClose();
            return;
        }
    }

    private void doExec() {
        if (pipe.isClosed() != 0) {
            return;
        }
        if (user.privilege > parent.promptPrivilege) {
            user.privilege = parent.promptPrivilege;
        }
        if (parent.loginLogging) {
            logger.info(user.user + " logged in from " + remote);
        }
        if (parent.banner) {
            pipe.linePut(parent.promptSuccess);
        }
        switch (parent.loginLast) {
            case 0:
                break;
            case 1:
                pipe.linePut(parent.promptLast + userLine.prevUserGlb);
                break;
            case 2:
                pipe.linePut(parent.promptLast + parent.prevUserLoc);
                break;
            case 3:
                pipe.linePut(parent.promptLast + userLine.prevUserGlb);
                pipe.linePut(parent.promptLast + parent.prevUserLoc);
                break;
            default:
                pipe.linePut(parent.promptLast + "unknown:" + parent.loginLast);
                break;
        }
        String s = user.user + " from " + remote + " at " + logger.getTimestamp();
        userLine.prevConfiger = s;
        userLine.prevUserGlb = s;
        parent.prevUserLoc = s;
        pipe.setTime(parent.execTimeOut);
        userRead rdr = new userRead(pipe, parent);
        pipe.settingsPut(pipeSetting.origin, remote);
        pipe.settingsPut(pipeSetting.authed, user);
        exe = new userExec(pipe, rdr);
        exe.privileged = user.privilege >= 15;
        exe.framedIface = parent.execIface;
        exe.physicalLin = physical != 0;
        exe.authorization = parent.authorizeList;
        exe.username = user.user;
        exe.needExpand = true;
        cfg = new userConfig(pipe, rdr);
        cfg.authorization = parent.authorizeList;
        cfg.username = user.user;
        cfg.needExpand = true;
        s = parent.autoCommand;
        if (s.length() > 0) {
            s = exe.repairCommand(s);
            exe.executeCommand(s);
        }
        if (parent.autoHangup) {
            if (parent.loginLogging) {
                logger.info(user.user + " logged out from " + remote);
            }
            return;
        }
        s = user.autoCommand;
        if (s.length() > 0) {
            s = exe.repairCommand(s);
            exe.executeCommand(s);
        }
        if (user.autoHangup) {
            if (parent.loginLogging) {
                logger.info(user.user + " logged out from " + remote);
            }
            return;
        }
        if (parent.monitor) {
            logger.pipeStart(pipe);
        }
        if (parent.expirity && (physical == 0)) {
            exe.last = bits.getTime();
            expTim = new Timer();
            userLineExpirity task = new userLineExpirity(this);
            expTim.schedule(task, 30 * 1000, 60 * 1000);
        }
        if (physical == 2) {
            for (;;) {
                userLine.doCommands(exe, cfg);
                pipe.linePut("% not possible on this line");
            }
        }
        userLine.doCommands(exe, cfg);
        if (parent.loginLogging) {
            logger.info(user.user + " logged out from " + remote);
        }
    }

    public void doExpire() {
        long tim = bits.getTime() - exe.last;
        if (tim > parent.execTimeOut) {
            if (parent.banner) {
                pipe.linePut(parent.promptGoodbye);
            }
            pipe.setClose();
            return;
        }
        if (tim < 60000) {
            return;
        }
        pipe.linePut("% session is about to expire in " + bits.timeDump((parent.execTimeOut - tim) / 1000));
    }

    public void run() {
        userLine.loggedUsers.put(this);
        try {
            doInit();
            doAuth();
            doExec();
            if (parent.banner) {
                pipe.linePut(parent.promptGoodbye);
            }
        } catch (Exception e) {
            logger.traceback(e, toUserStr() + " commands");
        }
        try {
            expTim.cancel();
        } catch (Exception e) {
        }
        pipe.setClose();
        userLine.loggedUsers.del(this);
    }

    public int compareTo(userLineHandler o) {
        if (hsh < o.hsh) {
            return -1;
        }
        if (hsh > o.hsh) {
            return +1;
        }
        return 0;
    }

}

class userLineExpirity extends TimerTask {

    public userLineHandler lower;

    public userLineExpirity(userLineHandler parent) {
        lower = parent;
    }

    public void run() {
        lower.doExpire();
    }

}
