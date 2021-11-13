package net.freertr.user;

import java.util.Comparator;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
import net.freertr.auth.authGeneric;
import net.freertr.auth.authResult;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgAuther;
import net.freertr.cfg.cfgIfc;
import net.freertr.cfg.cfgInit;
import net.freertr.pipe.pipeSetting;
import net.freertr.pipe.pipeSide;
import net.freertr.tab.tabGen;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.logger;

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
     * show timestamps
     */
    public boolean execTimes;

    /**
     * colorize
     */
    public boolean execColor;

    /**
     * space as tab
     */
    public boolean execSpace;

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
     * display banner
     */
    public boolean banner = true;

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
    protected static final tabGen<userLineHandler> loggedUsers = new tabGen<userLineHandler>();

    /**
     * previous user
     */
    protected static String prevUserGlb = "you are the first on this box";

    /**
     * previous user
     */
    protected String prevUserLoc = "you are the first on this line";

    /**
     * get running configuration
     *
     * @param beg beginning string
     * @param lst list to append
     */
    public void getShRun(String beg, List<String> lst) {
        if (execIface == null) {
            lst.add(beg + "no exec interface");
        } else {
            lst.add(beg + "exec interface " + execIface.name);
        }
        lst.add(beg + "exec timeout " + execTimeOut);
        lst.add(beg + "exec width " + execWidth);
        lst.add(beg + "exec height " + execHeight);
        lst.add(beg + "exec history " + execHistory);
        cmds.cfgLine(lst, !execTimes, beg, "exec timestamp", "");
        cmds.cfgLine(lst, !execColor, beg, "exec colorized", "");
        cmds.cfgLine(lst, !execSpace, beg, "exec spacetab", "");
        lst.add(beg + "exec tablemode " + userFormat.tabmod2str(execTables));
        lst.add(beg + "exec welcome " + promptWelcome);
        lst.add(beg + "exec ready " + promptSuccess);
        lst.add(beg + "exec before " + promptLast);
        lst.add(beg + "exec bye " + promptGoodbye);
        cmds.cfgLine(lst, !execLogging, beg, "exec logging", "");
        lst.add(beg + "exec autocommand " + autoCommand);
        cmds.cfgLine(lst, !autoHangup, beg, "exec autohangup", "");
        cmds.cfgLine(lst, !banner, beg, "exec banner", "");
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
                execIface = cfgAll.ifcFind(cmd.word(), false);
                if (execIface == null) {
                    return false;
                }
                if (execIface.type != cfgIfc.ifaceType.dialer) {
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
            if (s.equals("timestamp")) {
                execTimes = true;
                return false;
            }
            if (s.equals("colorized")) {
                execColor = true;
                return false;
            }
            if (s.equals("spacetab")) {
                execSpace = true;
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
        if (!s.equals("no")) {
            return true;
        }
        s = cmd.word();
        if (s.equals("exec")) {
            s = cmd.word();
            if (s.equals("timestamp")) {
                execTimes = false;
                return false;
            }
            if (s.equals("colorized")) {
                execColor = false;
                return false;
            }
            if (s.equals("spacetab")) {
                execSpace = false;
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
            return true;
        }
        return true;
    }

    /**
     * get help text
     *
     * @param l list of commands
     */
    public void getHelp(userHelping l) {
        l.add(null, "1 2  exec                           set executable parameters");
        l.add(null, "2 3    interface                    set interface to use for framing");
        l.add(null, "3 .      <name>                     name of interface");
        l.add(null, "2 .    logging                      enable logging");
        l.add(null, "2 .    timestamp                    enable timestamps");
        l.add(null, "2 .    colorized                    enable colorization");
        l.add(null, "2 .    spacetab                     enable space as tab");
        l.add(null, "2 3    tablemode                    set table mode");
        l.add(null, "3 .      csv                        select csv mode");
        l.add(null, "3 .      fancy                      select fancy mode");
        l.add(null, "3 .      html                       select html mode");
        l.add(null, "3 .      normal                     select normal mode");
        l.add(null, "3 .      raw                        select raw mode");
        l.add(null, "3 .      table                      select table mode");
        l.add(null, "2 3    timeout                      set timeout value");
        l.add(null, "3 .      <num>                      timeout in milliseconds");
        l.add(null, "2 3    width                        number of columns");
        l.add(null, "3 .      <num>                      width of terminal");
        l.add(null, "2 3    height                       set height of terminal");
        l.add(null, "3 .      <num>                      number of lines");
        l.add(null, "2 3    ready                        set ready message");
        l.add(null, "3 3,.    <text>                     text to display");
        l.add(null, "2 3    bye                          set goodbye message");
        l.add(null, "3 3,.    <text>                     text to display");
        l.add(null, "2 3    welcome                      set welcome message");
        l.add(null, "3 3,.    <text>                     text to display");
        l.add(null, "2 3    before                       set previous user message");
        l.add(null, "3 3,.    <text>                     text to display");
        l.add(null, "2 3    autocommand                  set automatic command");
        l.add(null, "3 3,.    <text>                     autocommand of user");
        l.add(null, "2 .    banner                       display banner");
        l.add(null, "2 .    expirity                     display expirity warnings");
        l.add(null, "2 .    monitor                      display logging information");
        l.add(null, "2 .    autohangup                   disconnect user after autocommand");
        l.add(null, "2 3    privilege                    set default privilege");
        l.add(null, "3 .      <num>                      privilege of terminal");
        l.add(null, "2 3    authorization                set authorization");
        l.add(null, "3 .      <name>                     name of authentication list");
        l.add(null, "1 2  login                          set login parameters");
        l.add(null, "2 3,.  last                         display last login line");
        l.add(null, "3 .      none                       nothing");
        l.add(null, "3 .      global                     globally");
        l.add(null, "3 .      local                      locally");
        l.add(null, "3 .      both                       everything");
        l.add(null, "2 .    logging                      enable logging");
        l.add(null, "2 3    authentication               set authentication");
        l.add(null, "3 .      <name>                     name of authentication list");
        l.add(null, "2 3    escape                       set escape character");
        l.add(null, "3 .      <num>                      ascii number");
        l.add(null, "2 3    activate                     set activation character");
        l.add(null, "3 .      <num>                      ascii number");
        l.add(null, "2 3    deactivate                   set deactivation character");
        l.add(null, "3 .      <num>                      ascii number");
        l.add(null, "2 3    timeout                      set timeout value");
        l.add(null, "3 .      <num>                      timeout in milliseconds");
        l.add(null, "2 3    retry                        set retry value");
        l.add(null, "3 .      <num>                      number of tries");
        l.add(null, "2 3    delay                        set delay value");
        l.add(null, "3 .      <num>                      timeout in milliseconds");
        l.add(null, "2 3    user                         set username prompt");
        l.add(null, "3 3,.    <text>                     text to display");
        l.add(null, "2 3    pass                         set password prompt");
        l.add(null, "3 3,.    <text>                     text to display");
        l.add(null, "2 3    fail                         set failed message");
        l.add(null, "3 3,.    <text>                     text to display");
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

class userLineHandler implements Runnable, Comparator<userLineHandler> {

    public final int hsh;

    public final pipeSide pipe;

    public final userLine parent;

    public final String remote;

    public final int physical;

    public final long since;

    public long last;

    public authResult user;

    public boolean preauthed;

    public Timer expTim;

    public userLineHandler(userLine prnt, pipeSide pip, String rem, int phys) {
        parent = prnt;
        pipe = pip;
        remote = rem;
        physical = phys;
        pipe.setTime(parent.execTimeOut);
        pipe.lineRx = pipeSide.modTyp.modeCRtryLF;
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        hsh = pip.hashCode();
        since = bits.getTime();
        new Thread(this).start();
    }

    public String toString() {
        return user.user + "|" + remote + "|" + bits.timePast(since);
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
        last = bits.getTime();
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
            if (cfgAll.passwdStars) {
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
        String s = user.user + " from " + remote + " at " + bits.time2str(cfgAll.timeZoneName, bits.getTime() + cfgAll.timeServerOffset, 3);
        userLine.prevUserGlb = s;
        parent.prevUserLoc = s;
        pipe.setTime(parent.execTimeOut);
        userReader rdr = new userReader(pipe, parent);
        pipe.settingsPut(pipeSetting.origin, remote);
        pipe.settingsPut(pipeSetting.authed, user);
        userExec exe = new userExec(pipe, rdr);
        userConfig cfg = new userConfig(pipe, rdr);
        exe.privileged = user.privilege >= 15;
        exe.framedIface = parent.execIface;
        exe.physicalLin = physical != 0;
        exe.authorization = parent.authorizeList;
        cfg.authorization = parent.authorizeList;
        exe.username = user.user;
        cfg.username = user.user;
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
            expTim = new Timer();
            userLineExpirity task = new userLineExpirity(this);
            expTim.schedule(task, 30 * 1000, 60 * 1000);
        }
        for (;;) {
            last = bits.getTime();
            userExec.cmdRes i = exe.doCommands();
            if (i == userExec.cmdRes.command) {
                continue;
            }
            if (i == userExec.cmdRes.logout) {
                if (physical == 2) {
                    pipe.linePut("% not possible on this line");
                    continue;
                }
                if (parent.loginLogging) {
                    logger.info(user.user + " logged out from " + remote);
                }
                return;
            }
            if (i != userExec.cmdRes.config) {
                continue;
            }
            if (cfgAll.configExclusive > 0) {
                cfgAll.configExclusive++;
            }
            logger.warn(user.user + " configuring from " + remote);
            cfg.resetMode();
            List<String> sesStart = null;
            if (exe.rollback) {
                logger.info("configuration checkpoint frozen!");
                sesStart = cfgAll.getShRun(1);
            }
            for (;;) {
                last = bits.getTime();
                if (cfg.doCommand()) {
                    break;
                }
            }
            if (pipe.isClosed() == 0) {
                sesStart = null;
            }
            if (sesStart != null) {
                sesStart = userFilter.getDiffs(cfgAll.getShRun(1), sesStart);
                rdr.putStrArr(bits.lst2lin(sesStart, false));
                int res = cfgInit.executeSWcommands(sesStart, false);
                rdr.putStrArr(bits.str2lst("errors=" + res));
                logger.info("configuration reverted to frozen checkpoint with " + res + " errors.");
            }
            if (cfgAll.configExclusive > 1) {
                cfgAll.configExclusive--;
            }
            if (cfgAll.configAsave) {
                exe.executeCommand("write memory");
            }
            logger.warn(user.user + " configured from " + remote);
        }
    }

    public void doExpire() {
        long tim = bits.getTime() - last;
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
            logger.traceback(e);
        }
        try {
            expTim.cancel();
        } catch (Exception e) {
        }
        pipe.setClose();
        userLine.loggedUsers.del(this);
    }

    public int compare(userLineHandler o1, userLineHandler o2) {
        if (o1.hsh < o2.hsh) {
            return -1;
        }
        if (o1.hsh > o2.hsh) {
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
