package prt;

import ifc.ifcThread;
import java.io.File;
import java.util.ArrayList;
import java.util.List;
import pipe.pipeDiscard;
import pipe.pipeLine;
import pipe.pipeShell;
import user.userFormat;
import user.userFlash;
import util.bits;
import util.cmds;
import util.debugger;
import util.logger;

/**
 * watchdog implementation for microcontroller of stanci
 *
 * @author matecsaba
 */
public class prtWatch implements Runnable {

    private String name;

    private ifcThread thrd;

    private final int ports;

    private String names[];

    private pipeShell procs[];

    private static List<prtWatch> dogList = new ArrayList<prtWatch>();

    private int lastTry = -1;

    private int portStates = 0;

    public String toString() {
        return "watchdog " + name;
    }

    /**
     * start watchdog
     *
     * @param name name of interface
     * @param thrd interface thread handler
     * @param par parameters
     */
    public static void ifcAdd(String name, ifcThread thrd, String par) {
        dogList.add(new prtWatch(name, thrd, par));
    }

    /**
     * generate show output
     *
     * @return output
     */
    public static userFormat doShow() {
        userFormat l = new userFormat("|", "prt|nam|run");
        for (int i = 0; i < dogList.size(); i++) {
            dogList.get(i).doShow(l);
        }
        return l;
    }

    /**
     * terminate the watchdogs
     */
    public static void doShut() {
        for (int o = 0; o < dogList.size(); o++) {
            prtWatch l = dogList.get(o);
            for (int i = 0; i < l.ports; i++) {
                l.doShut(i);
            }
            l.sendMagic(0x66);
        }
    }

    /**
     * do clear command
     *
     * @param cmd command
     */
    public static void doClear(cmds cmd) {
        int i = bits.str2num(cmd.word());
        if (i < 0) {
            return;
        }
        if (i >= dogList.size()) {
            return;
        }
        prtWatch dog = dogList.get(i);
        dog.doClear(bits.str2num(cmd.word()));
    }

    /**
     * start watchdog
     *
     * @param nam name of interface
     * @param thr interface thread handler
     * @param par parameters
     */
    public prtWatch(String nam, ifcThread thr, String par) {
        name = nam;
        thrd = thr;
        new Thread(this).start();
        ports = bits.str2num(par);
        names = new String[ports];
        procs = new pipeShell[ports];
    }

    private void doShow(userFormat l) {
        for (int i = 0; i < ports; i++) {
            l.add(i + "|" + names[i] + "|" + isRunning(i));
        }
    }

    private void doClear(int prt) {
        sendPortStat(prt, false);
        lastTry = prt;
    }

    private void doShut(int prt) {
        sendPortStat(prt, false);
        userFlash.delete(names[prt]);
        names[prt] = null;
        procs[prt] = null;
    }

    private void sendMagic(int val) {
        byte[] buf = new byte[1];
        buf[0] = (byte) val;
        try {
            thrd.txOnePack(buf, 0, 1);
            if (debugger.prtWatchEvnt) {
                logger.debug("command (" + bits.byteDump(buf, 0, -1) + ") sent");
            }
            bits.sleep(1000);
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

    private void sendDogUp() {
        if (debugger.prtWatchEvnt) {
            logger.debug("dog feed time");
        }
        sendMagic(0x72);
    }

    private void sendPortStat(int port, boolean stat) {
        if (debugger.prtWatchEvnt) {
            logger.debug("turn port=" + port + " stat=" + stat);
        }
        if (stat) {
            portStates |= 1 << port;
        } else {
            portStates &= 0xfff - (1 << port);
        }
        sendMagic(0x40 + portStates);
    }

    private pipeShell doExec(String cmd, boolean wait) {
        if (debugger.prtWatchEvnt) {
            logger.debug("shell (" + cmd + ") started");
        }
        pipeLine pl = new pipeLine(65536, false);
        pipeDiscard.discard(pl.getSide());
        pipeShell sh = pipeShell.exec(pl.getSide(), cmd, null, true, true);
        if (sh == null) {
            return null;
        }
        if (!wait) {
            if (debugger.prtWatchEvnt) {
                logger.debug("shell (" + cmd + ") is running");
            }
            return sh;
        }
        sh.waitFor();
        if (debugger.prtWatchEvnt) {
            logger.debug("shell (" + cmd + ") done");
        }
        return null;
    }

    private void doPort(int prt) {
        if (debugger.prtWatchEvnt) {
            logger.debug("restarting port " + prt);
        }
        userFlash.delete(names[prt]);
        procs[prt] = null;
        names[prt] = null;
        File[] f1 = userFlash.dirList("/dev/");
        sendPortStat(prt, false);
        bits.sleep(5000);
        sendDogUp();
        sendPortStat(prt, true);
        bits.sleep(15000);
        sendDogUp();
        doCheck(prt);
        doExec("/rtr/modeswitch.sh", true);
        bits.sleep(15000);
        doCheck(prt);
        doExec("/bin/mdev -s", true);
        File[] f2 = userFlash.dirList("/dev/");
        String s = null;
        for (int i = 0; i < f2.length; i++) {
            String a = f2[i].getName();
            if (!a.startsWith("tty")) {
                continue;
            }
            int p = -1;
            for (int o = 0; o < f1.length; o++) {
                if (a.equals(f1[o].getName())) {
                    p = o;
                }
            }
            if (p >= 0) {
                continue;
            }
            s = f2[i].getAbsolutePath();
            break;
        }
        if (debugger.prtWatchEvnt) {
            logger.debug("found device " + s);
        }
        if (s == null) {
            return;
        }
        names[prt] = s;
        int p = (prt * 2) + 20051;
        procs[prt] = doExec("/rtr/modem.sh " + s + " /rtr/modem.ser" + prt
                + " " + (p + 1) + " " + p, false);
    }

    private void doCheck(int prt) {
        for (int i = 0; i < names.length; i++) {
            if (i == prt) {
                continue;
            }
            if (isRunning(i)) {
                continue;
            }
            doShut(i);
        }
    }

    /**
     * check if port running
     *
     * @param prt port number
     * @return running or not
     */
    public boolean isRunning(int prt) {
        if (procs[prt] == null) {
            return false;
        }
        return procs[prt].isRunning();
    }

    public void run() {
        logger.info("starting watchdog process");
        sendDogUp();
        for (int i = 0; i < names.length; i++) {
            doShut(i);
        }
        for (;;) {
            bits.sleep(15000);
            sendDogUp();
            doCheck(-1);
            lastTry = (lastTry + 1) % names.length;
            if (procs[lastTry] != null) {
                if (procs[lastTry].isRunning()) {
                    continue;
                }
            }
            try {
                doPort(lastTry);
            } catch (Exception e) {
                logger.traceback(e);
            }
            if (isRunning(lastTry) == false) {
                doShut(lastTry);
            }
            sendDogUp();
        }
    }

}
