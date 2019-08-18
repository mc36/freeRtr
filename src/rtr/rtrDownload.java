package rtr;

import addr.addrIP;
import auth.authLocal;
import cfg.cfgAll;
import cfg.cfgTime;
import ip.ipCor4;
import ip.ipCor6;
import ip.ipFwd;
import ip.ipFwdRoute;
import ip.ipRtr;
import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
import pipe.pipeDiscard;
import pipe.pipeLine;
import pipe.pipeSide;
import tab.tabRoute;
import tab.tabRouteEntry;
import user.userFlash;
import user.userHelping;
import util.bits;
import util.cmds;
import util.logger;
import util.uniResLoc;

/**
 * downloader
 *
 * @author matecsaba
 */
public class rtrDownload extends ipRtr {

    /**
     * the forwarder protocol
     */
    public final ipFwd fwdCore;

    /**
     * route type
     */
    protected final tabRouteEntry.routeType rouTyp;

    /**
     * router number
     */
    protected final int rtrNum;

    /**
     * protocol version
     */
    protected final int proto;

    /**
     * time between runs
     */
    protected int interval;

    /**
     * initial delay
     */
    protected int initial;

    /**
     * url
     */
    protected String url;

    /**
     * hide commands
     */
    protected boolean hidden;

    /**
     * action logging
     */
    protected boolean logging;

    /**
     * status, false=stopped, true=running
     */
    protected boolean working;

    /**
     * time range when allowed
     */
    protected cfgTime time;

    private Timer keepTimer;

    private List<String> dled;

    /**
     * create download process
     *
     * @param forwarder forwarder to update
     * @param id process id
     */
    public rtrDownload(ipFwd forwarder, int id) {
        fwdCore = forwarder;
        rtrNum = id;
        switch (fwdCore.ipVersion) {
            case ipCor4.protocolVersion:
                rouTyp = tabRouteEntry.routeType.download4;
                proto = 4;
                break;
            case ipCor6.protocolVersion:
                rouTyp = tabRouteEntry.routeType.download6;
                proto = 6;
                break;
            default:
                rouTyp = null;
                proto = 0;
                break;
        }
        dled = new ArrayList<String>();
        url = "";
        routerComputedU = new tabRoute<addrIP>("rx");
        routerComputedM = new tabRoute<addrIP>("rx");
        routerComputedF = new tabRoute<addrIP>("rx");
        routerCreateComputed();
        fwdCore.routerAdd(this, rouTyp, id);
    }

    /**
     * convert to string
     *
     * @return string
     */
    public String toString() {
        return "download on " + fwdCore;
    }

    private synchronized void stopNow() {
        try {
            keepTimer.cancel();
        } catch (Exception e) {
        }
        keepTimer = null;
        working = false;
    }

    private synchronized void startNow() {
        if (working) {
            return;
        }
        if (interval < 1) {
            return;
        }
        working = true;
        keepTimer = new Timer();
        rtrDownloadTimer task = new rtrDownloadTimer(this);
        keepTimer.schedule(task, initial, interval);
    }

    /**
     * do one timer round
     */
    protected void doRound() {
        if (url == null) {
            return;
        }
        if (url.length() < 1) {
            return;
        }
        if (time != null) {
            if (time.matches(bits.getTime() + cfgAll.timeServerOffset)) {
                return;
            }
        }
        if (logging) {
            logger.info("starting download " + url);
        }
        pipeLine pipe = new pipeLine(32768, false);
        pipeDiscard.discard(pipe.getSide());
        pipeSide pip = pipe.getSide();
        pip.timeout = 120000;
        String tmp = "dnld" + this.hashCode() + ".tmp";
        userFlash.delete(tmp);
        if (userFlash.doReceive(pip, uniResLoc.parseOne(url), new File(tmp))) {
            logger.warn("error downloading " + url);
            return;
        }
        List<String> lst = bits.txt2buf(tmp);
        userFlash.delete(tmp);
        if (lst == null) {
            logger.warn("error reading " + url);
            return;
        }
        dled = lst;
        routerCreateComputed();
        if (logging) {
            logger.info("stopped download");
        }
    }

    /**
     * create computed
     */
    public synchronized void routerCreateComputed() {
        tabRoute<addrIP> res = new tabRoute<addrIP>("computed");
        for (int i = 0; i < dled.size(); i++) {
            String s = dled.get(i);
            if (s == null) {
                continue;
            }
            cmds cmd = new cmds("dl", s);
            ipFwdRoute red = new ipFwdRoute();
            if (red.fromString(proto, cmd)) {
                continue;
            }
            tabRouteEntry<addrIP> ntry = red.getPrefix();
            if (ntry == null) {
                continue;
            }
            ntry.rouTyp = rouTyp;
            ntry.protoNum = rtrNum;
            res.add(tabRoute.addType.better, ntry, false, false);
        }
        routerComputedU = res;
        fwdCore.routerChg(this);
    }

    /**
     * redistribution changed
     */
    public void routerRedistChanged() {
        routerCreateComputed();
    }

    /**
     * others changed
     */
    public void routerOthersChanged() {
    }

    /**
     * get help
     *
     * @param l list
     */
    public void routerGetHelp(userHelping l) {
        l.add("1  2      url                        specify url to download");
        l.add("2  2,.      <cmd>                    exec command to run");
        l.add("1  2      time                       specify time between runs");
        l.add("2  .        <num>                    milliseconds between runs");
        l.add("1  2      delay                      specify initial delay");
        l.add("2  .        <num>                    milliseconds between start");
        l.add("1  2      range                      specify time range");
        l.add("2  .        <name>                   name of time map");
        l.add("1  .      log                        log actions");
        l.add("1  .      runnow                     run one round now");
        l.add("1  .      hidden                     hide command");
    }

    /**
     * get config
     *
     * @param l list
     * @param beg beginning
     * @param filter filter
     */
    public void routerGetConfig(List<String> l, String beg, boolean filter) {
        cmds.cfgLine(l, !hidden, cmds.tabulator, "hidden", "");
        cmds.cfgLine(l, !logging, cmds.tabulator, "log", "");
        if (hidden) {
            l.add(beg + "url " + authLocal.passwdEncode(url));
        } else {
            l.add(beg + "url " + url);
        }
        cmds.cfgLine(l, time == null, cmds.tabulator, "range", "" + time);
        l.add(cmds.tabulator + "delay " + initial);
        l.add(cmds.tabulator + "time " + interval);
    }

    /**
     * configure
     *
     * @param cmd command
     * @return false if success, true if error
     */
    public boolean routerConfigure(cmds cmd) {
        String s = cmd.word();
        boolean negated = false;
        if (s.equals("no")) {
            s = cmd.word();
            negated = true;
        }
        if (s.equals("url")) {
            url = authLocal.passwdDecode(cmd.getRemaining());
            if (negated) {
                url = "";
            }
            return false;
        }
        if (s.equals("range")) {
            time = cfgAll.timeFind(cmd.word(), false);
            if (negated) {
                time = null;
            }
            return false;
        }
        if (s.equals("delay")) {
            initial = bits.str2num(cmd.word());
            if (negated) {
                initial = 0;
            }
            return false;
        }
        if (s.equals("time")) {
            stopNow();
            interval = bits.str2num(cmd.word());
            if (negated) {
                interval = 0;
            }
            startNow();
            return false;
        }
        if (s.equals("log")) {
            logging = !negated;
            return false;
        }
        if (s.equals("hidden")) {
            hidden = !negated;
            return false;
        }
        if (s.equals("runnow")) {
            doRound();
            return false;
        }
        return true;
    }

    /**
     * stop work
     */
    public void routerCloseNow() {
        stopNow();
    }

    /**
     * get neighbor count
     *
     * @return count
     */
    public int routerNeighCount() {
        return 0;
    }

    /**
     * neighbor list
     *
     * @param tab list
     */
    public void routerNeighList(tabRoute<addrIP> tab) {
    }

    /**
     * get interface count
     *
     * @return count
     */
    public int routerIfaceCount() {
        return 0;
    }

}

class rtrDownloadTimer extends TimerTask {

    private rtrDownload lower;

    public rtrDownloadTimer(rtrDownload parent) {
        lower = parent;
    }

    public void run() {
        try {
            lower.doRound();
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}
