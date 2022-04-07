package net.freertr.rtr;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrIPv4;
import net.freertr.auth.authLocal;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgInit;
import net.freertr.cfg.cfgTime;
import net.freertr.ip.ipCor4;
import net.freertr.ip.ipCor6;
import net.freertr.ip.ipFwd;
import net.freertr.ip.ipFwdRoute;
import net.freertr.ip.ipRtr;
import net.freertr.pipe.pipeDiscard;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabIndex;
import net.freertr.tab.tabRoute;
import net.freertr.tab.tabRouteAttr;
import net.freertr.tab.tabRouteEntry;
import net.freertr.user.userFlash;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.logger;
import net.freertr.util.uniResLoc;
import net.freertr.util.version;

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
    protected final tabRouteAttr.routeType rouTyp;

    /**
     * router number
     */
    protected final int rtrNum;

    /**
     * protocol version
     */
    protected final int proto;

    /**
     * respawn on termination
     */
    public boolean respawn = true;

    /**
     * time between runs
     */
    protected int interval;

    /**
     * initial delay
     */
    protected int initial;

    /**
     * random time between runs
     */
    public int randInt;

    /**
     * random initial delay
     */
    public int randIni;

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
                rouTyp = tabRouteAttr.routeType.download4;
                proto = 4;
                break;
            case ipCor6.protocolVersion:
                rouTyp = tabRouteAttr.routeType.download6;
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
        routerComputedI = new tabGen<tabIndex<addrIP>>();
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
        int del = initial;
        if (randIni > 0) {
            del += bits.random(1, randIni);
        }
        if (respawn) {
            keepTimer.schedule(task, del, interval);
        } else {
            keepTimer.schedule(task, del);
        }
    }

    /**
     * do one timer round
     */
    protected void doRound() {
        if (cfgInit.booting) {
            return;
        }
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
        if (randInt > 0) {
            bits.sleep(bits.random(1, randInt));
        }
        pipeLine pipe = new pipeLine(32768, false);
        pipeDiscard.discard(pipe.getSide());
        pipeSide pip = pipe.getSide();
        pip.setTime(120000);
        String tmp = version.getRWpath() + "rou" + bits.randomD() + ".tmp";
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
            ntry.best.rouTyp = rouTyp;
            ntry.best.protoNum = rtrNum;
            res.add(tabRoute.addType.better, ntry, false, false);
        }
        routerDoAggregates(rtrBgpUtil.sfiUnicast, res, res, fwdCore.commonLabel, null, 0);
        if (res.preserveTime(routerComputedU)) {
            return;
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
        l.add(null, "1  2      url                        specify url to download");
        l.add(null, "2  2,.      <str>                    url to download");
        l.add(null, "1  .      respawn                    restart on termination");
        l.add(null, "1  2      time                       specify time between runs");
        l.add(null, "2  .        <num>                    milliseconds between runs");
        l.add(null, "1  2      delay                      specify initial delay");
        l.add(null, "2  .        <num>                    milliseconds between start");
        l.add(null, "1  2      random-time                specify random time between runs");
        l.add(null, "2  .        <num>                    milliseconds between runs");
        l.add(null, "1  2      random-delay               specify random initial delay");
        l.add(null, "2  .        <num>                    milliseconds before start");
        l.add(null, "1  2      range                      specify time range");
        l.add(null, "2  .        <name:tm>                name of time map");
        l.add(null, "1  .      log                        log actions");
        l.add(null, "1  .      runnow                     run one round now");
        l.add(null, "1  .      hidden                     hide command");
    }

    /**
     * get config
     *
     * @param l list
     * @param beg beginning
     * @param filter filter
     */
    public void routerGetConfig(List<String> l, String beg, int filter) {
        cmds.cfgLine(l, !hidden, beg, "hidden", "");
        cmds.cfgLine(l, !logging, beg, "log", "");
        if (hidden) {
            l.add(beg + "url " + authLocal.passwdEncode(url, (filter & 2) != 0));
        } else {
            l.add(beg + "url " + url);
        }
        cmds.cfgLine(l, time == null, beg, "range", "" + time);
        cmds.cfgLine(l, !respawn, beg, "respawn", "");
        l.add(beg + "delay " + initial);
        l.add(beg + "time " + interval);
        l.add(beg + "random-time " + randInt);
        l.add(beg + "random-delay " + randIni);
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
        if (s.equals("random-time")) {
            randInt = bits.str2num(cmd.word());
            if (negated) {
                randInt = 0;
            }
            return false;
        }
        if (s.equals("random-delay")) {
            randIni = bits.str2num(cmd.word());
            if (negated) {
                randIni = 0;
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
        if (s.equals("respawn")) {
            respawn = !negated;
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

    /**
     * maximum recursion depth
     *
     * @return allowed number
     */
    public int routerRecursions() {
        return 1;
    }

    /**
     * get list of link states
     *
     * @param tab table to update
     * @param par parameter
     * @param asn asn
     * @param adv advertiser
     */
    public void routerLinkStates(tabRoute<addrIP> tab, int par, int asn, addrIPv4 adv) {
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
