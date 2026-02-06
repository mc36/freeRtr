package org.freertr.rtr;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.auth.authLocal;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgInit;
import org.freertr.cfg.cfgTime;
import org.freertr.ip.ipCor4;
import org.freertr.ip.ipCor6;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdRoute;
import org.freertr.ip.ipRtr;
import org.freertr.pipe.pipeDiscard;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.enc.encUrl;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabIndex;
import org.freertr.tab.tabRoute;
import org.freertr.tab.tabRouteAttr;
import org.freertr.tab.tabRouteEntry;
import org.freertr.user.userFlash;
import org.freertr.user.userHelp;
import org.freertr.user.userUpgrade;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.logger;

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

    /**
     * keepalive
     */
    protected rtrDownloadTimer keepTimer;

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
        keepTimer = new rtrDownloadTimer(this);
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
        String tmp = cfgInit.getRWpath() + "rou" + bits.randomD() + userUpgrade.tmpExt;
        userFlash.delete(tmp);
        if (userFlash.doReceive(pip, encUrl.parseOne(url), new File(tmp))) {
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
        fwdCore.routerChg(this, false);
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
    public void routerGetHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "url", "specify url to download");
        l.add(null, false, 2, new int[]{2, -1}, "<str>", "url to download");
        l.add(null, false, 1, new int[]{-1}, "respawn", "restart on termination");
        l.add(null, false, 1, new int[]{2}, "time", "specify time between runs");
        l.add(null, false, 2, new int[]{-1}, "<num>", "milliseconds between runs");
        l.add(null, false, 1, new int[]{2}, "delay", "specify initial delay");
        l.add(null, false, 2, new int[]{-1}, "<num>", "milliseconds between start");
        l.add(null, false, 1, new int[]{2}, "random-time", "specify random time between runs");
        l.add(null, false, 2, new int[]{-1}, "<num>", "milliseconds between runs");
        l.add(null, false, 1, new int[]{2}, "random-delay", "specify random initial delay");
        l.add(null, false, 2, new int[]{-1}, "<num>", "milliseconds before start");
        l.add(null, false, 1, new int[]{2}, "range", "specify time range");
        l.add(null, false, 2, new int[]{-1}, "<name:tm>", "name of time map");
        l.add(null, false, 1, new int[]{-1}, "log", "log actions");
        l.add(null, false, 1, new int[]{-1}, "runnow", "run one round now");
        l.add(null, false, 1, new int[]{-1}, "hidden", "hide command");
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
        if (s.equals(cmds.negated)) {
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
            stopNow();
            respawn = !negated;
            startNow();
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
        fwdCore.routerDel(this);
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

    /**
     * get state information
     *
     * @param lst list to append
     */
    public void routerStateGet(List<String> lst) {
    }

    /**
     * set state information
     *
     * @param cmd string to append
     * @return true on error, false on success
     */
    public boolean routerStateSet(cmds cmd) {
        return true;
    }

}

class rtrDownloadTimer implements Runnable {

    private final rtrDownload lower;

    public rtrDownloadTimer(rtrDownload parent) {
        lower = parent;
        new Thread(this).start();
    }

    public void run() {
        try {
            int i = lower.initial;
            if (lower.randIni > 0) {
                i += bits.random(1, lower.randIni);
            }
            if (i > 0) {
                bits.sleep(i);
            }
            lower.doRound();
            if (!lower.respawn) {
                return;
            }
            for (;;) {
                bits.sleep(lower.interval);
                if (lower.keepTimer != this) {
                    break;
                }
                if (!lower.working) {
                    break;
                }
                lower.doRound();
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}
