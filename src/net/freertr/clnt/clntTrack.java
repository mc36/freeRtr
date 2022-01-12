package net.freertr.clnt;

import java.util.ArrayList;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrPrefix;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgCheck;
import net.freertr.cfg.cfgIfc;
import net.freertr.cfg.cfgTrack;
import net.freertr.cfg.cfgVrf;
import net.freertr.ip.ipFwd;
import net.freertr.ip.ipFwdEcho;
import net.freertr.ip.ipFwdIface;
import net.freertr.ip.ipFwdTab;
import net.freertr.line.lineScript;
import net.freertr.pipe.pipeDiscard;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSetting;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGen;
import net.freertr.prt.prtUdp;
import net.freertr.rtr.rtrBfdClnt;
import net.freertr.rtr.rtrBfdNeigh;
import net.freertr.sec.secClient;
import net.freertr.tab.tabGen;
import net.freertr.user.userExec;
import net.freertr.user.userReader;
import net.freertr.user.userScript;
import net.freertr.user.userTerminal;
import net.freertr.util.bits;
import net.freertr.util.logger;
import net.freertr.util.notifier;
import net.freertr.util.state;

/**
 * tracker worker
 *
 * @author matecsaba
 */
public class clntTrack implements rtrBfdClnt {

    /**
     * create instance
     */
    public clntTrack() {
    }

    /**
     * operation mode
     */
    public enum operMod {

        /**
         * icmp echo request tester
         */
        icmp,
        /**
         * tcp connect tester
         */
        tcp,
        /**
         * bfd alive tester
         */
        bfd,
        /**
         * interface
         */
        iface,
        /**
         * route
         */
        route,
        /**
         * prefix
         */
        prefix,
        /**
         * script
         */
        script,
        /**
         * nrpe
         */
        nrpe,
        /**
         * other
         */
        other,
        /**
         * check
         */
        check,
        /**
         * udp echo tester
         */
        udp,
        /**
         * twamp tester
         */
        twamp,

    }

    /**
     * force mode
     */
    public enum forMode {

        /**
         * forced up
         */
        up,
        /**
         * forced down
         */
        down,
        /**
         * forced negated
         */
        neg,
        /**
         * forced normal
         */
        norm

    }

    /**
     * name of this tracker
     */
    public String name;

    /**
     * type of work
     */
    public operMod mode = operMod.icmp;

    /**
     * forced result
     */
    public forMode force = forMode.norm;

    /**
     * final script
     */
    public String script = null;

    /**
     * hide commands
     */
    public boolean hidden = false;

    /**
     * preferred ip protocol version
     */
    public int prefer = 0;

    /**
     * target of test
     */
    public String target = null;

    /**
     * exec up
     */
    public String execUp = null;

    /**
     * exec down
     */
    public String execDn = null;

    /**
     * notify vrf
     */
    public cfgVrf wakeVrf = null;

    /**
     * chat script
     */
    public lineScript chats = null;

    /**
     * security protocol to use
     */
    public int secProto = 0;

    /**
     * vrf of target
     */
    public cfgVrf vrf = null;

    /**
     * source interface
     */
    public cfgIfc srcIfc = null;

    /**
     * time between runs
     */
    public int interval;

    /**
     * random between runs
     */
    public int randInt;

    /**
     * random on startup
     */
    public int randIni;

    /**
     * timeout value
     */
    public int timeout;

    /**
     * type of service value
     */
    public int typOsrv;

    /**
     * flow label value
     */
    public int flowLab;

    /**
     * type of time to live
     */
    public int tim2liv = 255;

    /**
     * size of payload
     */
    public int size = 80;

    /**
     * delay for up
     */
    public int delayUp;

    /**
     * delay for down
     */
    public int delayDn;

    /**
     * action logging
     */
    public boolean logging = false;

    /**
     * status, false=stopped, true=running
     */
    public boolean working = false;

    /**
     * clients
     */
    public final tabGen<ipFwd> clients = new tabGen<ipFwd>();

    /**
     * final state
     */
    protected boolean finalState = false;

    /**
     * time when went to this state
     */
    protected long finalTime;

    /**
     * number of changes
     */
    protected int totalChng;

    /**
     * number of ups
     */
    protected int totalUp;

    /**
     * number of downs
     */
    protected int totalDn;

    private Timer keepTimer;

    /**
     * last state
     */
    protected boolean lastState = false;

    /**
     * results in this state
     */
    protected int lastCount;

    /**
     * time when went to this state
     */
    protected long lastTime;

    public String toString() {
        return "" + name;
    }

    /**
     * force to string
     *
     * @param f force
     * @return string
     */
    public static String force2string(forMode f) {
        switch (f) {
            case up:
                return "up";
            case down:
                return "down";
            case neg:
                return "negate";
            case norm:
                return "normal";
            default:
                return "unknown=" + f;
        }
    }

    /**
     * mode to string
     *
     * @param m mode
     * @return string
     */
    public static String mode2string(operMod m) {
        switch (m) {
            case icmp:
                return "icmp";
            case nrpe:
                return "nrpe";
            case other:
                return "other";
            case check:
                return "check";
            case tcp:
                return "tcp";
            case udp:
                return "udp";
            case twamp:
                return "twamp";
            case bfd:
                return "bfd";
            case iface:
                return "interface";
            case route:
                return "route";
            case prefix:
                return "prefix";
            case script:
                return "script";
            default:
                return "unknown=" + m;
        }
    }

    public void bfdPeerDown() {
        haveResult(false, true);
    }

    /**
     * check state
     *
     * @return true on up, false on error
     */
    public boolean getStatus() {
        switch (force) {
            case up:
                return true;
            case down:
                return false;
            default:
                break;
        }
        if (!working) {
            return false;
        }
        switch (force) {
            case neg:
                return !finalState;
            case norm:
                return finalState;
            default:
                return false;
        }
    }

    /**
     * get summary line
     *
     * @return string
     */
    public String getShSum() {
        return name + "|" + force2string(force) + "|" + mode2string(mode) + "|" + target + "|" + (getStatus() ? "up" : "down") + "|" + totalChng + "|" + bits.timePast(finalTime);
    }

    /**
     * get detailed status
     *
     * @return status strings
     */
    public List<String> getShStat() {
        List<String> l = new ArrayList<String>();
        l.add("name|" + name);
        l.add("type|" + force2string(force) + " " + mode2string(mode));
        l.add("target|" + target);
        l.add("reported|" + getStatus());
        l.add("since|" + bits.time2str(cfgAll.timeZoneName, finalTime + cfgAll.timeServerOffset, 3));
        l.add("for|" + bits.timePast(finalTime));
        l.add("changes|" + totalChng);
        l.add("measures|" + (totalUp + totalDn));
        l.add("ups|" + totalUp);
        l.add("downs|" + totalDn);
        l.add("current|" + lastState);
        l.add("count|" + lastCount);
        l.add("since|" + bits.time2str(cfgAll.timeZoneName, lastTime + cfgAll.timeServerOffset, 3));
        l.add("for|" + bits.timePast(lastTime));
        String a = "";
        for (int i = 0; i < clients.size(); i++) {
            ipFwd ntry = clients.get(i);
            a += " " + ntry;
        }
        l.add("clients|" + a);
        return l;
    }

    /**
     * stop running
     */
    public synchronized void stopNow() {
        try {
            keepTimer.cancel();
        } catch (Exception e) {
        }
        keepTimer = null;
        working = false;
        haveResult(false, true);
    }

    /**
     * start running
     */
    public synchronized void startNow() {
        if (working) {
            return;
        }
        if (interval < 1) {
            return;
        }
        working = true;
        keepTimer = new Timer();
        clntTrackTimer task = new clntTrackTimer(this);
        int del = 100;
        if (randIni > 0) {
            del += bits.random(1, randIni);
        }
        keepTimer.schedule(task, del, interval);
    }

    private static String doScript(String scrptTxt, boolean selfVal) {
        pipeLine pl = new pipeLine(32768, false);
        pipeSide pip = pl.getSide();
        pip.setTime(10000);
        pip.lineRx = pipeSide.modTyp.modeCRorLF;
        pip.lineTx = pipeSide.modTyp.modeCRLF;
        userScript t = new userScript(pip, "");
        t.allowExec = true;
        t.allowConfig = true;
        t.addLine("set selfVal " + (selfVal ? "1" : "0"));
        for (int i = 0; i < cfgAll.trackers.size(); i++) {
            cfgTrack trck = cfgAll.trackers.get(i);
            if (trck == null) {
                continue;
            }
            t.addLine("set " + trck.worker.name + " " + (trck.worker.getStatus() ? "1" : "0"));
        }
        t.addLine(scrptTxt);
        pip = pl.getSide();
        pip.lineRx = pipeSide.modTyp.modeCRorLF;
        pip.lineTx = pipeSide.modTyp.modeCR;
        t.cmdAll();
        pl.setClose();
        return pip.lineGet(1);
    }

    /**
     * do one timer round
     */
    public synchronized void doRound() {
        if (logging) {
            logger.info("starting action " + name);
        }
        if (randInt > 0) {
            bits.sleep(bits.random(1, randInt));
        }
        if (mode == null) {
            haveResult(false, false);
            return;
        }
        switch (mode) {
            case iface:
                if (target == null) {
                    haveResult(false, false);
                    return;
                }
                cfgIfc ifc = cfgAll.ifcFind(target, false);
                if (ifc == null) {
                    haveResult(false, false);
                    return;
                }
                haveResult(ifc.ethtyp.getState() == state.states.up, false);
                return;
            case route:
                if (target == null) {
                    haveResult(false, false);
                    return;
                }
                if (vrf == null) {
                    haveResult(false, false);
                    return;
                }
                addrIP adr = new addrIP();
                adr.fromString(target);
                ipFwd fwdCor = vrf.getFwd(adr);
                if (fwdCor == null) {
                    haveResult(false, false);
                    return;
                }
                haveResult(fwdCor.actualU.route(adr) != null, false);
                return;
            case prefix:
                if (target == null) {
                    haveResult(false, false);
                    return;
                }
                if (vrf == null) {
                    haveResult(false, false);
                    return;
                }
                addrPrefix<addrIP> prf = addrPrefix.str2ip(target);
                if (prf == null) {
                    haveResult(false, false);
                    return;
                }
                fwdCor = vrf.getFwd(prf.network);
                if (fwdCor == null) {
                    haveResult(false, false);
                    return;
                }
                haveResult(fwdCor.actualU.find(prf) != null, false);
                return;
            case script:
                if (target == null) {
                    haveResult(false, false);
                    return;
                }
                String a = doScript(target, true);
                if (logging) {
                    logger.info("got " + a + " from script");
                }
                if (a == null) {
                    haveResult(false, false);
                    return;
                }
                haveResult(bits.str2num(a) > 0, false);
                return;
            case other:
                if (target == null) {
                    haveResult(false, false);
                    return;
                }
                cfgTrack other = cfgAll.trackFind(target, false);
                if (other == null) {
                    haveResult(false, false);
                    return;
                }
                haveResult(other.worker.getStatus(), false);
                return;
            case check:
                if (target == null) {
                    haveResult(false, false);
                    return;
                }
                cfgCheck check = cfgAll.checkFind(target, false);
                if (check == null) {
                    haveResult(false, false);
                    return;
                }
                haveResult(check.doCheckBinary(), false);
                return;
            case nrpe:
                if (target == null) {
                    haveResult(false, false);
                    return;
                }
                int i = target.indexOf("/");
                if (i < 0) {
                    haveResult(false, false);
                    return;
                }
                clntNrpe nrpe = new clntNrpe(null);
                nrpe.server = target.substring(0, i);
                nrpe.check = target.substring(i + 1, target.length());
                haveResult(!nrpe.doCheck(), false);
                if (logging) {
                    a = "";
                    for (i = 0; i < nrpe.text.size(); i++) {
                        a += " " + nrpe.text.get(i);
                    }
                    logger.info("got " + a + " from remote");
                }
                return;
            default:
                break;
        }
        if (timeout < 1) {
            haveResult(false, false);
            return;
        }
        if (size < 1) {
            haveResult(false, false);
            return;
        }
        if (target == null) {
            haveResult(false, false);
            return;
        }
        if (vrf == null) {
            haveResult(false, false);
            return;
        }
        addrIP fwdTrg = userTerminal.justResolv(target, prefer);
        if (fwdTrg == null) {
            haveResult(false, false);
            return;
        }
        ipFwd fwdCor = vrf.getFwd(fwdTrg);
        prtUdp udpCor = vrf.getUdp(fwdTrg);
        ipFwdIface fwdIfc = null;
        if (srcIfc != null) {
            fwdIfc = srcIfc.getFwdIfc(fwdTrg);
        }
        if (fwdIfc == null) {
            fwdIfc = ipFwdTab.findSendingIface(fwdCor, fwdTrg);
        }
        if (fwdIfc == null) {
            haveResult(false, false);
            return;
        }
        switch (mode) {
            case icmp:
                ipFwdEcho ping = fwdCor.echoSendReq(fwdIfc.addr, fwdTrg, size, tim2liv, typOsrv, flowLab, 0, false);
                if (ping == null) {
                    haveResult(false, false);
                    break;
                }
                ping.notif.sleep(timeout);
                if (ping.notif.totalNotifies() < 1) {
                    haveResult(false, false);
                    break;
                }
                if (ping.res.size() < 1) {
                    haveResult(false, false);
                    break;
                }
                haveResult(ping.res.get(0).err == null, false);
                break;
            case tcp:
                prtGen tcp = vrf.getTcp(fwdTrg);
                if (tcp == null) {
                    haveResult(false, false);
                    break;
                }
                pipeSide pipe = tcp.streamConnect(new pipeLine(65536, false), fwdIfc, 0, fwdTrg, size, "track", null, -1);
                if (pipe == null) {
                    haveResult(false, false);
                    break;
                }
                if (pipe.wait4ready(timeout)) {
                    haveResult(false, false);
                    pipe.setClose();
                    break;
                }
                pipe.setTime(timeout);
                pipe = secClient.openSec(pipe, secProto, null, null);
                if (pipe == null) {
                    haveResult(false, false);
                    break;
                }
                pipe.setTime(timeout);
                if (chats == null) {
                    haveResult(true, false);
                } else {
                    haveResult(!chats.doScript(pipe), false);
                }
                pipe.setClose();
                break;
            case udp:
                clntEcho ech = new clntEcho();
                ech.notif = new notifier();
                ech.udp = udpCor;
                ech.src = fwdIfc;
                ech.trg = fwdTrg;
                ech.doWork();
                ech.notif.sleep(timeout);
                if (ech.notif.totalNotifies() < 1) {
                    haveResult(false, false);
                    break;
                }
                haveResult(true, false);
                break;
            case twamp:
                clntTwamp twm = new clntTwamp();
                twm.notif = new notifier();
                twm.udp = udpCor;
                twm.src = fwdIfc;
                twm.trg = fwdTrg;
                twm.doWork();
                twm.notif.sleep(timeout);
                if (twm.notif.totalNotifies() < 1) {
                    haveResult(false, false);
                    break;
                }
                haveResult(true, false);
                break;
            case bfd:
                fwdIfc.bfdAdd(fwdTrg, this, "tracker");
                rtrBfdNeigh bfd = fwdIfc.bfdFind(fwdTrg);
                if (bfd == null) {
                    haveResult(false, false);
                    break;
                }
                haveResult(bfd.getState(), false);
                break;
            default:
                break;
        }
        if (logging) {
            logger.info("stopped action " + name);
        }
    }

    /**
     * have result
     *
     * @param succ successful
     * @param imm need immediate action
     */
    public synchronized void haveResult(boolean succ, boolean imm) {
        if (logging) {
            logger.info("result=" + succ + " immediate=" + imm);
        }
        if (succ) {
            totalUp++;
        } else {
            totalDn++;
        }
        if (succ != lastState) {
            lastState = succ;
            lastCount = 0;
            lastTime = bits.getTime();
        } else {
            lastCount++;
        }
        if (!imm) {
            if (succ) {
                if (lastCount < delayUp) {
                    return;
                }
            } else if (lastCount < delayDn) {
                return;
            }
        }
        if (script != null) {
            String a = doScript(script, succ);
            if (logging) {
                logger.info("got " + a + " from script");
            }
            if (a == null) {
                succ = false;
            } else {
                succ = bits.str2num(a) > 0;
            }
        }
        if (finalState == succ) {
            return;
        }
        finalState = succ;
        finalTime = bits.getTime();
        totalChng++;
        for (int i = 0; i < clients.size(); i++) {
            ipFwd ntry = clients.get(i);
            if (ntry == null) {
                continue;
            }
            ntry.routerStaticChg();
        }
        if (wakeVrf != null) {
            wakeVrf.fwd4.routerStaticChg();
            wakeVrf.fwd6.routerStaticChg();
        }
        String cmd = null;
        if (succ) {
            logger.warn("tracker " + name + " up");
            cmd = execUp;
        } else {
            logger.error("tracker " + name + " down");
            cmd = execDn;
        }
        if (cmd == null) {
            return;
        }
        pipeLine pipe = new pipeLine(32768, false);
        pipeDiscard.discard(pipe.getSide());
        pipeSide pip = pipe.getSide();
        userReader rdr = new userReader(pip, null);
        pip.settingsPut(pipeSetting.height, 0);
        userExec exe = new userExec(pip, rdr);
        exe.privileged = true;
        pip.setTime(120000);
        String s = exe.repairCommand(cmd);
        exe.executeCommand(s);
        pipe.setClose();
        if (logging) {
            logger.info("command finished");
        }
    }

}

class clntTrackTimer extends TimerTask {

    private clntTrack lower;

    public clntTrackTimer(clntTrack parent) {
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
