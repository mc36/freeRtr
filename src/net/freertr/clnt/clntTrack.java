package net.freertr.clnt;

import java.util.ArrayList;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrPrefix;
import net.freertr.auth.authLocal;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgCheck;
import net.freertr.cfg.cfgIfc;
import net.freertr.cfg.cfgInit;
import net.freertr.cfg.cfgTime;
import net.freertr.cfg.cfgTrack;
import net.freertr.cfg.cfgVrf;
import net.freertr.cry.cryBase64;
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
import net.freertr.serv.servGeneric;
import net.freertr.tab.tabGen;
import net.freertr.user.userExec;
import net.freertr.user.userReader;
import net.freertr.user.userScript;
import net.freertr.user.userTerminal;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.logger;
import net.freertr.util.notifier;
import net.freertr.util.state;

/**
 * tracker worker
 *
 * @author matecsaba
 */
public class clntTrack implements Runnable, rtrBfdClnt {

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
     * client pubkey
     */
    public byte[] pubkey;

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
     * time range when allowed
     */
    public cfgTime time;

    /**
     * timeout value
     */
    public int timeout;

    /**
     * type of service value
     */
    public int typOsrv;

    /**
     * security group value
     */
    public int secGrp;

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
     * delay for start
     */
    public int delaySt;

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
    protected long finalTime = 0;

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

    private boolean working = false;

    private final notifier notif = new notifier();

    public String toString() {
        return "" + name;
    }

    /**
     * get config
     *
     * @param l list to append
     * @param filter filter
     */
    public void getConfig(List<String> l, int filter) {
        cmds.cfgLine(l, !hidden, cmds.tabulator, "hidden", "");
        cmds.cfgLine(l, !logging, cmds.tabulator, "log", "");
        l.add(cmds.tabulator + "mode " + clntTrack.mode2string(mode));
        l.add(cmds.tabulator + "force " + clntTrack.force2string(force));
        cmds.cfgLine(l, script == null, cmds.tabulator, "script", script);
        cmds.cfgLine(l, target == null, cmds.tabulator, "target", target);
        if (hidden) {
            cmds.cfgLine(l, execUp == null, cmds.tabulator, "exec-up", authLocal.passwdEncode(execUp, (filter & 2) != 0));
            cmds.cfgLine(l, execDn == null, cmds.tabulator, "exec-down", authLocal.passwdEncode(execDn, (filter & 2) != 0));
        } else {
            cmds.cfgLine(l, execUp == null, cmds.tabulator, "exec-up", execUp);
            cmds.cfgLine(l, execDn == null, cmds.tabulator, "exec-down", execDn);
        }
        if (wakeVrf != null) {
            l.add(cmds.tabulator + "wake-vrf " + wakeVrf.name);
        } else {
            l.add(cmds.tabulator + "no wake-vrf");
        }
        if (pubkey == null) {
            l.add(cmds.tabulator + "no pubkey");
        } else {
            l.add(cmds.tabulator + "pubkey " + cryBase64.encodeBytes(pubkey));
        }
        cmds.cfgLine(l, secProto == 0, cmds.tabulator, "security", servGeneric.proto2string(secProto));
        if (chats != null) {
            l.add(cmds.tabulator + "chat-script " + chats.scrName);
        } else {
            l.add(cmds.tabulator + "no chat-script");
        }
        if (vrf != null) {
            l.add(cmds.tabulator + "vrf " + vrf.name);
        } else {
            l.add(cmds.tabulator + "no vrf");
        }
        if (srcIfc != null) {
            l.add(cmds.tabulator + "source " + srcIfc.name);
        } else {
            l.add(cmds.tabulator + "no source");
        }
        l.add(cmds.tabulator + "random-interval " + randInt);
        l.add(cmds.tabulator + "random-initial " + randIni);
        l.add(cmds.tabulator + "interval " + interval);
        l.add(cmds.tabulator + "timeout " + timeout);
        l.add(cmds.tabulator + "sgt " + secGrp);
        l.add(cmds.tabulator + "tos " + typOsrv);
        l.add(cmds.tabulator + "flow " + flowLab);
        l.add(cmds.tabulator + "ttl " + tim2liv);
        l.add(cmds.tabulator + "size " + size);
        l.add(cmds.tabulator + "delay-start " + delaySt);
        l.add(cmds.tabulator + "delay-up " + delayUp);
        l.add(cmds.tabulator + "delay-down " + delayDn);
        cmds.cfgLine(l, time == null, cmds.tabulator, "range", "" + time);
        if (working) {
            l.add(cmds.tabulator + "start");
        } else {
            l.add(cmds.tabulator + "stop");
        }
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
        if (m == null) {
            return "null";
        }
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
        haveResult(false);
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
    public void stopNow() {
        working = false;
        notif.wakeup();
        haveResult(false);
    }

    /**
     * start running
     */
    public void startNow() {
        if (working) {
            return;
        }
        if (interval < 1) {
            return;
        }
        working = true;
        new Thread(this).start();
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
        if (time != null) {
            if (time.matches(bits.getTime() + cfgAll.timeServerOffset)) {
                haveResult(false);
                return;
            }
        }
        if (logging) {
            logger.info("starting action " + name);
        }
        if (mode == null) {
            haveResult(false);
            return;
        }
        switch (mode) {
            case iface:
                if (target == null) {
                    haveResult(false);
                    return;
                }
                cfgIfc ifc = cfgAll.ifcFind(target, 0);
                if (ifc == null) {
                    haveResult(false);
                    return;
                }
                haveResult(ifc.ethtyp.getState() == state.states.up);
                return;
            case route:
                if (target == null) {
                    haveResult(false);
                    return;
                }
                if (vrf == null) {
                    haveResult(false);
                    return;
                }
                addrIP adr = new addrIP();
                adr.fromString(target);
                ipFwd fwdCor = vrf.getFwd(adr);
                if (fwdCor == null) {
                    haveResult(false);
                    return;
                }
                haveResult(fwdCor.actualU.route(adr) != null);
                return;
            case prefix:
                if (target == null) {
                    haveResult(false);
                    return;
                }
                if (vrf == null) {
                    haveResult(false);
                    return;
                }
                addrPrefix<addrIP> prf = addrPrefix.str2ip(target);
                if (prf == null) {
                    haveResult(false);
                    return;
                }
                fwdCor = vrf.getFwd(prf.network);
                if (fwdCor == null) {
                    haveResult(false);
                    return;
                }
                haveResult(fwdCor.actualU.find(prf) != null);
                return;
            case script:
                if (target == null) {
                    haveResult(false);
                    return;
                }
                String a = doScript(target, true);
                if (logging) {
                    logger.info("got " + a + " from script");
                }
                if (a == null) {
                    haveResult(false);
                    return;
                }
                haveResult(bits.str2num(a) > 0);
                return;
            case other:
                if (target == null) {
                    haveResult(false);
                    return;
                }
                cfgTrack other = cfgAll.trackFind(target, false);
                if (other == null) {
                    haveResult(false);
                    return;
                }
                haveResult(other.worker.getStatus());
                return;
            case check:
                if (target == null) {
                    haveResult(false);
                    return;
                }
                cfgCheck check = cfgAll.checkFind(target, false);
                if (check == null) {
                    haveResult(false);
                    return;
                }
                haveResult(check.doCheckBinary());
                return;
            case nrpe:
                if (target == null) {
                    haveResult(false);
                    return;
                }
                if (vrf == null) {
                    haveResult(false);
                    return;
                }
                int i = target.indexOf("/");
                if (i < 0) {
                    haveResult(false);
                    return;
                }
                clntNrpe nrpe = new clntNrpe(null, vrf, srcIfc, target.substring(0, i));
                nrpe.timeout = timeout;
                haveResult(!nrpe.doCheck(target.substring(i + 1, target.length())));
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
            haveResult(false);
            return;
        }
        if (size < 1) {
            haveResult(false);
            return;
        }
        if (target == null) {
            haveResult(false);
            return;
        }
        if (vrf == null) {
            haveResult(false);
            return;
        }
        addrIP fwdTrg = userTerminal.justResolv(target, prefer);
        if (fwdTrg == null) {
            haveResult(false);
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
            haveResult(false);
            return;
        }
        switch (mode) {
            case icmp:
                ipFwdEcho ping = fwdCor.echoSendReq(fwdIfc.addr, fwdTrg, size, tim2liv, secGrp, typOsrv, flowLab, 0, false);
                if (ping == null) {
                    haveResult(false);
                    break;
                }
                ping.notif.sleep(timeout);
                if (ping.notif.totalNotifies() < 1) {
                    haveResult(false);
                    break;
                }
                if (ping.res.size() < 1) {
                    haveResult(false);
                    break;
                }
                haveResult(ping.res.get(0).err == null);
                break;
            case tcp:
                prtGen tcp = vrf.getTcp(fwdTrg);
                if (tcp == null) {
                    haveResult(false);
                    break;
                }
                pipeSide pipe = tcp.streamConnect(new pipeLine(65536, false), fwdIfc, 0, fwdTrg, size, "track", null, -1);
                if (pipe == null) {
                    haveResult(false);
                    break;
                }
                if (pipe.wait4ready(timeout)) {
                    haveResult(false);
                    pipe.setClose();
                    break;
                }
                pipe.setTime(timeout);
                pipe = secClient.openSec(pipe, secProto, pubkey, "", "");
                if (pipe == null) {
                    haveResult(false);
                    break;
                }
                pipe.setTime(timeout);
                if (chats == null) {
                    haveResult(true);
                } else {
                    haveResult(!chats.doScript(pipe));
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
                    haveResult(false);
                    break;
                }
                haveResult(true);
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
                    haveResult(false);
                    break;
                }
                haveResult(true);
                break;
            case bfd:
                fwdIfc.bfdAdd(fwdTrg, this, "tracker");
                rtrBfdNeigh bfd = fwdIfc.bfdFind(fwdTrg);
                if (bfd == null) {
                    haveResult(false);
                    break;
                }
                haveResult(bfd.getState());
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
     */
    protected synchronized void haveResult(boolean succ) {
        if (logging) {
            logger.info("result=" + succ);
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
        if (succ) {
            if (lastCount < delayUp) {
                return;
            }
        } else {
            if (lastCount < delayDn) {
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
        if ((finalState == succ) && (finalTime != 0)) {
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
        if (logging) {
            logger.info("executing " + cmd);
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

    public void run() {
        for (;;) {
            if (!cfgInit.booting) {
                break;
            }
            notif.sleep(1000);
        }
        int del = delaySt + 100;
        if (randIni > 0) {
            del += bits.random(1, randIni);
        }
        if (del > 0) {
            notif.sleep(del);
        }
        for (;;) {
            if (!working) {
                break;
            }
            try {
                doRound();
            } catch (Exception e) {
                logger.traceback(e);
            }
            del = interval;
            if (randInt > 0) {
                del += bits.random(1, randInt);
            }
            notif.sleep(del);
        }
    }

}
