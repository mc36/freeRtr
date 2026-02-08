package org.freertr.cfg;

import java.util.ArrayList;
import java.util.List;
import org.freertr.ifc.ifcUdpInt;
import org.freertr.pipe.pipeConnect;
import org.freertr.pipe.pipeDiscard;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeShell;
import org.freertr.pipe.pipeSide;
import org.freertr.tab.tabRouteIface;
import org.freertr.user.userFilter;
import org.freertr.user.userFlash;
import org.freertr.user.userHelp;
import org.freertr.user.userHwdet;
import org.freertr.user.userUpgrade;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.logBuf;
import org.freertr.util.logger;

/**
 * virtual ethernet
 *
 * @author matecsaba
 */
public class cfgVnet implements Comparable<cfgVnet>, cfgGeneric {

    /**
     * number of this bridge
     */
    public final int number;

    /**
     * hidden process
     */
    protected boolean hidden = false;

    /**
     * description of this bridge
     */
    public String description;

    /**
     * port number to use
     */
    protected int port;

    /**
     * side one
     */
    public final cfgVnetSide side1 = new cfgVnetSide(this, 1);

    /**
     * side two
     */
    public final cfgVnetSide side2 = new cfgVnetSide(this, 2);

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("vnet .*", cmds.tabulator + cmds.negated + cmds.tabulator + "description", null),
        new userFilter("vnet .*", cmds.tabulator + cmds.negated + cmds.tabulator + "side1 type", null),
        new userFilter("vnet .*", cmds.tabulator + cmds.negated + cmds.tabulator + "side1 local", null),
        new userFilter("vnet .*", cmds.tabulator + cmds.negated + cmds.tabulator + "side1 connect", null),
        new userFilter("vnet .*", cmds.tabulator + cmds.negated + cmds.tabulator + "side1 log-actions", null),
        new userFilter("vnet .*", cmds.tabulator + cmds.negated + cmds.tabulator + "side1 log-console", null),
        new userFilter("vnet .*", cmds.tabulator + cmds.negated + cmds.tabulator + "side1 log-collect", null),
        new userFilter("vnet .*", cmds.tabulator + "side1 time 1000", null),
        new userFilter("vnet .*", cmds.tabulator + "side1 delay 1000", null),
        new userFilter("vnet .*", cmds.tabulator + "side1 random-time 0", null),
        new userFilter("vnet .*", cmds.tabulator + "side1 random-delay 0", null),
        new userFilter("vnet .*", cmds.tabulator + cmds.negated + cmds.tabulator + "side2 type", null),
        new userFilter("vnet .*", cmds.tabulator + cmds.negated + cmds.tabulator + "side2 local", null),
        new userFilter("vnet .*", cmds.tabulator + cmds.negated + cmds.tabulator + "side2 connect", null),
        new userFilter("vnet .*", cmds.tabulator + cmds.negated + cmds.tabulator + "side2 log-actions", null),
        new userFilter("vnet .*", cmds.tabulator + cmds.negated + cmds.tabulator + "side2 log-console", null),
        new userFilter("vnet .*", cmds.tabulator + cmds.negated + cmds.tabulator + "side2 log-collect", null),
        new userFilter("vnet .*", cmds.tabulator + "side2 time 1000", null),
        new userFilter("vnet .*", cmds.tabulator + "side2 delay 1000", null),
        new userFilter("vnet .*", cmds.tabulator + "side2 random-time 0", null),
        new userFilter("vnet .*", cmds.tabulator + "side2 random-delay 0", null)
    };

    public int compareTo(cfgVnet o) {
        if (number < o.number) {
            return -1;
        }
        if (number > o.number) {
            return +1;
        }
        return 0;
    }

    /**
     * copy bytes
     *
     * @return copy
     */
    public cfgVnet copyBytes() {
        cfgVnet n = new cfgVnet("" + number);
        n.description = description;
        side1.copyBytes(n.side1);
        side2.copyBytes(n.side2);
        return n;
    }

    public String toString() {
        return "vnet " + number;
    }

    /**
     * create new bridge instance
     *
     * @param nam name of bridge
     */
    public cfgVnet(String nam) {
        number = bits.str2num(nam);
    }

    private void getHelpSide(userHelp l) {
        l.add(null, false, 2, new int[]{3}, "type", "type of process");
        l.add(null, false, 3, new int[]{-1}, "socat", "use socat");
        l.add(null, false, 3, new int[]{-1}, "pcap", "use pcapint");
        l.add(null, false, 3, new int[]{-1}, "raw", "use rawint");
        l.add(null, false, 3, new int[]{-1}, "map", "use mapint");
        l.add(null, false, 3, new int[]{-1}, "xsk", "use xskint");
        l.add(null, false, 3, new int[]{-1}, "bsd", "use bsdint");
        l.add(null, false, 3, new int[]{-1}, "cmp1", "use cmp1int");
        l.add(null, false, 3, new int[]{-1}, "cmp2", "use cmp2int");
        l.add(null, false, 3, new int[]{-1}, "urng", "use urngint");
        l.add(null, false, 2, new int[]{3}, "local", "name of local interface");
        l.add(null, false, 3, new int[]{-1}, "<str>", "name");
        l.add(null, false, 2, new int[]{3}, "connect", "name of connected interface");
        l.add(null, false, 3, new int[]{-1}, "<str>", "name");
        l.add(null, false, 2, new int[]{-1}, "log-actions", "log actions");
        l.add(null, false, 2, new int[]{-1}, "log-console", "log console activity");
        l.add(null, false, 2, new int[]{3}, "log-collect", "collect console activity");
        l.add(null, false, 3, new int[]{-1}, "<num>", "lines to store");
        l.add(null, false, 2, new int[]{3}, "time", "specify time between runs");
        l.add(null, false, 3, new int[]{-1}, "<num>", "milliseconds between runs");
        l.add(null, false, 2, new int[]{3}, "delay", "specify initial delay");
        l.add(null, false, 3, new int[]{-1}, "<num>", "milliseconds before start");
        l.add(null, false, 2, new int[]{3}, "random-time", "specify random time between runs");
        l.add(null, false, 3, new int[]{-1}, "<num>", "milliseconds between runs");
        l.add(null, false, 2, new int[]{3}, "random-delay", "specify random initial delay");
        l.add(null, false, 3, new int[]{-1}, "<num>", "milliseconds before start");
    }

    public void getHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2, -1}, "description", "description of this bridge");
        l.add(null, false, 2, new int[]{2, -1}, "[text]", "text describing this bridge");
        l.add(null, false, 1, new int[]{2}, "side1", "configure first side");
        getHelpSide(l);
        l.add(null, false, 1, new int[]{2}, "side2", "configure second side");
        getHelpSide(l);
    }

    public List<String> getShRun(int filter) {
        List<String> l = new ArrayList<String>();
        if (hidden) {
            return l;
        }
        l.add("vnet " + number);
        cmds.cfgLine(l, description == null, cmds.tabulator, "description", description);
        side1.getShRun(l, cmds.tabulator);
        side2.getShRun(l, cmds.tabulator);
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        if ((filter & 1) == 0) {
            return l;
        }
        return userFilter.filterText(l, defaultF);
    }

    public void doCfgStr(cmds cmd) {
        String a = cmd.word();
        if (a.equals("description")) {
            description = cmd.getRemaining();
            return;
        }
        if (a.equals("side1")) {
            side1.doCfgStr(cmd);
            return;
        }
        if (a.equals("side2")) {
            side2.doCfgStr(cmd);
            return;
        }
        if (!a.equals(cmds.negated)) {
            cmd.badCmd();
            return;
        }
        a = cmd.word();
        if (a.equals("description")) {
            description = null;
            return;
        }
        if (a.equals("side1")) {
            side1.doUnCfg(cmd);
            return;
        }
        if (a.equals("side2")) {
            side2.doUnCfg(cmd);
            return;
        }
        cmd.badCmd();
    }

    public String getPrompt() {
        return "vnet";
    }

    /**
     * stop work
     */
    public void stopNow() {
        side1.stopNow();
        side2.stopNow();
    }

    /**
     * start work
     *
     * @param prt port to use
     */
    public void startNow(int prt) {
        if (side1.need2run || side2.need2run) {
            return;
        }
        port = prt;
        List<String> lst = bits.str2lst(userHwdet.scrBeg);
        userHwdet.setupVeth(lst, "./", userHwdet.ifcTyp.raw, side1.getOSname(), side2.getOSname());
        userHwdet.setupIface(lst, "./", userHwdet.ifcTyp.raw, side1.getOSname(), 8192, null);
        userHwdet.setupIface(lst, "./", userHwdet.ifcTyp.raw, side2.getOSname(), 8192, null);
        String a = cfgInit.getRWpath() + "vnet" + bits.randomD() + userUpgrade.tmpExt;
        if (bits.buf2txt(true, lst, a)) {
            return;
        }
        userFlash.setFilePerm(a, true, false, true, true, false, true);
        pipeShell.exec(a, null, true, true, true);
        userFlash.delete(a);
        side1.startNow(prt + 0, prt + 1);
        side2.startNow(prt + 2, prt + 3);
    }

}

class cfgVnetSide implements Runnable {

    public final cfgVnet parent;

    public final int sideId;

    public userHwdet.ifcTyp ifcTyp;

    public tabRouteIface.ifaceType locTyp;

    public String locNam;

    public String conNam;

    public int prtLoc;

    public int prtRem;

    public boolean need2run;

    protected int interval = 1000;

    protected int initial = 1000;

    public int randInt;

    public int randIni;

    public boolean logAct = false;

    public boolean logCon = false;

    public logBuf logCol;

    public pipeSide cons;

    private pipeShell proc;

    private pipeSide pipe;

    public int restartE;

    public int restartC;

    public long restartT;

    public cfgVnetSide(cfgVnet p, int i) {
        parent = p;
        sideId = i;
    }

    public void copyBytes(cfgVnetSide n) {
        n.ifcTyp = ifcTyp;
        n.locTyp = locTyp;
        n.locNam = locNam;
        n.initial = initial;
        n.interval = interval;
        n.randIni = randIni;
        n.randInt = randInt;
        n.logAct = logAct;
        n.logCon = logCon;
        n.logCol = logCol;
        n.prtLoc = prtLoc;
        n.prtRem = prtRem;
    }

    public void getShRun(List<String> lst, String beg1) {
        String beg2 = "side" + sideId;
        cmds.cfgLine(lst, ifcTyp == null, beg1, beg2 + " type", "" + ifcTyp);
        cmds.cfgLine(lst, locNam == null, beg1, beg2 + " local", locNam);
        cmds.cfgLine(lst, conNam == null, beg1, beg2 + " connect", conNam);
        cmds.cfgLine(lst, !logAct, beg1, beg2 + " log-actions", "");
        cmds.cfgLine(lst, !logCon, beg1, beg2 + " log-console", "");
        cmds.cfgLine(lst, logCol == null, beg1, beg2 + " log-collect", "" + logBuf.getSize(logCol));
        lst.add(beg1 + beg2 + " delay " + initial);
        lst.add(beg1 + beg2 + " time " + interval);
        lst.add(beg1 + beg2 + " random-time " + randInt);
        lst.add(beg1 + beg2 + " random-delay " + randIni);
    }

    public void doCfgStr(cmds cmd) {
        String a = cmd.word();
        if (a.equals("delay")) {
            initial = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("time")) {
            interval = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("random-time")) {
            randInt = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("random-delay")) {
            randIni = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("log-actions")) {
            logAct = true;
            return;
        }
        if (a.equals("log-collect")) {
            logCol = new logBuf(bits.str2num(cmd.word()));
            return;
        }
        if (a.equals("log-console")) {
            logCon = true;
            return;
        }
        if (a.equals("local")) {
            String pnm[] = cfgIfc.dissectName(cmd.word());
            if (pnm == null) {
                cmd.error("invalid interface name");
                return;
            }
            a = pnm[0] + pnm[1] + pnm[2];
            locTyp = cfgIfc.string2type(a);
            if (locTyp == null) {
                cmd.error("bad name");
                return;
            }
            if (cfgInit.ifaceLst.find(new cfgVdcIfc(a, "")) != null) {
                cmd.error("physical interface exists");
                return;
            }
            locNam = a;
            return;
        }
        if (a.equals("connect")) {
            conNam = cmd.word();
            return;
        }
        if (a.equals("type")) {
            a = cmd.word();
            ifcTyp = userHwdet.string2type(a);
            return;
        }
        cmd.badCmd();
    }

    public void doUnCfg(cmds cmd) {
        String a = cmd.word();
        if (a.equals("random-time")) {
            randInt = 0;
            return;
        }
        if (a.equals("random-delay")) {
            randIni = 0;
            return;
        }
        if (a.equals("log-actions")) {
            logAct = false;
            return;
        }
        if (a.equals("log-collect")) {
            logCol = null;
            return;
        }
        if (a.equals("log-console")) {
            logCon = false;
            return;
        }
        if (a.equals("local")) {
            locNam = null;
            locTyp = null;
            return;
        }
        if (a.equals("connect")) {
            conNam = null;
            return;
        }
        if (a.equals("type")) {
            ifcTyp = null;
            return;
        }
        cmd.badCmd();
    }

    public void startNow(int pl, int pr) {
        prtLoc = pl;
        prtRem = pr;
        if (!cfgInit.booting) {
            return;
        }
        if (locNam != null) {
            ifcUdpInt hdr = new ifcUdpInt("127.0.0.1", pl, "127.0.0.1", pr, "-", locTyp != tabRouteIface.ifaceType.ether, false);
            cfgIfc ifc = cfgAll.ifcAdd(locNam, locTyp, hdr, 1);
            if (ifc == null) {
                return;
            }
            ifc.initPhysical();
        }
        if (ifcTyp == null) {
            return;
        }
        need2run = true;
        logger.startThread(this);
    }

    public void stopNow() {
        need2run = false;
        restartNow();
    }

    public void restartNow() {
        try {
            proc.kill();
        } catch (Exception e) {
        }
        try {
            pipe.setClose();
        } catch (Exception e) {
        }
    }

    public void run() {
        for (;;) {
            if (!cfgInit.booting) {
                break;
            }
            bits.sleep(1000);
        }
        int del = initial;
        if (randIni > 0) {
            del += bits.random(1, randIni);
        }
        if (del > 0) {
            bits.sleep(del);
        }
        for (;;) {
            bits.sleep(interval);
            if (!need2run) {
                break;
            }
            try {
                doRound();
            } catch (Exception e) {
                logger.traceback(e);
            }
        }
    }

    public String getOSname() {
        return "exthairpin" + parent.number + (sideId == 1 ? "a" : "b");
    }

    private synchronized boolean doRound() {
        String a = null;
        if (locNam != null) {
            a = userHwdet.interface2command("./", ifcTyp, getOSname(), prtLoc, prtRem);
        }
        if (conNam != null) {
            a = userHwdet.connection2command("./", ifcTyp, conNam, getOSname());
        }
        if (a == null) {
            return true;
        }
        if (logAct) {
            logger.info("restarting process vnet " + parent.number + " " + sideId);
        }
        if (randInt > 0) {
            bits.sleep(bits.random(1, randInt));
        }
        restartT = bits.getTime();
        restartC++;
        pipeLine pl = new pipeLine(65536, false);
        pipe = pl.getSide();
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        pipe.lineRx = pipeSide.modTyp.modeCRorLF;
        proc = pipeShell.exec(pl.getSide(), a, null, true, true, false, true);
        if (proc == null) {
            return false;
        }
        for (;;) {
            if (!proc.isRunning()) {
                break;
            }
            if (cons == null) {
                pipeDiscard.logLines("vnet " + parent.number + " " + sideId + " said ", pipe, logCon, logCol);
                bits.sleep(1000);
                continue;
            }
            boolean b = pipeConnect.redirect(pipe, cons);
            b |= pipeConnect.redirect(cons, pipe);
            if (b) {
                cons.setClose();
                cons = null;
            }
            bits.sleep(100);
        }
        restartE = proc.resultNum();
        return false;
    }

}
