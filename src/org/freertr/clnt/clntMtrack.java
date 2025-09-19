package org.freertr.clnt;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgVrf;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.pack.packHolder;
import org.freertr.pack.packMtrack;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServS;
import org.freertr.prt.prtUdp;
import org.freertr.tab.tabGen;
import org.freertr.user.userFormat;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.logger;
import org.freertr.util.notifier;

/**
 * mtracker worker
 *
 * @author matecsaba
 */
public class clntMtrack implements Runnable, prtServS {

    /**
     * create instance
     */
    public clntMtrack() {
    }

    /**
     * name of this tracker
     */
    public String name;

    /**
     * configured group, null=client
     */
    public addrIP cfgGrp;

    /**
     * configured targets
     */
    public tabGen<addrIP> cfgTrg = new tabGen<addrIP>();

    /**
     * port number
     */
    public int port = packMtrack.port;

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
    public int interval = 30000;

    /**
     * timeout value
     */
    public int timeout = 10;

    /**
     * type of service value
     */
    public int typOsrv;

    /**
     * type of time to live
     */
    public int tim2liv = 255;

    /**
     * flow label value
     */
    public int flwLab;

    /**
     * inter packet time
     */
    public int packTim = 10;

    /**
     * logging
     */
    public boolean logging = false;

    /**
     * change count
     */
    public int chngCnt;

    /**
     * change time
     */
    public long chngTim;

    private int working = 0;

    private int rnd = 0;

    private addrIP grp;

    private prtUdp udp;

    private ipFwd fwd;

    private ipFwdIface ifc;

    private tabGen<clntMtrackPeer> pers = new tabGen<clntMtrackPeer>();

    private notifier notif = new notifier();

    /**
     * get state
     *
     * @return state
     */
    public boolean getWorking() {
        return (working & 1) != 0;
    }

    /**
     * stop running
     */
    public synchronized void stopNow() {
        if (!getWorking()) {
            return;
        }
        working &= 0xfe;
        notif.wakeup();
        for (;;) {
            bits.sleep(100);
            if (working == 0) {
                break;
            }
        }
        doUnbind();
    }

    /**
     * start running
     */
    public synchronized void startNow() {
        if (getWorking()) {
            return;
        }
        if (interval < 1) {
            return;
        }
        if (vrf == null) {
            return;
        }
        if (srcIfc == null) {
            return;
        }
        if (cfgTrg.size() < 1) {
            return;
        }
        if (srcIfc.getFwdIfc(cfgTrg.get(0)) == null) {
            return;
        }
        doRebind();
        working = 1;
        new Thread(this).start();
    }

    private void doRound() {
        rnd++;
        if (grp == null) {
            doConfig();
            return;
        }
        if ((rnd % timeout) == 0) {
            doConfig();
        }
        doReport();
    }

    private void doUnbind() {
        udp.listenStop(ifc, port, null, 0);
        if (grp == null) {
            return;
        }
        if (!grp.isMulticast()) {
            return;
        }
        for (int i = 0; i < pers.size(); i++) {
            fwd.mcastDelFloodIfc(grp, pers.get(i).adr, null);
        }
    }

    private void doRebind() {
        if (cfgGrp != null) {
            pers.clear();
            for (int i = 0; i < cfgTrg.size(); i++) {
                pers.add(new clntMtrackPeer(cfgTrg.get(i)));
            }
            grp = cfgGrp.copyBytes();
        }
        for (int o = 0; o < pers.size(); o++) {
            tabGen<clntMtrackRprt> r = new tabGen<clntMtrackRprt>();
            for (int i = 0; i < pers.size(); i++) {
                r.add(new clntMtrackRprt(pers.get(i).adr));
            }
            pers.get(o).rprt = r;
        }
        addrIP srv = cfgTrg.get(0);
        udp = vrf.getUdp(srv);
        fwd = vrf.getFwd(srv);
        ifc = srcIfc.getFwdIfc(srv);
        rnd = 0;
        if (grp == null) {
            return;
        }
        udp.streamListen(this, new pipeLine(32768, true), ifc, port, null, 0, "mtrck", -1, null, -1, -1);
        if (!grp.isMulticast()) {
            return;
        }
        for (int i = 0; i < pers.size(); i++) {
            fwd.mcastAddFloodIfc(grp, pers.get(i).adr, null, -1);
        }
    }

    private void doConfig() {
        if (cfgGrp != null) {
            return;
        }
        pipeSide pipe = udp.streamConnect(new pipeLine(32768, true), ifc, 0, cfgTrg.get(0), port, "mtrck", -1, null, -1, -1);
        if (pipe == null) {
            return;
        }
        pipe.setTime(5000);
        packHolder pckB = new packHolder(true, true);
        packMtrack pck = new packMtrack();
        pck.typ = packMtrack.typCfgReq;
        pck.createPacket(pckB);
        pckB.pipeSend(pipe, 0, pckB.dataSize(), 2);
        tabGen<clntMtrackPeer> clnt = new tabGen<clntMtrackPeer>();
        addrIP cgrp = null;
        addrIP cfg = null;
        for (;;) {
            pckB = pipe.readPacket(true);
            if (pckB == null) {
                break;
            }
            pck.parsePacket(pckB);
            switch (pck.typ) {
                case packMtrack.typCfgRep:
                case packMtrack.typCfgEnd:
                    break;
                default:
                    continue;
            }
            for (int i = 0; i < pck.adrs.size(); i++) {
                addrIP adr = pck.adrs.get(i);
                if (cfg == null) {
                    cfg = adr;
                    continue;
                }
                if (cgrp == null) {
                    cgrp = adr;
                    continue;
                }
                clnt.add(new clntMtrackPeer(adr));
            }
            if (pck.typ == packMtrack.typCfgEnd) {
                break;
            }
        }
        pipe.setClose();
        if ((cfg == null) || (cgrp == null)) {
            return;
        }
        if (packMtrack.validateCfg(cgrp, cfg)) {
            logger.info("got invalid config");
            return;
        }
        if (bits.msbGetD(cfg.getBytes(), 0) != clnt.size()) {
            logger.info("got incomplete config");
            return;
        }
        interval = bits.msbGetD(cfg.getBytes(), 4);
        timeout = bits.msbGetD(cfg.getBytes(), 8);
        boolean b = grp == null;
        if (!b) {
            b = cgrp.compareTo(grp) != 0;
        }
        b |= clnt.size() != pers.size();
        if (!b) {
            for (int i = 0; i < clnt.size(); i++) {
                b |= pers.find(clnt.get(i)) == null;
            }
        }
        if (!b) {
            return;
        }
        if (logging) {
            logger.info("config changed, group=" + cgrp + " peer=" + clnt.size());
        }
        doUnbind();
        pers = clnt;
        grp = cgrp;
        doRebind();
    }

    private void doReport() {
        if (grp == null) {
            return;
        }
        List<pipeSide> pipes = new ArrayList<pipeSide>();
        if (grp.isMulticast()) {
            pipeSide pipe = udp.streamConnect(new pipeLine(32768, true), ifc, 0, grp, port, "mtrck", -1, null, -1, -1);
            if (pipe == null) {
                return;
            }
            pipes.add(pipe);
        } else {
            for (int i = 0; i < pers.size(); i++) {
                pipeSide pipe = udp.streamConnect(new pipeLine(32768, true), ifc, 0, pers.get(i).adr, port, "mtrck", -1, null, -1, -1);
                if (pipe == null) {
                    continue;
                }
                pipes.add(pipe);
            }
        }
        for (int o = 0; o < pipes.size(); o++) {
            pipes.get(o).setTime(20000);
        }
        packHolder pckB = new packHolder(true, true);
        packMtrack pck = new packMtrack();
        pck.typ = packMtrack.typReport;
        long tim = bits.getTime();
        pck.tim = tim;
        tim -= (interval * timeout);
        addrIP my = ifc.addr;
        clntMtrackPeer.computeRxing(pers, my);
        for (int i = 0; i < pers.size(); i++) {
            clntMtrackPeer ntry = pers.get(i);
            boolean chg = ntry.computeRxing(tim, my);
            if (chg) {
                ntry.chngCnt++;
                ntry.chngTim = tim;
                chngCnt++;
                chngTim = tim;
            }
            if (logging && chg) {
                logger.info("tracker " + ntry.adr + " " + cmds.upDown(ntry.rxing));
            }
            if (!ntry.rxing) {
                continue;
            }
            pck.adrs.add(ntry.adr);
            pck.rtts.add(ntry.rtt);
            if (pck.adrs.size() < packMtrack.maxAddrs) {
                continue;
            }
            pck.createPacket(pckB);
            for (int o = 0; o < pipes.size(); o++) {
                pckB.pipeSend(pipes.get(o), 0, pckB.dataSize(), 2);
            }
            bits.sleep(packTim);
            pck.adrs.clear();
            pck.tim = bits.getTime();
        }
        pck.typ = packMtrack.typLreport;
        pck.createPacket(pckB);
        for (int o = 0; o < pipes.size(); o++) {
            pipeSide pipe = pipes.get(o);
            pckB.pipeSend(pipe, 0, pckB.dataSize(), 2);
            pipe.setClose();
        }
    }

    /**
     * process one packet
     *
     * @param pipe pipe to use
     * @param addr address of peer
     * @param pckB packet to process
     */
    protected void doPacket(pipeSide pipe, addrIP addr, packHolder pckB) {
        packMtrack pck = new packMtrack();
        pck.parsePacket(pckB);
        switch (pck.typ) {
            case packMtrack.typLreport:
            case packMtrack.typReport:
                break;
            case packMtrack.typCfgReq:
                if (cfgGrp == null) {
                    return;
                }
                addrIP adr = new addrIP();
                bits.msbPutD(adr.getBytes(), 0, pers.size());
                bits.msbPutD(adr.getBytes(), 4, interval);
                bits.msbPutD(adr.getBytes(), 8, timeout);
                packMtrack.updateCfg(cfgGrp, adr);
                pck.typ = packMtrack.typCfgRep;
                pck.adrs.clear();
                pck.adrs.add(adr);
                pck.adrs.add(cfgGrp);
                for (int i = 0; i < cfgTrg.size(); i++) {
                    pck.adrs.add(cfgTrg.get(i));
                    if (pck.adrs.size() < packMtrack.maxAddrs) {
                        continue;
                    }
                    pck.createPacket(pckB);
                    pckB.pipeSend(pipe, 0, pckB.dataSize(), 2);
                    pck.adrs.clear();
                    bits.sleep(packTim);
                }
                pck.typ = packMtrack.typCfgEnd;
                pck.createPacket(pckB);
                pckB.pipeSend(pipe, 0, pckB.dataSize(), 2);
                return;
            case packMtrack.typCfgRep:
            case packMtrack.typCfgEnd:
                return;
            default:
                logger.info("got unknown type (" + pck.typ + ") from " + addr);
                return;
        }
        clntMtrackPeer ntry = new clntMtrackPeer(addr);
        ntry = pers.find(ntry);
        if (ntry == null) {
            return;
        }
        ntry.gotReport(pck);
        if (pck.typ == packMtrack.typLreport) {
            pipe.setClose();
        }
    }

    /**
     * get summary line
     *
     * @return string
     */
    public String getShSum() {
        int o = 0;
        int p = 0;
        for (int i = 0; i < pers.size(); i++) {
            if (pers.get(i).rxing) {
                o++;
            }
            if (pers.get(i).bidir) {
                p++;
            }
        }
        return name + "|" + grp + "|" + port + "|" + pers.size() + "|" + o + "|" + p + "|" + bits.timePast(chngTim);
    }

    /**
     * get detailed status
     *
     * @return status strings
     */
    public userFormat getShStat() {
        userFormat l = new userFormat("|", "category|value");
        l.add("name|" + name);
        l.add("round|" + rnd);
        l.add("group|" + grp);
        l.add("port|" + port);
        l.add("timer|" + interval + "/" + timeout);
        l.add("source|" + srcIfc);
        l.add("changes|" + chngCnt);
        l.add("ago|" + bits.timePast(chngTim) + ", at=" + bits.time2str(cfgAll.timeZoneName, chngTim + cfgAll.timeServerOffset, 3));
        return l;
    }

    /**
     * get detailed status
     *
     * @return status strings
     */
    public userFormat getShPeer() {
        userFormat l = new userFormat("|", "number|address|state|bidir|changes|ago|at|rtt|reports|last");
        for (int i = 0; i < pers.size(); i++) {
            clntMtrackPeer ntry = pers.get(i);
            l.add(i + "|" + ntry.adr + "|" + cmds.upDown(ntry.rxing) + "|" + cmds.upDown(ntry.bidir) + "|" + ntry.chngCnt + "|" + bits.timePast(ntry.chngTim) + "|" + bits.time2str(cfgAll.timeZoneName, ntry.chngTim + cfgAll.timeServerOffset, 3) + "|" + ntry.rtt + "|" + ntry.reports + "|" + bits.timePast(ntry.lastRx));
        }
        return l;
    }

    /**
     * get detailed status
     *
     * @return matrix
     */
    public userFormat getShList() {
        userFormat l = new userFormat("|", "who|from|state|changes|ago|at|rtt|reports|last");
        for (int o = 0; o < pers.size(); o++) {
            clntMtrackPeer ntry = pers.get(o);
            for (int i = 0; i < pers.size(); i++) {
                if (i == o) {
                    continue;
                }
                clntMtrackRprt rep = ntry.rprt.get(i);
                l.add(ntry.adr + "|" + rep.adr + "|" + cmds.upDown(rep.rxing) + "|" + rep.chngCnt + "|" + bits.timePast(rep.chngTim) + "|" + bits.time2str(cfgAll.timeZoneName, rep.chngTim + cfgAll.timeServerOffset, 3) + "|" + rep.rtt + "|" + rep.reports + "|" + bits.timePast(rep.lastRx));
            }
        }
        return l;
    }

    /**
     * get matrix
     *
     * @return matrix
     */
    public userFormat getShMatrixReach() {
        String s = "\\|";
        for (int i = 0; i < pers.size(); i++) {
            s += i + "|";
        }
        userFormat l = new userFormat("|", s);
        for (int o = 0; o < pers.size(); o++) {
            clntMtrackPeer ntry = pers.get(o);
            s = o + "|";
            for (int i = 0; i < pers.size(); i++) {
                if (o == i) {
                    s += "\\|";
                    continue;
                }
                clntMtrackRprt r = new clntMtrackRprt(pers.get(i).adr);
                r = ntry.rprt.find(r);
                if (r == null) {
                    s += "?|";
                    continue;
                }
                if (r.rxing) {
                    s += "+|";
                } else {
                    s += "-|";
                }
            }
            l.add(s);
        }
        return l;
    }

    /**
     * get matrix
     *
     * @return matrix
     */
    public userFormat getShMatrixTime() {
        String s = "-1|";
        for (int i = 0; i < pers.size(); i++) {
            s += i + "|";
        }
        userFormat l = new userFormat("|", s);
        for (int o = 0; o < pers.size(); o++) {
            clntMtrackPeer ntry = pers.get(o);
            s = o + "|";
            for (int i = 0; i < pers.size(); i++) {
                if (o == i) {
                    s += "-1|";
                    continue;
                }
                clntMtrackRprt r = new clntMtrackRprt(pers.get(i).adr);
                r = ntry.rprt.find(r);
                if (r == null) {
                    s += "0|";
                    continue;
                }
                if (r.rxing) {
                    s += r.rtt + "|";
                } else {
                    s += "0|";
                }
            }
            l.add(s);
        }
        return l;
    }

    /**
     * accept connection
     *
     * @param pipe pipeline
     * @param id connection
     * @return false on success, true on error
     */
    public boolean streamAccept(pipeSide pipe, prtGenConn id) {
        pipe.setTime(10000);
        id.sendTOS = typOsrv;
        id.sendTTL = tim2liv;
        id.sendFLW = flwLab;
        new clntMtrackConn(this, pipe, id.peerAddr);
        return false;
    }

    /**
     * interface closed
     *
     * @param ifc interface
     */
    public void closedInterface(ipFwdIface ifc) {
    }

    /**
     * get block mode
     *
     * @return mode
     */
    public boolean streamForceBlock() {
        return true;
    }

    public void run() {
        working |= 2;
        try {
            for (;;) {
                notif.sleep(interval);
                if (!getWorking()) {
                    break;
                }
                doRound();
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        working &= 0xfd;
    }
}

class clntMtrackConn implements Runnable {

    private clntMtrack lower;

    private pipeSide pipe;

    private addrIP addr;

    public clntMtrackConn(clntMtrack parent, pipeSide conn, addrIP peer) {
        lower = parent;
        pipe = conn;
        addr = peer;
        new Thread(this).start();
    }

    public void run() {
        try {
            pipe.wait4ready(10000);
            for (;;) {
                packHolder pck = pipe.readPacket(true);
                if (pck == null) {
                    break;
                }
                lower.doPacket(pipe, addr, pck);
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        pipe.setClose();
    }

}

class clntMtrackRprt implements Comparable<clntMtrackRprt> {

    public final addrIP adr;

    public long lastRx;

    public int rtt;

    public boolean rxing;

    public int reports;

    public int chngCnt;

    public long chngTim;

    public clntMtrackRprt(addrIP peer) {
        adr = peer.copyBytes();
    }

    public int compareTo(clntMtrackRprt o) {
        return adr.compareTo(o.adr);
    }

}

class clntMtrackPeer implements Comparable<clntMtrackPeer> {

    public final addrIP adr;

    public long lastRx;

    public int rtt;

    public boolean rxing;

    public boolean bidir;

    public int reports;

    public int chngCnt;

    public long chngTim;

    public tabGen<clntMtrackRprt> rprt = new tabGen<clntMtrackRprt>();

    public clntMtrackPeer(addrIP peer) {
        adr = peer.copyBytes();
    }

    public int compareTo(clntMtrackPeer o) {
        return adr.compareTo(o.adr);
    }

    public static void computeRxing(tabGen<clntMtrackPeer> pers, addrIP my) {
        clntMtrackPeer own = new clntMtrackPeer(my);
        own = pers.find(own);
        if (own == null) {
            return;
        }
        own.lastRx = bits.getTime();
        own.reports = -9;
        for (int i = 0; i < pers.size(); i++) {
            clntMtrackPeer ntry = pers.get(i);
            clntMtrackRprt r = new clntMtrackRprt(ntry.adr);
            r = own.rprt.find(r);
            if (r == null) {
                continue;
            }
            r.lastRx = ntry.lastRx;
        }
    }

    public boolean computeRxing(long tim, addrIP my) {
        boolean old = rxing;
        rxing = lastRx > tim;
        for (int i = 0; i < rprt.size(); i++) {
            clntMtrackRprt r = rprt.get(i);
            boolean bak = r.rxing;
            r.rxing = r.lastRx > tim;
            if (bak == r.rxing) {
                continue;
            }
            r.chngTim = tim;
            r.chngCnt++;
        }
        if (!rxing) {
            rtt = 0;
        }
        clntMtrackRprt ntry = new clntMtrackRprt(my);
        ntry = rprt.find(ntry);
        if (ntry == null) {
            bidir = false;
        } else {
            bidir = ntry.rxing;
        }
        return old != rxing;
    }

    public void gotReport(packMtrack pck) {
        lastRx = bits.getTime();
        rtt = (int) (lastRx - pck.tim);
        if (rtt < 0) {
            rtt = -rtt;
        }
        if (rtt > 255) {
            rtt = 255;
        }
        reports++;
        for (int i = 0; i < pck.adrs.size(); i++) {
            clntMtrackRprt r = new clntMtrackRprt(pck.adrs.get(i));
            r = rprt.find(r);
            if (r == null) {
                return;
            }
            r.rtt = pck.rtts.get(i);
            r.lastRx = lastRx;
            r.reports++;
        }
    }

}
