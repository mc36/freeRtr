package org.freertr.rtr;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgPrfxlst;
import org.freertr.cfg.cfgRoump;
import org.freertr.cfg.cfgRouplc;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipPrt;
import org.freertr.pack.packHolder;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabListing;
import org.freertr.tab.tabPrfxlstN;
import org.freertr.tab.tabRoute;
import org.freertr.tab.tabRtrmapN;
import org.freertr.tab.tabRtrplcN;
import org.freertr.user.userFormat;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.counter;
import org.freertr.util.counter.reasons;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.state;
import org.freertr.util.state.states;
import org.freertr.enc.encTlv;
import org.freertr.sec.secInfoCfg;
import org.freertr.sec.secInfoUtl;

/**
 * eigrp interface
 *
 * @author matecsaba
 */
public class rtrEigrpIface implements Comparable<rtrEigrpIface>, ipPrt {

    /**
     * ipinfo config
     */
    public secInfoCfg ipInfoCfg;

    /**
     * hello interval
     */
    public int helloTimer = 5000;

    /**
     * dead interval
     */
    public int deadTimer = 15000;

    /**
     * default distance
     */
    public int distance = 90;

    /**
     * in delay
     */
    public int delayIn = 10;

    /**
     * out delay
     */
    public int delayOut = 0;

    /**
     * bfd enabled
     */
    public boolean bfdTrigger;

    /**
     * passive interface
     */
    public boolean passiveInt;

    /**
     * advertise default route
     */
    public boolean defOrigin = false;

    /**
     * not advertise routes learned from interface back
     */
    public boolean splitHorizon = true;

    /**
     * suppress interface address
     */
    public boolean suppressAddr = false;

    /**
     * unsuppress interface address
     */
    public boolean unsuppressAddr = false;

    /**
     * check neighbor address is connected
     */
    public boolean connectedCheck = true;

    /**
     * ingress prefix list
     */
    public tabListing<tabPrfxlstN, addrIP> prflstIn;

    /**
     * egress prefix list
     */
    public tabListing<tabPrfxlstN, addrIP> prflstOut;

    /**
     * ingress route map
     */
    public tabListing<tabRtrmapN, addrIP> roumapIn;

    /**
     * egress route map
     */
    public tabListing<tabRtrmapN, addrIP> roumapOut;

    /**
     * ingress route policy
     */
    public tabListing<tabRtrplcN, addrIP> roupolIn;

    /**
     * egress route policy
     */
    public tabListing<tabRtrplcN, addrIP> roupolOut;

    /**
     * the lower layer
     */
    protected final rtrEigrp lower;

    /**
     * list of neighbors
     */
    protected tabGen<rtrEigrpNeigh> neighs;

    /**
     * the interface this works on
     */
    protected final ipFwdIface iface;

    /**
     * routes needed to advertise
     */
    protected tabRoute<addrIP> need2adv;

    /**
     * counter
     */
    protected counter cntr;

    /**
     * keepalive
     */
    protected rtrEigrpIfaceHello keepTimer;

    /**
     * create one instance
     *
     * @param parent the rip protocol
     * @param ifc the ip interface to work on
     */
    public rtrEigrpIface(rtrEigrp parent, ipFwdIface ifc) {
        lower = parent;
        iface = ifc;
        neighs = new tabGen<rtrEigrpNeigh>();
        cntr = new counter();
    }

    /**
     * unregister from ip
     */
    public void unregister2ip() {
        lower.fwdCore.protoDel(this, iface, null);
    }

    /**
     * register to ip
     */
    public void register2ip() {
        lower.fwdCore.protoAdd(this, iface, null);
    }

    /**
     * list of neighbors
     *
     * @param res list to update
     */
    protected void showNeighs(userFormat res) {
        for (int i = 0; i < neighs.size(); i++) {
            rtrEigrpNeigh nei = neighs.get(i);
            res.add(iface + "|" + nei.peer + "|" + nei.learned.size() + "|" + nei.adverted.size() + "|" + bits.timePast(nei.upTime));
        }
    }

    /**
     * find one neighbor
     *
     * @param adr address of peer
     * @return neighbor, null if not found
     */
    protected rtrEigrpNeigh findNeigh(addrIP adr) {
        for (int i = 0; i < neighs.size(); i++) {
            rtrEigrpNeigh nei = neighs.get(i);
            if (adr.compareTo(nei.peer) == 0) {
                return nei;
            }
        }
        return null;
    }

    /**
     * get configuration
     *
     * @param l list to add
     * @param beg beginning
     */
    public void routerGetConfig(List<String> l, String beg) {
        l.add(cmds.tabulator + beg + "enable");
        cmds.cfgLine(l, !splitHorizon, cmds.tabulator, beg + "split-horizon", "");
        cmds.cfgLine(l, !passiveInt, cmds.tabulator, beg + "passive", "");
        cmds.cfgLine(l, !bfdTrigger, cmds.tabulator, beg + "bfd", "");
        cmds.cfgLine(l, !defOrigin, cmds.tabulator, beg + "default-originate", "");
        cmds.cfgLine(l, !suppressAddr, cmds.tabulator, beg + "suppress-prefix", "");
        cmds.cfgLine(l, !unsuppressAddr, cmds.tabulator, beg + "unsuppress-prefix", "");
        cmds.cfgLine(l, !connectedCheck, cmds.tabulator, beg + "verify-source", "");
        l.add(cmds.tabulator + beg + "delay-in " + delayIn);
        l.add(cmds.tabulator + beg + "delay-out " + delayOut);
        l.add(cmds.tabulator + beg + "distance " + distance);
        l.add(cmds.tabulator + beg + "hello-time " + helloTimer);
        l.add(cmds.tabulator + beg + "dead-time " + deadTimer);
        cmds.cfgLine(l, prflstIn == null, cmds.tabulator, beg + "prefix-list-in", "" + prflstIn);
        cmds.cfgLine(l, prflstOut == null, cmds.tabulator, beg + "prefix-list-out", "" + prflstOut);
        cmds.cfgLine(l, roumapIn == null, cmds.tabulator, beg + "route-map-in", "" + roumapIn);
        cmds.cfgLine(l, roumapOut == null, cmds.tabulator, beg + "route-map-out", "" + roumapOut);
        cmds.cfgLine(l, roupolIn == null, cmds.tabulator, beg + "route-policy-in", "" + roupolIn);
        cmds.cfgLine(l, roupolOut == null, cmds.tabulator, beg + "route-policy-out", "" + roupolOut);
        secInfoUtl.getConfig(l, ipInfoCfg, cmds.tabulator + beg + "ipinfo ");
    }

    /**
     * get help text
     *
     * @param l list to update
     */
    public static void routerGetHelp(userHelp l) {
        l.add(null, false, 4, new int[]{-1}, "enable", "enable protocol processing");
        l.add(null, false, 4, new int[]{-1}, "bfd", "enable bfd triggered down");
        l.add(null, false, 4, new int[]{-1}, "default-originate", "send default route to peer");
        l.add(null, false, 4, new int[]{-1}, "split-horizon", "dont advertise back on rx interface");
        l.add(null, false, 4, new int[]{-1}, "passive", "do not form neighborship");
        l.add(null, false, 4, new int[]{-1}, "suppress-prefix", "do not advertise interface");
        l.add(null, false, 4, new int[]{-1}, "unsuppress-prefix", "do advertise interface");
        l.add(null, false, 4, new int[]{-1}, "verify-source", "check source address of updates");
        l.add(null, false, 4, new int[]{5}, "delay-in", "ingress delay of routes");
        l.add(null, false, 5, new int[]{-1}, "<num>", "set delay");
        l.add(null, false, 4, new int[]{5}, "delay-out", "egress delay of routes");
        l.add(null, false, 5, new int[]{-1}, "<num>", "set delay");
        l.add(null, false, 4, new int[]{5}, "distance", "administrative distance of routes");
        l.add(null, false, 5, new int[]{-1}, "<num>", "set administrative distance");
        l.add(null, false, 4, new int[]{5}, "hello-time", "time between hellos");
        l.add(null, false, 5, new int[]{-1}, "<num>", "time in ms");
        l.add(null, false, 4, new int[]{5}, "dead-time", "time before neighbor down");
        l.add(null, false, 5, new int[]{-1}, "<num>", "time in ms");
        l.add(null, false, 4, new int[]{5}, "route-map-in", "process prefixes in ingress updates");
        l.add(null, false, 5, new int[]{-1}, "<name:rm>", "name of route map");
        l.add(null, false, 4, new int[]{5}, "route-map-out", "process prefixes in egress updates");
        l.add(null, false, 5, new int[]{-1}, "<name:rm>", "name of route map");
        l.add(null, false, 4, new int[]{5}, "route-policy-in", "process prefixes in ingress updates");
        l.add(null, false, 5, new int[]{-1}, "<name:rpl>", "name of route policy");
        l.add(null, false, 4, new int[]{5}, "route-policy-out", "process prefixes in egress updates");
        l.add(null, false, 5, new int[]{-1}, "<name:rpl>", "name of route policy");
        l.add(null, false, 4, new int[]{5}, "prefix-list-in", "filter prefixes in ingress updates");
        l.add(null, false, 5, new int[]{-1}, "<name:pl>", "name of prefix list");
        l.add(null, false, 4, new int[]{5}, "prefix-list-out", "filter prefixes in egress updates");
        l.add(null, false, 5, new int[]{-1}, "<name:pl>", "name of prefix list");
        secInfoUtl.getHelp(l, 4, "ipinfo", "check peers");
    }

    /**
     * close all neighbors
     */
    protected void closeNeighbors() {
        for (int i = neighs.size(); i >= 0; i--) {
            rtrEigrpNeigh ntry = neighs.get(i);
            if (ntry == null) {
                continue;
            }
            ntry.stopWork();
        }
    }

    /**
     * setup timer thread
     *
     * @param shutdown set true to shut down
     */
    public void restartTimer(boolean shutdown) {
        keepTimer = null;
        if (shutdown) {
            return;
        }
        keepTimer = new rtrEigrpIfaceHello(this);
        keepTimer.start();
    }

    /**
     * do one config
     *
     * @param a command
     * @param cmd parameters
     */
    public void routerDoConfig(String a, cmds cmd) {
        if (a.equals("ipinfo")) {
            ipInfoCfg = secInfoUtl.doCfgStr(ipInfoCfg, cmd, false);
            return;
        }
        if (a.equals("bfd")) {
            bfdTrigger = true;
            return;
        }
        if (a.equals("default-originate")) {
            defOrigin = true;
            lower.notif.wakeup();
            return;
        }
        if (a.equals("split-horizon")) {
            splitHorizon = true;
            lower.notif.wakeup();
            return;
        }
        if (a.equals("suppress-prefix")) {
            suppressAddr = true;
            lower.notif.wakeup();
            return;
        }
        if (a.equals("unsuppress-prefix")) {
            unsuppressAddr = true;
            lower.notif.wakeup();
            return;
        }
        if (a.equals("verify-source")) {
            connectedCheck = true;
            return;
        }
        if (a.equals("passive")) {
            passiveInt = true;
            return;
        }
        if (a.equals("hello-time")) {
            helloTimer = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("dead-time")) {
            deadTimer = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("delay-in")) {
            delayIn = bits.str2num(cmd.word());
            lower.notif.wakeup();
            return;
        }
        if (a.equals("delay-out")) {
            delayOut = bits.str2num(cmd.word());
            lower.notif.wakeup();
            return;
        }
        if (a.equals("distance")) {
            distance = bits.str2num(cmd.word());
            lower.notif.wakeup();
            return;
        }
        if (a.equals("prefix-list-in")) {
            cfgPrfxlst ntry = cfgAll.prfxFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such prefix list");
                return;
            }
            prflstIn = ntry.prflst;
            lower.notif.wakeup();
            return;
        }
        if (a.equals("prefix-list-out")) {
            cfgPrfxlst ntry = cfgAll.prfxFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such prefix list");
                return;
            }
            prflstOut = ntry.prflst;
            lower.notif.wakeup();
            return;
        }
        if (a.equals("route-map-in")) {
            cfgRoump ntry = cfgAll.rtmpFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such route map");
                return;
            }
            roumapIn = ntry.roumap;
            lower.notif.wakeup();
            return;
        }
        if (a.equals("route-map-out")) {
            cfgRoump ntry = cfgAll.rtmpFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such route map");
                return;
            }
            roumapOut = ntry.roumap;
            lower.notif.wakeup();
            return;
        }
        if (a.equals("route-policy-in")) {
            cfgRouplc ntry = cfgAll.rtplFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such route policy");
                return;
            }
            roupolIn = ntry.rouplc;
            lower.notif.wakeup();
            return;
        }
        if (a.equals("route-policy-out")) {
            cfgRouplc ntry = cfgAll.rtplFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such route policy");
                return;
            }
            roupolOut = ntry.rouplc;
            lower.notif.wakeup();
            return;
        }
        cmd.badCmd();
    }

    /**
     * undo one config
     *
     * @param a command
     * @param cmd parameters
     */
    public void routerUnConfig(String a, cmds cmd) {
        if (a.equals("ipinfo")) {
            ipInfoCfg = secInfoUtl.doCfgStr(ipInfoCfg, cmd, true);
            return;
        }
        if (a.equals("bfd")) {
            bfdTrigger = false;
            return;
        }
        if (a.equals("default-originate")) {
            defOrigin = false;
            lower.notif.wakeup();
            return;
        }
        if (a.equals("split-horizon")) {
            splitHorizon = false;
            lower.notif.wakeup();
            return;
        }
        if (a.equals("suppress-prefix")) {
            suppressAddr = false;
            lower.notif.wakeup();
            return;
        }
        if (a.equals("unsuppress-prefix")) {
            unsuppressAddr = false;
            lower.notif.wakeup();
            return;
        }
        if (a.equals("verify-source")) {
            connectedCheck = false;
            return;
        }
        if (a.equals("passive")) {
            passiveInt = false;
            return;
        }
        if (a.equals("prefix-list-in")) {
            prflstIn = null;
            lower.notif.wakeup();
            return;
        }
        if (a.equals("prefix-list-out")) {
            prflstOut = null;
            lower.notif.wakeup();
            return;
        }
        if (a.equals("route-map-in")) {
            roumapIn = null;
            lower.notif.wakeup();
            return;
        }
        if (a.equals("route-map-out")) {
            roumapOut = null;
            lower.notif.wakeup();
            return;
        }
        if (a.equals("route-policy-in")) {
            roupolIn = null;
            lower.notif.wakeup();
            return;
        }
        if (a.equals("route-policy-out")) {
            roupolOut = null;
            lower.notif.wakeup();
            return;
        }
        cmd.badCmd();
    }

    public String toString() {
        return "eigrp on " + iface;
    }

    public int compareTo(rtrEigrpIface o) {
        if (iface.ifwNum < o.iface.ifwNum) {
            return -1;
        }
        if (iface.ifwNum > o.iface.ifwNum) {
            return +1;
        }
        return 0;
    }

    /**
     * get protocol number
     *
     * @return number
     */
    public int getProtoNum() {
        return rtrEigrp.protoNum;
    }

    /**
     * get counter
     *
     * @return counter
     */
    public counter getCounter() {
        return cntr;
    }

    /**
     * close interface
     *
     * @param iface interface
     */
    public void closeUp(ipFwdIface iface) {
        restartTimer(true);
        unregister2ip();
        closeNeighbors();
    }

    /**
     * set state
     *
     * @param iface interface
     * @param stat state
     */
    public void setState(ipFwdIface iface, states stat) {
        if (stat == state.states.up) {
            return;
        }
        closeNeighbors();
    }

    /**
     * received packet
     *
     * @param rxIfc interface
     * @param pck packet
     */
    public void recvPack(ipFwdIface rxIfc, packHolder pck) {
        cntr.rx(pck);
        if (passiveInt) {
            cntr.drop(pck, reasons.notUp);
            return;
        }
        if (pck.IPsrc.isEmpty()) {
            cntr.drop(pck, reasons.badNet);
            return;
        }
        if ((connectedCheck) && (!rxIfc.network.matches(pck.IPsrc))) {
            logger.info("got from out of subnet peer " + pck.IPsrc);
            return;
        }
        rtrEigrpNeigh nei = new rtrEigrpNeigh(lower, this, pck.IPsrc);
        rtrEigrpNeigh old = neighs.add(nei);
        boolean sndHll = false;
        if (old != null) {
            nei = old;
        } else {
            nei.startWork();
            sndHll = true;
        }
        try {
            nei.recvPack(pck);
            if (sndHll) {
                sendHello();
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

    /**
     * alert packet
     *
     * @param rxIfc interface
     * @param pck packet
     * @return false if success, true if error
     */
    public boolean alertPack(ipFwdIface rxIfc, packHolder pck) {
        return true;
    }

    /**
     * error packet
     *
     * @param err error code
     * @param rtr address
     * @param rxIfc interface
     * @param pck packet
     */
    public void errorPack(reasons err, addrIP rtr, ipFwdIface rxIfc, packHolder pck) {
    }

    /**
     * make packet header
     *
     * @param pck packet to update
     * @param opc opcode
     * @param flg flags
     * @param seq sequence
     * @param ack acknowledge
     */
    protected void makeHead(packHolder pck, int opc, int flg, int seq, int ack) {
        pck.merge2beg();
        if (debugger.rtrEigrpTraf) {
            logger.debug("sending op=" + rtrEigrpNeigh.opcode2string(opc) + " flg=" + flg + " ack=" + ack + " seq=" + seq + " on " + iface);
        }
        pck.putByte(0, rtrEigrp.verNum); // version
        pck.putByte(1, opc); // opcode
        pck.msbPutW(2, 0); // checksum
        pck.msbPutD(4, flg); // flags
        pck.msbPutD(8, seq); // sequence
        pck.msbPutD(12, ack); // acknowledge
        pck.msbPutD(16, lower.as); // as
        int i = pck.putIPsum(0, rtrEigrp.sizeHead, 0);
        i = pck.getIPsum(0, pck.dataSize(), i);
        pck.lsbPutW(2, 0xffff - i); // checksum
        pck.putSkip(rtrEigrp.sizeHead);
        pck.merge2beg();
    }

    /**
     * send one packet on this interface
     *
     * @param pck packet to send
     */
    protected void packSend(packHolder pck) {
        pck.IPdf = false;
        pck.IPfrg = 0;
        pck.IPalrt = -1;
        pck.IPttl = 255;
        pck.IPtos = 0;
        pck.IPid = 0;
        pck.IPprt = rtrEigrp.protoNum;
        pck.IPsrc.setAddr(iface.addr);
        if (iface.addr.isIPv4()) {
            pck.IPtrg.fromString("224.0.0.10");
        } else {
            pck.IPtrg.fromString("ff02::a");
        }
        lower.fwdCore.protoPack(iface, null, pck);
    }

    /**
     * send hello packet
     */
    protected void sendHello() {
        if (passiveInt) {
            return;
        }
        packHolder pck = new packHolder(true, true);
        encTlv tlv = rtrEigrp.getTlv();
        tlv.valDat[0] = (byte) lower.k1;
        tlv.valDat[1] = (byte) lower.k2;
        tlv.valDat[2] = (byte) lower.k3;
        tlv.valDat[3] = (byte) lower.k4;
        tlv.valDat[4] = (byte) lower.k5;
        tlv.valDat[5] = 0;
        bits.msbPutW(tlv.valDat, 6, deadTimer / 1000);
        tlv.putBytes(pck, 1, 8, tlv.valDat); // parameters
        tlv.valDat[0] = 12;
        tlv.valDat[1] = 1;
        tlv.valDat[2] = 1;
        tlv.valDat[3] = 0;
        tlv.putBytes(pck, 4, 4, tlv.valDat); // versions
        if (lower.stub != 0) {
            bits.msbPutW(tlv.valDat, 0, lower.stub);
            tlv.putBytes(pck, 6, 2, tlv.valDat); // stub
        }
        makeHead(pck, rtrEigrpNeigh.opcHello, 0, 0, 0);
        packSend(pck);
    }

    /**
     * purge neighbors
     */
    protected void purgeNeighs() {
        long tim = bits.getTime();
        for (int i = neighs.size() - 1; i >= 0; i--) {
            rtrEigrpNeigh nei = neighs.get(i);
            if (nei == null) {
                continue;
            }
            if ((tim - nei.lastHeard) < deadTimer) {
                continue;
            }
            nei.stopWork();
        }
    }

}

class rtrEigrpIfaceHello implements Runnable {

    private final rtrEigrpIface lower;

    public rtrEigrpIfaceHello(rtrEigrpIface parent) {
        lower = parent;
    }

    public void start() {
        new Thread(this).start();
    }

    public void run() {
        try {
            for (;;) {
                if (lower.keepTimer != this) {
                    break;
                }
                lower.sendHello();
                lower.purgeNeighs();
                bits.sleep(lower.helloTimer);
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}
