package net.freertr.user;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrMac;
import net.freertr.cfg.cfgAceslst;
import net.freertr.cfg.cfgAlias;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgBrdg;
import net.freertr.cfg.cfgDial;
import net.freertr.cfg.cfgIfc;
import net.freertr.cfg.cfgInit;
import net.freertr.cfg.cfgLin;
import net.freertr.cfg.cfgObjnet;
import net.freertr.cfg.cfgObjprt;
import net.freertr.cfg.cfgPrcss;
import net.freertr.cfg.cfgPrfxlst;
import net.freertr.cfg.cfgRoump;
import net.freertr.cfg.cfgRouplc;
import net.freertr.cfg.cfgRtr;
import net.freertr.cfg.cfgSched;
import net.freertr.cfg.cfgScrpt;
import net.freertr.cfg.cfgSensor;
import net.freertr.cfg.cfgTlmtry;
import net.freertr.cfg.cfgVdc;
import net.freertr.cfg.cfgVpdn;
import net.freertr.cfg.cfgVrf;
import net.freertr.clnt.clntDns;
import net.freertr.clnt.clntSmtp;
import net.freertr.ip.ipFwd;
import net.freertr.ip.ipFwdIface;
import net.freertr.ip.ipFwdTab;
import net.freertr.pack.packHolder;
import net.freertr.prt.prtGen;
import net.freertr.prt.prtWatch;
import net.freertr.rtr.rtrBabelNeigh;
import net.freertr.rtr.rtrBfdNeigh;
import net.freertr.rtr.rtrBgpNeigh;
import net.freertr.rtr.rtrBgpParam;
import net.freertr.rtr.rtrEigrpNeigh;
import net.freertr.rtr.rtrIsisNeigh;
import net.freertr.rtr.rtrLdpNeigh;
import net.freertr.rtr.rtrLsrpNeigh;
import net.freertr.rtr.rtrMsdpNeigh;
import net.freertr.rtr.rtrOlsrNeigh;
import net.freertr.rtr.rtrOspf4neigh;
import net.freertr.rtr.rtrOspf6neigh;
import net.freertr.rtr.rtrPvrpNeigh;
import net.freertr.rtr.rtrRip4neigh;
import net.freertr.rtr.rtrRip6neigh;
import net.freertr.serv.servBmp2mrt;
import net.freertr.tab.tabRouteAttr;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.logger;
import net.freertr.util.version;

/**
 * process clear commands
 *
 * @author matecsaba
 */
public class userClear {

    /**
     * create instance
     */
    public userClear() {
    }

    /**
     * command to use
     */
    public cmds cmd;

    /**
     * reader of user
     */
    public userReader rdr;

    /**
     * do the work
     *
     * @return command to execute, null if nothing
     */
    public cfgAlias doer() {
        String a = cmd.word();
        cfgAlias alias = cfgAll.aliasFind(a, cfgAlias.aliasType.clear, false);
        if (alias != null) {
            return alias;
        }
        if (a.equals("errors")) {
            a = version.myWorkDir() + "core";
            if (new File(a).exists()) {
                userFlash.rename(a, a + ".bak", true, true);
                bits.buf2txt(false, bits.str2lst("core dump detected"), version.myErrorFile());
            }
            List<String> err = bits.txt2buf(version.myErrorFile());
            if (err == null) {
                cmd.error("nothing to report");
                return null;
            }
            boolean res = true;
            for (;;) {
                a = cmd.word();
                if (a.length() < 1) {
                    break;
                }
                cmd.error("sending to " + a);
                clntSmtp sm = new clntSmtp(cmd.pipe);
                sm.rcpt = a;
                sm.putHead("errors@" + cfgAll.getFqdn(), a, "errors happened");
                sm.putText(err);
                sm.putFinish();
                res &= sm.doSend(1);
                sm.cleanUp();
            }
            if (res) {
                cmd.error("error sending mail");
                return null;
            }
            userFlash.delete(version.myErrorFile());
            cmd.error("errors mailed");
            return null;
        }
        if (a.equals("dial-peer")) {
            cfgDial ntry = cfgAll.dialFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such dial peer");
                return null;
            }
            ntry.stopCall(cmd.word());
            return null;
        }
        if (a.equals("bmp")) {
            servBmp2mrt srv = cfgAll.srvrFind(new servBmp2mrt(), cfgAll.dmnBmp, cmd.word());
            if (srv == null) {
                cmd.error("no such server");
                return null;
            }
            srv.doClear();
            return null;
        }
        if (a.equals("vdc")) {
            cfgVdc ntry = cfgInit.vdcLst.find(new cfgVdc(cmd.word()));
            if (ntry == null) {
                cmd.error("no such vdc");
                return null;
            }
            a = cmd.word();
            if (a.equals("start")) {
                ntry.setRespawn(true);
            }
            if (a.equals("stop")) {
                ntry.setRespawn(false);
            }
            ntry.restartNow();
            return null;
        }
        if (a.equals("process")) {
            cfgPrcss ntry = cfgAll.prcFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such process");
                return null;
            }
            ntry.restartNow();
            return null;
        }
        if (a.equals("bridge")) {
            cfgBrdg ntry = cfgAll.brdgFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such bridge");
                return null;
            }
            a = cmd.word();
            if (a.length() < 1) {
                ntry.bridgeHed.delMacs(null);
                return null;
            }
            cfgIfc ifc = cfgAll.ifcFind(a, false);
            if (ifc == null) {
                cmd.error("no such interface");
                return null;
            }
            if (ifc.bridgeIfc == null) {
                cmd.error("not a bridge interface");
                return null;
            }
            ntry.bridgeHed.delMacs(ifc.bridgeIfc);
            return null;
        }
        if (a.equals("object-group")) {
            a = cmd.word();
            if (a.equals("network")) {
                cfgObjnet ntry = cfgAll.objnetFind(cmd.word(), false);
                if (ntry == null) {
                    cmd.error("no such object group");
                    return null;
                }
                ntry.objgrp.counterClear();
                return null;
            }
            if (a.equals("port")) {
                cfgObjprt ntry = cfgAll.objprtFind(cmd.word(), false);
                if (ntry == null) {
                    cmd.error("no such object group");
                    return null;
                }
                ntry.objgrp.counterClear();
                return null;
            }
            cmd.badCmd();
            return null;
        }
        if (a.equals("reflected-acl")) {
            cfgAceslst ntry = cfgAll.aclsFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such list");
                return null;
            }
            int i = ntry.aceslst.purgeAged();
            cmd.error(i + " entries removed");
            return null;
        }
        if (a.equals("access-list")) {
            cfgAceslst ntry = cfgAll.aclsFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such list");
                return null;
            }
            ntry.aceslst.counterClear();
            return null;
        }
        if (a.equals("prefix-list")) {
            cfgPrfxlst ntry = cfgAll.prfxFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such list");
                return null;
            }
            ntry.prflst.counterClear();
            return null;
        }
        if (a.equals("route-map")) {
            cfgRoump ntry = cfgAll.rtmpFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such map");
                return null;
            }
            ntry.roumap.counterClear();
            return null;
        }
        if (a.equals("route-policy")) {
            cfgRouplc ntry = cfgAll.rtplFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such map");
                return null;
            }
            ntry.rouplc.counterClear();
            return null;
        }
        if (a.equals("counters")) {
            a = cmd.word();
            if (a.length() < 1) {
                logger.warn("counters cleared on all interfaces");
                cfgAll.moreInterfaces(6);
                return null;
            }
            cfgIfc ifc = cfgAll.ifcFind(a, false);
            if (ifc == null) {
                cmd.error("no such interface");
                return null;
            }
            logger.warn("counters cleared on " + ifc.name);
            ifc.ethtyp.clearSwCounter();
            ifc.ethtyp.clearHwCounter();
            return null;
        }
        if (a.equals("swcounters")) {
            a = cmd.word();
            if (a.length() < 1) {
                logger.warn("software counters cleared on all interfaces");
                cfgAll.moreInterfaces(2);
                return null;
            }
            cfgIfc ifc = cfgAll.ifcFind(a, false);
            if (ifc == null) {
                cmd.error("no such interface");
                return null;
            }
            logger.warn("software counters cleared on " + ifc.name);
            ifc.ethtyp.clearSwCounter();
            return null;
        }
        if (a.equals("hwcounters")) {
            a = cmd.word();
            if (a.length() < 1) {
                logger.warn("hardware counters cleared on all interfaces");
                cfgAll.moreInterfaces(5);
                return null;
            }
            cfgIfc ifc = cfgAll.ifcFind(a, false);
            if (ifc == null) {
                cmd.error("no such interface");
                return null;
            }
            logger.warn("hardware counters cleared on " + ifc.name);
            ifc.ethtyp.clearHwCounter();
            return null;
        }
        if (a.equals("socket")) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                cmd.error("no such vrf");
                return null;
            }
            String p = cmd.word();
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
            if (ifc == null) {
                cmd.error("no such interface");
                return null;
            }
            int lp = bits.str2num(cmd.word());
            int rp = bits.str2num(cmd.word());
            addrIP adr = new addrIP();
            if (adr.fromString(cmd.word())) {
                cmd.error("invalid address");
                return null;
            }
            ipFwdIface fifc = ifc.getFwdIfc(adr);
            if (fifc == null) {
                return null;
            }
            prtGen prt = null;
            if (p.equals("tcp")) {
                prt = vrf.getTcp(adr);
            }
            if (p.equals("udp")) {
                prt = vrf.getUdp(adr);
            }
            if (p.equals("ludp")) {
                prt = vrf.getLudp(adr);
            }
            if (p.equals("dccp")) {
                prt = vrf.getDccp(adr);
            }
            if (p.equals("sctp")) {
                prt = vrf.getSctp(adr);
            }
            if (prt == null) {
                cmd.error("invalid protocol");
                return null;
            }
            if (prt.connectStop(fifc, lp, adr, rp)) {
                cmd.error("no such socket");
                return null;
            }
            return null;
        }
        if (a.equals("telemetry")) {
            cfgSensor exp = cfgAll.sensorFind(cmd.word(), false);
            if (exp == null) {
                cmd.error("no such exporter");
                return null;
            }
            cmd.error("generating report");
            packHolder rep = exp.getReportKvGpb();
            if (rep == null) {
                logger.warn("sensor " + a + " returned nothing");
                return null;
            }
            for (;;) {
                a = cmd.word();
                if (a.length() < 1) {
                    break;
                }
                cmd.error("sending through " + a);
                cfgTlmtry dst = cfgAll.tlmdsFind(a, false);
                if (dst == null) {
                    logger.warn("telemetry destination " + a + " not found");
                    continue;
                }
                packHolder pck = new packHolder(true, true);
                pck.copyFrom(rep, true, true);
                dst.worker.sendReport(pck);
            }
            return null;
        }
        if (a.equals("tunnel-domain")) {
            cfgAll.moreInterfaces(1);
            return null;
        }
        if (a.equals("auto-bandwidth")) {
            cfgAll.moreInterfaces(3);
            return null;
        }
        if (a.equals("follow-tracker")) {
            cfgAll.moreInterfaces(4);
            return null;
        }
        if (a.equals("vpdn")) {
            cfgVpdn vpdn = cfgAll.vpdnFind(cmd.word(), false);
            if (vpdn == null) {
                cmd.error("no such vpdn");
                return null;
            }
            int i = bits.str2num(cmd.word());
            if (i < 1) {
                i = 1;
            }
            vpdn.doFlap(i);
            return null;
        }
        if (a.equals("scheduler")) {
            cfgSched sch = cfgAll.schedFind(cmd.word(), false);
            if (sch == null) {
                cmd.error("no such scheduler");
                return null;
            }
            sch.doRound();
            return null;
        }
        if (a.equals("script")) {
            cfgScrpt sch = cfgAll.scrptFind(cmd.word(), false);
            if (sch == null) {
                cmd.error("no such script");
                return null;
            }
            sch.doRound(null);
            return null;
        }
        if (a.equals("name-cache")) {
            clntDns.purgeLocalCache(true);
            return null;
        }
        if (a.equals("logging")) {
            logger.bufferClear();
            logger.error("log buffer cleared");
            return null;
        }
        if (a.equals("watchdog")) {
            prtWatch.doClear(cmd);
            return null;
        }
        if (a.equals("line")) {
            cfgLin lin = cfgAll.linFind(cmd.word());
            if (lin == null) {
                cmd.error("no such line");
                return null;
            }
            lin.runner.setDedi(lin.runner.getDedi());
            return null;
        }
        if (a.equals("interface")) {
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
            if (ifc == null) {
                cmd.error("no such interface");
                return null;
            }
            int i = bits.str2num(cmd.word());
            if (i < 1) {
                i = 1;
            }
            ifc.flapNow(i);
            return null;
        }
        if (a.equals("ipv4")) {
            a = cmd.word();
            if (a.equals("arp")) {
                cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
                if (ifc == null) {
                    cmd.error("no such interface");
                    return null;
                }
                addrIP adr = new addrIP();
                if (adr.fromString(cmd.word())) {
                    cmd.error("bad address");
                    return null;
                }
                if (ifc.ipIf4 == null) {
                    cmd.error("protocol not enabled");
                    return null;
                }
                ifc.ipIf4.updateL2info(2, new addrMac(), adr);
                return null;
            }
            if (a.equals("route")) {
                cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
                if (vrf == null) {
                    cmd.error("no such vrf");
                    return null;
                }
                vrf.fwd4.routerStaticChg();
                return null;
            }
            if (a.equals("nat")) {
                cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
                if (vrf == null) {
                    cmd.error("no such vrf");
                    return null;
                }
                vrf.fwd4.natTrns.clear();
                return null;
            }
            if (a.equals("bgp")) {
                doClearIpXbgp(tabRouteAttr.routeType.bgp4);
                return null;
            }
            if (a.equals("bfd")) {
                doClearIpXbfd(4);
                return null;
            }
            if (a.equals("babel")) {
                doClearIpXbabel(tabRouteAttr.routeType.babel4);
                return null;
            }
            if (a.equals("eigrp")) {
                doClearIpXeigrp(tabRouteAttr.routeType.eigrp4);
                return null;
            }
            if (a.equals("isis")) {
                doClearIpXisis(tabRouteAttr.routeType.isis4);
                return null;
            }
            if (a.equals("ldp")) {
                doClearIpXldp(4);
                return null;
            }
            if (a.equals("lsrp")) {
                doClearIpXlsrp(tabRouteAttr.routeType.lsrp4);
                return null;
            }
            if (a.equals("msdp")) {
                doClearIpXmsdp(tabRouteAttr.routeType.msdp4);
                return null;
            }
            if (a.equals("olsr")) {
                doClearIpXolsr(tabRouteAttr.routeType.olsr4);
                return null;
            }
            if (a.equals("ospf")) {
                doClearIpXospf4();
                return null;
            }
            if (a.equals("pvrp")) {
                doClearIpXpvrp(tabRouteAttr.routeType.pvrp4);
                return null;
            }
            if (a.equals("rip")) {
                doClearIpXrip4();
                return null;
            }
            if (a.equals("logger")) {
                doClearIpXlogger4();
                return null;
            }
            cmd.badCmd();
            return null;
        }
        if (a.equals("ipv6")) {
            a = cmd.word();
            if (a.equals("neighbor")) {
                cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
                if (ifc == null) {
                    cmd.error("no such interface");
                    return null;
                }
                addrIP adr = new addrIP();
                if (adr.fromString(cmd.word())) {
                    cmd.error("bad address");
                    return null;
                }
                if (ifc.ipIf6 == null) {
                    cmd.error("protocol not enabled");
                    return null;
                }
                ifc.ipIf6.updateL2info(2, new addrMac(), adr);
                return null;
            }
            if (a.equals("route")) {
                cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
                if (vrf == null) {
                    cmd.error("no such vrf");
                    return null;
                }
                vrf.fwd6.routerStaticChg();
                return null;
            }
            if (a.equals("nat")) {
                cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
                if (vrf == null) {
                    cmd.error("no such vrf");
                    return null;
                }
                vrf.fwd6.natTrns.clear();
                return null;
            }
            if (a.equals("bgp")) {
                doClearIpXbgp(tabRouteAttr.routeType.bgp6);
                return null;
            }
            if (a.equals("bfd")) {
                doClearIpXbfd(6);
                return null;
            }
            if (a.equals("babel")) {
                doClearIpXbabel(tabRouteAttr.routeType.babel6);
                return null;
            }
            if (a.equals("eigrp")) {
                doClearIpXeigrp(tabRouteAttr.routeType.eigrp6);
                return null;
            }
            if (a.equals("isis")) {
                doClearIpXisis(tabRouteAttr.routeType.isis6);
                return null;
            }
            if (a.equals("ldp")) {
                doClearIpXldp(6);
                return null;
            }
            if (a.equals("lsrp")) {
                doClearIpXlsrp(tabRouteAttr.routeType.lsrp6);
                return null;
            }
            if (a.equals("msdp")) {
                doClearIpXmsdp(tabRouteAttr.routeType.msdp6);
                return null;
            }
            if (a.equals("olsr")) {
                doClearIpXolsr(tabRouteAttr.routeType.olsr6);
                return null;
            }
            if (a.equals("ospf")) {
                doClearIpXospf6();
                return null;
            }
            if (a.equals("pvrp")) {
                doClearIpXpvrp(tabRouteAttr.routeType.pvrp6);
                return null;
            }
            if (a.equals("rip")) {
                doClearIpXrip6();
                return null;
            }
            if (a.equals("logger")) {
                doClearIpXlogger6();
                return null;
            }
            cmd.badCmd();
            return null;
        }
        cmd.badCmd();
        return null;
    }

    private void doClearIpXbgp(tabRouteAttr.routeType afi) {
        cfgRtr r = cfgAll.rtrFind(afi, bits.str2num(cmd.word()), false);
        if (r == null) {
            cmd.error("no such process");
            return;
        }
        String a = cmd.word();
        if (a.equals("flaps")) {
            r.bgp.flaps.clear();
            return;
        }
        if (a.equals("recompute")) {
            r.bgp.routerRedistChanged();
            return;
        }
        List<rtrBgpNeigh> neis = new ArrayList<rtrBgpNeigh>();
        if (a.equals("asn")) {
            neis = r.bgp.findPeers(1, cmd.word());
        }
        if (a.equals("peer")) {
            neis = r.bgp.findPeers(2, cmd.word());
        }
        if (a.equals("ibgp")) {
            neis = r.bgp.findPeers(3, "true");
        }
        if (a.equals("ebgp")) {
            neis = r.bgp.findPeers(3, "false");
        }
        if (a.equals("all")) {
            neis = r.bgp.findPeers(3, ".*");
        }
        if (neis.size() < 1) {
            cmd.error("no such neighbor");
            return;
        }
        a = cmd.word();
        if (a.equals("hard")) {
            for (int i = 0; i < neis.size(); i++) {
                neis.get(i).flapBgpConn();
            }
            return;
        }
        boolean in = a.equals("in");
        int sfi = rtrBgpParam.string2mask(cmd.word());
        if (sfi < 1) {
            return;
        }
        sfi = r.bgp.mask2safi(sfi);
        if (sfi < 1) {
            return;
        }
        for (int i = 0; i < neis.size(); i++) {
            rtrBgpNeigh nei = neis.get(i);
            if (in) {
                nei.conn.sendRefresh(sfi);
            } else {
                nei.conn.gotRefresh(sfi);
            }
        }
    }

    private void doClearIpXbfd(int afi) {
        cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
        if (vrf == null) {
            cmd.error("no such vrf");
            return;
        }
        ipFwd fwd;
        if (afi == 4) {
            fwd = vrf.fwd4;
        } else {
            fwd = vrf.fwd6;
        }
        addrIP adr = new addrIP();
        if (adr.fromString(cmd.word())) {
            cmd.error("bad address");
            return;
        }
        rtrBfdNeigh nei = ipFwdTab.bfdFindNeigh(fwd, adr);
        if (nei == null) {
            cmd.error("no such neighbor");
            return;
        }
        nei.stopNow();
    }

    private void doClearIpXbabel(tabRouteAttr.routeType afi) {
        cfgRtr r = cfgAll.rtrFind(afi, bits.str2num(cmd.word()), false);
        if (r == null) {
            cmd.error("no such process");
            return;
        }
        addrIP adr = new addrIP();
        if (adr.fromString(cmd.word())) {
            cmd.error("bad address");
            return;
        }
        rtrBabelNeigh nei = r.babel.findPeer(adr);
        if (nei == null) {
            cmd.error("no such neighbor");
            return;
        }
        nei.bfdPeerDown();
    }

    private void doClearIpXeigrp(tabRouteAttr.routeType afi) {
        cfgRtr r = cfgAll.rtrFind(afi, bits.str2num(cmd.word()), false);
        if (r == null) {
            cmd.error("no such process");
            return;
        }
        addrIP adr = new addrIP();
        if (adr.fromString(cmd.word())) {
            cmd.error("bad address");
            return;
        }
        rtrEigrpNeigh nei = r.eigrp.findNeigh(adr);
        if (nei == null) {
            cmd.error("no such neighbor");
            return;
        }
        nei.bfdPeerDown();
    }

    private void doClearIpXisis(tabRouteAttr.routeType afi) {
        cfgRtr r = cfgAll.rtrFind(afi, bits.str2num(cmd.word()), false);
        if (r == null) {
            cmd.error("no such process");
            return;
        }
        addrIP adr = new addrIP();
        if (adr.fromString(cmd.word())) {
            cmd.error("bad address");
            return;
        }
        rtrIsisNeigh nei = r.isis.findNeigh(adr, bits.str2num(cmd.word()));
        if (nei == null) {
            cmd.error("no such neighbor");
            return;
        }
        nei.bfdPeerDown();
    }

    private void doClearIpXldp(int afi) {
        cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
        if (vrf == null) {
            cmd.error("no such vrf");
            return;
        }
        ipFwd fwd;
        if (afi == 4) {
            fwd = vrf.fwd4;
        } else {
            fwd = vrf.fwd6;
        }
        addrIP adr = new addrIP();
        if (adr.fromString(cmd.word())) {
            cmd.error("bad address");
            return;
        }
        rtrLdpNeigh nei = fwd.ldpNeighFind(null, adr, false);
        if (nei == null) {
            cmd.error("no such neighbor");
            return;
        }
        nei.stopPeer();
    }

    private void doClearIpXlsrp(tabRouteAttr.routeType afi) {
        cfgRtr r = cfgAll.rtrFind(afi, bits.str2num(cmd.word()), false);
        if (r == null) {
            cmd.error("no such process");
            return;
        }
        addrIP adr = new addrIP();
        if (adr.fromString(cmd.word())) {
            cmd.error("bad address");
            return;
        }
        rtrLsrpNeigh nei = r.lsrp.findNeigh(adr);
        if (nei == null) {
            cmd.error("no such neighbor");
            return;
        }
        nei.bfdPeerDown();
    }

    private void doClearIpXmsdp(tabRouteAttr.routeType afi) {
        cfgRtr r = cfgAll.rtrFind(afi, bits.str2num(cmd.word()), false);
        if (r == null) {
            cmd.error("no such process");
            return;
        }
        addrIP adr = new addrIP();
        if (adr.fromString(cmd.word())) {
            cmd.error("bad address");
            return;
        }
        rtrMsdpNeigh nei = r.msdp.findPeer(adr);
        if (nei == null) {
            cmd.error("no such neighbor");
            return;
        }
        nei.bfdPeerDown();
    }

    private void doClearIpXolsr(tabRouteAttr.routeType afi) {
        cfgRtr r = cfgAll.rtrFind(afi, bits.str2num(cmd.word()), false);
        if (r == null) {
            cmd.error("no such process");
            return;
        }
        addrIP adr = new addrIP();
        if (adr.fromString(cmd.word())) {
            cmd.error("bad address");
            return;
        }
        rtrOlsrNeigh nei = r.olsr.findPeer(adr);
        if (nei == null) {
            cmd.error("no such neighbor");
            return;
        }
        nei.bfdPeerDown();
    }

    private void doClearIpXospf4() {
        cfgRtr r = cfgAll.rtrFind(tabRouteAttr.routeType.ospf4, bits.str2num(cmd.word()), false);
        if (r == null) {
            cmd.error("no such process");
            return;
        }
        int ara = bits.str2num(cmd.word());
        addrIP adr = new addrIP();
        if (adr.fromString(cmd.word())) {
            cmd.error("bad address");
            return;
        }
        rtrOspf4neigh nei = r.ospf4.findPeer(ara, adr);
        if (nei == null) {
            cmd.error("no such neighbor");
            return;
        }
        nei.bfdPeerDown();
    }

    private void doClearIpXospf6() {
        cfgRtr r = cfgAll.rtrFind(tabRouteAttr.routeType.ospf6, bits.str2num(cmd.word()), false);
        if (r == null) {
            cmd.error("no such process");
            return;
        }
        int ara = bits.str2num(cmd.word());
        addrIP adr = new addrIP();
        if (adr.fromString(cmd.word())) {
            cmd.error("bad address");
            return;
        }
        rtrOspf6neigh nei = r.ospf6.findPeer(ara, adr);
        if (nei == null) {
            cmd.error("no such neighbor");
            return;
        }
        nei.bfdPeerDown();
    }

    private void doClearIpXpvrp(tabRouteAttr.routeType afi) {
        cfgRtr r = cfgAll.rtrFind(afi, bits.str2num(cmd.word()), false);
        if (r == null) {
            cmd.error("no such process");
            return;
        }
        addrIP adr = new addrIP();
        if (adr.fromString(cmd.word())) {
            cmd.error("bad address");
            return;
        }
        rtrPvrpNeigh nei = r.pvrp.findNeigh(adr);
        if (nei == null) {
            cmd.error("no such neighbor");
            return;
        }
        nei.bfdPeerDown();
    }

    private void doClearIpXrip4() {
        cfgRtr r = cfgAll.rtrFind(tabRouteAttr.routeType.rip4, bits.str2num(cmd.word()), false);
        if (r == null) {
            cmd.error("no such process");
            return;
        }
        addrIP adr = new addrIP();
        if (adr.fromString(cmd.word())) {
            cmd.error("bad address");
            return;
        }
        rtrRip4neigh nei = r.rip4.findPeer(adr);
        if (nei == null) {
            cmd.error("no such neighbor");
            return;
        }
        nei.bfdPeerDown();
    }

    private void doClearIpXrip6() {
        cfgRtr r = cfgAll.rtrFind(tabRouteAttr.routeType.rip6, bits.str2num(cmd.word()), false);
        if (r == null) {
            cmd.error("no such process");
            return;
        }
        addrIP adr = new addrIP();
        if (adr.fromString(cmd.word())) {
            cmd.error("bad address");
            return;
        }
        rtrRip6neigh nei = r.rip6.findPeer(adr);
        if (nei == null) {
            cmd.error("no such neighbor");
            return;
        }
        nei.bfdPeerDown();
    }

    private void doClearIpXlogger4() {
        cfgRtr r = cfgAll.rtrFind(tabRouteAttr.routeType.logger4, bits.str2num(cmd.word()), false);
        if (r == null) {
            cmd.error("no such process");
            return;
        }
        r.logger.clearFlapstat();
    }

    private void doClearIpXlogger6() {
        cfgRtr r = cfgAll.rtrFind(tabRouteAttr.routeType.logger6, bits.str2num(cmd.word()), false);
        if (r == null) {
            cmd.error("no such process");
            return;
        }
        r.logger.clearFlapstat();
    }

}
