package user;

import addr.addrIP;
import addr.addrMac;
import cfg.cfgAlias;
import cfg.cfgAll;
import cfg.cfgDial;
import cfg.cfgIfc;
import cfg.cfgInit;
import cfg.cfgLin;
import cfg.cfgRtr;
import cfg.cfgSched;
import cfg.cfgScrpt;
import cfg.cfgVdc;
import cfg.cfgPrcss;
import cfg.cfgVpdn;
import cfg.cfgVrf;
import clnt.clntDns;
import ip.ipFwdIface;
import prt.prtGen;
import prt.prtWatch;
import rtr.rtrBgpNeigh;
import rtr.rtrBgpParam;
import tab.tabRouteEntry;
import serv.servBmp2mrt;
import util.bits;
import util.cmds;
import util.logger;

/**
 * process clear commands
 *
 * @author matecsaba
 */
public class userClear {

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
    public String doer() {
        String a = cmd.word();
        cfgAlias alias = cfgAll.aliasFind(a, cfgAlias.aliasType.clear, false);
        if (alias != null) {
            return alias.getCommand(cmd);
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
        if (a.equals("counters")) {
            a = cmd.word();
            if (a.length() < 1) {
                logger.warn("counters cleared on all interfaces");
                cfgAll.moreInterfaces(2);
                return null;
            }
            cfgIfc ifc = cfgAll.ifcFind(a, false);
            if (ifc == null) {
                cmd.error("no such interface");
                return null;
            }
            logger.warn("counters cleared on " + ifc.name);
            ifc.ethtyp.clearCounter();
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
        if (a.equals("tunnel-domain")) {
            cfgAll.moreInterfaces(1);
            return null;
        }
        if (a.equals("auto-bandwidth")) {
            cfgAll.moreInterfaces(3);
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
            sch.doRound();
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
            ifc.flapNow(i * 1000);
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
                doClearIpXbgp(tabRouteEntry.routeType.bgp4);
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
                doClearIpXbgp(tabRouteEntry.routeType.bgp6);
                return null;
            }
            cmd.badCmd();
            return null;
        }
        cmd.badCmd();
        return null;
    }

    private void doClearIpXbgp(tabRouteEntry.routeType afi) {
        cfgRtr r = cfgAll.rtrFind(afi, bits.str2num(cmd.word()), false);
        if (r == null) {
            cmd.error("no such process");
            return;
        }
        String a = cmd.word();
        if (a.equals("recompute")) {
            r.bgp.routerRedistChanged();
            return;
        }
        addrIP adr = new addrIP();
        adr.fromString(a);
        rtrBgpNeigh nei = r.bgp.findPeer(adr);
        if (nei == null) {
            cmd.error("no such neighbor");
            return;
        }
        a = cmd.word();
        if (a.equals("hard")) {
            nei.flapBgpConn();
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
        if (in) {
            nei.conn.sendRefresh(sfi);
        } else {
            nei.conn.gotRefresh(sfi);
        }
        return;
    }

}
