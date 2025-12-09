package org.freertr.user;

import java.io.File;
import java.io.RandomAccessFile;
import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrMac;
import org.freertr.cfg.cfgAceslst;
import org.freertr.cfg.cfgAlias;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgBrdg;
import org.freertr.cfg.cfgDial;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgInit;
import org.freertr.cfg.cfgLin;
import org.freertr.cfg.cfgObjnet;
import org.freertr.cfg.cfgObjprt;
import org.freertr.cfg.cfgPrcss;
import org.freertr.cfg.cfgPrfxlst;
import org.freertr.cfg.cfgRoump;
import org.freertr.cfg.cfgRouplc;
import org.freertr.cfg.cfgRtr;
import org.freertr.cfg.cfgSched;
import org.freertr.cfg.cfgScrpt;
import org.freertr.cfg.cfgSensor;
import org.freertr.cfg.cfgTlmtry;
import org.freertr.cfg.cfgTrack;
import org.freertr.cfg.cfgVdc;
import org.freertr.cfg.cfgVpdn;
import org.freertr.cfg.cfgVrf;
import org.freertr.clnt.clntDns;
import org.freertr.clnt.clntSmtp;
import org.freertr.clnt.clntWhois;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipFwdTab;
import org.freertr.pack.packHolder;
import org.freertr.prt.prtGen;
import org.freertr.prt.prtWatch;
import org.freertr.rtr.rtrBabelNeigh;
import org.freertr.rtr.rtrBfdNeigh;
import org.freertr.rtr.rtrBgpNeigh;
import org.freertr.rtr.rtrBgpParam;
import org.freertr.rtr.rtrEigrpNeigh;
import org.freertr.rtr.rtrIsisNeigh;
import org.freertr.rtr.rtrLdpNeigh;
import org.freertr.rtr.rtrLsrpNeigh;
import org.freertr.rtr.rtrMsdpNeigh;
import org.freertr.rtr.rtrOlsrNeigh;
import org.freertr.rtr.rtrOspf4neigh;
import org.freertr.rtr.rtrOspf6neigh;
import org.freertr.rtr.rtrPvrpNeigh;
import org.freertr.rtr.rtrRip4neigh;
import org.freertr.rtr.rtrRip6neigh;
import org.freertr.serv.servBmp2mrt;
import org.freertr.serv.servDhcp4;
import org.freertr.serv.servDhcp6;
import org.freertr.serv.servP4lang;
import org.freertr.tab.tabRouteAttr;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.enc.encXml;
import org.freertr.prt.prtRedun;
import org.freertr.rtr.rtrBgp;
import org.freertr.rtr.rtrBgpMrt;
import org.freertr.rtr.rtrBgpSpeak;
import org.freertr.rtr.rtrBgpUtil;
import org.freertr.rtr.rtrRiftIface;
import org.freertr.rtr.rtrRpkiNeigh;
import org.freertr.serv.servAmt;
import org.freertr.serv.servCapwap;
import org.freertr.serv.servEoIp;
import org.freertr.serv.servErspan;
import org.freertr.serv.servEtherIp;
import org.freertr.serv.servGre;
import org.freertr.serv.servGtp;
import org.freertr.serv.servL2f;
import org.freertr.serv.servL2tp2;
import org.freertr.serv.servL2tp3;
import org.freertr.serv.servLwapp;
import org.freertr.serv.servPckOudp;
import org.freertr.serv.servSdwan;
import org.freertr.serv.servSrEth;
import org.freertr.serv.servVxlan;
import org.freertr.util.logger;

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
    public userRead rdr;

    private void doOneCrashFile(File fl) {
        String a = fl.getName();
        if (a.endsWith(userUpgrade.bakExt)) {
            return;
        }
        boolean need = a.startsWith("core");
        need |= a.startsWith("hs_err");
        if (!need) {
            return;
        }
        a = fl.getAbsolutePath();
        userFlash.rename(a, a + userUpgrade.bakExt, true, true);
        bits.buf2txt(false, bits.str2lst("core dump detected at " + a), cfgInit.myErrorFile());
    }

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
        if (a.equals("flash")) {
            a = cmd.getRemaining();
            List<String> lst = userFlash.cleanBackups(a);
            rdr.putStrArr(lst);
            return null;
        }
        if (a.equals("upgrade")) {
            a = userUpgrade.stopReverter();
            cmd.error(a);
            return null;
        }
        if (a.equals("reload")) {
            a = "reload code cleared";
            logger.info(a);
            cfgInit.stopRouter(true, -16, a);
            return null;
        }
        if (a.equals("redundancy")) {
            a = cmd.word();
            if (a.equals("core")) {
                cmd.error(cmds.doneFail(prtRedun.doCore(cmd.word())));
                return null;
            }
            if (a.equals("config")) {
                cmd.error(cmds.doneFail(prtRedun.doConfig(cmd.word())));
                return null;
            }
            if (a.equals("state")) {
                cfgInit.stateSave();
                return null;
            }
            if (a.equals("local")) {
                prtRedun.setPrio(bits.str2num(cmd.word()));
                return null;
            }
            if (a.equals("peer")) {
                a = cmd.word();
                cmd.error(cmds.doneFail(prtRedun.setPrio(a, bits.str2num(cmd.word()))));
                return null;
            }
            cmd.badCmd();
            return null;
        }
        if (a.equals("errors")) {
            File[] fls = userFlash.dirList(cfgInit.getRWpath());
            if (fls != null) {
                for (int i = 0; i < fls.length; i++) {
                    doOneCrashFile(fls[i]);
                }
            }
            List<String> err = bits.txt2buf(cfgInit.myErrorFile());
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
            userFlash.delete(cfgInit.myErrorFile());
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
        if (a.equals("p4lang")) {
            servP4lang srv = cfgAll.srvrFind(new servP4lang(), cfgAll.dmnP4lang, cmd.word());
            if (srv == null) {
                cmd.error("no such server");
                return null;
            }
            srv.doClear();
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
        if (a.equals("p2poe")) {
            a = cmd.word();
            cfgIfc ifc = cfgAll.ifcFind(a, 0);
            if (ifc == null) {
                cmd.error("no such interface");
                return null;
            }
            if (ifc.pppoeS == null) {
                cmd.error("protocol not enabled");
                return null;
            }
            addrMac adr = new addrMac();
            if (adr.fromString(cmd.word())) {
                cmd.error("bad address");
                return null;
            }
            ifc.pppoeS.doClear(adr);
            return null;
        }
        if (a.equals("amt")) {
            servAmt srv = cfgAll.srvrFind(new servAmt(), cfgAll.dmnAmt, cmd.word());
            if (srv == null) {
                cmd.error("no such server");
                return null;
            }
            addrIP adr = new addrIP();
            if (adr.fromString(cmd.word())) {
                cmd.error("bad address");
                return null;
            }
            srv.doClear(adr);
            return null;
        }
        if (a.equals("erspan")) {
            servErspan srv = cfgAll.srvrFind(new servErspan(), cfgAll.dmnErspan, cmd.word());
            if (srv == null) {
                cmd.error("no such server");
                return null;
            }
            addrIP adr = new addrIP();
            if (adr.fromString(cmd.word())) {
                cmd.error("bad address");
                return null;
            }
            srv.doClear(adr);
            return null;
        }
        if (a.equals("sreth")) {
            servSrEth srv = cfgAll.srvrFind(new servSrEth(), cfgAll.dmnSrEth, cmd.word());
            if (srv == null) {
                cmd.error("no such server");
                return null;
            }
            addrIP adr = new addrIP();
            if (adr.fromString(cmd.word())) {
                cmd.error("bad address");
                return null;
            }
            srv.doClear(adr);
            return null;
        }
        if (a.equals("etherip")) {
            servEtherIp srv = cfgAll.srvrFind(new servEtherIp(), cfgAll.dmnEtherIp, cmd.word());
            if (srv == null) {
                cmd.error("no such server");
                return null;
            }
            addrIP adr = new addrIP();
            if (adr.fromString(cmd.word())) {
                cmd.error("bad address");
                return null;
            }
            srv.doClear(adr);
            return null;
        }
        if (a.equals("eoip")) {
            servEoIp srv = cfgAll.srvrFind(new servEoIp(), cfgAll.dmnEoIp, cmd.word());
            if (srv == null) {
                cmd.error("no such server");
                return null;
            }
            addrIP adr = new addrIP();
            if (adr.fromString(cmd.word())) {
                cmd.error("bad address");
                return null;
            }
            srv.doClear(adr);
            return null;
        }
        if (a.equals("gre")) {
            servGre srv = cfgAll.srvrFind(new servGre(), cfgAll.dmnGre, cmd.word());
            if (srv == null) {
                cmd.error("no such server");
                return null;
            }
            addrIP adr = new addrIP();
            if (adr.fromString(cmd.word())) {
                cmd.error("bad address");
                return null;
            }
            srv.doClear(adr);
            return null;
        }
        if (a.equals("gtp")) {
            servGtp srv = cfgAll.srvrFind(new servGtp(), cfgAll.dmnGtp, cmd.word());
            if (srv == null) {
                cmd.error("no such server");
                return null;
            }
            addrIP adr = new addrIP();
            if (adr.fromString(cmd.word())) {
                cmd.error("bad address");
                return null;
            }
            srv.doClear(adr);
            return null;
        }
        if (a.equals("l2f")) {
            servL2f srv = cfgAll.srvrFind(new servL2f(), cfgAll.dmnL2f, cmd.word());
            if (srv == null) {
                cmd.error("no such server");
                return null;
            }
            addrIP adr = new addrIP();
            if (adr.fromString(cmd.word())) {
                cmd.error("bad address");
                return null;
            }
            srv.doClear(adr);
            return null;
        }
        if (a.equals("l2tp2")) {
            servL2tp2 srv = cfgAll.srvrFind(new servL2tp2(), cfgAll.dmnL2tp2, cmd.word());
            if (srv == null) {
                cmd.error("no such server");
                return null;
            }
            addrIP adr = new addrIP();
            if (adr.fromString(cmd.word())) {
                cmd.error("bad address");
                return null;
            }
            srv.doClear(adr);
            return null;
        }
        if (a.equals("l2tp3")) {
            servL2tp3 srv = cfgAll.srvrFind(new servL2tp3(), cfgAll.dmnL2tp3, cmd.word());
            if (srv == null) {
                cmd.error("no such server");
                return null;
            }
            addrIP adr = new addrIP();
            if (adr.fromString(cmd.word())) {
                cmd.error("bad address");
                return null;
            }
            srv.doClear(adr);
            return null;
        }
        if (a.equals("pckoudp")) {
            servPckOudp srv = cfgAll.srvrFind(new servPckOudp(), cfgAll.dmnPckOudp, cmd.word());
            if (srv == null) {
                cmd.error("no such server");
                return null;
            }
            addrIP adr = new addrIP();
            if (adr.fromString(cmd.word())) {
                cmd.error("bad address");
                return null;
            }
            srv.doClear(adr);
            return null;
        }
        if (a.equals("capwap")) {
            servCapwap srv = cfgAll.srvrFind(new servCapwap(), cfgAll.dmnCapwap, cmd.word());
            if (srv == null) {
                cmd.error("no such server");
                return null;
            }
            addrIP adr = new addrIP();
            if (adr.fromString(cmd.word())) {
                cmd.error("bad address");
                return null;
            }
            srv.doClear(adr);
            return null;
        }
        if (a.equals("lwapp")) {
            servLwapp srv = cfgAll.srvrFind(new servLwapp(), cfgAll.dmnLwapp, cmd.word());
            if (srv == null) {
                cmd.error("no such server");
                return null;
            }
            addrIP adr = new addrIP();
            if (adr.fromString(cmd.word())) {
                cmd.error("bad address");
                return null;
            }
            srv.doClear(adr);
            return null;
        }
        if (a.equals("vxlan")) {
            servVxlan srv = cfgAll.srvrFind(new servVxlan(), cfgAll.dmnVxlan, cmd.word());
            if (srv == null) {
                cmd.error("no such server");
                return null;
            }
            addrIP adr = new addrIP();
            if (adr.fromString(cmd.word())) {
                cmd.error("bad address");
                return null;
            }
            srv.doClear(adr);
            return null;
        }
        if (a.equals("sdwan")) {
            servSdwan srv = cfgAll.srvrFind(new servSdwan(), cfgAll.dmnSdwan, cmd.word());
            if (srv == null) {
                cmd.error("no such server");
                return null;
            }
            addrIP adr = new addrIP();
            if (adr.fromString(cmd.word())) {
                cmd.error("bad address");
                return null;
            }
            srv.doClear(adr);
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
            a = cmd.word();
            if (a.equals("start")) {
                ntry.startNow();
            }
            if (a.equals("stop")) {
                ntry.stopNow();
            }
            ntry.restartNow();
            return null;
        }
        if (a.equals("tracker")) {
            cfgTrack ntry = cfgAll.trackFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such tracker");
                return null;
            }
            a = cmd.word();
            if (a.equals("start")) {
                ntry.worker.startNow();
            }
            if (a.equals("stop")) {
                ntry.worker.stopNow();
            }
            if (a.equals("")) {
                ntry.worker.doRound();
            }
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
            cfgIfc ifc = cfgAll.ifcFind(a, 0);
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
            cfgIfc ifc = cfgAll.ifcFind(a, 0);
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
            cfgIfc ifc = cfgAll.ifcFind(a, 0);
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
            cfgIfc ifc = cfgAll.ifcFind(a, 0);
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
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
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
        if (a.equals("sensor")) {
            cfgSensor exp = cfgAll.sensorFind(cmd.word(), false);
            if (exp == null) {
                cmd.error("no such sensor");
                return null;
            }
            a = cmd.word();
            if (a.equals("append-csv")) {
                bits.buf2txt(false, exp.getReportCsv(), cmd.getRemaining());
                return null;
            }
            if (a.equals("csv")) {
                bits.buf2txt(true, exp.getReportCsv(), cmd.getRemaining());
                return null;
            }
            if (a.equals("prometheus")) {
                bits.buf2txt(true, exp.getReportProm(), cmd.getRemaining());
                return null;
            }
            if (a.equals("xml")) {
                encXml xml = new encXml();
                exp.getReportNetConf(xml, "/");
                bits.buf2txt(true, xml.toXMLlst(), cmd.getRemaining());
                return null;
            }
            cmd.error("invalid format");
            return null;
        }
        if (a.equals("telemetry")) {
            cfgSensor exp = cfgAll.sensorFind(cmd.word(), false);
            if (exp == null) {
                cmd.error("no such sensor");
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
            a = cmd.word();
            if (a.equals("start")) {
                sch.startNow();
            }
            if (a.equals("stop")) {
                sch.stopNow();
            }
            if (a.equals("")) {
                sch.doRound();
            }
            return null;
        }
        if (a.equals("script")) {
            cfgScrpt sch = cfgAll.scrptFind(cmd.word(), false);
            if (sch == null) {
                cmd.error("no such script");
                return null;
            }
            a = cmd.word();
            if (a.equals("start")) {
                sch.startNow();
            }
            if (a.equals("stop")) {
                sch.stopNow();
            }
            if (a.equals("")) {
                sch.doRound(null);
            }
            return null;
        }
        if (a.equals("name-cache")) {
            clntDns.purgeLocalCache(true);
            return null;
        }
        if (a.equals("asn-cache")) {
            clntWhois.purgeLocalCache();
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
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
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
        if (a.equals("savemrt")) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                cmd.error("no such vrf");
                return null;
            }
            a = cmd.word();
            cmd.error("opening " + a);
            RandomAccessFile fs = null;
            try {
                fs = new RandomAccessFile(new File(a), "rw");
                fs.setLength(0);
            } catch (Exception e) {
                return null;
            }
            rtrBgp bgp = new rtrBgp(vrf.fwd4, vrf, null, 0);
            rtrBgpNeigh nei = new rtrBgpNeigh(bgp, new addrIP());
            rtrBgpSpeak spk = new rtrBgpSpeak(bgp, nei, null, 0);
            rtrBgpMrt.dumpTable(fs, spk, rtrBgpUtil.safiIp4uni, vrf.fwd4.actualU, false, 4, 0, 0, new addrIP(), new addrIP());
            rtrBgpMrt.dumpTable(fs, spk, rtrBgpUtil.safiIp6uni, vrf.fwd6.actualU, false, 6, 0, 0, new addrIP(), new addrIP());
            try {
                fs.close();
            } catch (Exception e) {
            }
            return null;
        }
        if (a.equals("ipv4")) {
            a = cmd.word();
            if (a.equals("arp")) {
                cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
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
                vrf.fwd4.routerConfigChg();
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
            if (a.equals("savemrt")) {
                cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
                if (vrf == null) {
                    cmd.error("no such vrf");
                    return null;
                }
                a = cmd.word();
                cmd.error("opening " + a);
                RandomAccessFile fs = null;
                try {
                    fs = new RandomAccessFile(new File(a), "rw");
                    fs.setLength(0);
                } catch (Exception e) {
                    return null;
                }
                rtrBgp bgp = new rtrBgp(vrf.fwd4, vrf, null, 0);
                rtrBgpNeigh nei = new rtrBgpNeigh(bgp, new addrIP());
                rtrBgpSpeak spk = new rtrBgpSpeak(bgp, nei, null, 0);
                rtrBgpMrt.dumpTable(fs, spk, rtrBgpUtil.safiIp4uni, vrf.fwd4.actualU, false, 4, 0, 0, new addrIP(), new addrIP());
                try {
                    fs.close();
                } catch (Exception e) {
                }
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
            if (a.equals("rpki")) {
                doClearIpXrpki(tabRouteAttr.routeType.rpki4);
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
            if (a.equals("rift")) {
                doClearIpXrift(tabRouteAttr.routeType.rift4);
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
                doClearIpXlogger(tabRouteAttr.routeType.logger4);
                return null;
            }
            if (a.equals("ghosthunt")) {
                doClearIpXghosthunt(tabRouteAttr.routeType.ghosthunt4);
                return null;
            }
            cmd.badCmd();
            return null;
        }
        if (a.equals("ipv6")) {
            a = cmd.word();
            if (a.equals("neighbor")) {
                cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
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
                vrf.fwd6.routerConfigChg();
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
            if (a.equals("savemrt")) {
                cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
                if (vrf == null) {
                    cmd.error("no such vrf");
                    return null;
                }
                a = cmd.word();
                cmd.error("opening " + a);
                RandomAccessFile fs = null;
                try {
                    fs = new RandomAccessFile(new File(a), "rw");
                    fs.setLength(0);
                } catch (Exception e) {
                    return null;
                }
                rtrBgp bgp = new rtrBgp(vrf.fwd6, vrf, null, 0);
                rtrBgpNeigh nei = new rtrBgpNeigh(bgp, new addrIP());
                rtrBgpSpeak spk = new rtrBgpSpeak(bgp, nei, null, 0);
                rtrBgpMrt.dumpTable(fs, spk, rtrBgpUtil.safiIp6uni, vrf.fwd6.actualU, false, 6, 0, 0, new addrIP(), new addrIP());
                try {
                    fs.close();
                } catch (Exception e) {
                }
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
            if (a.equals("rpki")) {
                doClearIpXrpki(tabRouteAttr.routeType.rpki6);
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
            if (a.equals("rift")) {
                doClearIpXrift(tabRouteAttr.routeType.rift6);
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
                doClearIpXlogger(tabRouteAttr.routeType.logger6);
                return null;
            }
            if (a.equals("ghosthunt")) {
                doClearIpXghosthunt(tabRouteAttr.routeType.ghosthunt6);
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
        if (a.equals("statistics")) {
            r.bgp.doClearMsgs();
            return;
        }
        if (a.equals("attribs")) {
            r.bgp.doClearAttrs();
            return;
        }
        if (a.equals("tinys")) {
            r.bgp.doClearTinys();
            return;
        }
        if (a.equals("peaks")) {
            r.bgp.doClearPeaks();
            return;
        }
        if (a.equals("flaps")) {
            r.bgp.doClearFlaps();
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
        if (a.equals("save")) {
            int idx = rtrBgpParam.string2idx(cmd.word());
            if (idx < 0) {
                return;
            }
            a = cmd.word();
            cmd.error("opening " + a);
            RandomAccessFile fs = null;
            try {
                fs = new RandomAccessFile(new File(a), "rw");
                fs.setLength(0);
            } catch (Exception e) {
                return;
            }
            for (int i = 0; i < neis.size(); i++) {
                rtrBgpNeigh nei = neis.get(i);
                cmd.error("saving " + nei.peerAddr);
                nei.saveTable(fs, idx, r.bgp.idx2safi[idx]);
            }
            try {
                fs.close();
            } catch (Exception e) {
            }
            return;
        }
        int mod = 0;
        if (a.equals("in")) {
            mod = 1;
        }
        if (a.equals("out")) {
            mod = 2;
        }
        if (a.equals("add")) {
            mod = 3;
        }
        if (a.equals("del")) {
            mod = 4;
        }
        if (mod < 1) {
            return;
        }
        int idx = rtrBgpParam.string2idx(cmd.word());
        if (idx < 0) {
            return;
        }
        int sfi = r.bgp.idx2safi[idx];
        for (int i = 0; i < neis.size(); i++) {
            rtrBgpNeigh nei = neis.get(i);
            switch (mod) {
                case 1:
                    nei.conn.sendRefresh(idx, sfi);
                    break;
                case 2:
                    nei.conn.gotRefresh(idx, sfi);
                    break;
                case 3:
                    nei.conn.sendDynamicCapa(true, true, idx, sfi);
                    break;
                case 4:
                    nei.conn.sendDynamicCapa(true, false, idx, sfi);
                    break;
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
        rtrLdpNeigh nei = fwd.ldpNeighFind(adr, false);
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

    private void doClearIpXrpki(tabRouteAttr.routeType afi) {
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
        rtrRpkiNeigh nei = r.rpki.findPeer(adr);
        if (nei == null) {
            cmd.error("no such neighbor");
            return;
        }
        nei.flapNow();
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

    private void doClearIpXrift(tabRouteAttr.routeType afi) {
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
        rtrRiftIface nei = r.rift.findNeigh(adr);
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

    private void doClearIpXlogger(tabRouteAttr.routeType afi) {
        cfgRtr r = cfgAll.rtrFind(afi, bits.str2num(cmd.word()), false);
        if (r == null) {
            cmd.error("no such process");
            return;
        }
        r.logger.clearFlapstat();
    }

    private void doClearIpXghosthunt(tabRouteAttr.routeType afi) {
        cfgRtr r = cfgAll.rtrFind(afi, bits.str2num(cmd.word()), false);
        if (r == null) {
            cmd.error("no such process");
            return;
        }
        r.ghosthunt.setPaused(cmd.word().equals("stop"));
    }

}
