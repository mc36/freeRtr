package user;

import addr.addrIP;
import addr.addrIpx;
import addr.addrPrefix;
import cfg.cfgAceslst;
import cfg.cfgAlias;
import cfg.cfgAll;
import cfg.cfgBndl;
import cfg.cfgBrdg;
import cfg.cfgDial;
import cfg.cfgIfc;
import cfg.cfgInit;
import cfg.cfgMtrack;
import cfg.cfgObjnet;
import cfg.cfgObjprt;
import cfg.cfgPrfxlst;
import cfg.cfgRoump;
import cfg.cfgRouplc;
import cfg.cfgRtr;
import cfg.cfgTrack;
import cfg.cfgVdc;
import cfg.cfgVdcIfc;
import cfg.cfgPrcss;
import cfg.cfgSched;
import cfg.cfgScrpt;
import cfg.cfgVrf;
import clnt.clntDns;
import clnt.clntNetflow;
import clnt.clntWhois;
import ifc.ifcThread;
import ip.ipFwd;
import ip.ipFwdIface;
import ip.ipFwdMcast;
import ip.ipFwdMpmp;
import ip.ipFwdTab;
import ip.ipFwdTrfng;
import ip.ipMpls;
import ip.ipRtr;
import java.util.ArrayList;
import java.util.List;
import pack.packLdpMp;
import pack.packLdpPwe;
import pipe.pipeLine;
import pipe.pipeReader;
import pipe.pipeSide;
import prt.prtRedun;
import prt.prtWatch;
import rtr.rtrBabelNeigh;
import rtr.rtrBgpGroup;
import rtr.rtrBgpNeigh;
import rtr.rtrBgpParam;
import rtr.rtrEigrpNeigh;
import rtr.rtrLdpNeigh;
import rtr.rtrLogger;
import rtr.rtrOlsrNeigh;
import rtr.rtrPvrpNeigh;
import rtr.rtrRip4neigh;
import rtr.rtrRip6neigh;
import serv.servBmp2mrt;
import tab.tabGen;
import tab.tabIntMatcher;
import tab.tabLabel;
import tab.tabLabelNtry;
import tab.tabListing;
import tab.tabNshNtry;
import tab.tabPrfxlstN;
import tab.tabQos;
import tab.tabRoute;
import tab.tabRouteEntry;
import tab.tabRtrmapN;
import tab.tabSession;
import util.bits;
import util.cmds;
import util.counter;
import util.history;
import util.logger;
import util.verCore;
import util.version;

/**
 * process show commands
 *
 * @author matecsaba
 */
public class userShow {

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
        cfgAlias alias = cfgAll.aliasFind(a, cfgAlias.aliasType.show, false);
        if (alias != null) {
            return alias.getCommand(cmd);
        }
        if (a.equals("version")) {
            a = cmd.word();
            if (a.equals("number")) {
                rdr.putStrArr(version.shLogo(0x200));
                return null;
            }
            rdr.putStrArr(version.shLogo(0xe0));
            return null;
        }
        if (a.equals("me-the")) {
            a = cmd.word();
            if (a.equals("key")) {
                rdr.putStrArr(version.shSecret(1));
                return null;
            }
            if (a.equals("love")) {
                rdr.putStrArr(version.shSecret(2));
                return null;
            }
            if (a.equals("bug")) {
                rdr.putStrArr(version.shSecret(3));
                return null;
            }
            return null;
        }
        if (a.equals("banner")) {
            cmd.pipe.strPut(new String(cfgAll.banner));
            return null;
        }
        if (a.equals("logo")) {
            a = cmd.getRemaining();
            List<String> l;
            if (a.length() > 0) {
                l = userScreen.fontText(a, " ", userFonts1.fontFiller, userFonts1.fontDefault());
            } else {
                l = version.shLogo(0x0e);
            }
            rdr.putStrArr(l);
            return null;
        }
        if (a.equals("platform")) {
            rdr.putStrArr(version.shPlat());
            return null;
        }
        if (a.equals("flash")) {
            a = cmd.getRemaining();
            if (verCore.release) {
                a = "";
            }
            if (a.length() < 1) {
                a = "./";
            }
            rdr.putStrTab(userFlash.dir2txt(userFlash.dirList(a)));
            return null;
        }
        if (a.equals("dial-peer")) {
            a = cmd.word();
            if (a.equals("description")) {
                userFormat l = new userFormat("|", "id|dir|description");
                for (int i = 0; i < cfgAll.dials.size(); i++) {
                    cfgDial ntry = cfgAll.dials.get(i);
                    if (ntry == null) {
                        continue;
                    }
                    l.add(ntry.name + "|" + ntry.getDir() + "|" + ntry.description);
                }
                rdr.putStrTab(l);
                return null;
            }
            if (a.equals("history")) {
                cfgDial ntry = cfgAll.dialFind(cmd.word(), false);
                if (ntry == null) {
                    cmd.error("no such dial peer");
                    return null;
                }
                rdr.putStrArr(ntry.getHist());
                return null;
            }
            if (a.equals("active")) {
                cfgDial ntry = cfgAll.dialFind(cmd.word(), false);
                if (ntry == null) {
                    cmd.error("no such dial peer");
                    return null;
                }
                cmd.error("inbound");
                rdr.putStrTab(ntry.getCalls(true));
                cmd.error("outbound");
                rdr.putStrTab(ntry.getCalls(false));
                return null;
            }
            boolean call = a.equals("voice");
            userFormat l = new userFormat("|", "id|totI|totO|totT|failI|failO|actI|actO");
            for (int i = 0; i < cfgAll.dials.size(); i++) {
                cfgDial ntry = cfgAll.dials.get(i);
                if (ntry == null) {
                    continue;
                }
                l.add(ntry.getStats(call));
            }
            rdr.putStrTab(l);
            return null;
        }
        if (a.equals("clock")) {
            a = cmd.word();
            List<String> l = new ArrayList<String>();
            long tim = bits.getTime() + cfgAll.timeServerOffset;
            if (a.equals("big")) {
                l = userScreen.fontText(bits.time2str(cfgAll.timeZoneName, tim, 2), " ", userFonts1.fontFiller, userFonts1.fontDefault());
                rdr.putStrArr(l);
                return null;
            }
            if (a.equals("raw")) {
                l.add("" + tim);
                rdr.putStrArr(l);
                return null;
            }
            l.add("clock: " + bits.time2str(cfgAll.timeZoneName, tim, 3));
            l.add("clock: " + bits.time2str(cfgAll.timeZoneName, tim, 4));
            l.add("zone: " + cfgAll.timeZoneName + " diff: " + bits.timeDump(cfgAll.timeServerOffset / 1000));
            rdr.putStrArr(l);
            return null;
        }
        if (a.equals("scheduler")) {
            userFormat l = new userFormat("|", "name|rerun|last|ago");
            for (int i = 0; i < cfgAll.schedulers.size(); i++) {
                cfgSched ntry = cfgAll.schedulers.get(i);
                l.add(ntry.name + "|" + ntry.restartC + "|" + bits.time2str(cfgAll.timeZoneName, ntry.restartT, 3) + "|" + bits.timePast(ntry.restartT));
            }
            rdr.putStrTab(l);
            return null;
        }
        if (a.equals("script")) {
            userFormat l = new userFormat("|", "name|rerun|last|ago");
            for (int i = 0; i < cfgAll.scripts.size(); i++) {
                cfgScrpt ntry = cfgAll.scripts.get(i);
                l.add(ntry.name + "|" + ntry.restartC + "|" + bits.time2str(cfgAll.timeZoneName, ntry.restartT, 3) + "|" + bits.timePast(ntry.restartT));
            }
            rdr.putStrTab(l);
            return null;
        }
        if (a.equals("vdc")) {
            a = cmd.word();
            if (a.equals("interface")) {
                userFormat l = new userFormat("|", "name|assigned");
                for (int i = 0; i < cfgInit.ifaceLst.size(); i++) {
                    cfgVdcIfc ntry = cfgInit.ifaceLst.get(i);
                    l.add(ntry.name + "|" + (cfgAll.ifcFind(ntry.name, false) == null));
                }
                rdr.putStrTab(l);
                return null;
            }
            if (a.equals("device")) {
                userFormat l = new userFormat("|", "name|rerun|last|ago");
                for (int i = 0; i < cfgInit.vdcLst.size(); i++) {
                    cfgVdc ntry = cfgInit.vdcLst.get(i);
                    l.add(ntry.name + "|" + ntry.restartC + "|" + bits.time2str(cfgAll.timeZoneName, ntry.restartT, 3) + "|" + bits.timePast(ntry.restartT));
                }
                rdr.putStrTab(l);
                return null;
            }
            cmd.badCmd();
            return null;
        }
        if (a.equals("process")) {
            a = cmd.word();
            if (a.equals("cpu")) {
                rdr.putStrArr(logger.listThreads());
                return null;
            }
            if (a.equals("external")) {
                userFormat l = new userFormat("|", "name|rerun|last|ago");
                for (int i = 0; i < cfgAll.prcs.size(); i++) {
                    cfgPrcss ntry = cfgAll.prcs.get(i);
                    l.add(ntry.name + "|" + ntry.restartC + "|" + bits.time2str(cfgAll.timeZoneName, ntry.restartT, 3) + "|" + bits.timePast(ntry.restartT));
                }
                rdr.putStrTab(l);
                return null;
            }
            cmd.badCmd();
            return null;
        }
        if (a.equals("redundancy")) {
            rdr.putStrTab(prtRedun.doShow());
            return null;
        }
        if (a.equals("name-cache")) {
            rdr.putStrTab(clntDns.showLocalCache());
            return null;
        }
        if (a.equals("whois")) {
            if (cfgAll.whoisServer == null) {
                cmd.error("not enabled");
                return null;
            }
            clntWhois w = new clntWhois(cfgAll.whoisServer);
            w.quest = cmd.getRemaining();
            rdr.putStrArr(w.doQuery(cmd));
            return null;
        }
        if (a.equals("watchdog")) {
            a = cmd.word();
            if (a.equals("software")) {
                rdr.putStrTab(ifcThread.showStalls());
                return null;
            }
            if (a.equals("hardware")) {
                rdr.putStrTab(prtWatch.doShow());
                return null;
            }
            if (a.equals("timer")) {
                doShowHistory("history", cfgInit.timerHistory);
                return null;
            }
            if (a.equals("memory")) {
                doShowHistory("history", cfgInit.memoryHistory);
                return null;
            }
            return null;
        }
        if (a.equals("reload")) {
            rdr.putStrArr(userReload.reloadShow(cfgAll.reload));
            return null;
        }
        if (a.equals("logging")) {
            a = cmd.word();
            if (a.equals("last")) {
                rdr.putStrArr(logger.bufferRead(bits.str2num(cmd.word())));
                return null;
            }
            rdr.putStrArr(logger.bufferRead());
            return null;
        }
        if (a.equals("rollback-config")) {
            rdr.putStrArr(userFilter.getDiffs(cfgAll.getShRun(true), bits.txt2buf(cfgInit.cfgFileSw)));
            return null;
        }
        if (a.equals("config-differences")) {
            rdr.putStrArr(userFilter.getDiffs(bits.txt2buf(cfgInit.cfgFileSw), cfgAll.getShRun(true)));
            return null;
        }
        if (a.equals("startup-config")) {
            List<String> lst = bits.txt2buf(cfgInit.cfgFileSw);
            if (cmd.size() > 0) {
                lst = userFilter.getSection(lst, userReader.filter2reg(cmd.getRemaining()));
            }
            rdr.putStrArr(lst);
            return null;
        }
        if (a.equals("running-config")) {
            a = cmd.word();
            if (a.equals("all")) {
                rdr.putStrArr(cfgAll.getShRun(false));
                return null;
            }
            if (a.equals("interface")) {
                cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
                if (ifc == null) {
                    cmd.error("no such interface");
                    return null;
                }
                rdr.putStrArr(ifc.getShRun(true));
                return null;
            }
            List<String> lst = cfgAll.getShRun(true);
            if (a.length() > 0) {
                lst = userFilter.getSection(lst, userReader.filter2reg(a + " " + cmd.getRemaining()));
            }
            rdr.putStrArr(lst);
            return null;
        }
        if (a.equals("alias")) {
            rdr.putStrTab(cfgAll.getShAlias());
            return null;
        }
        if (a.equals("vrf")) {
            doShowVrf();
            return null;
        }
        if (a.equals("bmp")) {
            servBmp2mrt srv = cfgAll.srvrFind(new servBmp2mrt(), cfgAll.dmnBmp, cmd.word());
            if (srv == null) {
                cmd.error("no such server");
                return null;
            }
            if (cmd.size() < 1) {
                rdr.putStrTab(srv.getShow());
                return null;
            }
            a = cmd.word();
            addrIP frm = new addrIP();
            frm.fromString(a);
            addrIP per = new addrIP();
            per.fromString(cmd.word());
            userFormat res = srv.getShow(frm, per);
            if (res == null) {
                cmd.error("no such peer");
                return null;
            }
            rdr.putStrTab(res);
            return null;
        }
        if (a.equals("bridge")) {
            cfgBrdg brdg = cfgAll.brdgFind(cmd.word(), false);
            if (brdg == null) {
                cmd.error("no such bridge");
                return null;
            }
            rdr.putStrTab(brdg.bridgeHed.getShowIfc());
            rdr.putStrTab(brdg.bridgeHed.getShowAdr());
            rdr.putStrTab(brdg.bridgeHed.getShowInsp());
            return null;
        }
        if (a.equals("bundle")) {
            cfgBndl bndl = cfgAll.bndlFind(cmd.word(), false);
            if (bndl == null) {
                cmd.error("no such bundle");
                return null;
            }
            rdr.putStrTab(bndl.bundleHed.getShowStt());
            rdr.putStrTab(bndl.bundleHed.getShowIfc());
            return null;
        }
        if (a.equals("transproxy")) {
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
            if (ifc == null) {
                cmd.error("no such interface");
                return null;
            }
            if (ifc.transProxy == null) {
                cmd.error("transparent proxy not configured");
                return null;
            }
            rdr.putStrTab(ifc.transProxy.getShConn());
            return null;
        }
        if (a.equals("lldp")) {
            a = cmd.word();
            if (a.equals("neighbor")) {
                rdr.putStrTab(cfgAll.getShIntTab(7));
                return null;
            }
            if (a.equals("detail")) {
                cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
                if (ifc == null) {
                    cmd.error("no such interface");
                    return null;
                }
                if (ifc.lldp == null) {
                    cmd.error("not running on that interface");
                    return null;
                }
                rdr.putStrArr(ifc.lldp.getShNeigh(true));
                return null;
            }
            cmd.badCmd();
            return null;
        }
        if (a.equals("udld")) {
            a = cmd.word();
            if (a.equals("neighbor")) {
                rdr.putStrTab(cfgAll.getShIntTab(8));
                return null;
            }
            if (a.equals("detail")) {
                cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
                if (ifc == null) {
                    cmd.error("no such interface");
                    return null;
                }
                if (ifc.udld == null) {
                    cmd.error("not running on that interface");
                    return null;
                }
                rdr.putStrArr(ifc.udld.getShNeigh(true));
                return null;
            }
            cmd.badCmd();
            return null;
        }
        if (a.equals("lacp")) {
            a = cmd.word();
            if (a.equals("neighbor")) {
                rdr.putStrTab(cfgAll.getShIntTab(14));
                return null;
            }
            if (a.equals("detail")) {
                cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
                if (ifc == null) {
                    cmd.error("no such interface");
                    return null;
                }
                if (ifc.lacp == null) {
                    cmd.error("not running on that interface");
                    return null;
                }
                rdr.putStrArr(ifc.lacp.getShNeigh(true));
                return null;
            }
            cmd.badCmd();
            return null;
        }
        if (a.equals("cdp")) {
            a = cmd.word();
            if (a.equals("neighbor")) {
                rdr.putStrTab(cfgAll.getShIntTab(6));
                return null;
            }
            if (a.equals("detail")) {
                cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
                if (ifc == null) {
                    cmd.error("no such interface");
                    return null;
                }
                if (ifc.cdp == null) {
                    cmd.error("not running on that interface");
                    return null;
                }
                rdr.putStrArr(ifc.cdp.getShNeigh(true));
                return null;
            }
            cmd.badCmd();
            return null;
        }
        if (a.equals("policy-map")) {
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
            if (ifc == null) {
                cmd.error("no such interface");
                return null;
            }
            a = cmd.word();
            tabQos tab = null;
            if (a.equals("in")) {
                tab = ifc.ethtyp.qosIn;
            }
            if (a.equals("out")) {
                tab = ifc.ethtyp.qosOut;
            }
            if (tab == null) {
                cmd.error("no such policy");
                return null;
            }
            rdr.putStrArr(tab.getStats());
            return null;
        }
        if (a.equals("object-group")) {
            a = cmd.word();
            if (a.equals("network")) {
                cfgObjnet og = cfgAll.objnetFind(cmd.word(), false);
                if (og == null) {
                    cmd.error("no such object group");
                    return null;
                }
                rdr.putStrArr(og.objgrp.getStats());
                return null;
            }
            if (a.equals("port")) {
                cfgObjprt og = cfgAll.objprtFind(cmd.word(), false);
                if (og == null) {
                    cmd.error("no such object group");
                    return null;
                }
                rdr.putStrArr(og.objgrp.getStats());
                return null;
            }
            cmd.badCmd();
            return null;
        }
        if (a.equals("access-list")) {
            cfgAceslst acl = cfgAll.aclsFind(cmd.word(), false);
            if (acl == null) {
                cmd.error("no such access list");
                return null;
            }
            rdr.putStrArr(acl.aceslst.getStats());
            return null;
        }
        if (a.equals("route-map")) {
            cfgRoump rtmp = cfgAll.rtmpFind(cmd.word(), false);
            if (rtmp == null) {
                cmd.error("no such route map");
                return null;
            }
            rdr.putStrArr(rtmp.roumap.getStats());
            return null;
        }
        if (a.equals("route-policy")) {
            cfgRouplc rtpl = cfgAll.rtplFind(cmd.word(), false);
            if (rtpl == null) {
                cmd.error("no such route policy");
                return null;
            }
            rdr.putStrArr(rtpl.rouplc.getStats());
            return null;
        }
        if (a.equals("prefix-list")) {
            cfgPrfxlst prfx = cfgAll.prfxFind(cmd.word(), false);
            if (prfx == null) {
                cmd.error("no such prefix list");
                return null;
            }
            rdr.putStrArr(prfx.prflst.getStats());
            return null;
        }
        if (a.equals("tracker")) {
            if (cmd.size() < 1) {
                rdr.putStrTab(cfgAll.getShTracker());
                return null;
            }
            cfgTrack trck = cfgAll.trackFind(cmd.word(), false);
            if (trck == null) {
                cmd.error("no such tracker");
                return null;
            }
            rdr.putStrArr(trck.worker.getShStat());
            return null;
        }
        if (a.equals("mtracker")) {
            if (cmd.size() < 1) {
                rdr.putStrTab(cfgAll.getShMtracker());
                return null;
            }
            cfgMtrack trck = cfgAll.mtrackFind(cmd.word(), false);
            if (trck == null) {
                cmd.error("no such mtracker");
                return null;
            }
            rdr.putStrArr(trck.worker.getShStat());
            rdr.putStrTab(trck.worker.getShMatrix());
            return null;
        }
        if (a.equals("interfaces")) {
            a = cmd.word();
            if (a.length() < 1) {
                rdr.putStrArr(cfgAll.getShIntTxt(1));
                return null;
            }
            if (a.equals("full")) {
                rdr.putStrArr(cfgAll.getShIntTxt(1));
                return null;
            }
            if (a.equals("description")) {
                rdr.putStrTab(cfgAll.getShIntTab(1));
                return null;
            }
            if (a.equals("summary")) {
                rdr.putStrTab(cfgAll.getShIntTab(2));
                return null;
            }
            if (a.equals("total")) {
                rdr.putStrTab(cfgAll.getShIntTab(10));
                return null;
            }
            if (a.equals("traffic")) {
                rdr.putStrTab(cfgAll.getShIntTab(9));
                return null;
            }
            if (a.equals("psummary")) {
                rdr.putStrTab(cfgAll.getShIntTab(11));
                return null;
            }
            if (a.equals("ptraffic")) {
                rdr.putStrTab(cfgAll.getShIntTab(12));
                return null;
            }
            if (a.equals("ptotal")) {
                rdr.putStrTab(cfgAll.getShIntTab(13));
                return null;
            }
            if (a.equals("vrf")) {
                rdr.putStrTab(cfgAll.getShIntTab(3));
                return null;
            }
            cfgIfc ifc = cfgAll.ifcFind(a, false);
            if (ifc == null) {
                cmd.error("no such interface");
                return null;
            }
            a = cmd.word();
            if (a.equals("full")) {
                rdr.putStrArr(ifc.getShIntTxt(1));
                rdr.putStrTab(doShowRates(ifc.ethtyp.getHistory()));
                rdr.putStrTab(ifc.ethtyp.getShTypes());
                rdr.putStrTab(ifc.ethtyp.getShLoss());
                rdr.putStrTab(ifc.ethtyp.getShProtos());
                rdr.putStrTab(ifc.ethtyp.getShSizes());
                rdr.putStrTab(ifc.ethtyp.getShClasses());
                rdr.putStrArr(ifc.getShIntTxt(2));
                return null;
            }
            if (a.equals("ethertypes")) {
                rdr.putStrTab(ifc.ethtyp.getShTypes());
                return null;
            }
            if (a.equals("lossdetect")) {
                rdr.putStrTab(ifc.ethtyp.getShLoss());
                return null;
            }
            if (a.equals("packetsizes")) {
                rdr.putStrTab(ifc.ethtyp.getShSizes());
                return null;
            }
            if (a.equals("trafficclasses")) {
                rdr.putStrTab(ifc.ethtyp.getShClasses());
                return null;
            }
            if (a.equals("protocols")) {
                rdr.putStrTab(ifc.ethtyp.getShProtos());
                return null;
            }
            if (a.equals("counters")) {
                rdr.putStrArr(ifc.getShIntTxt(1));
                return null;
            }
            if (a.length() < 1) {
                rdr.putStrArr(ifc.getShIntTxt(1));
                return null;
            }
            doShowHistory(a, ifc.ethtyp.getHistory());
            return null;
        }
        if (a.equals("nsh")) {
            a = cmd.word();
            if (a.equals("forwarding")) {
                int p = bits.str2num(cmd.word());
                if (p < 1) {
                    rdr.putStrTab(tabNshNtry.getShFor());
                    return null;
                }
                int i = bits.str2num(cmd.word());
                tabNshNtry ntry = new tabNshNtry(p, i);
                ntry = tabNshNtry.services.find(ntry);
                if (ntry == null) {
                    cmd.error("no such service");
                    return null;
                }
                rdr.putStrTab(ntry.getShow());
                return null;
            }
            if (a.equals("interfaces")) {
                rdr.putStrTab(tabNshNtry.getShInt());
                return null;
            }
            cmd.badCmd();
            return null;
        }
        if (a.equals("mpls")) {
            a = cmd.word();
            if (a.equals("forwarding")) {
                int i = bits.str2num(cmd.word());
                if (i < 1) {
                    rdr.putStrTab(tabLabel.getShFor());
                    return null;
                }
                tabLabelNtry ntry = tabLabel.find(i);
                if (ntry == null) {
                    cmd.error("no such label");
                    return null;
                }
                rdr.putStrTab(ntry.getShow());
                return null;
            }
            if (a.equals("inspect")) {
                cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
                if (ifc == null) {
                    cmd.error("no such interface");
                    return null;
                }
                if (ifc.mplsPack == null) {
                    cmd.error("not enabled");
                    return null;
                }
                if (ifc.mplsPack.inspect == null) {
                    cmd.error("not enabled");
                    return null;
                }
                a = cmd.word();
                if (a.equals("session")) {
                    rdr.putStrTab(ifc.mplsPack.inspect.doShowInsp());
                    return null;
                }
                if (a.equals("toptalk")) {
                    rdr.putStrTab(ifc.mplsPack.inspect.doShowTalk());
                    return null;
                }
                return null;
            }
            if (a.equals("interfaces")) {
                rdr.putStrTab(tabLabel.getShInt());
                return null;
            }
            cmd.badCmd();
            return null;
        }
        if (a.equals("router")) {
            tabRouteEntry.routeType typ = cfgRtr.name2num(cmd.word());
            if (typ == null) {
                cmd.error("invalid process");
                return null;
            }
            cfgRtr cfg = cfgAll.rtrFind(typ, bits.str2num(cmd.word()), false);
            if (cfg == null) {
                cmd.error("no such process");
                return null;
            }
            ipRtr rtr = cfg.getRouter();
            if (rtr == null) {
                cmd.error("not running");
                return null;
            }
            tabRoute<addrIP> tab = null;
            int dsp = 1;
            boolean redist = cmd.word().equals("redisted");
            int tabtyp = rtrLogger.str2afi(cmd.word());
            if (redist) {
                switch (tabtyp) {
                    case 1:
                        tab = rtr.routerRedistedU;
                        break;
                    case 2:
                        tab = rtr.routerRedistedM;
                        break;
                    case 3:
                        tab = rtr.routerRedistedF;
                        dsp = 5;
                        break;
                }
            } else {
                switch (tabtyp) {
                    case 1:
                        tab = rtr.routerComputedU;
                        break;
                    case 2:
                        tab = rtr.routerComputedM;
                        break;
                    case 3:
                        tab = rtr.routerComputedF;
                        dsp = 5;
                        break;
                }
            }
            if (tab == null) {
                cmd.error("invalid table");
                return null;
            }
            doShowRoutes(null, tab, dsp);
            return null;
        }
        if (a.equals("ipx")) {
            a = cmd.word();
            if (a.equals("route")) {
                cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
                if (vrf == null) {
                    cmd.error("no such vrf");
                    return null;
                }
                userFormat l = new userFormat("|", "typ|prefix|metric|iface|hop|time");
                for (int i = 0; i < vrf.ipx.actualR.size(); i++) {
                    tabRouteEntry<addrIpx> prf = vrf.ipx.actualR.get(i);
                    if (prf == null) {
                        continue;
                    }
                    l.add(tabRouteEntry.rouTyp2string(prf.rouTyp) + "|" + prf.prefix + "|" + prf.distance + "/" + prf.metric + "|" + prf.iface + "|" + prf.nextHop + "|" + bits.timePast(prf.time));
                }
                rdr.putStrTab(l);
                return null;
            }
            cmd.badCmd();
            return null;
        }
        if (a.equals("ipv4")) {
            a = cmd.word();
            if (a.equals("vrf")) {
                doShowIpXvrf(4);
                return null;
            }
            if (a.equals("interface")) {
                rdr.putStrTab(cfgAll.getShIntTab(4));
                return null;
            }
            if (a.equals("logger")) {
                doShowIpXlogger(tabRouteEntry.routeType.logger4);
                return null;
            }
            if (a.equals("isis")) {
                doShowIpXisis(tabRouteEntry.routeType.isis4);
                return null;
            }
            if (a.equals("pvrp")) {
                doShowIpXpvrp(tabRouteEntry.routeType.pvrp4);
                return null;
            }
            if (a.equals("lsrp")) {
                doShowIpXlsrp(tabRouteEntry.routeType.lsrp4);
                return null;
            }
            if (a.equals("eigrp")) {
                doShowIpXeigrp(tabRouteEntry.routeType.eigrp4);
                return null;
            }
            if (a.equals("ospf")) {
                cfgRtr r = cfgAll.rtrFind(tabRouteEntry.routeType.ospf4, bits.str2num(cmd.word()), false);
                if (r == null) {
                    cmd.error("no such process");
                    return null;
                }
                a = cmd.word();
                if (a.equals("neighbor")) {
                    rdr.putStrTab(r.ospf4.showNeighs());
                    return null;
                }
                if (a.equals("interface")) {
                    rdr.putStrTab(r.ospf4.showIfaces());
                    return null;
                }
                if (a.equals("database")) {
                    int i = bits.str2num(cmd.word());
                    if (cmd.size() < 1) {
                        rdr.putStrTab(r.ospf4.showDatabase(i));
                    } else {
                        rdr.putStrArr(r.ospf4.showDatabase(i, cmd));
                    }
                    return null;
                }
                if (a.equals("spf")) {
                    int i = bits.str2num(cmd.word());
                    rdr.putStrTab(r.ospf4.showSpfStat(i));
                    rdr.putStrTab(r.ospf4.showSpfLog(i));
                    return null;
                }
                if (a.equals("topology")) {
                    int i = bits.str2num(cmd.word());
                    rdr.putStrTab(r.ospf4.showSpfTopo(i, cmd));
                    return null;
                }
                if (a.equals("tree")) {
                    rdr.putStrArr(r.ospf4.showSpfTree(bits.str2num(cmd.word())));
                    return null;
                }
                if (a.equals("graph")) {
                    rdr.putStrArr(r.ospf4.showSpfGraph(bits.str2num(cmd.word())));
                    return null;
                }
                if (a.equals("route")) {
                    doShowRoutes(r.ospf4.fwdCore, r.ospf4.showRoute(bits.str2num(cmd.word())), 1);
                    return null;
                }
                if (a.equals("originate")) {
                    doShowRoutes(r.ospf4.fwdCore, r.ospf4.routerRedistedU, 1);
                    return null;
                }
                cmd.badCmd();
                return null;
            }
            if (a.equals("bfd")) {
                doShowIpXbfd(4);
                return null;
            }
            if (a.equals("pim")) {
                doShowIpXpim(4);
                return null;
            }
            if (a.equals("inspect")) {
                doShowIpXinspect(4);
                return null;
            }
            if (a.equals("toptalk")) {
                doShowIpXtoptalk(4);
                return null;
            }
            if (a.equals("flow")) {
                doShowIpXnetflow(4);
                return null;
            }
            if (a.equals("hsrp")) {
                doShowIpXhsrp(4);
                return null;
            }
            if (a.equals("vrrp")) {
                doShowIpXvrrp(4);
                return null;
            }
            if (a.equals("msdp")) {
                doShowIpXmsdp(tabRouteEntry.routeType.msdp4);
                return null;
            }
            if (a.equals("ldp")) {
                doShowIpXldp(4);
                return null;
            }
            if (a.equals("rsvp")) {
                doShowIpXrsvp(4);
                return null;
            }
            if (a.equals("bgp")) {
                doShowIpXbgp(tabRouteEntry.routeType.bgp4);
                return null;
            }
            if (a.equals("babel")) {
                doShowIpXbabel(tabRouteEntry.routeType.babel4);
                return null;
            }
            if (a.equals("olsr")) {
                doShowIpXolsr(tabRouteEntry.routeType.olsr4);
                return null;
            }
            if (a.equals("rip")) {
                cfgRtr r = cfgAll.rtrFind(tabRouteEntry.routeType.rip4, bits.str2num(cmd.word()), false);
                if (r == null) {
                    cmd.error("no such process");
                    return null;
                }
                a = cmd.word();
                if (a.equals("summary")) {
                    rdr.putStrTab(r.rip4.showNeighs());
                    return null;
                }
                if (a.equals("interface")) {
                    rdr.putStrTab(r.rip4.showIfaces());
                    return null;
                }
                if (a.equals("database")) {
                    doShowRoutes(r.rip4.fwdCore, r.rip4.routerComputedU, 1);
                    return null;
                }
                if (a.equals("originate")) {
                    doShowRoutes(r.rip4.fwdCore, r.rip4.routerRedistedU, 1);
                    return null;
                }
                if (!a.equals("neighbor")) {
                    cmd.badCmd();
                    return null;
                }
                addrIP adr = new addrIP();
                adr.fromString(cmd.word());
                rtrRip4neigh nei = r.rip4.findPeer(adr);
                if (nei == null) {
                    cmd.error("no such neighbor");
                    return null;
                }
                a = cmd.word();
                if (a.equals("learned")) {
                    doShowRoutes(r.rip4.fwdCore, nei.learned, 1);
                    return null;
                }
                cmd.badCmd();
                return null;
            }
            if (a.equals("nat")) {
                doShowIpXnat(4);
                return null;
            }
            if (a.equals("pbr")) {
                doShowIpXpbr(4);
                return null;
            }
            if (a.equals("arp")) {
                cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
                if (ifc == null) {
                    cmd.error("no such interface");
                    return null;
                }
                if (ifc.ipIf4 == null) {
                    cmd.error("protocol not enabled");
                    return null;
                }
                rdr.putStrTab(ifc.ipIf4.getShCache());
                return null;
            }
            if (a.equals("counter")) {
                doShowCounter(4);
                return null;
            }
            if (a.equals("route")) {
                doShowRouteU(4);
                return null;
            }
            if (a.equals("segrout")) {
                doShowRouteSR(4);
                return null;
            }
            if (a.equals("bier")) {
                doShowRouteBR(4);
                return null;
            }
            if (a.equals("rpf")) {
                doShowRouteM(4);
                return null;
            }
            if (a.equals("flwspc")) {
                doShowRouteF(4);
                return null;
            }
            if (a.equals("mroute")) {
                doShowMroute(4);
                return null;
            }
            if (a.equals("protocol")) {
                doShowProtocols(4);
                return null;
            }
            if (a.equals("sockets")) {
                doShowSockets(4);
                return null;
            }
            cmd.badCmd();
            return null;
        }
        if (a.equals("ipv6")) {
            a = cmd.word();
            if (a.equals("vrf")) {
                doShowIpXvrf(6);
                return null;
            }
            if (a.equals("interface")) {
                rdr.putStrTab(cfgAll.getShIntTab(5));
                return null;
            }
            if (a.equals("logger")) {
                doShowIpXlogger(tabRouteEntry.routeType.logger6);
                return null;
            }
            if (a.equals("isis")) {
                doShowIpXisis(tabRouteEntry.routeType.isis6);
                return null;
            }
            if (a.equals("pvrp")) {
                doShowIpXpvrp(tabRouteEntry.routeType.pvrp6);
                return null;
            }
            if (a.equals("lsrp")) {
                doShowIpXlsrp(tabRouteEntry.routeType.lsrp6);
                return null;
            }
            if (a.equals("eigrp")) {
                doShowIpXeigrp(tabRouteEntry.routeType.eigrp6);
                return null;
            }
            if (a.equals("ospf")) {
                cfgRtr r = cfgAll.rtrFind(tabRouteEntry.routeType.ospf6, bits.str2num(cmd.word()), false);
                if (r == null) {
                    cmd.error("no such process");
                    return null;
                }
                a = cmd.word();
                if (a.equals("neighbor")) {
                    rdr.putStrTab(r.ospf6.showNeighs());
                    return null;
                }
                if (a.equals("interface")) {
                    rdr.putStrTab(r.ospf6.showIfaces());
                    return null;
                }
                if (a.equals("database")) {
                    int i = bits.str2num(cmd.word());
                    if (cmd.size() < 1) {
                        rdr.putStrTab(r.ospf6.showDatabase(i));
                    } else {
                        rdr.putStrArr(r.ospf6.showDatabase(i, cmd));
                    }
                    return null;
                }
                if (a.equals("spf")) {
                    int i = bits.str2num(cmd.word());
                    rdr.putStrTab(r.ospf6.showSpfStat(i));
                    rdr.putStrTab(r.ospf6.showSpfLog(i));
                    return null;
                }
                if (a.equals("topology")) {
                    int i = bits.str2num(cmd.word());
                    rdr.putStrTab(r.ospf6.showSpfTopo(i, cmd));
                    return null;
                }
                if (a.equals("tree")) {
                    rdr.putStrArr(r.ospf6.showSpfTree(bits.str2num(cmd.word())));
                    return null;
                }
                if (a.equals("graph")) {
                    rdr.putStrArr(r.ospf6.showSpfGraph(bits.str2num(cmd.word())));
                    return null;
                }
                if (a.equals("route")) {
                    doShowRoutes(r.ospf6.fwdCore, r.ospf6.showRoute(bits.str2num(cmd.word())), 1);
                    return null;
                }
                if (a.equals("originate")) {
                    doShowRoutes(r.ospf6.fwdCore, r.ospf6.routerRedistedU, 1);
                    return null;
                }
                cmd.badCmd();
                return null;
            }
            if (a.equals("bfd")) {
                doShowIpXbfd(6);
                return null;
            }
            if (a.equals("pim")) {
                doShowIpXpim(6);
                return null;
            }
            if (a.equals("inspect")) {
                doShowIpXinspect(6);
                return null;
            }
            if (a.equals("toptalk")) {
                doShowIpXtoptalk(6);
                return null;
            }
            if (a.equals("flow")) {
                doShowIpXnetflow(6);
                return null;
            }
            if (a.equals("hsrp")) {
                doShowIpXhsrp(6);
                return null;
            }
            if (a.equals("vrrp")) {
                doShowIpXvrrp(6);
                return null;
            }
            if (a.equals("msdp")) {
                doShowIpXmsdp(tabRouteEntry.routeType.msdp6);
                return null;
            }
            if (a.equals("ldp")) {
                doShowIpXldp(6);
                return null;
            }
            if (a.equals("rsvp")) {
                doShowIpXrsvp(6);
                return null;
            }
            if (a.equals("bgp")) {
                doShowIpXbgp(tabRouteEntry.routeType.bgp6);
                return null;
            }
            if (a.equals("babel")) {
                doShowIpXbabel(tabRouteEntry.routeType.babel6);
                return null;
            }
            if (a.equals("olsr")) {
                doShowIpXolsr(tabRouteEntry.routeType.olsr6);
                return null;
            }
            if (a.equals("rip")) {
                cfgRtr r = cfgAll.rtrFind(tabRouteEntry.routeType.rip6, bits.str2num(cmd.word()), false);
                if (r == null) {
                    cmd.error("no such process");
                    return null;
                }
                a = cmd.word();
                if (a.equals("summary")) {
                    rdr.putStrTab(r.rip6.showNeighs());
                    return null;
                }
                if (a.equals("interface")) {
                    rdr.putStrTab(r.rip6.showIfaces());
                    return null;
                }
                if (a.equals("database")) {
                    doShowRoutes(r.rip6.fwdCore, r.rip6.routerComputedU, 1);
                    return null;
                }
                if (a.equals("originate")) {
                    doShowRoutes(r.rip6.fwdCore, r.rip6.routerRedistedU, 1);
                    return null;
                }
                if (!a.equals("neighbor")) {
                    cmd.badCmd();
                    return null;
                }
                addrIP adr = new addrIP();
                adr.fromString(cmd.word());
                rtrRip6neigh nei = r.rip6.findPeer(adr);
                if (nei == null) {
                    cmd.error("no such neighbor");
                    return null;
                }
                a = cmd.word();
                if (a.equals("learned")) {
                    doShowRoutes(r.rip6.fwdCore, nei.learned, 1);
                    return null;
                }
                cmd.badCmd();
                return null;
            }
            if (a.equals("nat")) {
                doShowIpXnat(6);
                return null;
            }
            if (a.equals("pbr")) {
                doShowIpXpbr(6);
                return null;
            }
            if (a.equals("neighbors")) {
                cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
                if (ifc == null) {
                    cmd.error("no such interface");
                    return null;
                }
                if (ifc.ipIf6 == null) {
                    cmd.error("protocol not enabled");
                    return null;
                }
                rdr.putStrTab(ifc.ipIf6.getShCache());
                return null;
            }
            if (a.equals("counter")) {
                doShowCounter(6);
                return null;
            }
            if (a.equals("route")) {
                doShowRouteU(6);
                return null;
            }
            if (a.equals("segrout")) {
                doShowRouteSR(6);
                return null;
            }
            if (a.equals("bier")) {
                doShowRouteBR(6);
                return null;
            }
            if (a.equals("rpf")) {
                doShowRouteM(6);
                return null;
            }
            if (a.equals("flwspc")) {
                doShowRouteF(6);
                return null;
            }
            if (a.equals("mroute")) {
                doShowMroute(6);
                return null;
            }
            if (a.equals("protocol")) {
                doShowProtocols(6);
                return null;
            }
            if (a.equals("sockets")) {
                doShowSockets(6);
                return null;
            }
            cmd.badCmd();
            return null;
        }
        cmd.badCmd();
        return null;
    }

    private void doShowIpXeigrp(tabRouteEntry.routeType afi) {
        cfgRtr r = cfgAll.rtrFind(afi, bits.str2num(cmd.word()), false);
        if (r == null) {
            cmd.error("no such process");
            return;
        }
        String a = cmd.word();
        if (a.equals("summary")) {
            rdr.putStrTab(r.eigrp.showNeighs());
            return;
        }
        if (a.equals("interface")) {
            rdr.putStrTab(r.eigrp.showIfaces());
            return;
        }
        if (a.equals("neighbor")) {
            addrIP adr = new addrIP();
            adr.fromString(cmd.word());
            rtrEigrpNeigh nei = r.eigrp.findNeigh(adr);
            if (nei == null) {
                cmd.error("no such neighbor");
                return;
            }
            a = cmd.word();
            if (a.equals("learned")) {
                doShowRoutes(r.eigrp.fwdCore, nei.learned, 1);
                return;
            }
            if (a.equals("adverted")) {
                doShowRoutes(r.eigrp.fwdCore, nei.adverted, 1);
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("route")) {
            doShowRoutes(r.eigrp.fwdCore, r.eigrp.need2adv, 1);
            return;
        }
        if (a.equals("originate")) {
            doShowRoutes(r.eigrp.fwdCore, r.eigrp.routerRedistedU, 1);
            return;
        }
        cmd.badCmd();
        return;
    }

    private void doShowIpXlogger(tabRouteEntry.routeType afi) {
        cfgRtr r = cfgAll.rtrFind(afi, bits.str2num(cmd.word()), false);
        if (r == null) {
            cmd.error("no such process");
            return;
        }
        String a = cmd.word();
        if (a.equals("flapstat")) {
            rdr.putStrTab(r.logger.getFlapstat(bits.str2num(cmd.word())));
            return;
        }
        if (a.equals("unicast")) {
            doShowRoutes(r.logger.fwdCore, r.logger.getRoutes(1), 1);
            return;
        }
        if (a.equals("multicast")) {
            doShowRoutes(r.logger.fwdCore, r.logger.getRoutes(2), 1);
            return;
        }
        if (a.equals("flowspec")) {
            doShowRoutes(r.logger.fwdCore, r.logger.getRoutes(3), 5);
            return;
        }
        if (a.equals("prefix-lengths")) {
            rdr.putStrTab(r.logger.prefixLengths());
            return;
        }
    }

    private void doShowIpXlsrp(tabRouteEntry.routeType afi) {
        cfgRtr r = cfgAll.rtrFind(afi, bits.str2num(cmd.word()), false);
        if (r == null) {
            cmd.error("no such process");
            return;
        }
        String a = cmd.word();
        if (a.equals("neighbor")) {
            rdr.putStrTab(r.lsrp.showNeighs());
            return;
        }
        if (a.equals("interface")) {
            rdr.putStrTab(r.lsrp.showIfaces());
            return;
        }
        if (a.equals("segrout")) {
            rdr.putStrTab(r.lsrp.showDatabase(2));
            return;
        }
        if (a.equals("uptime")) {
            rdr.putStrTab(r.lsrp.showDatabase(3));
            return;
        }
        if (a.equals("zonefile")) {
            rdr.putStrTab(r.lsrp.showZoneFile(cmd.word()));
            return;
        }
        if (a.equals("software")) {
            rdr.putStrTab(r.lsrp.showDatabase(4));
            return;
        }
        if (a.equals("bier")) {
            rdr.putStrTab(r.lsrp.showDatabase(5));
            return;
        }
        if (a.equals("database")) {
            if (cmd.size() < 1) {
                rdr.putStrTab(r.lsrp.showDatabase(1));
            } else {
                rdr.putStrTab(r.lsrp.showDatabase(cmd));
            }
            return;
        }
        if (a.equals("spf")) {
            rdr.putStrTab(r.lsrp.showSpfStat());
            rdr.putStrTab(r.lsrp.showSpfLog());
            return;
        }
        if (a.equals("topology")) {
            rdr.putStrTab(r.lsrp.showSpfTopo(cmd));
            return;
        }
        if (a.equals("tree")) {
            rdr.putStrArr(r.lsrp.showSpfTree());
            return;
        }
        if (a.equals("graph")) {
            rdr.putStrArr(r.lsrp.showSpfGraph());
            return;
        }
        if (a.equals("route")) {
            doShowRoutes(r.lsrp.fwdCore, r.lsrp.routerComputedU, 1);
            return;
        }
        if (a.equals("originate")) {
            doShowRoutes(r.lsrp.fwdCore, r.lsrp.routerRedistedU, 1);
            return;
        }
        cmd.badCmd();
        return;
    }

    private void doShowIpXpvrp(tabRouteEntry.routeType afi) {
        cfgRtr r = cfgAll.rtrFind(afi, bits.str2num(cmd.word()), false);
        if (r == null) {
            cmd.error("no such process");
            return;
        }
        String a = cmd.word();
        if (a.equals("summary")) {
            rdr.putStrTab(r.pvrp.showNeighs());
            return;
        }
        if (a.equals("interface")) {
            rdr.putStrTab(r.pvrp.showIfaces());
            return;
        }
        if (a.equals("neighbor")) {
            addrIP adr = new addrIP();
            adr.fromString(cmd.word());
            rtrPvrpNeigh nei = r.pvrp.findNeigh(adr);
            if (nei == null) {
                cmd.error("no such neighbor");
                return;
            }
            a = cmd.word();
            if (a.equals("learned")) {
                doShowRoutes(r.pvrp.fwdCore, nei.learned, 1);
                return;
            }
            if (a.equals("adverted")) {
                doShowRoutes(r.pvrp.fwdCore, nei.adverted, 1);
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("route")) {
            doShowRoutes(r.pvrp.fwdCore, r.pvrp.need2adv, 1);
            return;
        }
        if (a.equals("originate")) {
            doShowRoutes(r.pvrp.fwdCore, r.pvrp.routerRedistedU, 1);
            return;
        }
        cmd.badCmd();
        return;
    }

    private void doShowIpXisis(tabRouteEntry.routeType afi) {
        cfgRtr r = cfgAll.rtrFind(afi, bits.str2num(cmd.word()), false);
        if (r == null) {
            cmd.error("no such process");
            return;
        }
        String a = cmd.word();
        if (a.equals("neighbor")) {
            rdr.putStrTab(r.isis.showNeighs());
            return;
        }
        if (a.equals("interface")) {
            rdr.putStrTab(r.isis.showIfaces());
            return;
        }
        if (a.equals("database")) {
            int i = bits.str2num(cmd.word());
            if (cmd.size() < 1) {
                rdr.putStrTab(r.isis.showDatabase(i));
            } else {
                rdr.putStrArr(r.isis.showDatabase(i, cmd));
            }
            return;
        }
        if (a.equals("spf")) {
            int i = bits.str2num(cmd.word());
            rdr.putStrTab(r.isis.showSpfStat(i));
            rdr.putStrTab(r.isis.showSpfLog(i));
            return;
        }
        if (a.equals("topology")) {
            int i = bits.str2num(cmd.word());
            rdr.putStrTab(r.isis.showSpfTopo(i, cmd));
            return;
        }
        if (a.equals("tree")) {
            rdr.putStrArr(r.isis.showSpfTree(bits.str2num(cmd.word())));
            return;
        }
        if (a.equals("graph")) {
            rdr.putStrArr(r.isis.showSpfGraph(bits.str2num(cmd.word())));
            return;
        }
        if (a.equals("route")) {
            doShowRoutes(r.isis.fwdCore, r.isis.showRoute(bits.str2num(cmd.word())), 1);
            return;
        }
        if (a.equals("originate")) {
            doShowRoutes(r.isis.fwdCore, r.isis.routerRedistedU, 1);
            return;
        }
        cmd.badCmd();
        return;
    }

    private ipFwd findVrf(int ver) {
        cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
        if (vrf == null) {
            cmd.error("no such vrf");
            return null;
        }
        if (ver == 4) {
            return vrf.fwd4;
        } else {
            return vrf.fwd6;
        }
    }

    private void doShowIpXbfd(int ver) {
        ipFwd fwd = findVrf(ver);
        if (fwd == null) {
            return;
        }
        String a = cmd.word();
        if (a.equals("neighbor")) {
            rdr.putStrTab(ipFwdTab.bfdNeighShow(fwd));
            return;
        }
        cmd.badCmd();
    }

    private void doShowIpXtoptalk(int ver) {
        cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
        if (ifc == null) {
            cmd.error("no such interface");
            return;
        }
        tabSession ins = null;
        if (ver == 4) {
            if (ifc.fwdIf4 != null) {
                ins = ifc.fwdIf4.inspect;
            }
        } else {
            if (ifc.fwdIf6 != null) {
                ins = ifc.fwdIf6.inspect;
            }
        }
        if (ins == null) {
            cmd.error("not active");
            return;
        }
        rdr.putStrTab(ins.doShowTalk());
    }

    private void doShowIpXinspect(int ver) {
        cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
        if (ifc == null) {
            cmd.error("no such interface");
            return;
        }
        tabSession ins = null;
        if (ver == 4) {
            if (ifc.fwdIf4 != null) {
                ins = ifc.fwdIf4.inspect;
            }
        } else {
            if (ifc.fwdIf6 != null) {
                ins = ifc.fwdIf6.inspect;
            }
        }
        if (ins == null) {
            cmd.error("not active");
            return;
        }
        rdr.putStrTab(ins.doShowInsp());
    }

    private void doShowIpXnetflow(int ver) {
        cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
        ipFwd fwd;
        if (ver == 4) {
            fwd = vrf.fwd4;
        } else {
            fwd = vrf.fwd6;
        }
        clntNetflow flw = fwd.netflow;
        if (flw == null) {
            cmd.error("not active");
            return;
        }
        String a = cmd.word();
        if (a.equals("session")) {
            rdr.putStrTab(flw.session.doShowInsp());
            return;
        }
        if (a.equals("toptalk")) {
            rdr.putStrTab(flw.session.doShowTalk());
            return;
        }
        cmd.badCmd();
    }

    private void doShowIpXhsrp(int ver) {
        ipFwd fwd = findVrf(ver);
        if (fwd == null) {
            return;
        }
        String a = cmd.word();
        if (a.equals("neighbor")) {
            rdr.putStrTab(ipFwdTab.hsrpNeighShow(fwd));
            return;
        }
        cmd.badCmd();
    }

    private void doShowIpXvrrp(int ver) {
        ipFwd fwd = findVrf(ver);
        if (fwd == null) {
            return;
        }
        String a = cmd.word();
        if (a.equals("neighbor")) {
            rdr.putStrTab(ipFwdTab.vrrpNeighShow(fwd));
            return;
        }
        cmd.badCmd();
    }

    private void doShowIpXpim(int ver) {
        ipFwd fwd = findVrf(ver);
        if (fwd == null) {
            return;
        }
        String a = cmd.word();
        if (a.equals("neighbor")) {
            rdr.putStrTab(ipFwdTab.pimNeighShow(fwd));
            return;
        }
        if (a.equals("interface")) {
            rdr.putStrTab(ipFwdTab.pimIfaceShow(fwd));
            return;
        }
        cmd.badCmd();
    }

    private void doShowIpXmsdp(tabRouteEntry.routeType afi) {
        cfgRtr r = cfgAll.rtrFind(afi, bits.str2num(cmd.word()), false);
        if (r == null) {
            cmd.error("no such process");
            return;
        }
        String a = cmd.word();
        if (a.equals("neighbor")) {
            rdr.putStrTab(r.msdp.getNeighShow());
            return;
        }
        if (a.equals("database")) {
            rdr.putStrTab(r.msdp.getSourcesShow());
            return;
        }
        cmd.badCmd();
    }

    private void doShowPweList(tabGen<packLdpPwe> lst) {
        userFormat txt = new userFormat("|", "type|cw|group|vcid|mtu|vccv|label|description");
        for (int i = 0; i < lst.size(); i++) {
            txt.add("" + lst.get(i));
        }
        rdr.putStrTab(txt);
    }

    private void doShowPmpList(tabGen<packLdpMp> lst) {
        userFormat txt = new userFormat("|", "type|root|label|opaque");
        for (int i = 0; i < lst.size(); i++) {
            txt.add("" + lst.get(i));
        }
        rdr.putStrTab(txt);
    }

    private void doShowIpXrsvp(int ver) {
        ipFwd fwd = findVrf(ver);
        if (fwd == null) {
            return;
        }
        String a = cmd.word();
        if (a.equals("summary")) {
            rdr.putStrTab(ipFwdTab.rsvpTunnelShow(fwd));
            return;
        }
        if (a.equals("detail")) {
            addrIP a1 = new addrIP();
            a1.fromString(cmd.word());
            int i1 = bits.str2num(cmd.word());
            addrIP a2 = new addrIP();
            a2.fromString(cmd.word());
            int i2 = bits.str2num(cmd.word());
            ipFwdTrfng ntry = new ipFwdTrfng(a1, i1, a2, i2);
            ntry = fwd.trafEngs.find(ntry);
            if (ntry == null) {
                cmd.error("no such tunnel");
                return;
            }
            rdr.putStrArr(ntry.dump());
            return;
        }
        cmd.badCmd();
    }

    private static tabRoute<addrIP> nullLabels(tabRoute<addrIP> lst) {
        tabRoute<addrIP> res = new tabRoute<addrIP>("rx");
        for (int i = 0; i < lst.size(); i++) {
            tabRouteEntry<addrIP> ntry = lst.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.labelRem == null) {
                continue;
            }
            if (ntry.labelRem.size() != 1) {
                continue;
            }
            int o = ntry.labelRem.get(0);
            if ((o != ipMpls.labelImp) && (o != ipMpls.labelExp4) && (o != ipMpls.labelExp6)) {
                continue;
            }
            res.add(tabRoute.addType.always, ntry, false, false);
        }
        return res;
    }

    private void doShowIpXldp(int ver) {
        ipFwd fwd = findVrf(ver);
        if (fwd == null) {
            return;
        }
        String a = cmd.word();
        if (a.equals("summary")) {
            rdr.putStrTab(ipFwdTab.ldpNeighShow(fwd));
            return;
        }
        if (a.equals("database")) {
            doShowRoutes(fwd, fwd.labeldR, 3);
            return;
        }
        if (a.equals("mpdatabase")) {
            doShowMptab(fwd.mp2mpLsp);
            return;
        }
        if (!a.equals("neighbor")) {
            cmd.badCmd();
            return;
        }
        addrIP adr = new addrIP();
        adr.fromString(cmd.word());
        rtrLdpNeigh nei = fwd.ldpNeighFind(null, adr, false);
        if (nei == null) {
            cmd.error("no such neighbor");
            return;
        }
        a = cmd.word();
        if (a.equals("nulled")) {
            doShowRoutes(fwd, nullLabels(nei.prefLearn), 3);
            return;
        }
        if (a.equals("learned")) {
            doShowRoutes(fwd, nei.prefLearn, 3);
            return;
        }
        if (a.equals("advertised")) {
            doShowRoutes(fwd, nei.prefAdvert, 3);
            return;
        }
        if (a.equals("l2learned")) {
            doShowPweList(nei.pweLearn);
            return;
        }
        if (a.equals("l2advertised")) {
            doShowPweList(nei.pweAdvert);
            return;
        }
        if (a.equals("l2needed")) {
            doShowPweList(nei.pweNeed2adv);
            return;
        }
        if (a.equals("mplearned")) {
            doShowPmpList(nei.pmpLearn);
            return;
        }
        if (a.equals("mpadvertised")) {
            doShowPmpList(nei.pmpAdvert);
            return;
        }
        if (a.equals("status")) {
            List<String> l = new ArrayList<String>();
            nei.getStatus(l);
            rdr.putStrArr(l);
            return;
        }
        cmd.badCmd();
    }

    private void doShowIpXbabel(tabRouteEntry.routeType afi) {
        cfgRtr r = cfgAll.rtrFind(afi, bits.str2num(cmd.word()), false);
        if (r == null) {
            cmd.error("no such process");
            return;
        }
        String a = cmd.word();
        if (a.equals("summary")) {
            rdr.putStrTab(r.babel.showNeighs());
            return;
        }
        if (a.equals("interface")) {
            rdr.putStrTab(r.babel.showIfaces());
            return;
        }
        if (a.equals("database")) {
            doShowRoutes(r.babel.fwdCore, r.babel.routerComputedU, 1);
            return;
        }
        if (a.equals("originate")) {
            doShowRoutes(r.babel.fwdCore, r.babel.routerRedistedU, 1);
            return;
        }
        if (!a.equals("neighbor")) {
            cmd.badCmd();
            return;
        }
        addrIP adr = new addrIP();
        adr.fromString(cmd.word());
        rtrBabelNeigh nei = r.babel.findPeer(adr);
        if (nei == null) {
            cmd.error("no such neighbor");
            return;
        }
        a = cmd.word();
        if (a.equals("learned")) {
            doShowRoutes(r.babel.fwdCore, nei.learned, 1);
            return;
        }
        cmd.badCmd();
    }

    private void doShowIpXolsr(tabRouteEntry.routeType afi) {
        cfgRtr r = cfgAll.rtrFind(afi, bits.str2num(cmd.word()), false);
        if (r == null) {
            cmd.error("no such process");
            return;
        }
        String a = cmd.word();
        if (a.equals("summary")) {
            rdr.putStrTab(r.olsr.showNeighs());
            return;
        }
        if (a.equals("interface")) {
            rdr.putStrTab(r.olsr.showIfaces());
            return;
        }
        if (a.equals("database")) {
            doShowRoutes(r.olsr.fwdCore, r.olsr.routerComputedU, 1);
            return;
        }
        if (a.equals("originate")) {
            doShowRoutes(r.olsr.fwdCore, r.olsr.routerRedistedU, 1);
            return;
        }
        if (!a.equals("neighbor")) {
            cmd.badCmd();
            return;
        }
        addrIP adr = new addrIP();
        adr.fromString(cmd.word());
        rtrOlsrNeigh nei = r.olsr.findPeer(adr);
        if (nei == null) {
            cmd.error("no such neighbor");
            return;
        }
        a = cmd.word();
        if (a.equals("learned")) {
            doShowRoutes(r.olsr.fwdCore, nei.learned, 1);
            return;
        }
        cmd.badCmd();
    }

    private void compareTables(tabRoute<addrIP> uniq, tabRoute<addrIP> diff, tabRoute<addrIP> nei1, tabRoute<addrIP> nei2, int ign) {
        for (int i = 0; i < nei1.size(); i++) {
            tabRouteEntry<addrIP> prf1 = nei1.get(i);
            tabRouteEntry<addrIP> prf2 = nei2.find(prf1);
            if (prf2 == null) {
                uniq.add(tabRoute.addType.always, prf1, false, false);
                continue;
            }
            prf1 = prf1.copyBytes();
            prf2 = prf2.copyBytes();
            prf1.srcRtr = null;
            prf2.srcRtr = null;
            prf1.rouSrc = 0;
            prf2.rouSrc = 0;
            prf1.rouTyp = tabRouteEntry.routeType.bgp4;
            prf2.rouTyp = tabRouteEntry.routeType.bgp4;
            prf1.iface = null;
            prf2.iface = null;
            if ((ign & 0x1) != 0) {
                prf1.clustList = null;
                prf2.clustList = null;
            }
            if ((ign & 0x2) != 0) {
                prf1.nextHop = null;
                prf2.nextHop = null;
                prf1.oldHop = null;
                prf2.oldHop = null;
            }
            if ((ign & 0x4) != 0) {
                prf1.origin = 0;
                prf2.origin = 0;
            }
            if ((ign & 0x8) != 0) {
                prf1.metric = 0;
                prf2.metric = 0;
            }
            if ((ign & 0x10) != 0) {
                prf1.locPref = 0;
                prf2.locPref = 0;
            }
            if ((ign & 0x20) != 0) {
                prf1.distance = 0;
                prf2.distance = 0;
            }
            if ((ign & 0x40) != 0) {
                prf1.tag = 0;
                prf2.tag = 0;
            }
            if ((ign & 0x80) != 0) {
                prf1.validity = 0;
                prf2.validity = 0;
            }
            if ((ign & 0x100) != 0) {
                prf1.pathSeq = null;
                prf2.pathSeq = null;
                prf1.pathSet = null;
                prf2.pathSet = null;
            }
            if ((ign & 0x200) != 0) {
                prf1.confSeq = null;
                prf2.confSeq = null;
                prf1.confSet = null;
                prf2.confSet = null;
            }
            if ((ign & 0x400) != 0) {
                prf1.stdComm = null;
                prf2.stdComm = null;
            }
            if ((ign & 0x800) != 0) {
                prf1.extComm = null;
                prf2.extComm = null;
            }
            if ((ign & 0x1000) != 0) {
                prf1.accIgp = 0;
                prf2.accIgp = 0;
            }
            if ((ign & 0x2000) != 0) {
                prf1.bandwidth = 0;
                prf2.bandwidth = 0;
            }
            if ((ign & 0x4000) != 0) {
                prf1.labelLoc = null;
                prf2.labelLoc = null;
                prf1.labelRem = null;
                prf2.labelRem = null;
            }
            if ((ign & 0x8000) != 0) {
                prf1.atomicAggr = false;
                prf2.atomicAggr = false;
                prf1.aggrAs = 0;
                prf2.aggrAs = 0;
                prf1.aggrRtr = null;
                prf2.aggrRtr = null;
            }
            if ((ign & 0x10000) != 0) {
                prf1.originator = null;
                prf2.originator = null;
            }
            if ((ign & 0x20000) != 0) {
                prf1.pmsiLab = 0;
                prf2.pmsiLab = 0;
                prf1.pmsiTyp = 0;
                prf2.pmsiTyp = 0;
                prf1.pmsiTun = null;
                prf2.pmsiTun = null;
            }
            if ((ign & 0x40000) != 0) {
                prf1.segrouIdx = 0;
                prf2.segrouIdx = 0;
                prf1.segrouBeg = 0;
                prf2.segrouBeg = 0;
                prf1.segrouOld = 0;
                prf2.segrouOld = 0;
                prf1.segrouSiz = 0;
                prf2.segrouSiz = 0;
            }
            if ((ign & 0x80000) != 0) {
                prf1.lrgComm = null;
                prf2.lrgComm = null;
            }
            if ((ign & 0x100000) != 0) {
                prf1.tunelTyp = 0;
                prf2.tunelTyp = 0;
                prf1.tunelVal = null;
                prf2.tunelVal = null;
            }
            if ((ign & 0x200000) != 0) {
                prf1.attribAs = 0;
                prf2.attribAs = 0;
                prf1.attribVal = null;
                prf2.attribVal = null;
            }
            if ((ign & 0x400000) != 0) {
                prf1.bierIdx = 0;
                prf2.bierIdx = 0;
                prf1.bierBeg = 0;
                prf2.bierBeg = 0;
                prf1.bierOld = 0;
                prf2.bierOld = 0;
                prf1.bierSiz = 0;
                prf2.bierSiz = 0;
                prf1.bierHdr = 0;
                prf2.bierHdr = 0;
            }
            if (!prf1.differs(prf2)) {
                continue;
            }
            diff.add(tabRoute.addType.always, prf1, false, false);
        }
    }

    private void doShowIpXbgp(tabRouteEntry.routeType afi) {
        cfgRtr r = cfgAll.rtrFind(afi, bits.str2num(cmd.word()), false);
        if (r == null) {
            cmd.error("no such process");
            return;
        }
        String a = cmd.word();
        if (a.equals("bestpath")) {
            rdr.putStrTab(r.bgp.getBestpath());
            return;
        }
        if (a.equals("group")) {
            rtrBgpGroup grp = r.bgp.findGroup(bits.str2num(cmd.word()));
            if (grp == null) {
                cmd.error("no such group");
                return;
            }
            a = cmd.word();
            if (a.length() < 1) {
                rdr.putStrTab(r.bgp.showSummary(2));
                return;
            }
            if (a.equals("config")) {
                List<String> l = new ArrayList<String>();
                grp.getConfig(l, "", false);
                rdr.putStrArr(l);
                return;
            }
            if (a.equals("status")) {
                List<String> l = new ArrayList<String>();
                grp.getStatus(l);
                rdr.putStrArr(l);
                return;
            }
            int sfi = rtrBgpParam.string2mask(a);
            if (sfi < 1) {
                return;
            }
            int dsp = bgpMask2filter(sfi);
            sfi = r.bgp.mask2safi(sfi);
            if (sfi < 1) {
                return;
            }
            tabRoute<addrIP> tab = grp.getWilling(sfi);
            if (tab == null) {
                return;
            }
            doShowRoutes(r.bgp.fwdCore, tab, dsp);
            return;
        }
        if (a.equals("nexthop")) {
            rdr.putStrTab(r.bgp.showSummary(3));
            return;
        }
        if (a.equals("rpkisum")) {
            rdr.putStrTab(r.bgp.showRpkiNei());
            return;
        }
        if (a.equals("rpkitab")) {
            doShowRoutes(r.bgp.fwdCore, r.bgp.computedRpki, 4);
            return;
        }
        if (a.equals("graceful-restart")) {
            rdr.putStrTab(r.bgp.showSummary(4));
            return;
        }
        if (a.equals("resolve")) {
            rdr.putStrTab(r.bgp.showSummary(12));
            return;
        }
        if (a.equals("additional-path")) {
            rdr.putStrTab(r.bgp.showSummary(5));
            return;
        }
        if (a.equals("router-id")) {
            rdr.putStrTab(r.bgp.showSummary(6));
            return;
        }
        if (a.equals("buffer")) {
            rdr.putStrTab(r.bgp.showSummary(7));
            return;
        }
        if (a.equals("description")) {
            rdr.putStrTab(r.bgp.showSummary(8));
            return;
        }
        if (a.equals("hostname")) {
            rdr.putStrTab(r.bgp.showSummary(9));
            return;
        }
        if (a.equals("compression")) {
            rdr.putStrTab(r.bgp.showSummary(10));
            return;
        }
        if (a.equals("connection")) {
            rdr.putStrTab(r.bgp.showSummary(11));
            return;
        }
        if (a.equals("summary")) {
            rdr.putStrTab(r.bgp.showSummary(1));
            return;
        }
        int sfi = rtrBgpParam.string2mask(a);
        if (sfi > 0) {
            int dsp = bgpMask2filter(sfi);
            sfi = r.bgp.mask2safi(sfi);
            if (sfi < 1) {
                return;
            }
            a = cmd.word();
            if (a.equals("summary")) {
                rdr.putStrTab(r.bgp.showNeighs(sfi));
                return;
            }
            if (a.equals("asgraph")) {
                rdr.putStrArr(r.bgp.getAsGraph(sfi));
                return;
            }
            if (a.equals("asconn")) {
                rdr.putStrTab(r.bgp.getAsConns(sfi));
                return;
            }
            if (a.equals("prefix-lengths")) {
                rdr.putStrTab(rtrLogger.prefixLengths(r.bgp.getDatabase(sfi)));
                return;
            }
            if (a.equals("asinconsistent")) {
                rdr.putStrTab(r.bgp.getAsIncons(sfi));
                return;
            }
            if (a.equals("flapstat")) {
                rdr.putStrTab(r.bgp.getFlapstat(sfi, bits.str2num(cmd.word())));
                return;
            }
            if (a.equals("flappath")) {
                addrPrefix<addrIP> ntry = addrPrefix.str2ip(cmd.word());
                if (ntry == null) {
                    cmd.error("bad prefix");
                    return;
                }
                rdr.putStrTab(r.bgp.getFlappath(sfi, tabRtrmapN.string2rd(cmd.word()), ntry));
                return;
            }
            if (a.equals("allroute")) {
                tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
                ntry.prefix = addrPrefix.str2ip(cmd.word());
                if (ntry.prefix == null) {
                    cmd.error("bad prefix");
                    return;
                }
                ntry.rouDst = tabRtrmapN.string2rd(cmd.word());
                rdr.putStrArr(r.bgp.getAllRoutes(sfi, ntry));
                return;
            }
            if (a.equals("compare")) {
                addrIP adr = new addrIP();
                adr.fromString(cmd.word());
                rtrBgpNeigh nei1 = r.bgp.findPeer(adr);
                if (nei1 == null) {
                    cmd.error("no such neighbor");
                    return;
                }
                adr = new addrIP();
                adr.fromString(cmd.word());
                rtrBgpNeigh nei2 = r.bgp.findPeer(adr);
                if (nei2 == null) {
                    cmd.error("no such neighbor");
                    return;
                }
                int ign = 0;
                for (;;) {
                    a = cmd.word();
                    if (a.length() < 1) {
                        break;
                    }
                    if (a.equals("cluster")) {
                        ign |= 0x1;
                    }
                    if (a.equals("nexthop")) {
                        ign |= 0x2;
                    }
                    if (a.equals("origin")) {
                        ign |= 0x4;
                    }
                    if (a.equals("metric")) {
                        ign |= 0x8;
                    }
                    if (a.equals("locpref")) {
                        ign |= 0x10;
                    }
                    if (a.equals("distance")) {
                        ign |= 0x20;
                    }
                    if (a.equals("tag")) {
                        ign |= 0x40;
                    }
                    if (a.equals("validity")) {
                        ign |= 0x80;
                    }
                    if (a.equals("aspath")) {
                        ign |= 0x100;
                    }
                    if (a.equals("asconf")) {
                        ign |= 0x200;
                    }
                    if (a.equals("stdcomm")) {
                        ign |= 0x400;
                    }
                    if (a.equals("extcomm")) {
                        ign |= 0x800;
                    }
                    if (a.equals("aigp")) {
                        ign |= 0x1000;
                    }
                    if (a.equals("bandwidth")) {
                        ign |= 0x2000;
                    }
                    if (a.equals("label")) {
                        ign |= 0x4000;
                    }
                    if (a.equals("aggregate")) {
                        ign |= 0x8000;
                    }
                    if (a.equals("orignted")) {
                        ign |= 0x10000;
                    }
                    if (a.equals("pmsi")) {
                        ign |= 0x20000;
                    }
                    if (a.equals("segrout")) {
                        ign |= 0x40000;
                    }
                    if (a.equals("lrgcomm")) {
                        ign |= 0x80000;
                    }
                    if (a.equals("tunnel")) {
                        ign |= 0x100000;
                    }
                    if (a.equals("attrset")) {
                        ign |= 0x200000;
                    }
                    if (a.equals("bier")) {
                        ign |= 0x400000;
                    }
                }
                tabRoute<addrIP> acc1 = nei1.getAccepted(sfi);
                tabRoute<addrIP> acc2 = nei2.getAccepted(sfi);
                if ((acc1 == null) || (acc2 == null)) {
                    return;
                }
                tabRoute<addrIP> uni1 = new tabRoute<addrIP>("tab");
                tabRoute<addrIP> uni2 = new tabRoute<addrIP>("tab");
                tabRoute<addrIP> diff = new tabRoute<addrIP>("tab");
                compareTables(uni1, diff, acc1, acc2, ign);
                compareTables(uni2, diff, acc2, acc1, ign);
                cmd.error("unique to " + nei1);
                doShowRoutes(r.bgp.fwdCore, uni1, dsp);
                cmd.error("unique to " + nei2);
                doShowRoutes(r.bgp.fwdCore, uni2, dsp);
                cmd.error("attribute differs");
                doShowRoutes(r.bgp.fwdCore, diff, dsp);
                return;
            }
            tabRoute<addrIP> tab = r.bgp.getDatabase(sfi);
            if (tab == null) {
                return;
            }
            if (a.equals("database")) {
                doShowRoutes(r.bgp.fwdCore, tab, dsp);
                return;
            }
            if (a.equals("labels")) {
                doShowRoutes(r.bgp.fwdCore, tab, dsp + 1000);
                return;
            }
            if (a.equals("stdcomm")) {
                a = cmd.getRemaining();
                cmd = new cmds("", "");
                tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
                tabRtrmapN ntry = new tabRtrmapN();
                ntry.stdCommMatch = tabRtrmapN.string2stdComms(a);
                roumap.add(ntry);
                tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
                tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, res, tab, roumap, null, null);
                doShowRoutes(r.bgp.fwdCore, res, dsp);
                return;
            }
            if (a.equals("extcomm")) {
                a = cmd.getRemaining();
                cmd = new cmds("", "");
                tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
                tabRtrmapN ntry = new tabRtrmapN();
                ntry.extCommMatch = tabRtrmapN.string2extComms(a);
                roumap.add(ntry);
                tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
                tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, res, tab, roumap, null, null);
                doShowRoutes(r.bgp.fwdCore, res, dsp);
                return;
            }
            if (a.equals("lrgcomm")) {
                a = cmd.getRemaining();
                cmd = new cmds("", "");
                tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
                tabRtrmapN ntry = new tabRtrmapN();
                ntry.lrgCommMatch = tabRtrmapN.string2lrgComms(a);
                roumap.add(ntry);
                tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
                tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, res, tab, roumap, null, null);
                doShowRoutes(r.bgp.fwdCore, res, dsp);
                return;
            }
            if (a.equals("rd")) {
                a = cmd.getRemaining();
                cmd = new cmds("", "");
                tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
                tabRtrmapN ntry = new tabRtrmapN();
                ntry.rouDstMatch = tabRtrmapN.string2rd(a);
                roumap.add(ntry);
                tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
                tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, res, tab, roumap, null, null);
                doShowRoutes(r.bgp.fwdCore, res, dsp);
                return;
            }
            if (a.equals("regexp")) {
                a = cmd.getRemaining();
                cmd = new cmds("", "");
                tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
                tabRtrmapN ntry = new tabRtrmapN();
                ntry.aspathMatch = a;
                roumap.add(ntry);
                tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
                tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, res, tab, roumap, null, null);
                doShowRoutes(r.bgp.fwdCore, res, dsp);
                return;
            }
            if (a.equals("pathlen")) {
                a = cmd.getRemaining();
                cmd = new cmds("", "");
                tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
                tabRtrmapN ntry = new tabRtrmapN();
                ntry.pathlenMatch = new tabIntMatcher();
                ntry.pathlenMatch.fromString(a);
                roumap.add(ntry);
                tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
                tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, res, tab, roumap, null, null);
                doShowRoutes(r.bgp.fwdCore, res, dsp);
                return;
            }
            if (a.equals("distance")) {
                a = cmd.getRemaining();
                cmd = new cmds("", "");
                tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
                tabRtrmapN ntry = new tabRtrmapN();
                ntry.distanceMatch = new tabIntMatcher();
                ntry.distanceMatch.fromString(a);
                roumap.add(ntry);
                tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
                tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, res, tab, roumap, null, null);
                doShowRoutes(r.bgp.fwdCore, res, dsp);
                return;
            }
            if (a.equals("locpref")) {
                a = cmd.getRemaining();
                cmd = new cmds("", "");
                tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
                tabRtrmapN ntry = new tabRtrmapN();
                ntry.locPrefMatch = new tabIntMatcher();
                ntry.locPrefMatch.fromString(a);
                roumap.add(ntry);
                tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
                tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, res, tab, roumap, null, null);
                doShowRoutes(r.bgp.fwdCore, res, dsp);
                return;
            }
            if (a.equals("validity")) {
                a = cmd.getRemaining();
                cmd = new cmds("", "");
                tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
                tabRtrmapN ntry = new tabRtrmapN();
                ntry.validityMatch = new tabIntMatcher();
                ntry.validityMatch.fromString(a);
                roumap.add(ntry);
                tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
                tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, res, tab, roumap, null, null);
                doShowRoutes(r.bgp.fwdCore, res, dsp);
                return;
            }
            if (a.equals("aigp")) {
                a = cmd.getRemaining();
                cmd = new cmds("", "");
                tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
                tabRtrmapN ntry = new tabRtrmapN();
                ntry.accIgpMatch = new tabIntMatcher();
                ntry.accIgpMatch.fromString(a);
                roumap.add(ntry);
                tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
                tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, res, tab, roumap, null, null);
                doShowRoutes(r.bgp.fwdCore, res, dsp);
                return;
            }
            if (a.equals("bandwidth")) {
                a = cmd.getRemaining();
                cmd = new cmds("", "");
                tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
                tabRtrmapN ntry = new tabRtrmapN();
                ntry.bandwidthMatch = new tabIntMatcher();
                ntry.bandwidthMatch.fromString(a);
                roumap.add(ntry);
                tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
                tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, res, tab, roumap, null, null);
                doShowRoutes(r.bgp.fwdCore, res, dsp);
                return;
            }
            if (a.equals("origin")) {
                a = cmd.getRemaining();
                cmd = new cmds("", "");
                tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
                tabRtrmapN ntry = new tabRtrmapN();
                ntry.originMatch = new tabIntMatcher();
                ntry.originMatch.fromString(a);
                roumap.add(ntry);
                tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
                tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, res, tab, roumap, null, null);
                doShowRoutes(r.bgp.fwdCore, res, dsp);
                return;
            }
            if (a.equals("metric")) {
                a = cmd.getRemaining();
                cmd = new cmds("", "");
                tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
                tabRtrmapN ntry = new tabRtrmapN();
                ntry.metricMatch = new tabIntMatcher();
                ntry.metricMatch.fromString(a);
                roumap.add(ntry);
                tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
                tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, res, tab, roumap, null, null);
                doShowRoutes(r.bgp.fwdCore, res, dsp);
                return;
            }
            if (a.equals("tag")) {
                a = cmd.getRemaining();
                cmd = new cmds("", "");
                tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
                tabRtrmapN ntry = new tabRtrmapN();
                ntry.tagMatch = new tabIntMatcher();
                ntry.tagMatch.fromString(a);
                roumap.add(ntry);
                tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
                tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, res, tab, roumap, null, null);
                doShowRoutes(r.bgp.fwdCore, res, dsp);
                return;
            }
            if (a.equals("network")) {
                a = cmd.getRemaining();
                cmd = new cmds("", "");
                tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
                tabRtrmapN ntry = new tabRtrmapN();
                ntry.networkMatch = new tabPrfxlstN();
                ntry.networkMatch.fromString(a);
                roumap.add(ntry);
                tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
                tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, res, tab, roumap, null, null);
                doShowRoutes(r.bgp.fwdCore, res, dsp);
                return;
            }
            if (a.equals("nexthop")) {
                a = cmd.getRemaining();
                cmd = new cmds("", "");
                tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
                tabRtrmapN ntry = new tabRtrmapN();
                ntry.nexthopMatch = new addrIP();
                ntry.nexthopMatch.fromString(a);
                roumap.add(ntry);
                tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
                tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, res, tab, roumap, null, null);
                doShowRoutes(r.bgp.fwdCore, res, dsp);
                return;
            }
            if (a.equals("prefix-list")) {
                a = cmd.word();
                cfgPrfxlst fnd = cfgAll.prfxFind(a, false);
                if (fnd == null) {
                    cmd.error("no such prefix list");
                    return;
                }
                tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
                tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, res, tab, null, null, fnd.prflst);
                doShowRoutes(r.bgp.fwdCore, res, dsp);
                return;
            }
            if (a.equals("route-map")) {
                a = cmd.word();
                cfgRoump fnd = cfgAll.rtmpFind(a, false);
                if (fnd == null) {
                    cmd.error("no such route map");
                    return;
                }
                tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
                tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, res, tab, fnd.roumap, null, null);
                doShowRoutes(r.bgp.fwdCore, res, dsp);
                return;
            }
            if (a.equals("route-policy")) {
                a = cmd.word();
                cfgRouplc fnd = cfgAll.rtplFind(a, false);
                if (fnd == null) {
                    cmd.error("no such route policy");
                    return;
                }
                tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
                tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, res, tab, null, fnd.rouplc, null);
                doShowRoutes(r.bgp.fwdCore, res, dsp);
                return;
            }
            cmd.badCmd();
            return;
        }
        if (!a.equals("neighbor")) {
            cmd.badCmd();
            return;
        }
        addrIP adr = new addrIP();
        adr.fromString(cmd.word());
        rtrBgpNeigh nei = r.bgp.findPeer(adr);
        if (nei == null) {
            cmd.error("no such neighbor");
            return;
        }
        a = cmd.word();
        if (a.equals("config")) {
            List<String> l = new ArrayList<String>();
            nei.getConfig(l, "", false);
            rdr.putStrArr(l);
            return;
        }
        if (a.equals("status")) {
            List<String> l = new ArrayList<String>();
            nei.getStatus(l);
            rdr.putStrArr(l);
            return;
        }
        sfi = rtrBgpParam.string2mask(a);
        if (sfi < 1) {
            return;
        }
        int dsp = bgpMask2filter(sfi);
        sfi = r.bgp.mask2safi(sfi);
        if (sfi < 1) {
            return;
        }
        a = cmd.word();
        if (a.equals("learned")) {
            doShowRoutes(r.bgp.fwdCore, nei.conn.getLearned(sfi), dsp);
            return;
        }
        if (a.equals("accepted")) {
            doShowRoutes(r.bgp.fwdCore, nei.getAccepted(sfi), dsp);
            return;
        }
        if (a.equals("willing")) {
            doShowRoutes(r.bgp.fwdCore, nei.getWilling(sfi), dsp);
            return;
        }
        if (a.equals("advertised")) {
            doShowRoutes(r.bgp.fwdCore, nei.conn.getAdverted(sfi), dsp);
            return;
        }
        cmd.badCmd();
        return;
    }

    private int bgpMask2filter(int mask) {
        switch (mask) {
            case rtrBgpParam.mskEvpn:
            case rtrBgpParam.mskMspw:
            case rtrBgpParam.mskMdt:
            case rtrBgpParam.mskMvpn:
            case rtrBgpParam.mskMvpo:
            case rtrBgpParam.mskFlw:
            case rtrBgpParam.mskVpnF:
            case rtrBgpParam.mskVpoF:
                return 5;
            default:
                return 2;
        }
    }

    private void doShowIpXpbr(int ver) {
        ipFwd fwd = findVrf(ver);
        if (fwd == null) {
            return;
        }
        rdr.putStrArr(fwd.pbrCfg.getStats());
    }

    private void doShowIpXnat(int ver) {
        ipFwd fwd = findVrf(ver);
        if (fwd == null) {
            return;
        }
        String a = cmd.word();
        if (a.equals("translations")) {
            userFormat l = new userFormat("|", "proto|origSrc|origTrg|newSrc|newTrg");
            for (int i = 0; i < fwd.natTrns.size(); i++) {
                l.add("" + fwd.natTrns.get(i));
            }
            rdr.putStrTab(l);
            return;
        }
        if (a.equals("statistics")) {
            rdr.putStrArr(fwd.natCfg.getStats());
            return;
        }
        cmd.badCmd();
        return;
    }

    private void doShowIpXvrf(int ver) {
        if (cmd.size() > 0) {
            ipFwd fwd = findVrf(ver);
            if (fwd == null) {
                return;
            }
            doShowHistory(cmd.word(), fwd.hstry);
            return;
        }
        userFormat l = new userFormat("|", "vrf|interface|unicast|multicast|label|mroute|flwspc|p2p|mp2mp|nat|proto|pack|byte");
        for (int i = 0; i < cfgAll.vrfs.size(); i++) {
            cfgVrf v = cfgAll.vrfs.get(i);
            String s;
            if (ver == 4) {
                s = ipFwdTab.vrfListShow(v.fwd4);
            } else {
                s = ipFwdTab.vrfListShow(v.fwd6);
            }
            l.add(v.name + "|" + s);
        }
        rdr.putStrTab(l);
    }

    private void doShowSockets(int ver) {
        cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
        if (vrf == null) {
            cmd.error("no such vrf");
            return;
        }
        rdr.putStrTab(vrf.getShSockets(ver));
    }

    private void doShowProtocols(int ver) {
        ipFwd fwd = findVrf(ver);
        if (fwd == null) {
            return;
        }
        rdr.putStrTab(ipFwdTab.statisticShow(fwd));
        rdr.putStrTab(ipFwdTab.routersShow(fwd));
    }

    private void doShowMroute(int ver) {
        ipFwd fwd = findVrf(ver);
        if (fwd == null) {
            return;
        }
        userFormat l = new userFormat("|", "source|group|interface|upstream|targets");
        for (int o = 0; o < fwd.groups.size(); o++) {
            ipFwdMcast grp = fwd.groups.get(o);
            if (grp == null) {
                continue;
            }
            String s = "";
            for (int i = 0; i < grp.flood.size(); i++) {
                ipFwdIface ifc = grp.flood.get(i);
                s += " " + ifc;
            }
            if (grp.local) {
                s += " local";
            }
            if (grp.label != null) {
                s += " label";
            }
            if (grp.bier != null) {
                s += " bier";
            }
            l.add(grp.source + "|" + grp.group + "|" + grp.iface + "|" + grp.upstream + "|" + s);
        }
        rdr.putStrTab(l);
    }

    private void doShowCounter(int ver) {
        ipFwd fwd = findVrf(ver);
        if (fwd == null) {
            return;
        }
        doShowRoutes(fwd, fwd.actualU, 6);
    }

    private void doShowRouteU(int ver) {
        ipFwd fwd = findVrf(ver);
        if (fwd == null) {
            return;
        }
        doShowRoutes(fwd, fwd.actualU, 1);
    }

    private void doShowRouteSR(int ver) {
        ipFwd fwd = findVrf(ver);
        if (fwd == null) {
            return;
        }
        doShowRoutes(fwd, fwd.actualU, 7);
    }

    private void doShowRouteBR(int ver) {
        ipFwd fwd = findVrf(ver);
        if (fwd == null) {
            return;
        }
        doShowRoutes(fwd, fwd.actualU, 8);
    }

    private void doShowRouteM(int ver) {
        ipFwd fwd = findVrf(ver);
        if (fwd == null) {
            return;
        }
        doShowRoutes(fwd, fwd.actualM, 1);
    }

    private void doShowRouteF(int ver) {
        ipFwd fwd = findVrf(ver);
        if (fwd == null) {
            return;
        }
        doShowRoutes(fwd, fwd.actualF, 5);
    }

    private void doShowRoutes(ipFwd fwd, tabRoute<addrIP> tab, int typ) {
        String s = cmd.word();
        if (s.length() > 0) {
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = addrPrefix.str2ip(s);
            if (ntry.prefix == null) {
                addrIP adr = new addrIP();
                if (adr.fromString(s)) {
                    cmd.error("bad prefix");
                    return;
                }
                ntry = tab.route(adr);
            } else {
                ntry.rouDst = tabRtrmapN.string2rd(cmd.word());
                ntry = tab.find(ntry);
            }
            if (ntry == null) {
                cmd.error("no such prefix");
                return;
            }
            rdr.putStrArr(ntry.fullDump(fwd));
            return;
        }
        userFormat l;
        switch (typ) {
            case 1:
                l = new userFormat("|", "typ|prefix|metric|iface|hop|time");
                break;
            case 2:
            case 5:
                l = new userFormat("|", "prefix|hop|metric|aspath");
                break;
            case 1002:
            case 1005:
                l = new userFormat("|", "prefix|local|evpn*16|pmsi*16|remote|hop");
                break;
            case 3:
                l = new userFormat("|", "prefix|local|remote|hop");
                break;
            case 4:
                l = new userFormat("|", "prefix|max|as");
                break;
            case 6:
                l = new userFormat("|", "prefix|time|traffic");
                break;
            case 7:
                l = new userFormat("|", "prefix|index|base|oldbase");
                break;
            case 8:
                l = new userFormat("|", "prefix|index|base|oldbase|size");
                break;
            default:
                return;
        }
        for (int i = 0; i < tab.size(); i++) {
            tabRouteEntry<addrIP> prf = tab.get(i);
            if (prf == null) {
                continue;
            }
            switch (typ) {
                case 1:
                    l.add(tabRouteEntry.toShRoute(prf));
                    break;
                case 2:
                    l.add(tabRouteEntry.toShBgp(prf));
                    break;
                case 3:
                    l.add(tabRouteEntry.toShLdp(prf));
                    break;
                case 1002:
                case 1005:
                    l.add(tabRouteEntry.toShBgpLabels(prf, typ == 1005));
                    break;
                case 4:
                    l.add(tabRouteEntry.toShRpki(prf));
                    break;
                case 5:
                    l.add(tabRouteEntry.toShEvpn(prf));
                    break;
                case 6:
                    String a = tabRouteEntry.toShCntr(prf);
                    if (a == null) {
                        continue;
                    }
                    l.add(a);
                    break;
                case 7:
                    a = tabRouteEntry.toShSrRoute(prf);
                    if (a == null) {
                        continue;
                    }
                    l.add(a);
                    break;
                case 8:
                    a = tabRouteEntry.toShBrRoute(prf);
                    if (a == null) {
                        continue;
                    }
                    l.add(a);
                    break;
            }
        }
        rdr.putStrTab(l);
    }

    private void doShowMptab(tabGen<ipFwdMpmp> tab) {
        userFormat l = new userFormat("|", "type|root|opaque|uplink|peers");
        for (int i = 0; i < tab.size(); i++) {
            ipFwdMpmp ntry = tab.get(i);
            if (ntry == null) {
                continue;
            }
            l.add(ntry.dump());
        }
        rdr.putStrTab(l);
    }

    private userFormat doShowRates(history h) {
        userFormat l = new userFormat("|", "time|send|receive|drop|send|receive|drop");
        l.add(h.getShRate());
        return l;
    }

    private void doShowHistory(String a, history h) {
        if (a.equals("full")) {
            rdr.putStrArr(h.show(1));
            rdr.putStrArr(h.show(5));
            return;
        }
        if (a.equals("rates")) {
            rdr.putStrTab(doShowRates(h));
            return;
        }
        if (a.equals("realtime")) {
            rdr.putStrArr(h.show(9));
            return;
        }
        if (a.equals("numhist")) {
            userFormat res = new userFormat("|", "time|avgRx|maxRx|avgTx|maxTx|avgDr|maxDr");
            res.add(h.dump(1, 4));
            rdr.putStrTab(res);
            return;
        }
        if (a.equals("numphist")) {
            userFormat res = new userFormat("|", "time|avgRx|maxRx|avgTx|maxTx|avgDr|maxDr");
            res.add(h.dump(4, 4));
            rdr.putStrTab(res);
            return;
        }
        if (a.equals("history")) {
            rdr.putStrArr(h.show(1));
            return;
        }
        if (a.equals("rxhistory")) {
            rdr.putStrArr(h.show(2));
            return;
        }
        if (a.equals("txhistory")) {
            rdr.putStrArr(h.show(3));
            return;
        }
        if (a.equals("drhistory")) {
            rdr.putStrArr(h.show(4));
            return;
        }
        if (a.equals("phistory")) {
            rdr.putStrArr(h.show(5));
            return;
        }
        if (a.equals("rxphistory")) {
            rdr.putStrArr(h.show(6));
            return;
        }
        if (a.equals("txphistory")) {
            rdr.putStrArr(h.show(7));
            return;
        }
        if (a.equals("drphistory")) {
            rdr.putStrArr(h.show(8));
            return;
        }
        cmd.badCmd();
    }

    private void doShowVrf() {
        if (cmd.size() > 0) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                cmd.error("no such vrf");
                return;
            }
            doShowHistory(cmd.word(), vrf.fwd4.hstry.plus(vrf.fwd6.hstry));
            return;
        }
        userFormat l = new userFormat("|", "name|rd|int4|int6|pack|byte");
        for (int o = 0; o < cfgAll.vrfs.size(); o++) {
            cfgVrf v = cfgAll.vrfs.get(o);
            counter c = v.fwd4.cntr.plus(v.fwd6.cntr);
            l.add(v.name + "|" + tabRtrmapN.rd2string(v.rd) + "|" + v.fwd4.ifaces.size() + "|" + v.fwd6.ifaces.size() + "|" + c.packRx + "|" + c.byteRx);
        }
        rdr.putStrTab(l);
    }

}
