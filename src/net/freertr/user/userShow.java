package net.freertr.user;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrIPv4;
import net.freertr.addr.addrIPv6;
import net.freertr.addr.addrIpx;
import net.freertr.addr.addrPrefix;
import net.freertr.auth.authGeneric;
import net.freertr.auth.authResult;
import net.freertr.cfg.cfgAceslst;
import net.freertr.cfg.cfgAlias;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgAuther;
import net.freertr.cfg.cfgBndl;
import net.freertr.cfg.cfgBrdg;
import net.freertr.cfg.cfgCheck;
import net.freertr.cfg.cfgDial;
import net.freertr.cfg.cfgGeneric;
import net.freertr.cfg.cfgIfc;
import net.freertr.cfg.cfgInit;
import net.freertr.cfg.cfgMtrack;
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
import net.freertr.cfg.cfgSessn;
import net.freertr.cfg.cfgTrack;
import net.freertr.cfg.cfgVdc;
import net.freertr.cfg.cfgVdcIfc;
import net.freertr.cfg.cfgVrf;
import net.freertr.clnt.clntDns;
import net.freertr.clnt.clntNetflow;
import net.freertr.clnt.clntWhois;
import net.freertr.ifc.ifcThread;
import net.freertr.ip.ipFwd;
import net.freertr.ip.ipFwdIface;
import net.freertr.ip.ipFwdMcast;
import net.freertr.ip.ipFwdMpmp;
import net.freertr.ip.ipFwdTab;
import net.freertr.ip.ipFwdTrfng;
import net.freertr.ip.ipRtr;
import net.freertr.pack.packDnsZone;
import net.freertr.pack.packHolder;
import net.freertr.pack.packLdpMp;
import net.freertr.pack.packLdpPwe;
import net.freertr.pipe.pipeSetting;
import net.freertr.prt.prtRedun;
import net.freertr.prt.prtWatch;
import net.freertr.rtr.rtrBabelNeigh;
import net.freertr.rtr.rtrBgpGroup;
import net.freertr.rtr.rtrBgpNeigh;
import net.freertr.rtr.rtrBgpParam;
import net.freertr.rtr.rtrBgpTemp;
import net.freertr.rtr.rtrBgpUtil;
import net.freertr.rtr.rtrEigrpNeigh;
import net.freertr.rtr.rtrLdpNeigh;
import net.freertr.rtr.rtrLogger;
import net.freertr.rtr.rtrOlsrNeigh;
import net.freertr.rtr.rtrPvrpNeigh;
import net.freertr.rtr.rtrRip4neigh;
import net.freertr.rtr.rtrRip6neigh;
import net.freertr.serv.servBmp2mrt;
import net.freertr.serv.servDhcp4;
import net.freertr.serv.servDhcp6;
import net.freertr.serv.servDns;
import net.freertr.serv.servHttp;
import net.freertr.serv.servNetflow;
import net.freertr.serv.servSmtp;
import net.freertr.serv.servStreamingMdt;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabIndex;
import net.freertr.tab.tabIntMatcher;
import net.freertr.tab.tabLabel;
import net.freertr.tab.tabLabelEntry;
import net.freertr.tab.tabListing;
import net.freertr.tab.tabListingEntry;
import net.freertr.tab.tabNshEntry;
import net.freertr.tab.tabPrfxlstN;
import net.freertr.tab.tabQos;
import net.freertr.tab.tabRoute;
import net.freertr.tab.tabRouteAttr;
import net.freertr.tab.tabRouteEntry;
import net.freertr.tab.tabRtrmapN;
import net.freertr.tab.tabSession;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.counter;
import net.freertr.util.differ;
import net.freertr.util.history;
import net.freertr.util.logger;
import net.freertr.util.uniResLoc;
import net.freertr.util.verCore;
import net.freertr.util.version;

/**
 * process show commands
 *
 * @author matecsaba
 */
public class userShow {

    /**
     * create instance
     */
    public userShow() {
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
     * current help context
     */
    public userHelping hlp;

    /**
     * current config context
     */
    public cfgGeneric cfg;

    /**
     * do the work
     *
     * @return command to execute, null if nothing
     */
    public cfgAlias doer() {
        String a = cmd.word();
        cfgAlias alias = cfgAll.aliasFind(a, cfgAlias.aliasType.show, false);
        if (alias != null) {
            return alias;
        }
        if (a.equals("version")) {
            a = cmd.word();
            if (a.equals("brief")) {
                rdr.putStrArr(version.shLogo(0x2));
                return null;
            }
            if (a.equals("date-email")) {
                rdr.putStrArr(version.shLogo(0x800));
                return null;
            }
            if (a.equals("date-machine")) {
                rdr.putStrArr(version.shLogo(0x400));
                return null;
            }
            if (a.equals("user-agent")) {
                rdr.putStrArr(version.shLogo(0x1000));
                return null;
            }
            if (a.equals("url")) {
                rdr.putStrArr(version.shLogo(0x2000));
                return null;
            }
            if (a.equals("number")) {
                rdr.putStrArr(version.shLogo(0x200));
                return null;
            }
            rdr.putStrArr(version.shLogo(0xe0));
            return null;
        }
        if (a.equals("history")) {
            rdr.putStrArr(rdr.getHistory());
            return null;
        }
        if (a.equals("parser")) {
            if (hlp == null) {
                return null;
            }
            rdr.putStrArr(hlp.getUsage());
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
        if (a.equals("users")) {
            userFormat res = new userFormat("|", "user|from|since");
            for (int i = 0; i < userLine.loggedUsers.size(); i++) {
                res.add("" + userLine.loggedUsers.get(i));
            }
            rdr.putStrTab(res);
            return null;
        }
        if (a.equals("privilege")) {
            authResult usr = cmd.pipe.settingsGet(pipeSetting.authed, new authResult());
            userFormat lst = usr.dump();
            lst.add("origin|" + cmd.pipe.settingsGet(pipeSetting.origin, "?"));
            rdr.putStrTab(lst);
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
        if (a.equals("aaa")) {
            cfgAuther aa = cfgAll.autherFind(cmd.word(), null);
            if (aa == null) {
                cmd.error("no such aaa");
                return null;
            }
            authGeneric aaa = aa.getAuther();
            if (aaa == null) {
                cmd.error("no such aaa");
                return null;
            }
            rdr.putStrTab(aaa.getShow());
            return null;
        }
        if (a.equals("pppoe")) {
            a = cmd.word();
            cfgIfc ifc = cfgAll.ifcFind(a, false);
            if (ifc == null) {
                cmd.error("no such interface");
                return null;
            }
            if (ifc.pppoeS == null) {
                cmd.error("not enabled");
                return null;
            }
            userFormat l = ifc.pppoeS.getShow();
            rdr.putStrTab(l);
            return null;
        }
        if (a.equals("macsec")) {
            a = cmd.word();
            cfgIfc ifc = cfgAll.ifcFind(a, false);
            if (ifc == null) {
                cmd.error("no such interface");
                return null;
            }
            if (ifc.ethtyp.macSec == null) {
                cmd.error("not enabled");
                return null;
            }
            userFormat l = ifc.ethtyp.macSec.getShow();
            rdr.putStrTab(l);
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
            l.add("machine: " + bits.time2str(cfgAll.timeZoneName, tim, 3));
            l.add("email: " + bits.time2str(cfgAll.timeZoneName, tim, 4));
            int i = (int) (cfgAll.timeServerOffset % 1000);
            if (i < 0) {
                i = -i;
            }
            l.add("zone: " + cfgAll.timeZoneName + " diff: " + bits.timeDump(cfgAll.timeServerOffset / 1000) + "." + bits.padBeg("" + i, 3, "0"));
            rdr.putStrArr(l);
            return null;
        }
        if (a.equals("scheduler")) {
            userFormat l = new userFormat("|", "name|rerun|last|ago");
            for (int i = 0; i < cfgAll.schedulers.size(); i++) {
                cfgSched ntry = cfgAll.schedulers.get(i);
                l.add(ntry.name + "|" + ntry.restartC + "|" + bits.time2str(cfgAll.timeZoneName, ntry.restartT + cfgAll.timeServerOffset, 3) + "|" + bits.timePast(ntry.restartT));
            }
            rdr.putStrTab(l);
            return null;
        }
        if (a.equals("script")) {
            userFormat l = new userFormat("|", "name|rerun|last|ago");
            for (int i = 0; i < cfgAll.scripts.size(); i++) {
                cfgScrpt ntry = cfgAll.scripts.get(i);
                l.add(ntry.name + "|" + ntry.restartC + "|" + bits.time2str(cfgAll.timeZoneName, ntry.restartT + cfgAll.timeServerOffset, 3) + "|" + bits.timePast(ntry.restartT));
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
                    l.add(ntry.name + "|" + ntry.restartC + "|" + bits.time2str(cfgAll.timeZoneName, ntry.restartT + cfgAll.timeServerOffset, 3) + "|" + bits.timePast(ntry.restartT));
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
                rdr.putStrTab(logger.listThreads());
                return null;
            }
            if (a.equals("external")) {
                userFormat l = new userFormat("|", "name|rerun|last|ago");
                for (int i = 0; i < cfgAll.prcs.size(); i++) {
                    cfgPrcss ntry = cfgAll.prcs.get(i);
                    l.add(ntry.name + "|" + ntry.restartC + "|" + bits.time2str(cfgAll.timeZoneName, ntry.restartT + cfgAll.timeServerOffset, 3) + "|" + bits.timePast(ntry.restartT));
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
            rdr.putStrTab(clntDns.showLocalCache(false));
            rdr.putStrTab(clntDns.showLocalCache(true));
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
            if (a.equals("gc")) {
                rdr.putStrTab(logger.listGcs());
                return null;
            }
            if (a.equals("sys")) {
                rdr.putStrTab(logger.listSys());
                return null;
            }
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
            if (a.equals("file")) {
                rdr.putStrArr(bits.txt2buf(logger.logFilNam));
                return null;
            }
            if (a.equals("old-file")) {
                rdr.putStrArr(bits.txt2buf(logger.logRotNam));
                return null;
            }
            if (a.equals("last")) {
                rdr.putStrArr(logger.bufferRead(bits.str2num(cmd.word())));
                return null;
            }
            rdr.putStrArr(logger.bufferRead());
            return null;
        }
        if (a.equals("rollback-config")) {
            rdr.putStrArr(userFilter.getDiffs(cfgAll.getShRun(1), bits.txt2buf(cfgInit.cfgFileSw)));
            return null;
        }
        if (a.equals("config-differences")) {
            rdr.putStrArr(userFilter.getDiffs(bits.txt2buf(cfgInit.cfgFileSw), cfgAll.getShRun(1)));
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
            if (a.equals("this")) {
                if (cfg == null) {
                    return null;
                }
                rdr.putStrArr(cfg.getShRun(getConfigFilter(null, cmd)));
                return null;
            }
            if (a.equals("all")) {
                rdr.putStrArr(cfgAll.getShRun(getConfigFilter(a, cmd)));
                return null;
            }
            if (a.equals("hide")) {
                rdr.putStrArr(cfgAll.getShRun(getConfigFilter(a, cmd)));
                return null;
            }
            if (a.equals("router")) {
                tabRouteAttr.routeType t = cfgRtr.name2num(cmd.word());
                if (t == null) {
                    cmd.error("bad router type");
                    return null;
                }
                int i = bits.str2num(cmd.word());
                cfgRtr r = cfgAll.rtrFind(t, i, false);
                if (r == null) {
                    cmd.error("no such process");
                    return null;
                }
                int filt = getConfigFilter(null, cmd);
                rdr.putStrArr(r.getShRun(filt));
                return null;
            }
            if (a.equals("interface")) {
                cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
                if (ifc == null) {
                    cmd.error("no such interface");
                    return null;
                }
                rdr.putStrArr(ifc.getShRun(getConfigFilter(null, cmd)));
                return null;
            }
            List<String> lst = cfgAll.getShRun(1);
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
            a = cmd.word();
            if (a.equals("traffic")) {
                doShowVrfTraff();
                return null;
            }
            if (a.equals("icmp")) {
                doShowVrfIcmp();
                return null;
            }
            if (a.equals("routing")) {
                doShowVrfRout();
                return null;
            }
            cmd.badCmd();
            return null;
        }
        if (a.equals("dns")) {
            servDns srv = cfgAll.srvrFind(new servDns(), cfgAll.dmnDns, cmd.word());
            if (srv == null) {
                cmd.error("no such server");
                return null;
            }
            packDnsZone ntry = new packDnsZone(cmd.word());
            ntry = srv.zones.find(ntry);
            if (ntry == null) {
                cmd.error("no such zone");
                return null;
            }
            rdr.putStrTab(ntry.toUserStr(true));
            return null;
        }
        if (a.equals("http")) {
            servHttp srv = cfgAll.srvrFind(new servHttp(), cfgAll.dmnHttp, cmd.word());
            if (srv == null) {
                cmd.error("no such server");
                return null;
            }
            rdr.putStrTab(srv.getShow());
            return null;
        }
        if (a.equals("smtp")) {
            servSmtp srv = cfgAll.srvrFind(new servSmtp(), cfgAll.dmnSmtp, cmd.word());
            if (srv == null) {
                cmd.error("no such server");
                return null;
            }
            rdr.putStrTab(srv.getShow());
            return null;
        }
        if (a.equals("dhcp4")) {
            servDhcp4 srv = cfgAll.srvrFind(new servDhcp4(), cfgAll.dmnDhcp4, cmd.word());
            if (srv == null) {
                cmd.error("no such server");
                return null;
            }
            rdr.putStrTab(srv.getShow());
            return null;
        }
        if (a.equals("dhcp6")) {
            servDhcp6 srv = cfgAll.srvrFind(new servDhcp6(), cfgAll.dmnDhcp6, cmd.word());
            if (srv == null) {
                cmd.error("no such server");
                return null;
            }
            rdr.putStrTab(srv.getShow());
            return null;
        }
        if (a.equals("sensor")) {
            a = cmd.word();
            if (a.length() < 1) {
                userFormat l = new userFormat("|", "name|rep|time|last");
                for (int i = 0; i < cfgAll.sensors.size(); i++) {
                    cfgSensor exp = cfgAll.sensors.get(i);
                    l.add(exp.name + "|" + exp.cnt + "|" + exp.time + "|" + bits.timePast(exp.last));
                }
                rdr.putStrTab(l);
                return null;
            }
            cfgSensor exp = cfgAll.sensorFind(a, false);
            if (exp == null) {
                cmd.error("no such exporter");
                return null;
            }
            rdr.putStrArr(exp.getShow());
            return null;
        }
        if (a.equals("session")) {
            cfgSessn ntry = cfgAll.sessnFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such session");
                return null;
            }
            doShowSession(ntry.connects);
            return null;
        }
        if (a.equals("netflow")) {
            servNetflow srv = cfgAll.srvrFind(new servNetflow(), cfgAll.dmnNetflow, cmd.word());
            if (srv == null) {
                cmd.error("no such server");
                return null;
            }
            doShowSession(srv.connects);
            return null;
        }
        if (a.equals("streamingmdt")) {
            servStreamingMdt srv = cfgAll.srvrFind(new servStreamingMdt(), cfgAll.dmnStreamingMdt, cmd.word());
            if (srv == null) {
                cmd.error("no such server");
                return null;
            }
            if (cmd.size() < 1) {
                rdr.putStrTab(srv.getShow());
                return null;
            }
            addrIP adr = new addrIP();
            adr.fromString(cmd.word());
            if (cmd.size() < 1) {
                rdr.putStrTab(srv.getShow(adr));
                return null;
            }
            String p = cmd.word();
            String k = cmd.word();
            rdr.putStrTab(srv.getShow(adr, p, k));
            return null;
        }
        if (a.equals("check")) {
            a = cmd.word();
            if (a.length() < 1) {
                userFormat l = new userFormat("|", "name|state|asked|reply|times|last|times|last", "4|2pass|2fail");
                for (int i = 0; i < cfgAll.checks.size(); i++) {
                    cfgCheck ntry = cfgAll.checks.get(i);
                    l.add(ntry.name + "|" + (ntry.doCheck().size() < 1) + "|" + (ntry.okNum + ntry.errNum) + "|" + ntry.time + "|" + ntry.okNum + "|" + bits.timePast(ntry.okTim) + "|" + ntry.errNum + "|" + bits.timePast(ntry.errTim));
                }
                rdr.putStrTab(l);
                return null;
            }
            cfgCheck srv = cfgAll.checkFind(a, false);
            if (srv == null) {
                cmd.error("no such check");
                return null;
            }
            rdr.putStrArr(srv.getShow());
            return null;
        }
        if (a.equals("dashboard")) {
            List<String> rep = new ArrayList<String>();
            for (;;) {
                a = cmd.word();
                if (a.length() < 1) {
                    break;
                }
                if (a.equals("replace")) {
                    rep.add(cmd.word());
                    rep.add(cmd.word());
                    continue;
                }
                if (a.equals("text")) {
                    rdr.putStrArr(getDashText(cmd.word(), rep));
                    continue;
                }
                if (a.equals("iface")) {
                    rdr.putStrArr(getDashIfc(cmd.word(), rep));
                    continue;
                }
                if (a.equals("vrf")) {
                    rdr.putStrArr(getDashVrf(cmd.word(), rep));
                    continue;
                }
                if (a.equals("router")) {
                    rdr.putStrArr(getDashRtr(cmd.word(), rep));
                    continue;
                }
            }
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
            if (cmd.size() < 1) {
                rdr.putStrTab(srv.getShow(frm));
                return null;
            }
            a = cmd.word();
            addrIP per = new addrIP();
            per.fromString(a);
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
                userFormat res = new userFormat("|", "category|value");
                res.add(ifc.lldp.getShNeigh(true));
                rdr.putStrTab(res);
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
                userFormat res = new userFormat("|", "category|value");
                res.add(ifc.udld.getShNeigh(true));
                rdr.putStrTab(res);
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
                userFormat res = new userFormat("|", "category|value");
                res.add(ifc.lacp.getShNeigh(true));
                rdr.putStrTab(res);
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
                userFormat res = new userFormat("|", "category|value");
                res.add(ifc.cdp.getShNeigh(true));
                rdr.putStrTab(res);
                return null;
            }
            cmd.badCmd();
            return null;
        }
        if (a.equals("policy-map")) {
            a = cmd.word();
            if (a.equals("flowspec")) {
                cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
                if (vrf == null) {
                    cmd.error("no such vrf");
                    return null;
                }
                a = cmd.word();
                tabQos tab = null;
                if (a.equals("ipv4")) {
                    tab = vrf.fwd4.flowspec;
                }
                if (a.equals("ipv6")) {
                    tab = vrf.fwd6.flowspec;
                }
                if (tab == null) {
                    cmd.error("no such policy");
                    return null;
                }
                rdr.putStrArr(tab.getStats(true));
                return null;
            }
            if (a.equals("data-plane")) {
                cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
                if (vrf == null) {
                    cmd.error("no such vrf");
                    return null;
                }
                a = cmd.word();
                tabQos tab = null;
                if (a.equals("ipv4")) {
                    tab = vrf.fwd4.dapp;
                }
                if (a.equals("ipv6")) {
                    tab = vrf.fwd6.dapp;
                }
                if (tab == null) {
                    cmd.error("no such policy");
                    return null;
                }
                rdr.putStrArr(tab.getStats(false));
                return null;
            }
            if (a.equals("control-plane")) {
                cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
                if (vrf == null) {
                    cmd.error("no such vrf");
                    return null;
                }
                a = cmd.word();
                tabQos tab = null;
                if (a.equals("ipv4in")) {
                    tab = vrf.fwd4.coppIn;
                }
                if (a.equals("ipv4out")) {
                    tab = vrf.fwd4.coppOut;
                }
                if (a.equals("ipv6in")) {
                    tab = vrf.fwd6.coppIn;
                }
                if (a.equals("ipv6out")) {
                    tab = vrf.fwd6.coppOut;
                }
                if (tab == null) {
                    cmd.error("no such policy");
                    return null;
                }
                rdr.putStrArr(tab.getStats(false));
                return null;
            }
            if (a.equals("interface")) {
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
                rdr.putStrArr(tab.getStats(false));
                return null;
            }
            cmd.badCmd();
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
        if (a.equals("terminal")) {
            rdr.putStrTab(pipeSetting.getInfo(cmd.pipe));
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
            userFormat res = new userFormat("|", "category|value");
            res.add(trck.worker.getShStat());
            rdr.putStrTab(res);
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
            rdr.putStrTab(trck.worker.getShStat());
            rdr.putStrTab(trck.worker.getShPeer());
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
            if (a.equals("hwbwmon")) {
                rdr.putStrArr(cfgAll.getShIntTxt(27));
                return null;
            }
            if (a.equals("swbwmon")) {
                rdr.putStrArr(cfgAll.getShIntTxt(21));
                return null;
            }
            if (a.equals("summary")) {
                rdr.putStrTab(cfgAll.getShIntTab(2));
                return null;
            }
            if (a.equals("hwsummary")) {
                rdr.putStrTab(cfgAll.getShIntTab(15));
                return null;
            }
            if (a.equals("swsummary")) {
                rdr.putStrTab(cfgAll.getShIntTab(19));
                return null;
            }
            if (a.equals("total")) {
                rdr.putStrTab(cfgAll.getShIntTab(10));
                return null;
            }
            if (a.equals("hwtotal")) {
                rdr.putStrTab(cfgAll.getShIntTab(23));
                return null;
            }
            if (a.equals("swtotal")) {
                rdr.putStrTab(cfgAll.getShIntTab(25));
                return null;
            }
            if (a.equals("traffic")) {
                rdr.putStrTab(cfgAll.getShIntTab(9));
                return null;
            }
            if (a.equals("hwtraffic")) {
                rdr.putStrTab(cfgAll.getShIntTab(17));
                return null;
            }
            if (a.equals("swtraffic")) {
                rdr.putStrTab(cfgAll.getShIntTab(21));
                return null;
            }
            if (a.equals("psummary")) {
                rdr.putStrTab(cfgAll.getShIntTab(11));
                return null;
            }
            if (a.equals("hwpsummary")) {
                rdr.putStrTab(cfgAll.getShIntTab(16));
                return null;
            }
            if (a.equals("swpsummary")) {
                rdr.putStrTab(cfgAll.getShIntTab(20));
                return null;
            }
            if (a.equals("ptraffic")) {
                rdr.putStrTab(cfgAll.getShIntTab(12));
                return null;
            }
            if (a.equals("hwptraffic")) {
                rdr.putStrTab(cfgAll.getShIntTab(18));
                return null;
            }
            if (a.equals("swptraffic")) {
                rdr.putStrTab(cfgAll.getShIntTab(22));
                return null;
            }
            if (a.equals("ptotal")) {
                rdr.putStrTab(cfgAll.getShIntTab(13));
                return null;
            }
            if (a.equals("hwptotal")) {
                rdr.putStrTab(cfgAll.getShIntTab(24));
                return null;
            }
            if (a.equals("swptotal")) {
                rdr.putStrTab(cfgAll.getShIntTab(26));
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
                rdr.putStrArr(ifc.getShIntTxt(11));
                rdr.putStrTab(doShowRates(ifc.ethtyp.hwHstry));
                rdr.putStrArr(ifc.getShIntTxt(12));
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
            if (a.equals("hwcounters")) {
                rdr.putStrArr(ifc.getShIntTxt(11));
                return null;
            }
            if (a.length() < 1) {
                rdr.putStrArr(ifc.getShIntTxt(1));
                return null;
            }
            if (a.startsWith("hw")) {
                if (ifc.ethtyp.hwHstry == null) {
                    return null;
                }
                a = a.substring(2, a.length());
                doShowHistory(a, ifc.ethtyp.hwHstry);
                return null;
            }
            doShowHistory(a, ifc.ethtyp.getHistory());
            return null;
        }
        if (a.equals("polka")) {
            a = cmd.word();
            if (a.equals("routeid")) {
                a = cmd.word();
                cfgIfc ntry = cfgAll.ifcFind(a, false);
                if (ntry == null) {
                    cmd.error("no such interface");
                    return null;
                }
                if (ntry.tunPolka == null) {
                    cmd.error("not enabled");
                    return null;
                }
                rdr.putStrTab(ntry.tunPolka.getShRoute());
                rdr.putStrTab(ntry.tunPolka.getShDecode());
                return null;
            }
            if (a.equals("interfaces")) {
                a = cmd.word();
                if (a.length() < 1) {
                    userFormat lst = new userFormat("|", "interface|packet|headend");
                    for (int i = 0; i < cfgAll.ifaces.size(); i++) {
                        cfgIfc ntry = cfgAll.ifaces.get(i);
                        lst.add(ntry.name + "|" + (ntry.polkaPack != null) + "|" + (ntry.tunPolka != null));
                    }
                    rdr.putStrTab(lst);
                    return null;
                }
                cfgIfc ntry = cfgAll.ifcFind(a, false);
                if (ntry == null) {
                    cmd.error("no such interface");
                    return null;
                }
                if (ntry.polkaPack == null) {
                    cmd.error("not enabled");
                    return null;
                }
                rdr.putStrTab(ntry.polkaPack.getShow());
                return null;
            }
            return null;
        }
        if (a.equals("nsh")) {
            a = cmd.word();
            if (a.equals("forwarding")) {
                int p = bits.str2num(cmd.word());
                if (p < 1) {
                    rdr.putStrTab(tabNshEntry.getShFor());
                    return null;
                }
                int i = bits.str2num(cmd.word());
                tabNshEntry ntry = new tabNshEntry(p, i);
                ntry = tabNshEntry.services.find(ntry);
                if (ntry == null) {
                    cmd.error("no such service");
                    return null;
                }
                rdr.putStrTab(ntry.getShow());
                return null;
            }
            if (a.equals("interfaces")) {
                rdr.putStrTab(tabNshEntry.getShInt());
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
                tabLabelEntry ntry = tabLabel.find(i);
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
                doShowSession(ifc.mplsPack.inspect);
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
            tabRouteAttr.routeType typ = cfgRtr.name2num(cmd.word());
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
                    l.add(tabRouteAttr.rouTyp2string(prf.best) + "|" + prf.prefix + "|" + prf.best.distance + "/" + prf.best.metric + "|" + prf.best.iface + "|" + prf.best.nextHop + "|" + bits.timePast(prf.best.time));
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
                if (cmd.size() < 1) {
                    rdr.putStrTab(cfgAll.getShIntTab(4));
                    return null;
                }
                cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
                if (ifc == null) {
                    cmd.error("no such interface");
                    return null;
                }
                doShowIpXifc(ifc.fwdIf4);
                return null;
            }
            if (a.equals("logger")) {
                doShowIpXlogger(tabRouteAttr.routeType.logger4);
                return null;
            }
            if (a.equals("isis")) {
                doShowIpXisis(tabRouteAttr.routeType.isis4);
                return null;
            }
            if (a.equals("pvrp")) {
                doShowIpXpvrp(tabRouteAttr.routeType.pvrp4);
                return null;
            }
            if (a.equals("lsrp")) {
                doShowIpXlsrp(tabRouteAttr.routeType.lsrp4);
                return null;
            }
            if (a.equals("eigrp")) {
                doShowIpXeigrp(tabRouteAttr.routeType.eigrp4);
                return null;
            }
            if (a.equals("ospf")) {
                cfgRtr r = cfgAll.rtrFind(tabRouteAttr.routeType.ospf4, bits.str2num(cmd.word()), false);
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
                if (a.equals("hostnames")) {
                    rdr.putStrTab(r.ospf4.showHostnames(bits.str2num(cmd.word())));
                    return null;
                }
                if (a.equals("tree")) {
                    rdr.putStrArr(r.ospf4.showSpfTree(bits.str2num(cmd.word())));
                    return null;
                }
                if (a.equals("othertree")) {
                    rdr.putStrArr(r.ospf4.showSpfOtherTree(bits.str2num(cmd.word()), cmd));
                    return null;
                }
                if (a.equals("othertopology")) {
                    rdr.putStrTab(r.ospf4.showSpfOtherTopo(bits.str2num(cmd.word()), cmd));
                    return null;
                }
                if (a.equals("graph")) {
                    int i = bits.str2num(cmd.word());
                    boolean nocli = false;
                    boolean nonets = false;
                    boolean noints = false;
                    for (;;) {
                        a = cmd.word();
                        if (a.length() < 1) {
                            break;
                        }
                        if (a.equals("nocli")) {
                            nocli = true;
                            continue;
                        }
                        if (a.equals("nonets")) {
                            nonets = true;
                            continue;
                        }
                        if (a.equals("noints")) {
                            noints = true;
                            continue;
                        }
                    }
                    rdr.putStrArr(r.ospf4.showSpfGraph(i, nocli, nonets, noints));
                    return null;
                }
                if (a.equals("nhinconsistent")) {
                    int i = bits.str2num(cmd.word());
                    tabIntMatcher mtch = new tabIntMatcher();
                    mtch.fromString(cmd.word());
                    rdr.putStrTab(r.ospf4.showNhIncons(i, mtch));
                    return null;
                }
                if (a.equals("lnkinconsistent")) {
                    int i = bits.str2num(cmd.word());
                    tabIntMatcher mtch = new tabIntMatcher();
                    mtch.fromString(cmd.word());
                    rdr.putStrTab(r.ospf4.showMetIncons(i, mtch));
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
                doShowIpXmsdp(tabRouteAttr.routeType.msdp4);
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
                doShowIpXbgp(tabRouteAttr.routeType.bgp4);
                return null;
            }
            if (a.equals("babel")) {
                doShowIpXbabel(tabRouteAttr.routeType.babel4);
                return null;
            }
            if (a.equals("olsr")) {
                doShowIpXolsr(tabRouteAttr.routeType.olsr4);
                return null;
            }
            if (a.equals("rip")) {
                cfgRtr r = cfgAll.rtrFind(tabRouteAttr.routeType.rip4, bits.str2num(cmd.word()), false);
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
            if (a.equals("ecmp")) {
                doShowRouteE(4);
                return null;
            }
            if (a.equals("labels")) {
                doShowRouteL(4);
                return null;
            }
            if (a.equals("distribution")) {
                doShowDistrib(4);
                return null;
            }
            if (a.equals("segrout")) {
                doShowRouteSR(4);
                return null;
            }
            if (a.equals("srindex")) {
                doShowSRindex(4);
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
                if (cmd.size() < 1) {
                    rdr.putStrTab(cfgAll.getShIntTab(5));
                    return null;
                }
                cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
                if (ifc == null) {
                    cmd.error("no such interface");
                    return null;
                }
                doShowIpXifc(ifc.fwdIf6);
                return null;
            }
            if (a.equals("logger")) {
                doShowIpXlogger(tabRouteAttr.routeType.logger6);
                return null;
            }
            if (a.equals("isis")) {
                doShowIpXisis(tabRouteAttr.routeType.isis6);
                return null;
            }
            if (a.equals("pvrp")) {
                doShowIpXpvrp(tabRouteAttr.routeType.pvrp6);
                return null;
            }
            if (a.equals("lsrp")) {
                doShowIpXlsrp(tabRouteAttr.routeType.lsrp6);
                return null;
            }
            if (a.equals("eigrp")) {
                doShowIpXeigrp(tabRouteAttr.routeType.eigrp6);
                return null;
            }
            if (a.equals("ospf")) {
                cfgRtr r = cfgAll.rtrFind(tabRouteAttr.routeType.ospf6, bits.str2num(cmd.word()), false);
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
                if (a.equals("hostnames")) {
                    rdr.putStrTab(r.ospf6.showHostnames(bits.str2num(cmd.word())));
                    return null;
                }
                if (a.equals("tree")) {
                    rdr.putStrArr(r.ospf6.showSpfTree(bits.str2num(cmd.word())));
                    return null;
                }
                if (a.equals("othertree")) {
                    rdr.putStrArr(r.ospf6.showSpfOtherTree(bits.str2num(cmd.word()), cmd));
                    return null;
                }
                if (a.equals("othertopology")) {
                    rdr.putStrTab(r.ospf6.showSpfOtherTopo(bits.str2num(cmd.word()), cmd));
                    return null;
                }
                if (a.equals("graph")) {
                    int i = bits.str2num(cmd.word());
                    boolean nocli = false;
                    boolean nonets = false;
                    boolean noints = false;
                    for (;;) {
                        a = cmd.word();
                        if (a.length() < 1) {
                            break;
                        }
                        if (a.equals("nocli")) {
                            nocli = true;
                            continue;
                        }
                        if (a.equals("nonets")) {
                            nonets = true;
                            continue;
                        }
                        if (a.equals("noints")) {
                            noints = true;
                            continue;
                        }
                    }
                    rdr.putStrArr(r.ospf6.showSpfGraph(i, nocli, nonets, noints));
                    return null;
                }
                if (a.equals("nhinconsistent")) {
                    int i = bits.str2num(cmd.word());
                    tabIntMatcher mtch = new tabIntMatcher();
                    mtch.fromString(cmd.word());
                    rdr.putStrTab(r.ospf6.showNhIncons(i, mtch));
                    return null;
                }
                if (a.equals("lnkinconsistent")) {
                    int i = bits.str2num(cmd.word());
                    tabIntMatcher mtch = new tabIntMatcher();
                    mtch.fromString(cmd.word());
                    rdr.putStrTab(r.ospf6.showMetIncons(i, mtch));
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
                doShowIpXmsdp(tabRouteAttr.routeType.msdp6);
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
                doShowIpXbgp(tabRouteAttr.routeType.bgp6);
                return null;
            }
            if (a.equals("babel")) {
                doShowIpXbabel(tabRouteAttr.routeType.babel6);
                return null;
            }
            if (a.equals("olsr")) {
                doShowIpXolsr(tabRouteAttr.routeType.olsr6);
                return null;
            }
            if (a.equals("rip")) {
                cfgRtr r = cfgAll.rtrFind(tabRouteAttr.routeType.rip6, bits.str2num(cmd.word()), false);
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
            if (a.equals("ecmp")) {
                doShowRouteE(6);
                return null;
            }
            if (a.equals("labels")) {
                doShowRouteL(6);
                return null;
            }
            if (a.equals("distribution")) {
                doShowDistrib(6);
                return null;
            }
            if (a.equals("segrout")) {
                doShowRouteSR(6);
                return null;
            }
            if (a.equals("srindex")) {
                doShowSRindex(6);
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

    private void doShowIpXeigrp(tabRouteAttr.routeType afi) {
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
    }

    private void doShowIpXlogger(tabRouteAttr.routeType afi) {
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
        if (a.equals("interfaces")) {
            rdr.putStrTab(r.logger.outgoingInterfaces());
            return;
        }
    }

    private String getDashRep(String s, List<String> r) {
        for (int i = 0; i < r.size(); i += 2) {
            s = s.replaceAll(r.get(i + 0), r.get(i + 1));
        }
        return uniResLoc.percentEncode(s);
    }

    private List<String> getDashRtr(String u, List<String> r) {
        List<String> res = new ArrayList<String>();
        for (int i = 0; i < cfgAll.routers.size(); i++) {
            cfgRtr ntry = cfgAll.routers.get(i);
            if (ntry == null) {
                continue;
            }
            String a = u.replaceAll("%q%", "?").replaceAll("%s%", " ");
            a = a.replaceAll("%name%", getDashRep(cfgRtr.num2name(ntry.type), r));
            a = a.replaceAll("%id%", getDashRep("" + ntry.number, r));
            a = a.replaceAll("%Name%", cfgRtr.num2name(ntry.type));
            a = a.replaceAll("%Id%", "" + ntry.number);
            res.add(a);
        }
        return res;
    }

    private List<String> getDashVrf(String u, List<String> r) {
        List<String> res = new ArrayList<String>();
        for (int i = 0; i < cfgAll.vrfs.size(); i++) {
            cfgVrf ntry = cfgAll.vrfs.get(i);
            if (ntry == null) {
                continue;
            }
            String a = u.replaceAll("%q%", "?").replaceAll("%s%", " ");
            a = a.replaceAll("%name%", getDashRep(ntry.name, r));
            a = a.replaceAll("%desc%", getDashRep(ntry.description, r));
            a = a.replaceAll("%Name%", ntry.name);
            a = a.replaceAll("%Desc%", ntry.description);
            res.add(a);
        }
        return res;
    }

    private List<String> getDashIfc(String u, List<String> r) {
        List<String> res = new ArrayList<String>();
        for (int i = 0; i < cfgAll.ifaces.size(); i++) {
            cfgIfc ntry = cfgAll.ifaces.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.cloned != null) {
                continue;
            }
            String a = u.replaceAll("%q%", "?").replaceAll("%s%", " ");
            a = a.replaceAll("%name%", getDashRep(ntry.name, r));
            a = a.replaceAll("%desc%", getDashRep(ntry.description, r));
            a = a.replaceAll("%Name%", ntry.name);
            a = a.replaceAll("%Desc%", ntry.description);
            res.add(a);
        }
        return res;
    }

    private List<String> getDashText(String u, List<String> r) {
        List<String> res = new ArrayList<String>();
        String a = u.replaceAll("%q%", "?").replaceAll("%s%", " ");
        a = a.replaceAll("%hostname%", getDashRep(cfgAll.hostName, r));
        a = a.replaceAll("%domain%", getDashRep(cfgAll.domainName, r));
        a = a.replaceAll("%Hostname%", cfgAll.hostName);
        a = a.replaceAll("%Domain%", cfgAll.domainName);
        res.add(a);
        return res;
    }

    private void doShowIpXlsrp(tabRouteAttr.routeType afi) {
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
        if (a.equals("metric")) {
            rdr.putStrTab(r.lsrp.showMetrics());
            return;
        }
        if (a.equals("interface")) {
            rdr.putStrTab(r.lsrp.showIfaces());
            return;
        }
        if (a.equals("uptime")) {
            rdr.putStrTab(r.lsrp.showDatabase(2));
            return;
        }
        if (a.equals("zone-rev")) {
            a = cmd.word();
            String s = cmd.word();
            List<String> rep = new ArrayList<String>();
            for (;;) {
                String b = cmd.word();
                if (b.length() < 1) {
                    break;
                }
                rep.add(b);
            }
            rdr.putStrTab(r.lsrp.showZoneRev(a, s, rep));
            return;
        }
        if (a.equals("zone-fwd")) {
            a = cmd.word();
            String s = cmd.word();
            List<String> rep = new ArrayList<String>();
            for (;;) {
                String b = cmd.word();
                if (b.length() < 1) {
                    break;
                }
                rep.add(b);
            }
            rdr.putStrTab(r.lsrp.showZoneFwd(a, s, rep));
            return;
        }
        if (a.equals("software")) {
            rdr.putStrTab(r.lsrp.showDatabase(3));
            return;
        }
        if (a.equals("middleware")) {
            rdr.putStrTab(r.lsrp.showDatabase(4));
            return;
        }
        if (a.equals("kernel")) {
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
        if (a.equals("hostnames")) {
            rdr.putStrTab(r.lsrp.showHostnames());
            return;
        }
        if (a.equals("tree")) {
            rdr.putStrArr(r.lsrp.showSpfTree());
            return;
        }
        if (a.equals("othertree")) {
            rdr.putStrArr(r.lsrp.showSpfOtherTree(cmd));
            return;
        }
        if (a.equals("othertopology")) {
            rdr.putStrTab(r.lsrp.showSpfOtherTopo(cmd));
            return;
        }
        if (a.equals("graph")) {
            boolean nocli = false;
            boolean nonets = false;
            boolean noints = false;
            for (;;) {
                a = cmd.word();
                if (a.length() < 1) {
                    break;
                }
                if (a.equals("nocli")) {
                    nocli = true;
                    continue;
                }
                if (a.equals("nonets")) {
                    nonets = true;
                    continue;
                }
                if (a.equals("noints")) {
                    noints = true;
                    continue;
                }
            }
            rdr.putStrArr(r.lsrp.showSpfGraph(nocli, nonets, noints));
            return;
        }
        if (a.equals("nhinconsistent")) {
            tabIntMatcher mtch = new tabIntMatcher();
            mtch.fromString(cmd.word());
            rdr.putStrTab(r.lsrp.showNhIncons(mtch));
            return;
        }
        if (a.equals("lnkinconsistent")) {
            tabIntMatcher mtch = new tabIntMatcher();
            mtch.fromString(cmd.word());
            rdr.putStrTab(r.lsrp.showMetIncons(mtch));
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
    }

    private void doShowIpXpvrp(tabRouteAttr.routeType afi) {
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
        if (a.equals("metric")) {
            rdr.putStrTab(r.pvrp.showMetrics());
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
    }

    private void doShowIpXisis(tabRouteAttr.routeType afi) {
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
        if (a.equals("hostnames")) {
            rdr.putStrTab(r.isis.showHostnames(bits.str2num(cmd.word())));
            return;
        }
        if (a.equals("tree")) {
            rdr.putStrArr(r.isis.showSpfTree(bits.str2num(cmd.word())));
            return;
        }
        if (a.equals("othertree")) {
            rdr.putStrArr(r.isis.showSpfOtherTree(bits.str2num(cmd.word()), cmd));
            return;
        }
        if (a.equals("othertopology")) {
            rdr.putStrTab(r.isis.showSpfOtherTopo(bits.str2num(cmd.word()), cmd));
            return;
        }
        if (a.equals("graph")) {
            int i = bits.str2num(cmd.word());
            boolean nocli = false;
            boolean nonets = false;
            boolean noints = false;
            for (;;) {
                a = cmd.word();
                if (a.length() < 1) {
                    break;
                }
                if (a.equals("nocli")) {
                    nocli = true;
                    continue;
                }
                if (a.equals("nonets")) {
                    nonets = true;
                    continue;
                }
                if (a.equals("noints")) {
                    noints = true;
                    continue;
                }
            }
            rdr.putStrArr(r.isis.showSpfGraph(i, nocli, nonets, noints));
            return;
        }
        if (a.equals("nhinconsistent")) {
            int i = bits.str2num(cmd.word());
            tabIntMatcher mtch = new tabIntMatcher();
            mtch.fromString(cmd.word());
            rdr.putStrTab(r.isis.showNhIncons(i, mtch));
            return;
        }
        if (a.equals("lnkinconsistent")) {
            int i = bits.str2num(cmd.word());
            tabIntMatcher mtch = new tabIntMatcher();
            mtch.fromString(cmd.word());
            rdr.putStrTab(r.isis.showMetIncons(i, mtch));
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
        if (a.equals("other-route")) {
            doShowRoutes(r.isis.other.fwd, r.isis.showOroute(bits.str2num(cmd.word())), 1);
            return;
        }
        if (a.equals("other-originate")) {
            doShowRoutes(r.isis.other.fwd, r.isis.other.routerRedistedU, 1);
            return;
        }
        cmd.badCmd();
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
        if (vrf == null) {
            cmd.error("no such vrf");
            return;
        }
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
        doShowSession(flw.session);
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

    private void doShowIpXmsdp(tabRouteAttr.routeType afi) {
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
            userFormat res = new userFormat("|", "category|value");
            ntry.getDump(res);
            rdr.putStrTab(res);
            return;
        }
        cmd.badCmd();
    }

    private void doShowIpXldp(int ver) {
        ipFwd fwd = findVrf(ver);
        if (fwd == null) {
            return;
        }
        String a = cmd.word();
        if (a.equals("nulled-summary")) {
            rdr.putStrTab(ipFwdTab.ldpNulledShow(fwd));
            return;
        }
        if (a.equals("summary")) {
            rdr.putStrTab(ipFwdTab.ldpNeighShow(fwd));
            return;
        }
        if (a.equals("database")) {
            doShowRoutes(fwd, fwd.labeldR, 3);
            return;
        }
        if (a.equals("mpdatabase")) {
            doShowMptab(fwd.mp2mpLsp, null);
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
            doShowRoutes(fwd, tabRoute.nullLabeled(nei.prefLearn), 3);
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
            doShowMptab(nei.pmpLearn, adr);
            return;
        }
        if (a.equals("mpadvertised")) {
            doShowPmpList(nei.pmpAdvert);
            return;
        }
        if (a.equals("status")) {
            userFormat res = new userFormat("|", "category|value");
            nei.getStatus(res);
            rdr.putStrTab(res);
            return;
        }
        cmd.badCmd();
    }

    private void doShowIpXbabel(tabRouteAttr.routeType afi) {
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

    private void doShowIpXolsr(tabRouteAttr.routeType afi) {
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

    private void compareDiffs(tabRoute<addrIP> equ, tabRoute<addrIP> dif1, tabRoute<addrIP> dif2) {
        for (int i = 0; i < dif1.size(); i++) {
            tabRouteEntry<addrIP> prf1 = dif1.get(i);
            tabRouteEntry<addrIP> prf2 = dif2.find(prf1);
            if (prf2 == null) {
                continue;
            }
            equ.add(tabRoute.addType.always, prf1, false, false);
        }
    }

    private void ignoreAttribs(tabRouteAttr<addrIP> ntry, int ign) {
        ntry.srcRtr = null;
        ntry.rouSrc = 0;
        ntry.rouTyp = tabRouteAttr.routeType.bgp4;
        ntry.protoNum = 0;
        ntry.iface = null;
        ntry.ident = 0;
        if ((ign & 0x1) != 0) {
            ntry.clustList = null;
        }
        if ((ign & 0x2) != 0) {
            ntry.nextHop = null;
            ntry.oldHop = null;
        }
        if ((ign & 0x4) != 0) {
            ntry.origin = 0;
        }
        if ((ign & 0x8) != 0) {
            ntry.metric = 0;
        }
        if ((ign & 0x10) != 0) {
            ntry.locPref = 0;
        }
        if ((ign & 0x20) != 0) {
            ntry.distance = 0;
        }
        if ((ign & 0x40) != 0) {
            ntry.tag = 0;
        }
        if ((ign & 0x80) != 0) {
            ntry.validity = 0;
        }
        if ((ign & 0x100) != 0) {
            ntry.pathSeq = null;
            ntry.pathSet = null;
        }
        if ((ign & 0x200) != 0) {
            ntry.confSeq = null;
            ntry.confSet = null;
        }
        if ((ign & 0x400) != 0) {
            ntry.stdComm = null;
        }
        if ((ign & 0x800) != 0) {
            ntry.extComm = null;
        }
        if ((ign & 0x1000) != 0) {
            ntry.accIgp = 0;
        }
        if ((ign & 0x2000) != 0) {
            ntry.bandwidth = 0;
        }
        if ((ign & 0x4000) != 0) {
            ntry.labelLoc = null;
            ntry.labelRem = null;
        }
        if ((ign & 0x8000) != 0) {
            ntry.atomicAggr = false;
            ntry.aggrAs = 0;
            ntry.aggrRtr = null;
        }
        if ((ign & 0x10000) != 0) {
            ntry.originator = null;
        }
        if ((ign & 0x20000) != 0) {
            ntry.pmsiLab = 0;
            ntry.pmsiTyp = 0;
            ntry.pmsiTun = null;
        }
        if ((ign & 0x40000) != 0) {
            ntry.segrouIdx = 0;
            ntry.segrouBeg = 0;
            ntry.segrouOld = 0;
            ntry.segrouSiz = 0;
        }
        if ((ign & 0x80000) != 0) {
            ntry.lrgComm = null;
        }
        if ((ign & 0x100000) != 0) {
            ntry.tunelTyp = 0;
            ntry.tunelVal = null;
        }
        if ((ign & 0x200000) != 0) {
            ntry.attribAs = 0;
            ntry.attribVal = null;
        }
        if ((ign & 0x400000) != 0) {
            ntry.bierIdx = 0;
            ntry.bierBeg = 0;
            ntry.bierOld = 0;
            ntry.bierSiz = 0;
            ntry.bierHdr = 0;
        }
        if ((ign & 0x800000) != 0) {
            if (ntry.stdComm != null) {
                Collections.sort(ntry.stdComm);
            }
            if (ntry.extComm != null) {
                Collections.sort(ntry.extComm);
            }
        }
        if ((ign & 0x1000000) != 0) {
            ntry.linkStat = null;
        }
    }

    private void compareTables(tabRoute<addrIP> uniq, tabRoute<addrIP> diff, tabRoute<addrIP> nei1, tabRoute<addrIP> nei2, int ign, tabListing<tabRtrmapN, addrIP> flt, int safi, int asn1, int asn2, tabListing<tabRtrmapN, addrIP> upd) {
        for (int o = 0; o < nei1.size(); o++) {
            tabRouteEntry<addrIP> prf1 = nei1.get(o);
            if (prf1 == null) {
                continue;
            }
            if (flt != null) {
                if (flt.matches(safi, 0, prf1)) {
                    continue;
                }
            }
            prf1 = prf1.copyBytes(tabRoute.addType.alters);
            tabRouteEntry<addrIP> prf2 = nei2.find(prf1);
            if (prf2 == null) {
                uniq.add(tabRoute.addType.always, prf1, false, false);
                continue;
            }
            if (flt != null) {
                if (flt.matches(safi, 0, prf2)) {
                    continue;
                }
            }
            prf2 = prf2.copyBytes(tabRoute.addType.alters);
            for (int i = 0; i < prf1.alts.size(); i++) {
                ignoreAttribs(prf1.alts.get(i), ign);
            }
            for (int i = 0; i < prf2.alts.size(); i++) {
                ignoreAttribs(prf2.alts.get(i), ign);
            }
            if (upd != null) {
                upd.update(safi, asn1, prf1, false);
                upd.update(safi, asn2, prf2, false);
            }
            if (!prf1.differs(tabRoute.addType.alters, prf2)) {
                continue;
            }
            diff.add(tabRoute.addType.alters, prf1, false, false);
            diff.add(tabRoute.addType.alters, prf2, false, false);
        }
    }

    private int str2ignore(String a) {
        if (a.equals("cluster")) {
            return 0x1;
        }
        if (a.equals("nexthop")) {
            return 0x2;
        }
        if (a.equals("origin")) {
            return 0x4;
        }
        if (a.equals("metric")) {
            return 0x8;
        }
        if (a.equals("locpref")) {
            return 0x10;
        }
        if (a.equals("distance")) {
            return 0x20;
        }
        if (a.equals("tag")) {
            return 0x40;
        }
        if (a.equals("validity")) {
            return 0x80;
        }
        if (a.equals("aspath")) {
            return 0x100;
        }
        if (a.equals("asconf")) {
            return 0x200;
        }
        if (a.equals("stdcomm")) {
            return 0x400;
        }
        if (a.equals("extcomm")) {
            return 0x800;
        }
        if (a.equals("aigp")) {
            return 0x1000;
        }
        if (a.equals("bandwidth")) {
            return 0x2000;
        }
        if (a.equals("label")) {
            return 0x4000;
        }
        if (a.equals("aggregate")) {
            return 0x8000;
        }
        if (a.equals("orignted")) {
            return 0x10000;
        }
        if (a.equals("pmsi")) {
            return 0x20000;
        }
        if (a.equals("segrout")) {
            return 0x40000;
        }
        if (a.equals("lrgcomm")) {
            return 0x80000;
        }
        if (a.equals("tunnel")) {
            return 0x100000;
        }
        if (a.equals("attrset")) {
            return 0x200000;
        }
        if (a.equals("bier")) {
            return 0x400000;
        }
        if (a.equals("sortcomm")) {
            return 0x800000;
        }
        if (a.equals("lnksta")) {
            return 0x1000000;
        }
        return 0;
    }

    private void doShowIpXbgp(tabRouteAttr.routeType afi) {
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
        if (a.equals("template")) {
            rtrBgpTemp tmp = r.bgp.findTemp(cmd.word());
            if (tmp == null) {
                cmd.error("no such template");
                return;
            }
            a = cmd.word();
            if (a.equals("config")) {
                List<String> l = new ArrayList<String>();
                tmp.getConfig(l, "", 0);
                rdr.putStrArr(l);
                return;
            }
            return;
        }
        if (a.equals("group")) {
            a = cmd.word();
            if (a.length() < 1) {
                rdr.putStrTab(r.bgp.showSummary(2));
                return;
            }
            rtrBgpGroup grp = r.bgp.findGroup(bits.str2num(a));
            if (grp == null) {
                cmd.error("no such group");
                return;
            }
            a = cmd.word();
            if (a.equals("config")) {
                List<String> l = new ArrayList<String>();
                grp.getConfig(l, "", 0);
                rdr.putStrArr(l);
                return;
            }
            if (a.equals("status")) {
                rdr.putStrTab(grp.getStatus());
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
        if (a.equals("multiple-labels")) {
            rdr.putStrTab(r.bgp.showSummary(14));
            return;
        }
        if (a.equals("resolve")) {
            rdr.putStrTab(r.bgp.showSummary(12));
            return;
        }
        if (a.equals("afi")) {
            rdr.putStrTab(r.bgp.showSummary(1));
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
            rdr.putStrTab(r.bgp.showSummary(13));
            return;
        }
        if (a.equals("neighbor")) {
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
                nei.getConfig(l, "", 0);
                rdr.putStrArr(l);
                return;
            }
            if (a.equals("dampening")) {
                tabIntMatcher mtch = new tabIntMatcher();
                mtch.fromString(cmd.word());
                rdr.putStrTab(nei.getDampening(mtch));
                return;
            }
            if (a.equals("status")) {
                rdr.putStrTab(nei.getStatus());
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
        int sfi = rtrBgpParam.string2mask(a);
        if (sfi < 1) {
            cmd.badCmd();
            return;
        }
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
        if (a.equals("asorigin")) {
            rdr.putStrTab(r.bgp.getAsOrigin(sfi));
            return;
        }
        if (a.equals("astransit")) {
            rdr.putStrTab(r.bgp.getAsTransit(sfi));
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
            a = cmd.word();
            if (a.length() < 1) {
                a = "2-" + Integer.MAX_VALUE;
            }
            tabIntMatcher m = new tabIntMatcher();
            m.fromString(a);
            rdr.putStrTab(r.bgp.getAsIncons(sfi, m));
            return;
        }
        if (a.equals("nhinconsistent")) {
            a = cmd.word();
            if (a.length() < 1) {
                a = "2-" + Integer.MAX_VALUE;
            }
            tabIntMatcher m = new tabIntMatcher();
            m.fromString(a);
            rdr.putStrTab(r.bgp.getNhIncons(sfi, m));
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
            rdr.putStrTab(r.bgp.getFlappath(sfi, tabRtrmapN.string2rd(cmd.word()), ntry, false));
            return;
        }
        if (a.equals("flaprevpath")) {
            addrPrefix<addrIP> ntry = addrPrefix.str2ip(cmd.word());
            if (ntry == null) {
                cmd.error("bad prefix");
                return;
            }
            rdr.putStrTab(r.bgp.getFlappath(sfi, tabRtrmapN.string2rd(cmd.word()), ntry, true));
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
            rdr.putStrTab(r.bgp.getAllRoutes(sfi, ntry));
            return;
        }
        if (a.equals("differ")) {
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
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = addrPrefix.str2ip(cmd.word());
            if (ntry.prefix == null) {
                cmd.error("bad prefix");
                return;
            }
            ntry.rouDst = tabRtrmapN.string2rd(cmd.word());
            tabRoute<addrIP> acc1 = nei1.getAccepted(sfi);
            tabRoute<addrIP> acc2 = nei2.getAccepted(sfi);
            if ((acc1 == null) || (acc2 == null)) {
                return;
            }
            tabRouteEntry<addrIP> ntry1 = acc1.find(ntry);
            tabRouteEntry<addrIP> ntry2 = acc2.find(ntry);
            if (ntry1 == null) {
                cmd.error("not from neighbor 1");
                return;
            }
            if (ntry2 == null) {
                cmd.error("not from neighbor 2");
                return;
            }
            List<String> dump1 = ntry1.fullDump(r.bgp.fwdCore).formatAll(userFormat.tableMode.normal);
            List<String> dump2 = ntry2.fullDump(r.bgp.fwdCore).formatAll(userFormat.tableMode.normal);
            differ df = new differ();
            df.calc(dump1, dump2);
            rdr.putStrArr(df.getText(cmd.pipe.settingsGet(pipeSetting.width, 80), 0));
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
            tabListing<tabRtrmapN, addrIP> flt = null;
            tabListing<tabRtrmapN, addrIP> upd = null;
            for (;;) {
                a = cmd.word();
                if (a.length() < 1) {
                    break;
                }
                if (a.equals("exclude")) {
                    cfgRoump res = cfgAll.rtmpFind(cmd.word(), false);
                    if (res == null) {
                        continue;
                    }
                    flt = res.roumap;
                    continue;
                }
                if (a.equals("update")) {
                    cfgRoump res = cfgAll.rtmpFind(cmd.word(), false);
                    if (res == null) {
                        continue;
                    }
                    upd = res.roumap;
                    continue;
                }
                ign |= str2ignore(a);
            }
            tabRoute<addrIP> acc1 = nei1.getAccepted(sfi);
            tabRoute<addrIP> acc2 = nei2.getAccepted(sfi);
            if ((acc1 == null) || (acc2 == null)) {
                return;
            }
            tabRoute<addrIP> uni1 = new tabRoute<addrIP>("tab");
            tabRoute<addrIP> uni2 = new tabRoute<addrIP>("tab");
            tabRoute<addrIP> diff = new tabRoute<addrIP>("tab");
            compareTables(uni1, diff, acc1, acc2, ign, flt, sfi, nei1.remoteAs, nei2.remoteAs, upd);
            compareTables(uni2, diff, acc2, acc1, ign, flt, sfi, nei2.remoteAs, nei1.remoteAs, upd);
            cmd.error("unique to " + nei1);
            doShowRoutes(r.bgp.fwdCore, uni1, dsp);
            cmd.error("unique to " + nei2);
            doShowRoutes(r.bgp.fwdCore, uni2, dsp);
            cmd.error("attribute differs");
            doShowRoutes(r.bgp.fwdCore, diff, dsp);
            return;
        }
        if (a.equals("dcompare")) {
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
            int tim = 5000;
            tabListing<tabRtrmapN, addrIP> flt = null;
            tabListing<tabRtrmapN, addrIP> upd = null;
            for (;;) {
                a = cmd.word();
                if (a.length() < 1) {
                    break;
                }
                if (a.equals("time")) {
                    tim = bits.str2num(cmd.word());
                    continue;
                }
                if (a.equals("exclude")) {
                    cfgRoump res = cfgAll.rtmpFind(cmd.word(), false);
                    if (res == null) {
                        continue;
                    }
                    flt = res.roumap;
                    continue;
                }
                if (a.equals("update")) {
                    cfgRoump res = cfgAll.rtmpFind(cmd.word(), false);
                    if (res == null) {
                        continue;
                    }
                    upd = res.roumap;
                    continue;
                }
                ign |= str2ignore(a);
            }
            tabRoute<addrIP> acc1 = nei1.getAccepted(sfi);
            tabRoute<addrIP> acc2 = nei2.getAccepted(sfi);
            if ((acc1 == null) || (acc2 == null)) {
                return;
            }
            tabRoute<addrIP> dif1 = new tabRoute<addrIP>("tab");
            compareTables(dif1, dif1, acc1, acc2, ign, flt, sfi, nei1.remoteAs, nei2.remoteAs, upd);
            compareTables(dif1, dif1, acc2, acc1, ign, flt, sfi, nei2.remoteAs, nei1.remoteAs, upd);
            bits.sleep(tim);
            tabRoute<addrIP> dif2 = new tabRoute<addrIP>("tab");
            compareTables(dif2, dif2, acc1, acc2, ign, flt, sfi, nei1.remoteAs, nei2.remoteAs, upd);
            compareTables(dif2, dif2, acc2, acc1, ign, flt, sfi, nei2.remoteAs, nei1.remoteAs, upd);
            tabRoute<addrIP> dif3 = new tabRoute<addrIP>("tab");
            cmd.error("constant differences");
            compareDiffs(dif3, dif1, dif2);
            doShowRoutes(r.bgp.fwdCore, dif3, dsp);
            return;
        }
        tabRoute<addrIP> tab = r.bgp.getDatabase(sfi);
        if (tab == null) {
            return;
        }
        if (a.equals("wireformat")) {
            a = cmd.word();
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = addrPrefix.str2ip(a);
            if (ntry.prefix == null) {
                addrIP adr = new addrIP();
                if (adr.fromString(a)) {
                    cmd.error("bad prefix");
                    return;
                }
                ntry = tab.route(adr);
                if (ntry == null) {
                    cmd.error("no such route");
                    return;
                }
            }
            ntry.rouDst = tabRtrmapN.string2rd(cmd.word());
            ntry = tab.find(ntry);
            if (ntry == null) {
                cmd.error("no such prefix");
                return;
            }
            ntry = ntry.copyBytes(tabRoute.addType.better);
            if (ntry.best.nextHop == null) {
                ntry.best.nextHop = new addrIP();
                if (r.bgp.fwdCore.ipVersion == 4) {
                    ntry.best.nextHop.fromIPv4addr(new addrIPv4());
                } else {
                    ntry.best.nextHop.fromIPv6addr(new addrIPv6());
                }
            }
            packHolder pck = new packHolder(true, true);
            List<tabRouteEntry<addrIP>> lst = new ArrayList<tabRouteEntry<addrIP>>();
            lst.add(ntry);
            rtrBgpUtil.createReachable(pck, new packHolder(true, true), sfi, false, true, lst, null);
            rtrBgpUtil.createHeader(pck, rtrBgpUtil.msgUpdate);
            List<String> l = new ArrayList<String>();
            userFlash.buf2hex(l, pck.getCopy(), 0);
            rdr.putStrArr(l);
            return;
        }
        if (a.equals("wireunformat")) {
            a = cmd.word();
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = addrPrefix.str2ip(a);
            if (ntry.prefix == null) {
                addrIP adr = new addrIP();
                if (adr.fromString(a)) {
                    cmd.error("bad prefix");
                    return;
                }
                ntry = tab.route(adr);
                if (ntry == null) {
                    cmd.error("no such route");
                    return;
                }
            }
            ntry.rouDst = tabRtrmapN.string2rd(cmd.word());
            ntry = tab.find(ntry);
            if (ntry == null) {
                cmd.error("no such prefix");
                return;
            }
            ntry = ntry.copyBytes(tabRoute.addType.better);
            if (ntry.best.nextHop == null) {
                ntry.best.nextHop = new addrIP();
                if (r.bgp.fwdCore.ipVersion == 4) {
                    ntry.best.nextHop.fromIPv4addr(new addrIPv4());
                } else {
                    ntry.best.nextHop.fromIPv6addr(new addrIPv6());
                }
            }
            packHolder pck = new packHolder(true, true);
            List<tabRouteEntry<addrIP>> lst = new ArrayList<tabRouteEntry<addrIP>>();
            lst.add(ntry);
            rtrBgpUtil.createWithdraw(pck, new packHolder(true, true), sfi, false, lst);
            rtrBgpUtil.createHeader(pck, rtrBgpUtil.msgUpdate);
            List<String> l = new ArrayList<String>();
            userFlash.buf2hex(l, pck.getCopy(), 0);
            rdr.putStrArr(l);
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
        if (a.equals("ecmp")) {
            doShowRoutes(r.bgp.fwdCore, tab, dsp + 2000);
            return;
        }
        if (a.equals("nostdcomm")) {
            a = cmd.getRemaining();
            cmd = new cmds("", "");
            tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
            tabRtrmapN ntry = new tabRtrmapN();
            ntry.action = tabListingEntry.actionType.actPermit;
            ntry.noStdComm = true;
            roumap.add(ntry);
            tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
            tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, 0, res, tab, false, roumap, null, null);
            doShowRoutes(r.bgp.fwdCore, res, dsp);
            return;
        }
        if (a.equals("noextcomm")) {
            a = cmd.getRemaining();
            cmd = new cmds("", "");
            tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
            tabRtrmapN ntry = new tabRtrmapN();
            ntry.action = tabListingEntry.actionType.actPermit;
            ntry.noExtComm = true;
            roumap.add(ntry);
            tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
            tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, 0, res, tab, false, roumap, null, null);
            doShowRoutes(r.bgp.fwdCore, res, dsp);
            return;
        }
        if (a.equals("nolrgcomm")) {
            a = cmd.getRemaining();
            cmd = new cmds("", "");
            tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
            tabRtrmapN ntry = new tabRtrmapN();
            ntry.action = tabListingEntry.actionType.actPermit;
            ntry.noLrgComm = true;
            roumap.add(ntry);
            tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
            tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, 0, res, tab, false, roumap, null, null);
            doShowRoutes(r.bgp.fwdCore, res, dsp);
            return;
        }
        if (a.equals("privateas")) {
            a = cmd.getRemaining();
            cmd = new cmds("", "");
            tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
            tabRtrmapN ntry = new tabRtrmapN();
            ntry.action = tabListingEntry.actionType.actPermit;
            ntry.privasMatch = true;
            roumap.add(ntry);
            tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
            tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, 0, res, tab, false, roumap, null, null);
            doShowRoutes(r.bgp.fwdCore, res, dsp);
            return;
        }
        if (a.equals("stdcomm")) {
            a = cmd.getRemaining();
            cmd = new cmds("", "");
            tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
            tabRtrmapN ntry = new tabRtrmapN();
            ntry.action = tabListingEntry.actionType.actPermit;
            ntry.stdCommMatch = tabRtrmapN.string2stdComms(a);
            roumap.add(ntry);
            tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
            tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, 0, res, tab, false, roumap, null, null);
            doShowRoutes(r.bgp.fwdCore, res, dsp);
            return;
        }
        if (a.equals("extcomm")) {
            a = cmd.getRemaining();
            cmd = new cmds("", "");
            tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
            tabRtrmapN ntry = new tabRtrmapN();
            ntry.action = tabListingEntry.actionType.actPermit;
            ntry.extCommMatch = tabRtrmapN.string2extComms(a);
            roumap.add(ntry);
            tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
            tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, 0, res, tab, false, roumap, null, null);
            doShowRoutes(r.bgp.fwdCore, res, dsp);
            return;
        }
        if (a.equals("lrgcomm")) {
            a = cmd.getRemaining();
            cmd = new cmds("", "");
            tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
            tabRtrmapN ntry = new tabRtrmapN();
            ntry.action = tabListingEntry.actionType.actPermit;
            ntry.lrgCommMatch = tabRtrmapN.string2lrgComms(a);
            roumap.add(ntry);
            tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
            tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, 0, res, tab, false, roumap, null, null);
            doShowRoutes(r.bgp.fwdCore, res, dsp);
            return;
        }
        if (a.equals("rd")) {
            a = cmd.getRemaining();
            cmd = new cmds("", "");
            tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
            tabRtrmapN ntry = new tabRtrmapN();
            ntry.action = tabListingEntry.actionType.actPermit;
            ntry.rouDstMatch = tabRtrmapN.string2rd(a);
            roumap.add(ntry);
            tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
            tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, 0, res, tab, false, roumap, null, null);
            doShowRoutes(r.bgp.fwdCore, res, dsp);
            return;
        }
        if (a.equals("regexp")) {
            a = cmd.getRemaining();
            cmd = new cmds("", "");
            tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
            tabRtrmapN ntry = new tabRtrmapN();
            ntry.action = tabListingEntry.actionType.actPermit;
            ntry.aspathMatch = a;
            roumap.add(ntry);
            tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
            tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, 0, res, tab, false, roumap, null, null);
            doShowRoutes(r.bgp.fwdCore, res, dsp);
            return;
        }
        if (a.equals("pathlen")) {
            a = cmd.getRemaining();
            cmd = new cmds("", "");
            tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
            tabRtrmapN ntry = new tabRtrmapN();
            ntry.action = tabListingEntry.actionType.actPermit;
            ntry.pathlenMatch = new tabIntMatcher();
            ntry.pathlenMatch.fromString(a);
            roumap.add(ntry);
            tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
            tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, 0, res, tab, false, roumap, null, null);
            doShowRoutes(r.bgp.fwdCore, res, dsp);
            return;
        }
        if (a.equals("distance")) {
            a = cmd.getRemaining();
            cmd = new cmds("", "");
            tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
            tabRtrmapN ntry = new tabRtrmapN();
            ntry.action = tabListingEntry.actionType.actPermit;
            ntry.distanceMatch = new tabIntMatcher();
            ntry.distanceMatch.fromString(a);
            roumap.add(ntry);
            tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
            tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, 0, res, tab, false, roumap, null, null);
            doShowRoutes(r.bgp.fwdCore, res, dsp);
            return;
        }
        if (a.equals("locpref")) {
            a = cmd.getRemaining();
            cmd = new cmds("", "");
            tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
            tabRtrmapN ntry = new tabRtrmapN();
            ntry.action = tabListingEntry.actionType.actPermit;
            ntry.locPrefMatch = new tabIntMatcher();
            ntry.locPrefMatch.fromString(a);
            roumap.add(ntry);
            tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
            tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, 0, res, tab, false, roumap, null, null);
            doShowRoutes(r.bgp.fwdCore, res, dsp);
            return;
        }
        if (a.equals("validity")) {
            a = cmd.getRemaining();
            cmd = new cmds("", "");
            tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
            tabRtrmapN ntry = new tabRtrmapN();
            ntry.action = tabListingEntry.actionType.actPermit;
            ntry.validityMatch = new tabIntMatcher();
            ntry.validityMatch.fromString(a);
            roumap.add(ntry);
            tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
            tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, 0, res, tab, false, roumap, null, null);
            doShowRoutes(r.bgp.fwdCore, res, dsp);
            return;
        }
        if (a.equals("aigp")) {
            a = cmd.getRemaining();
            cmd = new cmds("", "");
            tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
            tabRtrmapN ntry = new tabRtrmapN();
            ntry.action = tabListingEntry.actionType.actPermit;
            ntry.accIgpMatch = new tabIntMatcher();
            ntry.accIgpMatch.fromString(a);
            roumap.add(ntry);
            tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
            tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, 0, res, tab, false, roumap, null, null);
            doShowRoutes(r.bgp.fwdCore, res, dsp);
            return;
        }
        if (a.equals("bandwidth")) {
            a = cmd.getRemaining();
            cmd = new cmds("", "");
            tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
            tabRtrmapN ntry = new tabRtrmapN();
            ntry.action = tabListingEntry.actionType.actPermit;
            ntry.bandwidthMatch = new tabIntMatcher();
            ntry.bandwidthMatch.fromString(a);
            roumap.add(ntry);
            tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
            tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, 0, res, tab, false, roumap, null, null);
            doShowRoutes(r.bgp.fwdCore, res, dsp);
            return;
        }
        if (a.equals("origin")) {
            a = cmd.getRemaining();
            cmd = new cmds("", "");
            tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
            tabRtrmapN ntry = new tabRtrmapN();
            ntry.action = tabListingEntry.actionType.actPermit;
            ntry.originMatch = new tabIntMatcher();
            ntry.originMatch.fromString(a);
            roumap.add(ntry);
            tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
            tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, 0, res, tab, false, roumap, null, null);
            doShowRoutes(r.bgp.fwdCore, res, dsp);
            return;
        }
        if (a.equals("metric")) {
            a = cmd.getRemaining();
            cmd = new cmds("", "");
            tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
            tabRtrmapN ntry = new tabRtrmapN();
            ntry.action = tabListingEntry.actionType.actPermit;
            ntry.metricMatch = new tabIntMatcher();
            ntry.metricMatch.fromString(a);
            roumap.add(ntry);
            tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
            tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, 0, res, tab, false, roumap, null, null);
            doShowRoutes(r.bgp.fwdCore, res, dsp);
            return;
        }
        if (a.equals("tag")) {
            a = cmd.getRemaining();
            cmd = new cmds("", "");
            tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
            tabRtrmapN ntry = new tabRtrmapN();
            ntry.action = tabListingEntry.actionType.actPermit;
            ntry.tagMatch = new tabIntMatcher();
            ntry.tagMatch.fromString(a);
            roumap.add(ntry);
            tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
            tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, 0, res, tab, false, roumap, null, null);
            doShowRoutes(r.bgp.fwdCore, res, dsp);
            return;
        }
        if (a.equals("network")) {
            a = cmd.getRemaining();
            cmd = new cmds("", "");
            tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
            tabRtrmapN ntry = new tabRtrmapN();
            ntry.action = tabListingEntry.actionType.actPermit;
            ntry.networkMatch = new tabPrfxlstN();
            ntry.networkMatch.fromString(a);
            roumap.add(ntry);
            tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
            tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, 0, res, tab, false, roumap, null, null);
            doShowRoutes(r.bgp.fwdCore, res, dsp);
            return;
        }
        if (a.equals("nexthop")) {
            a = cmd.getRemaining();
            cmd = new cmds("", "");
            tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
            tabRtrmapN ntry = new tabRtrmapN();
            ntry.action = tabListingEntry.actionType.actPermit;
            ntry.nexthopMatch = new addrIP();
            ntry.nexthopMatch.fromString(a);
            roumap.add(ntry);
            tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
            tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, 0, res, tab, false, roumap, null, null);
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
            tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, 0, res, tab, false, null, null, fnd.prflst);
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
            tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, 0, res, tab, false, fnd.roumap, null, null);
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
            tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, 0, res, tab, false, null, fnd.rouplc, null);
            doShowRoutes(r.bgp.fwdCore, res, dsp);
            return;
        }
        if (a.equals("measure-list")) {
            a = cmd.word();
            cfgPrfxlst fnd = cfgAll.prfxFind(a, false);
            if (fnd == null) {
                cmd.error("no such prefix list");
                return;
            }
            tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
            long tim = bits.getTime();
            tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, 0, res, tab, false, null, null, fnd.prflst);
            cmd.error("took " + (bits.getTime() - tim) + " ms, permitted " + res.size() + " of " + tab.size());
            return;
        }
        if (a.equals("measure-map")) {
            a = cmd.word();
            cfgRoump fnd = cfgAll.rtmpFind(a, false);
            if (fnd == null) {
                cmd.error("no such route map");
                return;
            }
            tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
            long tim = bits.getTime();
            tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, 0, res, tab, false, fnd.roumap, null, null);
            cmd.error("took " + (bits.getTime() - tim) + " ms, permitted " + res.size() + " of " + tab.size());
            return;
        }
        if (a.equals("measure-policy")) {
            a = cmd.word();
            cfgRouplc fnd = cfgAll.rtplFind(a, false);
            if (fnd == null) {
                cmd.error("no such route policy");
                return;
            }
            tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
            long tim = bits.getTime();
            tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, 0, res, tab, false, null, fnd.rouplc, null);
            cmd.error("took " + (bits.getTime() - tim) + " ms, permitted " + res.size() + " of " + tab.size());
            return;
        }
        cmd.badCmd();
    }

    private int bgpMask2filter(int mask) {
        switch (mask) {
            case rtrBgpParam.mskEvpn:
            case rtrBgpParam.mskMspw:
            case rtrBgpParam.mskMdt:
            case rtrBgpParam.mskMvpn:
            case rtrBgpParam.mskMvpo:
            case rtrBgpParam.mskFlw:
            case rtrBgpParam.mskOtrF:
            case rtrBgpParam.mskVpnF:
            case rtrBgpParam.mskVpoF:
            case rtrBgpParam.mskNsh:
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
            userFormat l = new userFormat("|", "proto|source|target|source|target|age|last|timeout|pack|byte", "1|2original|2translated|5");
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
    }

    private void doShowIpXvrf(int ver) {
        if (cmd.size() > 0) {
            ipFwd fwd = findVrf(ver);
            if (fwd == null) {
                return;
            }
            String a = cmd.word();
            if (!a.equals("interface")) {
                doShowHistory(a, fwd.hstryT);
                return;
            }
            userFormat l = new userFormat("|", "interface");
            for (int i = 0; i < cfgAll.ifaces.size(); i++) {
                cfgIfc ntry = cfgAll.ifaces.get(i);
                if (ntry == null) {
                    continue;
                }
                if (ntry.vrfFor == null) {
                    continue;
                }
                if ((ntry.vrfFor.fwd4 != fwd) && (ntry.vrfFor.fwd6 != fwd)) {
                    continue;
                }
                l.add(ntry.name);
            }
            rdr.putStrTab(l);
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

    private void doShowIpXifc(ipFwdIface ifc) {
        if (ifc == null) {
            cmd.error("protocol not enabled");
            return;
        }
        rdr.putStrArr(ifc.getShow());
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
        String a = cmd.word();
        if (a.length() > 0) {
            addrIP src = new addrIP();
            src.fromString(a);
            addrIP grp = new addrIP();
            grp.fromString(cmd.word());
            ipFwdMcast mr = fwd.groups.find(new ipFwdMcast(grp, src));
            if (mr == null) {
                cmd.error("no such group");
                return;
            }
            userFormat res = new userFormat("|", "category|value");
            mr.getDump(res);
            rdr.putStrTab(res);
            return;
        }
        userFormat l = new userFormat("|", "source|group|interface|upstream|targets|bytes");
        for (int o = 0; o < fwd.groups.size(); o++) {
            ipFwdMcast mr = fwd.groups.get(o);
            if (mr == null) {
                continue;
            }
            l.add(mr.getShow());
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

    private void doShowRouteL(int ver) {
        ipFwd fwd = findVrf(ver);
        if (fwd == null) {
            return;
        }
        doShowRoutes(fwd, fwd.actualU, 3);
    }

    private void doShowRouteE(int ver) {
        ipFwd fwd = findVrf(ver);
        if (fwd == null) {
            return;
        }
        doShowRoutes(fwd, fwd.actualU, 9);
    }

    private void doShowDistrib(int ver) {
        ipFwd fwd = findVrf(ver);
        if (fwd == null) {
            return;
        }
        rdr.putStrTab(rtrLogger.outgointInterfaces(fwd.actualU));
        rdr.putStrTab(rtrLogger.prefixLengths(fwd.actualU));
    }

    private void doShowRouteSR(int ver) {
        ipFwd fwd = findVrf(ver);
        if (fwd == null) {
            return;
        }
        doShowRoutes(fwd, fwd.actualU, 7);
    }

    private void doShowSRindex(int ver) {
        ipFwd fwd = findVrf(ver);
        if (fwd == null) {
            return;
        }
        userFormat lst = new userFormat("|", "index|prefix|bytes");
        for (int i = 0; i < fwd.actualI.size(); i++) {
            tabIndex<addrIP> prf = fwd.actualI.get(i);
            if (prf == null) {
                continue;
            }
            String a = "";
            if (prf.hwCntr != null) {
                a = "+" + prf.hwCntr.byteRx;
            }
            lst.add(prf.index + "|" + addrPrefix.ip2str(prf.prefix) + "|" + prf.cntr.byteRx + a);
        }
        rdr.putStrTab(lst);
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
                if (ntry == null) {
                    cmd.error("no such route");
                    return;
                }
            }
            ntry.rouDst = tabRtrmapN.string2rd(cmd.word());
            ntry = tab.find(ntry);
            if (ntry == null) {
                cmd.error("no such prefix");
                return;
            }
            rdr.putStrTab(ntry.fullDump(fwd));
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
                l = new userFormat("|", "prefix|pack|byte|time");
                break;
            case 7:
                l = new userFormat("|", "prefix|index|base|oldbase");
                break;
            case 8:
                l = new userFormat("|", "prefix|index|base|oldbase|size");
                break;
            case 2002:
            case 2005:
            case 9:
                l = new userFormat("|", "prefix|alts|ecmp|best|proto|source");
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
                    tabRouteEntry.toShRoute(l, prf);
                    break;
                case 2:
                    tabRouteEntry.toShBgp(l, prf);
                    break;
                case 3:
                    tabRouteEntry.toShLdp(l, prf);
                    break;
                case 1002:
                case 1005:
                    tabRouteEntry.toShBgpLabels(l, prf, typ == 1005);
                    break;
                case 4:
                    tabRouteEntry.toShRpki(l, prf);
                    break;
                case 5:
                    tabRouteEntry.toShEvpn(l, prf);
                    break;
                case 6:
                    tabRouteEntry.toShCntr(l, prf);
                    break;
                case 7:
                    tabRouteEntry.toShSrRoute(l, prf);
                    break;
                case 8:
                    tabRouteEntry.toShBrRoute(l, prf);
                    break;
                case 2002:
                case 2005:
                case 9:
                    tabRouteEntry.toShEcmp(l, prf, typ == 2005);
                    break;
            }
        }
        rdr.putStrTab(l);
    }

    private void doShowMptab(tabGen<ipFwdMpmp> tab, addrIP peer) {
        String a;
        if (peer == null) {
            a = "type|local|root|opaque|uplink|peers";
        } else {
            a = "type|local|root|label|opaque|uplink";
        }
        userFormat l = new userFormat("|", a);
        for (int i = 0; i < tab.size(); i++) {
            ipFwdMpmp ntry = tab.get(i);
            if (ntry == null) {
                continue;
            }
            l.add(ntry.dump(peer));
        }
        rdr.putStrTab(l);
    }

    private userFormat doShowRates(history h) {
        if (h == null) {
            return null;
        }
        userFormat l = new userFormat("|", "time|tx|rx|drop|tx|rx|drop", "1|3packet|3byte");
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

    private void doShowVrfRout() {
        userFormat l = new userFormat("|", "name|rd|v4|v6|v4|v6|v4|v6|v4|v6|v4|v6|v4|v6", "2|2ifc|2uni|2mlt|2flw|2lab|2con");
        for (int o = 0; o < cfgAll.vrfs.size(); o++) {
            cfgVrf v = cfgAll.vrfs.get(o);
            l.add(v.name + "|" + tabRtrmapN.rd2string(v.rd) + "|" + v.fwd4.ifaces.size() + "|" + v.fwd6.ifaces.size() + "|" + v.fwd4.actualU.size() + "|" + v.fwd6.actualU.size() + "|" + v.fwd4.actualM.size() + "|" + v.fwd6.actualM.size() + "|" + v.fwd4.actualF.size() + "|" + v.fwd6.actualF.size() + "|" + v.fwd4.labeldR.size() + "|" + v.fwd6.labeldR.size() + "|" + v.fwd4.connedR.size() + "|" + v.fwd6.connedR.size());
        }
        rdr.putStrTab(l);
    }

    private void doShowVrfIcmp() {
        userFormat l = new userFormat("|", "name|rd|v4|v6|v4|v6|v4|v6|v4|v6|v4|v6|v4|v6", "2|2echSnt|2echGot|2echOk|2echPnd|2errSnt|2errGot");
        for (int o = 0; o < cfgAll.vrfs.size(); o++) {
            cfgVrf v = cfgAll.vrfs.get(o);
            l.add(v.name + "|" + tabRtrmapN.rd2string(v.rd) + "|" + v.fwd4.echoSent + "|" + v.fwd6.echoSent + "|" + v.fwd4.echoRcvd + "|" + v.fwd6.echoRcvd + "|" + v.fwd4.echoRply + "|" + v.fwd6.echoRply + "|" + v.fwd4.echoes.size() + "|" + v.fwd6.echoes.size() + "|" + v.fwd4.errorSent + "|" + v.fwd6.errorSent + "|" + v.fwd4.errorRcvd + "|" + v.fwd6.errorRcvd);
        }
        rdr.putStrTab(l);
    }

    private void doShowVrfTraff() {
        if (cmd.size() > 0) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                cmd.error("no such vrf");
                return;
            }
            doShowHistory(cmd.word(), vrf.fwd4.hstryT.plus(vrf.fwd6.hstryT));
            return;
        }
        userFormat l = new userFormat("|", "name|rd|pack|byte|pack|byte|pack|byte", "2|2total|2local|2forward");
        for (int o = 0; o < cfgAll.vrfs.size(); o++) {
            cfgVrf v = cfgAll.vrfs.get(o);
            counter ct = v.fwd4.cntrT.plus(v.fwd6.cntrT).sumUp(false);
            counter cl = v.fwd4.cntrL.plus(v.fwd6.cntrL).sumUp(false);
            counter cf = ct.minus(cl);
            l.add(v.name + "|" + tabRtrmapN.rd2string(v.rd) + "|" + ct.packRx + "|" + ct.byteRx + "|" + cl.packRx + "|" + cl.byteRx + "|" + cf.packRx + "|" + cf.byteRx);
        }
        rdr.putStrTab(l);
    }

    private void doShowSession(tabSession ses) {
        if (ses == null) {
            cmd.error("not enabled");
            return;
        }
        String a = cmd.word();
        if (a.equals("session")) {
            rdr.putStrTab(ses.doShowInsp());
            return;
        }
        if (a.equals("toptalk")) {
            rdr.putStrTab(ses.doShowTalk());
            return;
        }
    }

    private static int getConfigFilter(int flt, String cmd) {
        if (cmd.equals("all")) {
            return flt & ~1;
        }
        if (cmd.equals("hide")) {
            return flt | 2;
        }
        return flt;
    }

    private static int getConfigFilter(String ini, cmds cmd) {
        int flt = 1;
        if (ini != null) {
            flt = getConfigFilter(flt, ini);
        }
        for (;;) {
            String a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            flt = getConfigFilter(flt, a);
        }
        return flt;
    }

}
