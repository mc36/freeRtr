package net.freertr.user;

import java.util.ArrayList;
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
import net.freertr.cfg.cfgTime;
import net.freertr.cfg.cfgTrack;
import net.freertr.cfg.cfgVdc;
import net.freertr.cfg.cfgVdcIfc;
import net.freertr.cfg.cfgVpdn;
import net.freertr.cfg.cfgVrf;
import net.freertr.clnt.clntCurl;
import net.freertr.clnt.clntDns;
import net.freertr.clnt.clntFtp;
import net.freertr.clnt.clntHttp;
import net.freertr.clnt.clntNetflow;
import net.freertr.clnt.clntProxy;
import net.freertr.clnt.clntSmtp;
import net.freertr.clnt.clntTftp;
import net.freertr.clnt.clntWhois;
import net.freertr.ifc.ifcPolka;
import net.freertr.ifc.ifcThread;
import net.freertr.ip.ipFwd;
import net.freertr.ip.ipFwdIface;
import net.freertr.ip.ipFwdMcast;
import net.freertr.ip.ipFwdMpmp;
import net.freertr.ip.ipFwdTab;
import net.freertr.ip.ipFwdTrfng;
import net.freertr.ip.ipRtr;
import net.freertr.pack.packDnsRec;
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
import net.freertr.serv.servAmt;
import net.freertr.serv.servBmp2mrt;
import net.freertr.serv.servDhcp4;
import net.freertr.serv.servDhcp6;
import net.freertr.serv.servDns;
import net.freertr.serv.servEtherIp;
import net.freertr.serv.servGenList;
import net.freertr.serv.servGeneric;
import net.freertr.serv.servGre;
import net.freertr.serv.servGtp;
import net.freertr.serv.servHttp;
import net.freertr.serv.servL2f;
import net.freertr.serv.servL2tp2;
import net.freertr.serv.servL2tp3;
import net.freertr.serv.servMplsIp;
import net.freertr.serv.servMplsUdp;
import net.freertr.serv.servNetflow;
import net.freertr.serv.servP4lang;
import net.freertr.serv.servPckOudp;
import net.freertr.serv.servSdwan;
import net.freertr.serv.servSmtp;
import net.freertr.serv.servStreamingMdt;
import net.freertr.serv.servVxlan;
import net.freertr.enc.enc7bit;
import net.freertr.enc.encUrl;
import net.freertr.pack.packRedundancy;
import net.freertr.serv.servOpenflow;
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
import net.freertr.tab.tabRouteUtil;
import net.freertr.tab.tabRtrmapN;
import net.freertr.tab.tabSession;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.counter;
import net.freertr.util.differ;
import net.freertr.util.history;
import net.freertr.util.logBuf;
import net.freertr.util.logger;
import net.freertr.util.syncInt;
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
            rdr.putStrArr(version.shLogo(0x40e0));
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
            if (a.equals("ascii")) {
                int x = cmd.pipe.settingsGet(pipeSetting.width, 80);
                int y = cmd.pipe.settingsGet(pipeSetting.height, 25);
                List<String> lst = userFlash.asciiArt(cmd.getRemaining(), x, y);
                rdr.putStrArr(lst);
                return null;
            }
            if (a.equals("meme")) {
                String nam = cmd.word();
                String tld = cmd.word();
                String trg = cmd.word();
                String red = cmd.word();
                List<String> res = new ArrayList<String>();
                res.add("server dns ns");
                res.add("      zone " + tld + " rr " + nam + "." + tld + " cname " + red);
                res.add("server http inet");
                res.add("      host " + nam + "." + tld + " redir http://" + trg);
                rdr.putStrArr(res);
                return null;
            }
            if (a.equals("hack")) {
                a = cmd.getRemaining();
                a = enc7bit.toHackedStr(a);
                rdr.putStrArr(bits.str2lst(a));
                return null;
            }
            if (a.equals("7bit")) {
                a = cmd.getRemaining();
                a = enc7bit.doOneString(a);
                rdr.putStrArr(bits.str2lst(a));
                return null;
            }
            if (a.equals("rev7")) {
                a = cmd.getRemaining();
                a = enc7bit.decodeExtStr(a);
                rdr.putStrArr(bits.str2lst(a));
                return null;
            }
            if (a.equals("time")) {
                a = bits.time2str(cfgAll.timeZoneName, bits.getTime(), 2);
                List<String> l = userScreen.fontText(a, " ", userFonts.fontFiller, userFonts.font8x16());
                rdr.putStrArr(l);
                return null;
            }
            int i = version.findSecret(a);
            rdr.putStrArr(version.shSecret(i));
            return null;
        }
        if (a.equals("users")) {
            userFormat res = userLine.listLoggedIns();
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
            rdr.putStrArr(enc7bit.doOneArray(cfgAll.banner, "banner"));
            return null;
        }
        if (a.equals("logo")) {
            a = cmd.getRemaining();
            List<String> l;
            if (a.length() > 0) {
                l = userScreen.fontText(a, " ", userFonts.fontFiller, userFonts.font8x16());
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
        if (a.equals("disk")) {
            a = cmd.getRemaining();
            if (verCore.release) {
                a = "";
            }
            if (a.length() < 1) {
                a = "./";
            }
            rdr.putStrTab(userFlash.diskInfo(a));
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
            rdr.putStrTab(aaa.getShowGlob());
            rdr.putStrTab(aaa.getShowSpec());
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
                cmd.error("not enabled");
                return null;
            }
            userFormat l = ifc.pppoeS.getShow();
            rdr.putStrTab(l);
            return null;
        }
        if (a.equals("macsec")) {
            a = cmd.word();
            cfgIfc ifc = cfgAll.ifcFind(a, 0);
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
                l = userScreen.fontText(bits.time2str(cfgAll.timeZoneName, tim, 2), " ", userFonts.fontFiller, userFonts.font8x16());
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
            userFormat l = new userFormat("|", "name|rerun|ago|last");
            for (int i = 0; i < cfgAll.schedulers.size(); i++) {
                cfgSched ntry = cfgAll.schedulers.get(i);
                l.add(ntry.getShow());
            }
            rdr.putStrTab(l);
            return null;
        }
        if (a.equals("script")) {
            userFormat l = new userFormat("|", "name|rerun|ago|last");
            for (int i = 0; i < cfgAll.scripts.size(); i++) {
                cfgScrpt ntry = cfgAll.scripts.get(i);
                l.add(ntry.getShow());
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
                    l.add(ntry.name + "|" + (cfgAll.ifcFind(ntry.name, 0) == null));
                }
                rdr.putStrTab(l);
                return null;
            }
            if (a.equals("device")) {
                userFormat l = new userFormat("|", "name|rerun|pid|chld|cpu|ago|last");
                for (int i = 0; i < cfgInit.vdcLst.size(); i++) {
                    cfgVdc ntry = cfgInit.vdcLst.get(i);
                    l.add(ntry.getShow());
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
                a = cmd.word();
                if (a.length() < 1) {
                    rdr.putStrTab(logger.listThreads());
                } else {
                    rdr.putStrTab(logger.listThreads(bits.str2long(a)));
                }
                return null;
            }
            if (a.equals("external")) {
                userFormat l = new userFormat("|", "name|rerun|pid|chld|cpu|ago|last");
                for (int i = 0; i < cfgAll.prcs.size(); i++) {
                    cfgPrcss ntry = cfgAll.prcs.get(i);
                    l.add(ntry.getShow());
                }
                rdr.putStrTab(l);
                return null;
            }
            cmd.badCmd();
            return null;
        }
        if (a.equals("redundancy")) {
            a = cmd.word();
            if (a.equals("status")) {
                rdr.putStrTab(prtRedun.doShowStatus());
                return null;
            }
            if (a.equals("description")) {
                rdr.putStrTab(prtRedun.doShowDescr());
                return null;
            }
            if (a.equals("core")) {
                rdr.putStrTab(prtRedun.doShowHash(packRedundancy.fnCore));
                return null;
            }
            if (a.equals("config")) {
                rdr.putStrTab(prtRedun.doShowHash(packRedundancy.fnStart));
                return null;
            }
            if (a.equals("platform")) {
                rdr.putStrTab(prtRedun.doShowHash(packRedundancy.fnPlatform));
                return null;
            }
            if (a.equals("remote-status")) {
                rdr.putStrTab(prtRedun.doShowHash(packRedundancy.fnRemRedun));
                return null;
            }
            rdr.putStrTab(prtRedun.doShowStatus());
            return null;
        }
        if (a.equals("as-info")) {
            a = cmd.word();
            int i = bits.str2num(a);
            a = clntWhois.asn2info(i);
            cmd.error("asn " + i + " have info at " + a);
            rdr.putStrArr(clntWhois.asn2infos(i));
            return null;
        }
        if (a.equals("as-name")) {
            long tim = bits.getTime();
            a = cmd.word();
            int i = bits.str2num(a);
            a = clntWhois.asn2name(i, true);
            rdr.putStrArr(bits.str2lst("just queried asn " + i + " is " + a + " in " + bits.timePast(tim)));
            return null;
        }
        if (a.equals("asn-cache")) {
            rdr.putStrTab(clntWhois.showLocalCache());
            return null;
        }
        if (a.equals("name-cache")) {
            rdr.putStrTab(clntDns.showLocalCache(false));
            rdr.putStrTab(clntDns.showLocalCache(true));
            return null;
        }
        if (a.equals("resolve")) {
            a = cmd.word();
            int i = clntDns.getTypPri(0);
            addrIP adr = new addrIP();
            if (!adr.fromString(a)) {
                a = packDnsRec.generateReverse(adr);
                i = packDnsRec.typePTR;
            }
            cmd.error("resolving " + packDnsRec.type2str(i) + " " + a);
            List<addrIP> srvs = new ArrayList<addrIP>();
            srvs.addAll(cfgAll.nameServerAddr);
            clntDns clnt = new clntDns();
            clnt.doResolvList(srvs, a, true, i);
            packDnsRec res = clnt.findAnswer(i);
            if (res == null) {
                cmd.error("no reply");
                return null;
            }
            rdr.putStrArr(res.toUserStr(" ", "", false));
            return null;
        }
        if (a.equals("url")) {
            a = cmd.getRemaining();
            List<String> res = clntCurl.doGetUrl(cmd.pipe, a);
            cmd.error(cmds.doneFail(res == null));
            rdr.putStrArr(res);
            return null;
        }
        if (a.equals("whois")) {
            if (cfgAll.whoisServer == null) {
                cmd.error("not enabled");
                return null;
            }
            clntWhois w = new clntWhois(cmd.pipe, cfgAll.getClntPrx(cfgAll.whoisProxy), cfgAll.whoisServer);
            a = cmd.getRemaining();
            rdr.putStrArr(w.doQuery(a));
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
            if (a.equals("script")) {
                cfgScrpt ntry = cfgAll.scrptFind(cmd.word(), false);
                if (ntry == null) {
                    cmd.error("no such script");
                    return null;
                }
                rdr.putStrArr(logBuf.getLines(ntry.logCol));
                return null;
            }
            if (a.equals("scheduler")) {
                cfgSched ntry = cfgAll.schedFind(cmd.word(), false);
                if (ntry == null) {
                    cmd.error("no such scheduler");
                    return null;
                }
                rdr.putStrArr(logBuf.getLines(ntry.logCol));
                return null;
            }
            if (a.equals("process")) {
                cfgPrcss ntry = cfgAll.prcFind(cmd.word(), false);
                if (ntry == null) {
                    cmd.error("no such process");
                    return null;
                }
                rdr.putStrArr(logBuf.getLines(ntry.logCol));
                return null;
            }
            if (a.equals("vdc")) {
                cfgVdc ntry = cfgAll.vdcFind(cmd.word(), false);
                if (ntry == null) {
                    cmd.error("no such vdc");
                    return null;
                }
                rdr.putStrArr(logBuf.getLines(ntry.logCol));
                return null;
            }
            if (a.equals("file")) {
                rdr.putStrArr(bits.txt2buf(logger.fileName()));
                return null;
            }
            if (a.equals("old-file")) {
                a = logger.fileRotate();
                if (a == null) {
                    return null;
                }
                cmd = new cmds("fn", a);
                cmd.word();
                rdr.putStrArr(bits.txt2buf(cmd.word()));
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
            if ((!verCore.release) && (cfgAll.limited)) {
                cmd.error("not in a vdc");
                return null;
            }
            rdr.putStrArr(userFilter.getDiffs(cfgAll.getShRun(1), bits.txt2buf(cfgInit.cfgFileSw)));
            return null;
        }
        if (a.equals("config-differences")) {
            if ((!verCore.release) && (cfgAll.limited)) {
                cmd.error("not in a vdc");
                return null;
            }
            rdr.putStrArr(userFilter.getDiffs(bits.txt2buf(cfgInit.cfgFileSw), cfgAll.getShRun(1)));
            return null;
        }
        if (a.equals("startup-config")) {
            if ((!verCore.release) && (cfgAll.limited)) {
                cmd.error("not in a vdc");
                return null;
            }
            List<String> lst = bits.txt2buf(cfgInit.cfgFileSw);
            if (cmd.size() > 0) {
                lst = userFilter.getSection(lst, userReader.filter2reg(cmd.getRemaining()));
            }
            rdr.putStrArr(lst);
            return null;
        }
        if (a.equals("running-config")) {
            a = cmd.word();
            if (a.equals("console0")) {
                rdr.putStrArr(cfgAll.con0.getShRun(getConfigFilter(null, cmd)));
                return null;
            }
            if (a.equals("this")) {
                if (cfg == null) {
                    return null;
                }
                rdr.putStrArr(cfg.getShRun(getConfigFilter(null, cmd)));
                return null;
            }
            if ((!verCore.release) && (cfgAll.limited)) {
                cmd.error("not in a vdc");
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
            if (a.equals("server")) {
                a = cmd.word();
                servGeneric ntry = servGenList.srvFind(a, cmd.word(), false);
                if (ntry == null) {
                    cmd.error("invalid server");
                    return null;
                }
                int filt = getConfigFilter(null, cmd);
                rdr.putStrArr(ntry.getShRun(filt));
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
            if (a.equals("vrf")) {
                cfgVrf v = cfgAll.vrfFind(cmd.word(), false);
                if (v == null) {
                    cmd.error("no such vrf");
                    return null;
                }
                int filt = getConfigFilter(null, cmd);
                rdr.putStrArr(v.getShRun(filt));
                rdr.putStrArr(v.getShRun2(filt));
                return null;
            }
            if (a.equals("route-map")) {
                cfgRoump v = cfgAll.rtmpFind(cmd.word(), false);
                if (v == null) {
                    cmd.error("no such route map");
                    return null;
                }
                int filt = getConfigFilter(null, cmd);
                rdr.putStrArr(v.getShRun(filt));
                return null;
            }
            if (a.equals("route-policy")) {
                cfgRouplc v = cfgAll.rtplFind(cmd.word(), false);
                if (v == null) {
                    cmd.error("no such route policy");
                    return null;
                }
                int filt = getConfigFilter(null, cmd);
                rdr.putStrArr(v.getShRun(filt));
                return null;
            }
            if (a.equals("prefix-list")) {
                cfgPrfxlst v = cfgAll.prfxFind(cmd.word(), false);
                if (v == null) {
                    cmd.error("no such prefix list");
                    return null;
                }
                int filt = getConfigFilter(null, cmd);
                rdr.putStrArr(v.getShRun(filt));
                return null;
            }
            if (a.equals("access-list")) {
                cfgAceslst v = cfgAll.aclsFind(cmd.word(), false);
                if (v == null) {
                    cmd.error("no such access list");
                    return null;
                }
                int filt = getConfigFilter(null, cmd);
                rdr.putStrArr(v.getShRun(filt));
                return null;
            }
            if (a.equals("interface")) {
                cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
                if (ifc == null) {
                    cmd.error("no such interface");
                    return null;
                }
                rdr.putStrArr(ifc.getShRun(0x40000000 | getConfigFilter(null, cmd)));
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
        if (a.equals("amt")) {
            servAmt srv = cfgAll.srvrFind(new servAmt(), cfgAll.dmnAmt, cmd.word());
            if (srv == null) {
                cmd.error("no such server");
                return null;
            }
            rdr.putStrTab(srv.getShow());
            return null;
        }
        if (a.equals("etherip")) {
            servEtherIp srv = cfgAll.srvrFind(new servEtherIp(), cfgAll.dmnEtherIp, cmd.word());
            if (srv == null) {
                cmd.error("no such server");
                return null;
            }
            rdr.putStrTab(srv.getShow());
            return null;
        }
        if (a.equals("gre")) {
            servGre srv = cfgAll.srvrFind(new servGre(), cfgAll.dmnGre, cmd.word());
            if (srv == null) {
                cmd.error("no such server");
                return null;
            }
            rdr.putStrTab(srv.getShow());
            return null;
        }
        if (a.equals("gtp")) {
            servGtp srv = cfgAll.srvrFind(new servGtp(), cfgAll.dmnGtp, cmd.word());
            if (srv == null) {
                cmd.error("no such server");
                return null;
            }
            rdr.putStrTab(srv.getShow());
            return null;
        }
        if (a.equals("l2f")) {
            servL2f srv = cfgAll.srvrFind(new servL2f(), cfgAll.dmnL2f, cmd.word());
            if (srv == null) {
                cmd.error("no such server");
                return null;
            }
            rdr.putStrTab(srv.getShow());
            return null;
        }
        if (a.equals("l2tp2")) {
            servL2tp2 srv = cfgAll.srvrFind(new servL2tp2(), cfgAll.dmnL2tp2, cmd.word());
            if (srv == null) {
                cmd.error("no such server");
                return null;
            }
            rdr.putStrTab(srv.getShow());
            return null;
        }
        if (a.equals("l2tp3")) {
            servL2tp3 srv = cfgAll.srvrFind(new servL2tp3(), cfgAll.dmnL2tp3, cmd.word());
            if (srv == null) {
                cmd.error("no such server");
                return null;
            }
            rdr.putStrTab(srv.getShow());
            return null;
        }
        if (a.equals("pckoudp")) {
            servPckOudp srv = cfgAll.srvrFind(new servPckOudp(), cfgAll.dmnPckOudp, cmd.word());
            if (srv == null) {
                cmd.error("no such server");
                return null;
            }
            rdr.putStrTab(srv.getShow());
            return null;
        }
        if (a.equals("vxlan")) {
            servVxlan srv = cfgAll.srvrFind(new servVxlan(), cfgAll.dmnVxlan, cmd.word());
            if (srv == null) {
                cmd.error("no such server");
                return null;
            }
            rdr.putStrTab(srv.getShow());
            return null;
        }
        if (a.equals("sdwan")) {
            servSdwan srv = cfgAll.srvrFind(new servSdwan(), cfgAll.dmnSdwan, cmd.word());
            if (srv == null) {
                cmd.error("no such server");
                return null;
            }
            rdr.putStrTab(srv.getShow());
            return null;
        }
        if (a.equals("http")) {
            servHttp srv = cfgAll.srvrFind(new servHttp(), cfgAll.dmnHttp, cmd.word());
            if (srv == null) {
                cmd.error("no such server");
                return null;
            }
            a = cmd.word();
            if (a.equals("stat")) {
                rdr.putStrTab(srv.getShStat());
                return null;
            }
            if (a.equals("zone")) {
                a = cmd.word();
                rdr.putStrArr(srv.getShZone(a));
                return null;
            }
            rdr.putStrTab(srv.getShStat());
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
        if (a.equals("clients")) {
            userFormat l = new userFormat("|", "name|start|error|stop|wait");
            doOneClient(l, "proxy", clntProxy.cntrStart, clntProxy.cntrError, clntProxy.cntrStop);
            doOneClient(l, "whois", clntWhois.cntrStart, clntWhois.cntrError, clntWhois.cntrStop);
            doOneClient(l, "smtp", clntSmtp.cntrStart, clntSmtp.cntrError, clntSmtp.cntrStop);
            doOneClient(l, "http", clntHttp.cntrStart, clntHttp.cntrError, clntHttp.cntrStop);
            doOneClient(l, "tftp", clntTftp.cntrStart, clntTftp.cntrError, clntTftp.cntrStop);
            doOneClient(l, "ftp", clntFtp.cntrStart, clntFtp.cntrError, clntFtp.cntrStop);
            doOneClient(l, "dns", clntDns.cntrStart, clntDns.cntrError, clntDns.cntrStop);
            rdr.putStrTab(l);
            return null;
        }
        if (a.equals("check")) {
            a = cmd.word();
            if (a.length() < 1) {
                userFormat l = new userFormat("|", "name|state|asked|reply|times|last|times|last", "4|2pass|2fail");
                for (int i = 0; i < cfgAll.checks.size(); i++) {
                    cfgCheck ntry = cfgAll.checks.get(i);
                    l.add(ntry.name + "|" + ntry.doCheckBinary() + "|" + (ntry.okNum + ntry.errNum) + "|" + ntry.time + "|" + ntry.okNum + "|" + bits.timePast(ntry.okTim) + "|" + ntry.errNum + "|" + bits.timePast(ntry.errTim));
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
        if (a.equals("ppp")) {
            cfgIfc ntry = cfgAll.ifcFind(cmd.word(), 0);
            if (ntry == null) {
                cmd.error("no such interface");
                return null;
            }
            if (ntry.ppp == null) {
                cmd.error("not enabled");
                return null;
            }
            rdr.putStrTab(ntry.ppp.getShow());
            return null;
        }
        if (a.equals("vpdn")) {
            cfgVpdn cln = cfgAll.vpdnFind(cmd.word(), false);
            if (cln == null) {
                cmd.error("no such vpdn");
                return null;
            }
            rdr.putStrTab(cln.getShow());
            return null;
        }
        if (a.equals("openflow")) {
            a = cmd.word();
            servOpenflow gen = cfgAll.dmnOpenflow.get(0);
            if (gen == null) {
                return null;
            }
            if (a.length() < 1) {
                a = gen.getShGenOneLiner();
                rdr.putStrArr(bits.str2lst(a));
                return null;
            }
            servOpenflow srv = cfgAll.srvrFind(new servOpenflow(), cfgAll.dmnOpenflow, a);
            if (srv == null) {
                cmd.error("no such server");
                return null;
            }
            a = cmd.word();
            if (a.equals("general")) {
                a = gen.getShGenOneLiner();
                rdr.putStrArr(bits.str2lst(a));
                return null;
            }
            a = srv.getShGenOneLiner();
            rdr.putStrArr(bits.str2lst(a));
            return null;
        }
        if (a.equals("p4lang")) {
            a = cmd.word();
            servP4lang gen = cfgAll.dmnP4lang.get(0);
            if (gen == null) {
                return null;
            }
            if (a.length() < 1) {
                a = gen.getShGenOneLiner();
                rdr.putStrArr(bits.str2lst(a));
                return null;
            }
            servP4lang srv = cfgAll.srvrFind(new servP4lang(), cfgAll.dmnP4lang, a);
            if (srv == null) {
                cmd.error("no such server");
                return null;
            }
            a = cmd.word();
            if (a.length() < 1) {
                a = srv.getShGenOneLiner();
                rdr.putStrArr(bits.str2lst(a));
                return null;
            }
            if (a.equals("general")) {
                a = srv.getShGenOneLiner();
                rdr.putStrArr(bits.str2lst(a));
                return null;
            }
            if (a.equals("dataplanes")) {
                rdr.putStrTab(srv.getShowGen(1));
                return null;
            }
            if (a.equals("config")) {
                rdr.putStrTab(srv.getShowGen(bits.str2num(cmd.word()), 1));
                return null;
            }
            if (a.equals("status")) {
                rdr.putStrTab(srv.getShowGen(bits.str2num(cmd.word()), 1));
                return null;
            }
            if (a.equals("api-tx")) {
                rdr.putStrTab(srv.getShowGen(bits.str2num(cmd.word()), 2));
                return null;
            }
            if (a.equals("api-rx")) {
                rdr.putStrTab(srv.getShowGen(bits.str2num(cmd.word()), 3));
                return null;
            }
            if (a.equals("port-names")) {
                rdr.putStrTab(srv.getShowGen(bits.str2num(cmd.word()), 4));
                return null;
            }
            if (a.equals("port-magics")) {
                rdr.putStrTab(srv.getShowGen(bits.str2num(cmd.word()), 9));
                return null;
            }
            if (a.equals("done-vrf")) {
                rdr.putStrTab(srv.getShowGen(bits.str2num(cmd.word()), 10));
                return null;
            }
            if (a.equals("done-interface")) {
                rdr.putStrTab(srv.getShowGen(bits.str2num(cmd.word()), 5));
                return null;
            }
            if (a.equals("done-neighbor")) {
                rdr.putStrTab(srv.getShowGen(bits.str2num(cmd.word()), 6));
                return null;
            }
            if (a.equals("done-mpls")) {
                rdr.putStrTab(srv.getShowGen(bits.str2num(cmd.word()), 7));
                return null;
            }
            if (a.equals("done-nsh")) {
                rdr.putStrTab(srv.getShowGen(bits.str2num(cmd.word()), 8));
                return null;
            }
            if (a.equals("done-bridge")) {
                int i = bits.str2num(cmd.word());
                rdr.putStrTab(srv.getShowBri(i, bits.str2num(cmd.word())));
                return null;
            }
            if (a.equals("done-route4")) {
                int i = bits.str2num(cmd.word());
                doShowRoutes(null, srv.getShowRou(4, i, bits.str2num(cmd.word())), 1);
                return null;
            }
            if (a.equals("done-route6")) {
                int i = bits.str2num(cmd.word());
                doShowRoutes(null, srv.getShowRou(6, i, bits.str2num(cmd.word())), 1);
                return null;
            }
            if (a.equals("backplane-ports")) {
                rdr.putStrTab(srv.getShowBp1(bits.str2num(cmd.word()), 1));
                return null;
            }
            if (a.equals("backplane-spf")) {
                rdr.putStrTab(srv.getShowBp1(bits.str2num(cmd.word()), 2));
                return null;
            }
            if (a.equals("backplane-topology")) {
                rdr.putStrTab(srv.getShowBp1(bits.str2num(cmd.word()), 3));
                return null;
            }
            if (a.equals("backplane-tree")) {
                rdr.putStrArr(srv.getShowBp2(bits.str2num(cmd.word()), 1));
                return null;
            }
            if (a.equals("backplane-graph")) {
                rdr.putStrArr(srv.getShowBp2(bits.str2num(cmd.word()), 2));
                return null;
            }
            if (a.equals("backplane-route")) {
                doShowRoutes(null, srv.getShowBp3(bits.str2num(cmd.word())), 1);
                return null;
            }
            if (!a.equals("port-counters")) {
                rdr.putStrTab(srv.getShowGen(1));
                return null;
            }
            a = cmd.word();
            cfgIfc ifc = cfgAll.ifcFind(a, 0);
            if (ifc == null) {
                cmd.error("no such interface");
                return null;
            }
            rdr.putStrTab(srv.getShowIface(bits.str2num(cmd.word()), ifc));
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
            rdr.putStrTab(brdg.bridgeHed.getShowStp());
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
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
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
                cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
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
                cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
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
                cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
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
                cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
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
                rdr.putStrTab(tab.getStats(true));
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
                rdr.putStrTab(tab.getStats(false));
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
                rdr.putStrTab(tab.getStats(false));
                return null;
            }
            if (a.equals("interface")) {
                cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
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
                rdr.putStrTab(tab.getStats(false));
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
                rdr.putStrTab(og.objgrp.getStats(3));
                return null;
            }
            if (a.equals("port")) {
                cfgObjprt og = cfgAll.objprtFind(cmd.word(), false);
                if (og == null) {
                    cmd.error("no such object group");
                    return null;
                }
                rdr.putStrTab(og.objgrp.getStats(3));
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
            rdr.putStrTab(acl.aceslst.getStats(256 | 3));
            return null;
        }
        if (a.equals("time-map")) {
            cfgTime rtmp = cfgAll.timeFind(cmd.word(), false);
            if (rtmp == null) {
                cmd.error("no such time map");
                return null;
            }
            long tim;
            a = cmd.getRemaining();
            if (a.length() < 1) {
                tim = bits.getTime();
            } else {
                tim = bits.str2time(cfgAll.timeZoneName, a);
            }
            rdr.putStrArr(bits.str2lst(bits.time2str(cfgAll.timeZoneName, tim + cfgAll.timeServerOffset, 3) + " returns " + rtmp.matches(tim)));
            return null;
        }
        if (a.equals("route-map")) {
            cfgRoump rtmp = cfgAll.rtmpFind(cmd.word(), false);
            if (rtmp == null) {
                cmd.error("no such route map");
                return null;
            }
            rdr.putStrTab(rtmp.roumap.getStats(256 | 3));
            return null;
        }
        if (a.equals("route-policy")) {
            cfgRouplc rtpl = cfgAll.rtplFind(cmd.word(), false);
            if (rtpl == null) {
                cmd.error("no such route policy");
                return null;
            }
            rdr.putStrTab(rtpl.rouplc.getStats(3));
            return null;
        }
        if (a.equals("prefix-list")) {
            cfgPrfxlst prfx = cfgAll.prfxFind(cmd.word(), false);
            if (prfx == null) {
                cmd.error("no such prefix list");
                return null;
            }
            rdr.putStrTab(prfx.prflst.getStats(256 | 3));
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
            if (a.equals("status")) {
                rdr.putStrTab(cfgAll.getShIntTab(27));
                return null;
            }
            if (a.equals("lastio")) {
                rdr.putStrTab(cfgAll.getShIntTab(28));
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
            if (a.equals("bpratio")) {
                rdr.putStrTab(cfgAll.getShIntTab(29));
                return null;
            }
            if (a.equals("hwswratio")) {
                rdr.putStrTab(cfgAll.getShIntTab(30));
                return null;
            }
            if (a.equals("hwswpratio")) {
                rdr.putStrTab(cfgAll.getShIntTab(31));
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
            cfgIfc ifc = cfgAll.ifcFind(a, 0);
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
                cfgIfc ntry = cfgAll.ifcFind(a, 0);
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
                cfgIfc ntry = cfgAll.ifcFind(a, 0);
                if (ntry == null) {
                    cmd.error("no such interface");
                    return null;
                }
                if (ntry.polkaPack == null) {
                    cmd.error("not enabled");
                    return null;
                }
                rdr.putStrTab(ifcPolka.getShow(ntry.polkaPack.coeffs));
                return null;
            }
            return null;
        }
        if (a.equals("mpolka")) {
            a = cmd.word();
            if (a.equals("routeid")) {
                a = cmd.word();
                cfgIfc ntry = cfgAll.ifcFind(a, 0);
                if (ntry == null) {
                    cmd.error("no such interface");
                    return null;
                }
                if (ntry.tunMpolka == null) {
                    cmd.error("not enabled");
                    return null;
                }
                rdr.putStrTab(ntry.tunMpolka.getShRoute());
                rdr.putStrTab(ntry.tunMpolka.getShDecode());
                return null;
            }
            if (a.equals("interfaces")) {
                a = cmd.word();
                if (a.length() < 1) {
                    userFormat lst = new userFormat("|", "interface|packet|headend");
                    for (int i = 0; i < cfgAll.ifaces.size(); i++) {
                        cfgIfc ntry = cfgAll.ifaces.get(i);
                        lst.add(ntry.name + "|" + (ntry.mpolkaPack != null) + "|" + (ntry.tunMpolka != null));
                    }
                    rdr.putStrTab(lst);
                    return null;
                }
                cfgIfc ntry = cfgAll.ifcFind(a, 0);
                if (ntry == null) {
                    cmd.error("no such interface");
                    return null;
                }
                if (ntry.mpolkaPack == null) {
                    cmd.error("not enabled");
                    return null;
                }
                rdr.putStrTab(ifcPolka.getShow(ntry.mpolkaPack.coeffs));
                return null;
            }
            return null;
        }
        if (a.equals("sgt")) {
            userFormat lst = new userFormat("|", "interface|tagging|assign");
            for (int i = 0; i < cfgAll.ifaces.size(); i++) {
                cfgIfc ntry = cfgAll.ifaces.get(i);
                lst.add(ntry.name + "|" + ntry.ethtyp.getSgt() + "|" + ntry.ethtyp.sgtSet);
            }
            rdr.putStrTab(lst);
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
                cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
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
            if (a.equals("server-udp")) {
                servMplsUdp srv = cfgAll.srvrFind(new servMplsUdp(), cfgAll.dmnMplsUdp, cmd.word());
                if (srv == null) {
                    cmd.error("no such server");
                    return null;
                }
                rdr.putStrTab(srv.getShow());
                return null;
            }
            if (a.equals("server-ip")) {
                servMplsIp srv = cfgAll.srvrFind(new servMplsIp(), cfgAll.dmnMplsIp, cmd.word());
                if (srv == null) {
                    cmd.error("no such server");
                    return null;
                }
                rdr.putStrTab(srv.getShow());
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
                cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
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
            if (a.equals("ghosthunt")) {
                doShowIpXghosthunt(tabRouteAttr.routeType.ghosthunt4);
                return null;
            }
            if (a.equals("isis")) {
                doShowIpXisis(tabRouteAttr.routeType.isis4);
                return null;
            }
            if (a.equals("rift")) {
                doShowIpXrift(tabRouteAttr.routeType.rift4);
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
                if (r.ospf4 == null) {
                    cmd.error("uninitialized process");
                    return null;
                }
                a = cmd.word();
                if (a.equals("neighbor")) {
                    rdr.putStrTab(r.ospf4.showNeighs(cmd.word().equals("brief")));
                    return null;
                }
                if (a.equals("metric")) {
                    rdr.putStrTab(r.ospf4.showMetrics());
                    return null;
                }
                if (a.equals("interface")) {
                    rdr.putStrTab(r.ospf4.showIfaces());
                    return null;
                }
                if (a.equals("flexalgo")) {
                    int i = bits.str2num(cmd.word());
                    rdr.putStrTab(r.ospf4.showAlgorithms(i));
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
                    int o = getGraphMask();
                    rdr.putStrArr(r.ospf4.showSpfGraph(i, o));
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
                if (r.rip4 == null) {
                    cmd.error("uninitialized process");
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
                cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
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
                doShowRouteCount(4);
                return null;
            }
            if (a.equals("compress")) {
                doShowRouteCompr(4);
                return null;
            }
            if (a.equals("changes")) {
                doShowRouteChngs(4);
                return null;
            }
            if (a.equals("route")) {
                doShowRouteUni(4);
                return null;
            }
            if (a.equals("ecmp")) {
                doShowRouteEcmp(4);
                return null;
            }
            if (a.equals("labels")) {
                doShowRouteLab(4);
                return null;
            }
            if (a.equals("just-network")) {
                doShowRouteNet(4);
                return null;
            }
            if (a.equals("just-interface")) {
                doShowRouteIfc(4);
                return null;
            }
            if (a.equals("just-nexthop")) {
                doShowRouteHop(4);
                return null;
            }
            if (a.equals("just-recursive")) {
                doShowRouteRec(4);
                return null;
            }
            if (a.equals("just-protocol")) {
                doShowRoutePrt(4);
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
                doShowRouteMul(4);
                return null;
            }
            if (a.equals("flwspc")) {
                doShowRouteFlw(4);
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
                cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
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
            if (a.equals("ghosthunt")) {
                doShowIpXghosthunt(tabRouteAttr.routeType.ghosthunt6);
                return null;
            }
            if (a.equals("isis")) {
                doShowIpXisis(tabRouteAttr.routeType.isis6);
                return null;
            }
            if (a.equals("rift")) {
                doShowIpXrift(tabRouteAttr.routeType.rift6);
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
                if (r.ospf6 == null) {
                    cmd.error("uninitialized process");
                    return null;
                }
                a = cmd.word();
                if (a.equals("neighbor")) {
                    rdr.putStrTab(r.ospf6.showNeighs(cmd.word().equals("brief")));
                    return null;
                }
                if (a.equals("metric")) {
                    rdr.putStrTab(r.ospf6.showMetrics());
                    return null;
                }
                if (a.equals("interface")) {
                    rdr.putStrTab(r.ospf6.showIfaces());
                    return null;
                }
                if (a.equals("flexalgo")) {
                    int i = bits.str2num(cmd.word());
                    rdr.putStrTab(r.ospf6.showAlgorithms(i));
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
                    int o = getGraphMask();
                    rdr.putStrArr(r.ospf6.showSpfGraph(i, o));
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
                if (r.rip6 == null) {
                    cmd.error("uninitialized process");
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
                cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
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
                doShowRouteCount(6);
                return null;
            }
            if (a.equals("compress")) {
                doShowRouteCompr(6);
                return null;
            }
            if (a.equals("changes")) {
                doShowRouteChngs(6);
                return null;
            }
            if (a.equals("route")) {
                doShowRouteUni(6);
                return null;
            }
            if (a.equals("ecmp")) {
                doShowRouteEcmp(6);
                return null;
            }
            if (a.equals("labels")) {
                doShowRouteLab(6);
                return null;
            }
            if (a.equals("just-network")) {
                doShowRouteNet(6);
                return null;
            }
            if (a.equals("just-interface")) {
                doShowRouteIfc(6);
                return null;
            }
            if (a.equals("just-nexthop")) {
                doShowRouteHop(6);
                return null;
            }
            if (a.equals("just-recursive")) {
                doShowRouteRec(6);
                return null;
            }
            if (a.equals("just-protocol")) {
                doShowRoutePrt(6);
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
                doShowRouteMul(6);
                return null;
            }
            if (a.equals("flwspc")) {
                doShowRouteFlw(6);
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
        if (r.eigrp == null) {
            cmd.error("uninitialized process");
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

    private void doShowIpXghosthunt(tabRouteAttr.routeType afi) {
        cfgRtr r = cfgAll.rtrFind(afi, bits.str2num(cmd.word()), false);
        if (r == null) {
            cmd.error("no such process");
            return;
        }
        if (r.ghosthunt == null) {
            cmd.error("uninitialized process");
            return;
        }
        String a = cmd.word();
        if (a.equals("status")) {
            rdr.putStrTab(r.ghosthunt.getStats());
            return;
        }
        if (a.equals("attrib")) {
            rdr.putStrArr(r.ghosthunt.getAttribed(cmd.pipe.settingsGet(pipeSetting.width, 80)));
            return;
        }
        if (a.equals("differ")) {
            rdr.putStrArr(r.ghosthunt.getDiffer(cmd, cmd.pipe.settingsGet(pipeSetting.width, 80)));
            return;
        }
        if (a.equals("ghost")) {
            rdr.putStrArr(r.ghosthunt.getGhosted());
            return;
        }
        if (a.equals("found")) {
            rdr.putStrArr(r.ghosthunt.getFound());
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
        if (r.logger == null) {
            cmd.error("uninitialized process");
            return;
        }
        String a = cmd.word();
        if (a.equals("flapstat")) {
            rdr.putStrTab(r.logger.getFlapstat(bits.str2num(cmd.word())));
            return;
        }
        if (a.equals("database")) {
            doShowRoutes(r.logger.fwdCore, r.logger.getRoutes(), r.logger.getDispMod());
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
        return encUrl.percentEncode(s);
    }

    private int getGraphMask() {
        int i = 0;
        for (;;) {
            String a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            if (a.equals("nocli")) {
                i |= 0x1;
                continue;
            }
            if (a.equals("nosvg")) {
                i |= 0x2;
                continue;
            }
            if (a.equals("nonets")) {
                i |= 0x4;
                continue;
            }
            if (a.equals("noints")) {
                i |= 0x8;
                continue;
            }
        }
        return i;
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
        if (r.lsrp == null) {
            cmd.error("uninitialized process");
            return;
        }
        String a = cmd.word();
        if (a.equals("neighbor")) {
            rdr.putStrTab(r.lsrp.showNeighs(cmd.word().equals("brief")));
            return;
        }
        if (a.equals("flexalgo")) {
            rdr.putStrTab(r.lsrp.showAlgorithms());
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
        if (a.equals("hardware")) {
            rdr.putStrTab(r.lsrp.showDatabase(6));
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
            int i = getGraphMask();
            rdr.putStrArr(r.lsrp.showSpfGraph(i));
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

    private void doShowIpXrift(tabRouteAttr.routeType afi) {
        cfgRtr r = cfgAll.rtrFind(afi, bits.str2num(cmd.word()), false);
        if (r == null) {
            cmd.error("no such process");
            return;
        }
        if (r.rift == null) {
            cmd.error("uninitialized process");
            return;
        }
        String a = cmd.word();
        if (a.equals("neighbor")) {
            rdr.putStrTab(r.rift.showNeighs(cmd.word().equals("brief")));
            return;
        }
        if (a.equals("database")) {
            if (cmd.size() < 1) {
                rdr.putStrTab(r.rift.showDatabase());
            } else {
                rdr.putStrArr(r.rift.showDatabase(cmd));
            }
            return;
        }
        if (a.equals("interface")) {
            rdr.putStrTab(r.rift.showIfaces());
            return;
        }
        if (a.equals("metric")) {
            rdr.putStrTab(r.rift.showMetrics());
            return;
        }
        if (a.equals("spf")) {
            a = cmd.word();
            rdr.putStrTab(r.rift.showSpfStat(a));
            rdr.putStrTab(r.rift.showSpfLog(a));
            return;
        }
        if (a.equals("topology")) {
            rdr.putStrTab(r.rift.showSpfTopo(cmd));
            return;
        }
        if (a.equals("hostnames")) {
            rdr.putStrTab(r.rift.showHostnames(cmd.word()));
            return;
        }
        if (a.equals("tree")) {
            rdr.putStrArr(r.rift.showSpfTree(cmd.word()));
            return;
        }
        if (a.equals("othertree")) {
            rdr.putStrArr(r.rift.showSpfOtherTree(cmd));
            return;
        }
        if (a.equals("othertopology")) {
            rdr.putStrTab(r.rift.showSpfOtherTopo(cmd));
            return;
        }
        if (a.equals("graph")) {
            String dir = cmd.word();
            int i = getGraphMask();
            rdr.putStrArr(r.rift.showSpfGraph(dir, i));
            return;
        }
        if (a.equals("nhinconsistent")) {
            a = cmd.word();
            tabIntMatcher mtch = new tabIntMatcher();
            mtch.fromString(cmd.word());
            rdr.putStrTab(r.rift.showNhIncons(a, mtch));
            return;
        }
        if (a.equals("lnkinconsistent")) {
            a = cmd.word();
            tabIntMatcher mtch = new tabIntMatcher();
            mtch.fromString(cmd.word());
            rdr.putStrTab(r.rift.showMetIncons(a, mtch));
            return;
        }
        if (a.equals("route")) {
            doShowRoutes(r.rift.fwdCore, r.rift.routerComputedU, 1);
            return;
        }
        if (a.equals("originate")) {
            doShowRoutes(r.rift.fwdCore, r.rift.routerRedistedU, 1);
            return;
        }
    }

    private void doShowIpXpvrp(tabRouteAttr.routeType afi) {
        cfgRtr r = cfgAll.rtrFind(afi, bits.str2num(cmd.word()), false);
        if (r == null) {
            cmd.error("no such process");
            return;
        }
        if (r.pvrp == null) {
            cmd.error("uninitialized process");
            return;
        }
        String a = cmd.word();
        if (a.equals("summary")) {
            rdr.putStrTab(r.pvrp.showNeighs(cmd.word().equals("brief")));
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
        if (r.isis == null) {
            cmd.error("uninitialized process");
            return;
        }
        String a = cmd.word();
        if (a.equals("neighbor")) {
            rdr.putStrTab(r.isis.showNeighs(cmd.word().equals("brief")));
            return;
        }
        if (a.equals("metric")) {
            rdr.putStrTab(r.isis.showMetrics());
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
        if (a.equals("flexalgo")) {
            int i = bits.str2num(cmd.word());
            rdr.putStrTab(r.isis.showAlgorithms(i));
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
            int o = getGraphMask();
            rdr.putStrArr(r.isis.showSpfGraph(i, o));
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
        cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
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
        cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
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
        if (r.msdp == null) {
            cmd.error("uninitialized process");
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
        if (r.babel == null) {
            cmd.error("uninitialized process");
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
        if (r.olsr == null) {
            cmd.error("uninitialized process");
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
                tabRouteAttr.ignoreAttribs(prf1.alts.get(i), ign);
            }
            for (int i = 0; i < prf2.alts.size(); i++) {
                tabRouteAttr.ignoreAttribs(prf2.alts.get(i), ign);
            }
            if (upd != null) {
                upd.update(safi, asn1, prf1, false);
                upd.update(safi, asn2, prf2, false);
            }
            if (prf1.differs(tabRoute.addType.alters, prf2) == 0) {
                continue;
            }
            diff.add(tabRoute.addType.alters, prf1, false, false);
            diff.add(tabRoute.addType.alters, prf2, false, false);
        }
    }

    private void doShowIpXbgp(tabRouteAttr.routeType afi) {
        cfgRtr r = cfgAll.rtrFind(afi, bits.str2num(cmd.word()), false);
        if (r == null) {
            cmd.error("no such process");
            return;
        }
        if (r.bgp == null) {
            cmd.error("uninitialized process");
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
        if (a.equals("desummary")) {
            rdr.putStrTab(r.bgp.showSummary(17));
            return;
        }
        if (a.equals("graceful-restart")) {
            rdr.putStrTab(r.bgp.showSummary(4));
            return;
        }
        if (a.equals("longlived-graceful")) {
            rdr.putStrTab(r.bgp.showSummary(15));
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
        if (a.equals("software")) {
            rdr.putStrTab(r.bgp.showSummary(16));
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
            rdr.putStrTab(r.bgp.getFlappath(sfi, tabRouteUtil.string2rd(cmd.word()), ntry, false));
            return;
        }
        if (a.equals("flaprevpath")) {
            addrPrefix<addrIP> ntry = addrPrefix.str2ip(cmd.word());
            if (ntry == null) {
                cmd.error("bad prefix");
                return;
            }
            rdr.putStrTab(r.bgp.getFlappath(sfi, tabRouteUtil.string2rd(cmd.word()), ntry, true));
            return;
        }
        if (a.equals("allroute")) {
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = addrPrefix.str2ip(cmd.word());
            if (ntry.prefix == null) {
                cmd.error("bad prefix");
                return;
            }
            ntry.rouDst = tabRouteUtil.string2rd(cmd.word());
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
            ntry.rouDst = tabRouteUtil.string2rd(cmd.word());
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
            List<String> dump1 = ntry1.fullDump("", r.bgp.fwdCore).formatAll(userFormat.tableMode.normal);
            List<String> dump2 = ntry2.fullDump("", r.bgp.fwdCore).formatAll(userFormat.tableMode.normal);
            int dif = ntry1.differs(tabRoute.addType.alters, ntry2);
            differ df = new differ();
            df.calc(dump1, dump2);
            List<String> res = df.getText(cmd.pipe.settingsGet(pipeSetting.width, 80), 0);
            res.add(0, "difference=" + dif);
            rdr.putStrArr(res);
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
                ign |= tabRouteAttr.string2ignore(a);
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
                ign |= tabRouteAttr.string2ignore(a);
            }
            tabRoute<addrIP> acc1 = nei1.getAccepted(sfi);
            tabRoute<addrIP> acc2 = nei2.getAccepted(sfi);
            if ((acc1 == null) || (acc2 == null)) {
                return;
            }
            tabRoute<addrIP> dif1 = new tabRoute<addrIP>("tab");
            compareTables(dif1, dif1, acc1, acc2, ign, flt, sfi, nei1.remoteAs, nei2.remoteAs, upd);
            compareTables(dif1, dif1, acc2, acc1, ign, flt, sfi, nei2.remoteAs, nei1.remoteAs, upd);
            tabRoute<addrIP> dif2 = new tabRoute<addrIP>("tab");
            if (dif1.size() > 0) {
                bits.sleep(tim);
                compareTables(dif2, dif2, acc1, acc2, ign, flt, sfi, nei1.remoteAs, nei2.remoteAs, upd);
                compareTables(dif2, dif2, acc2, acc1, ign, flt, sfi, nei2.remoteAs, nei1.remoteAs, upd);
            }
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
            ntry.rouDst = tabRouteUtil.string2rd(cmd.word());
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
            rtrBgpUtil.createReachable(pck, new packHolder(true, true), sfi, false, true, true, lst);
            rtrBgpUtil.createHeader(pck, rtrBgpUtil.msgUpdate);
            List<String> l = new ArrayList<String>();
            enc7bit.buf2hex(l, pck.getCopy(), 0);
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
            String s = cmd.word();
            ntry.rouDst = tabRouteUtil.string2rd(s);
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
            enc7bit.buf2hex(l, pck.getCopy(), 0);
            rdr.putStrArr(l);
            return;
        }
        if (a.equals("hacked")) {
            String str = cmd.word();
            String rd = cmd.word();
            doShowRoutesHacked(str, rd, r.bgp.fwdCore, tab, dsp);
            return;
        }
        if (a.equals("database")) {
            doShowRoutes(r.bgp.fwdCore, tab, dsp);
            return;
        }
        if (a.equals("compress")) {
            tab = new tabRoute<addrIP>(tab);
            tabRoute.compressTable(rtrBgpUtil.sfiUnicast, tab, null);
            doShowRoutes(r.bgp.fwdCore, tab, dsp);
            return;
        }
        if (a.equals("asnames")) {
            doShowRoutes(r.bgp.fwdCore, tab, 11);
            return;
        }
        if (a.equals("asinfos")) {
            doShowRoutes(r.bgp.fwdCore, tab, 12);
            return;
        }
        if (a.equals("changes")) {
            doShowRoutes(r.bgp.fwdCore, tab, dsp + 3000);
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
            cmd.clear();
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
            cmd.clear();
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
            cmd.clear();
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
            cmd.clear();
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
            cmd.clear();
            tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
            tabRtrmapN ntry = new tabRtrmapN();
            ntry.action = tabListingEntry.actionType.actPermit;
            ntry.stdCommMatch = tabRouteUtil.string2stdComms(a);
            roumap.add(ntry);
            tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
            tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, 0, res, tab, false, roumap, null, null);
            doShowRoutes(r.bgp.fwdCore, res, dsp);
            return;
        }
        if (a.equals("extcomm")) {
            a = cmd.getRemaining();
            cmd.clear();
            tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
            tabRtrmapN ntry = new tabRtrmapN();
            ntry.action = tabListingEntry.actionType.actPermit;
            ntry.extCommMatch = tabRouteUtil.string2extComms(a);
            roumap.add(ntry);
            tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
            tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, 0, res, tab, false, roumap, null, null);
            doShowRoutes(r.bgp.fwdCore, res, dsp);
            return;
        }
        if (a.equals("lrgcomm")) {
            a = cmd.getRemaining();
            cmd.clear();
            tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
            tabRtrmapN ntry = new tabRtrmapN();
            ntry.action = tabListingEntry.actionType.actPermit;
            ntry.lrgCommMatch = tabRouteUtil.string2lrgComms(a);
            roumap.add(ntry);
            tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
            tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, 0, res, tab, false, roumap, null, null);
            doShowRoutes(r.bgp.fwdCore, res, dsp);
            return;
        }
        if (a.equals("rd")) {
            a = cmd.getRemaining();
            cmd.clear();
            tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
            tabRtrmapN ntry = new tabRtrmapN();
            ntry.action = tabListingEntry.actionType.actPermit;
            ntry.rouDstMatch = tabRouteUtil.string2rd(a);
            roumap.add(ntry);
            tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
            tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, 0, res, tab, false, roumap, null, null);
            doShowRoutes(r.bgp.fwdCore, res, dsp);
            return;
        }
        if (a.equals("regexp")) {
            a = cmd.getRemaining();
            cmd.clear();
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
            cmd.clear();
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
        if (a.equals("unknowns")) {
            a = cmd.getRemaining();
            cmd.clear();
            tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
            tabRtrmapN ntry = new tabRtrmapN();
            ntry.action = tabListingEntry.actionType.actPermit;
            ntry.unknownMatch = new tabIntMatcher();
            ntry.unknownMatch.fromString(a);
            roumap.add(ntry);
            tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
            tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, 0, res, tab, false, roumap, null, null);
            doShowRoutes(r.bgp.fwdCore, res, dsp);
            return;
        }
        if (a.equals("asend")) {
            a = cmd.getRemaining();
            cmd.clear();
            tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
            tabRtrmapN ntry = new tabRtrmapN();
            ntry.action = tabListingEntry.actionType.actPermit;
            ntry.asendMatch = new tabIntMatcher();
            ntry.asendMatch.fromString(a);
            roumap.add(ntry);
            tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
            tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, 0, res, tab, false, roumap, null, null);
            doShowRoutes(r.bgp.fwdCore, res, dsp);
            return;
        }
        if (a.equals("asbeg")) {
            a = cmd.getRemaining();
            cmd.clear();
            tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
            tabRtrmapN ntry = new tabRtrmapN();
            ntry.action = tabListingEntry.actionType.actPermit;
            ntry.asbegMatch = new tabIntMatcher();
            ntry.asbegMatch.fromString(a);
            roumap.add(ntry);
            tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
            tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, 0, res, tab, false, roumap, null, null);
            doShowRoutes(r.bgp.fwdCore, res, dsp);
            return;
        }
        if (a.equals("asmid")) {
            a = cmd.getRemaining();
            cmd.clear();
            tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
            tabRtrmapN ntry = new tabRtrmapN();
            ntry.action = tabListingEntry.actionType.actPermit;
            ntry.asmidMatch = new tabIntMatcher();
            ntry.asmidMatch.fromString(a);
            roumap.add(ntry);
            tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
            tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, 0, res, tab, false, roumap, null, null);
            doShowRoutes(r.bgp.fwdCore, res, dsp);
            return;
        }
        if (a.equals("distance")) {
            a = cmd.getRemaining();
            cmd.clear();
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
            cmd.clear();
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
            cmd.clear();
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
            cmd.clear();
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
            cmd.clear();
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
            cmd.clear();
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
            cmd.clear();
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
            cmd.clear();
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
            cmd.clear();
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
            cmd.clear();
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
            case rtrBgpParam.mskOflw:
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
        rdr.putStrTab(fwd.pbrCfg.getStats(3));
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
            rdr.putStrTab(fwd.natCfg.getStats(3));
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
        rdr.putStrTab(ifc.getShow());
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

    private void doShowRouteCount(int ver) {
        ipFwd fwd = findVrf(ver);
        if (fwd == null) {
            return;
        }
        doShowRoutes(fwd, fwd.actualU, 6);
    }

    private void doShowRouteUni(int ver) {
        ipFwd fwd = findVrf(ver);
        if (fwd == null) {
            return;
        }
        doShowRoutes(fwd, fwd.actualU, 1);
    }

    private void doShowRouteCompr(int ver) {
        ipFwd fwd = findVrf(ver);
        if (fwd == null) {
            return;
        }
        tabRoute<addrIP> tab = new tabRoute<addrIP>(fwd.actualU);
        tabRoute.compressTable(rtrBgpUtil.sfiUnicast, tab, null);
        doShowRoutes(fwd, tab, 1);
    }

    private void doShowRouteChngs(int ver) {
        ipFwd fwd = findVrf(ver);
        if (fwd == null) {
            return;
        }
        doShowRoutes(fwd, fwd.actualU, 10);
    }

    private void doShowRouteLab(int ver) {
        ipFwd fwd = findVrf(ver);
        if (fwd == null) {
            return;
        }
        doShowRoutes(fwd, fwd.actualU, 3);
    }

    private void doShowRouteNet(int ver) {
        ipFwd fwd = findVrf(ver);
        if (fwd == null) {
            return;
        }
        String a = cmd.getRemaining();
        cmd.clear();
        tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
        tabRtrmapN ntry = new tabRtrmapN();
        ntry.action = tabListingEntry.actionType.actPermit;
        ntry.networkMatch = new tabPrfxlstN();
        ntry.networkMatch.fromString(a);
        roumap.add(ntry);
        tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
        tabRoute.addUpdatedTable(tabRoute.addType.better, rtrBgpUtil.sfiUnicast, 0, res, fwd.actualU, false, roumap, null, null);
        doShowRoutes(fwd, res, 1);
    }

    private void doShowRouteIfc(int ver) {
        ipFwd fwd = findVrf(ver);
        if (fwd == null) {
            return;
        }
        cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
        if (ifc == null) {
            cmd.error("no such interface");
            return;
        }
        tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
        tabRtrmapN ntry = new tabRtrmapN();
        ntry.action = tabListingEntry.actionType.actPermit;
        ntry.ifaceMatch = ifc;
        roumap.add(ntry);
        tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
        tabRoute.addUpdatedTable(tabRoute.addType.better, rtrBgpUtil.sfiUnicast, 0, res, fwd.actualU, false, roumap, null, null);
        doShowRoutes(fwd, res, 1);
    }

    private void doShowRouteHop(int ver) {
        ipFwd fwd = findVrf(ver);
        if (fwd == null) {
            return;
        }
        tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
        tabRtrmapN ntry = new tabRtrmapN();
        ntry.action = tabListingEntry.actionType.actPermit;
        ntry.nexthopMatch = new addrIP();
        ntry.nexthopMatch.fromString(cmd.word());
        roumap.add(ntry);
        tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
        tabRoute.addUpdatedTable(tabRoute.addType.better, rtrBgpUtil.sfiUnicast, 0, res, fwd.actualU, false, roumap, null, null);
        doShowRoutes(fwd, res, 1);
    }

    private void doShowRouteRec(int ver) {
        ipFwd fwd = findVrf(ver);
        if (fwd == null) {
            return;
        }
        tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
        tabRtrmapN ntry = new tabRtrmapN();
        ntry.action = tabListingEntry.actionType.actPermit;
        ntry.oldhopMatch = new addrIP();
        ntry.oldhopMatch.fromString(cmd.word());
        roumap.add(ntry);
        tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
        tabRoute.addUpdatedTable(tabRoute.addType.better, rtrBgpUtil.sfiUnicast, 0, res, fwd.actualU, false, roumap, null, null);
        doShowRoutes(fwd, res, 1);
    }

    private void doShowRoutePrt(int ver) {
        ipFwd fwd = findVrf(ver);
        if (fwd == null) {
            return;
        }
        tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
        tabRtrmapN ntry = new tabRtrmapN();
        ntry.action = tabListingEntry.actionType.actPermit;
        ntry.protoTypMatch = cfgRtr.name2num(cmd.word());
        ntry.protoNumMatch = bits.str2num(cmd.word());
        if (!cfgRtr.num2proc(ntry.protoTypMatch)) {
            ntry.protoNumMatch = -1;
        }
        roumap.add(ntry);
        tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
        tabRoute.addUpdatedTable(tabRoute.addType.better, rtrBgpUtil.sfiUnicast, 0, res, fwd.actualU, false, roumap, null, null);
        doShowRoutes(fwd, res, 1);
    }

    private void doShowRouteEcmp(int ver) {
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
        userFormat lst = new userFormat("|", "index|conn|prefix|peers|bytes");
        for (int i = 0; i < fwd.actualIU.size(); i++) {
            tabIndex<addrIP> prf = fwd.actualIU.get(i);
            if (prf == null) {
                continue;
            }
            String b = "";
            if (prf.neighs != null) {
                for (int o = 0; o < prf.neighs.size(); o++) {
                    b += " " + prf.neighs.get(o).index;
                }
            }
            String a = "";
            if (prf.hwCntr != null) {
                a = "+" + prf.hwCntr.byteRx;
            }
            lst.add(prf.index + "|" + prf.conned + "|" + addrPrefix.ip2str(prf.prefix) + "|" + b + "|" + prf.cntr.byteRx + a);
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

    private void doShowRouteMul(int ver) {
        ipFwd fwd = findVrf(ver);
        if (fwd == null) {
            return;
        }
        doShowRoutes(fwd, fwd.actualM, 1);
    }

    private void doShowRouteFlw(int ver) {
        ipFwd fwd = findVrf(ver);
        if (fwd == null) {
            return;
        }
        doShowRoutes(fwd, fwd.actualF, 5);
    }

    private void doShowRoutesHacked(String str, String rd, ipFwd fwd, tabRoute<addrIP> tab, int typ) {
        if (str.length() < 1) {
            int tabSiz = tab.size();
            if (tabSiz > 0xffff) {
                rdr.putStrArr(bits.str2lst(cmds.errbeg + "too big " + tabSiz));
                return;
            }
            userFormat lst = tabRoute.convertTableFull(tab, typ);
            if (lst == null) {
                rdr.putStrArr(bits.str2lst(cmds.errbeg + "bad table format"));
                return;
            }
            List<String> res = lst.formatAll(cmd.pipe.settingsGet(pipeSetting.tabMod, userFormat.tableMode.normal));
            res = enc7bit.toHackedLst(res);
            rdr.putStrArr(res);
            return;
        }
        rdr.putStrArr(bits.str2lst(cmds.errbeg + "looking up" + " pfx=" + str + " rd=" + rd + " on len=" + tab.size()));
        List<String> res = doShowRouteDetail("hckd-", str, rd, fwd, tab);
        if (res == null) {
            rdr.putStrArr(bits.str2lst(cmds.errbeg + "no such prefix"));
            return;
        }
        res = enc7bit.toHackedLst(res);
        rdr.putStrArr(res);
    }

    private tabRouteEntry<addrIP> doFindOneRoute(String str, String rd, tabRoute<addrIP> tab) {
        if ((tab == null) || (str == null) || (rd == null)) {
            cmd.error("getting table");
            return null;
        }
        if (str.length() < 1) {
            cmd.error("no such prefix");
            return null;
        }
        tabRouteEntry<addrIP> nw = new tabRouteEntry<addrIP>();
        nw.prefix = addrPrefix.str2ip(str);
        if (nw.prefix == null) {
            addrIP adr = new addrIP();
            if (adr.fromString(str)) {
                cmd.error("bad prefix format");
                return null;
            }
            nw = tab.route(adr);
            if (nw == null) {
                cmd.error("no such route");
                return null;
            }
            nw = nw.copyBytes(tabRoute.addType.alters);
        }
        nw.rouDst = tabRouteUtil.string2rd(rd);
        nw = nw.copyBytes(tabRoute.addType.alters);
        tabRouteEntry<addrIP> ntry = tab.find(nw);
        if (ntry == null) {
            cmd.error("no such route");
            return null;
        }
        return ntry.copyBytes(tabRoute.addType.alters);
    }

    private List<String> doShowRouteDetail(String beg, String str, String rd, ipFwd fwd, tabRoute<addrIP> tab) {
        if (tab == null) {
            return null;
        }
        tabRouteEntry<addrIP> ntry = doFindOneRoute(str, rd, tab);
        if (ntry == null) {
            return null;
        }
        userFormat lst = ntry.fullDump(beg, fwd);
        if (lst == null) {
            return null;
        }
        return lst.formatAll(cmd.pipe.settingsGet(pipeSetting.tabMod, userFormat.tableMode.normal));
    }

    private void doShowRoutes(ipFwd fwd, tabRoute<addrIP> tab, int typ) {
        String str = cmd.word();
        if (str.length() > 0) {
            String rd = cmd.word();
            rdr.putStrArr(doShowRouteDetail("", str, rd, fwd, tab));
            return;
        }
        userFormat lst = tabRoute.convertTableHead(typ);
        if (lst == null) {
            return;
        }
        if (tab.size() < 1) {
            rdr.putStrTab(lst);
            return;
        }
        final int lines = cmd.pipe.settingsGet(pipeSetting.riblines, 8192);
        for (int pos = 0; pos < tab.size(); pos += lines) {
            tabRoute<addrIP> sub = tab.getSubset(pos, pos + lines);
            lst = tabRoute.convertTableFull(sub, typ);
            if (rdr.putStrTab(lst)) {
                break;
            }
        }
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
            l.add(v.name + "|" + tabRouteUtil.rd2string(v.fwd4.rd) + "|" + v.fwd4.ifaces.size() + "|" + v.fwd6.ifaces.size() + "|" + v.fwd4.actualU.size() + "|" + v.fwd6.actualU.size() + "|" + v.fwd4.actualM.size() + "|" + v.fwd6.actualM.size() + "|" + v.fwd4.actualF.size() + "|" + v.fwd6.actualF.size() + "|" + v.fwd4.labeldR.size() + "|" + v.fwd6.labeldR.size() + "|" + v.fwd4.connedR.size() + "|" + v.fwd6.connedR.size());
        }
        rdr.putStrTab(l);
    }

    private void doShowVrfIcmp() {
        userFormat l = new userFormat("|", "name|rd|v4|v6|v4|v6|v4|v6|v4|v6|v4|v6|v4|v6", "2|2echSnt|2echGot|2echOk|2echPnd|2errSnt|2errGot");
        for (int o = 0; o < cfgAll.vrfs.size(); o++) {
            cfgVrf v = cfgAll.vrfs.get(o);
            l.add(v.name + "|" + tabRouteUtil.rd2string(v.fwd4.rd) + "|" + v.fwd4.echoSent + "|" + v.fwd6.echoSent + "|" + v.fwd4.echoRcvd + "|" + v.fwd6.echoRcvd + "|" + v.fwd4.echoRply + "|" + v.fwd6.echoRply + "|" + v.fwd4.echoes.size() + "|" + v.fwd6.echoes.size() + "|" + v.fwd4.errorSent + "|" + v.fwd6.errorSent + "|" + v.fwd4.errorRcvd + "|" + v.fwd6.errorRcvd);
        }
        rdr.putStrTab(l);
    }

    private String doShowVrfTraff(ipFwd f) {
        counter ch = f.cntrH.sumUp(false);
        counter ct = f.cntrT.sumUp(false);
        counter cl = f.cntrL.sumUp(false);
        return f.cfgName + ":" + f.ipVersion + "|" + tabRouteUtil.rd2string(f.rd) + "|" + ch.packRx + "|" + ch.byteRx + "|" + ct.packRx + "|" + ct.byteRx + "|" + cl.packRx + "|" + cl.byteRx;
    }

    private void doShowVrfTraff() {
        if (cmd.size() > 0) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                cmd.error("no such vrf");
                return;
            }
            String a = cmd.word();
            doShowHistory(a, vrf.fwd4.hstryH);
            doShowHistory(a, vrf.fwd6.hstryH);
            doShowHistory(a, vrf.fwd4.hstryT);
            doShowHistory(a, vrf.fwd6.hstryT);
            doShowHistory(a, vrf.fwd4.hstryL);
            doShowHistory(a, vrf.fwd6.hstryL);
            return;
        }
        userFormat l = new userFormat("|", "name|rd|pack|byte|pack|byte|pack|byte", "2|2hardware|2software|2local");
        for (int o = 0; o < cfgAll.vrfs.size(); o++) {
            cfgVrf v = cfgAll.vrfs.get(o);
            l.add(doShowVrfTraff(v.fwd4));
            l.add(doShowVrfTraff(v.fwd6));
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

    private static void doOneClient(userFormat lst, String nam, syncInt startS, syncInt errorS, syncInt stopS) {
        int startI = startS.get();
        int errorI = errorS.get();
        int stopI = stopS.get();
        lst.add(nam + "|" + startI + "|" + errorI + "|" + stopI + "|" + (startI - stopI - errorI));
    }

}
