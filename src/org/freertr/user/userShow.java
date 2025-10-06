package org.freertr.user;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIpx;
import org.freertr.addr.addrPrefix;
import org.freertr.auth.authGeneric;
import org.freertr.auth.authResult;
import org.freertr.cfg.cfgAceslst;
import org.freertr.cfg.cfgAlias;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgAuther;
import org.freertr.cfg.cfgBndl;
import org.freertr.cfg.cfgBrdg;
import org.freertr.cfg.cfgCheck;
import org.freertr.cfg.cfgDial;
import org.freertr.cfg.cfgGeneric;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgInit;
import org.freertr.cfg.cfgLin;
import org.freertr.cfg.cfgMtrack;
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
import org.freertr.cfg.cfgSessn;
import org.freertr.cfg.cfgTime;
import org.freertr.cfg.cfgTrack;
import org.freertr.cfg.cfgVdc;
import org.freertr.cfg.cfgVdcIfc;
import org.freertr.cfg.cfgVpdn;
import org.freertr.cfg.cfgVrf;
import org.freertr.clnt.clntCurl;
import org.freertr.clnt.clntDns;
import org.freertr.clnt.clntFtp;
import org.freertr.clnt.clntHttp;
import org.freertr.clnt.clntNetflow;
import org.freertr.clnt.clntNrpe;
import org.freertr.clnt.clntProxy;
import org.freertr.clnt.clntSmtp;
import org.freertr.clnt.clntTftp;
import org.freertr.clnt.clntWhois;
import org.freertr.ifc.ifcPolka;
import org.freertr.ifc.ifcThread;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipFwdMcast;
import org.freertr.ip.ipFwdMpmp;
import org.freertr.ip.ipFwdTab;
import org.freertr.ip.ipFwdTrfng;
import org.freertr.ip.ipRtr;
import org.freertr.pack.packDnsRec;
import org.freertr.pack.packDnsZone;
import org.freertr.pack.packHolder;
import org.freertr.pack.packLdpMp;
import org.freertr.pack.packLdpPwe;
import org.freertr.pipe.pipeSetting;
import org.freertr.prt.prtRedun;
import org.freertr.prt.prtWatch;
import org.freertr.rtr.rtrBabelNeigh;
import org.freertr.rtr.rtrBgpGroup;
import org.freertr.rtr.rtrBgpNeigh;
import org.freertr.rtr.rtrBgpParam;
import org.freertr.rtr.rtrBgpTemp;
import org.freertr.rtr.rtrBgpUtil;
import org.freertr.rtr.rtrEigrpNeigh;
import org.freertr.rtr.rtrLdpNeigh;
import org.freertr.rtr.rtrLogger;
import org.freertr.rtr.rtrOlsrNeigh;
import org.freertr.rtr.rtrPvrpNeigh;
import org.freertr.rtr.rtrRip4neigh;
import org.freertr.rtr.rtrRip6neigh;
import org.freertr.serv.servAmt;
import org.freertr.serv.servBmp2mrt;
import org.freertr.serv.servDhcp4;
import org.freertr.serv.servDhcp6;
import org.freertr.serv.servDns;
import org.freertr.serv.servEtherIp;
import org.freertr.serv.servGenList;
import org.freertr.serv.servGeneric;
import org.freertr.serv.servGre;
import org.freertr.serv.servGtp;
import org.freertr.serv.servHttp;
import org.freertr.serv.servL2f;
import org.freertr.serv.servL2tp2;
import org.freertr.serv.servL2tp3;
import org.freertr.serv.servMplsIp;
import org.freertr.serv.servMplsUdp;
import org.freertr.serv.servNetflow;
import org.freertr.serv.servP4lang;
import org.freertr.serv.servPckOudp;
import org.freertr.serv.servSdwan;
import org.freertr.serv.servSmtp;
import org.freertr.serv.servStreamingMdt;
import org.freertr.serv.servVxlan;
import org.freertr.enc.enc7bit;
import org.freertr.enc.encUrl;
import org.freertr.ip.ipCor4;
import org.freertr.ip.ipCor6;
import org.freertr.pack.packRedundancy;
import org.freertr.rtr.rtrBgp;
import org.freertr.rtr.rtrBgpDump;
import org.freertr.rtr.rtrBgpSpeak;
import org.freertr.rtr.rtrRpki;
import org.freertr.rtr.rtrRpkiNeigh;
import org.freertr.serv.servNrpe;
import org.freertr.serv.servOpenflow;
import org.freertr.serv.servRpki;
import org.freertr.serv.servRtpStat;
import org.freertr.serv.servStack;
import org.freertr.tab.tabAceslstN;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabIndex;
import org.freertr.tab.tabIntMatcher;
import org.freertr.tab.tabLabel;
import org.freertr.tab.tabLabelEntry;
import org.freertr.tab.tabListing;
import org.freertr.tab.tabListingEntry;
import org.freertr.tab.tabNshEntry;
import org.freertr.tab.tabPrfxlstN;
import org.freertr.tab.tabQos;
import org.freertr.tab.tabRpkiRoa;
import org.freertr.tab.tabRpkiUtil;
import org.freertr.tab.tabRoute;
import org.freertr.tab.tabRouteAttr;
import org.freertr.tab.tabRouteEntry;
import org.freertr.tab.tabRouteUtil;
import org.freertr.tab.tabRpkiAspa;
import org.freertr.tab.tabRpkiKey;
import org.freertr.tab.tabRtrmapN;
import org.freertr.tab.tabRtrplc;
import org.freertr.tab.tabSession;
import org.freertr.tab.tabSessionEntry;
import org.freertr.tab.tabTime;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.counter;
import org.freertr.util.differ;
import org.freertr.util.history;
import org.freertr.util.logBuf;
import org.freertr.util.logger;
import org.freertr.util.syncInt;

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
    public userRead rdr;

    /**
     * current help context
     */
    public userHelp hlp;

    /**
     * current config context
     */
    public cfgGeneric cfg;

    /**
     * current commit context
     */
    public List<String> cmt;

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
                rdr.putStrArr(cfgInit.getShLogo(0x2));
                return null;
            }
            if (a.equals("date-email")) {
                rdr.putStrArr(cfgInit.getShLogo(0x800));
                return null;
            }
            if (a.equals("date-machine")) {
                rdr.putStrArr(cfgInit.getShLogo(0x400));
                return null;
            }
            if (a.equals("user-agent")) {
                rdr.putStrArr(cfgInit.getShLogo(0x1000));
                return null;
            }
            if (a.equals("url")) {
                rdr.putStrArr(cfgInit.getShLogo(0x2000));
                return null;
            }
            if (a.equals("number")) {
                rdr.putStrArr(cfgInit.getShLogo(0x200));
                return null;
            }
            rdr.putStrArr(cfgInit.getShLogo(0x40e0));
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
                userScreen scr = new userScreen(cmd.pipe);
                List<String> lst = userFlash.asciiArt(cmd.getRemaining(), scr);
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
            if (a.equals("clock")) {
                userScreen scr = new userScreen(cmd.pipe);
                a = bits.time2str(cfgAll.timeZoneName, bits.getTime(), 2);
                userGame t = new userGame(scr, rdr);
                t.drawClock(a, userScreen.colBlack, userScreen.colWhite);
                rdr.putStrArr(scr.getAscii());
                return null;
            }
            if (a.equals("calendar")) {
                int i = bits.str2num(cmd.word());
                int o = bits.str2num(cmd.word());
                List<String> l = tabTime.getCalendar(i, o);
                rdr.putStrArr(l);
                return null;
            }
            List<String> lst = cfgInit.secretsFind(a);
            if (lst == null) {
                cmd.badCmd();
                return null;
            }
            rdr.putStrArr(lst);
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
            rdr.putStrArr(enc7bit.doOneArray(cfgAll.bannerEnc, ""));
            return null;
        }
        if (a.equals("logo")) {
            a = cmd.getRemaining();
            List<String> l;
            if (a.length() > 0) {
                l = userScreen.fontText(a, " ", userFonts.fontFiller, userFonts.font8x16());
            } else {
                l = cfgInit.getShLogo(0x0e);
            }
            rdr.putStrArr(l);
            return null;
        }
        if (a.equals("platform")) {
            rdr.putStrArr(cfgInit.getShPlat());
            return null;
        }
        if (a.equals("flash")) {
            a = getDiskPath();
            rdr.putStrTab(userFlash.dir2txt(userFlash.dirList(a)));
            return null;
        }
        if (a.equals("disk")) {
            a = getDiskPath();
            rdr.putStrTab(userFlash.diskInfo(a));
            return null;
        }
        if (a.equals("file")) {
            rdr.putStrArr(bits.txt2buf(cmd.getRemaining()));
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
            userFormat l = new userFormat("|", "mac|sess|iface");
            if (ifc.pppoeR != null) {
                ifc.pppoeR.getShow(l);
            }
            if (ifc.pppoeC != null) {
                ifc.pppoeC.getShow(l);
            }
            if (ifc.pppoeS != null) {
                ifc.pppoeS.getShow(l);
            }
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
            if (a.equals("calendar")) {
                a = bits.time2str(cfgAll.timeZoneName, tim, 1);
                int i = a.indexOf("-");
                l = tabTime.getCalendar(bits.str2num(a.substring(0, i)), bits.str2num(a.substring(i + 1, i + 3)));
                rdr.putStrArr(l);
                return null;
            }
            if (a.equals("analog")) {
                a = bits.time2str(cfgAll.timeZoneName, bits.getTime(), 2);
                a = a.substring(0, a.length() - 3);
                userScreen scr = new userScreen(cmd.pipe);
                userGame t = new userGame(scr, rdr);
                t.drawClock(a, userScreen.colBlack, userScreen.colWhite);
                rdr.putStrArr(scr.getAscii());
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
                userFormat l = new userFormat("|", "name|rerun|code|pid|chld|cpu|mem|ago|last");
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
                userFormat l = new userFormat("|", "name|rerun|code|pid|chld|cpu|mem|ago|last");
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
            if (a.equals("state-hash")) {
                rdr.putStrTab(prtRedun.doShowHash(packRedundancy.fnState));
                return null;
            }
            if (a.equals("state-save")) {
                rdr.putStrArr(bits.txt2buf(cfgInit.myStateFile()));
                return null;
            }
            if (a.equals("state-data")) {
                rdr.putStrArr(cfgInit.stateData());
                return null;
            }
            if (a.equals("remote")) {
                rdr.putStrArr(prtRedun.doShowCmd(cmd.getRemaining()));
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
            rdr.putStrArr(bits.str2lst("queried asn " + bits.num2str(i) + " is " + a + " in " + bits.timePast(tim)));
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
            clntWhois w = new clntWhois(cmd.pipe, cfgAll.getClntPrx(cfgAll.whoisProxy), cfgAll.whoisServer, cfgAll.whoisOption);
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
            if (a.equals("system")) {
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
            if (a.equals("bgpdump")) {
                List<String> txt = logger.bufferRead();
                List<packHolder> pcks = rtrBgpDump.logs2pcks(txt);
                int o = pcks.size();
                cmd.error(o + " dumps found");
                cfgVrf vrf = new cfgVrf("bgp");
                vrf.allocThisVrf();
                rtrBgp bgp = new rtrBgp(vrf.fwd4, vrf, null, 0);
                rtrBgpNeigh nei = new rtrBgpNeigh(bgp, new addrIP());
                rtrBgpSpeak spk = new rtrBgpSpeak(bgp, nei, null, 0);
                packHolder tmp = new packHolder(true, true);
                tabGen<tabSessionEntry> ses = new tabGen<tabSessionEntry>();
                for (int i = 0; i < o; i++) {
                    packHolder pck = pcks.get(i);
                    txt = rtrBgpDump.dumpPacketFull(spk, vrf.core4, vrf.core6, ses, tmp, pck);
                    txt.add("");
                    rdr.putStrArr(txt);
                }
                return null;
            }
            if (a.equals("oldfile")) {
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
        if (a.equals("uncommitted-config")) {
            rdr.putStrArr(cmt);
            return null;
        }
        if (a.equals("rollback-config")) {
            List<String> lst = bits.txt2buf(cfgInit.cfgFileSw);
            a = cmd.getRemaining().trim();
            if (a.equals("this")) {
                if (cfg == null) {
                    return null;
                }
                List<String> cur = cfg.getShRun(getConfigFilter(null, cmd));
                lst = userFilter.getSection(lst, userRead.section2filter(cur));
                rdr.putStrArr(userFilter.getDiffs(cur, lst));
                return null;
            }
            rdr.putStrArr(userFilter.getDiffs(cfgAll.getShRun(1), lst));
            return null;
        }
        if (a.equals("config-differences")) {
            List<String> lst = bits.txt2buf(cfgInit.cfgFileSw);
            a = cmd.getRemaining().trim();
            if (a.equals("this")) {
                if (cfg == null) {
                    return null;
                }
                List<String> cur = cfg.getShRun(getConfigFilter(null, cmd));
                lst = userFilter.getSection(lst, userRead.section2filter(cur));
                rdr.putStrArr(userFilter.getDiffs(lst, cur));
                return null;
            }
            rdr.putStrArr(userFilter.getDiffs(lst, cfgAll.getShRun(1)));
            return null;
        }
        if (a.equals("backup-config")) {
            doStatCfg(cfgInit.getBackupCfgName());
            return null;
        }
        if (a.equals("startup-config")) {
            doStatCfg(cfgInit.cfgFileSw);
            return null;
        }
        if (a.equals("running-config")) {
            a = cmd.word();
            if (a.equals("this")) {
                if (cfg == null) {
                    return null;
                }
                List<String> cur = cfg.getShRun(getConfigFilter(null, cmd));
                rdr.putStrArr(cur);
                return null;
            }
            if (cfgAll.evalVdcPrivs()) {
                cmd.error("not in a vdc");
                return null;
            }
            if (a.equals("console0")) {
                rdr.putStrArr(cfgAll.con0.getShRun(getConfigFilter(null, cmd)));
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
            if (a.equals("tracker")) {
                cfgTrack v = cfgAll.trackFind(cmd.word(), false);
                if (v == null) {
                    cmd.error("no such tracker");
                    return null;
                }
                int filt = getConfigFilter(null, cmd);
                rdr.putStrArr(v.getShRun(filt));
                return null;
            }
            if (a.equals("check")) {
                cfgCheck v = cfgAll.checkFind(cmd.word(), false);
                if (v == null) {
                    cmd.error("no such check");
                    return null;
                }
                int filt = getConfigFilter(null, cmd);
                rdr.putStrArr(v.getShRun(filt));
                return null;
            }
            if (a.equals("sensor")) {
                cfgSensor v = cfgAll.sensorFind(cmd.word(), false);
                if (v == null) {
                    cmd.error("no such sensor");
                    return null;
                }
                int filt = getConfigFilter(null, cmd);
                rdr.putStrArr(v.getShRun(filt));
                return null;
            }
            if (a.equals("vdc")) {
                cfgVdc v = cfgAll.vdcFind(cmd.word(), false);
                if (v == null) {
                    cmd.error("no such vdc");
                    return null;
                }
                int filt = getConfigFilter(null, cmd);
                rdr.putStrArr(v.getShRun(filt));
                return null;
            }
            if (a.equals("process")) {
                cfgPrcss v = cfgAll.prcFind(cmd.word(), false);
                if (v == null) {
                    cmd.error("no such process");
                    return null;
                }
                int filt = getConfigFilter(null, cmd);
                rdr.putStrArr(v.getShRun(filt));
                return null;
            }
            if (a.equals("scheduler")) {
                cfgSched v = cfgAll.schedFind(cmd.word(), false);
                if (v == null) {
                    cmd.error("no such scheduler");
                    return null;
                }
                int filt = getConfigFilter(null, cmd);
                rdr.putStrArr(v.getShRun(filt));
                return null;
            }
            if (a.equals("script")) {
                cfgScrpt v = cfgAll.scrptFind(cmd.word(), false);
                if (v == null) {
                    cmd.error("no such script");
                    return null;
                }
                int filt = getConfigFilter(null, cmd);
                rdr.putStrArr(v.getShRun(filt));
                return null;
            }
            if (a.equals("vpdn")) {
                cfgVpdn v = cfgAll.vpdnFind(cmd.word(), false);
                if (v == null) {
                    cmd.error("no such vpdn");
                    return null;
                }
                int filt = getConfigFilter(null, cmd);
                rdr.putStrArr(v.getShRun(filt));
                return null;
            }
            if (a.equals("line")) {
                cfgLin v = cfgAll.linFind(cmd.word());
                if (v == null) {
                    cmd.error("no such line");
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
                lst = userFilter.getSection(lst, userRead.filter2reg(a + " " + cmd.getRemaining()));
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
        if (a.equals("rpki")) {
            servRpki srv = cfgAll.srvrFind(new servRpki(), cfgAll.dmnRpki, cmd.word());
            if (srv == null) {
                cmd.error("no such server");
                return null;
            }
            rdr.putStrTab(srv.getShow());
            return null;
        }
        if (a.equals("nrpe")) {
            servNrpe srv = cfgAll.srvrFind(new servNrpe(), cfgAll.dmnNrpe, cmd.word());
            if (srv == null) {
                cmd.error("no such server");
                return null;
            }
            rdr.putStrTab(srv.getShow());
            return null;
        }
        if (a.equals("dhcp4")) {
            String profileName = cmd.word();
            servDhcp4 srv = cfgAll.srvrFind(new servDhcp4(), cfgAll.dmnDhcp4, profileName);
            if (srv == null) {
                cmd.error("no such server");
                return null;
            }
            rdr.putStrTab(srv.getShow());
            return null;
        }
        if (a.equals("dhcp6")) {
            String profileName = cmd.word();
            servDhcp6 srv = cfgAll.srvrFind(new servDhcp6(), cfgAll.dmnDhcp6, profileName);
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
            a = cmd.word();
            if (a.equals("status")) {
                rdr.putStrTab(exp.getShowDetail());
                return null;
            }
            if (a.equals("output")) {
                rdr.putStrArr(exp.getResult());
                return null;
            }
            if (a.equals("yang")) {
                rdr.putStrArr(exp.getYang());
                return null;
            }
            if (a.equals("prometheus")) {
                rdr.putStrArr(exp.getReportProm());
                return null;
            }
            if (a.equals("csv")) {
                rdr.putStrArr(exp.getReportCsv());
                return null;
            }
            if (a.equals("netconf")) {
                rdr.putStrArr(exp.getShowNetconf());
                return null;
            }
            if (a.equals("xml")) {
                rdr.putStrArr(exp.getShowXml());
                return null;
            }
            if (a.equals("kvgpb")) {
                rdr.putStrArr(exp.getShowKvgpb());
                return null;
            }
            if (a.equals("memory")) {
                rdr.putStrArr(exp.getShowMemory());
                return null;
            }
            if (a.equals("history")) {
                rdr.putStrArr(exp.getShowHistory());
                return null;
            }
            if (a.equals("graph")) {
                a = cmd.word();
                userScreen scr = new userScreen(cmd.pipe);
                exp.getShowGraph(bits.str2num(a), scr);
                rdr.putStrArr(scr.getAscii());
                return null;
            }
            if (a.equals("oldgraph")) {
                a = cmd.word();
                userScreen scr = new userScreen(cmd.pipe);
                exp.getShowOldGraph(bits.str2num(a), scr);
                rdr.putStrArr(scr.getAscii());
                return null;
            }
            cmd.badCmd();
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
            doOneClient(l, "nrpe", clntNrpe.cntrStart, clntNrpe.cntrError, clntNrpe.cntrStop);
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
                rdr.putStrTab(cfgAll.getShCheck());
                return null;
            }
            cfgCheck srv = cfgAll.checkFind(a, false);
            if (srv == null) {
                cmd.error("no such check");
                return null;
            }
            a = cmd.word();
            if (a.equals("status")) {
                rdr.putStrTab(srv.getShowDetail());
                return null;
            }
            if (a.equals("output")) {
                rdr.putStrArr(srv.getResult());
                return null;
            }
            if (a.equals("result")) {
                rdr.putStrArr(srv.doCheckText());
                return null;
            }
            if (a.equals("error")) {
                rdr.putStrArr(srv.getShowError());
                return null;
            }
            cmd.badCmd();
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
            if (a.equals("status")) {
                rdr.putStrTab(srv.getShowGen());
                return null;
            }
            if (a.equals("api-tx")) {
                rdr.putStrTab(srv.getShowApiTx());
                return null;
            }
            if (a.equals("api-rx")) {
                rdr.putStrTab(srv.getShowApiRx());
                return null;
            }
            if (a.equals("port-names")) {
                rdr.putStrTab(srv.getShowFront());
                return null;
            }
            if (a.equals("port-magics")) {
                rdr.putStrTab(srv.getShowMagics());
                return null;
            }
            if (a.equals("done-vrf")) {
                rdr.putStrTab(srv.getShowVrfs());
                return null;
            }
            if (a.equals("done-interface")) {
                rdr.putStrTab(srv.getShowIfaces());
                return null;
            }
            if (a.equals("done-neighbor")) {
                rdr.putStrTab(srv.getShowNeighs());
                return null;
            }
            if (a.equals("done-mpls")) {
                rdr.putStrTab(srv.getShowMpls());
                return null;
            }
            if (a.equals("done-nsh")) {
                rdr.putStrTab(srv.getShowNsh());
                return null;
            }
            if (a.equals("done-bridge")) {
                int i = bits.str2num(cmd.word());
                rdr.putStrTab(srv.getShowBri(i));
                return null;
            }
            if (a.equals("done-route4")) {
                int i = bits.str2num(cmd.word());
                doShowRoutes(null, srv.getShowRou(4, i), 1);
                return null;
            }
            if (a.equals("done-route6")) {
                int i = bits.str2num(cmd.word());
                doShowRoutes(null, srv.getShowRou(6, i), 1);
                return null;
            }
            if (!a.equals("port-counters")) {
                return null;
            }
            a = cmd.word();
            cfgIfc ifc = cfgAll.ifcFind(a, 0);
            if (ifc == null) {
                cmd.error("no such interface");
                return null;
            }
            rdr.putStrTab(srv.getShowIface(ifc));
            return null;
        }
        if (a.equals("stack")) {
            a = cmd.word();
            servStack gen = cfgAll.dmnStack.get(0);
            if (gen == null) {
                return null;
            }
            if (a.length() < 1) {
                a = gen.getShGenOneLiner();
                rdr.putStrArr(bits.str2lst(a));
                return null;
            }
            servStack srv = cfgAll.srvrFind(new servStack(), cfgAll.dmnStack, a);
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
            if (a.equals("dataplanes")) {
                rdr.putStrTab(srv.getShowBcks());
                return null;
            }
            if (a.equals("ports")) {
                rdr.putStrTab(srv.getShowPorts(bits.str2num(cmd.word())));
                return null;
            }
            if (a.equals("spf")) {
                rdr.putStrTab(srv.getShowSpf(bits.str2num(cmd.word())));
                return null;
            }
            if (a.equals("topology")) {
                rdr.putStrTab(srv.getShowTopo(bits.str2num(cmd.word()), cmd));
                return null;
            }
            if (a.equals("tree")) {
                rdr.putStrArr(srv.getShowTree(bits.str2num(cmd.word()), cmd));
                return null;
            }
            if (a.equals("graph")) {
                rdr.putStrArr(srv.getShowGraph(bits.str2num(cmd.word()), cmd));
                return null;
            }
            if (a.equals("route")) {
                doShowRoutes(null, srv.getShowRoute(bits.str2num(cmd.word())), 1);
                return null;
            }
            return null;
        }
        if (a.equals("rtpstat")) {
            servRtpStat srv = cfgAll.srvrFind(new servRtpStat(), cfgAll.dmnRtpStat, cmd.word());
            if (srv == null) {
                cmd.error("no such server");
                return null;
            }
            rdr.putStrTab(srv.getShow());
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
            if (a.equals("interface")) {
                rdr.putStrTab(cfgAll.getShIntTab(32));
                return null;
            }
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
        if (a.equals("radiotap")) {
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
            if (ifc == null) {
                cmd.error("no such interface");
                return null;
            }
            if (ifc.radioTap == null) {
                cmd.error("not running on that interface");
                return null;
            }
            userFormat res = new userFormat("|", "mac|rate|chan|sign|for|since");
            res.add(ifc.radioTap.getShNeigh());
            rdr.putStrTab(res);
            return null;
        }
        if (a.equals("udld")) {
            a = cmd.word();
            if (a.equals("interface")) {
                rdr.putStrTab(cfgAll.getShIntTab(33));
                return null;
            }
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
            if (a.equals("interface")) {
                rdr.putStrTab(cfgAll.getShIntTab(34));
                return null;
            }
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
            if (a.equals("interface")) {
                rdr.putStrTab(cfgAll.getShIntTab(35));
                return null;
            }
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
        if (a.equals("acl-merge")) {
            cfgAceslst cfg1 = cfgAll.aclsFind(cmd.word(), false);
            cfgAceslst cfg2 = cfgAll.aclsFind(cmd.word(), false);
            tabListing<tabAceslstN<addrIP>, addrIP> acl1 = null;
            tabListing<tabAceslstN<addrIP>, addrIP> acl2 = null;
            if (cfg1 != null) {
                acl1 = tabAceslstN.unrollAcl(cfg1.aceslst);
            }
            if (cfg2 != null) {
                acl2 = tabAceslstN.unrollAcl(cfg2.aceslst);
            }
            tabListing<tabAceslstN<addrIP>, addrIP> res = new tabListing<tabAceslstN<addrIP>, addrIP>();
            res.mergeTwo(acl1, acl2);
            List<String> lst = res.dump("", 0);
            rdr.putStrArr(lst);
            return null;
        }
        if (a.equals("acl-packet")) {
            cfgAceslst acl = cfgAll.aclsFind(cmd.word(), false);
            if (acl == null) {
                cmd.error("no such access list");
                return null;
            }
            packHolder pck = new packHolder(true, true);
            pck.IPprt = bits.str2num(cmd.word());
            pck.IPsrc.fromString(cmd.word());
            pck.UDPsrc = bits.str2num(cmd.word());
            pck.IPtrg.fromString(cmd.word());
            pck.UDPtrg = bits.str2num(cmd.word());
            cmd.error("permitted=" + acl.aceslst.matches(false, false, pck));
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
        if (a.equals("rm2rpl")) {
            cfgRoump rtmp = cfgAll.rtmpFind(cmd.word(), false);
            if (rtmp == null) {
                cmd.error("no such route map");
                return null;
            }
            List<String> lst = rtmp.getShRun(1);
            rdr.putStrArr(tabRtrplc.convertRm2rpl(lst));
            return null;
        }
        if (a.equals("rpl2rm")) {
            cfgRouplc rtpl = cfgAll.rtplFind(cmd.word(), false);
            if (rtpl == null) {
                cmd.error("no such route policy");
                return null;
            }
            List<String> lst = rtpl.getShRun(1);
            rdr.putStrArr(tabRtrplc.convertRpl2rm(lst));
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
            a = cmd.word();
            if (a.equals("status")) {
                rdr.putStrTab(trck.worker.getShStat());
                return null;
            }
            if (a.equals("peer")) {
                rdr.putStrTab(trck.worker.getShPeer());
                return null;
            }
            if (a.equals("reach")) {
                rdr.putStrTab(trck.worker.getShMatrixReach());
                return null;
            }
            if (a.equals("time")) {
                rdr.putStrTab(trck.worker.getShMatrixTime());
                return null;
            }
            if (a.equals("loss")) {
                rdr.putStrTab(trck.worker.getShMatrixLoss());
                return null;
            }
            if (a.equals("list")) {
                rdr.putStrTab(trck.worker.getShList());
                return null;
            }
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
                rdr.putStrTab(ifc.ethtyp.getShTimes());
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
            if (a.equals("timetolives")) {
                rdr.putStrTab(ifc.ethtyp.getShTimes());
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
            if (a.equals("routeid-multicast")) {
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
            if (a.equals("routeid-unicast")) {
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
                    userFormat lst = new userFormat("|", "interface|packet|uni-head|mul-head");
                    for (int i = 0; i < cfgAll.ifaces.size(); i++) {
                        cfgIfc ntry = cfgAll.ifaces.get(i);
                        lst.add(ntry.name + "|" + (ntry.polkaPack != null) + "|" + (ntry.tunPolka != null) + "|" + (ntry.tunMpolka != null));
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
            cfgRtr rcfg = cfgAll.rtrFind(typ, bits.str2num(cmd.word()), false);
            if (rcfg == null) {
                cmd.error("no such process");
                return null;
            }
            ipRtr rtr = rcfg.getRouter();
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
                if (a.equals("statistics")) {
                    cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
                    if (ifc == null) {
                        cmd.error("no such interface");
                        return null;
                    }
                    rdr.putStrTab(r.ospf4.showStats(ifc.fwdIf4));
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
                    rdr.putStrArr(r.ospf4.showSpfTree(bits.str2num(cmd.word()), cmd));
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
                    rdr.putStrArr(r.ospf4.showSpfGraph(i, cmd));
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
                if (a.equals("nonredundant")) {
                    int i = bits.str2num(cmd.word());
                    rdr.putStrTab(r.ospf4.showNonRedundant(i));
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
            if (a.equals("dlep")) {
                doShowIpXdlep(4);
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
            if (a.equals("rpki")) {
                doShowIpXrpki(tabRouteAttr.routeType.rpki4);
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
            if (a.equals("overlap")) {
                doShowRouteOvrlp(4);
                return null;
            }
            if (a.equals("unused")) {
                doShowRouteUnusd(4);
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
            if (a.equals("hostwatch")) {
                doShowHostWatch(4);
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
            if (a.equals("nexthops")) {
                doShowNexthops(4);
                return null;
            }
            if (a.equals("out-interfaces")) {
                doShowOutIfaces(4);
                return null;
            }
            if (a.equals("prefix-lengths")) {
                doShowPrefLens(4);
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
                if (a.equals("statistics")) {
                    cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
                    if (ifc == null) {
                        cmd.error("no such interface");
                        return null;
                    }
                    rdr.putStrTab(r.ospf6.showStats(ifc.fwdIf6));
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
                    rdr.putStrArr(r.ospf6.showSpfTree(bits.str2num(cmd.word()), cmd));
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
                    rdr.putStrArr(r.ospf6.showSpfGraph(i, cmd));
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
                if (a.equals("nonredundant")) {
                    int i = bits.str2num(cmd.word());
                    rdr.putStrTab(r.ospf6.showNonRedundant(i));
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
            if (a.equals("dlep")) {
                doShowIpXdlep(6);
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
            if (a.equals("rpki")) {
                doShowIpXrpki(tabRouteAttr.routeType.rpki6);
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
            if (a.equals("overlap")) {
                doShowRouteOvrlp(6);
                return null;
            }
            if (a.equals("unused")) {
                doShowRouteUnusd(6);
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
            if (a.equals("hostwatch")) {
                doShowHostWatch(6);
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
            if (a.equals("nexthops")) {
                doShowNexthops(6);
                return null;
            }
            if (a.equals("out-interfaces")) {
                doShowOutIfaces(6);
                return null;
            }
            if (a.equals("prefix-lengths")) {
                doShowPrefLens(6);
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
        if (a.equals("statistics")) {
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
            if (ifc == null) {
                cmd.error("no such interface");
                return;
            }
            if (afi == tabRouteAttr.routeType.lsrp4) {
                rdr.putStrTab(r.lsrp.showStats(ifc.fwdIf4));
            } else {
                rdr.putStrTab(r.lsrp.showStats(ifc.fwdIf6));
            }
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
        if (a.equals("forwarder")) {
            rdr.putStrTab(r.lsrp.showDatabase(7));
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
            rdr.putStrArr(r.lsrp.showSpfTree(cmd));
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
            rdr.putStrArr(r.lsrp.showSpfGraph(cmd));
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
        if (a.equals("nonredundant")) {
            rdr.putStrTab(r.lsrp.showNonRedundant());
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
            rdr.putStrArr(r.rift.showSpfTree(cmd.word(), cmd));
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
            a = cmd.word();
            rdr.putStrArr(r.rift.showSpfGraph(a, cmd));
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
        if (a.equals("nonredundant")) {
            rdr.putStrTab(r.rift.showNonRedundant(cmd.word()));
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
        if (a.equals("statistics")) {
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
            if (ifc == null) {
                cmd.error("no such interface");
                return;
            }
            if (afi == tabRouteAttr.routeType.pvrp4) {
                rdr.putStrTab(r.pvrp.showStats(ifc.fwdIf4));
            } else {
                rdr.putStrTab(r.pvrp.showStats(ifc.fwdIf6));
            }
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
        if (a.equals("statistics")) {
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
            if (ifc == null) {
                cmd.error("no such interface");
                return;
            }
            if (afi == tabRouteAttr.routeType.isis4) {
                rdr.putStrTab(r.isis.showStats(ifc.fwdIf4));
            } else {
                rdr.putStrTab(r.isis.showStats(ifc.fwdIf6));
            }
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
            rdr.putStrArr(r.isis.showSpfTree(bits.str2num(cmd.word()), cmd));
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
            rdr.putStrArr(r.isis.showSpfGraph(i, cmd));
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
        if (a.equals("nonredundant")) {
            int i = bits.str2num(cmd.word());
            rdr.putStrTab(r.isis.showNonRedundant(i));
            return;
        }
        if (a.equals("afinconsistent")) {
            int i = bits.str2num(cmd.word());
            rdr.putStrTab(r.isis.showAfiIncons(i));
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

    private void doShowIpXdlep(int ver) {
        cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
        if (ifc == null) {
            cmd.error("no such interface");
            return;
        }
        ipFwdIface fic;
        if (ver == 4) {
            fic = ifc.fwdIf4;
        } else {
            fic = ifc.fwdIf6;
        }
        if (fic == null) {
            cmd.error("not configured");
            return;
        }
        if (fic.dlepCfg == null) {
            cmd.error("not enabled");
            return;
        }
        String a = cmd.word();
        if (a.equals("neighbor")) {
            rdr.putStrTab(fic.dlepCfg.getShNeigh());
            return;
        }
        if (a.equals("clients")) {
            rdr.putStrTab(fic.dlepCfg.getShClnts());
            return;
        }
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

    private void doShowIpXrpkiComp(rtrRpki rtr, int ver) {
        addrIP adr = new addrIP();
        if (adr.fromString(cmd.word())) {
            cmd.error("bad address");
            return;
        }
        rtrRpkiNeigh nei1 = rtr.findPeer(adr);
        if (nei1 == null) {
            cmd.error("no such neighbor");
            return;
        }
        adr = new addrIP();
        if (adr.fromString(cmd.word())) {
            cmd.error("bad address");
            return;
        }
        rtrRpkiNeigh nei2 = rtr.findPeer(adr);
        if (nei2 == null) {
            cmd.error("no such neighbor");
            return;
        }
        switch (ver) {
            case -1: {
                tabGen<tabRpkiAspa> tab1 = nei1.getFinalTabAspa();
                tabGen<tabRpkiAspa> tab2 = nei2.getFinalTabAspa();
                tabGen<tabRpkiAspa> uni1 = new tabGen<tabRpkiAspa>();
                tabGen<tabRpkiAspa> uni2 = new tabGen<tabRpkiAspa>();
                tabGen<tabRpkiAspa> dif1 = new tabGen<tabRpkiAspa>();
                tabGen<tabRpkiAspa> dif2 = new tabGen<tabRpkiAspa>();
                tabRpkiUtil.diffTwoAspa(uni1, dif1, tab1, tab2);
                tabRpkiUtil.diffTwoAspa(uni2, dif2, tab2, tab1);
                cmd.error("unique to " + nei1);
                doShowAspas(uni1, 1);
                cmd.error("unique to " + nei2);
                doShowAspas(uni2, 1);
                cmd.error("attribute differs");
                doShowAspas(dif1, 1);
                doShowAspas(dif2, 1);
                return;
            }
            case -2: {
                tabGen<tabRpkiKey> tab1 = nei1.getFinalTabKey();
                tabGen<tabRpkiKey> tab2 = nei2.getFinalTabKey();
                tabGen<tabRpkiKey> uni1 = new tabGen<tabRpkiKey>();
                tabGen<tabRpkiKey> uni2 = new tabGen<tabRpkiKey>();
                tabGen<tabRpkiKey> dif1 = new tabGen<tabRpkiKey>();
                tabGen<tabRpkiKey> dif2 = new tabGen<tabRpkiKey>();
                tabRpkiUtil.diffTwoKey(uni1, dif1, tab1, tab2);
                tabRpkiUtil.diffTwoKey(uni2, dif2, tab2, tab1);
                cmd.error("unique to " + nei1);
                doShowKeys(uni1, 1);
                cmd.error("unique to " + nei2);
                doShowKeys(uni2, 1);
                cmd.error("attribute differs");
                doShowKeys(dif1, 1);
                doShowKeys(dif2, 1);
                return;
            }
        }
        tabGen<tabRpkiRoa> tab1 = nei1.getFinalTabRoa(ver);
        tabGen<tabRpkiRoa> tab2 = nei2.getFinalTabRoa(ver);
        tabGen<tabRpkiRoa> uni1 = new tabGen<tabRpkiRoa>();
        tabGen<tabRpkiRoa> uni2 = new tabGen<tabRpkiRoa>();
        tabGen<tabRpkiRoa> dif1 = new tabGen<tabRpkiRoa>();
        tabGen<tabRpkiRoa> dif2 = new tabGen<tabRpkiRoa>();
        tabRpkiUtil.diffTwoRoa(uni1, dif1, tab1, tab2);
        tabRpkiUtil.diffTwoRoa(uni2, dif2, tab2, tab1);
        cmd.error("unique to " + nei1);
        doShowRoas(uni1, 1);
        cmd.error("unique to " + nei2);
        doShowRoas(uni2, 1);
        cmd.error("attribute differs");
        doShowRoas(dif1, 1);
        doShowRoas(dif2, 1);
    }

    private void doShowIpXrpki(tabRouteAttr.routeType afi) {
        cfgRtr r = cfgAll.rtrFind(afi, bits.str2num(cmd.word()), false);
        if (r == null) {
            cmd.error("no such process");
            return;
        }
        if (r.rpki == null) {
            cmd.error("uninitialized process");
            return;
        }
        String a = cmd.word();
        if (a.equals("summary")) {
            rdr.putStrTab(r.rpki.getNeighShow());
            return;
        }
        if (a.equals("status")) {
            rdr.putStrTab(r.rpki.getGenShow());
            return;
        }
        if (a.equals("connection")) {
            rdr.putStrTab(r.rpki.showConnSumm());
            return;
        }
        if (a.equals("sessions")) {
            rdr.putStrTab(r.rpki.getNeighShow());
            return;
        }
        if (a.equals("database4")) {
            doShowRoas(r.rpki.getFinalTabRoa(4), 1);
            return;
        }
        if (a.equals("database6")) {
            doShowRoas(r.rpki.getFinalTabRoa(6), 1);
            return;
        }
        if (a.equals("databasep")) {
            doShowAspas(r.rpki.getFinalTabAspa(), 1);
            return;
        }
        if (a.equals("databasek")) {
            doShowKeys(r.rpki.getFinalTabKey(), 1);
            return;
        }
        if (a.equals("aspagraph")) {
            tabGen<tabRpkiAspa> tab = r.rpki.getFinalTabAspa();
            rdr.putStrArr(tabRpkiUtil.getAspaGraph(tab));
            return;
        }
        if (a.equals("prefixes4")) {
            tabGen<tabRpkiRoa> tab = r.rpki.getFinalTabRoa(4);
            tab = tabRpkiUtil.allowedRoa(tab, bits.str2num(cmd.word()));
            doShowRoas(tab, 1);
            return;
        }
        if (a.equals("prefixes6")) {
            tabGen<tabRpkiRoa> tab = r.rpki.getFinalTabRoa(6);
            tab = tabRpkiUtil.allowedRoa(tab, bits.str2num(cmd.word()));
            doShowRoas(tab, 1);
            return;
        }
        if (a.equals("provider")) {
            tabGen<tabRpkiAspa> tab = r.rpki.getFinalTabAspa();
            tab = tabRpkiUtil.allowedAspa(tab, bits.str2num(cmd.word()));
            doShowAspas(tab, 1);
            return;
        }
        if (a.equals("pubkey")) {
            tabGen<tabRpkiKey> tab = r.rpki.getFinalTabKey();
            tab = tabRpkiUtil.allowedKey(tab, bits.str2num(cmd.word()));
            doShowKeys(tab, 1);
            return;
        }
        if (a.equals("learned4")) {
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
            doShowRoas(nei.getFinalTabRoa(4), 1);
            return;
        }
        if (a.equals("learned6")) {
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
            doShowRoas(nei.getFinalTabRoa(6), 1);
            return;
        }
        if (a.equals("learnedp")) {
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
            doShowAspas(nei.getFinalTabAspa(), 1);
            return;
        }
        if (a.equals("learnedk")) {
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
            doShowKeys(nei.getFinalTabKey(), 1);
            return;
        }
        if (a.equals("compare4")) {
            doShowIpXrpkiComp(r.rpki, 4);
            return;
        }
        if (a.equals("compare6")) {
            doShowIpXrpkiComp(r.rpki, 6);
            return;
        }
        if (a.equals("comparep")) {
            doShowIpXrpkiComp(r.rpki, -1);
            return;
        }
        if (a.equals("comparek")) {
            doShowIpXrpkiComp(r.rpki, -2);
            return;
        }
        if (a.equals("evaluate")) {
            int loc = bits.str2num(cmd.word());
            addrPrefix<addrIP> pfx = addrPrefix.str2ip(cmd.word());
            if (pfx == null) {
                cmd.error("bad prefix");
                return;
            }
            tabRouteAttr<addrIP> ntry = new tabRouteAttr<addrIP>();
            ntry.pathSeq = new ArrayList<Integer>();
            for (;;) {
                a = cmd.word();
                if (a.length() < 1) {
                    break;
                }
                ntry.pathSeq.add(bits.str2num(a));
            }
            int ver;
            if (pfx.network.isIPv4()) {
                ver = 4;
            } else {
                ver = 6;
            }
            tabGen<tabRpkiRoa> roas = r.rpki.getFinalTabRoa(ver);
            tabGen<tabRpkiAspa> aspas = r.rpki.getFinalTabAspa();
            tabRpkiRoa rv = tabRpkiUtil.lookupRoa(roas, pfx);
            int o = tabRpkiUtil.calcValidityRoa(pfx, ntry, rv);
            int p = tabRpkiUtil.calcValidityAspa(ntry, aspas, loc);
            cmd.error("asn=" + clntWhois.asn2mixed(loc, true) + " pfx=" + addrPrefix.ip2str(pfx));
            cmd.error("path=" + ntry.asMixedStr());
            cmd.error("roa=" + tabRpkiUtil.validity2string(o) + " aspa=" + tabRpkiUtil.validity2string(p));
            return;
        }
        if (a.equals("lookup4")) {
            addrPrefix<addrIP> pfx = addrPrefix.str2ip(cmd.word());
            if (pfx == null) {
                cmd.error("bad prefix");
                return;
            }
            tabGen<tabRpkiRoa> tab = r.rpki.getFinalTabRoa(4);
            tabRpkiRoa ntry = tabRpkiUtil.lookupRoa(tab, pfx);
            if (ntry == null) {
                cmd.error("no matching roa");
                return;
            }
            rdr.putStrTab(ntry.fullDump());
            return;
        }
        if (a.equals("lookup6")) {
            addrPrefix<addrIP> pfx = addrPrefix.str2ip(cmd.word());
            if (pfx == null) {
                cmd.error("bad prefix");
                return;
            }
            tabGen<tabRpkiRoa> tab = r.rpki.getFinalTabRoa(6);
            tabRpkiRoa ntry = tabRpkiUtil.lookupRoa(tab, pfx);
            if (ntry == null) {
                cmd.error("no matching roa");
                return;
            }
            rdr.putStrTab(ntry.fullDump());
            return;
        }
        if (a.equals("lookupp")) {
            tabGen<tabRpkiAspa> tab = r.rpki.getFinalTabAspa();
            tabRpkiAspa ntry = tabRpkiUtil.lookupAspa(tab, bits.str2num(cmd.word()));
            if (ntry == null) {
                cmd.error("no matching aspa");
                return;
            }
            rdr.putStrTab(ntry.fullDump());
            return;
        }
        if (a.equals("lookupk")) {
            tabGen<tabRpkiKey> tab = r.rpki.getFinalTabKey();
            tabRpkiKey ntry = new tabRpkiKey();
            ntry.fromString(cmd);
            ntry = tabRpkiUtil.lookupKey(tab, ntry.asn, ntry.ski);
            if (ntry == null) {
                cmd.error("no matching key");
                return;
            }
            rdr.putStrTab(ntry.fullDump());
            return;
        }
    }

    private rtrRpki getRpkiTable() {
        tabRouteAttr.routeType rt = cfgRtr.name2num(cmd.word());
        if (ipRtr.isRPKI(rt) < 0) {
            cmd.error("not an rpki process");
            return null;
        }
        int rn = bits.str2num(cmd.word());
        cfgRtr rc = cfgAll.rtrFind(rt, rn, false);
        if (rc == null) {
            cmd.error("no such process");
            return null;
        }
        ipRtr ri = rc.getRouter();
        if (ri == null) {
            cmd.error("process not initialized");
            return null;
        }
        tabGen<tabRpkiRoa> rp;
        tabGen<tabRpkiAspa> ra;
        try {
            rtrRpki rr = (rtrRpki) ri;
            return rr;
        } catch (Exception e) {
            logger.traceback(e);
        }
        return null;
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
        rtrLdpNeigh nei = fwd.ldpNeighFind(adr, false);
        if (nei == null) {
            cmd.error("no such neighbor");
            return;
        }
        a = cmd.word();
        if (a.equals("nulled")) {
            doShowRoutes(fwd, tabRouteUtil.nullLabeled(nei.prefLearn), 3);
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
        if (a.equals("other-database")) {
            doShowRoutes(r.babel.fwdCore, r.babel.other.routerComputedU, 1);
            return;
        }
        if (a.equals("other-originate")) {
            doShowRoutes(r.babel.fwdCore, r.babel.other.routerRedistedU, 1);
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
        if (a.equals("other-learned")) {
            doShowRoutes(r.babel.fwdCore, nei.olearned, 1);
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
        if (a.equals("status")) {
            rdr.putStrTab(r.bgp.getStatus());
            return;
        }
        if (a.equals("tables")) {
            rdr.putStrTab(r.bgp.getTables());
            return;
        }
        if (a.equals("statistics")) {
            rdr.putStrTab(r.bgp.getMsgStats());
            return;
        }
        if (a.equals("attributes")) {
            rdr.putStrTab(r.bgp.getAttrStats());
            return;
        }
        if (a.equals("lspf")) {
            a = cmd.word();
            if (a.equals("spf")) {
                rdr.putStrTab(r.bgp.getSpfStat());
                rdr.putStrTab(r.bgp.getSpfLog());
                return;
            }
            if (a.equals("topology")) {
                rdr.putStrTab(r.bgp.getSpfTopo(cmd));
                return;
            }
            if (a.equals("tree")) {
                rdr.putStrArr(r.bgp.getSpfTree(cmd));
                return;
            }
            if (a.equals("othertree")) {
                rdr.putStrArr(r.bgp.getSpfOtherTree(cmd));
                return;
            }
            if (a.equals("othertopology")) {
                rdr.putStrTab(r.bgp.getSpfOtherTopo(cmd));
                return;
            }
            if (a.equals("graph")) {
                rdr.putStrArr(r.bgp.getSpfGraph(cmd));
                return;
            }
            if (a.equals("nhinconsistent")) {
                tabIntMatcher mtch = new tabIntMatcher();
                mtch.fromString(cmd.word());
                rdr.putStrTab(r.bgp.getNhIncons(mtch));
                return;
            }
            if (a.equals("lnkinconsistent")) {
                tabIntMatcher mtch = new tabIntMatcher();
                mtch.fromString(cmd.word());
                rdr.putStrTab(r.bgp.getMetIncons(mtch));
                return;
            }
            if (a.equals("nonredundant")) {
                rdr.putStrTab(r.bgp.showNonRedundant());
                return;
            }
            if (a.equals("route")) {
                doShowRoutes(r.bgp.fwdCore, r.bgp.getSpfRoute(), 1);
                return;
            }
            if (a.equals("originate")) {
                doShowRoutes(r.bgp.fwdCore, r.bgp.routerRedistedU, 1);
                return;
            }
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
            if (a.equals("tables")) {
                rdr.putStrTab(grp.getTables());
                return;
            }
            int idx = rtrBgpParam.string2idx(a);
            if (idx < 0) {
                return;
            }
            int dsp = rtrBgpParam.displayModel(idx);
            tabRoute<addrIP> tab = grp.willing[idx];
            doShowRoutes(r.bgp.fwdCore, tab, dsp);
            return;
        }
        if (a.equals("nexthop")) {
            rdr.putStrTab(r.bgp.showSummary(3));
            return;
        }
        if (a.equals("desummary")) {
            rdr.putStrTab(r.bgp.showSummary(17));
            return;
        }
        if (a.equals("asummary")) {
            rdr.putStrTab(r.bgp.showSummary(19));
            return;
        }
        if (a.equals("pfxsummary")) {
            rdr.putStrTab(r.bgp.showSummary(20));
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
        if (a.equals("unknowns")) {
            rdr.putStrTab(r.bgp.showSummary(18));
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
            if (a.equals("attributes")) {
                rdr.putStrTab(nei.getAttrStats());
                return;
            }
            if (a.equals("statistics")) {
                rdr.putStrTab(nei.getMsgStats());
                return;
            }
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
            if (a.equals("tables")) {
                rdr.putStrTab(nei.getTables());
                return;
            }
            int idx = rtrBgpParam.string2idx(a);
            if (idx < 0) {
                return;
            }
            int dsp = rtrBgpParam.displayModel(idx);
            a = cmd.word();
            if (a.equals("learned")) {
                doShowRoutes(r.bgp.fwdCore, nei.conn.learnt[idx], dsp);
                return;
            }
            if (a.equals("accepted")) {
                doShowRoutes(r.bgp.fwdCore, nei.acceptd[idx], dsp);
                return;
            }
            if (a.equals("willing")) {
                doShowRoutes(r.bgp.fwdCore, nei.willing[idx], dsp);
                return;
            }
            if (a.equals("advertised")) {
                doShowRoutes(r.bgp.fwdCore, nei.conn.advert[idx], dsp);
                return;
            }
            cmd.badCmd();
            return;
        }
        int idx = rtrBgpParam.string2idx(a);
        if (idx < 0) {
            cmd.badCmd();
            return;
        }
        int dsp = rtrBgpParam.displayModel(idx);
        int sfi = r.bgp.idx2safi(idx);
        if (sfi < 1) {
            return;
        }
        a = cmd.word();
        if (a.equals("summary")) {
            rdr.putStrTab(r.bgp.showNeighs(idx));
            return;
        }
        if (a.equals("asgraph")) {
            rdr.putStrArr(r.bgp.getAsGraph(idx));
            return;
        }
        if (a.equals("astree")) {
            a = cmd.word();
            int i = bits.str2num(a);
            rdr.putStrArr(r.bgp.getAsTree(idx, i));
            return;
        }
        if (a.equals("asorigin")) {
            rdr.putStrTab(r.bgp.getAsOrigin(idx));
            return;
        }
        if (a.equals("asuplink")) {
            rdr.putStrTab(r.bgp.getAsUplink(idx));
            return;
        }
        if (a.equals("astransit")) {
            rdr.putStrTab(r.bgp.getAsTransit(idx));
            return;
        }
        if (a.equals("asconn")) {
            rdr.putStrTab(r.bgp.getAsConns(idx));
            return;
        }
        if (a.equals("pathsof")) {
            a = cmd.word();
            rdr.putStrTab(r.bgp.getPathContain(idx, bits.str2num(a)));
            return;
        }
        if (a.equals("pathsat")) {
            a = cmd.word();
            rdr.putStrTab(r.bgp.getPathAround(idx, bits.str2num(a)));
            return;
        }
        if (a.equals("pathstat")) {
            rdr.putStrTab(r.bgp.getPathStat(idx));
            return;
        }
        if (a.equals("prefix-lengths")) {
            rdr.putStrTab(rtrLogger.prefixLengths(r.bgp.computd[idx], rtrBgpUtil.safi2ipVers(sfi)));
            return;
        }
        if (a.equals("asinconsistent")) {
            a = cmd.word();
            if (a.length() < 1) {
                a = "2-" + Integer.MAX_VALUE;
            }
            tabIntMatcher m = new tabIntMatcher();
            m.fromString(a);
            rdr.putStrTab(r.bgp.getAsIncons(idx, m));
            return;
        }
        if (a.equals("nhinconsistent")) {
            a = cmd.word();
            if (a.length() < 1) {
                a = "2-" + Integer.MAX_VALUE;
            }
            tabIntMatcher m = new tabIntMatcher();
            m.fromString(a);
            rdr.putStrTab(r.bgp.getNhIncons(idx, m));
            return;
        }
        if (a.equals("nhprefixes")) {
            rdr.putStrTab(r.bgp.getNhPrfxes(idx));
            return;
        }
        if (a.equals("nhtransit")) {
            rdr.putStrTab(r.bgp.getNhTrnsit(idx));
            return;
        }
        if (a.equals("nhorigin")) {
            rdr.putStrTab(r.bgp.getNhOrigin(idx));
            return;
        }
        if (a.equals("flapstat")) {
            rdr.putStrTab(r.bgp.getFlapstat(idx, bits.str2num(cmd.word())));
            return;
        }
        if (a.equals("flappath")) {
            addrPrefix<addrIP> ntry = addrPrefix.str2ip(cmd.word());
            if (ntry == null) {
                cmd.error("bad prefix");
                return;
            }
            rdr.putStrTab(r.bgp.getFlappath(idx, tabRouteUtil.string2rd(cmd.word()), ntry, false));
            return;
        }
        if (a.equals("flaprevpath")) {
            addrPrefix<addrIP> ntry = addrPrefix.str2ip(cmd.word());
            if (ntry == null) {
                cmd.error("bad prefix");
                return;
            }
            rdr.putStrTab(r.bgp.getFlappath(idx, tabRouteUtil.string2rd(cmd.word()), ntry, true));
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
            rdr.putStrTab(r.bgp.getAllRoutes(idx, ntry));
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
            tabRoute<addrIP> acc1 = nei1.acceptd[idx];
            tabRoute<addrIP> acc2 = nei2.acceptd[idx];
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
            df.calc1by1(dump1, dump2);
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
            long ign = 0;
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
            tabRoute<addrIP> acc1 = nei1.acceptd[idx];
            tabRoute<addrIP> acc2 = nei2.acceptd[idx];
            tabRoute<addrIP> uni1 = new tabRoute<addrIP>("tab");
            tabRoute<addrIP> uni2 = new tabRoute<addrIP>("tab");
            tabRoute<addrIP> diff = new tabRoute<addrIP>("tab");
            tabRouteUtil.compareTables(uni1, diff, acc1, acc2, ign, flt, sfi, nei1.remoteAs, nei2.remoteAs, upd);
            tabRouteUtil.compareTables(uni2, diff, acc2, acc1, ign, flt, sfi, nei2.remoteAs, nei1.remoteAs, upd);
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
            long ign = 0;
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
            tabRoute<addrIP> acc1 = nei1.acceptd[idx];
            tabRoute<addrIP> acc2 = nei2.acceptd[idx];
            tabRoute<addrIP> dif1 = new tabRoute<addrIP>("tab");
            tabRouteUtil.compareTables(dif1, dif1, acc1, acc2, ign, flt, sfi, nei1.remoteAs, nei2.remoteAs, upd);
            tabRouteUtil.compareTables(dif1, dif1, acc2, acc1, ign, flt, sfi, nei2.remoteAs, nei1.remoteAs, upd);
            tabRoute<addrIP> dif2 = new tabRoute<addrIP>("tab");
            if (dif1.size() > 0) {
                bits.sleep(tim);
                tabRouteUtil.compareTables(dif2, dif2, acc1, acc2, ign, flt, sfi, nei1.remoteAs, nei2.remoteAs, upd);
                tabRouteUtil.compareTables(dif2, dif2, acc2, acc1, ign, flt, sfi, nei2.remoteAs, nei1.remoteAs, upd);
            }
            tabRoute<addrIP> dif3 = new tabRoute<addrIP>("tab");
            cmd.error("constant differences");
            tabRouteUtil.compareDiffs(dif3, dif1, dif2);
            doShowRoutes(r.bgp.fwdCore, dif3, dsp);
            return;
        }
        tabRoute<addrIP> tab = r.bgp.computd[idx];
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
            rtrBgpNeigh nei = new rtrBgpNeigh(r.bgp, new addrIP());
            rtrBgpSpeak spk = new rtrBgpSpeak(r.bgp, nei, null, 0);
            packHolder pck = new packHolder(true, true);
            packHolder tmp = new packHolder(true, true);
            rtrBgpDump.witeFormat(spk, sfi, ntry, r.bgp.fwdCore.ipVersion, pck, tmp, true);
            ipCor4 ic4 = new ipCor4();
            ipCor6 ic6 = new ipCor6();
            tabGen<tabSessionEntry> ses = new tabGen<tabSessionEntry>();
            List<String> l = rtrBgpDump.dumpPacketFull(spk, ic4, ic6, ses, tmp, pck);
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
            rtrBgpNeigh nei = new rtrBgpNeigh(r.bgp, new addrIP());
            rtrBgpSpeak spk = new rtrBgpSpeak(r.bgp, nei, null, 0);
            packHolder pck = new packHolder(true, true);
            packHolder tmp = new packHolder(true, true);
            rtrBgpDump.witeFormat(spk, sfi, ntry, r.bgp.fwdCore.ipVersion, pck, tmp, false);
            ipCor4 ic4 = new ipCor4();
            ipCor6 ic6 = new ipCor6();
            tabGen<tabSessionEntry> ses = new tabGen<tabSessionEntry>();
            List<String> l = rtrBgpDump.dumpPacketFull(spk, ic4, ic6, ses, tmp, pck);
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
        if (a.equals("unusual")) {
            long ign = 0;
            for (;;) {
                a = cmd.word();
                if (a.length() < 1) {
                    break;
                }
                ign |= tabRouteAttr.string2ignore(a);
            }
            tab = tabRouteUtil.unusualAttribs(tab, ign);
            doShowRoutes(r.bgp.fwdCore, tab, dsp);
            return;
        }
        if (a.equals("compress")) {
            tab = new tabRoute<addrIP>(tab);
            tabRouteUtil.compressTable(sfi, tab, null);
            doShowRoutes(r.bgp.fwdCore, tab, dsp);
            return;
        }
        if (a.equals("overlap")) {
            tabIntMatcher mtch = new tabIntMatcher();
            mtch.fromString(cmd.word());
            rdr.putStrTab(tabRouteUtil.overlapTable(tab, mtch));
            return;
        }
        if (a.equals("deaggregated")) {
            tab = tabRouteUtil.deaggregatedPaths(tab);
            doShowRoutes(r.bgp.fwdCore, tab, dsp);
            return;
        }
        if (a.equals("pathprep")) {
            a = cmd.word();
            if (a.length() < 1) {
                a = "1-" + Integer.MAX_VALUE;
            }
            tabIntMatcher m = new tabIntMatcher();
            m.fromString(a);
            tab = tabRouteUtil.prependsUsed(tab, m);
            doShowRoutes(r.bgp.fwdCore, tab, dsp);
            return;
        }
        if (a.equals("pathloop")) {
            a = cmd.word();
            if (a.length() < 1) {
                a = "1-" + Integer.MAX_VALUE;
            }
            tabIntMatcher m = new tabIntMatcher();
            m.fromString(a);
            tab = tabRouteUtil.loopsFound(tab, m);
            doShowRoutes(r.bgp.fwdCore, tab, dsp);
            return;
        }
        if (a.equals("prefix-unused")) {
            List<String> res = new ArrayList<String>();
            tabRouteUtil.unusedPrefixes(tab, res);
            rdr.putStrArr(res);
            return;
        }
        if (a.equals("validated")) {
            doShowRoutes(r.bgp.fwdCore, tab, 4);
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
        if (a.equals("asmixed")) {
            doShowRoutes(r.bgp.fwdCore, tab, 13);
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
        if (a.equals("segrout")) {
            doShowRoutes(r.bgp.fwdCore, tab, 7);
            return;
        }
        if (a.equals("bier")) {
            doShowRoutes(r.bgp.fwdCore, tab, 8);
            return;
        }
        if (a.equals("ecmp")) {
            doShowRoutes(r.bgp.fwdCore, tab, dsp + 2000);
            return;
        }
        if (a.equals("validof")) {
            int asn = bits.str2num(cmd.word());
            rtrRpki rp = getRpkiTable();
            if (rp == null) {
                return;
            }
            tabGen<tabRpkiRoa> ro = rp.getFinalTabRoa(rtrBgpUtil.safi2ipVers(sfi));
            tabGen<tabRpkiAspa> ra = rp.getFinalTabAspa();
            tabRoute<addrIP> res = new tabRoute<addrIP>("rpki");
            for (int i = 0; i < tab.size(); i++) {
                tabRouteEntry<addrIP> ntry = tab.get(i);
                if (ntry == null) {
                    continue;
                }
                if (ntry.best.asPathEnd() != asn) {
                    continue;
                }
                ntry = ntry.copyBytes(tabRoute.addType.better);
                tabRpkiRoa rv = tabRpkiUtil.lookupRoa(ro, ntry.prefix);
                int o = tabRpkiUtil.calcValidityRoa(ntry.prefix, ntry.best, rv);
                int p = tabRpkiUtil.calcValidityAspa(ntry.best, ra, r.bgp.localAs);
                tabRpkiUtil.updateJustValidity(ntry, o, p);
                res.add(tabRoute.addType.better, ntry, false, false);
            }
            doShowRoutes(r.bgp.fwdCore, res, 4);
            return;
        }
        if (a.equals("validsum")) {
            rtrRpki rp = getRpkiTable();
            if (rp == null) {
                return;
            }
            tabGen<tabRpkiRoa> ro = rp.getFinalTabRoa(rtrBgpUtil.safi2ipVers(sfi));
            tabGen<tabRpkiAspa> ra = rp.getFinalTabAspa();
            int roaC[] = new int[4];
            int roaE[] = new int[4];
            int roaV[] = new int[4];
            int aspaC[] = new int[4];
            int aspaE[] = new int[4];
            int aspaV[] = new int[4];
            for (int i = 0; i < tab.size(); i++) {
                tabRouteEntry<addrIP> ntry = tab.get(i);
                if (ntry == null) {
                    continue;
                }
                tabRpkiRoa rv = tabRpkiUtil.lookupRoa(ro, ntry.prefix);
                int o = tabRpkiUtil.calcValidityRoa(ntry.prefix, ntry.best, rv);
                roaC[o]++;
                o = tabRouteUtil.getValidExtCommRoa(ntry.best.extComm);
                roaE[o]++;
                roaV[ntry.best.validRoa]++;
                o = tabRpkiUtil.calcValidityAspa(ntry.best, ra, r.bgp.localAs);
                aspaC[o]++;
                o = tabRouteUtil.getValidExtCommAspa(ntry.best.extComm);
                aspaE[o]++;
                aspaV[ntry.best.validAspa]++;
            }
            userFormat sum = new userFormat("|", "result|calc|using|encod|calc|using|encod", "1|3roa|3aspa");
            for (int i = 0; i < roaC.length; i++) {
                sum.add(tabRpkiUtil.validity2string(i) + "|" + roaC[i] + "|" + roaV[i] + "|" + roaE[i] + "|" + aspaC[i] + "|" + aspaV[i] + "|" + aspaE[i]);
            }
            rdr.putStrTab(sum);
            return;
        }
        if (a.equals("validtest")) {
            rtrRpki rp = getRpkiTable();
            if (rp == null) {
                return;
            }
            tabGen<tabRpkiRoa> ro = rp.getFinalTabRoa(rtrBgpUtil.safi2ipVers(sfi));
            tabGen<tabRpkiAspa> ra = rp.getFinalTabAspa();
            tabRoute<addrIP> res = new tabRoute<addrIP>("rpki");
            for (int i = 0; i < tab.size(); i++) {
                tabRouteEntry<addrIP> ntry = tab.get(i);
                if (ntry == null) {
                    continue;
                }
                ntry = ntry.copyBytes(tabRoute.addType.better);
                tabRpkiRoa rv = tabRpkiUtil.lookupRoa(ro, ntry.prefix);
                int o = tabRpkiUtil.calcValidityRoa(ntry.prefix, ntry.best, rv);
                int p = tabRpkiUtil.calcValidityAspa(ntry.best, ra, r.bgp.localAs);
                tabRpkiUtil.updateJustValidity(ntry, o, p);
                res.add(tabRoute.addType.better, ntry, false, false);
            }
            doShowRoutes(r.bgp.fwdCore, res, 4);
            return;
        }
        if (a.equals("validmismark")) {
            rtrRpki rp = getRpkiTable();
            if (rp == null) {
                return;
            }
            tabGen<tabRpkiRoa> ro = rp.getFinalTabRoa(rtrBgpUtil.safi2ipVers(sfi));
            tabGen<tabRpkiAspa> ra = rp.getFinalTabAspa();
            tabRoute<addrIP> res = new tabRoute<addrIP>("rpki");
            for (int i = 0; i < tab.size(); i++) {
                tabRouteEntry<addrIP> ntry = tab.get(i);
                if (ntry == null) {
                    continue;
                }
                tabRpkiRoa rv = tabRpkiUtil.lookupRoa(ro, ntry.prefix);
                int o = tabRpkiUtil.calcValidityRoa(ntry.prefix, ntry.best, rv);
                int p = tabRouteUtil.getValidExtCommRoa(ntry.best.extComm);
                if (o != p) {
                    res.add(tabRoute.addType.better, ntry, false, false);
                    continue;
                }
                o = tabRpkiUtil.calcValidityAspa(ntry.best, ra, r.bgp.localAs);
                p = tabRouteUtil.getValidExtCommAspa(ntry.best.extComm);
                if (o != p) {
                    res.add(tabRoute.addType.better, ntry, false, false);
                    continue;
                }
            }
            doShowRoutes(r.bgp.fwdCore, res, 4);
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
        tabRtrmapN ntry = new tabRtrmapN();
        if (ntry.cfgDoMatch(a, cmd)) {
            return;
        }
        tabListing<tabRtrmapN, addrIP> roumap = new tabListing<tabRtrmapN, addrIP>();
        ntry.action = tabListingEntry.actionType.actPermit;
        roumap.add(ntry);
        tabRoute<addrIP> res = new tabRoute<addrIP>("dump");
        tabRoute.addUpdatedTable(tabRoute.addType.better, sfi, 0, res, tab, false, roumap, null, null);
        doShowRoutes(r.bgp.fwdCore, res, dsp);
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

    private void doShowHostWatch(int ver) {
        cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
        if (ifc == null) {
            cmd.error("no such interface");
            return;
        }
        ipFwdIface fwd;
        if (ver == 4) {
            fwd = ifc.fwdIf4;
        } else {
            fwd = ifc.fwdIf6;
        }
        if (fwd == null) {
            cmd.error("protocol not enabled");
            return;
        }
        if (fwd.hostWatch == null) {
            cmd.error("feature not enabled");
            return;
        }
        rdr.putStrTab(fwd.hostWatch.getShow());
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
        tabRouteUtil.compressTable(rtrBgpUtil.sfiUnicast, tab, null);
        doShowRoutes(fwd, tab, 1);
    }

    private void doShowRouteOvrlp(int ver) {
        ipFwd fwd = findVrf(ver);
        if (fwd == null) {
            return;
        }
        tabIntMatcher mtch = new tabIntMatcher();
        mtch.fromString(cmd.word());
        rdr.putStrTab(tabRouteUtil.overlapTable(fwd.actualU, mtch));
    }

    private void doShowRouteUnusd(int ver) {
        ipFwd fwd = findVrf(ver);
        if (fwd == null) {
            return;
        }
        List<String> res = new ArrayList<String>();
        tabRouteUtil.unusedPrefixes(fwd.actualU, res);
        rdr.putStrArr(res);
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

    private void doShowPrefLens(int ver) {
        ipFwd fwd = findVrf(ver);
        if (fwd == null) {
            return;
        }
        rdr.putStrTab(rtrLogger.prefixLengths(fwd.actualU, fwd.ipVersion));
    }

    private void doShowNexthops(int ver) {
        ipFwd fwd = findVrf(ver);
        if (fwd == null) {
            return;
        }
        rdr.putStrTab(rtrLogger.nexthopDistribution(fwd.actualU));
    }

    private void doShowOutIfaces(int ver) {
        ipFwd fwd = findVrf(ver);
        if (fwd == null) {
            return;
        }
        rdr.putStrTab(rtrLogger.outgointInterfaces(fwd.actualU));
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

    private void doShowRoas(tabGen<tabRpkiRoa> tab, int typ) {
        userFormat lst = tabRpkiUtil.convertRoaHead(typ);
        if (lst == null) {
            return;
        }
        if (tab.size() < 1) {
            rdr.putStrTab(lst);
            return;
        }
        final int lines = cmd.pipe.settingsGet(pipeSetting.riblines, 8192);
        for (int pos = 0; pos < tab.size(); pos += lines) {
            tabGen<tabRpkiRoa> sub = tabRpkiUtil.getSubsetRoa(tab, pos, pos + lines);
            tabRpkiUtil.convertRoaBody(lst, sub, typ);
            if (rdr.putStrTab(lst)) {
                break;
            }
        }
    }

    private void doShowAspas(tabGen<tabRpkiAspa> tab, int typ) {
        userFormat lst = tabRpkiUtil.convertAspaHead(typ);
        if (lst == null) {
            return;
        }
        if (tab.size() < 1) {
            rdr.putStrTab(lst);
            return;
        }
        final int lines = cmd.pipe.settingsGet(pipeSetting.riblines, 8192);
        for (int pos = 0; pos < tab.size(); pos += lines) {
            tabGen<tabRpkiAspa> sub = tabRpkiUtil.getSubsetAspa(tab, pos, pos + lines);
            tabRpkiUtil.convertAspaBody(lst, sub, typ);
            if (rdr.putStrTab(lst)) {
                break;
            }
        }
    }

    private void doShowKeys(tabGen<tabRpkiKey> tab, int typ) {
        userFormat lst = tabRpkiUtil.convertKeyHead(typ);
        if (lst == null) {
            return;
        }
        if (tab.size() < 1) {
            rdr.putStrTab(lst);
            return;
        }
        final int lines = cmd.pipe.settingsGet(pipeSetting.riblines, 8192);
        for (int pos = 0; pos < tab.size(); pos += lines) {
            tabGen<tabRpkiKey> sub = tabRpkiUtil.getSubsetKey(tab, pos, pos + lines);
            tabRpkiUtil.convertKeyBody(lst, sub, typ);
            if (rdr.putStrTab(lst)) {
                break;
            }
        }
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
        if (cmd.size() > 0) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                cmd.error("no such vrf");
                return;
            }
            String a = cmd.word();
            doShowHistory(a, vrf.fwd4.hstryR);
            doShowHistory(a, vrf.fwd6.hstryR);
            return;
        }
        userFormat l = new userFormat("|", "name|rd|v4|v6|v4|v6|v4|v6|v4|v6|v4|v6|v4|v6|v4|v6|v4|v6", "2|2ifc|2uni|2mlt|2flw|2lab|2con|2nat|2grp");
        for (int o = 0; o < cfgAll.vrfs.size(); o++) {
            cfgVrf v = cfgAll.vrfs.get(o);
            l.add(v.name + "|" + tabRouteUtil.rd2string(v.fwd4.rd) + "|" + v.fwd4.ifaces.size() + "|" + v.fwd6.ifaces.size() + "|" + v.fwd4.actualU.size() + "|" + v.fwd6.actualU.size() + "|" + v.fwd4.actualM.size() + "|" + v.fwd6.actualM.size() + "|" + v.fwd4.actualF.size() + "|" + v.fwd6.actualF.size() + "|" + v.fwd4.labeldR.size() + "|" + v.fwd6.labeldR.size() + "|" + v.fwd4.connedR.size() + "|" + v.fwd6.connedR.size() + "|" + v.fwd4.natTrns.size() + "|" + v.fwd6.natTrns.size() + "|" + v.fwd4.groups.size() + "|" + v.fwd6.groups.size());
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

    private String getDiskPath() {
        String a = cmd.getRemaining();
        if (a.length() < 1) {
            return cfgInit.getRWpath();
        }
        if (cfgAll.buggy) {
            return a;
        }
        if (cfgAll.evalVdcPrivs()) {
            return cfgInit.getRWpath() + a;
        }
        return a;
    }

    private void doStatCfg(String a) {
        List<String> lst = bits.txt2buf(a);
        if (lst == null) {
            cmd.error("error reading file");
            return;
        }
        a = cmd.getRemaining().trim();
        if (a.equals("this")) {
            if (cfg == null) {
                return;
            }
            List<String> cur = cfg.getShRun(getConfigFilter(null, cmd));
            lst = userFilter.getSection(lst, userRead.section2filter(cur));
            rdr.putStrArr(lst);
            return;
        }
        if (cfgAll.evalVdcPrivs()) {
            cmd.error("not in a vdc");
            return;
        }
        if (a.length() > 0) {
            lst = userFilter.getSection(lst, userRead.filter2reg(a));
        }
        rdr.putStrArr(lst);
    }

}
