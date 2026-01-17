package org.freertr.serv;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrIPv6;
import org.freertr.addr.addrMac;
import org.freertr.addr.addrPrefix;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgIfc;
import org.freertr.clnt.clntAmt;
import org.freertr.clnt.clntEoIp;
import org.freertr.clnt.clntEtherIp;
import org.freertr.clnt.clntGtp;
import org.freertr.clnt.clntL2tp2;
import org.freertr.clnt.clntL2tp3;
import org.freertr.clnt.clntMplsPwe;
import org.freertr.clnt.clntPckOudp;
import org.freertr.clnt.clntVxlan;
import org.freertr.ifc.ifcBridgeAdr;
import org.freertr.ifc.ifcBridgeIfc;
import org.freertr.ifc.ifcBundleIfc;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcP2pOEservSess;
import org.freertr.ifc.ifcPolka;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdBier;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipFwdMcast;
import org.freertr.ip.ipFwdMpNe;
import org.freertr.ip.ipFwdMpmp;
import org.freertr.ip.ipIcmp4;
import org.freertr.ip.ipIcmp6;
import org.freertr.ip.ipIfc;
import org.freertr.ip.ipMpls;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenServ;
import org.freertr.prt.prtGre;
import org.freertr.prt.prtIpIp;
import org.freertr.prt.prtMgre;
import org.freertr.prt.prtTcp;
import org.freertr.prt.prtTmux;
import org.freertr.prt.prtUdp;
import org.freertr.rtr.rtrBgpEvpnPeer;
import org.freertr.rtr.rtrBgpUtil;
import org.freertr.tab.tabAceslstN;
import org.freertr.tab.tabConnect;
import org.freertr.tab.tabConnectEntry;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabIndex;
import org.freertr.tab.tabLabel;
import org.freertr.tab.tabLabelBier;
import org.freertr.tab.tabLabelBierN;
import org.freertr.tab.tabLabelDup;
import org.freertr.tab.tabLabelEntry;
import org.freertr.tab.tabListing;
import org.freertr.tab.tabListingEntry;
import org.freertr.tab.tabNatCfgN;
import org.freertr.tab.tabNatTraN;
import org.freertr.tab.tabNshEntry;
import org.freertr.tab.tabPbrN;
import org.freertr.tab.tabPrfxlstN;
import org.freertr.tab.tabQos;
import org.freertr.tab.tabQosN;
import org.freertr.tab.tabRoute;
import org.freertr.tab.tabRouteAttr;
import org.freertr.tab.tabRouteEntry;
import org.freertr.tab.tabRouteIface;
import org.freertr.tab.tabRouteUtil;
import org.freertr.tab.tabRtrmapN;
import org.freertr.tab.tabRtrplcN;
import org.freertr.tab.tabSession;
import org.freertr.tab.tabSessionEntry;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.history;
import org.freertr.util.keyword;
import org.freertr.util.logger;
import org.freertr.util.state;

/**
 * one p4lang connection
 *
 * @author matecsaba
 */
public class servP4langConn implements Runnable {

    /**
     * pipeline in use
     */
    protected final pipeSide pipe;

    private final servP4lang lower;

    private int keepalive;

    /**
     * exported labels
     */
    protected tabGen<tabLabelEntry> labels = new tabGen<tabLabelEntry>();

    /**
     * exported labels
     */
    protected tabGen<servP4langStrL<tabLabelEntry, tabLabelBierN>> labeld = new tabGen<servP4langStrL<tabLabelEntry, tabLabelBierN>>();

    /**
     * exported nshs
     */
    protected tabGen<tabNshEntry> nshs = new tabGen<tabNshEntry>();

    /**
     * create instance
     *
     * @param pip pipeline
     * @param upper config
     */
    protected servP4langConn(pipeSide pip, servP4lang upper) {
        pipe = pip;
        lower = upper;
    }

    /**
     * start working
     */
    protected void startWork() {
        new Thread(this).start();
    }

    public void run() {
        try {
            if (doNegot()) {
                pipe.setClose();
                return;
            }
            for (;;) {
                if (doReports()) {
                    break;
                }
                doExports();
                lower.notif.sleep(lower.expDelay);
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        logger.error("neighbor " + lower.remote + " down");
        pipe.setClose();
    }

    private boolean doNegot() {
        lower.started = bits.getTime();
        lower.reconns++;
        lower.neighs.clear();
        for (int i = lower.expIfc.size() - 1; i >= 0; i--) {
            servP4langIfc ntry = lower.expIfc.get(i);
            if (ntry.ifc == null) {
                lower.expIfc.del(ntry);
                continue;
            }
            if (ntry.hidden && ntry.dynamic) {
                lower.expIfc.del(ntry);
                continue;
            }
            ntry.doClear();
        }
        for (int i = 0; i < lower.expVrf.size(); i++) {
            lower.expVrf.get(i).doClear();
        }
        for (int i = 0; i < lower.expBr.size(); i++) {
            lower.expBr.get(i).doClear();
        }
        lower.capability = null;
        lower.platform = null;
        lower.ifcRngBeg = -1;
        lower.ifcRngEnd = -2;
        lower.vrfRngBeg = -1;
        lower.vrfRngEnd = -2;
        lower.neiRngBeg = -1;
        lower.neiRngEnd = -2;
        lower.cpuPort = -3;
        lower.statsPrt = -4;
        lower.statsTxt = null;
        lower.statsNtf = null;
        lower.frontnam.clear();
        lower.fwderrcr.clear();
        lower.autonegs.clear();
        lower.flwctrls.clear();
        for (;;) {
            if (pipe.isClosed() != 0) {
                logger.info("error negotiating with the dataplane");
                return true;
            }
            String s = pipe.lineGet(0x11).trim();
            if (s.length() < 1) {
                continue;
            }
            if (debugger.servP4langRx) {
                logger.debug("rx: " + s);
            }
            cmds cmd = new cmds("p4lang", s);
            lower.msgsGot++;
            keyword.update(lower.apiStatRx, s);
            s = cmd.word();
            if (s.equals("nomore")) {
                break;
            }
            if (s.equals("portname")) {
                int i = bits.str2num(cmd.word());
                s = cmd.getRemaining().replaceAll(" ", "_");
                servP4langMgcN ntry = new servP4langMgcN(i, s);
                lower.frontnam.put(ntry);
                continue;
            }
            if (s.equals("fecname")) {
                int i = bits.str2num(cmd.word());
                s = cmd.getRemaining().replaceAll(" ", "_");
                servP4langMgcN ntry = new servP4langMgcN(i, s);
                lower.fwderrcr.put(ntry);
                continue;
            }
            if (s.equals("anegname")) {
                int i = bits.str2num(cmd.word());
                s = cmd.getRemaining().replaceAll(" ", "_");
                servP4langMgcN ntry = new servP4langMgcN(i, s);
                lower.autonegs.put(ntry);
                continue;
            }
            if (s.equals("flwctrname")) {
                int i = bits.str2num(cmd.word());
                s = cmd.getRemaining().replaceAll(" ", "_");
                servP4langMgcN ntry = new servP4langMgcN(i, s);
                lower.flwctrls.put(ntry);
                continue;
            }
            if (s.equals("platform")) {
                lower.platform = cmd.getRemaining();
                continue;
            }
            if (s.equals("cpuport")) {
                lower.cpuPort = bits.str2num(cmd.word());
                continue;
            }
            if (s.equals("dynrange")) {
                lower.ifcRngBeg = bits.str2num(cmd.word());
                lower.ifcRngEnd = bits.str2num(cmd.word());
                continue;
            }
            if (s.equals("vrfrange")) {
                lower.vrfRngBeg = bits.str2num(cmd.word());
                lower.vrfRngEnd = bits.str2num(cmd.word());
                continue;
            }
            if (s.equals("neirange")) {
                lower.neiRngBeg = bits.str2num(cmd.word());
                lower.neiRngEnd = bits.str2num(cmd.word());
                continue;
            }
            if (s.equals("capabilities")) {
                lower.capability = cmd.getRemaining();
                continue;
            }
            if (s.equals("dataplane-say")) {
                logger.info("dataplane said: " + cmd.getRemaining());
                continue;
            }
            if (debugger.servP4langErr) {
                logger.debug("got unneeded report: " + cmd.getOriginal());
            }
        }
        if (lower.ifcRngBeg >= lower.ifcRngEnd) {
            logger.info("error negotiating interface range");
            return true;
        }
        if (lower.vrfRngBeg >= lower.vrfRngEnd) {
            logger.info("error negotiating vrf range");
            return true;
        }
        if (lower.neiRngBeg >= lower.neiRngEnd) {
            logger.info("error negotiating neighbor range");
            return true;
        }
        int nxt = lower.ifcRngBeg;
        for (int i = 0; i < lower.expIfc.size(); i++) {
            servP4langIfc ntry = lower.expIfc.get(i);
            ntry.setup2apiPack();
            if (ntry.reinit != null) {
                cmds cmd = new cmds("exp", ntry.reinit);
                int prt = servP4langUtil.toNum(lower.frontnam, cmd.word(), -1);
                if (prt < 0) {
                    continue;
                }
                lower.expIfc.del(ntry);
                ntry.id = prt;
                ntry.speed = cmd.word();
                ntry.errCorr = servP4langUtil.toNum(lower.fwderrcr, cmd.word(), 0);
                ntry.autoNeg = servP4langUtil.toNum(lower.autonegs, cmd.word(), 0);
                ntry.flowCtrl = servP4langUtil.toNum(lower.flwctrls, cmd.word(), 0);
                ntry.reinit = null;
                lower.expIfc.add(ntry);
            }
            if (!ntry.suppressState()) {
                lower.sendLine("ports_add " + ntry.id + " " + ntry.getStateEnding());
            }
            if (!ntry.dynamic) {
                continue;
            }
            ntry.id = nxt;
            nxt++;
        }
        nxt = lower.vrfRngBeg;
        for (int i = 0; i < lower.expVrf.size(); i++) {
            servP4langVrf ntry = lower.expVrf.get(i);
            ntry.id = nxt;
            nxt++;
        }
        logger.warn("neighbor " + lower.remote + " up");
        return false;
    }

    private boolean doReports() {
        for (;;) {
            if (pipe.isClosed() != 0) {
                return true;
            }
            if (pipe.ready2rx() < 8) {
                return false;
            }
            String s = pipe.lineGet(0x11).trim();
            if (s.length() < 1) {
                continue;
            }
            if (debugger.servP4langRx) {
                logger.debug("rx: " + s);
            }
            cmds cmd = new cmds("p4lang", s);
            lower.msgsGot++;
            keyword.update(lower.apiStatRx, s);
            s = cmd.word();
            if (s.equals("state")) {
                servP4langIfc ntry = lower.findIfc(bits.str2num(cmd.word()));
                if (ntry == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    continue;
                }
                if (cmd.word().equals("1")) {
                    ntry.lastState = state.states.up;
                } else {
                    ntry.lastState = state.states.down;
                }
                ntry.upper.setState(ntry.lastState);
                continue;
            }
            if (s.equals("counter")) {
                servP4langIfc ntry = lower.findIfc(bits.str2num(cmd.word()));
                if (ntry == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    continue;
                }
                ntry.ifc.ethtyp.hwCntr.packRx = bits.str2long(cmd.word());
                ntry.ifc.ethtyp.hwCntr.byteRx = bits.str2long(cmd.word());
                ntry.ifc.ethtyp.hwCntr.packTx = bits.str2long(cmd.word());
                ntry.ifc.ethtyp.hwCntr.byteTx = bits.str2long(cmd.word());
                ntry.ifc.ethtyp.hwCntr.packDr = bits.str2long(cmd.word());
                ntry.ifc.ethtyp.hwCntr.byteDr = bits.str2long(cmd.word());
                ntry.ifc.ethtyp.hwHstry.update(ntry.ifc.ethtyp.hwCntr, true);
                if (ntry.ifc.ethtyp.hwSub == null) {
                    continue;
                }
                ntry.ifc.ethtyp.hwCntr = ntry.ifc.ethtyp.hwCntr.minus(ntry.ifc.ethtyp.hwSub);
                continue;
            }
            if (s.equals("ethertype")) {
                servP4langIfc ntry = lower.findIfc(bits.str2num(cmd.word()));
                if (ntry == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    continue;
                }
                int i = bits.str2num(cmd.word());
                counter c = new counter();
                c.packRx = bits.str2long(cmd.word());
                c.byteRx = bits.str2long(cmd.word());
                ntry.ifc.ethtyp.putHwEthTyp(i, c);
                continue;
            }
            if (s.equals("nattrns4_cnt")) {
                servP4langVrf vrf = new servP4langVrf(bits.str2num(cmd.word()));
                vrf = lower.expVrf.find(vrf);
                if (vrf == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    continue;
                }
                servP4langUtil.updateNatTrans(cmd, vrf.vrf.fwd4);
                continue;
            }
            if (s.equals("nattrns6_cnt")) {
                servP4langVrf vrf = new servP4langVrf(bits.str2num(cmd.word()));
                vrf = lower.expVrf.find(vrf);
                if (vrf == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    continue;
                }
                servP4langUtil.updateNatTrans(cmd, vrf.vrf.fwd6);
                continue;
            }
            if (s.equals("inspect4_cnt")) {
                servP4langIfc ifc = new servP4langIfc(lower, bits.str2num(cmd.word()));
                ifc = lower.expIfc.find(ifc);
                if (ifc == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    continue;
                }
                servP4langUtil.updateInsp(cmd, ifc.sentInsp4);
                continue;
            }
            if (s.equals("inspect6_cnt")) {
                servP4langIfc ifc = new servP4langIfc(lower, bits.str2num(cmd.word()));
                ifc = lower.expIfc.find(ifc);
                if (ifc == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    continue;
                }
                if (ifc.ifc.fwdIf6 == null) {
                    continue;
                }
                servP4langUtil.updateInsp(cmd, ifc.sentInsp6);
                continue;
            }
            if (s.equals("macsec_cnt")) {
                servP4langIfc ntry = lower.findIfc(bits.str2num(cmd.word()));
                if (ntry == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    continue;
                }
                if (ntry.ifc.ethtyp.macSec == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    continue;
                }
                ntry.ifc.ethtyp.macSec.hwCntr = new counter();
                ntry.ifc.ethtyp.macSec.hwCntr.packRx = bits.str2long(cmd.word());
                ntry.ifc.ethtyp.macSec.hwCntr.byteRx = bits.str2long(cmd.word());
                ntry.ifc.ethtyp.macSec.hwCntr.packTx = bits.str2long(cmd.word());
                ntry.ifc.ethtyp.macSec.hwCntr.byteTx = bits.str2long(cmd.word());
                ntry.ifc.ethtyp.macSec.hwCntr.packDr = bits.str2long(cmd.word());
                ntry.ifc.ethtyp.macSec.hwCntr.byteDr = bits.str2long(cmd.word());
                continue;
            }
            if (s.equals("bridge_cnt")) {
                servP4langBr br = new servP4langBr(bits.str2num(cmd.word()));
                br = lower.expBr.find(br);
                if (br == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    continue;
                }
                addrMac mac = new addrMac();
                mac.fromString(cmd.word());
                ifcBridgeAdr ntry = br.br.bridgeHed.findMacAddr(mac);
                if (ntry == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    continue;
                }
                servP4langIfc ifc = lower.findIfc(ntry.ifc);
                if (ifc == null) {
                    servStackFwd oth = lower.parent.findIfc(lower.parid, ntry.ifc);
                    if (oth != null) {
                        continue;
                    }
                }
                counter old = ntry.hwCntr;
                ntry.hwCntr = new counter();
                ntry.hwCntr.packRx = bits.str2long(cmd.word());
                ntry.hwCntr.byteRx = bits.str2long(cmd.word());
                ntry.hwCntr.packTx = bits.str2long(cmd.word());
                ntry.hwCntr.byteTx = bits.str2long(cmd.word());
                if (old == null) {
                    continue;
                }
                if (old.compareTo(ntry.hwCntr) == 0) {
                    continue;
                }
                ntry.time = bits.getTime();
                continue;
            }
            if (s.equals("inacl4_cnt")) {
                servP4langIfc ntry = lower.findIfc(bits.str2num(cmd.word()));
                if (ntry == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    continue;
                }
                servP4langUtil.updateAcl(cmd, ntry.sentAcl4inF);
                continue;
            }
            if (s.equals("inacl6_cnt")) {
                servP4langIfc ntry = lower.findIfc(bits.str2num(cmd.word()));
                if (ntry == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    continue;
                }
                servP4langUtil.updateAcl(cmd, ntry.sentAcl6inF);
                continue;
            }
            if (s.equals("outacl4_cnt")) {
                servP4langIfc ntry = lower.findIfc(bits.str2num(cmd.word()));
                if (ntry == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    continue;
                }
                servP4langUtil.updateAcl(cmd, ntry.sentAcl4outF);
                continue;
            }
            if (s.equals("outacl6_cnt")) {
                servP4langIfc ntry = lower.findIfc(bits.str2num(cmd.word()));
                if (ntry == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    continue;
                }
                servP4langUtil.updateAcl(cmd, ntry.sentAcl6outF);
                continue;
            }
            if (s.equals("natacl4_cnt")) {
                servP4langVrf vrf = new servP4langVrf(bits.str2num(cmd.word()));
                vrf = lower.expVrf.find(vrf);
                if (vrf == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    continue;
                }
                servP4langUtil.updateAcl(cmd, vrf.natCfg4f);
                continue;
            }
            if (s.equals("natacl6_cnt")) {
                servP4langVrf vrf = new servP4langVrf(bits.str2num(cmd.word()));
                vrf = lower.expVrf.find(vrf);
                if (vrf == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    continue;
                }
                servP4langUtil.updateAcl(cmd, vrf.natCfg6f);
                continue;
            }
            if (s.equals("pbracl4_cnt")) {
                servP4langVrf vrf = new servP4langVrf(bits.str2num(cmd.word()));
                vrf = lower.expVrf.find(vrf);
                if (vrf == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    continue;
                }
                servP4langUtil.updatePbr(cmd, vrf.pbrCfg4);
                continue;
            }
            if (s.equals("pbracl6_cnt")) {
                servP4langVrf vrf = new servP4langVrf(bits.str2num(cmd.word()));
                vrf = lower.expVrf.find(vrf);
                if (vrf == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    continue;
                }
                servP4langUtil.updatePbr(cmd, vrf.pbrCfg6);
                continue;
            }
            if (s.equals("coppacl4_cnt")) {
                servP4langVrf vrf = new servP4langVrf(bits.str2num(cmd.word()));
                vrf = lower.expVrf.find(vrf);
                if (vrf == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    continue;
                }
                servP4langUtil.updateAcl(cmd, vrf.copp4f);
                continue;
            }
            if (s.equals("coppacl6_cnt")) {
                servP4langVrf vrf = new servP4langVrf(bits.str2num(cmd.word()));
                vrf = lower.expVrf.find(vrf);
                if (vrf == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    continue;
                }
                servP4langUtil.updateAcl(cmd, vrf.copp6f);
                continue;
            }
            if (s.equals("inqos4_cnt")) {
                servP4langIfc ntry = lower.findIfc(bits.str2num(cmd.word()));
                if (ntry == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    continue;
                }
                if (ntry.sentQos4inF.size() < 1) {
                    continue;
                }
                servP4langUtil.updateAcl(cmd, ntry.sentQos4inF.get(0));
                continue;
            }
            if (s.equals("inqos6_cnt")) {
                servP4langIfc ntry = lower.findIfc(bits.str2num(cmd.word()));
                if (ntry == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    continue;
                }
                if (ntry.sentQos6inF.size() < 1) {
                    continue;
                }
                servP4langUtil.updateAcl(cmd, ntry.sentQos6inF.get(0));
                continue;
            }
            if (s.equals("outqos4_cnt")) {
                servP4langIfc ntry = lower.findIfc(bits.str2num(cmd.word()));
                if (ntry == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    continue;
                }
                if (ntry.sentQos4outF.size() < 1) {
                    continue;
                }
                servP4langUtil.updateAcl(cmd, ntry.sentQos4outF.get(0));
                continue;
            }
            if (s.equals("outqos6_cnt")) {
                servP4langIfc ntry = lower.findIfc(bits.str2num(cmd.word()));
                if (ntry == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    continue;
                }
                if (ntry.sentQos6outF.size() < 1) {
                    continue;
                }
                servP4langUtil.updateAcl(cmd, ntry.sentQos6outF.get(0));
                continue;
            }
            if (s.equals("flowspec4_cnt")) {
                servP4langVrf vrf = new servP4langVrf(bits.str2num(cmd.word()));
                vrf = lower.expVrf.find(vrf);
                if (vrf == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    continue;
                }
                servP4langUtil.updateAcl(cmd, vrf.flwSpc4);
                continue;
            }
            if (s.equals("flowspec6_cnt")) {
                servP4langVrf vrf = new servP4langVrf(bits.str2num(cmd.word()));
                vrf = lower.expVrf.find(vrf);
                if (vrf == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    continue;
                }
                servP4langUtil.updateAcl(cmd, vrf.flwSpc6);
                continue;
            }
            if (s.equals("mroute4_cnt")) {
                servP4langVrf vrf = new servP4langVrf(bits.str2num(cmd.word()));
                vrf = lower.expVrf.find(vrf);
                if (vrf == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    continue;
                }
                servP4langUtil.updateMroute(cmd, vrf.vrf.fwd4);
                continue;
            }
            if (s.equals("mroute6_cnt")) {
                servP4langVrf vrf = new servP4langVrf(bits.str2num(cmd.word()));
                vrf = lower.expVrf.find(vrf);
                if (vrf == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    continue;
                }
                servP4langUtil.updateMroute(cmd, vrf.vrf.fwd6);
                continue;
            }
            if (s.equals("vrf4_cnt")) {
                servP4langVrf vrf = new servP4langVrf(bits.str2num(cmd.word()));
                vrf = lower.expVrf.find(vrf);
                if (vrf == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    continue;
                }
                vrf.vrf.fwd4.cntrH.packRx = bits.str2long(cmd.word());
                vrf.vrf.fwd4.cntrH.byteRx = bits.str2long(cmd.word());
                continue;
            }
            if (s.equals("vrf6_cnt")) {
                servP4langVrf vrf = new servP4langVrf(bits.str2num(cmd.word()));
                vrf = lower.expVrf.find(vrf);
                if (vrf == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    continue;
                }
                vrf.vrf.fwd6.cntrH.packRx = bits.str2long(cmd.word());
                vrf.vrf.fwd6.cntrH.byteRx = bits.str2long(cmd.word());
                continue;
            }
            if (s.equals("route4_cnt")) {
                servP4langVrf vrf = new servP4langVrf(bits.str2num(cmd.word()));
                vrf = lower.expVrf.find(vrf);
                if (vrf == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    continue;
                }
                addrIPv4 adr = new addrIPv4();
                adr.fromString(cmd.word());
                addrPrefix<addrIPv4> prf = new addrPrefix<addrIPv4>(adr, bits.str2num(cmd.word()));
                servP4langUtil.updateRoute(cmd, vrf.vrf.fwd4, addrPrefix.ip4toIP(prf));
                continue;
            }
            if (s.equals("route6_cnt")) {
                servP4langVrf vrf = new servP4langVrf(bits.str2num(cmd.word()));
                vrf = lower.expVrf.find(vrf);
                if (vrf == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    continue;
                }
                addrIPv6 adr = new addrIPv6();
                adr.fromString(cmd.word());
                addrPrefix<addrIPv6> prf = new addrPrefix<addrIPv6>(adr, bits.str2num(cmd.word()));
                servP4langUtil.updateRoute(cmd, vrf.vrf.fwd6, addrPrefix.ip6toIP(prf));
                continue;
            }
            if (s.equals("mpls_cnt")) {
                tabLabelEntry ntry = new tabLabelEntry(bits.str2num(cmd.word()));
                ntry = tabLabel.labels.find(ntry);
                if (ntry == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    continue;
                }
                ntry.hwCntr = new counter();
                ntry.hwCntr.packRx = bits.str2long(cmd.word());
                ntry.hwCntr.byteRx = bits.str2long(cmd.word());
                continue;
            }
            if (s.equals("polka_cnt")) {
                servP4langVrf vrf = new servP4langVrf(bits.str2num(cmd.word()));
                vrf = lower.expVrf.find(vrf);
                if (vrf == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    continue;
                }
                tabIndex<addrIP> ntry = new tabIndex<addrIP>(bits.str2num(cmd.word()), null);
                tabIndex<addrIP> res = vrf.vrf.fwd4.actualIU.find(ntry);
                if (res == null) {
                    res = vrf.vrf.fwd6.actualIU.find(ntry);
                }
                if (res == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    continue;
                }
                res.hwCntr = new counter();
                res.hwCntr.packRx = bits.str2long(cmd.word());
                res.hwCntr.byteRx = bits.str2long(cmd.word());
                continue;
            }
            if (s.equals("mpolka_cnt")) {
                servP4langVrf vrf = new servP4langVrf(bits.str2num(cmd.word()));
                vrf = lower.expVrf.find(vrf);
                if (vrf == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    continue;
                }
                tabIndex<addrIP> ntry = new tabIndex<addrIP>(bits.str2num(cmd.word()), null);
                tabIndex<addrIP> res = vrf.vrf.fwd6.actualIC.find(ntry);
                if (res == null) {
                    res = vrf.vrf.fwd4.actualIC.find(ntry);
                }
                if (res == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    continue;
                }
                res.hwCntr = new counter();
                res.hwCntr.packRx = bits.str2long(cmd.word());
                res.hwCntr.byteRx = bits.str2long(cmd.word());
                continue;
            }
            if (s.equals("nsh_cnt")) {
                int i = bits.str2num(cmd.word());
                tabNshEntry ntry = new tabNshEntry(i, bits.str2num(cmd.word()));
                ntry = tabNshEntry.services.find(ntry);
                if (ntry == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    continue;
                }
                ntry.hwCntr = new counter();
                ntry.hwCntr.packRx = bits.str2long(cmd.word());
                ntry.hwCntr.byteRx = bits.str2long(cmd.word());
                continue;
            }
            if (s.equals("neigh_cnt")) {
                continue;
            }
            if (s.equals("tunnel4_cnt")) {
                servP4langVrf vrf = new servP4langVrf(bits.str2num(cmd.word()));
                vrf = lower.expVrf.find(vrf);
                if (vrf == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    continue;
                }
                switch (bits.str2num(cmd.word())) {
                    case prtUdp.protoNum:
                        servP4langUtil.updateTunn(cmd, vrf.vrf.fwd4, vrf.vrf.udp4);
                        continue;
                    case prtTcp.protoNum:
                        servP4langUtil.updateTunn(cmd, vrf.vrf.fwd4, vrf.vrf.tcp4);
                        continue;
                }
                continue;
            }
            if (s.equals("tunnel6_cnt")) {
                servP4langVrf vrf = new servP4langVrf(bits.str2num(cmd.word()));
                vrf = lower.expVrf.find(vrf);
                if (vrf == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    continue;
                }
                switch (bits.str2num(cmd.word())) {
                    case prtUdp.protoNum:
                        servP4langUtil.updateTunn(cmd, vrf.vrf.fwd6, vrf.vrf.udp6);
                        continue;
                    case prtTcp.protoNum:
                        servP4langUtil.updateTunn(cmd, vrf.vrf.fwd6, vrf.vrf.tcp6);
                        continue;
                }
                continue;
            }
            if (s.equals("dataplane-say")) {
                logger.info("dataplane said: " + cmd.getRemaining());
                continue;
            }
            if (s.equals("stats_beg")) {
                int i = bits.str2num(cmd.word());
                if (lower.statsPrt != i) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    continue;
                }
                if (lower.statsTxt != null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    continue;
                }
                lower.statsTxt = new ArrayList<String>();
                continue;
            }
            if (s.equals("stats_txt")) {
                int i = bits.str2num(cmd.word());
                if (lower.statsPrt != i) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    continue;
                }
                if (lower.statsTxt == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    continue;
                }
                lower.statsTxt.add(cmd.getRemaining());
                continue;
            }
            if (s.equals("stats_end")) {
                int i = bits.str2num(cmd.word());
                if (lower.statsPrt != i) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    continue;
                }
                if (lower.statsNtf == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    continue;
                }
                lower.statsNtf.wakeup();
                continue;
            }
            if (debugger.servP4langErr) {
                logger.debug("got unneeded report: " + cmd.getOriginal());
            }
            continue;
        }
    }

    private void doExports() {
        if (pipe.ready2tx() < lower.minBuf) {
            lower.rndSkipLast = bits.getTime();
            lower.rndSkipNum++;
            return;
        }
        long tim = bits.getTime();
        for (int i = 0; i < lower.neighs.size(); i++) {
            lower.neighs.get(i).need = 0;
        }
        keepalive++;
        if (keepalive > 30) {
            String a = "keepalive";
            lower.sendLine(a);
            keepalive = 0;
        }
        for (int i = 0; i < lower.parent.bckplnLab.length; i++) {
            doBckpln(i);
            doLab0(lower.parent.bckplnLab[i], i);
        }
        for (int i = 0; i < lower.expBr.size(); i++) {
            doBrdg(lower.expBr.get(i));
        }
        doDynAcc();
        for (int i = lower.expIfc.size() - 1; i >= 0; i--) {
            servP4langIfc ifc = lower.expIfc.get(i);
            if (ifc.ifc == null) {
                continue;
            }
            if (doIface(ifc)) {
                lower.expIfc.del(ifc);
                ifc.tearDown();
                continue;
            }
            doNeighs(true, ifc, ifc.ifc.ipIf4);
            doNeighs(false, ifc, ifc.ifc.ipIf6);
        }
        for (int i = 0; i < lower.expVrf.size(); i++) {
            servP4langVrf vrf = lower.expVrf.get(i);
            if (servP4langUtil.needAcl(vrf.copp4p, vrf.copp4c, null, null, null, vrf.copp4f)) {
                sendAcl(0, "copp4_del " + vrf.id + " ", "", "", "", "", true, false, vrf.copp4f, null, null, null);
                vrf.copp4p = vrf.copp4c;
                sendAcl(0, "copp4_add " + vrf.id + " ", "", "", "", "", true, false, vrf.copp4p, null, null, vrf.copp4f);
            }
            if (servP4langUtil.needAcl(vrf.copp6p, vrf.copp6c, null, null, null, vrf.copp6f)) {
                sendAcl(0, "copp6_del " + vrf.id + " ", "", "", "", "", false, false, vrf.copp6f, null, null, null);
                vrf.copp6p = vrf.copp6c;
                sendAcl(0, "copp6_add " + vrf.id + " ", "", "", "", "", false, false, vrf.copp6p, null, null, vrf.copp6f);
            }
            doVrf(vrf);
            doRoutes(true, vrf.id, vrf.vrf.fwd4.commonLabel, vrf.vrf.fwd4.actualU, vrf.routes4, vrf.routed4, vrf.compress4, vrf.prflst4, vrf.roumap4, vrf.roupol4);
            doRoutes(false, vrf.id, vrf.vrf.fwd6.commonLabel, vrf.vrf.fwd6.actualU, vrf.routes6, vrf.routed6, vrf.compress6, vrf.prflst6, vrf.roumap6, vrf.roupol6);
            doIndexes("", vrf.id, vrf.vrf.fwd4.actualIU, vrf.indexUd4, vrf.vrf.fwd4.actualU, vrf.indexUs4);
            doIndexes("", vrf.id, vrf.vrf.fwd6.actualIU, vrf.indexUd6, vrf.vrf.fwd6.actualU, vrf.indexUs6);
            doIndexes("m", vrf.id, vrf.vrf.fwd4.actualIC, vrf.indexCd4, vrf.vrf.fwd4.actualU, vrf.indexCs4);
            doIndexes("m", vrf.id, vrf.vrf.fwd6.actualIC, vrf.indexCd6, vrf.vrf.fwd6.actualU, vrf.indexCs6);
            doMroutes(true, vrf.id, vrf.vrf.fwd4.commonLabel, vrf.vrf.fwd4.groups, vrf.mroutes4, vrf.mrouted4);
            doMroutes(false, vrf.id, vrf.vrf.fwd6.commonLabel, vrf.vrf.fwd6.groups, vrf.mroutes6, vrf.mrouted6);
            vrf.natCfg4 = doNatCfg(true, vrf.id, vrf.vrf.fwd4.natCfg, vrf.natCfg4, vrf.natCfg4f);
            vrf.natCfg6 = doNatCfg(false, vrf.id, vrf.vrf.fwd6.natCfg, vrf.natCfg6, vrf.natCfg6f);
            doNatTrns(true, vrf.id, vrf.vrf.fwd4.natTrns, vrf.natTrns4);
            doNatTrns(false, vrf.id, vrf.vrf.fwd6.natTrns, vrf.natTrns6);
            doPbrCfg(true, vrf, vrf.vrf.fwd4.pbrCfg, vrf.pbrCfg4);
            doPbrCfg(false, vrf, vrf.vrf.fwd6.pbrCfg, vrf.pbrCfg6);
            vrf.flwSpc4 = doFlwSpc(true, vrf, vrf.vrf.fwd4, vrf.flwSpc4);
            vrf.flwSpc6 = doFlwSpc(false, vrf, vrf.vrf.fwd6, vrf.flwSpc6);
            if (!lower.expSck) {
                continue;
            }
            doSockets(false, vrf.id, vrf.vrf.udp4.getProtoNum(), vrf.vrf.udp4.srvrs, vrf.udp4);
            doSockets(true, vrf.id, vrf.vrf.udp6.getProtoNum(), vrf.vrf.udp6.srvrs, vrf.udp6);
            doSockets(false, vrf.id, vrf.vrf.tcp4.getProtoNum(), vrf.vrf.tcp4.srvrs, vrf.tcp4);
            doSockets(true, vrf.id, vrf.vrf.tcp6.getProtoNum(), vrf.vrf.tcp6.srvrs, vrf.tcp6);
        }
        for (int i = 0; i < tabLabel.labels.size(); i++) {
            doLab1(tabLabel.labels.get(i));
        }
        for (int i = labels.size() - 1; i >= 0; i--) {
            doLab2(labels.get(i));
        }
        for (int i = 0; i < tabNshEntry.services.size(); i++) {
            doNsh1(tabNshEntry.services.get(i));
        }
        for (int i = nshs.size() - 1; i >= 0; i--) {
            doNsh2(nshs.get(i));
        }
        for (int i = lower.neighs.size() - 1; i >= 0; i--) {
            doNeighs(lower.neighs.get(i));
        }
        lower.rndDoneLast = bits.getTime();
        lower.rndDoneTime = (int) (lower.rndDoneLast - tim);
        lower.rndDoneNum++;
    }

    private void doBckpln(int fwid) {
        if (fwid == lower.parid.id) {
            return;
        }
        tabRouteEntry<addrIP> oru = servStack.forwarder2route(fwid);
        servP4langIfc ifc = servP4langUtil.forwarder2iface(lower, fwid);
        servP4langNei ntry = new servP4langNei(ifc, oru.prefix.network);
        ifc.cloned = ifc;
        servP4langNei old = lower.neighs.find(ntry);
        boolean added = old == null;
        if (added) {
            old = lower.genNeighId(ntry);
            if (old == null) {
                return;
            }
        }
        old.need++;
        old.iface.viaN = old;
        oru = lower.parid.routes.find(oru);
        if (oru == null) {
            return;
        }
        if (oru.best.iface == null) {
            return;
        }
        servStackIfc bck = lower.parid.ifaces.get(oru.best.iface.ifwNum);
        if (bck == null) {
            return;
        }
        if (!bck.ready) {
            return;
        }
        ifc = lower.findIfc(bck.ifc);
        if (ifc == null) {
            return;
        }
        servP4langVrf vrf = lower.findVrf(ifc);
        if (vrf == null) {
            return;
        }
        old.vrf = vrf;
        old.viaI = ifc;
        ntry.mac = bck.lastPort.getMac();
        int outIfc = ifc.id;
        String act;
        if (added || (old.mac == null)) {
            act = "add";
        } else {
            act = "mod";
            if ((ntry.mac.compareTo(old.mac) == 0) && (old.sentIfc == outIfc)) {
                return;
            }
        }
        old.mac = ntry.mac;
        old.sentIfc = outIfc;
        lower.sendLine("neigh6_" + act + " " + old.id + " " + old.adr + " " + old.mac.toEmuStr() + " " + vrf.id + " " + ifc.getMac().toEmuStr() + " " + old.sentIfc);
        outIfc = ifc.getUcast().id;
        lower.sendLine("nhop2port_" + act + " " + old.id + " " + ifc.id + " " + outIfc);
        old.sentIgNhop = outIfc;
    }

    private void doLab4(ipFwd fwd, tabLabelEntry need, tabLabelEntry done, servP4langStrL<tabLabelEntry, tabLabelBierN> nstr, servP4langStrL<tabLabelEntry, tabLabelBierN> dstr, boolean bef) {
        if (done.bier == null) {
            done.bier = new tabLabelBier(0, 0);
        }
        if (dstr == null) {
            dstr = new servP4langStrL<tabLabelEntry, tabLabelBierN>(done);
        }
        servP4langVrf vrf = lower.findVrf(fwd);
        if (vrf == null) {
            return;
        }
        int gid = need.getHashW();
        int si = need.label - need.bier.base;
        int bits = tabLabelBier.bsl2num(need.bier.bsl);
        if (bits != 256) {
            return;
        }
        int sis = bits * si;
        byte[] ful = tabLabelBier.bsl2msk(need.bier.bsl);
        for (int i = 0; i < done.bier.peers.size(); i++) {
            tabLabelBierN ntry = done.bier.peers.get(i);
            if (need.bier.peers.find(ntry) != null) {
                continue;
            }
            servP4langNei hop = lower.findNei(ntry.iface, ntry.hop);
            if (hop == null) {
                continue;
            }
            if (hop.mac == null) {
                continue;
            }
            String a = servP4langUtil.getBierLabs(ntry, ful, sis);
            servP4langIfc ifc = hop.getVia();
            lower.sendLine("bierlabel" + fwd.ipVersion + "_del " + vrf.id + " " + gid + " " + need.label + " " + ifc.getMcast(gid, hop).id + " " + ifc.id + " " + hop.id + " " + (ntry.label + si) + a);
        }
        String act;
        for (int i = 0; i < need.bier.peers.size(); i++) {
            tabLabelBierN ntry = need.bier.peers.get(i);
            if (done.bier.peers.find(ntry) != null) {
                act = "mod";
            } else {
                act = "add";
            }
            servP4langNei hop = lower.findNei(ntry.iface, ntry.hop);
            if (hop == null) {
                continue;
            }
            if (hop.mac == null) {
                continue;
            }
            String a = servP4langUtil.getBierLabs(ntry, ful, sis);
            servP4langIfc ifc = hop.getVia();
            lower.sendLine("bierlabel" + fwd.ipVersion + "_" + act + " " + vrf.id + " " + gid + " " + need.label + " " + ifc.getMcast(gid, hop).id + " " + ifc.id + " " + hop.id + " " + (ntry.label + si) + a);
        }
        for (int i = 0; i < dstr.list.size(); i++) {
            tabLabelBierN ntry = dstr.list.get(i);
            if (nstr.list.find(ntry) != null) {
                continue;
            }
            servP4langIfc ifc = servP4langUtil.forwarder2iface(lower, servStack.addr2forwarder(ntry.hop));
            servP4langNei hop = lower.findNei(ifc, ntry.hop);
            if (hop == null) {
                continue;
            }
            ifc = hop.getVia();
            String a = servP4langUtil.getBierLabs(ntry, ful, sis);
            lower.sendLine("bierlabel" + fwd.ipVersion + "_del " + vrf.id + " " + gid + " " + need.label + " " + ifc.getMcast(gid, hop).id + " " + ifc.id + " " + hop.id + " " + need.label + a);
        }
        for (int i = 0; i < nstr.list.size(); i++) {
            tabLabelBierN ntry = nstr.list.get(i);
            if (dstr.list.find(ntry) != null) {
                act = "mod";
            } else {
                act = "add";
            }
            servP4langIfc ifc = servP4langUtil.forwarder2iface(lower, servStack.addr2forwarder(ntry.hop));
            servP4langNei hop = lower.findNei(ifc, ntry.hop);
            if (hop == null) {
                continue;
            }
            ifc = hop.getVia();
            String a = servP4langUtil.getBierLabs(ntry, ful, sis);
            lower.sendLine("bierlabel" + fwd.ipVersion + "_" + act + " " + vrf.id + " " + gid + " " + need.label + " " + ifc.getMcast(gid, hop).id + " " + ifc.id + " " + hop.id + " " + need.label + a);
        }
        if (bef) {
            act = "mod";
        } else {
            act = "add";
        }
        String a = servP4langUtil.getBierLabs(need.bier.getIdxMask(), ful, sis);
        lower.sendLine("bierlabloc" + fwd.ipVersion + "_" + act + " " + vrf.id + " " + gid + " " + need.label + a);
    }

    private void doLab3(ipFwd fwd, tabLabelEntry need, tabLabelEntry done, boolean bef) {
        if (done.duplicate == null) {
            done.duplicate = new tabGen<tabLabelDup>();
        }
        servP4langVrf vrf = lower.findVrf(fwd);
        if (vrf == null) {
            return;
        }
        int gid = need.getHashW();
        int now = 0;
        if (need.needLocal) {
            now++;
        }
        for (int i = 0; i < done.duplicate.size(); i++) {
            tabLabelDup ntry = done.duplicate.get(i);
            if (need.duplicate.find(ntry) != null) {
                continue;
            }
            servP4langNei hop = lower.findNei(ntry.iface, ntry.hop);
            if (hop == null) {
                continue;
            }
            if (hop.mac == null) {
                continue;
            }
            servP4langIfc ifc = hop.getVia();
            lower.sendLine("duplabel" + fwd.ipVersion + "_del " + vrf.id + " " + gid + " " + need.label + " " + ifc.getMcast(gid, hop).id + " " + ifc.id + " " + hop.id + " " + servP4langUtil.get1stLabel(ntry.label));
        }
        String act;
        for (int i = 0; i < need.duplicate.size(); i++) {
            tabLabelDup ntry = need.duplicate.get(i);
            if (done.duplicate.find(ntry) != null) {
                act = "mod";
            } else {
                act = "add";
            }
            servP4langNei hop = lower.findNei(ntry.iface, ntry.hop);
            if (hop == null) {
                continue;
            }
            if (hop.mac == null) {
                continue;
            }
            servP4langIfc ifc = hop.getVia();
            lower.sendLine("duplabel" + fwd.ipVersion + "_" + act + " " + vrf.id + " " + gid + " " + need.label + " " + ifc.getMcast(gid, hop).id + " " + ifc.id + " " + hop.id + " " + servP4langUtil.get1stLabel(ntry.label));
            now++;
        }
        if (bef) {
            act = "mod";
            if (now < 1) {
                act = "del";
            }
        } else {
            act = "add";
        }
        lower.sendLine("duplabloc" + fwd.ipVersion + "_" + (need.needLocal ? "add " : "del ") + vrf.id + " " + gid + " " + need.label + " " + act + " " + fwd.commonLabel.label);
    }

    private void doLab2(tabLabelEntry ntry) {
        if (tabLabel.labels.find(ntry) != null) {
            return;
        }
        labels.del(ntry);
        if (ntry.bier != null) {
            tabLabelEntry empty = new tabLabelEntry(ntry.label);
            empty.bier = new tabLabelBier(0, 0);
            servP4langStrL<tabLabelEntry, tabLabelBierN> str = new servP4langStrL<tabLabelEntry, tabLabelBierN>(empty);
            doLab4(ntry.forwarder, empty, ntry, str, labeld.find(str), true);
            labeld.del(str);
            return;
        }
        if (ntry.duplicate != null) {
            tabLabelEntry empty = new tabLabelEntry(ntry.label);
            empty.duplicate = new tabGen<tabLabelDup>();
            doLab3(ntry.forwarder, empty, ntry, true);
            return;
        }
        if (ntry.nextHop == null) {
            servP4langVrf vrf = lower.findVrf(ntry.forwarder);
            if (vrf == null) {
                return;
            }
            lower.sendLine("mylabel" + ntry.forwarder.ipVersion + "_del" + " " + ntry.label + " " + vrf.id);
            if (lower.expSrv6 == null) {
                return;
            }
            if (lower.expSrv6.addr6 == null) {
                return;
            }
            servP4langVrf vr = lower.findVrf(lower.expSrv6.vrfFor.fwd6);
            if (vr == null) {
                return;
            }
            addrIPv6 adr = lower.expSrv6.addr6.copyBytes();
            bits.msbPutD(adr.getBytes(), 12, ntry.label);
            lower.sendLine("mysrv" + ntry.forwarder.ipVersion + "_del " + vr.id + " " + adr + " " + vrf.id);
            return;
        }
        String afi;
        if (ntry.nextHop.isIPv4()) {
            afi = "4";
        } else {
            afi = "6";
        }
        servP4langNei hop = lower.findNei(ntry.iface, ntry.nextHop);
        if (hop == null) {
            lower.sendLine("cpulabel_del " + ntry.label);
            return;
        }
        int lab = servP4langUtil.get1stLabel(ntry.remoteLab);
        if (lab < 0) {
            lower.sendLine("unlabel" + afi + "_del " + ntry.label + " " + hop.id + " " + ntry.nextHop);
        } else {
            lower.sendLine("label" + afi + "_del " + ntry.label + " " + hop.id + " " + ntry.nextHop + " " + lab);
        }
    }

    private void doLab1(tabLabelEntry ntry) {
        if (ntry == null) {
            return;
        }
        ntry = ntry.copyBytes();
        if (ntry.pweIfc != null) {
            return;
        }
        if (ntry.bier != null) {
            tabLabelEntry old = labels.find(ntry);
            servP4langStrL<tabLabelEntry, tabLabelBierN> str = new servP4langStrL<tabLabelEntry, tabLabelBierN>(ntry);
            str.list = lower.parent.mergeBier(lower.parid, ntry.bier.peers, null);
            servP4langStrL<tabLabelEntry, tabLabelBierN> ostr = labeld.find(str);
            boolean bef;
            if (old != null) {
                if ((!old.differs(ntry)) && (!str.differs(ostr))) {
                    return;
                }
                bef = true;
            } else {
                old = new tabLabelEntry(ntry.label);
                bef = false;
            }
            labels.put(ntry);
            labeld.put(str);
            doLab4(ntry.forwarder, ntry, old, str, ostr, bef);
            return;
        }
        if (ntry.duplicate != null) {
            tabLabelEntry old = labels.find(ntry);
            boolean bef;
            if (old != null) {
                if (!old.differs(ntry)) {
                    return;
                }
                bef = true;
            } else {
                old = new tabLabelEntry(ntry.label);
                bef = false;
            }
            labels.put(ntry);
            doLab3(ntry.forwarder, ntry, old, bef);
            return;
        }
        if (ntry.nextHop == null) {
            servP4langVrf vrf = lower.findVrf(ntry.forwarder);
            if (vrf == null) {
                return;
            }
            tabLabelEntry old = labels.find(ntry);
            String act = "add";
            if (old != null) {
                if (!old.differs(ntry)) {
                    return;
                }
                act = "mod";
            }
            labels.put(ntry);
            lower.sendLine("mylabel" + ntry.forwarder.ipVersion + "_" + act + " " + ntry.label + " " + vrf.id);
            if (lower.expSrv6 == null) {
                return;
            }
            if (lower.expSrv6.addr6 == null) {
                return;
            }
            servP4langVrf vr = lower.findVrf(lower.expSrv6.vrfFor.fwd6);
            if (vr == null) {
                return;
            }
            addrIPv6 adr = lower.expSrv6.addr6.copyBytes();
            bits.msbPutD(adr.getBytes(), 12, ntry.label);
            lower.sendLine("mysrv" + ntry.forwarder.ipVersion + "_" + act + " " + vr.id + " " + adr + " " + vrf.id);
            return;
        }
        if (ntry.forwarder != null) {
            tabRouteEntry<addrIP> rou = ntry.forwarder.actualU.route(ntry.nextHop);
            rou = lower.convRou(rou, false);
            if (rou != null) {
                if (rou.best.attribAs == ipMpls.typeU) {
                    ntry.iface = (ipFwdIface) rou.best.iface;
                    ntry.nextHop = rou.best.nextHop;
                    ntry.remoteLab = rou.best.labelRem;
                }
            }
        }
        tabLabelEntry old = labels.find(ntry);
        String act = "add";
        if (old != null) {
            if (!old.differs(ntry)) {
                return;
            }
            act = "mod";
        }
        servP4langNei hop = lower.findNei(ntry.iface, ntry.nextHop);
        if (hop == null) {
            servStackFwd oth = lower.parent.findIfc(lower.parid, ntry.iface);
            if (oth == null) {
                return;
            }
            addrIP adr = servStack.forwarder2addr(oth.id);
            servP4langIfc ifc = servP4langUtil.forwarder2iface(lower, oth.id);
            hop = lower.findNei(ifc, adr);
            if (hop == null) {
                return;
            }
            labels.put(ntry);
            lower.sendLine("label4_" + act + " " + ntry.label + " " + hop.id + " " + ntry.nextHop + " " + ntry.label);
            return;
        }
        String afi;
        if (ntry.nextHop.isIPv4()) {
            afi = "4";
        } else {
            afi = "6";
        }
        labels.put(ntry);
        int lab = servP4langUtil.get1stLabel(ntry.remoteLab);
        if (lab < 0) {
            lower.sendLine("unlabel" + afi + "_" + act + " " + ntry.label + " " + hop.id + " " + ntry.nextHop);
            return;
        }
        int lab2 = servP4langUtil.get2ndLabel(ntry.remoteLab);
        if (lab2 < 0) {
            lower.sendLine("label" + afi + "_" + act + " " + ntry.label + " " + hop.id + " " + ntry.nextHop + " " + lab);
            return;
        }
        lower.sendLine("vpnlabel" + afi + "_" + act + " " + ntry.label + " " + hop.id + " " + ntry.nextHop + " " + lab + " " + lab2);
    }

    private void doLab0(tabLabelEntry ntry, int fwid) {
        if (ntry == null) {
            return;
        }
        ntry = ntry.copyBytes();
        tabLabelEntry old = labels.find(ntry);
        String act = "add";
        if (old != null) {
            if (!old.differs(ntry)) {
                return;
            }
            act = "mod";
        }
        if (fwid == lower.parid.id) {
            labels.put(ntry);
            lower.sendLine("mylabel4_" + act + " " + ntry.label + " 0");
            return;
        }
        addrIP adr = servStack.forwarder2addr(fwid);
        servP4langIfc ifc = servP4langUtil.forwarder2iface(lower, fwid);
        servP4langNei hop = lower.findNei(ifc, adr);
        if (hop == null) {
            return;
        }
        labels.put(ntry);
        lower.sendLine("label4_" + act + " " + ntry.label + " " + hop.id + " " + ntry.nextHop + " " + ntry.label);
    }

    private String doNsh3(tabNshEntry ntry, String act) {
        if (ntry.iface != null) {
            servP4langIfc ifc = lower.findIfc(ntry.iface);
            if (ifc == null) {
                servStackFwd oth = lower.parent.findIfc(lower.parid, ntry.iface);
                if (oth == null) {
                    return null;
                }
                addrIP adr = servStack.forwarder2addr(oth.id);
                ifc = servP4langUtil.forwarder2iface(lower, oth.id);
                servP4langNei hop = lower.findNei(ifc, adr);
                if (hop == null) {
                    return null;
                }
                return "nshnei_" + act + " " + ntry.sp + " " + ntry.si + " " + hop.id + " " + ntry.sp + " " + ntry.si;
            }
            if (ifc.viaN != null) {
                return "nshnei_" + act + " " + ntry.sp + " " + ntry.si + " " + ifc.viaN.id + " " + ntry.trgSp + " " + ntry.trgSi;
            }
            return "nshifc_" + act + " " + ntry.sp + " " + ntry.si + " " + ifc.id + " " + ifc.getMac().toEmuStr() + " " + ntry.target.toEmuStr() + " " + ntry.trgSp + " " + ntry.trgSi;
        }
        if (ntry.route4 != null) {
            servP4langVrf vrf = lower.findVrf(ntry.route4);
            if (vrf == null) {
                return null;
            }
            return "nshloc_" + act + " " + ntry.sp + " " + ntry.si + " " + vrf.id + "";
        }
        return null;
    }

    private void doNsh2(tabNshEntry ntry) {
        if (tabNshEntry.services.find(ntry) != null) {
            return;
        }
        nshs.del(ntry);
        String act = doNsh3(ntry, "del");
        if (act == null) {
            return;
        }
        lower.sendLine(act);
    }

    private void doNsh1(tabNshEntry ntry) {
        if (ntry == null) {
            return;
        }
        ntry = ntry.copyBytes();
        tabNshEntry old = nshs.find(ntry);
        String act = "add";
        if (old != null) {
            if (!old.differs(ntry)) {
                return;
            }
            act = "mod";
        }
        act = doNsh3(ntry, act);
        if (act == null) {
            return;
        }
        nshs.put(ntry);
        lower.sendLine(act);
    }

    private void addDynBr(servP4langBr br, ifcBridgeIfc ntry, ifcDn ifc) {
        int id = lower.getNextDynIfc();
        if (id < 0) {
            return;
        }
        servP4langIfc res = new servP4langIfc(lower, id);
        res.brif = ntry;
        res.dynamic = true;
        res.hidden = true;
        res.doClear();
        lower.expIfc.put(res);
        br.ifcs.put(ntry);
        lower.sendLine("portbridge_add " + id + " " + br.br.number);
    }

    private void doBrLab(String act, servP4langBr br, int l) {
        lower.sendLine("bridgelabel_" + act + " " + br.br.number + " " + l);
        if (lower.expSrv6 == null) {
            return;
        }
        if (lower.expSrv6.addr6 == null) {
            return;
        }
        servP4langVrf vr = lower.findVrf(lower.expSrv6.vrfFor.fwd6);
        if (vr == null) {
            return;
        }
        addrIPv6 adr = lower.expSrv6.addr6.copyBytes();
        bits.msbPutD(adr.getBytes(), 12, l);
        lower.sendLine("bridgesrv_" + act + " " + br.br.number + " " + vr.id + " " + adr);
    }

    private void doBrdg(servP4langBr br) {
        br.routed = lower.findIfc(br.br);
        if (br.routed != null) {
            servP4langVrf vrf = lower.findVrf(br.routed);
            if (vrf == null) {
                return;
            }
            if (!br.sentLab) {
                lower.sendLine("pwhelab_add " + br.br.bridgeHed.label.label + " " + br.routed.id);
                br.sentLab = true;
            }
            tabGen<ifcBridgeIfc> seenI = new tabGen<ifcBridgeIfc>();
            for (int i = 0;; i++) {
                ifcBridgeIfc ntry = br.br.bridgeHed.getIface(i);
                if (ntry == null) {
                    break;
                }
                seenI.put(ntry);
                if (br.ifcs.find(ntry) != null) {
                    continue;
                }
                int l = -1;
                try {
                    clntMplsPwe ifc = (clntMplsPwe) ntry.lowerIf;
                    if (ifc.getLabelRem() < 0) {
                        continue;
                    }
                    l = ifc.getLabelLoc();
                } catch (Exception e) {
                }
                try {
                    rtrBgpEvpnPeer ifc = (rtrBgpEvpnPeer) ntry.lowerIf;
                    if (ifc.getLabelRem() < 0) {
                        continue;
                    }
                    l = ifc.getLabelLoc();
                    if (br.findIfc(l)) {
                        continue;
                    }
                    br.ifcs.put(ntry);
                    br.labs.put(new servP4langBrLab(ntry, l));
                    lower.sendLine("pwhelab_add " + l + " " + br.routed.id);
                    continue;
                } catch (Exception e) {
                }
                if (l < 1) {
                    continue;
                }
                int id = lower.getNextDynIfc();
                if (id < 0) {
                    continue;
                }
                servP4langIfc res = new servP4langIfc(lower, id);
                res.brif = ntry;
                res.dynamic = true;
                res.hidden = true;
                res.doClear();
                lower.expIfc.put(res);
                br.ifcs.put(ntry);
                br.labs.put(new servP4langBrLab(ntry, l));
                lower.sendLine("pwhelab_add " + l + " " + id);
                lower.sendLine("portvrf_add " + id + " " + vrf.id);
            }
            for (int i = br.ifcs.size() - 1; i >= 0; i--) {
                ifcBridgeIfc ntry = br.ifcs.get(i);
                if (seenI.find(ntry) != null) {
                    continue;
                }
                br.ifcs.del(ntry);
                servP4langBrLab lab = br.labs.del(new servP4langBrLab(ntry, 0));
                if (lab == null) {
                    continue;
                }
                servP4langIfc brif = lower.findDynBr(ntry);
                if (brif == null) {
                    continue;
                }
                lower.sendLine("pwhelab_del " + lab.lab + " " + brif.id);
                lower.sendLine("portvrf_del " + brif.id + " " + vrf.id);
                lower.expIfc.del(brif);
            }
            return;
        }
        if (!br.sentLab) {
            doBrLab("add", br, br.br.bridgeHed.label.label);
            br.sentLab = true;
        }
        tabGen<ifcBridgeIfc> seenI = new tabGen<ifcBridgeIfc>();
        for (int i = 0;; i++) {
            ifcBridgeIfc ntry = br.br.bridgeHed.getIface(i);
            if (ntry == null) {
                break;
            }
            seenI.put(ntry);
            int l = -1;
            try {
                clntMplsPwe ifc = (clntMplsPwe) ntry.lowerIf;
                if (ifc.getLabelRem() < 0) {
                    continue;
                }
                l = ifc.getLabelLoc();
            } catch (Exception e) {
            }
            try {
                rtrBgpEvpnPeer ifc = (rtrBgpEvpnPeer) ntry.lowerIf;
                if (ifc.getLabelRem() < 0) {
                    continue;
                }
                l = ifc.getLabelLoc();
                if (br.findIfc(l)) {
                    continue;
                }
            } catch (Exception e) {
            }
            if (br.ifcs.find(ntry) != null) {
                continue;
            }
            try {
                clntVxlan ifc = (clntVxlan) ntry.lowerIf;
                addDynBr(br, ntry, ifc);
                continue;
            } catch (Exception e) {
            }
            try {
                servVxlanConn ifc = (servVxlanConn) ntry.lowerIf;
                addDynBr(br, ntry, ifc);
                continue;
            } catch (Exception e) {
            }
            try {
                clntEtherIp ifc = (clntEtherIp) ntry.lowerIf;
                addDynBr(br, ntry, ifc);
                continue;
            } catch (Exception e) {
            }
            try {
                clntEoIp ifc = (clntEoIp) ntry.lowerIf;
                addDynBr(br, ntry, ifc);
                continue;
            } catch (Exception e) {
            }
            try {
                clntPckOudp ifc = (clntPckOudp) ntry.lowerIf;
                addDynBr(br, ntry, ifc);
                continue;
            } catch (Exception e) {
            }
            try {
                servPckOudpConn ifc = (servPckOudpConn) ntry.lowerIf;
                addDynBr(br, ntry, ifc);
                continue;
            } catch (Exception e) {
            }
            if (l < 1) {
                continue;
            }
            br.ifcs.put(ntry);
            br.labs.put(new servP4langBrLab(ntry, l));
            doBrLab("add", br, l);
            continue;
        }
        for (int i = br.ifcs.size() - 1; i >= 0; i--) {
            ifcBridgeIfc ntry = br.ifcs.get(i);
            if (seenI.find(ntry) != null) {
                continue;
            }
            br.ifcs.del(ntry);
            servP4langBrLab lab = br.labs.del(new servP4langBrLab(ntry, 0));
            if (lab != null) {
                doBrLab("del", br, lab.lab);
            }
            servP4langIfc brif = lower.findDynBr(ntry);
            if (brif == null) {
                continue;
            }
            lower.sendLine("portbridge_del " + brif.id + " " + br.br.number);
            lower.expIfc.del(brif);
            String s = brif.sentBrTun;
            if (s == null) {
                continue;
            }
            cmds cmd = new cmds("lin", s);
            s = cmd.word() + " ";
            s = servP4langUtil.negateOneCommand(s);
            s += cmd.word();
            cmd.word();
            lower.sendLine(s + " " + addrMac.getRandom().toEmuStr() + " " + cmd.getRemaining());
        }
        tabGen<ifcBridgeAdr> seenM = new tabGen<ifcBridgeAdr>();
        for (int i = 0;; i++) {
            ifcBridgeAdr ntry = br.br.bridgeHed.getMacAddr(i);
            if (ntry == null) {
                break;
            }
            ntry = ntry.copyBytes();
            seenM.put(ntry);
            ifcBridgeAdr old = br.macs.find(ntry);
            String a = "add";
            if (old != null) {
                if (old.ifc == ntry.ifc) {
                    continue;
                }
                a = "mod";
            }
            br.macs.put(ntry);
            servP4langIfc ifc = lower.findIfc(ntry.ifc);
            if (ifc != null) {
                if ((ifc.ifc.type != tabRouteIface.ifaceType.dialer) && (ifc.ifc.type != tabRouteIface.ifaceType.tunnel) && (ifc.ifc.type != tabRouteIface.ifaceType.virtppp)) {
                    lower.sendLine("bridgemac_" + a + " " + br.br.number + " " + ntry.adr.toEmuStr() + " " + ifc.id);
                    continue;
                }
                servP4langNei nei = lower.findNei(ifc.ifc.fwdIf4, new addrIP());
                if (nei == null) {
                    nei = lower.findNei(ifc.ifc.fwdIf6, new addrIP());
                }
                if (nei == null) {
                    br.macs.del(ntry);
                    continue;
                }
                String p = "0";
                if ((ifc.ifc.type == tabRouteIface.ifaceType.dialer) || (ifc.ifc.type == tabRouteIface.ifaceType.virtppp)) {
                    p = "1";
                }
                lower.sendLine("routedmac_" + a + " " + br.br.number + " " + ntry.adr.toEmuStr() + " " + nei.id + " " + p);
                continue;
            }
            if (ntry.ifc.lowerIf == null) {
                servStackFwd oth = lower.parent.findIfc(lower.parid, br.br);
                if (oth == null) {
                    br.macs.del(ntry);
                    continue;
                }
                addrIP adr = servStack.forwarder2addr(oth.id);
                ifc = servP4langUtil.forwarder2iface(lower, oth.id);
                servP4langNei hop = lower.findNei(ifc, adr);
                if (hop == null) {
                    br.macs.del(ntry);
                    continue;
                }
                lower.sendLine("bridgevpls_" + a + " " + br.br.number + " " + ntry.adr.toEmuStr() + " " + adr + " " + hop.id + " " + lower.parent.bckplnLab[oth.id] + " " + br.br.bridgeHed.label.label);
                continue;
            }
            servStackFwd oth = lower.parent.findIfc(lower.parid, ntry.ifc);
            if (oth != null) {
                addrIP adr = servStack.forwarder2addr(oth.id);
                ifc = servP4langUtil.forwarder2iface(lower, oth.id);
                servP4langNei hop = lower.findNei(ifc, adr);
                if (hop == null) {
                    br.macs.del(ntry);
                    continue;
                }
                lower.sendLine("bridgevpls_" + a + " " + br.br.number + " " + ntry.adr.toEmuStr() + " " + adr + " " + hop.id + " " + lower.parent.bckplnLab[oth.id] + " " + br.br.bridgeHed.label.label);
                continue;
            }
            addrIP adr = null;
            try {
                clntVxlan iface = (clntVxlan) ntry.ifc.lowerIf;
                servP4langIfc brif = lower.findDynBr(ntry.ifc);
                if (brif == null) {
                    continue;
                }
                adr = iface.getRemAddr();
                if (adr == null) {
                    continue;
                }
                addrIP src = iface.getLocAddr();
                if (src == null) {
                    continue;
                }
                ipFwd ofwd = iface.vrf.getFwd(adr);
                servP4langVrf ovrf = lower.findVrf(ofwd);
                if (ovrf == null) {
                    continue;
                }
                servP4langNei hop = lower.findHop(ofwd, adr);
                if (hop == null) {
                    br.macs.del(ntry);
                    continue;
                }
                a = "bridgevxlan" + (adr.isIPv4() ? "4" : "6") + "_" + a + " " + br.br.number + " " + ntry.adr.toEmuStr() + " " + src + " " + adr + " " + hop.id + " " + iface.inst + " " + ovrf.id + " " + brif.id + " " + iface.getLocPort() + " " + iface.getRemPort();
                brif.sentBrTun = a;
                lower.sendLine(a);
                continue;
            } catch (Exception e) {
            }
            try {
                servVxlanConn iface = (servVxlanConn) ntry.ifc.lowerIf;
                servP4langIfc brif = lower.findDynBr(ntry.ifc);
                if (brif == null) {
                    continue;
                }
                adr = iface.getRemAddr();
                if (adr == null) {
                    continue;
                }
                addrIP src = iface.getLocAddr();
                if (src == null) {
                    continue;
                }
                ipFwd ofwd = iface.getFwder();
                servP4langVrf ovrf = lower.findVrf(ofwd);
                if (ovrf == null) {
                    continue;
                }
                servP4langNei hop = lower.findHop(ofwd, adr);
                if (hop == null) {
                    br.macs.del(ntry);
                    continue;
                }
                a = "bridgevxlan" + (adr.isIPv4() ? "4" : "6") + "_" + a + " " + br.br.number + " " + ntry.adr.toEmuStr() + " " + src + " " + adr + " " + hop.id + " " + iface.getInst() + " " + ovrf.id + " " + brif.id + " " + iface.getLocPort() + " " + iface.getRemPort();
                brif.sentBrTun = a;
                lower.sendLine(a);
                continue;
            } catch (Exception e) {
            }
            try {
                clntEtherIp iface = (clntEtherIp) ntry.ifc.lowerIf;
                servP4langIfc brif = lower.findDynBr(ntry.ifc);
                if (brif == null) {
                    continue;
                }
                adr = iface.getRemAddr();
                if (adr == null) {
                    continue;
                }
                addrIP src = iface.getLocAddr();
                if (src == null) {
                    continue;
                }
                ipFwd ofwd = iface.getFwder();
                servP4langVrf ovrf = lower.findVrf(ofwd);
                if (ovrf == null) {
                    continue;
                }
                servP4langNei hop = lower.findHop(ofwd, adr);
                if (hop == null) {
                    br.macs.del(ntry);
                    continue;
                }
                a = "bridgeetherip" + (adr.isIPv4() ? "4" : "6") + "_" + a + " " + br.br.number + " " + ntry.adr.toEmuStr() + " " + src + " " + adr + " " + hop.id + " " + ovrf.id + " " + brif.id;
                brif.sentBrTun = a;
                lower.sendLine(a);
                continue;
            } catch (Exception e) {
            }
            try {
                clntEoIp iface = (clntEoIp) ntry.ifc.lowerIf;
                servP4langIfc brif = lower.findDynBr(ntry.ifc);
                if (brif == null) {
                    continue;
                }
                adr = iface.getRemAddr();
                if (adr == null) {
                    continue;
                }
                addrIP src = iface.getLocAddr();
                if (src == null) {
                    continue;
                }
                ipFwd ofwd = iface.getFwder();
                servP4langVrf ovrf = lower.findVrf(ofwd);
                if (ovrf == null) {
                    continue;
                }
                servP4langNei hop = lower.findHop(ofwd, adr);
                if (hop == null) {
                    br.macs.del(ntry);
                    continue;
                }
                a = "bridgeeoip" + (adr.isIPv4() ? "4" : "6") + "_" + a + " " + br.br.number + " " + ntry.adr.toEmuStr() + " " + src + " " + adr + " " + hop.id + " " + iface.tunId + " " + ovrf.id + " " + brif.id;
                brif.sentBrTun = a;
                lower.sendLine(a);
                continue;
            } catch (Exception e) {
            }
            try {
                clntPckOudp iface = (clntPckOudp) ntry.ifc.lowerIf;
                servP4langIfc brif = lower.findDynBr(ntry.ifc);
                if (brif == null) {
                    continue;
                }
                adr = iface.getRemAddr();
                if (adr == null) {
                    continue;
                }
                addrIP src = iface.getLocAddr();
                if (src == null) {
                    continue;
                }
                ipFwd ofwd = iface.vrf.getFwd(adr);
                servP4langVrf ovrf = lower.findVrf(ofwd);
                if (ovrf == null) {
                    continue;
                }
                servP4langNei hop = lower.findHop(ofwd, adr);
                if (hop == null) {
                    br.macs.del(ntry);
                    continue;
                }
                a = "bridgepckoudp" + (adr.isIPv4() ? "4" : "6") + "_" + a + " " + br.br.number + " " + ntry.adr.toEmuStr() + " " + src + " " + adr + " " + iface.getLocPort() + " " + iface.getRemPort() + " " + hop.id + " " + ovrf.id + " " + brif.id;
                brif.sentBrTun = a;
                lower.sendLine(a);
                continue;
            } catch (Exception e) {
            }
            try {
                servPckOudpConn iface = (servPckOudpConn) ntry.ifc.lowerIf;
                servP4langIfc brif = lower.findDynBr(ntry.ifc);
                if (brif == null) {
                    continue;
                }
                adr = iface.getRemAddr();
                if (adr == null) {
                    continue;
                }
                addrIP src = iface.getLocAddr();
                if (src == null) {
                    continue;
                }
                ipFwd ofwd = iface.getFwder();
                servP4langVrf ovrf = lower.findVrf(ofwd);
                if (ovrf == null) {
                    continue;
                }
                servP4langNei hop = lower.findHop(ofwd, adr);
                if (hop == null) {
                    br.macs.del(ntry);
                    continue;
                }
                a = "bridgepckoudp" + (adr.isIPv4() ? "4" : "6") + "_" + a + " " + br.br.number + " " + ntry.adr.toEmuStr() + " " + src + " " + adr + " " + iface.getLocPort() + " " + iface.getRemPort() + " " + hop.id + " " + ovrf.id + " " + brif.id;
                brif.sentBrTun = a;
                lower.sendLine(a);
                continue;
            } catch (Exception e) {
            }
            int l = -1;
            addrIP srv = null;
            tabRouteEntry<addrIP> rou = null;
            try {
                clntMplsPwe iface = (clntMplsPwe) ntry.ifc.lowerIf;
                l = iface.getLabelRem();
                adr = iface.getRemote();
                if (adr == null) {
                    continue;
                }
                rou = iface.vrf.getFwd(adr).actualU.route(adr);
            } catch (Exception e) {
            }
            try {
                rtrBgpEvpnPeer iface = (rtrBgpEvpnPeer) ntry.ifc.lowerIf;
                l = iface.getLabelRem();
                adr = iface.getRemote();
                srv = iface.getSrvRem();
                if (srv == null) {
                    rou = iface.getForwarder().actualU.route(adr);
                } else {
                    rou = iface.getForwarder().actualU.route(srv);
                }
            } catch (Exception e) {
            }
            if (l < 1) {
                br.macs.del(ntry);
                continue;
            }
            rou = lower.convRou(rou, false);
            if (rou == null) {
                br.macs.del(ntry);
                continue;
            }
            servP4langNei hop = lower.findNei(rou.best.iface, rou.best.nextHop);
            if (hop == null) {
                oth = lower.parent.findIfc(lower.parid, rou.best.iface);
                if (oth == null) {
                    br.macs.del(ntry);
                    continue;
                }
                adr = servStack.forwarder2addr(oth.id);
                ifc = servP4langUtil.forwarder2iface(lower, oth.id);
                hop = lower.findNei(ifc, adr);
                if (hop == null) {
                    br.macs.del(ntry);
                    continue;
                }
                lower.sendLine("bridgevpls_" + a + " " + br.br.number + " " + ntry.adr.toEmuStr() + " " + adr + " " + hop.id + " " + lower.parent.bckplnLab[oth.id] + " " + br.br.bridgeHed.label.label);
                continue;
            }
            if (srv == null) {
                lower.sendLine("bridgevpls_" + a + " " + br.br.number + " " + ntry.adr.toEmuStr() + " " + adr + " " + hop.id + " " + servP4langUtil.getLabel(rou) + " " + l);
            } else {
                lower.sendLine("bridgesrv6_" + a + " " + br.br.number + " " + ntry.adr.toEmuStr() + " " + adr + " " + hop.id + " " + srv);
            }
        }
        for (int i = br.macs.size() - 1; i >= 0; i--) {
            ifcBridgeAdr ntry = br.macs.get(i);
            if (ntry == null) {
                continue;
            }
            if (seenM.find(ntry) != null) {
                continue;
            }
            br.macs.del(ntry);
            servP4langIfc ifc = lower.findIfc(ntry.ifc);
            if (ifc != null) {
                lower.sendLine("bridgemac_del " + br.br.number + " " + ntry.adr.toEmuStr() + " " + ifc.id);
                continue;
            }
        }
    }

    private void doVrf(servP4langVrf vrf) {
        int i = 0;
        if (vrf.vrf.fwd4.netflow != null) {
            i = vrf.vrf.fwd4.netflow.session.maxSamp;
        }
        if (vrf.sentSample4 != i) {
            String a;
            if (vrf.sentSample4 < 1) {
                a = "add";
            } else {
                a = "mod";
            }
            if (i < 1) {
                a = "del";
            }
            lower.sendLine("sampler4_" + a + " " + vrf.id + " " + i);
        }
        vrf.sentSample4 = i;
        i = 0;
        if (vrf.vrf.fwd6.netflow != null) {
            i = vrf.vrf.fwd6.netflow.session.maxSamp;
        }
        if (vrf.sentSample6 != i) {
            String a;
            if (vrf.sentSample6 < 1) {
                a = "add";
            } else {
                a = "mod";
            }
            if (i < 1) {
                a = "del";
            }
            lower.sendLine("sampler6_" + a + " " + vrf.id + " " + i);
        }
        vrf.sentSample6 = i;
        if (vrf.sentMcast) {
            return;
        }
        vrf.sentMcast = true;
        lower.sendLine("polkaown_add 0 " + vrf.id);
        lower.sendLine("myaddr4_add 224.0.0.0/4 -1 " + vrf.id);
        lower.sendLine("myaddr4_add 255.255.255.255/32 -1 " + vrf.id);
        lower.sendLine("myaddr6_add ff00::/8 -1 " + vrf.id);
    }

    private void doDynAcc() {
        for (int i = 0; i < cfgAll.ifaces.size(); i++) {
            cfgIfc ifc = cfgAll.ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            if (ifc.vlanNum != 0) {
                servP4langIfc old = lower.findIfc(ifc.ethtyp);
                if (old != null) {
                    continue;
                }
                old = lower.findIfc(ifc.parent);
                if (old == null) {
                    continue;
                }
                int id = lower.getNextDynIfc();
                if (id < 0) {
                    continue;
                }
                servP4langIfc ntry = new servP4langIfc(lower, id);
                ntry.ifc = ifc;
                ntry.dynamic = true;
                ntry.hidden = true;
                ntry.master = old;
                ntry.doClear();
                ifc.ethtyp.hwHstry = new history();
                ifc.ethtyp.hwCntr = new counter();
                lower.expIfc.put(ntry);
                continue;
            }
            if (ifc.cloned == null) {
                continue;
            }
            servP4langIfc ntry = lower.findIfc(ifc);
            if (ntry != null) {
                doIface(ntry);
                doNeighs(true, ntry, ntry.ifc.ipIf4);
                doNeighs(false, ntry, ntry.ifc.ipIf6);
                continue;
            }
            servP4langIfc prnt = lower.findIfc(ifc.cloned);
            if (prnt == null) {
                continue;
            }
            int id = lower.getNextDynIfc();
            if (id < 0) {
                continue;
            }
            ntry = new servP4langIfc(lower, id);
            ntry.doClear();
            ntry.ifc = ifc;
            ntry.dynamic = true;
            ntry.hidden = true;
            ntry.cloned = prnt;
            ntry.ifc.ethtyp.hwHstry = new history();
            ntry.ifc.ethtyp.hwCntr = new counter();
            lower.expIfc.put(ntry);
            continue;
        }
    }

    private boolean doIface(servP4langIfc ifc) {
        if (cfgAll.ifaces.find(ifc.ifc) == null) {
            return true;
        }
        ifc.setup2apiPack();
        int i = -1;
        int o = -1;
        if (ifc.ifc.ethtyp.rateIn != null) {
            i = ifc.ifc.ethtyp.rateIn.getBytePerInt();
            o = ifc.ifc.ethtyp.rateIn.getInterval();
        }
        if ((i != ifc.sentRatBin) || (o != ifc.sentRatTin)) {
            String a;
            if (ifc.sentRatBin < 0) {
                a = "add";
            } else {
                a = "mod";
            }
            if (i < 0) {
                a = "del";
            }
            lower.sendLine("ratein_" + a + " " + ifc.id + " " + i + " " + o);
            ifc.sentRatBin = i;
            ifc.sentRatTin = o;
        }
        i = -1;
        o = -1;
        if (ifc.ifc.ethtyp.rateOut != null) {
            i = ifc.ifc.ethtyp.rateOut.getBytePerInt();
            o = ifc.ifc.ethtyp.rateOut.getInterval();
        }
        if ((i != ifc.sentRatBout) || (o != ifc.sentRatTout)) {
            String a;
            if (ifc.sentRatBout < 0) {
                a = "add";
            } else {
                a = "mod";
            }
            if (i < 0) {
                a = "del";
            }
            lower.sendLine("rateout_" + a + " " + ifc.id + " " + i + " " + o);
            ifc.sentRatBout = i;
            ifc.sentRatTout = o;
        }
        i = -1;
        o = -1;
        if (ifc.ifc.ethtyp.monSes != null) {
            servP4langIfc res = lower.findIfc(ifc.ifc.ethtyp.monSes);
            if (res != null) {
                i = res.id;
                o = res.getUcast().id;
            }
        }
        if (i != ifc.sentMon) {
            int p = i;
            String a;
            if (ifc.sentMon < 0) {
                a = "add";
            } else {
                a = "mod";
            }
            if (i < 0) {
                p = ifc.sentMon;
                a = "del";
            }
            int smp = ifc.ifc.ethtyp.monSmpN;
            if (smp < 1) {
                smp = 1;
            }
            int trn = ifc.ifc.ethtyp.monTrnc;
            if (trn < 1) {
                trn = ifc.ifc.ethtyp.getMTUsize();
            }
            lower.sendLine("monitor_" + a + " " + ifc.id + " " + p + " " + o + " " + ifc.ifc.ethtyp.monDir + " " + smp + " " + trn);
            ifc.sentMon = i;
        }
        sendQos("in", true, "4", ifc.id, ifc.ifc.ethtyp.qosIn, ifc.sentQos4in, ifc.sentQos4inF, ifc.sentQos4inB, ifc.sentQos4inI);
        sendQos("in", false, "6", ifc.id, ifc.ifc.ethtyp.qosIn, ifc.sentQos6in, ifc.sentQos6inF, ifc.sentQos6inB, ifc.sentQos6inI);
        sendQos("out", true, "4", ifc.id, ifc.ifc.ethtyp.qosOut, ifc.sentQos4out, ifc.sentQos4outF, ifc.sentQos4outB, ifc.sentQos4outI);
        sendQos("out", false, "6", ifc.id, ifc.ifc.ethtyp.qosOut, ifc.sentQos6out, ifc.sentQos6outF, ifc.sentQos6outB, ifc.sentQos6outI);
        if (ifc.ifc.ethtyp.macSec == null) {
            if (ifc.sentMacsec != null) {
                lower.sendLine("macsec_del " + ifc.id + " " + ifc.sentMacsec);
            }
            ifc.sentMacsec = null;
        } else {
            String a = null;
            if (ifc.ifc.ethtyp.macSec.keyHashRx != null) {
                a = ifc.ifc.ethtyp.macSec.myTyp + " " + ifc.ifc.ethtyp.macSec.cphrSiz + " " + ifc.ifc.ethtyp.macSec.hashSiz + " " + ifc.ifc.ethtyp.macSec.tagSiz + " " + ifc.ifc.ethtyp.macSec.getModeFlags() + " " + ifc.ifc.ethtyp.macSec.profil.trans.encr2str() + " " + ifc.ifc.ethtyp.macSec.profil.trans.hash2str() + " " + bits.toHex(ifc.ifc.ethtyp.macSec.keyEncrRx) + " " + bits.toHex(ifc.ifc.ethtyp.macSec.keyEncrTx) + " " + bits.toHex(ifc.ifc.ethtyp.macSec.keyIvRx) + " " + bits.toHex(ifc.ifc.ethtyp.macSec.keyIvTx) + " " + bits.toHex(ifc.ifc.ethtyp.macSec.keyHashRx) + " " + bits.toHex(ifc.ifc.ethtyp.macSec.keyHashTx);
            }
            if (a != null) {
                if (ifc.sentMacsec != null) {
                    if (!a.equals(ifc.sentMacsec)) {
                        lower.sendLine("macsec_mod " + ifc.id + " " + a);
                    }
                } else {
                    lower.sendLine("macsec_add " + ifc.id + " " + a);
                }
            } else {
                if (ifc.sentMacsec != null) {
                    lower.sendLine("macsec_del " + ifc.id + " " + ifc.sentMacsec);
                }
            }
            ifc.sentMacsec = a;
        }
        i = 0;
        if (ifc.ifc.ethtyp.getSgt()) {
            i = 1;
            if ((ifc.ifc.ethtyp.macSec != null) && (ifc.sentMacsec == null)) {
                i = 0;
            }
        }
        if (ifc.sentSgtTag != i) {
            String a;
            if (i == 0) {
                a = "del";
            } else {
                a = "add";
            }
            lower.sendLine("sgttag_" + a + " " + ifc.id + " " + i);
            ifc.sentSgtTag = i;
        }
        i = ifc.ifc.ethtyp.sgtSet;
        if (ifc.sentSgtSet != i) {
            String a;
            if (ifc.sentSgtSet >= 0) {
                a = "mod";
            } else {
                a = "add";
            }
            if (i < 0) {
                a = "del";
            }
            lower.sendLine("sgtset_" + a + " " + ifc.id + " " + i);
            ifc.sentSgtSet = i;
        }
        if (ifc.ifc.pppoeC != null) {
            servP4langIfc res = lower.findIfc(ifc.ifc.pppoeC.clnIfc);
            if (res != null) {
                res.pppoe = ifc;
            }
        }
        if (ifc.ifc.pppoeR != null) {
            servP4langIfc res = lower.findIfc(ifc.ifc.pppoeR.clnIfc);
            if (res != null) {
                res.pppoe = ifc;
            }
        }
        state.states sta;
        if (ifc.ifc.ethtyp.forcedDN != 0) {
            sta = state.states.admin;
        } else {
            sta = state.states.up;
        }
        i = ifc.ifc.ethtyp.getMTUsize();
        if (ifc.suppressState()) {
            ifc.sentState = sta;
            ifc.sentMtu = i;
        }
        if (ifc.sentState != sta) {
            String a;
            if (sta == state.states.up) {
                a = "1";
            } else {
                a = "0";
            }
            lower.sendLine("state " + ifc.id + " " + a + " " + ifc.getStateEnding());
            ifc.sentState = sta;
        }
        if (ifc.sentMtu != i) {
            lower.sendLine("mtu " + ifc.id + " " + i);
            ifc.sentMtu = i;
        }
        if ((ifc.master != null) && (ifc.sentVlan == 0)) {
            if (ifc.master.master == null) {
                lower.sendLine("portvlan_add " + ifc.id + " " + ifc.master.id + " " + ifc.ifc.vlanNum);
            } else {
                lower.sendLine("portqinq_add " + ifc.id + " " + ifc.master.master.id + " " + ifc.master.id + " " + ifc.master.ifc.vlanNum + " " + ifc.ifc.vlanNum);
            }
            ifc.sentVlan = ifc.ifc.vlanNum;
        }
        if (ifc.ifc.hairpinHed != null) {
            o = 0;
            for (i = 0; i < lower.expIfc.size(); i++) {
                servP4langIfc ntry = lower.expIfc.get(i);
                if (ntry == ifc) {
                    continue;
                }
                if (ntry.ifc == null) {
                    continue;
                }
                if (ntry.ifc.hairpinHed != ifc.ifc.hairpinHed) {
                    continue;
                }
                o = ntry.id;
                break;
            }
            if (o != ifc.sentHairpin) {
                String act;
                if (o < 1) {
                    act = "del";
                } else {
                    if (ifc.sentHairpin > 0) {
                        act = "mod";
                    } else {
                        act = "add";
                    }
                }
                lower.sendLine("hairpin_" + act + " " + ifc.id + " " + o);
                ifc.sentHairpin = o;
            }
        }
        if ((ifc.ifc.bundleHed != null) && (ifc.ifc.bundleIfc == null)) {
            List<servP4langIfc> ap = new ArrayList<servP4langIfc>();
            List<servP4langIfc> tp = new ArrayList<servP4langIfc>();
            if (ifc.ifc.bundleHed.bundleHed.backup > 0) {
                ifcBundleIfc sel = ifc.ifc.bundleHed.bundleHed.ifaces.get(ifc.ifc.bundleHed.bundleHed.selected);
                for (i = 0; i < lower.expIfc.size(); i++) {
                    servP4langIfc ntry = lower.expIfc.get(i);
                    if (ntry == ifc) {
                        continue;
                    }
                    if (ntry.ifc == null) {
                        continue;
                    }
                    if (ntry.ifc.bundleHed != ifc.ifc.bundleHed) {
                        continue;
                    }
                    tp.add(ntry);
                    if (ntry.ifc.bundleIfc != sel) {
                        continue;
                    }
                    ap.add(ntry);
                }
            } else {
                for (i = 0; i < lower.expIfc.size(); i++) {
                    servP4langIfc ntry = lower.expIfc.get(i);
                    if (ntry == ifc) {
                        continue;
                    }
                    if (ntry.ifc == null) {
                        continue;
                    }
                    if (ntry.ifc.bundleHed != ifc.ifc.bundleHed) {
                        continue;
                    }
                    tp.add(ntry);
                    if (ntry.ifc.ethtyp.getState() != state.states.up) {
                        continue;
                    }
                    ap.add(ntry);
                }
            }
            ifc.members = ap;
            List<servP4langIfc> vln = new ArrayList<servP4langIfc>();
            List<servP4langIfc> qvl = new ArrayList<servP4langIfc>();
            for (i = 0; i < lower.expIfc.size(); i++) {
                servP4langIfc ntry = lower.expIfc.get(i);
                if (ntry == ifc) {
                    continue;
                }
                if (ntry.master == null) {
                    continue;
                }
                if (ntry.master.master == ifc) {
                    qvl.add(ntry);
                }
                if (ntry.master == ifc) {
                    vln.add(ntry);
                }
            }
            o = ap.size();
            for (i = 0; i < ap.size(); i++) {
                o += ap.get(i).id;
            }
            if (o != ifc.sentBundle) {
                String a;
                if (ifc.sentBundle < 1) {
                    a = "add";
                } else {
                    a = "mod";
                }
                ifc.sentBundle = o;
                if (o < 1) {
                    a = "del";
                    ap.add(new servP4langIfc(lower, 0));
                }
                for (i = 0; i < 16; i++) {
                    lower.sendLine("portbundle_" + a + " " + ifc.id + " " + i + " " + ap.get(i % ap.size()).id);
                }
                String s = "";
                for (i = 0; i < ap.size(); i++) {
                    s += " " + ap.get(i).id;
                }
                lower.sendLine("bundlelist_" + a + " " + ifc.id + s);
            }
            if (ifc.sentVlan != (vln.size() + qvl.size())) {
                for (o = 0; o < tp.size(); o++) {
                    servP4langIfc ntry = tp.get(o);
                    for (i = 0; i < vln.size(); i++) {
                        servP4langIfc sub = vln.get(i);
                        lower.sendLine("bundlevlan_add " + ntry.id + " " + sub.ifc.vlanNum + " " + sub.id);
                    }
                    for (i = 0; i < qvl.size(); i++) {
                        servP4langIfc sub = qvl.get(i);
                        lower.sendLine("bundleqinq_add " + sub.master.id + " " + sub.ifc.vlanNum + " " + sub.id + " " + ntry.id + " " + sub.master.ifc.vlanNum);
                    }
                }
                ifc.sentVlan = vln.size() + qvl.size();
            }
        }
        String a;
        if (ifc.sentVrf == 0) {
            a = "add";
        } else {
            a = "mod";
        }
        if ((ifc.ifc.bridgeHed != null) && (ifc.ifc.bridgeIfc != null)) {
            servP4langBr br = new servP4langBr(ifc.ifc.bridgeHed.number);
            br = lower.expBr.find(br);
            if (br == null) {
                br = new servP4langBr(0);
            }
            if (br.routed == null) {
                i = ifc.ifc.bridgeIfc.tcp4mssIn;
                o = ifc.ifc.bridgeIfc.tcp4mssOut;
                int p = ifc.ifc.bridgeIfc.pmtud4valIn;
                int q = ifc.ifc.bridgeIfc.pmtud4valOut;
                if (i != ifc.sentMss4in) {
                    lower.sendLine("tcpmss4in_" + a + " " + ifc.id + " " + i);
                    ifc.sentMss4in = i;
                }
                if (o != ifc.sentMss4out) {
                    lower.sendLine("tcpmss4out_" + a + " " + ifc.id + " " + o);
                    ifc.sentMss4out = o;
                }
                if (p != ifc.sentPmtud4in) {
                    lower.sendLine("pmtud4in_" + a + " " + ifc.id + " " + p);
                    ifc.sentPmtud4in = p;
                }
                if (q != ifc.sentPmtud4out) {
                    lower.sendLine("pmtud4out_" + a + " " + ifc.id + " " + p);
                    ifc.sentPmtud4in = q;
                }
                i = ifc.ifc.bridgeIfc.tcp6mssIn;
                o = ifc.ifc.bridgeIfc.tcp6mssOut;
                p = ifc.ifc.bridgeIfc.pmtud6valIn;
                q = ifc.ifc.bridgeIfc.pmtud6valOut;
                if (i != ifc.sentMss6in) {
                    lower.sendLine("tcpmss6in_" + a + " " + ifc.id + " " + i);
                    ifc.sentMss6in = i;
                }
                if (o != ifc.sentMss6out) {
                    lower.sendLine("tcpmss6out_" + a + " " + ifc.id + " " + o);
                    ifc.sentMss6out = o;
                }
                if (p != ifc.sentPmtud6in) {
                    lower.sendLine("pmtud6in_" + a + " " + ifc.id + " " + p);
                    ifc.sentPmtud6in = p;
                }
                if (q != ifc.sentPmtud6out) {
                    lower.sendLine("pmtud6out_" + a + " " + ifc.id + " " + p);
                    ifc.sentPmtud6in = q;
                }
                if (servP4langUtil.needAcl(ifc.sentAcl4in1, ifc.ifc.bridgeIfc.filter4in, null, null, null, ifc.sentAcl4inF)) {
                    sendAcl(0, "inacl4_del " + ifc.id + " ", "", "", "", "", true, false, ifc.sentAcl4inF, null, null, null);
                    ifc.sentAcl4in1 = ifc.ifc.bridgeIfc.filter4in;
                    ifc.sentAcl4in2 = null;
                    sendAcl(0, "inacl4_add " + ifc.id + " ", "", "", "", "", true, false, ifc.sentAcl4in1, null, null, ifc.sentAcl4inF);
                }
                if (servP4langUtil.needAcl(ifc.sentAcl4out1, ifc.ifc.bridgeIfc.filter4out, null, null, null, ifc.sentAcl4outF)) {
                    sendAcl(0, "outacl4_del " + ifc.id + " ", "", "", "", "", true, false, ifc.sentAcl4outF, null, null, null);
                    ifc.sentAcl4out1 = ifc.ifc.bridgeIfc.filter4out;
                    ifc.sentAcl4out2 = null;
                    sendAcl(0, "outacl4_add " + ifc.id + " ", "", "", "", "", true, false, ifc.sentAcl4out1, null, null, ifc.sentAcl4outF);
                }
                if (servP4langUtil.needAcl(ifc.sentAcl6in1, ifc.ifc.bridgeIfc.filter6in, null, null, null, ifc.sentAcl6inF)) {
                    sendAcl(0, "inacl6_del " + ifc.id + " ", "", "", "", "", false, false, ifc.sentAcl6inF, null, null, null);
                    ifc.sentAcl6in1 = ifc.ifc.bridgeIfc.filter6in;
                    ifc.sentAcl6in2 = null;
                    sendAcl(0, "inacl6_add " + ifc.id + " ", "", "", "", "", false, false, ifc.sentAcl6in1, null, null, ifc.sentAcl6inF);
                }
                if (servP4langUtil.needAcl(ifc.sentAcl6out1, ifc.ifc.bridgeIfc.filter6out, null, null, null, ifc.sentAcl6outF)) {
                    sendAcl(0, "outacl6_del " + ifc.id + " ", "", "", "", "", false, false, ifc.sentAcl6outF, null, null, null);
                    ifc.sentAcl6out1 = ifc.ifc.bridgeIfc.filter6out;
                    ifc.sentAcl6out2 = null;
                    sendAcl(0, "outacl6_add " + ifc.id + " ", "", "", "", "", false, false, ifc.sentAcl6out1, null, null, ifc.sentAcl6outF);
                }
                if (ifc.sentVrf == -2) {
                    return false;
                }
                lower.sendLine("portbridge_" + a + " " + ifc.id + " " + ifc.ifc.bridgeHed.number);
                ifc.sentVrf = -2;
                return false;
            }
        }
        if (ifc.ifc.xconn != null) {
            if (ifc.ifc.xconn.pwom == null) {
                return false;
            }
            int ll = ifc.ifc.xconn.pwom.getLabelLoc();
            if (ll < 0) {
                return false;
            }
            if ((ifc.sentVrf == -1) && (ll == ifc.sentLabel)) {
                return false;
            }
            int lr = ifc.ifc.xconn.pwom.getLabelRem();
            if (lr < 0) {
                return false;
            }
            ipFwd ofwd = ifc.ifc.xconn.vrf.getFwd(ifc.ifc.xconn.adr);
            servP4langVrf ovrf = lower.findVrf(ofwd);
            if (ovrf == null) {
                return false;
            }
            tabRouteEntry<addrIP> ntry = ofwd.actualU.route(ifc.ifc.xconn.adr);
            ntry = lower.convRou(ntry, false);
            if (ntry == null) {
                return false;
            }
            if (ntry.best.iface == null) {
                return false;
            }
            servP4langNei hop = lower.findNei(ntry.best.iface, ntry.best.nextHop);
            if (hop == null) {
                return false;
            }
            lower.sendLine("xconnect_" + a + " " + ifc.id + " " + ifc.ifc.xconn.adr + " " + hop.id + " " + servP4langUtil.getLabel(ntry) + " " + ll + " " + lr);
            ifc.sentLabel = ll;
            ifc.sentVrf = -1;
            return false;
        }
        if (ifc.ifc.nshXcon != null) {
            if (ifc.sentVrf == -4) {
                return false;
            }
            lower.sendLine("nshconn_" + a + " " + ifc.id + " " + ifc.ifc.nshXcon.sp + " " + ifc.ifc.nshXcon.si);
            ifc.sentVrf = -4;
            return false;
        }
        if (ifc.ifc.iconn != null) {
            servP4langIfc per = lower.findIfc(ifc.ifc.iconn);
            if (per == null) {
                return false;
            }
            i = 0;
            if (per.viaN != null) {
                i = per.viaN.id;
            }
            if ((ifc.sentVrf == -3) && (ifc.sentLabel == i)) {
                return false;
            }
            if (i == 0) {
                lower.sendLine("loconnifc_" + a + " " + ifc.id + " " + per.id);
            } else {
                lower.sendLine("loconnnei_" + a + " " + ifc.id + " " + i);
            }
            ifc.sentVrf = -3;
            ifc.sentLabel = i;
            return false;
        }
        servP4langIfc mstr = ifc;
        if (ifc.ifc.bundleIfc != null) {
            mstr = lower.findBundl(ifc.ifc.bundleHed);
        }
        if (ifc.ifc.bridgeIfc != null) {
            mstr = lower.findIfc(ifc.ifc.bridgeHed);
        }
        servP4langVrf vrf = lower.findVrf(mstr);
        if (vrf == null) {
            return false;
        }
        if (mstr.ifc.fwdIf4 != null) {
            ifc.sentInsp4 = sendSess(ifc.id, true, ifc.sentInsp4, mstr.ifc.fwdIf4.inspect);
            if (servP4langUtil.needAcl(ifc.sentAcl4in1, mstr.ifc.fwdIf4.filterIn, ifc.sentAcl4in2, mstr.ifc.fwdIf4.cfilterIn, ifc.sentInsp4, ifc.sentAcl4inF)) {
                sendAcl(0, "inacl4_del " + ifc.id + " ", "", "", "", "", true, false, ifc.sentAcl4inF, null, null, null);
                ifc.sentAcl4in1 = mstr.ifc.fwdIf4.filterIn;
                ifc.sentAcl4in2 = mstr.ifc.fwdIf4.cfilterIn;
                sendAcl(0, "inacl4_add " + ifc.id + " ", "", "", "", "", true, false, ifc.sentAcl4in1, ifc.sentAcl4in2, ifc.sentInsp4, ifc.sentAcl4inF);
            }
            if (servP4langUtil.needAcl(ifc.sentAcl4out1, mstr.ifc.fwdIf4.filterOut, ifc.sentAcl4out2, mstr.ifc.fwdIf4.cfilterOut, ifc.sentInsp4, ifc.sentAcl4outF)) {
                sendAcl(0, "outacl4_del " + ifc.id + " ", "", "", "", "", true, false, ifc.sentAcl4outF, null, null, null);
                ifc.sentAcl4out1 = mstr.ifc.fwdIf4.filterOut;
                ifc.sentAcl4out2 = mstr.ifc.fwdIf4.cfilterOut;
                sendAcl(0, "outacl4_add " + ifc.id + " ", "", "", "", "", true, false, ifc.sentAcl4out1, ifc.sentAcl4out2, ifc.sentInsp4, ifc.sentAcl4outF);
            }
        }
        if (mstr.ifc.fwdIf6 != null) {
            ifc.sentInsp6 = sendSess(ifc.id, false, ifc.sentInsp6, mstr.ifc.fwdIf6.inspect);
            if (servP4langUtil.needAcl(ifc.sentAcl6in1, mstr.ifc.fwdIf6.filterIn, ifc.sentAcl6in2, mstr.ifc.fwdIf6.cfilterIn, ifc.sentInsp6, ifc.sentAcl6inF)) {
                sendAcl(0, "inacl6_del " + ifc.id + " ", "", "", "", "", false, false, ifc.sentAcl6inF, null, null, null);
                ifc.sentAcl6in1 = mstr.ifc.fwdIf6.filterIn;
                ifc.sentAcl6in2 = mstr.ifc.fwdIf6.cfilterIn;
                sendAcl(0, "inacl6_add " + ifc.id + " ", "", "", "", "", false, false, ifc.sentAcl6in1, ifc.sentAcl6in2, ifc.sentInsp6, ifc.sentAcl6inF);
            }
            if (servP4langUtil.needAcl(ifc.sentAcl6out1, mstr.ifc.fwdIf6.filterOut, ifc.sentAcl6out2, mstr.ifc.fwdIf6.cfilterOut, ifc.sentInsp6, ifc.sentAcl6outF)) {
                sendAcl(0, "outacl6_del " + ifc.id + " ", "", "", "", "", false, false, ifc.sentAcl6outF, null, null, null);
                ifc.sentAcl6out1 = mstr.ifc.fwdIf6.filterOut;
                ifc.sentAcl6out2 = mstr.ifc.fwdIf6.cfilterOut;
                sendAcl(0, "outacl6_add " + ifc.id + " ", "", "", "", "", false, false, ifc.sentAcl6out1, ifc.sentAcl6out2, ifc.sentInsp6, ifc.sentAcl6outF);
            }
        }
        if (vrf.id != ifc.sentVrf) {
            lower.sendLine("portvrf_" + a + " " + ifc.id + " " + vrf.id);
            ifc.sentVrf = vrf.id;
            ifc.sentMss4in = -1;
            ifc.sentMss4out = -1;
            ifc.sentMss6in = -1;
            ifc.sentMss6out = -1;
            ifc.sentPmtud4in = -1;
            ifc.sentPmtud6in = -1;
            ifc.sentPmtud4out = -1;
            ifc.sentPmtud6out = -1;
            ifc.sentVerify4 = -1;
            ifc.sentVerify6 = -1;
            ifc.sentMpls = -1;
            ifc.sentNsh = -1;
        }
        i = 0;
        o = 0;
        if (mstr.ifc.fwdIf4 != null) {
            i = mstr.ifc.fwdIf4.tcpMssIn;
            o = mstr.ifc.fwdIf4.tcpMssOut;
        }
        if (i != ifc.sentMss4in) {
            lower.sendLine("tcpmss4in_" + a + " " + ifc.id + " " + i);
            ifc.sentMss4in = i;
        }
        if (o != ifc.sentMss4out) {
            lower.sendLine("tcpmss4out_" + a + " " + ifc.id + " " + o);
            ifc.sentMss4out = o;
        }
        i = 0;
        o = 0;
        if (mstr.ifc.fwdIf6 != null) {
            i = mstr.ifc.fwdIf6.tcpMssIn;
            o = mstr.ifc.fwdIf6.tcpMssOut;
        }
        if (i != ifc.sentMss6in) {
            lower.sendLine("tcpmss6in_" + a + " " + ifc.id + " " + i);
            ifc.sentMss6in = i;
        }
        if (o != ifc.sentMss6out) {
            lower.sendLine("tcpmss6out_" + a + " " + ifc.id + " " + o);
            ifc.sentMss6out = o;
        }
        i = 0;
        o = 0;
        if (mstr.ifc.fwdIf4 != null) {
            i = mstr.ifc.fwdIf4.ttlSetIn;
            o = mstr.ifc.fwdIf4.ttlSetOut;
        }
        if (i != ifc.sentTtl4in) {
            lower.sendLine("ttlset4in_" + a + " " + ifc.id + " " + i);
            ifc.sentTtl4in = i;
        }
        if (o != ifc.sentTtl4out) {
            lower.sendLine("ttlset4out_" + a + " " + ifc.id + " " + o);
            ifc.sentTtl4out = o;
        }
        i = 0;
        o = 0;
        if (mstr.ifc.fwdIf6 != null) {
            i = mstr.ifc.fwdIf6.ttlSetIn;
            o = mstr.ifc.fwdIf6.ttlSetOut;
        }
        if (i != ifc.sentTtl6in) {
            lower.sendLine("ttlset6in_" + a + " " + ifc.id + " " + i);
            ifc.sentTtl6in = i;
        }
        if (o != ifc.sentTtl6out) {
            lower.sendLine("ttlset6out_" + a + " " + ifc.id + " " + o);
            ifc.sentTtl6out = o;
        }
        i = 0;
        o = 0;
        if (mstr.ifc.fwdIf4 != null) {
            i = mstr.ifc.fwdIf4.pmtudIn;
            o = mstr.ifc.fwdIf4.pmtudOut;
        }
        if (i != ifc.sentPmtud4in) {
            lower.sendLine("pmtud4in_" + a + " " + ifc.id + " " + i);
            ifc.sentPmtud4in = i;
        }
        if (o != ifc.sentPmtud4out) {
            lower.sendLine("pmtud4out_" + a + " " + ifc.id + " " + o);
            ifc.sentPmtud4out = o;
        }
        i = 0;
        o = 0;
        if (mstr.ifc.fwdIf6 != null) {
            i = mstr.ifc.fwdIf6.pmtudIn;
            o = mstr.ifc.fwdIf6.pmtudOut;
        }
        if (i != ifc.sentPmtud6in) {
            lower.sendLine("pmtud6in_" + a + " " + ifc.id + " " + i);
            ifc.sentPmtud6in = i;
        }
        if (o != ifc.sentPmtud6out) {
            lower.sendLine("pmtud6out_" + a + " " + ifc.id + " " + o);
            ifc.sentPmtud6out = o;
        }
        i = servP4langUtil.getVerifySource(mstr.ifc.fwdIf4);
        if (i != ifc.sentVerify4) {
            lower.sendLine("verify4_" + a + " " + ifc.id + " " + i);
            ifc.sentVerify4 = i;
        }
        i = servP4langUtil.getVerifySource(mstr.ifc.fwdIf6);
        if (i != ifc.sentVerify6) {
            lower.sendLine("verify6_" + a + " " + ifc.id + " " + i);
            ifc.sentVerify6 = i;
        }
        i = 0;
        o = 0;
        if (mstr.ifc.fwdIf4 != null) {
            if (mstr.ifc.fwdIf4.mplsPropTtlAllow) {
                i = 1;
            }
            if (mstr.ifc.fwdIf4.disableFlowspec) {
                o = 1;
            }
        }
        if (i != ifc.sentPropagate4) {
            lower.sendLine("mplsttl4_" + a + " " + ifc.id + " " + i);
            ifc.sentPropagate4 = i;
        }
        if (o != ifc.sentFlowDis4) {
            lower.sendLine("flowdis4_" + a + " " + ifc.id + " " + o);
            ifc.sentFlowDis4 = o;
        }
        i = 0;
        o = 0;
        if (mstr.ifc.fwdIf6 != null) {
            if (mstr.ifc.fwdIf6.mplsPropTtlAllow) {
                i = 1;
            }
            if (mstr.ifc.fwdIf6.disableFlowspec) {
                o = 1;
            }
        }
        if (i != ifc.sentPropagate6) {
            lower.sendLine("mplsttl6_" + a + " " + ifc.id + " " + i);
            ifc.sentPropagate6 = i;
        }
        if (o != ifc.sentFlowDis6) {
            lower.sendLine("flowdis6_" + a + " " + ifc.id + " " + o);
            ifc.sentFlowDis6 = o;
        }
        i = 0;
        if (mstr.ifc.mplsPack != null) {
            i = 1;
        }
        if (i != ifc.sentMpls) {
            lower.sendLine("mplspack_" + a + " " + ifc.id + " " + i);
            ifc.sentMpls = i;
        }
        i = 0;
        if (mstr.ifc.nshPack != null) {
            i = 1;
        }
        if (i != ifc.sentNsh) {
            lower.sendLine("nshpack_" + a + " " + ifc.id + " " + i);
            ifc.sentNsh = i;
        }
        i = -1;
        o = -1;
        if (mstr.ifc.polkaPack != null) {
            i = mstr.ifc.polkaPack.localId;
            o = mstr.ifc.polkaPack.coeffs[i].intCoeff();
        }
        if (i != ifc.sentPolka) {
            if (ifc.sentPolka >= 0) {
                a = "mod";
            } else {
                a = "add";
            }
            if (i < 0) {
                a = "del";
            }
            lower.sendLine("polkapoly_" + a + " " + ifc.id + " " + o);
            lower.sendLine("mpolkapoly_" + a + " " + ifc.id + " " + o);
            ifc.sentPolka = i;
        }
        return false;
    }

    private void doNeighs(servP4langNei ntry) {
        if (ntry.iface.id < 0) {
            return;
        }
        if (ntry.need < 1) {
            lower.neighs.del(ntry);
            if (ntry.sentIgNhop >= 0) {
                lower.sendLine("nhop2port_del " + ntry.id + " " + ntry.iface.id + " " + ntry.sentIgNhop);
            }
            if (ntry.sentEnc != null) {
                lower.sendLine(servP4langUtil.negateOneCommand(ntry.sentEnc));
                return;
            }
            if (ntry.mac == null) {
                return;
            }
            lower.sendLine("neigh" + (ntry.adr.isIPv4() ? "4" : "6") + "_del " + ntry.id + " " + ntry.adr + " " + ntry.mac.toEmuStr() + " " + ntry.vrf.id + " " + ntry.iface.getMac().toEmuStr() + " " + ntry.sentIfc);
            return;
        }
        int val = ntry.getVia().getUcast().id;
        if (val == ntry.sentIgNhop) {
            return;
        }
        String act;
        if (ntry.sentIgNhop < 0) {
            act = "add";
        } else {
            act = "mod";
        }
        lower.sendLine("nhop2port_" + act + " " + ntry.id + " " + ntry.iface.id + " " + val);
        ntry.sentIgNhop = val;
    }

    private boolean doCheckPpp(servP4langIfc ifc) {
        if (ifc.ifc.ppp != null) {
            if (ifc.ifc.ppp.getState() != state.states.up) {
                return true;
            }
        }
        if (ifc.ifc.frmrly != null) {
            if (ifc.ifc.frmrly.getState() != state.states.up) {
                return true;
            }
        }
        return false;
    }

    private void doL3tpClnt(clntL2tp3 ntry, servP4langNei nei, servP4langIfc ifc) {
        if (doCheckPpp(ifc)) {
            return;
        }
        ipFwd ofwd = ntry.getFwd();
        servP4langVrf ovrf = lower.findVrf(ofwd);
        if (ovrf == null) {
            return;
        }
        addrIP adr = ntry.getAddrRem();
        if (adr == null) {
            return;
        }
        addrIP src = ntry.getAddrLoc();
        if (src == null) {
            return;
        }
        servP4langNei hop = lower.findHop(ofwd, adr);
        if (hop == null) {
            return;
        }
        if (hop.mac == null) {
            return;
        }
        int ses = ntry.getSessRem();
        if (ses == 0) {
            return;
        }
        String act;
        if (nei.mac == null) {
            act = "add";
        } else {
            act = "mod";
            if ((hop.mac.compareTo(nei.mac) == 0) && (nei.viaH == hop) && (nei.sentIfc == hop.sentIfc) && (nei.sentTun == ses)) {
                return;
            }
        }
        nei.viaH = hop;
        nei.mac = hop.mac.copyBytes();
        nei.sentIfc = hop.sentIfc;
        nei.sentTun = ses;
        String afi;
        if (adr.isIPv4()) {
            afi = "4";
        } else {
            afi = "6";
        }
        lower.sendLine("l3tp" + afi + "_" + act + " " + nei.id + " " + ifc.id + " " + hop.sentIfc + " " + src + " " + adr + " " + hop.mac.toEmuStr() + " " + ovrf.id + " " + hop.iface.getMac().toEmuStr() + " " + ses);
    }

    private void doL2tpClnt(clntL2tp2 ntry, servP4langNei nei, servP4langIfc ifc) {
        if (doCheckPpp(ifc)) {
            return;
        }
        ipFwd ofwd = ntry.getFwd();
        servP4langVrf ovrf = lower.findVrf(ofwd);
        if (ovrf == null) {
            return;
        }
        addrIP adr = ntry.getAddrRem();
        if (adr == null) {
            return;
        }
        addrIP src = ntry.getAddrLoc();
        if (src == null) {
            return;
        }
        servP4langNei hop = lower.findHop(ofwd, adr);
        if (hop == null) {
            return;
        }
        if (hop.mac == null) {
            return;
        }
        int tun = ntry.getTunnRem();
        if (tun < 1) {
            return;
        }
        int ses = ntry.getSessRem();
        if (ses < 1) {
            return;
        }
        tun = (tun << 16) | ses;
        int lp = ntry.getPortLoc();
        if (lp < 1) {
            return;
        }
        int rp = ntry.getPortRem();
        if (rp < 1) {
            return;
        }
        String act;
        if (nei.mac == null) {
            act = "add";
        } else {
            act = "mod";
            if ((hop.mac.compareTo(nei.mac) == 0) && (nei.viaH == hop) && (nei.sentIfc == hop.sentIfc) && (nei.sentTun == tun)) {
                return;
            }
        }
        nei.viaH = hop;
        nei.mac = hop.mac.copyBytes();
        nei.sentIfc = hop.sentIfc;
        nei.sentTun = tun;
        String afi;
        if (adr.isIPv4()) {
            afi = "4";
        } else {
            afi = "6";
        }
        lower.sendLine("l2tp" + afi + "_" + act + " " + nei.id + " " + ifc.id + " " + hop.sentIfc + " " + src + " " + adr + " " + hop.mac.toEmuStr() + " " + ovrf.id + " " + hop.iface.getMac().toEmuStr() + " " + lp + " " + rp + " " + tun);
    }

    private void doNeighs(boolean ipv4, servP4langIfc ifc, ipIfc ipi) {
        if (ipi == null) {
            return;
        }
        servP4langVrf vrf = lower.findVrf(ifc);
        if (vrf == null) {
            return;
        }
        if (ifc.cloned != null) {
            servP4langNei nei = lower.findNei(ifc.ifc.fwdIf4, new addrIP());
            if (nei == null) {
                nei = lower.findNei(ifc.ifc.fwdIf6, new addrIP());
            }
            if (nei == null) {
                return;
            }
            nei.need++;
            nei.iface.viaN = nei;
            nei.vrf = vrf;
            try {
                ifcP2pOEservSess ntry = (ifcP2pOEservSess) ifc.ifc.lower;
                addrMac macL = new addrMac();
                servP4langIfc prnt = lower.findIfc(ntry.getLower(macL));
                if (prnt == null) {
                    return;
                }
                addrMac macR = new addrMac();
                int ses = ntry.getSession(macR);
                if (ses == ifc.sentPppoe) {
                    return;
                }
                String act;
                int sess;
                if (ses < 0) {
                    act = "del";
                    sess = ifc.sentPppoe;
                } else {
                    if (ifc.sentPppoe < 0) {
                        act = "add";
                    } else {
                        act = "mod";
                    }
                    sess = ses;
                }
                nei.viaI = prnt;
                lower.sendLine("pppoe_" + act + " " + ifc.id + " " + prnt.id + " " + nei.id + " " + vrf.id + " " + sess + " " + macR.toEmuStr() + " " + macL.toEmuStr());
                ifc.sentPppoe = ses;
                return;
            } catch (Exception e) {
            }
            try {
                clntL2tp3 ntry = (clntL2tp3) ifc.ifc.lower;
                doL3tpClnt(ntry, nei, ifc);
                return;
            } catch (Exception e) {
            }
            try {
                clntL2tp2 ntry = (clntL2tp2) ifc.ifc.lower;
                doL2tpClnt(ntry, nei, ifc);
                return;
            } catch (Exception e) {
            }
            try {
                clntGtp ntry = (clntGtp) ifc.ifc.lower;
                ipFwd ofwd = ntry.getFwd();
                servP4langVrf ovrf = lower.findVrf(ofwd);
                if (ovrf == null) {
                    return;
                }
                addrIP adr = ntry.getRemAddr();
                if (adr == null) {
                    return;
                }
                addrIP src = ntry.getLocAddr();
                if (src == null) {
                    return;
                }
                servP4langNei hop = lower.findHop(ofwd, adr);
                if (hop == null) {
                    return;
                }
                if (hop.mac == null) {
                    return;
                }
                int tun = ntry.getTunnRem();
                if (tun < 1) {
                    return;
                }
                int lp = ntry.getLocPort();
                if (lp < 1) {
                    return;
                }
                int rp = ntry.getRemPort();
                if (rp < 1) {
                    return;
                }
                String act;
                if (nei.mac == null) {
                    act = "add";
                } else {
                    act = "mod";
                    if ((hop.mac.compareTo(nei.mac) == 0) && (nei.viaH == hop) && (nei.sentIfc == hop.sentIfc) && (nei.sentTun == tun)) {
                        return;
                    }
                }
                nei.viaH = hop;
                nei.mac = hop.mac.copyBytes();
                nei.sentIfc = hop.sentIfc;
                nei.sentTun = tun;
                String afi;
                if (adr.isIPv4()) {
                    afi = "4";
                } else {
                    afi = "6";
                }
                lower.sendLine("gtp" + afi + "_" + act + " " + nei.id + " " + ifc.id + " " + hop.sentIfc + " " + src + " " + adr + " " + hop.mac.toEmuStr() + " " + ovrf.id + " " + hop.iface.getMac().toEmuStr() + " " + lp + " " + rp + " " + tun);
                return;
            } catch (Exception e) {
            }
            try {
                clntAmt ntry = (clntAmt) ifc.ifc.lower;
                ipFwd ofwd = ntry.getFwd();
                servP4langVrf ovrf = lower.findVrf(ofwd);
                if (ovrf == null) {
                    return;
                }
                addrIP adr = ntry.getRemAddr();
                if (adr == null) {
                    return;
                }
                addrIP src = ntry.getLocAddr();
                if (src == null) {
                    return;
                }
                servP4langNei hop = lower.findHop(ofwd, adr);
                if (hop == null) {
                    return;
                }
                if (hop.mac == null) {
                    return;
                }
                int lp = ntry.getLocPort();
                if (lp < 1) {
                    return;
                }
                int rp = ntry.getRemPort();
                if (rp < 1) {
                    return;
                }
                String act;
                if (nei.mac == null) {
                    act = "add";
                } else {
                    act = "mod";
                    if ((hop.mac.compareTo(nei.mac) == 0) && (nei.viaH == hop) && (nei.sentIfc == hop.sentIfc)) {
                        return;
                    }
                }
                nei.viaH = hop;
                nei.mac = hop.mac.copyBytes();
                nei.sentIfc = hop.sentIfc;
                String afi;
                if (adr.isIPv4()) {
                    afi = "4";
                } else {
                    afi = "6";
                }
                lower.sendLine("amt" + afi + "_" + act + " " + nei.id + " " + ifc.id + " " + hop.sentIfc + " " + src + " " + adr + " " + hop.mac.toEmuStr() + " " + ovrf.id + " " + hop.iface.getMac().toEmuStr() + " " + lp + " " + rp);
                return;
            } catch (Exception e) {
            }
            try {
                prtGre ntry = (prtGre) ifc.ifc.lower;
                addrIP adr = ntry.getAddrRem();
                if (adr == null) {
                    return;
                }
                addrIP src = ntry.getAddrLoc();
                if (src == null) {
                    return;
                }
                ipFwd ofwd = ntry.getFwd();
                if (ofwd == null) {
                    return;
                }
                servP4langVrf ovrf = lower.findVrf(ofwd);
                if (ovrf == null) {
                    return;
                }
                servP4langNei hop = lower.findHop(ofwd, adr);
                if (hop == null) {
                    return;
                }
                if (hop.mac == null) {
                    return;
                }
                String act;
                if (nei.mac == null) {
                    act = "add";
                } else {
                    act = "mod";
                    if ((hop.mac.compareTo(nei.mac) == 0) && (nei.sentIfc == hop.sentIfc) && (nei.viaH == hop)) {
                        return;
                    }
                }
                nei.viaH = hop;
                nei.mac = hop.mac.copyBytes();
                nei.sentIfc = hop.sentIfc;
                String afi;
                if (adr.isIPv4()) {
                    afi = "4";
                } else {
                    afi = "6";
                }
                lower.sendLine("gre" + afi + "_" + act + " " + nei.id + " " + ifc.id + " " + hop.sentIfc + " " + src + " " + adr + " " + hop.mac.toEmuStr() + " " + ovrf.id + " " + hop.iface.getMac().toEmuStr());
                return;
            } catch (Exception e) {
            }
            try {
                prtTmux ntry = (prtTmux) ifc.ifc.lower;
                addrIP adr = ntry.getAddrRem();
                if (adr == null) {
                    return;
                }
                addrIP src = ntry.getAddrLoc();
                if (src == null) {
                    return;
                }
                ipFwd ofwd = ntry.getFwd();
                if (ofwd == null) {
                    return;
                }
                servP4langVrf ovrf = lower.findVrf(ofwd);
                if (ovrf == null) {
                    return;
                }
                servP4langNei hop = lower.findHop(ofwd, adr);
                if (hop == null) {
                    return;
                }
                if (hop.mac == null) {
                    return;
                }
                String act;
                if (nei.mac == null) {
                    act = "add";
                } else {
                    act = "mod";
                    if ((hop.mac.compareTo(nei.mac) == 0) && (nei.sentIfc == hop.sentIfc) && (nei.viaH == hop)) {
                        return;
                    }
                }
                nei.viaH = hop;
                nei.mac = hop.mac.copyBytes();
                nei.sentIfc = hop.sentIfc;
                String afi;
                if (adr.isIPv4()) {
                    afi = "4";
                } else {
                    afi = "6";
                }
                lower.sendLine("tmux" + afi + "_" + act + " " + nei.id + " " + ifc.id + " " + hop.sentIfc + " " + src + " " + adr + " " + hop.mac.toEmuStr() + " " + ovrf.id + " " + hop.iface.getMac().toEmuStr());
                return;
            } catch (Exception e) {
            }
            try {
                prtIpIp ntry = (prtIpIp) ifc.ifc.lower;
                addrIP adr = ntry.getAddrRem();
                if (adr == null) {
                    return;
                }
                addrIP src = ntry.getAddrLoc();
                if (src == null) {
                    return;
                }
                ipFwd ofwd = ntry.getFwd();
                if (ofwd == null) {
                    return;
                }
                servP4langVrf ovrf = lower.findVrf(ofwd);
                if (ovrf == null) {
                    return;
                }
                servP4langNei hop = lower.findHop(ofwd, adr);
                if (hop == null) {
                    return;
                }
                if (hop.mac == null) {
                    return;
                }
                String act;
                if (nei.mac == null) {
                    act = "add";
                } else {
                    act = "mod";
                    if ((hop.mac.compareTo(nei.mac) == 0) && (nei.sentIfc == hop.sentIfc) && (nei.viaH == hop)) {
                        return;
                    }
                }
                nei.viaH = hop;
                nei.mac = hop.mac.copyBytes();
                nei.sentIfc = hop.sentIfc;
                String afi;
                if (adr.isIPv4()) {
                    afi = "4";
                } else {
                    afi = "6";
                }
                lower.sendLine("ipip" + afi + "_" + act + " " + nei.id + " " + ifc.id + " " + hop.sentIfc + " " + src + " " + adr + " " + hop.mac.toEmuStr() + " " + ovrf.id + " " + hop.iface.getMac().toEmuStr());
                return;
            } catch (Exception e) {
            }
            try {
                servL2tp2sess ntry = (servL2tp2sess) ifc.ifc.lower;
                addrIP src = ntry.getAddrLoc();
                if (src == null) {
                    return;
                }
                addrIP trg = ntry.getAddrRem();
                if (trg == null) {
                    return;
                }
                ipFwd ofwd = ntry.getFwder();
                servP4langVrf ovrf = lower.findVrf(ofwd);
                if (ovrf == null) {
                    return;
                }
                servP4langNei hop = lower.findHop(ofwd, trg);
                if (hop == null) {
                    return;
                }
                if (hop.mac == null) {
                    return;
                }
                int tun = ntry.getTunnRem();
                if (tun < 1) {
                    return;
                }
                int ses = ntry.getSessRem();
                if (ses < 1) {
                    return;
                }
                tun = (tun << 16) | ses;
                int lp = ntry.getPortLoc();
                if (lp < 1) {
                    return;
                }
                int rp = ntry.getPortRem();
                if (rp < 1) {
                    return;
                }
                String act;
                if (nei.mac == null) {
                    act = "add";
                } else {
                    act = "mod";
                    if ((hop.mac.compareTo(nei.mac) == 0) && (nei.viaH == hop) && (nei.sentIfc == hop.sentIfc) && (nei.sentTun == rp)) {
                        return;
                    }
                }
                nei.viaH = hop;
                nei.mac = hop.mac.copyBytes();
                nei.sentIfc = hop.sentIfc;
                nei.sentTun = rp;
                String afi;
                if (trg.isIPv4()) {
                    afi = "4";
                } else {
                    afi = "6";
                }
                String a = "l2tp" + afi + "_" + act + " " + nei.id + " " + ifc.id + " " + hop.sentIfc + " " + src + " " + trg + " " + hop.mac.toEmuStr() + " " + ovrf.id + " " + hop.iface.getMac().toEmuStr() + " " + lp + " " + rp + " " + tun;
                nei.sentEnc = a;
                lower.sendLine(a);
                return;
            } catch (Exception e) {
            }
            try {
                servL2tp3sess ntry = (servL2tp3sess) ifc.ifc.lower;
                addrIP src = ntry.getAddrLoc();
                if (src == null) {
                    return;
                }
                addrIP trg = ntry.getAddrRem();
                if (trg == null) {
                    return;
                }
                ipFwd ofwd = ntry.getFwder();
                servP4langVrf ovrf = lower.findVrf(ofwd);
                if (ovrf == null) {
                    return;
                }
                servP4langNei hop = lower.findHop(ofwd, trg);
                if (hop == null) {
                    return;
                }
                if (hop.mac == null) {
                    return;
                }
                int ses = ntry.getSessRem();
                if (ses < 1) {
                    return;
                }
                String act;
                if (nei.mac == null) {
                    act = "add";
                } else {
                    act = "mod";
                    if ((hop.mac.compareTo(nei.mac) == 0) && (nei.viaH == hop) && (nei.sentIfc == hop.sentIfc) && (nei.sentTun == ses)) {
                        return;
                    }
                }
                nei.viaH = hop;
                nei.mac = hop.mac.copyBytes();
                nei.sentIfc = hop.sentIfc;
                nei.sentTun = ses;
                String afi;
                if (trg.isIPv4()) {
                    afi = "4";
                } else {
                    afi = "6";
                }
                String a = "l3tp" + afi + "_" + act + " " + nei.id + " " + ifc.id + " " + hop.sentIfc + " " + src + " " + trg + " " + hop.mac.toEmuStr() + " " + ovrf.id + " " + hop.iface.getMac().toEmuStr() + " " + ses;
                nei.sentEnc = a;
                lower.sendLine(a);
                return;
            } catch (Exception e) {
            }
            try {
                servAmtConn ntry = (servAmtConn) ifc.ifc.lower;
                addrIP src = ntry.getAddrLoc();
                if (src == null) {
                    return;
                }
                addrIP trg = ntry.getAddrRem();
                if (trg == null) {
                    return;
                }
                ipFwd ofwd = ntry.getFwder();
                servP4langVrf ovrf = lower.findVrf(ofwd);
                if (ovrf == null) {
                    return;
                }
                servP4langNei hop = lower.findHop(ofwd, trg);
                if (hop == null) {
                    return;
                }
                if (hop.mac == null) {
                    return;
                }
                int lp = ntry.getPortLoc();
                if (lp < 1) {
                    return;
                }
                int rp = ntry.getPortRem();
                if (rp < 1) {
                    return;
                }
                String act;
                if (nei.mac == null) {
                    act = "add";
                } else {
                    act = "mod";
                    if ((hop.mac.compareTo(nei.mac) == 0) && (nei.viaH == hop) && (nei.sentIfc == hop.sentIfc) && (nei.sentTun == rp)) {
                        return;
                    }
                }
                nei.viaH = hop;
                nei.mac = hop.mac.copyBytes();
                nei.sentIfc = hop.sentIfc;
                nei.sentTun = rp;
                String afi;
                if (trg.isIPv4()) {
                    afi = "4";
                } else {
                    afi = "6";
                }
                String a = "amt" + afi + "_" + act + " " + nei.id + " " + ifc.id + " " + hop.sentIfc + " " + src + " " + trg + " " + hop.mac.toEmuStr() + " " + ovrf.id + " " + hop.iface.getMac().toEmuStr() + " " + lp + " " + rp;
                nei.sentEnc = a;
                lower.sendLine(a);
                return;
            } catch (Exception e) {
            }
            try {
                servGtpSess ntry = (servGtpSess) ifc.ifc.lower;
                addrIP src = ntry.getAddrLoc();
                if (src == null) {
                    return;
                }
                addrIP trg = ntry.getAddrRem();
                if (trg == null) {
                    return;
                }
                ipFwd ofwd = ntry.getFwder();
                servP4langVrf ovrf = lower.findVrf(ofwd);
                if (ovrf == null) {
                    return;
                }
                servP4langNei hop = lower.findHop(ofwd, trg);
                if (hop == null) {
                    return;
                }
                if (hop.mac == null) {
                    return;
                }
                int lp = ntry.getPortLoc();
                if (lp < 1) {
                    return;
                }
                int rp = ntry.getPortRem();
                if (rp < 1) {
                    return;
                }
                String act;
                if (nei.mac == null) {
                    act = "add";
                } else {
                    act = "mod";
                    if ((hop.mac.compareTo(nei.mac) == 0) && (nei.viaH == hop) && (nei.sentIfc == hop.sentIfc) && (nei.sentTun == rp)) {
                        return;
                    }
                }
                nei.viaH = hop;
                nei.mac = hop.mac.copyBytes();
                nei.sentIfc = hop.sentIfc;
                nei.sentTun = rp;
                String afi;
                if (trg.isIPv4()) {
                    afi = "4";
                } else {
                    afi = "6";
                }
                String a = "gtp" + afi + "_" + act + " " + nei.id + " " + ifc.id + " " + hop.sentIfc + " " + src + " " + trg + " " + hop.mac.toEmuStr() + " " + ovrf.id + " " + hop.iface.getMac().toEmuStr() + " " + lp + " " + rp + " " + ntry.teidDat;
                nei.sentEnc = a;
                lower.sendLine(a);
                return;
            } catch (Exception e) {
            }
            return;
        }
        if (ifc.ifc.type == tabRouteIface.ifaceType.virtppp) {
            servP4langNei nei = lower.findNei(ifc.ifc.fwdIf4, new addrIP());
            if (nei == null) {
                nei = lower.findNei(ifc.ifc.fwdIf6, new addrIP());
            }
            if (nei == null) {
                return;
            }
            nei.need++;
            nei.iface.viaN = nei;
            nei.vrf = vrf;
            if (ifc.ifc.pwhe == null) {
                return;
            }
            if (ifc.ifc.pwhe.l2tp3 != null) {
                doL3tpClnt(ifc.ifc.pwhe.l2tp3, nei, ifc);
                return;
            }
            if (ifc.ifc.pwhe.l2tp2 != null) {
                doL2tpClnt(ifc.ifc.pwhe.l2tp2, nei, ifc);
                return;
            }
            return;
        }
        if (ifc.ifc.type == tabRouteIface.ifaceType.tunnel) {
            servP4langNei nei = lower.findNei(ifc.ifc.fwdIf4, new addrIP());
            if (nei == null) {
                nei = lower.findNei(ifc.ifc.fwdIf6, new addrIP());
            }
            if (nei == null) {
                return;
            }
            nei.need++;
            nei.iface.viaN = nei;
            nei.vrf = vrf;
            String prt;
            String par = "";
            if (ifc.ifc.tunMode == null) {
                return;
            }
            int[] labs = null;
            switch (ifc.ifc.tunMode) {
                case mgre:
                    prtMgre ntry = (prtMgre) ifc.ifc.lower;
                    addrIP adr = ntry.getAddrRem();
                    if (adr == null) {
                        return;
                    }
                    addrIP src = ntry.getAddrLoc();
                    if (src == null) {
                        return;
                    }
                    addrIP grp = ntry.getAddrGrp();
                    if (grp == null) {
                        return;
                    }
                    ipFwd ofwd = ntry.getFwd();
                    if (ofwd == null) {
                        return;
                    }
                    servP4langVrf ovrf = lower.findVrf(ofwd);
                    if (ovrf == null) {
                        return;
                    }
                    servP4langNei hop = lower.findHop(ofwd, adr);
                    if (hop == null) {
                        return;
                    }
                    if (hop.mac == null) {
                        return;
                    }
                    String act;
                    if (nei.mac == null) {
                        act = "add";
                    } else {
                        act = "mod";
                        if ((hop.mac.compareTo(nei.mac) == 0) && (nei.sentIfc == hop.sentIfc) && (nei.viaH == hop)) {
                            return;
                        }
                    }
                    nei.viaH = hop;
                    nei.mac = hop.mac.copyBytes();
                    nei.sentIfc = hop.sentIfc;
                    String afi;
                    if (adr.isIPv4()) {
                        afi = "4";
                    } else {
                        afi = "6";
                    }
                    lower.sendLine("mgre" + afi + "_" + act + " " + nei.id + " " + ifc.id + " " + hop.sentIfc + " " + src + " " + grp + " " + adr + " " + grp.conv2multiMac().toEmuStr() + " " + ovrf.id + " " + hop.iface.getMac().toEmuStr());
                    return;
                case gre:
                    prt = "gre";
                    break;
                case ipip:
                    prt = "ipip";
                    break;
                case tmux:
                    prt = "tmux";
                    break;
                case ipsec:
                    prt = "ipsec";
                    if (ifc.ifc.tunIPsec1 != null) {
                        par = servP4langUtil.getIpsecParam(ifc.ifc.tunIPsec1.espRx, ifc.ifc.tunIPsec1.espTx, ifc.ifc.tunIPsec1.transform);
                    }
                    if (ifc.ifc.tunIPsec2 != null) {
                        par = servP4langUtil.getIpsecParam(ifc.ifc.tunIPsec2.espRx, ifc.ifc.tunIPsec2.espTx, ifc.ifc.tunIPsec2.transform);
                    }
                    if (par.length() < 1) {
                        return;
                    }
                    break;
                case openvpn:
                    if (ifc.ifc.tunOpenvpn == null) {
                        return;
                    }
                    if (ifc.ifc.tunOpenvpn.keyEncr == null) {
                        return;
                    }
                    int lp = ifc.ifc.tunOpenvpn.getLocPort();
                    if (lp < 1) {
                        return;
                    }
                    prt = "openvpn";
                    par = " " + lp + " " + ifc.ifc.tunOpenvpn.getRemPort() + " " + ifc.ifc.tunOpenvpn.timTx + " " + ifc.ifc.tunOpenvpn.cphrSiz + " " + ifc.ifc.tunOpenvpn.hashSiz + " " + ifc.ifc.tunOpenvpn.transform.encr2str() + " " + ifc.ifc.tunOpenvpn.transform.hash2str() + " " + bits.toHex(ifc.ifc.tunOpenvpn.keyEncr) + " " + bits.toHex(ifc.ifc.tunOpenvpn.keyHash);
                    break;
                case wireguard:
                    if (ifc.ifc.tunWireguard == null) {
                        return;
                    }
                    if (ifc.ifc.tunWireguard.keyTx == null) {
                        return;
                    }
                    lp = ifc.ifc.tunWireguard.getLocPort();
                    if (lp < 1) {
                        return;
                    }
                    prt = "wireguard";
                    par = " " + lp + " " + ifc.ifc.tunWireguard.getRemPort() + " " + ifc.ifc.tunWireguard.idxTx + " " + bits.toHex(ifc.ifc.tunWireguard.keyTx) + " " + bits.toHex(ifc.ifc.tunWireguard.keyRx);
                    break;
                case amt:
                    if (ifc.ifc.tunAmt == null) {
                        return;
                    }
                    lp = ifc.ifc.tunAmt.getLocPort();
                    if (lp < 1) {
                        return;
                    }
                    prt = "amt";
                    par = " " + lp + " " + ifc.ifc.tunAmt.getRemPort();
                    break;
                case gtp:
                    if (ifc.ifc.tunGtp == null) {
                        return;
                    }
                    lp = ifc.ifc.tunGtp.getLocPort();
                    if (lp < 1) {
                        return;
                    }
                    prt = "gtp";
                    par = " " + lp + " " + ifc.ifc.tunGtp.getRemPort() + " " + ifc.ifc.tunGtp.teidDat;
                    break;
                case srMpls:
                    if (ifc.ifc.tunSrMpls == null) {
                        return;
                    }
                    labs = ifc.ifc.tunSrMpls.getLabels();
                    if (labs == null) {
                        return;
                    }
                    prt = "labsnei";
                    break;
                case ldpTe:
                    if (ifc.ifc.tunLdpTe == null) {
                        return;
                    }
                    labs = ifc.ifc.tunLdpTe.getLabels();
                    if (labs == null) {
                        return;
                    }
                    prt = "labsnei";
                    break;
                default:
                    return;
            }
            if (ifc.ifc.tunVrf == null) {
                return;
            }
            if (ifc.ifc.tunTrg == null) {
                return;
            }
            if (ifc.ifc.tunSrc == null) {
                return;
            }
            addrIP src = ifc.ifc.tunSrc.getLocAddr(ifc.ifc.tunTrg);
            if (src == null) {
                return;
            }
            ipFwd ofwd = ifc.ifc.tunVrf.getFwd(ifc.ifc.tunTrg);
            servP4langVrf ovrf = lower.findVrf(ofwd);
            if (ovrf == null) {
                return;
            }
            servP4langNei hop = lower.findHop(ofwd, ifc.ifc.tunTrg);
            if (hop == null) {
                return;
            }
            if (hop.mac == null) {
                return;
            }
            String act;
            if (nei.mac == null) {
                act = "add";
            } else {
                act = "mod";
                if ((hop.mac.compareTo(nei.mac) == 0) && (nei.sentIfc == hop.sentIfc) && (nei.viaH == hop) && (par.equals(nei.sentIpsec))) {
                    return;
                }
            }
            nei.viaH = hop;
            nei.mac = hop.mac.copyBytes();
            nei.sentIfc = hop.sentIfc;
            nei.sentIpsec = par;
            String afi;
            if (ifc.ifc.tunTrg.isIPv4()) {
                afi = "4";
            } else {
                afi = "6";
            }
            if (labs == null) {
                lower.sendLine(prt + afi + "_" + act + " " + nei.id + " " + ifc.id + " " + hop.sentIfc + " " + src + " " + ifc.ifc.tunTrg + " " + hop.mac.toEmuStr() + " " + ovrf.id + " " + hop.iface.getMac().toEmuStr() + par);
                return;
            }
            par = " " + labs.length;
            for (int i = labs.length - 1; i >= 0; i--) {
                par += " " + labs[i];
            }
            for (int i = labs.length; i < 10; i++) {
                par += " 0";
            }
            lower.sendLine("labsnei_" + act + " " + nei.id + " " + hop.mac.toEmuStr() + " " + ovrf.id + " " + hop.iface.getMac().toEmuStr() + " " + hop.sentIfc + " " + ifc.id + par);
            return;
        }
        if (ifc.ifc.type == tabRouteIface.ifaceType.dialer) {
            if (ifc.pppoe == null) {
                return;
            }
            servP4langNei nei = lower.findNei(ifc.ifc.fwdIf4, new addrIP());
            if (nei == null) {
                nei = lower.findNei(ifc.ifc.fwdIf6, new addrIP());
            }
            if (nei == null) {
                return;
            }
            nei.need++;
            nei.iface.viaN = nei;
            nei.vrf = vrf;
            int ses = -1;
            addrMac mac = new addrMac();
            if (ifc.pppoe.ifc.pppoeC != null) {
                ses = ifc.pppoe.ifc.pppoeC.getSession(mac);
            }
            if (ifc.pppoe.ifc.pppoeR != null) {
                ses = ifc.pppoe.ifc.pppoeR.getSession(mac);
            }
            if (ses == ifc.sentPppoe) {
                return;
            }
            String act;
            int sess;
            if (ses < 0) {
                act = "del";
                sess = ifc.sentPppoe;
            } else {
                if (ifc.sentPppoe < 0) {
                    act = "add";
                } else {
                    act = "mod";
                }
                sess = ses;
            }
            nei.viaI = ifc.pppoe;
            lower.sendLine("pppoe_" + act + " " + ifc.id + " " + ifc.pppoe.id + " " + nei.id + " " + vrf.id + " " + sess + " " + mac.toEmuStr() + " " + ifc.pppoe.getMac().toEmuStr());
            ifc.sentPppoe = ses;
            return;
        }
        String afi;
        if (ipv4) {
            afi = "4";
        } else {
            afi = "6";
        }
        if (ifc.ifc.type == tabRouteIface.ifaceType.pweth) {
            if (ifc.ifc.pwhe == null) {
                return;
            }
            if (ifc.ifc.pwhe.pwom == null) {
                return;
            }
            int ll = ifc.ifc.pwhe.pwom.getLabelLoc();
            if (ll < 0) {
                if (ifc.sentLabel < 0) {
                    return;
                }
                lower.sendLine("pwhelab_del " + ifc.sentLabel + " " + ifc.id);
                ifc.sentLabel = -1;
                return;
            }
            if (ll != ifc.sentLabel) {
                String a;
                if (ifc.sentLabel < 0) {
                    a = "add";
                } else {
                    a = "mod";
                }
                lower.sendLine("pwhelab_" + a + " " + ll + " " + ifc.id);
                ifc.sentLabel = ll;
            }
            int lr = ifc.ifc.pwhe.pwom.getLabelRem();
            if (lr < 0) {
                return;
            }
            addrIP adr = ifc.ifc.pwhe.pwom.getRemote();
            if (adr == null) {
                return;
            }
            ipFwd ofwd = ifc.ifc.pwhe.pwom.vrf.getFwd(adr);
            servP4langVrf ovrf = lower.findVrf(ofwd);
            if (ovrf == null) {
                return;
            }
            tabRouteEntry<addrIP> ntry = ofwd.actualU.route(adr);
            ntry = lower.convRou(ntry, false);
            if (ntry == null) {
                return;
            }
            if (ntry.best.iface == null) {
                return;
            }
            servP4langNei hop = lower.findNei(ntry.best.iface, ntry.best.nextHop);
            if (hop == null) {
                return;
            }
            if (hop.mac == null) {
                return;
            }
            ll = servP4langUtil.getLabel(ntry);
            for (int i = 0;; i++) {
                servP4langNei nei = new servP4langNei(ifc, new addrIP());
                nei.mac = new addrMac();
                if (ipi.getL2info(i, nei.adr, nei.mac)) {
                    break;
                }
                if (!ipi.checkConnected(nei.adr)) {
                    continue;
                }
                servP4langNei old = lower.neighs.find(nei);
                boolean added = old == null;
                if (added) {
                    old = lower.genNeighId(nei);
                    if (old == null) {
                        continue;
                    }
                }
                old.need++;
                old.vrf = vrf;
                old.iface.viaN = old;
                old.viaI = hop.getVia();
                int outIfc = old.viaI.id;
                String act;
                if (added || (old.mac == null)) {
                    act = "add";
                } else {
                    act = "mod";
                    if ((nei.mac.compareTo(old.mac) == 0) && (old.sentIfc == outIfc)) {
                        continue;
                    }
                }
                old.mac = nei.mac;
                old.sentIfc = outIfc;
                lower.sendLine("pwhenei" + afi + "_" + act + " " + old.id + " " + old.adr + " " + old.mac.toEmuStr() + " " + vrf.id + " " + ifc.getMac().toEmuStr() + " " + old.sentIfc + " " + ifc.id + " " + hop.mac.toEmuStr() + " " + old.viaI.getMac().toEmuStr() + " " + ll + " " + lr);
            }
            return;
        }
        if (ifc.ifc.bridgeHed == null) {
            for (int i = 0;; i++) {
                servP4langNei nei = new servP4langNei(ifc, new addrIP());
                nei.mac = new addrMac();
                if (ipi.getL2info(i, nei.adr, nei.mac)) {
                    break;
                }
                if (!ipi.checkConnected(nei.adr)) {
                    continue;
                }
                servP4langNei old = lower.neighs.find(nei);
                boolean added = old == null;
                if (added) {
                    old = lower.genNeighId(nei);
                    if (old == null) {
                        continue;
                    }
                }
                old.need++;
                old.vrf = vrf;
                int outIfc = ifc.id;
                String act;
                if (added || (old.mac == null)) {
                    act = "add";
                } else {
                    act = "mod";
                    if ((nei.mac.compareTo(old.mac) == 0) && (old.sentIfc == outIfc)) {
                        continue;
                    }
                }
                old.mac = nei.mac;
                old.sentIfc = outIfc;
                lower.sendLine("neigh" + afi + "_" + act + " " + old.id + " " + old.adr + " " + old.mac.toEmuStr() + " " + vrf.id + " " + ifc.getMac().toEmuStr() + " " + old.sentIfc);
            }
            return;
        }
        for (int i = 0;; i++) {
            servP4langNei nei = new servP4langNei(ifc, new addrIP());
            nei.mac = new addrMac();
            if (ipi.getL2info(i, nei.adr, nei.mac)) {
                break;
            }
            if (!ipi.checkConnected(nei.adr)) {
                continue;
            }
            servP4langNei old = lower.neighs.find(nei);
            boolean added = old == null;
            if (added) {
                old = lower.genNeighId(nei);
                if (old == null) {
                    continue;
                }
            }
            old.need++;
            old.vrf = vrf;
            ifcBridgeAdr bra = ifc.ifc.bridgeHed.bridgeHed.findMacAddr(nei.mac);
            if (bra == null) {
                continue;
            }
            servP4langIfc oif = lower.findIfc(bra.ifc);
            if (oif != null) {
                int outIfc = oif.id;
                old.viaI = oif;
                String act;
                if (added || (old.mac == null)) {
                    act = "add";
                } else {
                    act = "mod";
                    if ((nei.mac.compareTo(old.mac) == 0) && (old.sentIfc == outIfc)) {
                        continue;
                    }
                }
                old.mac = nei.mac;
                old.sentIfc = outIfc;
                lower.sendLine("neigh" + afi + "_" + act + " " + old.id + " " + old.adr + " " + old.mac.toEmuStr() + " " + vrf.id + " " + ifc.getMac().toEmuStr() + " " + old.sentIfc);
                continue;
            }
            servStackFwd oth = lower.parent.findIfc(lower.parid, bra.ifc);
            if (oth != null) {
                addrIP adr = servStack.forwarder2addr(oth.id);
                servP4langIfc oifc = servP4langUtil.forwarder2iface(lower, oth.id);
                servP4langNei hop = lower.findNei(oifc, adr);
                if (hop == null) {
                    continue;
                }
                if (hop.mac == null) {
                    continue;
                }
                old.viaI = hop.getVia();
                int outIfc = old.viaI.id;
                String act;
                if (added || (old.mac == null)) {
                    act = "add";
                } else {
                    act = "mod";
                    if ((nei.mac.compareTo(old.mac) == 0) && (old.sentIfc == outIfc)) {
                        continue;
                    }
                }
                old.mac = nei.mac;
                old.sentIfc = outIfc;
                lower.sendLine("pwhenei" + afi + "_" + act + " " + old.id + " " + old.adr + " " + old.mac.toEmuStr() + " " + vrf.id + " " + ifc.getMac().toEmuStr() + " " + old.sentIfc + " " + ifc.id + " " + hop.mac.toEmuStr() + " " + old.viaI.getMac().toEmuStr() + " " + lower.parent.bckplnLab[oth.id] + " " + ifc.ifc.bridgeHed.bridgeHed.label.label);
                continue;
            }
            int l = -1;
            addrIP adr = null;
            tabRouteEntry<addrIP> rou = null;
            try {
                clntMplsPwe iface = (clntMplsPwe) bra.ifc.lowerIf;
                l = iface.getLabelRem();
                adr = iface.getRemote();
                if (adr == null) {
                    continue;
                }
                rou = iface.vrf.getFwd(adr).actualU.route(adr);
            } catch (Exception e) {
            }
            try {
                rtrBgpEvpnPeer iface = (rtrBgpEvpnPeer) bra.ifc.lowerIf;
                l = iface.getLabelRem();
                adr = iface.getRemote();
                rou = iface.getForwarder().actualU.route(adr);
            } catch (Exception e) {
            }
            if (l < 1) {
                continue;
            }
            rou = lower.convRou(rou, false);
            if (rou == null) {
                continue;
            }
            servP4langNei hop = lower.findNei(rou.best.iface, rou.best.nextHop);
            if (hop == null) {
                oth = lower.parent.findIfc(lower.parid, rou.best.iface);
                if (oth == null) {
                    continue;
                }
                adr = servStack.forwarder2addr(oth.id);
                servP4langIfc oifc = servP4langUtil.forwarder2iface(lower, oth.id);
                hop = lower.findNei(oifc, adr);
                if (hop == null) {
                    continue;
                }
                if (hop.mac == null) {
                    continue;
                }
                old.viaI = hop.getVia();
                int outIfc = old.viaI.id;
                String act;
                if (added || (old.mac == null)) {
                    act = "add";
                } else {
                    act = "mod";
                    if ((nei.mac.compareTo(old.mac) == 0) && (old.sentIfc == outIfc)) {
                        continue;
                    }
                }
                old.mac = nei.mac;
                old.sentIfc = outIfc;
                lower.sendLine("pwhenei" + afi + "_" + act + " " + old.id + " " + old.adr + " " + old.mac.toEmuStr() + " " + vrf.id + " " + ifc.getMac().toEmuStr() + " " + old.sentIfc + " " + ifc.id + " " + hop.mac.toEmuStr() + " " + old.viaI.getMac().toEmuStr() + " " + lower.parent.bckplnLab[oth.id] + " " + ifc.ifc.bridgeHed.bridgeHed.label.label);
                continue;
            }
            if (hop.mac == null) {
                continue;
            }
            old.viaI = hop.getVia();
            int outIfc = old.viaI.id;
            String act;
            if (added || (old.mac == null)) {
                act = "add";
            } else {
                act = "mod";
                if ((nei.mac.compareTo(old.mac) == 0) && (old.sentIfc == outIfc)) {
                    continue;
                }
            }
            old.mac = nei.mac;
            old.sentIfc = outIfc;
            lower.sendLine("pwhenei" + afi + "_" + act + " " + old.id + " " + old.adr + " " + old.mac.toEmuStr() + " " + vrf.id + " " + ifc.getMac().toEmuStr() + " " + old.sentIfc + " " + ifc.id + " " + hop.mac.toEmuStr() + " " + old.viaI.getMac().toEmuStr() + " " + servP4langUtil.getLabel(rou) + " " + l);
        }
    }

    private void doNatTrns(boolean ipv4, int vrf, tabGen<tabNatTraN> need, tabGen<tabNatTraN> done) {
        String afi;
        if (ipv4) {
            afi = "4";
        } else {
            afi = "6";
        }
        for (int i = 0; i < need.size(); i++) {
            tabNatTraN ntry = need.get(i);
            if (ntry == null) {
                continue;
            }
            if (done.find(ntry) != null) {
                continue;
            }
            switch (ntry.protocol) {
                case ipIcmp4.protoNum:
                case ipIcmp6.protoNum:
                    continue;
            }
            lower.sendLine("nattrns" + afi + "_add " + vrf + " " + servP4langUtil.natTrns2str(ntry));
            done.add(ntry);
        }
        for (int i = done.size() - 1; i >= 0; i--) {
            tabNatTraN ntry = done.get(i);
            if (need.find(ntry) != null) {
                continue;
            }
            switch (ntry.protocol) {
                case ipIcmp4.protoNum:
                case ipIcmp6.protoNum:
                    continue;
            }
            lower.sendLine("nattrns" + afi + "_del " + vrf + " " + servP4langUtil.natTrns2str(ntry));
            done.del(ntry);
        }
    }

    private tabListing<tabAceslstN<addrIP>, addrIP> doNatCfg(boolean ipv4, int vrf, tabListing<tabNatCfgN, addrIP> curr, tabListing<tabAceslstN<addrIP>, addrIP> old, tabListing<tabAceslstN<addrIP>, addrIP> res) {
        tabListing<tabAceslstN<addrIP>, addrIP> need = new tabListing<tabAceslstN<addrIP>, addrIP>();
        for (int i = 0; i < curr.size(); i++) {
            tabNatCfgN ntry = curr.get(i);
            if (ntry.origSrcList == null) {
                continue;
            }
            need.mergeOne(ntry.origSrcList, 0);
        }
        if (old.size() == need.size()) {
            return old;
        }
        String afi;
        if (ipv4) {
            afi = "4";
        } else {
            afi = "6";
        }
        sendAcl(0, "natcfg" + afi + "_del " + vrf + " ", "", "", "", "", ipv4, false, res, null, null, null);
        sendAcl(0, "natcfg" + afi + "_add " + vrf + " ", "", "", "", "", ipv4, false, need, null, null, res);
        return need;
    }

    private tabListing<tabAceslstN<addrIP>, addrIP> doFlwSpc(boolean ipv4, servP4langVrf vrf, ipFwd fwd, tabListing<tabAceslstN<addrIP>, addrIP> sent) {
        List<tabAceslstN<addrIP>> acl = new ArrayList<tabAceslstN<addrIP>>();
        List<tabQosN> qos = new ArrayList<tabQosN>();
        String afi;
        if (ipv4) {
            afi = "4";
        } else {
            afi = "6";
        }
        for (int i = 0;; i++) {
            if (fwd.flowspec == null) {
                break;
            }
            if (fwd.flowspec.getClass(i + 1) == null) {
                break;
            }
            tabQosN curQ = fwd.flowspec.getClass(i);
            if (curQ == null) {
                break;
            }
            tabListing<tabAceslstN<addrIP>, addrIP> curA = curQ.entry.aclMatch;
            tabAceslstN<addrIP> curE = null;
            if (curA != null) {
                curE = curA.get(0);
            }
            if (curE == null) {
                curE = new tabAceslstN<addrIP>(new addrIP());
                curE.action = tabListingEntry.actionType.actPermit;
            }
            acl.add(curE);
            qos.add(curQ);
        }
        for (int i = acl.size(); i < sent.size(); i++) {
            tabAceslstN<addrIP> old = sent.get(i);
            lower.sendLine("flowspec" + afi + "_del " + vrf.id + " " + i + " 0 0 " + servP4langUtil.ace2str(i, ipv4, old, false, false));
        }
        tabListing<tabAceslstN<addrIP>, addrIP> res = new tabListing<tabAceslstN<addrIP>, addrIP>();
        for (int i = 0; i < acl.size(); i++) {
            tabQosN curQ = qos.get(i);
            tabAceslstN<addrIP> curE = acl.get(i);
            tabAceslstN<addrIP> old = sent.get(i);
            curE.sequence = i + 1;
            res.add(curE);
            if (old != null) {
                curQ.entry.hwCntr = old.hwCntr;
            }
            if (curE == old) {
                continue;
            }
            String a;
            if (old == null) {
                a = "_add";
            } else {
                a = "_mod";
            }
            lower.sendLine("flowspec" + afi + a + " " + vrf.id + " " + i + " " + curQ.getBytePerInt() + " " + curQ.getInterval() + " " + servP4langUtil.ace2str(i, ipv4, curE, false, curQ.entry.action == tabListingEntry.actionType.actPermit));
        }
        return res;
    }

    private void doPbrCfg(boolean ipv4, servP4langVrf vrf, tabListing<tabPbrN, addrIP> ned, tabListing<tabPbrN, addrIP> don) {
        int seq = 0;
        for (int i = 0; i < ned.size(); i++) {
            tabPbrN ntry = ned.get(i);
            tabPbrN old = don.get(i);
            if (old == null) {
                old = new tabPbrN();
                old.matcher = new tabListing<tabAceslstN<addrIP>, addrIP>();
            }
            tabPbrN res = new tabPbrN();
            res.matcher = doPbrCfg(seq, ipv4, vrf, ntry, old.matcher, ntry.matcher);
            res.sequence = ntry.sequence;
            don.add(res);
            seq += ntry.matcher.size();
        }
        for (int i = don.size() - 1; i >= 0; i--) {
            tabPbrN ntry = don.get(i);
            if (ned.find(ntry) != null) {
                continue;
            }
            doPbrCfg(seq, ipv4, vrf, null, ntry.matcher, null);
            don.del(ntry);
            seq += ntry.matcher.size();
        }
    }

    private tabListing<tabAceslstN<addrIP>, addrIP> doPbrCfg(int seq, boolean ipv4, servP4langVrf vrf, tabPbrN ntry, tabListing<tabAceslstN<addrIP>, addrIP> old, tabListing<tabAceslstN<addrIP>, addrIP> need) {
        String cmd = "norm";
        String par = "0 0 ";
        if (ntry != null) {
            servP4langVrf tvrf = lower.findVrf(ntry.setVrf);
            if (tvrf == null) {
                return old;
            }
            if (ntry.setHop == null) {
                cmd = "vrf";
                par = tvrf.id + " 0 ";
            } else {
                servP4langNei hop;
                tabRouteEntry<addrIP> rou = null;
                if (ntry.setIfc != null) {
                    hop = lower.findNei(ntry.setIfc, ntry.setHop);
                } else {
                    if (ipv4) {
                        rou = tvrf.vrf.fwd4.actualU.route(ntry.setHop);
                    } else {
                        rou = tvrf.vrf.fwd6.actualU.route(ntry.setHop);
                    }
                    rou = lower.convRou(rou, false);
                    if (rou == null) {
                        return old;
                    }
                    if (rou.best.iface == null) {
                        return old;
                    }
                    addrIP nh = rou.best.nextHop;
                    if (nh == null) {
                        nh = ntry.setHop;
                    }
                    hop = lower.findNei(rou.best.iface, nh);
                }
                if (hop == null) {
                    return old;
                }
                cmd = "hop";
                par = tvrf.id + " " + hop.id + " ";
                int i = -1;
                if (rou != null) {
                    i = servP4langUtil.get1stLabel(rou.best.labelRem);
                }
                if (i > 0) {
                    cmd = "lab";
                    par += i + " ";
                }
            }
        }
        if (old == need) {
            return old;
        }
        String afi;
        if (ipv4) {
            afi = "4";
        } else {
            afi = "6";
        }
        sendAcl(seq, "pbr" + afi, "norm", "norm", "_del " + vrf.id + " " + par, "_del " + vrf.id + " 0 0 ", ipv4, false, old, null, null, null);
        sendAcl(seq, "pbr" + afi, cmd, "norm", "_add " + vrf.id + " " + par, "_add " + vrf.id + " 0 0 ", ipv4, false, need, null, null, null);
        return need;
    }

    private boolean doMroutes(String afi, int vrf, tabLabelEntry lab, ipFwdMcast need, ipFwdMcast done, servP4langStrL<ipFwdMcast, addrIP> nstr, servP4langStrL<ipFwdMcast, addrIP> dstr, boolean bef) {
        int gid = need.group.getHashW() ^ need.source.getHashW() ^ vrf;
        ipFwdIface source = need.iface;
        if (need.upsVrf != null) {
            if (need.upstream == null) {
                return true;
            }
            tabRouteEntry<addrIP> rou = need.upsVrf.actualU.route(need.upstream);
            if (rou == null) {
                return true;
            }
            source = (ipFwdIface) rou.best.iface;
        }
        servP4langIfc ingr = lower.findIfc(source);
        if (ingr == null) {
            servStackFwd oth = lower.parent.findIfc(lower.parid, source);
            if (oth == null) {
                return true;
            }
            tabRouteEntry<addrIP> oru = servStack.forwarder2route(oth.id);
            oru = lower.parid.routes.find(oru);
            if (oru == null) {
                return true;
            }
            if (oru.best.iface == null) {
                return true;
            }
            servStackIfc bck = lower.parid.ifaces.get(oru.best.iface.ifwNum);
            if (bck == null) {
                return true;
            }
            ingr = lower.findIfc(bck.ifc);
            if (ingr == null) {
                return true;
            }
        }
        tabGen<ipFwdIface> nflood = need.flood;
        tabGen<ipFwdIface> dflood = done.flood;
        ipFwdMpmp nlabel = need.label;
        ipFwdMpmp dlabel = done.label;
        ipFwdBier nbier = need.bier;
        ipFwdBier dbier = done.bier;
        if (nlabel == null) {
            nlabel = new ipFwdMpmp(false, new addrIP(), new byte[0]);
        }
        if (dlabel == null) {
            dlabel = new ipFwdMpmp(false, new addrIP(), new byte[0]);
        }
        if (nbier == null) {
            nbier = new ipFwdBier(0);
        }
        if (dbier == null) {
            dbier = new ipFwdBier(0);
        }
        int now = 0;
        if (need.local) {
            nflood = new tabGen<ipFwdIface>();
            nlabel = new ipFwdMpmp(false, new addrIP(), new byte[0]);
            nbier = new ipFwdBier(0);
            now++;
        }
        if (done.local) {
            dflood = new tabGen<ipFwdIface>();
            dlabel = new ipFwdMpmp(false, new addrIP(), new byte[0]);
            dbier = new ipFwdBier(0);
        }
        addrMac mac = need.group.conv2multiMac();
        String act;
        for (int i = 0; i < dbier.fwds.size(); i++) {
            tabLabelBierN ntry = dbier.fwds.get(i);
            if (nbier.fwds.find(ntry) != null) {
                continue;
            }
            servP4langNei hop = lower.findNei(ntry.iface, ntry.hop);
            if (hop == null) {
                continue;
            }
            if (tabLabelBier.bsl2num(ntry.bsl) != 256) {
                continue;
            }
            String a = servP4langUtil.getBierLabs(ntry, tabLabelBier.bsl2msk(ntry.bsl), 0);
            servP4langIfc ifc = hop.getVia();
            if (ntry.vpnlab == 0) {
                lower.sendLine("mbierroute" + afi + "_del " + vrf + " " + gid + " " + need.group + " " + need.source + " " + ingr.id + " " + ifc.getMcast(gid, hop).id + " " + hop.id + " " + ntry.label + " " + ifc.id + " " + dbier.srcId + " 0" + a);
            } else {
                lower.sendLine("mbiervpnrou" + afi + "_del " + vrf + " " + gid + " " + need.group + " " + need.source + " " + ingr.id + " " + ifc.getMcast(gid, hop).id + " " + hop.id + " " + ntry.label + " " + ntry.vpnlab + " " + ifc.id + " " + dbier.srcId + " 0" + a);
            }
        }
        for (int i = 0; i < nbier.fwds.size(); i++) {
            tabLabelBierN ntry = nbier.fwds.get(i);
            if (tabLabelBier.bsl2num(ntry.bsl) != 256) {
                continue;
            }
            if (dbier.fwds.find(ntry) != null) {
                act = "mod";
            } else {
                act = "add";
            }
            servP4langNei hop = lower.findNei(ntry.iface, ntry.hop);
            if (hop == null) {
                continue;
            }
            String a = servP4langUtil.getBierLabs(ntry, tabLabelBier.bsl2msk(ntry.bsl), 0);
            servP4langIfc ifc = hop.getVia();
            if (ntry.vpnlab == 0) {
                lower.sendLine("mbierroute" + afi + "_" + act + " " + vrf + " " + gid + " " + need.group + " " + need.source + " " + ingr.id + " " + ifc.getMcast(gid, hop).id + " " + hop.id + " " + ntry.label + " " + ifc.id + " " + nbier.srcId + " 0" + a);
            } else {
                lower.sendLine("mbiervpnrou" + afi + "_" + act + " " + vrf + " " + gid + " " + need.group + " " + need.source + " " + ingr.id + " " + ifc.getMcast(gid, hop).id + " " + hop.id + " " + ntry.label + " " + ntry.vpnlab + " " + ifc.id + " " + nbier.srcId + " 0" + a);
            }
            now++;
        }
        for (int i = 0; i < dlabel.neighs.size(); i++) {
            ipFwdMpNe ntry = dlabel.neighs.get(i);
            if (ntry.labelR < 0) {
                continue;
            }
            if (nlabel.neighs.find(ntry) != null) {
                continue;
            }
            servP4langNei hop = lower.findNei(ntry.iface, ntry.addr);
            if (hop == null) {
                continue;
            }
            servP4langIfc ifc = hop.getVia();
            lower.sendLine("mlabroute" + afi + "_del " + vrf + " " + gid + " " + need.group + " " + need.source + " " + ingr.id + " " + ifc.getMcast(gid, hop).id + " " + hop.id + " " + ntry.labelR + " " + ifc.id);
        }
        for (int i = 0; i < nlabel.neighs.size(); i++) {
            ipFwdMpNe ntry = nlabel.neighs.get(i);
            if (ntry.labelR < 0) {
                continue;
            }
            if (dlabel.neighs.find(ntry) != null) {
                act = "mod";
            } else {
                act = "add";
            }
            servP4langNei hop = lower.findNei(ntry.iface, ntry.addr);
            if (hop == null) {
                continue;
            }
            servP4langIfc ifc = hop.getVia();
            lower.sendLine("mlabroute" + afi + "_" + act + " " + vrf + " " + gid + " " + need.group + " " + need.source + " " + ingr.id + " " + ifc.getMcast(gid, hop).id + " " + hop.id + " " + ntry.labelR + " " + ifc.id);
            now++;
        }
        for (int i = 0; i < dstr.list.size(); i++) {
            addrIP bck = dstr.list.get(i);
            if (nstr.list.find(bck) != null) {
                continue;
            }
            servP4langIfc ifc = servP4langUtil.forwarder2iface(lower, servStack.addr2forwarder(bck));
            servP4langNei hop = lower.findNei(ifc, bck);
            if (hop == null) {
                continue;
            }
            ifc = hop.getVia();
            lower.sendLine("mlabroute" + afi + "_del " + vrf + " " + gid + " " + need.group + " " + need.source + " " + ingr.id + " " + ifc.getMcast(gid, hop).id + " " + hop.id + " " + lab + " " + ifc.id);
        }
        for (int i = 0; i < nstr.list.size(); i++) {
            addrIP bck = nstr.list.get(i);
            if (dstr.list.find(bck) != null) {
                act = "mod";
            } else {
                act = "add";
            }
            servP4langIfc ifc = servP4langUtil.forwarder2iface(lower, servStack.addr2forwarder(bck));
            servP4langNei hop = lower.findNei(ifc, bck);
            if (hop == null) {
                continue;
            }
            ifc = hop.getVia();
            lower.sendLine("mlabroute" + afi + "_" + act + " " + vrf + " " + gid + " " + need.group + " " + need.source + " " + ingr.id + " " + ifc.getMcast(gid, hop).id + " " + hop.id + " " + lab + " " + ifc.id);
            now++;
        }
        for (int i = 0; i < dflood.size(); i++) {
            ipFwdIface ntry = dflood.get(i);
            if (nflood.find(ntry) != null) {
                continue;
            }
            servP4langIfc ifc = lower.findIfc(ntry);
            if (ifc == null) {
                continue;
            }
            lower.sendLine("mroute" + afi + "_del " + vrf + " " + gid + " " + need.group + " " + need.source + " " + ingr.id + " " + ifc.getMcast(gid, null).id + " " + ifc.id + " " + ifc.getMac().toEmuStr() + " " + mac.toEmuStr());
        }
        for (int i = 0; i < nflood.size(); i++) {
            ipFwdIface ntry = nflood.get(i);
            servP4langIfc ifc = lower.findIfc(ntry);
            if (ifc == null) {
                continue;
            }
            if (dflood.find(ntry) != null) {
                act = "mod";
            } else {
                act = "add";
            }
            now++;
            if (ifc.viaN != null) {
                lower.sendLine("mneiroute" + afi + "_" + act + " " + vrf + " " + gid + " " + need.group + " " + need.source + " " + ingr.id + " " + ifc.viaN.getVia().getMcast(gid, null).id + " " + ifc.viaN.id + " " + ifc.id);
                continue;
            }
            if (ntry.mcastAsBcast) {
                lower.sendLine("mroute" + afi + "_" + act + " " + vrf + " " + gid + " " + need.group + " " + need.source + " " + ingr.id + " " + ifc.getMcast(gid, null).id + " " + ifc.id + " " + ifc.getMac().toEmuStr() + " " + addrMac.getBroadcast().toEmuStr());
                continue;
            }
            if (!ntry.mcastAsUcast) {
                lower.sendLine("mroute" + afi + "_" + act + " " + vrf + " " + gid + " " + need.group + " " + need.source + " " + ingr.id + " " + ifc.getMcast(gid, null).id + " " + ifc.id + " " + ifc.getMac().toEmuStr() + " " + mac.toEmuStr());
                continue;
            }
            addrIP adrI = new addrIP();
            addrMac adrM = new addrMac();
            if (ntry.lower.getL2info(0, adrI, adrM)) {
                lower.sendLine("mroute" + afi + "_" + act + " " + vrf + " " + gid + " " + need.group + " " + need.source + " " + ingr.id + " " + ifc.getMcast(gid, null).id + " " + ifc.id + " " + ifc.getMac().toEmuStr() + " " + mac.toEmuStr());
                continue;
            }
            servP4langNei nei = lower.findNei(ntry, adrI);
            if (nei == null) {
                lower.sendLine("mroute" + afi + "_" + act + " " + vrf + " " + gid + " " + need.group + " " + need.source + " " + ingr.id + " " + ifc.getMcast(gid, null).id + " " + ifc.id + " " + ifc.getMac().toEmuStr() + " " + adrM.toEmuStr());
                continue;
            }
            lower.sendLine("mneiroute" + afi + "_" + act + " " + vrf + " " + gid + " " + need.group + " " + need.source + " " + ingr.id + " " + nei.getVia().getMcast(gid, null).id + " " + nei.id + " " + ifc.id);
        }
        if (bef) {
            act = "mod";
            if (now < 1) {
                act = "del";
            }
        } else {
            act = "add";
        }
        lower.sendLine("mlocal" + afi + "_" + (need.local ? "add " : "del ") + vrf + " " + gid + " " + need.group + " " + need.source + " " + ingr.id + " " + act);
        return false;
    }

    private void doMroutes(boolean ipv4, int vrf, tabLabelEntry lab, tabGen<ipFwdMcast> need, tabGen<ipFwdMcast> done, tabGen<servP4langStrL<ipFwdMcast, addrIP>> store) {
        String afi;
        if (ipv4) {
            afi = "4";
        } else {
            afi = "6";
        }
        for (int i = 0; i < need.size(); i++) {
            ipFwdMcast ntry = need.get(i);
            if (ntry == null) {
                continue;
            }
            ntry = ntry.copyBytes();
            servP4langStrL<ipFwdMcast, addrIP> str = new servP4langStrL<ipFwdMcast, addrIP>(ntry);
            str.list = lower.parent.mergeMcast(lower.parid, ntry.flood, ntry.iface);
            if (ntry.bier != null) {
                tabGen<tabLabelBierN> res = lower.parent.mergeBier(lower.parid, ntry.bier.fwds, ntry.iface);
                for (int o = 0; o < res.size(); o++) {
                    tabLabelBierN cur = res.get(o);
                    str.list.add(cur.hop);
                }
            }
            ipFwdMcast old = done.find(ntry);
            servP4langStrL<ipFwdMcast, addrIP> ostr = store.find(str);
            boolean bef;
            if (old != null) {
                if ((!ntry.differs(old)) && (!str.differs(ostr))) {
                    continue;
                }
                bef = true;
            } else {
                old = new ipFwdMcast(ntry.group, ntry.source);
                ostr = new servP4langStrL<ipFwdMcast, addrIP>(old);
                bef = false;
            }
            if (doMroutes(afi, vrf, lab, ntry, old, str, ostr, bef)) {
                continue;
            }
            done.put(ntry);
            store.put(str);
        }
        for (int i = done.size() - 1; i >= 0; i--) {
            ipFwdMcast ntry = done.get(i);
            if (need.find(ntry) != null) {
                continue;
            }
            servP4langStrL<ipFwdMcast, addrIP> str = new servP4langStrL<ipFwdMcast, addrIP>(ntry);
            str = store.find(str);
            ipFwdMcast empty = new ipFwdMcast(ntry.group, ntry.source);
            empty.iface = ntry.iface;
            servP4langStrL<ipFwdMcast, addrIP> estr = new servP4langStrL<ipFwdMcast, addrIP>(ntry);
            if (doMroutes(afi, vrf, lab, empty, ntry, estr, str, true)) {
                continue;
            }
            done.del(ntry);
            store.del(str);
        }
    }

    private void doSockets(boolean ipv4, int vrf, int prt, tabConnect<addrIP, prtGenServ> need, tabConnect<addrIP, prtGenServ> done) {
        String afi;
        if (ipv4) {
            afi = "4";
        } else {
            afi = "6";
        }
        for (int i = 0; i < done.size(); i++) {
            tabConnectEntry<addrIP, prtGenServ> ntry = done.read(i);
            prtGenServ old = need.get(ntry.iface, null, ntry.local, ntry.remote);
            if (old != null) {
                continue;
            }
            done.del(ntry.iface, null, ntry.local, ntry.remote);
            servP4langIfc fif = lower.findIfc(ntry.iface);
            String sif = "-1";
            if (fif != null) {
                sif = "" + fif.id;
            }
            lower.sendLine("socket" + afi + "_del " + vrf + " " + prt + " " + sif + " " + ntry.local + " " + ntry.remote);
        }
        for (int i = 0; i < need.size(); i++) {
            tabConnectEntry<addrIP, prtGenServ> ntry = need.read(i);
            if (ntry == null) {
                continue;
            }
            prtGenServ old = done.get(ntry.iface, null, ntry.local, ntry.remote);
            if (old != null) {
                continue;
            }
            done.add(ntry.iface, null, ntry.local, ntry.remote, new prtGenServ(), "save");
            servP4langIfc fif = lower.findIfc(ntry.iface);
            String sif = "-1";
            if (fif != null) {
                sif = "" + fif.id;
            }
            lower.sendLine("socket" + afi + "_add " + vrf + " " + prt + " " + sif + " " + ntry.local + " " + ntry.remote);
        }
    }

    private void doIndexes(String beg, int vrf, tabGen<tabIndex<addrIP>> need, tabGen<tabIndex<addrIP>> done, tabRoute<addrIP> routes, tabGen<servP4langStrI<tabIndex<addrIP>>> store) {
        for (int i = 0; i < need.size(); i++) {
            tabIndex<addrIP> ntry = need.get(i);
            if (ntry == null) {
                continue;
            }
            ntry = ntry.copyBytes();
            tabRouteEntry<addrIP> rou = routes.find(ntry.prefix);
            if (rou == null) {
                continue;
            }
            if (rou.best.nextHop == null) {
                continue;
            }
            servP4langNei hop = lower.findNei(rou.best.iface, rou.best.nextHop);
            if (hop == null) {
                servStackFwd oth = lower.parent.findIfc(lower.parid, rou.best.iface);
                if (oth == null) {
                    continue;
                }
                addrIP adr = servStack.forwarder2addr(oth.id);
                servP4langIfc ifc = servP4langUtil.forwarder2iface(lower, oth.id);
                hop = lower.findNei(ifc, adr);
                if (hop == null) {
                    continue;
                }
            }
            servP4langStrI<tabIndex<addrIP>> str = new servP4langStrI<tabIndex<addrIP>>(ntry);
            str.stor1 = hop.id;
            tabIndex<addrIP> old = done.find(ntry);
            String act = "add";
            if (old != null) {
                if (!ntry.differs(old) && !str.differs(store)) {
                    continue;
                }
                act = "mod";
            }
            done.put(ntry);
            store.put(str);
            lower.sendLine(beg + "polkaidx_" + act + " " + ntry.index + " " + vrf + " " + hop.id);
        }
        for (int i = done.size() - 1; i >= 0; i--) {
            tabIndex<addrIP> ntry = done.get(i);
            if (need.find(ntry) != null) {
                continue;
            }
            done.del(ntry);
            store.del(new servP4langStrI<tabIndex<addrIP>>(ntry));
            lower.sendLine(beg + "polkaidx_del " + ntry.index + " " + vrf + " 0");
        }
    }

    private boolean doRemRou(String afi, String act, String a, int vrf, tabLabelEntry cml, tabRouteEntry<addrIP> ntry, tabRouteEntry<addrIP> recur) {
        servStackFwd oth = lower.parent.findIfc(lower.parid, recur.best.iface);
        if (oth == null) {
            return false;
        }
        addrIP adr = servStack.forwarder2addr(oth.id);
        servP4langIfc ifc = servP4langUtil.forwarder2iface(lower, oth.id);
        servP4langNei hop = lower.findNei(ifc, adr);
        if (hop == null) {
            return false;
        }
        lower.sendLine("vpnroute" + afi + "_" + act + " " + a + " " + hop.id + " " + ntry.best.nextHop + " " + vrf + " " + lower.parent.bckplnLab[oth.id] + " " + cml.label);
        return true;
    }

    private void doRoutes(boolean ipv4, int vrf, tabLabelEntry cml, tabRoute<addrIP> need, tabRoute<addrIP> done, tabGen<servP4langStrI<tabRouteEntry<addrIP>>> store, boolean cmpr, tabListing<tabPrfxlstN, addrIP> prflst, tabListing<tabRtrmapN, addrIP> roumap, tabListing<tabRtrplcN, addrIP> roupol) {
        if (cmpr) {
            need = new tabRoute<addrIP>(need);
            tabRouteUtil.compressTable(rtrBgpUtil.sfiUnicast, need, null);
        }
        String afi;
        if (ipv4) {
            afi = "4";
        } else {
            afi = "6";
        }
        for (int i = 0; i < need.size(); i++) {
            tabRouteEntry<addrIP> ntry = need.get(i);
            ntry = tabRoute.doUpdateEntry(rtrBgpUtil.sfiUnicast, 0, ntry, roumap, roupol, prflst);
            if (ntry == null) {
                continue;
            }
            tabRouteEntry<addrIP> old = done.find(ntry);
            if ((ntry.best.iface == null) && (ntry.best.rouTab != null)) {
                tabRouteEntry<addrIP> recur;
                if (ntry.best.segrouPrf == null) {
                    recur = ntry.best.rouTab.actualU.route(ntry.best.nextHop);
                } else {
                    recur = ntry.best.rouTab.actualU.route(ntry.best.segrouPrf);
                }
                recur = lower.convRou(recur, true);
                if (recur == null) {
                    continue;
                }
                if (recur.best.rouTyp == tabRouteAttr.routeType.conn) {
                    recur.best.nextHop = ntry.best.nextHop.copyBytes();
                }
                if (recur.best.nextHop == null) {
                    continue;
                }
                servP4langNei hop = lower.findNei(recur.best.iface, recur.best.nextHop);
                if (hop == null) {
                    String a;
                    if (ipv4) {
                        a = "" + addrPrefix.ip2ip4(ntry.prefix);
                    } else {
                        a = "" + addrPrefix.ip2ip6(ntry.prefix);
                    }
                    String act = "add";
                    if (old != null) {
                        if (ntry.differs(tabRoute.addType.notyet, old) == 0) {
                            continue;
                        }
                        act = "mod";
                    }
                    if (doRemRou(afi, act, a, vrf, cml, ntry, recur)) {
                        done.add(tabRoute.addType.always, ntry, true, true);
                    }
                    continue;
                }
                servP4langStrI<tabRouteEntry<addrIP>> str = new servP4langStrI<tabRouteEntry<addrIP>>(ntry);
                str.stor1 = hop.id;
                str.stor2 = servP4langUtil.getLabel(recur);
                String act = "add";
                if (old != null) {
                    if ((ntry.differs(tabRoute.addType.notyet, old) == 0) && !str.differs(store)) {
                        continue;
                    }
                    act = "mod";
                }
                done.add(tabRoute.addType.always, ntry, true, true);
                store.put(str);
                String a;
                if (ipv4) {
                    a = "" + addrPrefix.ip2ip4(ntry.prefix);
                } else {
                    a = "" + addrPrefix.ip2ip6(ntry.prefix);
                }
                if (ntry.best.segrouPrf == null) {
                    lower.sendLine("vpnroute" + afi + "_" + act + " " + a + " " + hop.id + " " + ntry.best.nextHop + " " + vrf + " " + servP4langUtil.getLabel(recur) + " " + servP4langUtil.getLabel(ntry));
                } else {
                    lower.sendLine("srvroute" + afi + "_" + act + " " + a + " " + hop.id + " " + ntry.best.nextHop + " " + vrf + " " + ntry.best.segrouPrf);
                }
                continue;
            }
            String act = "add";
            if (old != null) {
                if (ntry.best.nextHop != null) {
                    lower.findNei(ntry.best.iface, ntry.best.nextHop);
                }
                if (ntry.differs(tabRoute.addType.notyet, old) == 0) {
                    continue;
                }
                act = "mod";
            }
            done.add(tabRoute.addType.always, ntry, true, true);
            old = lower.convRou(ntry, true);
            if (old == null) {
                done.del(ntry);
                continue;
            }
            ntry = old;
            String a;
            if (ipv4) {
                a = "" + addrPrefix.ip2ip4(ntry.prefix);
            } else {
                a = "" + addrPrefix.ip2ip6(ntry.prefix);
            }
            if (ntry.best.iface != null) {
                ipFwdIface ifc = (ipFwdIface) ntry.best.iface;
                if (ifc.ifwTyp == tabRouteIface.ifaceType.nul) {
                    lower.sendLine("droproute" + afi + "_" + act + " " + a + " " + vrf);
                    continue;
                }
            }
            if (ntry.best.nextHop == null) {
                servP4langIfc fif = lower.findIfc(ntry.best.iface);
                String sif = "-1";
                if (fif != null) {
                    sif = "" + fif.id;
                } else {
                    if (doRemRou(afi, act, a, vrf, cml, ntry, ntry)) {
                        continue;
                    }
                }
                lower.sendLine("myaddr" + afi + "_" + act + " " + a + " " + sif + " " + vrf);
                continue;
            }
            servP4langNei hop = lower.findNei(ntry.best.iface, ntry.best.nextHop);
            if (hop == null) {
                if (doRemRou(afi, act, a, vrf, cml, ntry, ntry)) {
                    continue;
                }
                done.del(ntry);
                continue;
            }
            if (ntry.best.attribAs == ifcPolka.type) {
                lower.sendLine("polroute" + afi + "_" + act + " " + a + " " + hop.id + " " + ntry.best.nextHop + " " + vrf + " " + bits.toHex(ntry.best.attribVal));
                continue;
            }
            if (ntry.best.attribAs == (ifcPolka.type + 1)) {
                lower.sendLine("mpolroute" + afi + "_" + act + " " + a + " " + hop.id + " " + ntry.best.nextHop + " " + vrf + " " + bits.toHex(ntry.best.attribVal));
                continue;
            }
            if (ntry.best.labelRem != null) {
                lower.sendLine("labroute" + afi + "_" + act + " " + a + " " + hop.id + " " + ntry.best.nextHop + " " + vrf + " " + servP4langUtil.getLabel(ntry));
                continue;
            }
            lower.sendLine("route" + afi + "_" + act + " " + a + " " + hop.id + " " + ntry.best.nextHop + " " + vrf + " " + hop.iface.id);
        }
        for (int i = done.size() - 1; i >= 0; i--) {
            tabRouteEntry<addrIP> ntry = done.get(i);
            if (need.find(ntry) != null) {
                continue;
            }
            if ((ntry.best.iface == null) && (ntry.best.rouTab != null)) {
                tabRouteEntry<addrIP> old;
                if (ntry.best.segrouPrf == null) {
                    old = ntry.best.rouTab.actualU.route(ntry.best.nextHop);
                } else {
                    old = ntry.best.rouTab.actualU.route(ntry.best.segrouPrf);
                }
                old = lower.convRou(old, true);
                if (old == null) {
                    continue;
                }
                if (old.best.rouTyp == tabRouteAttr.routeType.conn) {
                    old.best.nextHop = ntry.best.nextHop.copyBytes();
                }
                servP4langNei hop = lower.findNei(old.best.iface, old.best.nextHop);
                if (hop == null) {
                    continue;
                }
                done.del(ntry);
                store.del(new servP4langStrI<tabRouteEntry<addrIP>>(ntry));
                String a;
                if (ipv4) {
                    a = "" + addrPrefix.ip2ip4(ntry.prefix);
                } else {
                    a = "" + addrPrefix.ip2ip6(ntry.prefix);
                }
                if (ntry.best.segrouPrf == null) {
                    lower.sendLine("vpnroute" + afi + "_del " + a + " " + hop.id + " " + ntry.best.nextHop + " " + vrf + " " + servP4langUtil.getLabel(old) + " " + servP4langUtil.getLabel(ntry));
                } else {
                    lower.sendLine("srvroute" + afi + "_del " + a + " " + hop.id + " " + ntry.best.nextHop + " " + vrf + " " + ntry.best.segrouPrf);
                }
                continue;
            }
            done.del(ntry);
            ntry = lower.convRou(ntry, true);
            if (ntry == null) {
                continue;
            }
            String a;
            if (ipv4) {
                a = "" + addrPrefix.ip2ip4(ntry.prefix);
            } else {
                a = "" + addrPrefix.ip2ip6(ntry.prefix);
            }
            if (ntry.best.nextHop == null) {
                servP4langIfc fif = lower.findIfc(ntry.best.iface);
                String sif = "-1";
                if (fif != null) {
                    sif = "" + fif.id;
                }
                lower.sendLine("myaddr" + afi + "_del " + a + " " + sif + " " + vrf);
                continue;
            }
            servP4langNei hop = lower.findNei(ntry.best.iface, ntry.best.nextHop);
            if (hop == null) {
                servP4langIfc fif = lower.findIfc(ntry.best.iface);
                String sif = "-1";
                if (fif != null) {
                    sif = "" + fif.id;
                }
                lower.sendLine("myaddr" + afi + "_del " + a + " " + sif + " " + vrf);
                continue;
            }
            if (ntry.best.labelRem != null) {
                lower.sendLine("labroute" + afi + "_del " + a + " " + hop.id + " " + ntry.best.nextHop + " " + vrf + " " + servP4langUtil.getLabel(ntry));
                continue;
            }
            lower.sendLine("route" + afi + "_del " + a + " " + hop.id + " " + ntry.best.nextHop + " " + vrf + " " + hop.iface.id);
        }
    }

    private void sendQos(String dir, boolean ipv4, String prt, int ifc, tabQos qos, List<tabListing<tabAceslstN<addrIP>, addrIP>> done, List<tabListing<tabAceslstN<addrIP>, addrIP>> sent, List<Long> byts, List<Integer> ints) {
        int seq = 0;
        if (qos == null) {
            for (int i = 0; i < sent.size(); i++) {
                seq = sendAcl(seq, dir + "qos" + prt + "_del " + ifc + " " + (ifc + i) + " ", "", "", "", "", ipv4, true, sent.get(i), null, null, null);
                if (ipv4) {
                    lower.sendLine(dir + "qos_del " + (ifc + i) + " " + byts.get(i) + " " + ints.get(i));
                }
            }
            sent.clear();
            done.clear();
            byts.clear();
            ints.clear();
            return;
        }
        boolean ned = false;
        int saw = 0;
        for (int i = 0;; i++) {
            tabQosN cur = qos.getClass(i);
            if (cur == null) {
                break;
            }
            if (cur.entry.aclMatch == null) {
                continue;
            }
            long o = cur.getBytePerInt();
            int p = cur.getInterval();
            String a;
            if (saw >= done.size()) {
                done.add(null);
                sent.add(new tabListing<tabAceslstN<addrIP>, addrIP>());
                byts.add(-1L);
                ints.add(-1);
                ned = true;
                a = "add";
            } else {
                ned |= o != byts.get(saw);
                ned |= p != ints.get(saw);
                a = "mod";
            }
            if (!ned) {
                ned = servP4langUtil.needAcl(done.get(saw), cur.entry.aclMatch, null, null, null, sent.get(saw));
            }
            if (!ned) {
                saw++;
                continue;
            }
            if (ipv4) {
                lower.sendLine(dir + "qos_" + a + " " + (ifc + saw) + " " + o + " " + p);
            }
            sendAcl(seq, dir + "qos" + prt + "_del " + ifc + " " + (ifc + saw) + " ", "", "", "", "", ipv4, true, sent.get(saw), null, null, null);
            byts.set(saw, o);
            ints.set(saw, p);
            done.set(saw, cur.entry.aclMatch);
            seq = sendAcl(seq, dir + "qos" + prt + "_add " + ifc + " " + (ifc + saw) + " ", "", "", "", "", ipv4, true, cur.entry.aclMatch, null, null, sent.get(saw));
            saw++;
        }
        for (int i = saw; i < sent.size(); i++) {
            seq = sendAcl(seq, dir + "qos" + prt + "_del " + ifc + " " + (ifc + i) + " ", "", "", "", "", ipv4, true, sent.get(saw), null, null, null);
            if (ipv4) {
                lower.sendLine(dir + "qos_del " + (ifc + i) + " " + byts.get(saw) + " " + ints.get(saw));
            }
            done.remove(saw);
            sent.remove(saw);
            byts.remove(saw);
            ints.remove(saw);
        }
    }

    private int sendAcl(int seq, String pre1, String perm, String deny, String pre2, String pre3, boolean ipv4, boolean check, tabListing<tabAceslstN<addrIP>, addrIP> iface, tabListing<tabAceslstN<addrIP>, addrIP> infra, tabSession sess, tabListing<tabAceslstN<addrIP>, addrIP> res) {
        if (res == null) {
            res = new tabListing<tabAceslstN<addrIP>, addrIP>();
        }
        res.clear();
        if (iface != null) {
            iface = tabAceslstN.unrollAcl(iface);
        }
        if (infra != null) {
            infra = tabAceslstN.unrollAcl(infra);
        }
        res.mergeTwo(infra, iface);
        for (int i = 0; i < res.size(); i++) {
            tabAceslstN<addrIP> ace = res.get(i);
            String a = servP4langUtil.ace2str(seq + i, ipv4, ace, check, false);
            if (a == null) {
                continue;
            }
            if (ace.action == tabListingEntry.actionType.actPermit) {
                lower.sendLine(pre1 + perm + pre2 + a);
            } else {
                lower.sendLine(pre1 + deny + pre3 + a);
            }
        }
        seq += res.size();
        if (sess == null) {
            return seq;
        }
        tabAceslstN<addrIP> ace = new tabAceslstN<addrIP>(new addrIP());
        ace.reflectFwd = new tabListing<tabAceslstN<addrIP>, addrIP>();
        ace.action = tabListingEntry.actionType.actPermit;
        ace.sequence = res.nextseq();
        res.add(ace);
        lower.sendLine(pre1 + perm + pre2 + servP4langUtil.ace2str(seq, ipv4, ace, check, false));
        return seq + 1;
    }

    private tabSession sendSess(int ifc, boolean ipv4, tabSession old, tabSession ned) {
        if (ned == null) {
            if (old == null) {
                return null;
            }
            ned = new tabSession(old.bidir, old.timeout);
        }
        if (old == null) {
            old = new tabSession(ned.bidir, ned.timeout);
        }
        String afi;
        if (ipv4) {
            afi = "4";
        } else {
            afi = "6";
        }
        tabGen<tabSessionEntry> nds;
        if (ned.master == null) {
            nds = ned.connects;
        } else {
            nds = ned.master.connects;
        }
        for (int i = old.connects.size() - 1; i >= 0; i--) {
            tabSessionEntry ntry = old.connects.get(i);
            if (nds.find(ntry) != null) {
                continue;
            }
            switch (ntry.ipPrt) {
                case ipIcmp4.protoNum:
                case ipIcmp6.protoNum:
                    continue;
            }
            lower.sendLine("inspect" + afi + "_del " + ifc + " " + servP4langUtil.sess2str(ntry));
            old.connects.del(ntry);
        }
        for (int i = 0; i < nds.size(); i++) {
            tabSessionEntry ntry = nds.get(i);
            if (ntry == null) {
                continue;
            }
            if (old.connects.find(ntry) != null) {
                continue;
            }
            if (ntry.evaluating != null) {
                continue;
            }
            switch (ntry.ipPrt) {
                case ipIcmp4.protoNum:
                case ipIcmp6.protoNum:
                    continue;
            }
            lower.sendLine("inspect" + afi + "_add " + ifc + " " + servP4langUtil.sess2str(ntry));
            old.connects.put(ntry);
        }
        return old;
    }

}
