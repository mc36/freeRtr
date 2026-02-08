package org.freertr.serv;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrMac;
import org.freertr.addr.addrPrefix;
import org.freertr.enc.encTlv;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipFwdMcast;
import org.freertr.ip.ipIfc4;
import org.freertr.ip.ipIfc4arp;
import org.freertr.ip.ipIfc6;
import org.freertr.ip.ipMpls;
import org.freertr.pack.packHolder;
import org.freertr.pack.packOpenflow;
import org.freertr.pipe.pipeSide;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabLabel;
import org.freertr.tab.tabLabelDup;
import org.freertr.tab.tabLabelEntry;
import org.freertr.tab.tabRoute;
import org.freertr.tab.tabRouteEntry;
import org.freertr.util.bits;
import org.freertr.util.logger;

/**
 * one openflow transmitter
 *
 * @author matecsaba
 */
class servOpenflowTx implements Runnable {

    /**
     * true if the transmitter is working
     */
    protected boolean working = true;

    private final pipeSide pipe;

    private final servOpenflow lower;

    private tabGen<servOpenflowFlw> tabGroup = new tabGen<servOpenflowFlw>();

    private tabGen<servOpenflowFlw> tabPort = new tabGen<servOpenflowFlw>();

    private tabGen<servOpenflowFlw> tabMpls = new tabGen<servOpenflowFlw>();

    private tabGen<servOpenflowFlw> tabIpv4 = new tabGen<servOpenflowFlw>();

    private tabGen<servOpenflowFlw> tabIpv6 = new tabGen<servOpenflowFlw>();

    /**
     * create the transmitter
     *
     * @param stream
     * @param parent
     */
    public servOpenflowTx(pipeSide stream, servOpenflow parent) {
        pipe = stream;
        lower = parent;
        logger.startThread(this);
    }

    public void run() {
        try {
            doWord();
        } catch (Exception e) {
            logger.traceback(e);
        }
        pipe.setClose();
    }

    private void sendTable(packHolder pckB, packOpenflow pckO, int tab, tabGen<servOpenflowFlw> done, tabGen<servOpenflowFlw> need) {
        for (int i = done.size() - 1; i >= 0; i--) {
            servOpenflowFlw ntry = done.get(i);
            if (need.find(ntry) != null) {
                continue;
            }
            done.del(ntry);
            pckB.clear();
            pckO.putMatchBuf(pckB, ntry.match);
            pckO.createFlowMod(pckB, ntry.cookie, tab, packOpenflow.flowCmdDels, ntry.prio);
            lower.sendPack(pckB, pckO);
        }
        for (int i = 0; i < need.size(); i++) {
            servOpenflowFlw ntry = need.get(i);
            servOpenflowFlw old = done.find(ntry);
            if (old == null) {
                done.put(ntry);
                pckB.clear();
                pckB.putCopy(ntry.action, 0, 0, ntry.action.length);
                pckB.putSkip(ntry.action.length);
                pckB.merge2beg();
                pckO.putMatchBuf(pckB, ntry.match);
                pckO.createFlowMod(pckB, ntry.cookie, tab, packOpenflow.flowCmdAdd, ntry.prio);
                lower.sendPack(pckB, pckO);
                continue;
            }
            if (ntry.sameAct(old)) {
                continue;
            }
            done.put(ntry);
            pckB.clear();
            pckB.putCopy(ntry.action, 0, 0, ntry.action.length);
            pckB.putSkip(ntry.action.length);
            pckB.merge2beg();
            pckO.putMatchBuf(pckB, ntry.match);
            pckO.createFlowMod(pckB, ntry.cookie, tab, packOpenflow.flowCmdMdfs, ntry.prio);
            lower.sendPack(pckB, pckO);
        }
    }

    private void sendGroup(packHolder pckB, packOpenflow pckO, tabGen<servOpenflowFlw> done, tabGen<servOpenflowFlw> need) {
        for (int i = done.size() - 1; i >= 0; i--) {
            servOpenflowFlw ntry = done.get(i);
            if (need.find(ntry) != null) {
                continue;
            }
            done.del(ntry);
            pckB.clear();
            pckO.createGroupMod(pckB, packOpenflow.groupCmdDel, packOpenflow.groupTypAll, ntry.cookie);
            lower.sendPack(pckB, pckO);
        }
        for (int i = 0; i < need.size(); i++) {
            servOpenflowFlw ntry = need.get(i);
            servOpenflowFlw old = done.find(ntry);
            if (old == null) {
                done.put(ntry);
                pckB.clear();
                pckB.putCopy(ntry.action, 0, 0, ntry.action.length);
                pckB.putSkip(ntry.action.length);
                pckB.merge2beg();
                pckO.createGroupMod(pckB, packOpenflow.groupCmdAdd, packOpenflow.groupTypAll, ntry.cookie);
                lower.sendPack(pckB, pckO);
                continue;
            }
            if (ntry.sameAct(old)) {
                continue;
            }
            done.put(ntry);
            pckB.clear();
            pckB.putCopy(ntry.action, 0, 0, ntry.action.length);
            pckB.putSkip(ntry.action.length);
            pckB.merge2beg();
            pckO.createGroupMod(pckB, packOpenflow.groupCmdMdf, packOpenflow.groupTypAll, ntry.cookie);
            lower.sendPack(pckB, pckO);
        }
    }

    private void addTable(tabGen<servOpenflowFlw> trg, tabGen<servOpenflowFlw> src, servOpenflowFlw ntry) {
        servOpenflowFlw old = src.find(ntry);
        if (old != null) {
            ntry.cookie = old.cookie;
            trg.put(ntry);
            return;
        }
        for (;;) {
            ntry.cookie = bits.random(8, 0x7ffffff0);
            boolean fnd = false;
            for (int i = 0; i < src.size(); i++) {
                if (src.get(i).cookie != ntry.cookie) {
                    continue;
                }
                fnd = true;
                break;
            }
            if (!fnd) {
                break;
            }
        }
        trg.put(ntry);
    }

    private int getLabel(List<Integer> lab) {
        if (lab == null) {
            return -1;
        }
        if (lab.size() < 1) {
            return -1;
        }
        int i = lab.get(0);
        if (i == ipMpls.labelImp) {
            return -1;
        }
        return i;
    }

    private void createGroupMcast(packHolder pckB, packHolder pckT, packOpenflow pckO, tabGen<servOpenflowFlw> n, boolean ipv4, tabGen<servOpenflowIfc2> ifcs) {
        tabGen<ipFwdMcast> groups;
        if (ipv4) {
            groups = lower.expVrf.fwd4.groups;
        } else {
            groups = lower.expVrf.fwd6.groups;
        }
        for (int i = 0; i < groups.size(); i++) {
            ipFwdMcast grp = groups.get(i);
            if (grp == null) {
                continue;
            }
            if (grp.local) {
                continue;
            }
            if (grp.iface == null) {
                continue;
            }
            servOpenflowFlw ntry = new servOpenflowFlw();
            ntry.match = new byte[33];
            ntry.match[0] = 2;
            grp.group.toBuffer(ntry.match, 1);
            grp.source.toBuffer(ntry.match, 17);
            pckB.clear();
            for (int o = 0; o < grp.flood.size(); o++) {
                servOpenflowIfc2 ifc = new servOpenflowIfc2(grp.flood.get(o), null, null);
                if (ifc.ifc == null) {
                    continue;
                }
                ifc = ifcs.find(ifc);
                if (ifc == null) {
                    continue;
                }
                if (ifc.ifo.id == servOpenflow.tabGrp) {
                    continue;
                }
                List<encTlv> tlvs = new ArrayList<encTlv>();
                pckT.clear();
                pckO.createMatchMac(pckT, true, (addrMac) ifc.ifo.ifc.ethtyp.getHwAddr(), null);
                tlvs.add(pckO.getActionSetField(pckT));
                tlvs.add(pckO.getActionOutput(ifc.ifo.id));
                pckO.createBucketAct(pckB, tlvs);
                pckB.merge2end();
            }
            tabGen<addrIP> bck = lower.parent.mergeMcast(lower.parid, grp.flood, grp.iface);
            for (int o = 0; o < bck.size(); o++) {
                tabRouteEntry<addrIP> oru = lower.parid.routes.route(bck.get(o));
                if (oru == null) {
                    continue;
                }
                servStackIfc stk = lower.parid.ifaces.get(oru.best.iface.ifwNum);
                if (stk == null) {
                    continue;
                }
                if (!stk.ready) {
                    continue;
                }
                servOpenflowIfc1 oif = lower.findIfc(stk.ifc);
                if (oif == null) {
                    continue;
                }
                List<encTlv> tlvs = new ArrayList<encTlv>();
                pckT.clear();
                pckO.createMatchMac(pckT, true, (addrMac) oif.ifc.ethtyp.getHwAddr(), null);
                tlvs.add(pckO.getActionSetField(pckT));
                tlvs.add(pckO.getActionOutput(oif.id));
                pckO.createBucketAct(pckB, tlvs);
                pckB.merge2end();
            }
            ntry.action = pckB.getCopy();
            addTable(n, tabGroup, ntry);
        }
    }

    private tabGen<servOpenflowFlw> createGroup(packHolder pckB, packHolder pckT, packOpenflow pckO, tabGen<servOpenflowIfc2> ifcs4, tabGen<servOpenflowIfc2> ifcs6) {
        tabGen<servOpenflowFlw> n = new tabGen<servOpenflowFlw>();
        if (lower.expVrf == null) {
            return n;
        }
        for (int i = 0; i < lower.expIfc.size(); i++) {
            servOpenflowIfc1 ifc = lower.expIfc.get(i);
            if (ifc.id != servOpenflow.tabGrp) {
                continue;
            }
            servOpenflowFlw ntry = new servOpenflowFlw();
            ntry.match = new byte[5];
            ntry.match[0] = 1;
            bits.msbPutD(ntry.match, 1, ifc.grp);
            pckB.clear();
            for (int o = 0; o < lower.expIfc.size(); o++) {
                servOpenflowIfc1 ic = lower.expIfc.get(o);
                if (ic.ifc.bridgeIfc == null) {
                    continue;
                }
                if (ic.ifc.bridgeIfc.lowerBr != ifc.ifc.bridgeHed.bridgeHed) {
                    continue;
                }
                if (ic.ifc.bridgeIfc.blocked) {
                    continue;
                }
                List<encTlv> tlvs = new ArrayList<encTlv>();
                tlvs.add(pckO.getActionOutput(ic.id));
                pckO.createBucketAct(pckB, tlvs);
                pckB.merge2end();
            }
            ntry.action = pckB.getCopy();
            addTable(n, tabGroup, ntry);
            ifc.cook = ntry.cookie;
        }
        createGroupMcast(pckB, pckT, pckO, n, true, ifcs4);
        createGroupMcast(pckB, pckT, pckO, n, false, ifcs6);
        for (int i = tabLabel.labels.size() - 1; i >= 0; i--) {
            tabLabelEntry lab = tabLabel.labels.get(i);
            if (lab == null) {
                continue;
            }
            if (lab.forwarder == null) {
                continue;
            }
            if (lab.duplicate == null) {
                continue;
            }
            if (lab.needLocal) {
                continue;
            }
            tabGen<servOpenflowIfc2> ifcs = null;
            if (lab.forwarder == lower.expVrf.fwd4) {
                ifcs = ifcs4;
            }
            if (lab.forwarder == lower.expVrf.fwd6) {
                ifcs = ifcs6;
            }
            if (ifcs == null) {
                continue;
            }
            servOpenflowFlw ntry = new servOpenflowFlw();
            ntry.match = new byte[5];
            ntry.match[0] = 3;
            bits.msbPutD(ntry.match, 1, lab.label);
            pckB.clear();
            for (int o = 0; o < lab.duplicate.size(); o++) {
                tabLabelDup dup = lab.duplicate.get(o);
                if (dup == null) {
                    continue;
                }
                int lr = getLabel(dup.label);
                if (lr < 0) {
                    continue;
                }
                servOpenflowIfc2 ifc = new servOpenflowIfc2(dup.iface, null, null);
                if (ifc.ifc == null) {
                    continue;
                }
                ifc = ifcs.find(ifc);
                if (ifc == null) {
                    continue;
                }
                if (ifc.ifo.id == servOpenflow.tabGrp) {
                    continue;
                }
                addrMac mac = (addrMac) ifc.ipi.getL2info(dup.hop);
                if (mac == null) {
                    continue;
                }
                List<encTlv> tlvs = new ArrayList<encTlv>();
                pckT.clear();
                pckO.createMatchMac(pckT, true, (addrMac) ifc.ifo.ifc.ethtyp.getHwAddr(), null);
                tlvs.add(pckO.getActionSetField(pckT));
                pckT.clear();
                pckO.createMatchMac(pckT, false, mac, null);
                tlvs.add(pckO.getActionSetField(pckT));
                pckT.clear();
                pckO.createMatchMplsLab(pckT, lr);
                tlvs.add(pckO.getActionSetField(pckT));
                tlvs.add(pckO.getActionOutput(ifc.ifo.id));
                pckO.createBucketAct(pckB, tlvs);
                pckB.merge2end();
            }
            ntry.action = pckB.getCopy();
            addTable(n, tabGroup, ntry);
        }
        return n;
    }

    private void createPortPunt(packHolder pckB, packOpenflow pckO, servOpenflowIfc1 ifc, servOpenflowFlw ntry) {
        ntry.prio = 1;
        pckB.clear();
        pckO.createMatchPort(pckB, ifc.id);
        pckB.merge2beg();
        ntry.match = pckB.getCopy();
        List<encTlv> tlvs = new ArrayList<encTlv>();
        tlvs.add(pckO.getActionOutput(packOpenflow.cntrlPort));
        pckB.clear();
        pckO.createInstrAct(pckB, tlvs);
        pckB.merge2beg();
        ntry.action = pckB.getCopy();
    }

    private void createPortMcast(tabGen<servOpenflowFlw> n, packHolder pckB, packOpenflow pckO, boolean ipv4, tabGen<servOpenflowIfc2> ifcs) {
        tabGen<ipFwdMcast> groups;
        if (ipv4) {
            groups = lower.expVrf.fwd4.groups;
        } else {
            groups = lower.expVrf.fwd6.groups;
        }
        for (int i = 0; i < groups.size(); i++) {
            ipFwdMcast grp = groups.get(i);
            if (grp == null) {
                continue;
            }
            if (grp.iface == null) {
                continue;
            }
            servOpenflowIfc2 ifc = new servOpenflowIfc2(grp.iface, null, null);
            ifc = ifcs.find(ifc);
            pckB.clear();
            if (ifc != null) {
                if (ifc.ifo.id == servOpenflow.tabGrp) {
                    continue;
                }
                pckO.createMatchPort(pckB, ifc.ifo.id);
            } else {
                servStackFwd oth = lower.parent.findIfc(lower.parid, grp.iface);
                if (oth == null) {
                    continue;
                }
                tabRouteEntry<addrIP> oru = servStack.forwarder2route(oth.id);
                oru = lower.parid.routes.find(oru);
                if (oru == null) {
                    continue;
                }
                if (oru.best.iface == null) {
                    continue;
                }
                servStackIfc stk = lower.parid.ifaces.get(oru.best.iface.ifwNum);
                if (stk == null) {
                    continue;
                }
                if (!stk.ready) {
                    continue;
                }
                servOpenflowIfc1 oif = lower.findIfc(stk.ifc);
                if (oif == null) {
                    continue;
                }
                if (oif.id == servOpenflow.tabGrp) {
                    continue;
                }
                pckO.createMatchPort(pckB, oif.id);
            }
            servOpenflowFlw ntry = new servOpenflowFlw();
            ntry.match = new byte[33];
            ntry.match[0] = 2;
            grp.group.toBuffer(ntry.match, 1);
            grp.source.toBuffer(ntry.match, 17);
            ntry = tabGroup.find(ntry);
            if (ntry == null) {
                continue;
            }
            int cook = ntry.cookie;
            ntry = new servOpenflowFlw();
            ntry.prio = 3;
            if (ipv4) {
                pckO.createMatchEthTyp(pckB, ipIfc4.type);
                pckO.createMatchIpv4(pckB, true, grp.source, null);
                pckO.createMatchIpv4(pckB, false, grp.group, null);
            } else {
                pckO.createMatchEthTyp(pckB, ipIfc6.type);
                pckO.createMatchIpv6(pckB, true, grp.source, null);
                pckO.createMatchIpv6(pckB, false, grp.group, null);
            }
            pckB.merge2beg();
            ntry.match = pckB.getCopy();
            pckB.clear();
            List<encTlv> tlvs = new ArrayList<encTlv>();
            tlvs.add(pckO.getActionGroup(cook));
            pckO.createInstrAct(pckB, tlvs);
            pckB.merge2beg();
            ntry.action = pckB.getCopy();
            addTable(n, tabPort, ntry);
        }
    }

    private tabGen<servOpenflowFlw> createPort(packHolder pckB, packOpenflow pckO, tabGen<servOpenflowIfc2> ifcs4, tabGen<servOpenflowIfc2> ifcs6) {
        tabGen<servOpenflowFlw> n = new tabGen<servOpenflowFlw>();
        for (int i = 0; i < lower.expIfc.size(); i++) {
            servOpenflowIfc1 ifc = lower.expIfc.get(i);
            if (ifc.id == servOpenflow.tabGrp) {
                continue;
            }
            servOpenflowFlw ntry = new servOpenflowFlw();
            if (lower.expVrf == null) {
                createPortPunt(pckB, pckO, ifc, ntry);
                addTable(n, tabPort, ntry);
                continue;
            }
            if (ifc.ifc.bridgeIfc != null) {
                servOpenflowIfc1 ic = new servOpenflowIfc1();
                ic.id = servOpenflow.tabGrp;
                ic.grp = ifc.ifc.bridgeHed.number;
                ic = lower.expIfc.find(ic);
                if (ic == null) {
                    createPortPunt(pckB, pckO, ifc, ntry);
                    addTable(n, tabPort, ntry);
                    continue;
                }
                if (ifc.ifc.bridgeIfc.blocked) {
                    createPortPunt(pckB, pckO, ifc, ntry);
                    addTable(n, tabPort, ntry);
                    continue;
                }
                ntry.prio = 2;
                pckB.clear();
                pckO.createMatchPort(pckB, ifc.id);
                pckO.createMatchMac(pckB, false, addrMac.getMultiBase(), addrMac.getMultiBase());
                pckB.merge2beg();
                ntry.match = pckB.getCopy();
                pckB.clear();
                List<encTlv> tlvs = new ArrayList<encTlv>();
                tlvs.add(pckO.getActionOutput(packOpenflow.cntrlPort));
                pckO.createInstrAct(pckB, tlvs);
                pckB.merge2beg();
                ntry.action = pckB.getCopy();
                addTable(n, tabPort, ntry);
                ntry = new servOpenflowFlw();
                ntry.prio = 2;
                pckB.clear();
                pckO.createMatchPort(pckB, ifc.id);
                pckO.createMatchEthTyp(pckB, ipMpls.typeU);
                pckO.createMatchMac(pckB, false, (addrMac) ic.ifc.ethtyp.getHwAddr(), null);
                pckB.merge2beg();
                ntry.match = pckB.getCopy();
                pckB.clear();
                pckO.createInstrGoto(pckB, servOpenflow.tabMpls);
                pckB.merge2beg();
                ntry.action = pckB.getCopy();
                addTable(n, tabPort, ntry);
                ntry = new servOpenflowFlw();
                ntry.prio = 2;
                pckB.clear();
                pckO.createMatchPort(pckB, ifc.id);
                pckO.createMatchEthTyp(pckB, ipIfc4.type);
                pckO.createMatchMac(pckB, false, (addrMac) ic.ifc.ethtyp.getHwAddr(), null);
                pckB.merge2beg();
                ntry.match = pckB.getCopy();
                pckB.clear();
                pckO.createInstrGoto(pckB, servOpenflow.tabIpv4);
                pckB.merge2beg();
                ntry.action = pckB.getCopy();
                addTable(n, tabPort, ntry);
                ntry = new servOpenflowFlw();
                ntry.prio = 2;
                pckB.clear();
                pckO.createMatchPort(pckB, ifc.id);
                pckO.createMatchEthTyp(pckB, ipIfc4arp.type);
                pckO.createMatchMac(pckB, false, (addrMac) ic.ifc.ethtyp.getHwAddr(), null);
                pckB.merge2beg();
                ntry.match = pckB.getCopy();
                pckB.clear();
                tlvs = new ArrayList<encTlv>();
                tlvs.add(pckO.getActionOutput(packOpenflow.cntrlPort));
                pckO.createInstrAct(pckB, tlvs);
                pckB.merge2beg();
                ntry.action = pckB.getCopy();
                addTable(n, tabPort, ntry);
                ntry = new servOpenflowFlw();
                ntry.prio = 2;
                pckB.clear();
                pckO.createMatchPort(pckB, ifc.id);
                pckO.createMatchEthTyp(pckB, ipIfc6.type);
                pckO.createMatchMac(pckB, false, (addrMac) ic.ifc.ethtyp.getHwAddr(), null);
                pckB.merge2beg();
                ntry.match = pckB.getCopy();
                pckB.clear();
                pckO.createInstrGoto(pckB, servOpenflow.tabIpv6);
                pckB.merge2beg();
                ntry.action = pckB.getCopy();
                addTable(n, tabPort, ntry);
                ntry = new servOpenflowFlw();
                ntry.prio = 1;
                pckB.clear();
                pckO.createMatchPort(pckB, ifc.id);
                pckB.merge2beg();
                ntry.match = pckB.getCopy();
                pckB.clear();
                tlvs = new ArrayList<encTlv>();
                tlvs.add(pckO.getActionGroup(ic.cook));
                pckO.createInstrAct(pckB, tlvs);
                pckB.merge2beg();
                ntry.action = pckB.getCopy();
                addTable(n, tabPort, ntry);
                continue;
            }
            if (ifc.ifc.vrfFor == null) {
                createPortPunt(pckB, pckO, ifc, ntry);
                addTable(n, tabPort, ntry);
                continue;
            }
            if (lower.expVrf.compareTo(ifc.ifc.vrfFor) != 0) {
                createPortPunt(pckB, pckO, ifc, ntry);
                addTable(n, tabPort, ntry);
                continue;
            }
            ntry.prio = 2;
            pckB.clear();
            pckO.createMatchPort(pckB, ifc.id);
            pckO.createMatchMac(pckB, false, addrMac.getMultiBase(), addrMac.getMultiBase());
            pckB.merge2beg();
            ntry.match = pckB.getCopy();
            pckB.clear();
            List<encTlv> tlvs = new ArrayList<encTlv>();
            tlvs.add(pckO.getActionOutput(packOpenflow.cntrlPort));
            pckO.createInstrAct(pckB, tlvs);
            pckB.merge2beg();
            ntry.action = pckB.getCopy();
            addTable(n, tabPort, ntry);
            if (ifc.ifc.mplsPack != null) {
                ntry = new servOpenflowFlw();
                ntry.prio = 1;
                pckB.clear();
                pckO.createMatchPort(pckB, ifc.id);
                pckO.createMatchEthTyp(pckB, ipMpls.typeU);
                pckB.merge2beg();
                ntry.match = pckB.getCopy();
                pckB.clear();
                pckO.createInstrGoto(pckB, servOpenflow.tabMpls);
                pckB.merge2beg();
                ntry.action = pckB.getCopy();
                addTable(n, tabPort, ntry);
            }
            if ((ifc.ifc.mplsPack != null) || (ifc.ifc.addr4 != null)) {
                ntry = new servOpenflowFlw();
                ntry.prio = 1;
                pckB.clear();
                pckO.createMatchPort(pckB, ifc.id);
                pckO.createMatchEthTyp(pckB, ipIfc4.type);
                pckB.merge2beg();
                ntry.match = pckB.getCopy();
                pckB.clear();
                pckO.createInstrGoto(pckB, servOpenflow.tabIpv4);
                pckB.merge2beg();
                ntry.action = pckB.getCopy();
                addTable(n, tabPort, ntry);
                ntry = new servOpenflowFlw();
                ntry.prio = 3;
                pckB.clear();
                pckO.createMatchPort(pckB, ifc.id);
                pckO.createMatchEthTyp(pckB, ipIfc4arp.type);
                pckB.merge2beg();
                ntry.match = pckB.getCopy();
                pckB.clear();
                tlvs = new ArrayList<encTlv>();
                tlvs.add(pckO.getActionOutput(packOpenflow.cntrlPort));
                pckO.createInstrAct(pckB, tlvs);
                pckB.merge2beg();
                ntry.action = pckB.getCopy();
                addTable(n, tabPort, ntry);
            }
            if ((ifc.ifc.mplsPack != null) || (ifc.ifc.addr6 != null)) {
                ntry = new servOpenflowFlw();
                ntry.prio = 1;
                pckB.clear();
                pckO.createMatchPort(pckB, ifc.id);
                pckO.createMatchEthTyp(pckB, ipIfc6.type);
                pckB.merge2beg();
                ntry.match = pckB.getCopy();
                pckB.clear();
                pckO.createInstrGoto(pckB, servOpenflow.tabIpv6);
                pckB.merge2beg();
                ntry.action = pckB.getCopy();
                addTable(n, tabPort, ntry);
            }
        }
        if (lower.expVrf != null) {
            createPortMcast(n, pckB, pckO, true, ifcs4);
            createPortMcast(n, pckB, pckO, false, ifcs6);
        }
        addTable(n, tabPort, new servOpenflowFlw());
        return n;
    }

    private void createIpvXpunt(packHolder pckB, packOpenflow pckO, boolean ipv4, tabRouteEntry<addrIP> rou, servOpenflowFlw ntry) {
        ntry.prio = 100 + rou.prefix.maskLen;
        pckB.clear();
        if (ipv4) {
            pckO.createMatchEthTyp(pckB, ipIfc4.type);
            pckO.createMatchIpv4(pckB, false, rou.prefix.network, rou.prefix.mask);
        } else {
            pckO.createMatchEthTyp(pckB, ipIfc6.type);
            pckO.createMatchIpv6(pckB, false, rou.prefix.network, rou.prefix.mask);
        }
        pckB.merge2beg();
        ntry.match = pckB.getCopy();
        List<encTlv> tlvs = new ArrayList<encTlv>();
        tlvs.add(pckO.getActionOutput(packOpenflow.cntrlPort));
        pckB.clear();
        pckO.createInstrAct(pckB, tlvs);
        pckB.merge2beg();
        ntry.action = pckB.getCopy();
    }

    private tabGen<servOpenflowFlw> createIpvX(packHolder pckB, packOpenflow pckO, boolean ipv4, tabGen<servOpenflowIfc2> ifcs) {
        tabGen<servOpenflowFlw> n = new tabGen<servOpenflowFlw>();
        if (lower.expVrf == null) {
            if (ipv4) {
                addTable(n, tabIpv4, new servOpenflowFlw());
            } else {
                addTable(n, tabIpv6, new servOpenflowFlw());
            }
            return n;
        }
        tabRoute<addrIP> rous;
        if (ipv4) {
            rous = lower.expVrf.fwd4.actualU;
        } else {
            rous = lower.expVrf.fwd6.actualU;
        }
        for (int i = 0; i < rous.size(); i++) {
            tabRouteEntry<addrIP> rou = rous.get(i);
            if (rou == null) {
                continue;
            }
            servOpenflowFlw ntry = new servOpenflowFlw();
            if (rou.best.nextHop == null) {
                createIpvXpunt(pckB, pckO, ipv4, rou, ntry);
                if (ipv4) {
                    addTable(n, tabIpv4, ntry);
                } else {
                    addTable(n, tabIpv6, ntry);
                }
                continue;
            }
            if (rou.best.iface == null) {
                createIpvXpunt(pckB, pckO, ipv4, rou, ntry);
                if (ipv4) {
                    addTable(n, tabIpv4, ntry);
                } else {
                    addTable(n, tabIpv6, ntry);
                }
                continue;
            }
            servOpenflowIfc2 ifc = new servOpenflowIfc2((ipFwdIface) rou.best.iface, null, null);
            ifc = ifcs.find(ifc);
            if (ifc == null) {
                servStackFwd oth = lower.parent.findIfc(lower.parid, rou.best.iface);
                if (oth == null) {
                    createIpvXpunt(pckB, pckO, ipv4, rou, ntry);
                    if (ipv4) {
                        addTable(n, tabIpv4, ntry);
                    } else {
                        addTable(n, tabIpv6, ntry);
                    }
                    continue;
                }
                tabRouteEntry<addrIP> oru = servStack.forwarder2route(oth.id);
                oru = lower.parid.routes.find(oru);
                if (oru == null) {
                    createIpvXpunt(pckB, pckO, ipv4, rou, ntry);
                    if (ipv4) {
                        addTable(n, tabIpv4, ntry);
                    } else {
                        addTable(n, tabIpv6, ntry);
                    }
                    continue;
                }
                if (oru.best.iface == null) {
                    createIpvXpunt(pckB, pckO, ipv4, rou, ntry);
                    if (ipv4) {
                        addTable(n, tabIpv4, ntry);
                    } else {
                        addTable(n, tabIpv6, ntry);
                    }
                    continue;
                }
                servStackIfc bck = lower.parid.ifaces.get(oru.best.iface.ifwNum);
                if (bck == null) {
                    createIpvXpunt(pckB, pckO, ipv4, rou, ntry);
                    if (ipv4) {
                        addTable(n, tabIpv4, ntry);
                    } else {
                        addTable(n, tabIpv6, ntry);
                    }
                    continue;
                }
                if (!bck.ready) {
                    createIpvXpunt(pckB, pckO, ipv4, rou, ntry);
                    if (ipv4) {
                        addTable(n, tabIpv4, ntry);
                    } else {
                        addTable(n, tabIpv6, ntry);
                    }
                    continue;
                }
                servOpenflowIfc1 oif = lower.findIfc(bck.ifc);
                if (oif == null) {
                    createIpvXpunt(pckB, pckO, ipv4, rou, ntry);
                    if (ipv4) {
                        addTable(n, tabIpv4, ntry);
                    } else {
                        addTable(n, tabIpv6, ntry);
                    }
                    continue;
                }
                addrMac macR = bck.lastPort.getMac();
                addrMac macL = (addrMac) oif.ifc.ethtyp.getHwAddr();
                ntry.prio = 100 + rou.prefix.maskLen;
                pckB.clear();
                if (ipv4) {
                    pckO.createMatchEthTyp(pckB, ipIfc4.type);
                    pckO.createMatchIpv4(pckB, false, rou.prefix.network, rou.prefix.mask);
                } else {
                    pckO.createMatchEthTyp(pckB, ipIfc6.type);
                    pckO.createMatchIpv6(pckB, false, rou.prefix.network, rou.prefix.mask);
                }
                pckB.merge2beg();
                ntry.match = pckB.getCopy();
                List<encTlv> tlvs = new ArrayList<encTlv>();
                pckB.clear();
                pckO.createMatchMac(pckB, true, macL, null);
                tlvs.add(pckO.getActionSetField(pckB));
                pckB.clear();
                pckO.createMatchMac(pckB, false, macR, null);
                tlvs.add(pckO.getActionSetField(pckB));
                tlvs.add(pckO.getActionOutput(oif.id));
                pckB.clear();
                pckO.createInstrAct(pckB, tlvs);
                pckB.merge2beg();
                ntry.action = pckB.getCopy();
                if (ipv4) {
                    addTable(n, tabIpv4, ntry);
                } else {
                    addTable(n, tabIpv6, ntry);
                }
                continue;
            }
            addrMac mac = (addrMac) ifc.ipi.getL2info(rou.best.nextHop);
            if (mac == null) {
                createIpvXpunt(pckB, pckO, ipv4, rou, ntry);
                if (ipv4) {
                    addTable(n, tabIpv4, ntry);
                } else {
                    addTable(n, tabIpv6, ntry);
                }
                continue;
            }
            ntry.prio = 100 + rou.prefix.maskLen;
            pckB.clear();
            if (ipv4) {
                pckO.createMatchEthTyp(pckB, ipIfc4.type);
                pckO.createMatchIpv4(pckB, false, rou.prefix.network, rou.prefix.mask);
            } else {
                pckO.createMatchEthTyp(pckB, ipIfc6.type);
                pckO.createMatchIpv6(pckB, false, rou.prefix.network, rou.prefix.mask);
            }
            pckB.merge2beg();
            ntry.match = pckB.getCopy();
            List<encTlv> tlvs = new ArrayList<encTlv>();
            pckB.clear();
            pckO.createMatchMac(pckB, true, (addrMac) ifc.ifo.ifc.ethtyp.getHwAddr(), null);
            tlvs.add(pckO.getActionSetField(pckB));
            pckB.clear();
            pckO.createMatchMac(pckB, false, mac, null);
            tlvs.add(pckO.getActionSetField(pckB));
            int o = getLabel(rou.best.labelRem);
            if (o >= 0) {
                tlvs.add(pckO.getActionPush(packOpenflow.actionMplsPush, ipMpls.typeU));
                pckB.clear();
                pckO.createMatchMplsLab(pckB, o);
                tlvs.add(pckO.getActionSetField(pckB));
                tlvs.add(pckO.getActionTtl(packOpenflow.actionMplsTtlSet, 255));
            } else {
                tlvs.add(pckO.getActionTtl(packOpenflow.actionIpTtlDec, 0));
            }
            if (ifc.ifo.id == servOpenflow.tabGrp) {
                tlvs.add(pckO.getActionGroup(ifc.ifo.cook));
            } else {
                tlvs.add(pckO.getActionOutput(ifc.ifo.id));
            }
            pckB.clear();
            pckO.createInstrAct(pckB, tlvs);
            pckB.merge2beg();
            ntry.action = pckB.getCopy();
            if (ipv4) {
                addTable(n, tabIpv4, ntry);
            } else {
                addTable(n, tabIpv6, ntry);
            }
        }
        for (int i = 0; i < ifcs.size(); i++) {
            servOpenflowIfc2 ifc = ifcs.get(i);
            if (!ifc.ifc.ready) {
                continue;
            }
            if (!ipv4) {
                addrIP adr = ifc.ipi.getLinkLocalAddr();
                tabRouteEntry<addrIP> rou = new tabRouteEntry<addrIP>();
                rou.prefix = new addrPrefix<addrIP>(adr, addrIP.size * 8);
                servOpenflowFlw ntry = new servOpenflowFlw();
                createIpvXpunt(pckB, pckO, false, rou, ntry);
                addTable(n, tabIpv6, ntry);
            }
            for (int o = 0;; o++) {
                addrIP per = new addrIP();
                addrMac mac = new addrMac();
                if (ifc.ipi.getL2info(o, per, mac)) {
                    break;
                }
                if (!ifc.ipi.checkConnected(per)) {
                    continue;
                }
                servOpenflowFlw ntry = new servOpenflowFlw();
                ntry.prio = 300;
                pckB.clear();
                if (ipv4) {
                    pckO.createMatchEthTyp(pckB, ipIfc4.type);
                    pckO.createMatchIpv4(pckB, false, per, null);
                } else {
                    pckO.createMatchEthTyp(pckB, ipIfc6.type);
                    pckO.createMatchIpv6(pckB, false, per, null);
                }
                pckB.merge2beg();
                ntry.match = pckB.getCopy();
                List<encTlv> tlvs = new ArrayList<encTlv>();
                pckB.clear();
                pckO.createMatchMac(pckB, true, (addrMac) ifc.ifo.ifc.ethtyp.getHwAddr(), null);
                tlvs.add(pckO.getActionSetField(pckB));
                pckB.clear();
                pckO.createMatchMac(pckB, false, mac, null);
                tlvs.add(pckO.getActionSetField(pckB));
                if (ifc.ifo.id == servOpenflow.tabGrp) {
                    tlvs.add(pckO.getActionGroup(ifc.ifo.cook));
                } else {
                    tlvs.add(pckO.getActionOutput(ifc.ifo.id));
                }
                pckB.clear();
                pckO.createInstrAct(pckB, tlvs);
                pckB.merge2beg();
                ntry.action = pckB.getCopy();
                if (ipv4) {
                    addTable(n, tabIpv4, ntry);
                } else {
                    addTable(n, tabIpv6, ntry);
                }
            }
        }
        if (ipv4) {
            addTable(n, tabIpv4, new servOpenflowFlw());
        } else {
            addTable(n, tabIpv6, new servOpenflowFlw());
        }
        return n;
    }

    private servOpenflowFlw createMplsPop(packHolder pckB, packOpenflow pckO, int lab, boolean bottom, int tab, int typ) {
        servOpenflowFlw ntry = new servOpenflowFlw();
        ntry.prio = 1;
        if (bottom) {
            ntry.prio++;
        }
        pckB.clear();
        pckO.createMatchEthTyp(pckB, ipMpls.typeU);
        pckO.createMatchMplsLab(pckB, lab);
        if (bottom) {
            pckO.createMatchMplsBos(pckB, bottom);
        }
        pckB.merge2beg();
        ntry.match = pckB.getCopy();
        List<encTlv> tlvs = new ArrayList<encTlv>();
        tlvs.add(pckO.getActionPush(packOpenflow.actionMplsPop, typ));
        pckB.clear();
        pckO.createInstrAct(pckB, tlvs);
        pckO.createInstrGoto(pckB, tab);
        pckB.merge2beg();
        ntry.action = pckB.getCopy();
        return ntry;
    }

    private void createMplsPunt(packHolder pckB, packOpenflow pckO, tabLabelEntry lab, servOpenflowFlw ntry) {
        pckB.clear();
        pckO.createMatchEthTyp(pckB, ipMpls.typeU);
        pckO.createMatchMplsLab(pckB, lab.label);
        pckB.merge2beg();
        ntry.match = pckB.getCopy();
        List<encTlv> tlvs = new ArrayList<encTlv>();
        tlvs.add(pckO.getActionOutput(packOpenflow.cntrlPort));
        pckB.clear();
        pckO.createInstrAct(pckB, tlvs);
        pckB.merge2beg();
        ntry.action = pckB.getCopy();
    }

    private tabGen<servOpenflowFlw> createMpls(packHolder pckB, packOpenflow pckO, tabGen<servOpenflowIfc2> ifcs4, tabGen<servOpenflowIfc2> ifcs6) {
        tabGen<servOpenflowFlw> n = new tabGen<servOpenflowFlw>();
        if (lower.expVrf == null) {
            addTable(n, tabMpls, new servOpenflowFlw());
            return n;
        }
        addTable(n, tabMpls, createMplsPop(pckB, pckO, ipMpls.labelExp4, true, servOpenflow.tabIpv4, ipIfc4.type));
        addTable(n, tabMpls, createMplsPop(pckB, pckO, ipMpls.labelExp6, true, servOpenflow.tabIpv6, ipIfc6.type));
        for (int i = tabLabel.labels.size() - 1; i >= 0; i--) {
            tabLabelEntry lab = tabLabel.labels.get(i);
            if (lab == null) {
                continue;
            }
            if (lab.forwarder == null) {
                continue;
            }
            servOpenflowFlw ntry = new servOpenflowFlw();
            ntry.prio = 1;
            int proto = 0;
            int typ = 0;
            int tab = 0;
            if (lab.forwarder == lower.expVrf.fwd4) {
                proto = 1;
                typ = ipIfc4.type;
                tab = servOpenflow.tabIpv4;
            }
            if (lab.forwarder == lower.expVrf.fwd6) {
                proto = 2;
                typ = ipIfc6.type;
                tab = servOpenflow.tabIpv6;
            }
            if (proto < 1) {
                createMplsPunt(pckB, pckO, lab, ntry);
                addTable(n, tabMpls, ntry);
                continue;
            }
            if (lab.bier != null) {
                createMplsPunt(pckB, pckO, lab, ntry);
                addTable(n, tabMpls, ntry);
                continue;
            }
            if (lab.duplicate != null) {
                ntry.match = new byte[5];
                ntry.match[0] = 3;
                bits.msbPutD(ntry.match, 1, lab.label);
                ntry = tabGroup.find(ntry);
                if (ntry == null) {
                    continue;
                }
                int cook = ntry.cookie;
                ntry = new servOpenflowFlw();
                ntry.prio = 1;
                pckB.clear();
                pckO.createMatchEthTyp(pckB, ipMpls.typeU);
                pckO.createMatchMplsLab(pckB, lab.label);
                pckB.merge2beg();
                ntry.match = pckB.getCopy();
                List<encTlv> tlvs = new ArrayList<encTlv>();
                tlvs.add(pckO.getActionGroup(cook));
                pckB.clear();
                pckO.createInstrAct(pckB, tlvs);
                pckB.merge2beg();
                ntry.action = pckB.getCopy();
                addTable(n, tabMpls, ntry);
                continue;
            }
            if (lab.iface == null) {
                if (lab.needLocal) {
                    addTable(n, tabMpls, createMplsPop(pckB, pckO, lab.label, true, tab, typ));
                    continue;
                }
                createMplsPunt(pckB, pckO, lab, ntry);
                addTable(n, tabMpls, ntry);
                continue;
            }
            if (lab.nextHop == null) {
                createMplsPunt(pckB, pckO, lab, ntry);
                addTable(n, tabMpls, ntry);
                continue;
            }
            servOpenflowIfc2 ifc = new servOpenflowIfc2(lab.iface, null, null);
            if (proto == 1) {
                ifc = ifcs4.find(ifc);
            } else {
                ifc = ifcs6.find(ifc);
            }
            if (ifc == null) {
                servStackFwd oth = lower.parent.findIfc(lower.parid, lab.iface);
                if (oth == null) {
                    createMplsPunt(pckB, pckO, lab, ntry);
                    addTable(n, tabMpls, ntry);
                    continue;
                }
                tabRouteEntry<addrIP> oru = servStack.forwarder2route(oth.id);
                oru = lower.parid.routes.find(oru);
                if (oru == null) {
                    createMplsPunt(pckB, pckO, lab, ntry);
                    addTable(n, tabMpls, ntry);
                    continue;
                }
                if (oru.best.iface == null) {
                    createMplsPunt(pckB, pckO, lab, ntry);
                    addTable(n, tabMpls, ntry);
                    continue;
                }
                servStackIfc bck = lower.parid.ifaces.get(oru.best.iface.ifwNum);
                if (bck == null) {
                    createMplsPunt(pckB, pckO, lab, ntry);
                    addTable(n, tabMpls, ntry);
                    continue;
                }
                if (!bck.ready) {
                    createMplsPunt(pckB, pckO, lab, ntry);
                    addTable(n, tabMpls, ntry);
                    continue;
                }
                servOpenflowIfc1 oif = lower.findIfc(bck.ifc);
                if (oif == null) {
                    createMplsPunt(pckB, pckO, lab, ntry);
                    addTable(n, tabMpls, ntry);
                    continue;
                }
                addrMac macR = bck.lastPort.getMac();
                addrMac macL = (addrMac) oif.ifc.ethtyp.getHwAddr();
                ntry.prio = 2;
                pckB.clear();
                pckO.createMatchEthTyp(pckB, ipMpls.typeU);
                pckO.createMatchMplsLab(pckB, lab.label);
                pckO.createMatchMplsBos(pckB, true);
                pckB.merge2beg();
                ntry.match = pckB.getCopy();
                List<encTlv> tlvs = new ArrayList<encTlv>();
                pckB.clear();
                pckO.createMatchMac(pckB, true, macL, null);
                tlvs.add(pckO.getActionSetField(pckB));
                pckB.clear();
                pckO.createMatchMac(pckB, false, macR, null);
                tlvs.add(pckO.getActionSetField(pckB));
                tlvs.add(pckO.getActionOutput(oif.id));
                pckB.clear();
                pckO.createInstrAct(pckB, tlvs);
                pckB.merge2beg();
                ntry.action = pckB.getCopy();
                addTable(n, tabMpls, ntry);
                continue;
            }
            addrMac macR = (addrMac) ifc.ipi.getL2info(lab.nextHop);
            if (macR == null) {
                createMplsPunt(pckB, pckO, lab, ntry);
                addTable(n, tabMpls, ntry);
                continue;
            }
            int lr = getLabel(lab.remoteLab);
            addrMac macL = (addrMac) ifc.ifo.ifc.ethtyp.getHwAddr();
            ntry.prio = 2;
            pckB.clear();
            pckO.createMatchEthTyp(pckB, ipMpls.typeU);
            pckO.createMatchMplsLab(pckB, lab.label);
            pckO.createMatchMplsBos(pckB, true);
            pckB.merge2beg();
            ntry.match = pckB.getCopy();
            List<encTlv> tlvs = new ArrayList<encTlv>();
            pckB.clear();
            pckO.createMatchMac(pckB, true, macL, null);
            tlvs.add(pckO.getActionSetField(pckB));
            pckB.clear();
            pckO.createMatchMac(pckB, false, macR, null);
            tlvs.add(pckO.getActionSetField(pckB));
            if (lr >= 0) {
                pckB.clear();
                pckO.createMatchMplsLab(pckB, lr);
                tlvs.add(pckO.getActionSetField(pckB));
                tlvs.add(pckO.getActionTtl(packOpenflow.actionMplsTtlDec, 0));
            } else {
                tlvs.add(pckO.getActionPush(packOpenflow.actionMplsPop, typ));
            }
            if (ifc.ifo.id == servOpenflow.tabGrp) {
                tlvs.add(pckO.getActionGroup(ifc.ifo.cook));
            } else {
                tlvs.add(pckO.getActionOutput(ifc.ifo.id));
            }
            pckB.clear();
            pckO.createInstrAct(pckB, tlvs);
            pckB.merge2beg();
            ntry.action = pckB.getCopy();
            addTable(n, tabMpls, ntry);
            ntry = new servOpenflowFlw();
            ntry.prio = 1;
            pckB.clear();
            pckO.createMatchEthTyp(pckB, ipMpls.typeU);
            pckO.createMatchMplsLab(pckB, lab.label);
            pckB.merge2beg();
            ntry.match = pckB.getCopy();
            tlvs = new ArrayList<encTlv>();
            pckB.clear();
            pckO.createMatchMac(pckB, true, macL, null);
            tlvs.add(pckO.getActionSetField(pckB));
            pckB.clear();
            pckO.createMatchMac(pckB, false, macR, null);
            tlvs.add(pckO.getActionSetField(pckB));
            if (lr >= 0) {
                pckB.clear();
                pckO.createMatchMplsLab(pckB, lr);
                tlvs.add(pckO.getActionSetField(pckB));
                tlvs.add(pckO.getActionTtl(packOpenflow.actionMplsTtlDec, 0));
            } else {
                tlvs.add(pckO.getActionPush(packOpenflow.actionMplsPop, ipMpls.typeU));
            }
            if (ifc.ifo.id == servOpenflow.tabGrp) {
                tlvs.add(pckO.getActionGroup(ifc.ifo.cook));
            } else {
                tlvs.add(pckO.getActionOutput(ifc.ifo.id));
            }
            pckB.clear();
            pckO.createInstrAct(pckB, tlvs);
            pckB.merge2beg();
            ntry.action = pckB.getCopy();
            addTable(n, tabMpls, ntry);
        }
        for (int i = 0; i < lower.parent.bckplnLab.length; i++) {
            tabLabelEntry lab = lower.parent.bckplnLab[i];
            if (lab == null) {
                continue;
            }
            servOpenflowFlw ntry = new servOpenflowFlw();
            ntry.prio = 3;
            if (i == lower.parid.id) {
                createMplsPunt(pckB, pckO, lab, ntry);
                addTable(n, tabMpls, ntry);
                continue;
            }
            tabRouteEntry<addrIP> oru = servStack.forwarder2route(i);
            oru = lower.parid.routes.find(oru);
            if (oru == null) {
                createMplsPunt(pckB, pckO, lab, ntry);
                addTable(n, tabMpls, ntry);
                continue;
            }
            if (oru.best.iface == null) {
                createMplsPunt(pckB, pckO, lab, ntry);
                addTable(n, tabMpls, ntry);
                continue;
            }
            servStackIfc bck = lower.parid.ifaces.get(oru.best.iface.ifwNum);
            if (bck == null) {
                createMplsPunt(pckB, pckO, lab, ntry);
                addTable(n, tabMpls, ntry);
                continue;
            }
            if (!bck.ready) {
                createMplsPunt(pckB, pckO, lab, ntry);
                addTable(n, tabMpls, ntry);
                continue;
            }
            servOpenflowIfc1 oif = lower.findIfc(bck.ifc);
            if (oif == null) {
                createMplsPunt(pckB, pckO, lab, ntry);
                addTable(n, tabMpls, ntry);
                continue;
            }
            addrMac macR = bck.lastPort.getMac();
            addrMac macL = (addrMac) oif.ifc.ethtyp.getHwAddr();
            pckB.clear();
            pckO.createMatchEthTyp(pckB, ipMpls.typeU);
            pckO.createMatchMplsLab(pckB, lab.label);
            pckO.createMatchMplsBos(pckB, true);
            pckB.merge2beg();
            ntry.match = pckB.getCopy();
            List<encTlv> tlvs = new ArrayList<encTlv>();
            pckB.clear();
            pckO.createMatchMac(pckB, true, macL, null);
            tlvs.add(pckO.getActionSetField(pckB));
            pckB.clear();
            pckO.createMatchMac(pckB, false, macR, null);
            tlvs.add(pckO.getActionSetField(pckB));
            tlvs.add(pckO.getActionOutput(oif.id));
            pckB.clear();
            pckO.createInstrAct(pckB, tlvs);
            pckB.merge2beg();
            ntry.action = pckB.getCopy();
            addTable(n, tabMpls, ntry);
        }
        addTable(n, tabMpls, new servOpenflowFlw());
        return n;
    }

    private void doWord() {
        packHolder pckB = new packHolder(true, true);
        packHolder pckT = new packHolder(true, true);
        packOpenflow pckO = new packOpenflow();
        pckO.pipe = pipe;
        pckO.createHello();
        lower.sendPack(pckB, pckO);
        pckB.clear();
        pckO.createFeatures(pckB);
        lower.sendPack(pckB, pckO);
        pckB.clear();
        pckO.createMultipart(pckB, 13, 0);
        lower.sendPack(pckB, pckO);
        pckB.clear();
        pckO.createGroupMod(pckB, packOpenflow.groupCmdDel, packOpenflow.groupTypAll, 0xfffffffc);
        lower.sendPack(pckB, pckO);
        pckB.clear();
        pckO.putMatchBuf(pckB, new byte[0]);
        pckO.createFlowMod(pckB, 0, 0xff, packOpenflow.flowCmdDel, 0);
        lower.sendPack(pckB, pckO);
        for (int i = 0; i < lower.expIfc.size(); i++) {
            lower.expIfc.get(i).sendState(0);
        }
        for (;;) {
            if (!working) {
                return;
            }
            if (pipe.isClosed() != 0) {
                return;
            }
            tabGen<servOpenflowIfc2> ifc4 = new tabGen<servOpenflowIfc2>();
            tabGen<servOpenflowIfc2> ifc6 = new tabGen<servOpenflowIfc2>();
            if (lower.expVrf != null) {
                for (int i = 0; i < lower.expIfc.size(); i++) {
                    servOpenflowIfc1 ifc = lower.expIfc.get(i);
                    if (ifc.ifc.vrfFor == null) {
                        continue;
                    }
                    if (lower.expVrf.compareTo(ifc.ifc.vrfFor) != 0) {
                        continue;
                    }
                    if (ifc.ifc.addr4 != null) {
                        ifc4.add(new servOpenflowIfc2(ifc.ifc.fwdIf4, ifc.ifc.ipIf4, ifc));
                    }
                    if (ifc.ifc.addr6 != null) {
                        ifc6.add(new servOpenflowIfc2(ifc.ifc.fwdIf6, ifc.ifc.ipIf6, ifc));
                    }
                }
            }
            sendGroup(pckB, pckO, tabGroup, createGroup(pckB, pckT, pckO, ifc4, ifc6));
            sendTable(pckB, pckO, servOpenflow.tabPort, tabPort, createPort(pckB, pckO, ifc4, ifc6));
            sendTable(pckB, pckO, servOpenflow.tabMpls, tabMpls, createMpls(pckB, pckO, ifc4, ifc6));
            sendTable(pckB, pckO, servOpenflow.tabIpv4, tabIpv4, createIpvX(pckB, pckO, true, ifc4));
            sendTable(pckB, pckO, servOpenflow.tabIpv6, tabIpv6, createIpvX(pckB, pckO, false, ifc6));
            for (int i = 0; i < lower.expIfc.size(); i++) {
                servOpenflowIfc1 ifc = lower.expIfc.get(i);
                pckB.clear();
                pckB.msbPutD(0, ifc.id);
                pckB.msbPutD(4, 0);
                pckB.putSkip(8);
                pckB.merge2beg();
                pckO.createMultipart(pckB, 4, 0);
                lower.sendPack(pckB, pckO);
            }
            lower.notif.misleep(5000);
        }
    }

}
