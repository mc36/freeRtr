package rtr;

import addr.addrIP;
import addr.addrIPv4;
import addr.addrPrefix;
import ip.ipFwd;
import ip.ipFwdIface;
import ip.ipFwdMpmp;
import java.util.Comparator;
import java.util.List;
import pack.packLdp;
import pack.packLdpMp;
import pack.packLdpPwe;
import pipe.pipeLine;
import pipe.pipeSide;
import prt.prtAccept;
import prt.prtTcp;
import prt.prtUdp;
import tab.tabGen;
import tab.tabLabel;
import tab.tabListing;
import tab.tabPrfxlstN;
import tab.tabRoute;
import tab.tabRouteEntry;
import util.bits;
import util.debugger;
import util.logger;
import util.counter;

/**
 * label distribution protocol (rfc5036) neighbor
 *
 * @author matecsaba
 */
public class rtrLdpNeigh implements Runnable, Comparator<rtrLdpNeigh> {

    /**
     * peer address
     */
    public final addrIP peer;

    /**
     * heard interface hello
     */
    public boolean helloIfc;

    /**
     * heard targeted hello
     */
    public boolean helloTrg;

    /**
     * input label filter
     */
    public tabListing<tabPrfxlstN, addrIP> filterIn;

    /**
     * output label filter
     */
    public tabListing<tabPrfxlstN, addrIP> filterOut;

    /**
     * advertised prefixes
     */
    public tabRoute<addrIP> prefAdvert = new tabRoute<addrIP>("tx");

    /**
     * learned prefixes
     */
    public tabRoute<addrIP> prefLearn = new tabRoute<addrIP>("rx");

    /**
     * need to advertise pseudowire
     */
    public tabGen<packLdpPwe> pweNeed2adv = new tabGen<packLdpPwe>();

    /**
     * advertised pseudowire
     */
    public tabGen<packLdpPwe> pweAdvert = new tabGen<packLdpPwe>();

    /**
     * learned pseudowire
     */
    public tabGen<packLdpPwe> pweLearn = new tabGen<packLdpPwe>();

    /**
     * advertised (multi)point to multipoint
     */
    public tabGen<packLdpMp> pmpAdvert = new tabGen<packLdpMp>();

    /**
     * learned (multi)point to multipoint
     */
    public tabGen<packLdpMp> pmpLearn = new tabGen<packLdpMp>();

    /**
     * transport address
     */
    public addrIP trans;

    /**
     * lsr id address
     */
    public addrIPv4 lsrID;

    /**
     * session hello interval
     */
    public int sessHelloIntrvl;

    /**
     * session hello hold time
     */
    public int sessHelloHldtm;

    /**
     * source interface
     */
    public ipFwdIface ifc;

    /**
     * ip layer
     */
    public ipFwd ip;

    /**
     * udp layer
     */
    public prtUdp udp;

    /**
     * tcp layer
     */
    public prtTcp tcp;

    /**
     * connection
     */
    public pipeSide conn;

    /**
     * counter
     */
    public counter cntr = new counter();

    private long upTime;

    private boolean need2run;

    /**
     * create ldp neighbor
     *
     * @param adr address of peer
     */
    public rtrLdpNeigh(addrIP adr) {
        peer = adr.copyBytes();
    }

    public int compare(rtrLdpNeigh o1, rtrLdpNeigh o2) {
        return o1.peer.compare(o1.peer, o2.peer);
    }

    public String toString() {
        return "ldp with " + peer;
    }

    /**
     * get show neighbor string
     *
     * @return show line
     */
    public String getShNeigh() {
        return prefLearn.size() + "|" + prefAdvert.size() + "|" + pweLearn.size() + "|" + pweAdvert.size() + "|" + pmpLearn.size() + "|" + pmpAdvert.size() + "|" + peer + "|" + bits.timePast(upTime);
    }

    /**
     * get status of peer
     *
     * @param l list to append
     */
    public void getStatus(List<String> l) {
        l.add("peer = " + peer);
        l.add("transport = " + trans);
        l.add("lsrid = " + lsrID);
        l.add("local = " + ifc.addr);
        l.add("uptime = " + bits.timePast(upTime));
        l.add("hold time = " + bits.timeDump(sessHelloHldtm / 1000));
        l.add("keepalive time = " + bits.timeDump(sessHelloIntrvl / 1000));
        l.add("prefix learned = " + prefLearn.size());
        l.add("pwe learned = " + pweLearn.size());
        l.add("p2mp learned = " + pmpLearn.size());
        l.add("prefix advertised = " + prefAdvert.size() + " of " + ip.labeldR.size());
        l.add("pwe advertised = " + pweAdvert.size() + " of " + pweNeed2adv.size());
        l.add("p2mp advertised = " + pmpAdvert.size());
        l.add("connection = " + cntr.getShStat());
    }

    /**
     * start this peer
     */
    public void startPeer() {
        if (debugger.rtrLdpEvnt) {
            logger.debug("starting peer " + peer + " (" + trans + ")");
        }
        upTime = bits.getTime();
        need2run = true;
        new Thread(this).start();
    }

    /**
     * stop this peer
     */
    public void stopPeer() {
        if (debugger.rtrLdpEvnt) {
            logger.debug("stopping peer " + peer + " (" + trans + ")");
        }
        need2run = false;
        if (conn != null) {
            conn.setClose();
        }
    }

    /**
     * send keep alive
     */
    public void sendKeepAlive() {
        packLdp pck = new packLdp();
        pck.conn = conn;
        pck.cntr = cntr;
        pck.lsrID = ifc.addr.toIPv4();
        pck.msgTyp = packLdp.msgTkepAlv;
        pck.msgID = bits.randomD();
        pck.createLDPheader();
        pck.sendPack();
    }

    /**
     * send initialization
     */
    public void sendInitialization() {
        packLdp pck = new packLdp();
        pck.conn = conn;
        pck.cntr = cntr;
        pck.lsrID = lsrID;
        pck.msgTyp = packLdp.msgTinit;
        pck.holdTime = sessHelloHldtm / 1000;
        pck.putSessParam();
        pck.putMP2MPparam();
        pck.lsrID = ifc.addr.toIPv4();
        pck.createLDPheader();
        pck.sendPack();
    }

    /**
     * send address list
     */
    public void sendAddressList() {
        packLdp pck = new packLdp();
        pck.conn = conn;
        pck.cntr = cntr;
        pck.lsrID = ifc.addr.toIPv4();
        pck.transAddr = ifc.addr.copyBytes();
        pck.msgTyp = packLdp.msgTadrAdv;
        pck.msgID = bits.randomD();
        pck.putAddrMapping();
        pck.createLDPheader();
        pck.sendPack();
    }

    /**
     * send label mapping
     *
     * @param prf prefix to advertise
     */
    public void sendLabelMap(tabRouteEntry<addrIP> prf) {
        if (debugger.rtrLdpTraf) {
            logger.debug("tx reachable prefix=" + prf.prefix + " label=" + prf.labelLoc.getValue());
        }
        packLdp pck = new packLdp();
        pck.conn = conn;
        pck.cntr = cntr;
        pck.lsrID = ifc.addr.toIPv4();
        pck.msgTyp = packLdp.msgTlabMap;
        pck.msgID = bits.randomD();
        pck.label = prf.labelLoc.getValue();
        pck.putFECaddr(prf.prefix);
        pck.putGenLabel();
        pck.createLDPheader();
        pck.sendPack();
    }

    /**
     * send label mapping
     *
     * @param pwe pseudowire to advertise
     */
    public void sendLabelMap(packLdpPwe pwe) {
        if (debugger.rtrLdpTraf) {
            logger.debug("tx reachable pseudowire=" + pwe + " label=" + pwe.label);
        }
        packLdp pck = new packLdp();
        pck.conn = conn;
        pck.cntr = cntr;
        pck.lsrID = ifc.addr.toIPv4();
        pck.msgTyp = packLdp.msgTlabMap;
        pck.msgID = bits.randomD();
        pck.label = pwe.label;
        pck.stat = 0;
        pck.putFECpwe(pwe, false);
        pck.putGenLabel();
        pck.putPwStatus();
        pck.createLDPheader();
        pck.sendPack();
    }

    /**
     * send label mapping
     *
     * @param pmp (multi)point to multipoint to advertise
     */
    public void sendLabelMap(packLdpMp pmp) {
        if (debugger.rtrLdpTraf) {
            logger.debug("tx reachable multipoint=" + pmp + " label=" + pmp.label);
        }
        packLdp pck = new packLdp();
        pck.conn = conn;
        pck.cntr = cntr;
        pck.lsrID = ifc.addr.toIPv4();
        pck.msgTyp = packLdp.msgTlabMap;
        pck.msgID = bits.randomD();
        pck.label = pmp.label;
        pck.stat = 0;
        pck.putFECpmp(pmp);
        pck.putGenLabel();
        pck.putPwStatus();
        pck.createLDPheader();
        pck.sendPack();
    }

    /**
     * send label withdraw
     *
     * @param prf prefix to withdraw
     */
    public void sendLabelWdrw(addrPrefix<addrIP> prf) {
        if (debugger.rtrLdpTraf) {
            logger.debug("tx withdraw prefix=" + prf);
        }
        packLdp pck = new packLdp();
        pck.conn = conn;
        pck.cntr = cntr;
        pck.lsrID = ifc.addr.toIPv4();
        pck.msgTyp = packLdp.msgTlabWdr;
        pck.msgID = bits.randomD();
        pck.putFECaddr(prf);
        pck.createLDPheader();
        pck.sendPack();
    }

    /**
     * send label withdraw
     *
     * @param pwe pseudowire to withdraw
     */
    public void sendLabelWdrw(packLdpPwe pwe) {
        if (debugger.rtrLdpTraf) {
            logger.debug("tx withdraw pseudowire=" + pwe);
        }
        packLdp pck = new packLdp();
        pck.conn = conn;
        pck.cntr = cntr;
        pck.lsrID = ifc.addr.toIPv4();
        pck.msgTyp = packLdp.msgTlabWdr;
        pck.msgID = bits.randomD();
        pck.putFECpwe(pwe, true);
        pck.createLDPheader();
        pck.sendPack();
    }

    /**
     * send label withdraw
     *
     * @param pmp (multi)point to multipoint to withdraw
     */
    public void sendLabelWdrw(packLdpMp pmp) {
        if (debugger.rtrLdpTraf) {
            logger.debug("tx withdraw multipoint=" + pmp);
        }
        packLdp pck = new packLdp();
        pck.conn = conn;
        pck.cntr = cntr;
        pck.lsrID = ifc.addr.toIPv4();
        pck.msgTyp = packLdp.msgTlabWdr;
        pck.msgID = bits.randomD();
        pck.putFECpmp(pmp);
        pck.createLDPheader();
        pck.sendPack();
    }

    /**
     * send label release
     *
     * @param prf prefix to release
     */
    public void sendLabelRlse(addrPrefix<addrIP> prf) {
        if (debugger.rtrLdpTraf) {
            logger.debug("tx release prefix=" + prf);
        }
        packLdp pck = new packLdp();
        pck.conn = conn;
        pck.cntr = cntr;
        pck.lsrID = ifc.addr.toIPv4();
        pck.msgTyp = packLdp.msgTlabRel;
        pck.msgID = bits.randomD();
        pck.putFECaddr(prf);
        pck.createLDPheader();
        pck.sendPack();
    }

    /**
     * send label release
     *
     * @param pwe pseudowire to release
     */
    public void sendLabelRlse(packLdpPwe pwe) {
        if (debugger.rtrLdpTraf) {
            logger.debug("tx release pseudowire=" + pwe);
        }
        packLdp pck = new packLdp();
        pck.conn = conn;
        pck.cntr = cntr;
        pck.lsrID = ifc.addr.toIPv4();
        pck.msgTyp = packLdp.msgTlabRel;
        pck.msgID = bits.randomD();
        pck.putFECpwe(pwe, true);
        pck.createLDPheader();
        pck.sendPack();
    }

    /**
     * send label release
     *
     * @param pmp (multi)point to multipoint to release
     */
    public void sendLabelRlse(packLdpMp pmp) {
        if (debugger.rtrLdpTraf) {
            logger.debug("tx release multipoint=" + pmp);
        }
        packLdp pck = new packLdp();
        pck.conn = conn;
        pck.cntr = cntr;
        pck.lsrID = ifc.addr.toIPv4();
        pck.msgTyp = packLdp.msgTlabRel;
        pck.msgID = bits.randomD();
        pck.putFECpmp(pmp);
        pck.createLDPheader();
        pck.sendPack();
    }

    /**
     * got initialization
     *
     * @param pck packet
     */
    public void gotInitialization(packLdp pck) {
        if (pck.getSessParam()) {
            return;
        }
        if ((sessHelloHldtm / 1000) <= pck.holdTime) {
            return;
        }
        sessHelloHldtm = pck.holdTime * 1000;
        sessHelloIntrvl = sessHelloHldtm / 3;
    }

    /**
     * got notification
     *
     * @param pck packet
     */
    public void gotNotification(packLdp pck) {
        pck.getStatus();
        logger.info("got notify (" + pck.stat + "/" + pck.msgID + "/" + pck.msgTyp + ") from " + peer);
    }

    /**
     * got label mapping
     *
     * @param pck packet
     */
    public void gotLabelMap(packLdp pck) {
        if (pck.getGenLabel()) {
            return;
        }
        if (pck.getFEClist()) {
            return;
        }
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        ntry.labelRem = tabLabel.int2labels(pck.label);
        ntry.nextHop = peer.copyBytes();
        ntry.iface = ifc;
        for (int i = 0; i < pck.prfLst.size(); i++) {
            ntry.prefix = pck.prfLst.get(i);
            if (debugger.rtrLdpTraf) {
                logger.debug("rx reachable prefix=" + ntry.prefix);
            }
            if (filterIn != null) {
                if (!filterIn.matches(rtrBgpUtil.safiUnicast, ntry.prefix)) {
                    continue;
                }
            }
            prefLearn.add(tabRoute.addType.always, ntry, true, true);
        }
        for (int i = 0; i < pck.pweLst.size(); i++) {
            packLdpPwe pwe = pck.pweLst.get(i);
            pwe.label = pck.label;
            if (debugger.rtrLdpTraf) {
                logger.debug("rx reachable pseudowire=" + pwe);
            }
            pweLearn.put(pwe);
        }
        for (int i = 0; i < pck.pmpLst.size(); i++) {
            packLdpMp pmp = pck.pmpLst.get(i);
            pmp.label = pck.label;
            if (debugger.rtrLdpTraf) {
                logger.debug("rx reachable multipoint=" + pmp);
            }
            pmpLearn.put(pmp);
            ipFwdMpmp sta = new ipFwdMpmp(pmp.typ != packLdp.fecTp2mp, pmp.root, pmp.opaque);
            ipFwdMpmp old = ip.mp2mpLsp.add(sta);
            if (old != null) {
                sta = old;
            }
            sta.addPeer(peer, ifc, pmp.label, sta.mp2mp);
            sta.updateState(ip);
            if (pmp.typ != packLdp.fecTmp2mpDn) {
                continue;
            }
            pmp = sta.getReverse(peer, pmp.typ);
            if (pmp == null) {
                continue;
            }
            pmpAdvert.put(pmp);
            sendLabelMap(pmp);
        }
    }

    /**
     * got label mapping
     *
     * @param pck packet
     */
    public void gotLabelWdrw(packLdp pck) {
        pck.getGenLabel();
        if (pck.getFEClist()) {
            return;
        }
        for (int i = 0; i < pck.prfLst.size(); i++) {
            addrPrefix<addrIP> ntry = pck.prfLst.get(i);
            if (debugger.rtrLdpTraf) {
                logger.debug("rx withdraw prefix=" + ntry);
            }
            prefLearn.del(ntry);
        }
        for (int i = 0; i < pck.pweLst.size(); i++) {
            packLdpPwe pwe = pck.pweLst.get(i);
            if (debugger.rtrLdpTraf) {
                logger.debug("rx withdraw pseudowire=" + pwe);
            }
            pweLearn.del(pwe);
        }
        for (int i = 0; i < pck.pmpLst.size(); i++) {
            packLdpMp pmp = pck.pmpLst.get(i);
            if (debugger.rtrLdpTraf) {
                logger.debug("rx withdraw multipoint=" + pmp);
            }
            pmpLearn.del(pmp);
            ipFwdMpmp sta = new ipFwdMpmp(pmp.typ != packLdp.fecTp2mp, pmp.root, pmp.opaque);
            sta = ip.mp2mpLsp.find(sta);
            if (sta == null) {
                continue;
            }
            pmp = sta.getReverse(peer, pmp.typ);
            sta.delPeer(peer);
            sta.updateState(ip);
            if (pmp == null) {
                continue;
            }
            if (pmp.typ != packLdp.fecTmp2mpUp) {
                continue;
            }
            pmpAdvert.del(pmp);
            sendLabelWdrw(pmp);
        }
        for (int i = 0; i < pck.prfLst.size(); i++) {
            sendLabelRlse(pck.prfLst.get(i));
        }
        for (int i = 0; i < pck.pweLst.size(); i++) {
            sendLabelRlse(pck.pweLst.get(i));
        }
        for (int i = 0; i < pck.pmpLst.size(); i++) {
            sendLabelRlse(pck.pmpLst.get(i));
        }
    }

    /**
     * got label release
     *
     * @param pck packet
     */
    public void gotLabelRlse(packLdp pck) {
        pck.getGenLabel();
        if (pck.getFEClist()) {
            return;
        }
        for (int i = 0; i < pck.prfLst.size(); i++) {
            addrPrefix<addrIP> ntry = pck.prfLst.get(i);
            if (debugger.rtrLdpTraf) {
                logger.debug("rx release prefix=" + ntry);
            }
        }
        for (int i = 0; i < pck.pweLst.size(); i++) {
            packLdpPwe pwe = pck.pweLst.get(i);
            if (debugger.rtrLdpTraf) {
                logger.debug("rx release pseudowire=" + pwe);
            }
        }
        for (int i = 0; i < pck.pmpLst.size(); i++) {
            packLdpMp pmp = pck.pmpLst.get(i);
            if (debugger.rtrLdpTraf) {
                logger.debug("rx release multipoint=" + pmp);
            }
        }
    }

    /**
     * got label request
     *
     * @param pck packet
     */
    public void gotLabelRqst(packLdp pck) {
        if (pck.getFEClist()) {
            return;
        }
        for (int i = 0; i < pck.prfLst.size(); i++) {
            addrPrefix<addrIP> ntry = pck.prfLst.get(i);
            if (debugger.rtrLdpTraf) {
                logger.debug("rx request prefix=" + ntry);
            }
            tabRouteEntry<addrIP> prf = ip.labeldR.find(ntry);
            if (prf == null) {
                continue;
            }
            if (filterOut != null) {
                if (!filterOut.matches(rtrBgpUtil.safiUnicast, ntry)) {
                    continue;
                }
            }
            sendLabelMap(prf);
        }
        for (int i = 0; i < pck.pweLst.size(); i++) {
            packLdpPwe pwe = pck.pweLst.get(i);
            if (debugger.rtrLdpTraf) {
                logger.debug("rx request pseudowire=" + pwe);
            }
            pwe = pweNeed2adv.find(pwe);
            if (pwe == null) {
                continue;
            }
            sendLabelMap(pwe);
        }
        for (int i = 0; i < pck.pmpLst.size(); i++) {
            packLdpMp pmp = pck.pmpLst.get(i);
            if (debugger.rtrLdpTraf) {
                logger.debug("rx request multipoint=" + pmp);
            }
            pmp = pmpAdvert.find(pmp);
            if (pmp == null) {
                continue;
            }
            sendLabelMap(pmp);
        }
    }

    public void run() {
        try {
            bits.sleep(bits.random(1000, 5000));
            if (trans.compare(trans, ifc.addr) > 0) {
                if (debugger.rtrLdpEvnt) {
                    logger.debug("accepting " + trans);
                }
                prtAccept ac = new prtAccept(tcp, new pipeLine(65536, false), ifc, packLdp.port, trans, 0, 0, "ldp", ifc.ldpasFind(trans), -1);
                ac.wait4conn(30000);
                conn = ac.getConn(true);
            } else {
                if (debugger.rtrLdpEvnt) {
                    logger.debug("connecting " + trans);
                }
                conn = tcp.streamConnect(new pipeLine(65536, false), ifc, 0, trans, packLdp.port, "ldp", ifc.ldpasFind(trans), -1);
            }
            if (conn == null) {
                ip.ldpNeighDel(this);
                return;
            }
            conn.timeout = sessHelloHldtm;
            if (conn.wait4ready(0)) {
                ip.ldpNeighDel(this);
                return;
            }
            sendInitialization();
            sendKeepAlive();
            sendAddressList();
            sendKeepAlive();
            new rtrLdpNeighRx(this);
            logger.warn("neighbor " + peer + " up");
            ip.routerStaticChg();
            int i = 0;
            for (;;) {
                bits.sleep(1000);
                if (!need2run) {
                    break;
                }
                if (conn.isClosed() != 0) {
                    break;
                }
                doAdvert();
                i += 1000;
                if (i < sessHelloIntrvl) {
                    continue;
                }
                sendKeepAlive();
                i = 0;
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        logger.error("neighbor " + peer + " down");
        conn.setClose();
        ip.ldpNeighDel(this);
    }

    /**
     * do receive work
     */
    public void doRxWork() {
        try {
            for (;;) {
                if (!need2run) {
                    break;
                }
                packLdp pck = new packLdp();
                pck.conn = conn;
                pck.cntr = cntr;
                if (pck.recvPack()) {
                    break;
                }
                if (pck.parseLDPheader()) {
                    break;
                }
                for (;;) {
                    if (pck.parseMSGheader()) {
                        break;
                    }
                    switch (pck.msgTyp) {
                        case packLdp.msgTlabMap:
                            gotLabelMap(pck);
                            ip.routerStaticChg();
                            break;
                        case packLdp.msgTlabWdr:
                            gotLabelWdrw(pck);
                            ip.routerStaticChg();
                            break;
                        case packLdp.msgTlabRel:
                            gotLabelRlse(pck);
                            break;
                        case packLdp.msgTlabReq:
                            gotLabelRqst(pck);
                            break;
                        case packLdp.msgTinit:
                            gotInitialization(pck);
                            break;
                        case packLdp.msgTnotify:
                            gotNotification(pck);
                            break;
                        case packLdp.msgTkepAlv:
                            break;
                        default:
                            if (debugger.rtrLdpTraf) {
                                logger.debug("rx " + pck.type2string(pck.msgTyp));
                            }
                            break;
                    }
                }
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        if (debugger.rtrLdpEvnt) {
            logger.debug("stopped peer " + peer + " (" + trans + ")");
        }
        conn.setClose();
        ip.ldpNeighDel(this);
    }

    /**
     * do advertisement work
     */
    public void doAdvert() {
        if (conn.ready2tx() < 4096) {
            return;
        }
        for (int i = 0; i < prefAdvert.size(); i++) {
            tabRouteEntry<addrIP> ntry = prefAdvert.get(i);
            if (ntry == null) {
                continue;
            }
            if (ip.labeldR.find(ntry) != null) {
                continue;
            }
            prefAdvert.del(ntry.prefix);
            sendLabelWdrw(ntry.prefix);
            if (conn.ready2tx() < 4096) {
                return;
            }
        }
        for (int i = 0; i < ip.labeldR.size(); i++) {
            tabRouteEntry<addrIP> ntry = ip.labeldR.get(i);
            if (ntry == null) {
                continue;
            }
            if (!ntry.differs(prefAdvert.find(ntry))) {
                continue;
            }
            if (ntry.labelLoc == null) {
                continue;
            }
            if (filterOut != null) {
                if (!filterOut.matches(rtrBgpUtil.safiUnicast, ntry.prefix)) {
                    continue;
                }
            }
            prefAdvert.add(tabRoute.addType.always, ntry, true, true);
            sendLabelMap(ntry);
            if (conn.ready2tx() < 4096) {
                return;
            }
        }
        for (int i = 0; i < pweAdvert.size(); i++) {
            packLdpPwe ntry = pweAdvert.get(i);
            if (ntry == null) {
                continue;
            }
            if (pweNeed2adv.find(ntry) != null) {
                continue;
            }
            pweAdvert.del(ntry);
            sendLabelWdrw(ntry);
            if (conn.ready2tx() < 4096) {
                return;
            }
        }
        for (int i = 0; i < pweNeed2adv.size(); i++) {
            packLdpPwe ntry = pweNeed2adv.get(i);
            if (!ntry.differs(pweAdvert.find(ntry))) {
                continue;
            }
            if (ntry.label == 0) {
                continue;
            }
            pweAdvert.put(ntry);
            sendLabelMap(ntry);
            if (conn.ready2tx() < 4096) {
                return;
            }
        }
    }

}

class rtrLdpNeighRx implements Runnable {

    private rtrLdpNeigh lower;

    public rtrLdpNeighRx(rtrLdpNeigh parent) {
        lower = parent;
        new Thread(this).start();
    }

    public void run() {
        lower.doRxWork();
    }

}
