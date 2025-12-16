package org.freertr.rtr;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipFwdTab;
import org.freertr.ip.ipFwdTrfng;
import org.freertr.ip.ipPrt;
import org.freertr.pack.packHolder;
import org.freertr.pack.packRsvp;
import org.freertr.tab.tabHop;
import org.freertr.tab.tabLabel;
import org.freertr.tab.tabLabelEntry;
import org.freertr.tab.tabRouteEntry;
import org.freertr.util.bits;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.state;

/**
 * resource reservation protocol (rfc2205) interface
 *
 * @author matecsaba
 */
public class rtrRsvpIface implements ipPrt {

    /**
     * counter
     */
    public counter cntr = new counter();

    private final ipFwd fwdCore;

    private final ipFwdIface fwdIfc;

    /**
     * create one interface handler
     *
     * @param fwdr forwarder to use
     * @param iface interface to use
     */
    public rtrRsvpIface(ipFwd fwdr, ipFwdIface iface) {
        fwdCore = fwdr;
        fwdIfc = iface;
    }

    /**
     * register protocol
     */
    public void register2ip() {
        if (debugger.rtrRsvpEvnt) {
            logger.debug("reg to " + fwdIfc);
        }
        fwdCore.protoAdd(this, fwdIfc, null);
    }

    /**
     * unregister protocol
     */
    public void unregister2ip() {
        if (debugger.rtrRsvpEvnt) {
            logger.debug("unreg from " + fwdIfc);
        }
        fwdCore.protoDel(this, fwdIfc, null);
    }

    /**
     * get protocol number
     *
     * @return number
     */
    public int getProtoNum() {
        return packRsvp.proto;
    }

    public String toString() {
        return "rsvp";
    }

    /**
     * close interface
     *
     * @param iface interface
     */
    public void closeUp(ipFwdIface iface) {
    }

    /**
     * get counter
     *
     * @return counter
     */
    public counter getCounter() {
        return cntr;
    }

    /**
     * alert packet
     *
     * @param rxIfc interface
     * @param pckBin packet
     * @return false on success, true on error
     */
    public boolean alertPack(ipFwdIface rxIfc, packHolder pckBin) {
        recvPack(rxIfc, pckBin);
        return false;
    }

    /**
     * error packet
     *
     * @param err error code
     * @param rtr address
     * @param rxIfc interface
     * @param pck packet
     */
    public void errorPack(counter.reasons err, addrIP rtr, ipFwdIface rxIfc, packHolder pck) {
    }

    private int getHop(packRsvp pck, ipFwdTrfng ntry) {
        ntry.trgHop = new addrIP();
        boolean saw = false;
        for (;;) {
            if (pck.expRout.size() < 1) {
                break;
            }
            tabHop hp = pck.expRout.get(0);
            saw = true;
            tabRouteEntry<addrIP> rt = fwdCore.actualU.route(hp.adr);
            if (rt == null) {
                return 0;
            }
            ntry.trgIfc = (ipFwdIface) rt.best.iface;
            if (ntry.trgIfc == null) {
                return 0;
            }
            switch (rt.best.rouTyp) {
                case local:
                    pck.expRout.remove(0);
                    break;
                case conn:
                    if (ntry.trgIfc.lower.checkMyAddress(hp.adr)) {
                        pck.expRout.remove(0);
                        break;
                    }
                    ntry.trgHop.setAddr(hp.adr);
                    return 1;
                default:
                    if (rt.best.nextHop != null) {
                        ntry.trgHop.setAddr(rt.best.nextHop);
                    } else {
                        ntry.trgHop.setAddr(hp.adr);
                    }
                    return 1;
            }
        }
        if (saw) {
            return 2;
        }
        return 0;
    }

    private boolean allocLabel(ipFwdTrfng ntry) {
        if (ntry.locLab != null) {
            return false;
        }
        ipFwdTrfng prnt = ntry.getParent();
        ipFwdTrfng old = fwdCore.trafEngs.find(prnt);
        if (old != null) {
            prnt = old;
        } else {
            prnt.timeout = 1000;
            fwdCore.trafEngs.put(prnt);
        }
        if (prnt.locLab == null) {
            prnt.locLab = tabLabel.allocate(tabLabelEntry.owner.rsvp);
            if (ntry.isP2MP()) {
                prnt.locLab.clrDupMpls(tabLabelEntry.owner.rsvp);
            }
        }
        if (prnt.locLab == null) {
            return true;
        }
        ntry.locLab = prnt.locLab;
        return false;
    }

    /**
     * received packet
     *
     * @param rxIfc interface
     * @param pckBin packet
     */
    public void recvPack(ipFwdIface rxIfc, packHolder pckBin) {
        packRsvp pckRvp = new packRsvp();
        if (pckRvp.parseHeader(pckBin)) {
            logger.info("got bad header from " + pckBin.IPsrc);
            return;
        }
        ipFwdTrfng ntry;
        switch (pckRvp.typ) {
            case packRsvp.typPathReq:
                if (pckRvp.parseDatPatReq(pckBin)) {
                    logger.info("got bad pathReq from " + pckBin.IPsrc);
                    return;
                }
                if (debugger.rtrRsvpTraf) {
                    logger.debug("rx " + pckRvp);
                }
                ntry = new ipFwdTrfng(pckRvp.sndrAdr, pckRvp.sndrId, pckRvp.sbgrpOrg, pckRvp.sbgrpId);
                ipFwdTrfng old = fwdCore.trafEngs.add(ntry);
                if (old != null) {
                    ntry = old;
                }
                if (ntry.srcLoc > 0) {
                    ntry.srcLoc = 5;
                    return;
                }
                ntry.srcHop = pckRvp.hopAdr.copyBytes();
                ntry.srcIfc = ipFwdTab.findSendingIface(fwdCore, ntry.srcHop);
                if (ntry.srcIfc == null) {
                    return;
                }
                ntry.updated = bits.getTime();
                ntry.timeout = pckRvp.timeVal * 4;
                ntry.trgAdr = pckRvp.getTrg();
                ntry.trgId = pckRvp.sessId;
                ntry.trgHop = new addrIP();
                ntry.bwdt = pckRvp.flwSpcPeak;
                ntry.priS = pckRvp.sessStp;
                ntry.priH = pckRvp.sessHld;
                ntry.affE = pckRvp.sessExc;
                ntry.affI = pckRvp.sessInc;
                ntry.affM = pckRvp.sessMst;
                ntry.descr = pckRvp.sessNam;
                ntry.recRou = pckRvp.recRout != null;
                ntry.asocTyp = pckRvp.assocTyp;
                ntry.asocId = pckRvp.assocId;
                ntry.asocGlb = pckRvp.assocGlb;
                ntry.asocAdr = pckRvp.assocAdr;
                switch (getHop(pckRvp, ntry)) {
                    case 1: // forward
                        break;
                    case 2: // local
                        ntry.trgLoc = true;
                        if (pckRvp.recRout != null) {
                            pckRvp.recRout = new ArrayList<tabHop>();
                        }
                        if (allocLabel(ntry)) {
                            return;
                        }
                        ntry.locLab.setFwdCommon(tabLabelEntry.owner.rsvp, fwdCore);
                        fwdCore.tableChanger();
                        if (ntry.trgIfc.ifwNum != ntry.srcIfc.ifwNum) {
                            pckRvp.updateRecRout(ntry.trgIfc.addr, true);
                        }
                        pckRvp.updateRecRout(ntry.srcIfc.addr, true);
                        pckRvp.ttl = 255;
                        pckBin.clear();
                        pckRvp.createHolder(pckBin);
                        pckBin.IPsrc.setAddr(ntry.srcIfc.addr);
                        pckBin.IPtrg.setAddr(ntry.srcHop);
                        pckRvp.hopAdr = ntry.srcIfc.addr.copyBytes();
                        pckRvp.styleVal = 18;
                        pckRvp.labelVal = ntry.locLab.label;
                        pckRvp.createDatResReq(pckBin);
                        pckRvp.createHeader(pckBin);
                        fwdCore.protoPack(ntry.srcIfc, ntry.srcHop, pckBin);
                        if (debugger.rtrRsvpTraf) {
                            logger.debug("tx " + pckRvp);
                        }
                        return;
                    default:
                        return;
                }
                ntry.midAdrs = pckRvp.expRout;
                pckRvp.updateRecRout(ntry.srcIfc.addr, true);
                pckRvp.updateRecRout(ntry.trgIfc.addr, true);
                pckRvp.ttl--;
                pckRvp.adsHops++;
                pckBin.clear();
                ipFwdTab.fillRsvpFrst(fwdCore, ntry, pckRvp);
                pckRvp.createHolder(pckBin);
                pckRvp.hopAdr = ntry.trgIfc.addr.copyBytes();
                pckRvp.createDatPatReq(pckBin);
                pckRvp.createHeader(pckBin);
                fwdCore.protoPack(ntry.trgIfc, ntry.trgHop, pckBin);
                if (debugger.rtrRsvpTraf) {
                    logger.debug("tx " + pckRvp);
                }
                return;
            case packRsvp.typResvReq:
                if (pckRvp.parseDatResReq(pckBin)) {
                    logger.info("got bad resvReq from " + pckBin.IPsrc);
                    return;
                }
                if (debugger.rtrRsvpTraf) {
                    logger.debug("rx " + pckRvp);
                }
                if (ipFwdTab.findSendingIface(fwdCore, pckRvp.hopAdr) == null) {
                    return;
                }
                ntry = new ipFwdTrfng(pckRvp.sndrAdr, pckRvp.sndrId, pckRvp.sbgrpOrg, pckRvp.sbgrpId);
                ntry = fwdCore.trafEngs.find(ntry);
                if (ntry == null) {
                    return;
                }
                ntry.trgLab = pckRvp.labelVal;
                ntry.updated = bits.getTime();
                ntry.timeout = pckRvp.timeVal * 4;
                if (ntry.srcLoc > 0) {
                    return;
                }
                if (allocLabel(ntry)) {
                    return;
                }
                List<Integer> labs = tabLabel.int2labels(ntry.trgLab);
                if (ntry.isP2MP()) {
                    ntry.locLab.addDupMpls(tabLabelEntry.owner.rsvp, fwdCore, ntry.trgIfc, ntry.trgHop, labs);
                } else {
                    ntry.locLab.setFwdMpls(tabLabelEntry.owner.rsvp, fwdCore, ntry.trgIfc, ntry.trgHop, labs);
                }
                fwdCore.tableChanger();
                pckRvp.updateRecRout(ntry.trgIfc.addr, true);
                pckRvp.updateRecRout(ntry.srcIfc.addr, true);
                pckRvp.ttl--;
                pckBin.clear();
                pckRvp.createHolder(pckBin);
                pckBin.IPsrc.setAddr(ntry.srcIfc.addr);
                pckBin.IPtrg.setAddr(ntry.srcHop);
                pckRvp.hopAdr = ntry.srcIfc.addr.copyBytes();
                pckRvp.labelVal = ntry.locLab.label;
                pckRvp.createDatResReq(pckBin);
                pckRvp.createHeader(pckBin);
                fwdCore.protoPack(ntry.srcIfc, ntry.srcHop, pckBin);
                if (debugger.rtrRsvpTraf) {
                    logger.debug("tx " + pckRvp);
                }
                return;
            case packRsvp.typPathTear:
                if (pckRvp.parseDatPatTer(pckBin)) {
                    logger.info("got bad pathTear from " + pckBin.IPsrc);
                    return;
                }
                if (debugger.rtrRsvpTraf) {
                    logger.debug("rx " + pckRvp);
                }
                if (ipFwdTab.findSendingIface(fwdCore, pckRvp.hopAdr) == null) {
                    return;
                }
                ntry = new ipFwdTrfng(pckRvp.sndrAdr, pckRvp.sndrId, pckRvp.sbgrpOrg, pckRvp.sbgrpId);
                ntry = fwdCore.trafEngs.find(ntry);
                if (ntry == null) {
                    return;
                }
                ntry.timeout = 1;
                if (ntry.trgLoc) {
                    return;
                }
                pckRvp.ttl--;
                pckRvp.adsHops++;
                pckBin.clear();
                pckRvp.createHolder(pckBin);
                pckRvp.hopAdr = ntry.trgIfc.addr.copyBytes();
                pckRvp.createDatPatTer(pckBin);
                pckRvp.createHeader(pckBin);
                fwdCore.protoPack(ntry.trgIfc, ntry.trgHop, pckBin);
                if (debugger.rtrRsvpTraf) {
                    logger.debug("tx " + pckRvp);
                }
                return;
            case packRsvp.typResvTear:
                if (pckRvp.parseDatResTer(pckBin)) {
                    logger.info("got bad resvTear from " + pckBin.IPsrc);
                    return;
                }
                if (debugger.rtrRsvpTraf) {
                    logger.debug("rx " + pckRvp);
                }
                if (ipFwdTab.findSendingIface(fwdCore, pckRvp.hopAdr) == null) {
                    return;
                }
                ntry = new ipFwdTrfng(pckRvp.sndrAdr, pckRvp.sndrId, pckRvp.sbgrpOrg, pckRvp.sbgrpId);
                ntry = fwdCore.trafEngs.find(ntry);
                if (ntry == null) {
                    return;
                }
                if (ntry.srcLoc > 0) {
                    ntry.srcLoc = 3;
                    return;
                }
                ntry.timeout = 1;
                pckRvp.ttl--;
                pckBin.clear();
                pckRvp.createHolder(pckBin);
                pckBin.IPsrc.setAddr(ntry.srcIfc.addr);
                pckBin.IPtrg.setAddr(ntry.srcHop);
                pckRvp.hopAdr = ntry.srcIfc.addr.copyBytes();
                pckRvp.createDatResTer(pckBin);
                pckRvp.createHeader(pckBin);
                fwdCore.protoPack(ntry.srcIfc, ntry.srcHop, pckBin);
                if (debugger.rtrRsvpTraf) {
                    logger.debug("tx " + pckRvp);
                }
                return;
            case packRsvp.typPathErr:
                if (pckRvp.parseDatPatErr(pckBin)) {
                    logger.info("got bad pathErr from " + pckBin.IPsrc);
                    return;
                }
                if (debugger.rtrRsvpTraf) {
                    logger.debug("rx " + pckRvp);
                }
                ntry = new ipFwdTrfng(pckRvp.sndrAdr, pckRvp.sndrId, pckRvp.sbgrpOrg, pckRvp.sbgrpId);
                ntry = fwdCore.trafEngs.find(ntry);
                if (ntry == null) {
                    return;
                }
                if (ntry.srcLoc > 0) {
                    ntry.srcLoc = 3;
                    return;
                }
                ntry.timeout = 1;
                pckRvp.ttl--;
                pckRvp.adsHops++;
                pckBin.clear();
                pckRvp.createHolder(pckBin);
                pckBin.IPsrc.setAddr(ntry.srcIfc.addr);
                pckBin.IPtrg.setAddr(ntry.srcHop);
                pckRvp.hopAdr = ntry.srcIfc.addr.copyBytes();
                pckRvp.createDatPatErr(pckBin);
                pckRvp.createHeader(pckBin);
                fwdCore.protoPack(ntry.srcIfc, ntry.srcHop, pckBin);
                if (debugger.rtrRsvpTraf) {
                    logger.debug("tx " + pckRvp);
                }
                return;
            case packRsvp.typResvErr:
                if (pckRvp.parseDatResErr(pckBin)) {
                    logger.info("got bad resvErr from " + pckBin.IPsrc);
                    return;
                }
                if (debugger.rtrRsvpTraf) {
                    logger.debug("rx " + pckRvp);
                }
                if (ipFwdTab.findSendingIface(fwdCore, pckRvp.hopAdr) == null) {
                    return;
                }
                ntry = new ipFwdTrfng(pckRvp.sndrAdr, pckRvp.sndrId, pckRvp.sbgrpOrg, pckRvp.sbgrpId);
                ntry = fwdCore.trafEngs.find(ntry);
                if (ntry == null) {
                    return;
                }
                ntry.timeout = 1;
                if (ntry.trgLoc) {
                    return;
                }
                pckRvp.ttl--;
                pckBin.clear();
                pckRvp.createHolder(pckBin);
                pckRvp.hopAdr = ntry.trgIfc.addr.copyBytes();
                pckRvp.createDatResErr(pckBin);
                pckRvp.createHeader(pckBin);
                fwdCore.protoPack(ntry.trgIfc, ntry.trgHop, pckBin);
                if (debugger.rtrRsvpTraf) {
                    logger.debug("tx " + pckRvp);
                }
                return;
            default:
                pckRvp.parseDatAll(pckBin);
                if (debugger.rtrRsvpTraf) {
                    logger.debug("rx " + pckRvp);
                }
        }
    }

    /**
     * set state
     *
     * @param iface interface
     * @param stat state
     */
    public void setState(ipFwdIface iface, state.states stat) {
        if (stat == state.states.up) {
            return;
        }
        for (int i = fwdCore.trafEngs.size(); i >= 0; i--) {
            ipFwdTrfng ntry = fwdCore.trafEngs.get(i);
            if (ntry == null) {
                continue;
            }
            if ((ntry.srcIfc == iface) || (ntry.trgIfc == iface)) {
                fwdCore.tetunDel(ntry);
            }
        }
    }

}
