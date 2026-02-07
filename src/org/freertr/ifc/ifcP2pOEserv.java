package org.freertr.ifc;

import org.freertr.addr.addrMac;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgIfc;
import org.freertr.pack.packHolder;
import org.freertr.pack.packPppOE;
import org.freertr.tab.tabGen;
import org.freertr.user.userFormat;
import org.freertr.util.bits;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.state;
import org.freertr.enc.encTlv;

/**
 * ppp over ethernet (rfc2516) protocol server handler
 *
 * @author matecsaba
 */
public class ifcP2pOEserv implements ifcUp {

    /**
     * create instance
     */
    public ifcP2pOEserv() {
    }

    /**
     * counter of this interface
     */
    public counter cntr = new counter();

    /**
     * server that sends our packets
     */
    public ifcDn lower = new ifcNull();

    /**
     * configured service name
     */
    public String serviceNam = "pppoe";

    /**
     * configured service delay
     */
    public int serviceDly = 0;

    /**
     * client limit on this entry
     */
    public int serviceMax = 0;

    /**
     * interface to clone
     */
    public cfgIfc clnIfc;

    /**
     * interface to attached
     */
    public cfgIfc pktIfc;

    /**
     * list of clients
     */
    public tabGen<ifcP2pOEservSess> clnts = new tabGen<ifcP2pOEservSess>();

    /**
     * hardware address
     */
    public addrMac hwaddr = addrMac.getRandom();

    public String toString() {
        String a = "";
        if (!serviceNam.equals("pppoe")) {
            a += " name " + serviceNam;
        }
        if (serviceDly > 0) {
            a += " delay " + serviceDly;
        }
        if (serviceMax > 0) {
            a += " sessions " + serviceMax;
        }
        return clnIfc.name + a;
    }

    /**
     * get show
     *
     * @param l list to append
     */
    public void getShow(userFormat l) {
        for (int i = 0; i < clnts.size(); i++) {
            ifcP2pOEservSess ntry = clnts.get(i);
            if (ntry == null) {
                continue;
            }
            l.add(ntry.mac + "|" + ntry.sessid + "|" + ntry.ifc.name);
        }
    }

    /**
     * do clear
     *
     * @param peer peer ip
     */
    public void doClear(addrMac peer) {
        ifcP2pOEservSess ntry = new ifcP2pOEservSess(this, peer);
        ntry = clnts.find(ntry);
        if (ntry == null) {
            return;
        }
        ntry.closeDn();
    }

    public counter getCounter() {
        return cntr;
    }

    public void setState(state.states stat) {
    }

    public void closeUp() {
    }

    public void setParent(ifcDn parent) {
        lower = parent;
        hwaddr = (addrMac) lower.getHwAddr();
    }

    private ifcP2pOEservSess findSessId(int id) {
        for (int i = 0; i < clnts.size(); i++) {
            ifcP2pOEservSess ntry = clnts.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.sessid == id) {
                return ntry;
            }
        }
        return null;
    }

    /**
     * remove client from list
     *
     * @param ntry original entry
     */
    public void delClient(ifcP2pOEservSess ntry) {
        clnts.del(ntry);
    }

    public void recvPack(packHolder pck) {
        packPppOE poe = new packPppOE();
        if (poe.parseHeader(pck)) {
            cntr.drop(pck, counter.reasons.badHdr);
            return;
        }
        ifcP2pOEservSess ntry = new ifcP2pOEservSess(this, pck.ETHsrc);
        if (poe.cod == packPppOE.codeData) {
            ntry = clnts.find(ntry);
            if (ntry == null) {
                cntr.drop(pck, counter.reasons.badAddr);
                return;
            }
            if (ntry.sessid != poe.ses) {
                cntr.drop(pck, counter.reasons.badVal);
                return;
            }
            ntry.send2upper(pck);
            return;
        }
        if (debugger.ifcP2pOEserv) {
            logger.debug("rx " + packPppOE.code2string(poe.cod) + " sess=" + poe.ses);
        }
        encTlv tlv = new encTlv(packPppOE.tlv);
        byte[] host = null;
        for (;;) {
            if (tlv.getBytes(pck)) {
                break;
            }
            if (tlv.valTyp == packPppOE.typeEol) {
                break;
            }
            switch (tlv.valTyp) {
                case packPppOE.typeHstUnq:
                    host = tlv.copyBytes();
                    break;
                case packPppOE.typeEol:
                case packPppOE.typeSrvNam:
                case packPppOE.typeACnam:
                case packPppOE.typeACcok:
                case packPppOE.typeRlySes:
                case packPppOE.typeVndSpc:
                case packPppOE.typeSrvNm:
                case packPppOE.typeSysErr:
                case packPppOE.typeGenErr:
                    break;
            }
        }
        pck.clear();
        pck.putStart();
        pck.ETHsrc.setAddr(hwaddr);
        pck.ETHtrg.setAddr(ntry.mac);
        if (host != null) {
            tlv.putBytes(pck, packPppOE.typeHstUnq, host);
        }
        tlv.putStr(pck, packPppOE.typeACnam, cfgAll.hostName);
        tlv.putStr(pck, packPppOE.typeSrvNam, serviceNam);
        switch (poe.cod) {
            case packPppOE.codePadI:
                if (serviceMax > 0) {
                    if (clnts.size() > serviceMax) {
                        cntr.drop(pck, counter.reasons.noBuffer);
                        break;
                    }
                }
                if (debugger.ifcP2pOEserv) {
                    logger.debug("tx pado");
                }
                packPppOE.updateHeader(pck, packPppOE.codePadO, 0);
                ifcDelay.sendPack(serviceDly, lower, pck);
                break;
            case packPppOE.codePadR:
                ifcP2pOEservSess old = null;
                for (int i = 0; i < 16; i++) {
                    ntry.sessid = bits.random(1, 0xfffe);
                    old = findSessId(ntry.sessid);
                    if (old == null) {
                        break;
                    }
                }
                if (old != null) {
                    logger.error("failed to allocate session id");
                    break;
                }
                old = clnts.add(ntry);
                if (old != null) {
                    ntry = old;
                } else {
                    ntry.startUpper();
                }
                if (debugger.ifcP2pOEserv) {
                    logger.debug("tx pads");
                }
                packPppOE.updateHeader(pck, packPppOE.codePadS, ntry.sessid);
                lower.sendPack(pck);
                break;
            case packPppOE.codePadT:
                ntry = clnts.find(ntry);
                if (ntry == null) {
                    return;
                }
                if (ntry.sessid != poe.ses) {
                    return;
                }
                ntry.closeDn();
                break;
            case packPppOE.codePadO:
            case packPppOE.codePadS:
                break;
            default:
                return;
        }
    }

}
