package net.freertr.ifc;

import net.freertr.addr.addrMac;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgIfc;
import net.freertr.pack.packHolder;
import net.freertr.pack.packPppOE;
import net.freertr.tab.tabGen;
import net.freertr.user.userFormat;
import net.freertr.util.bits;
import net.freertr.util.counter;
import net.freertr.util.debugger;
import net.freertr.util.logger;
import net.freertr.util.state;
import net.freertr.util.typLenVal;

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
    public addrMac hwaddr;

    public String toString() {
        String a = "";
        if (!serviceNam.equals("pppoe")) {
            a += " name " + serviceNam;
        }
        if (serviceDly > 0) {
            a += " delay " + serviceDly;
        }
        return clnIfc.name + a;
    }

    /**
     * get show
     *
     * @return show
     */
    public userFormat getShow() {
        userFormat l = new userFormat("|", "mac|sess|iface");
        for (int i = 0; i < clnts.size(); i++) {
            ifcP2pOEservSess ntry = clnts.get(i);
            if (ntry == null) {
                continue;
            }
            l.add(ntry.mac + "|" + ntry.sessid + "|" + ntry.ifc.name);
        }
        return l;
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
        typLenVal tlv = new typLenVal(packPppOE.tlv);
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
                if (debugger.ifcP2pOEserv) {
                    logger.debug("tx pado");
                }
                packPppOE.updateHeader(pck, packPppOE.codePadO, 0);
                ifcDelay.sendPack(serviceDly, lower, pck);
                break;
            case packPppOE.codePadR:
                if (debugger.ifcP2pOEserv) {
                    logger.debug("tx pads");
                }
                ntry.sessid = bits.random(1, 0xfffe);
                ifcP2pOEservSess old = clnts.add(ntry);
                if (old != null) {
                    ntry = old;
                } else {
                    ntry.startUpper();
                }
                packPppOE.updateHeader(pck, packPppOE.codePadS, ntry.sessid);
                lower.sendPack(pck);
                break;
            case packPppOE.codePadT:
                ntry = clnts.find(ntry);
                if (ntry == null) {
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
