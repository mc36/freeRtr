package org.freertr.ifc;

import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrMac;
import org.freertr.addr.addrType;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgIfc;
import org.freertr.pack.packHolder;
import org.freertr.pack.packPppOE;
import org.freertr.util.bits;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.state;
import org.freertr.enc.encTlv;
import org.freertr.user.userFormat;

/**
 * ppp over ethernet (rfc2516) protocol relay handler
 *
 * @author matecsaba
 */
public class ifcP2pOErely implements ifcUp {

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
     * client address
     */
    public addrMac clntAdr = new addrMac();

    /**
     * client session
     */
    public int clntSes = -1;

    /**
     * hardware address
     */
    public addrMac hwaddr = addrMac.getRandom();

    /**
     * serial handler
     */
    public final ifcP2pOErelySer ser;

    /**
     * dialer handler
     */
    public final ifcP2pOErelyDial dia;

    /**
     * dialer interface handler
     */
    public final ifcDn diaI;

    /**
     * get show
     *
     * @param l list to append
     */
    public void getShow(userFormat l) {
        l.add(clntAdr + "|" + clntSes + "|" + clnIfc.name);
    }

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
     * create new instance
     *
     * @param mod true for serial, false for dialer
     */
    public ifcP2pOErely(boolean mod) {
        if (mod) {
            ser = new ifcP2pOErelySer(this);
            dia = null;
            diaI = null;
        } else {
            dia = new ifcP2pOErelyDial(this);
            ser = null;
            diaI = dia;
        }
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

    public void recvPack(packHolder pck) {
        packPppOE poe = new packPppOE();
        if (poe.parseHeader(pck)) {
            cntr.drop(pck, counter.reasons.badHdr);
            return;
        }
        if (poe.cod == packPppOE.codeData) {
            if (clntSes != poe.ses) {
                cntr.drop(pck, counter.reasons.badVal);
                return;
            }
            if (clntAdr.compareTo(pck.ETHsrc) != 0) {
                cntr.drop(pck, counter.reasons.badAddr);
                return;
            }
            pck.putStart();
            pck.msbPutW(0, ifcPpp.preamble);
            pck.putSkip(2);
            pck.merge2beg();
            if (ser != null) {
                ser.lower.sendPack(pck);
            } else {
                dia.upper.recvPack(pck);
            }
            return;
        }
        if (debugger.ifcP2pOErely) {
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
        addrMac src = pck.ETHsrc.copyBytes();
        pck.clear();
        pck.putStart();
        pck.ETHtrg.setAddr(src);
        pck.ETHsrc.setAddr(hwaddr);
        if (host != null) {
            tlv.putBytes(pck, packPppOE.typeHstUnq, host);
        }
        tlv.putStr(pck, packPppOE.typeACnam, cfgAll.hostName);
        tlv.putStr(pck, packPppOE.typeSrvNam, serviceNam);
        switch (poe.cod) {
            case packPppOE.codePadI:
                if (debugger.ifcP2pOErely) {
                    logger.debug("tx pado");
                }
                packPppOE.updateHeader(pck, packPppOE.codePadO, 0);
                ifcDelay.sendPack(serviceDly, lower, pck);
                break;
            case packPppOE.codePadR:
                if (debugger.ifcP2pOErely) {
                    logger.debug("tx pads");
                }
                clntSes = bits.random(1, 0xfffe);
                clntAdr = src;
                packPppOE.updateHeader(pck, packPppOE.codePadS, clntSes);
                lower.sendPack(pck);
                break;
            case packPppOE.codePadT:
                if (clntSes != poe.ses) {
                    cntr.drop(pck, counter.reasons.badVal);
                    return;
                }
                if (clntAdr.compareTo(src) != 0) {
                    cntr.drop(pck, counter.reasons.badAddr);
                    return;
                }
                clntAdr = new addrMac();
                clntSes = -1;
                break;
            case packPppOE.codePadO:
            case packPppOE.codePadS:
                break;
            default:
                return;
        }
    }

    /**
     * send one packet
     *
     * @param pck packet to send
     */
    protected void doTxPack(packHolder pck) {
        if (clntSes == -1) {
            cntr.drop(pck, counter.reasons.notInTab);
            return;
        }
        cntr.rx(pck);
        pck.getSkip(2);
        pck.ETHtrg.setAddr(clntAdr);
        pck.ETHsrc.setAddr(hwaddr);
        packPppOE.updateHeader(pck, packPppOE.codeData, clntSes);
        lower.sendPack(pck);
    }

    /**
     * get session info
     *
     * @param mac mac address
     * @return session id, negative if down
     */
    public int getSession(addrMac mac) {
        mac.setAddr(clntAdr);
        return clntSes;
    }

}

class ifcP2pOErelySer implements ifcUp {

    public final ifcP2pOErely peer;

    public counter cntr = new counter();

    public ifcDn lower = new ifcNull();

    public ifcP2pOErelySer(ifcP2pOErely lower) {
        peer = lower;
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
    }

    public void recvPack(packHolder pck) {
        peer.doTxPack(pck);
    }

}

class ifcP2pOErelyDial implements ifcDn {

    public final ifcP2pOErely peer;

    public counter cntr = new counter();

    public ifcUp upper = new ifcNull();

    public ifcP2pOErelyDial(ifcP2pOErely lower) {
        peer = lower;
    }

    public counter getCounter() {
        return cntr;
    }

    public void sendPack(packHolder pck) {
        peer.doTxPack(pck);
    }

    public addrType getHwAddr() {
        return new addrEmpty();
    }

    public void setFilter(boolean promisc) {
    }

    public state.states getState() {
        return state.states.up;
    }

    public void closeDn() {
    }

    public void flapped() {
    }

    public int getMTUsize() {
        return peer.lower.getMTUsize() - packPppOE.size;
    }

    public long getBandwidth() {
        return peer.lower.getBandwidth();
    }

    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
    }

}
