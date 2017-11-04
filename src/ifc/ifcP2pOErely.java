package ifc;

import addr.addrMac;
import cfg.cfgIfc;
import pack.packHolder;
import pack.packPppOE;
import util.bits;
import util.counter;
import util.debugger;
import util.logger;
import util.state;
import util.typLenVal;

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
    public String serviceCfg = "pppoe";

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
    public int clntSes = 0;

    /**
     * hardware address
     */
    public addrMac hwaddr;

    /**
     * serial handler
     */
    public final ifcP2pOErelySer peer = new ifcP2pOErelySer(this);

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
            if (hwaddr.compare(clntAdr, pck.ETHsrc) != 0) {
                cntr.drop(pck, counter.reasons.badAddr);
                return;
            }
            pck.putStart();
            pck.msbPutW(0, 0xff03);
            pck.putSkip(2);
            pck.merge2beg();
            peer.lower.sendPack(pck);
            return;
        }
        if (debugger.ifcP2pOErely) {
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
        addrMac src = pck.ETHsrc.copyBytes();
        pck.clear();
        pck.putStart();
        pck.ETHtrg.setAddr(src);
        pck.ETHsrc.setAddr(hwaddr);
        if (host != null) {
            tlv.putBytes(pck, packPppOE.typeHstUnq, host);
        }
        tlv.putStr(pck, packPppOE.typeACnam, "" + hwaddr);
        tlv.putStr(pck, packPppOE.typeSrvNam, serviceCfg);
        switch (poe.cod) {
            case packPppOE.codePadI:
                if (debugger.ifcP2pOErely) {
                    logger.debug("tx pado");
                }
                packPppOE.updateHeader(pck, packPppOE.codePadO, 0);
                lower.sendPack(pck);
                break;
            case packPppOE.codePadR:
                if (debugger.ifcP2pOErely) {
                    logger.debug("tx pads");
                }
                clntSes = bits.randomW() + 1;
                clntAdr = src;
                packPppOE.updateHeader(pck, packPppOE.codePadS, clntSes);
                lower.sendPack(pck);
                break;
            case packPppOE.codePadT:
                if (hwaddr.compare(clntAdr, pck.ETHsrc) != 0) {
                    cntr.drop(pck, counter.reasons.badAddr);
                    return;
                }
                clntAdr = new addrMac();
                clntSes = 0;
                break;
            case packPppOE.codePadO:
            case packPppOE.codePadS:
                break;
            default:
                return;
        }
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
        if (peer.clntSes == 0) {
            cntr.drop(pck, counter.reasons.notInTab);
            return;
        }
        cntr.rx(pck);
        pck.getSkip(2);
        pck.ETHtrg.setAddr(peer.clntAdr);
        pck.ETHsrc.setAddr(peer.hwaddr);
        packPppOE.updateHeader(pck, packPppOE.codeData, peer.clntSes);
        peer.lower.sendPack(pck);
    }

}
