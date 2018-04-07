package ifc;

import addr.addrMac;
import addr.addrType;
import cfg.cfgIfc;
import java.util.Comparator;
import pack.packHolder;
import pack.packPppOE;
import tab.tabGen;
import util.bits;
import util.counter;
import util.debugger;
import util.logger;
import util.state;
import util.typLenVal;

/**
 * ppp over ethernet (rfc2516) protocol server handler
 *
 * @author matecsaba
 */
public class ifcP2pOEserv implements ifcUp {

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
     * list of clients
     */
    public tabGen<ifcP2pOEservNtry> clnts = new tabGen<ifcP2pOEservNtry>();

    /**
     * hardware address
     */
    public addrMac hwaddr;

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
     * @param ntry
     */
    public void delClient(ifcP2pOEservNtry ntry) {
        clnts.del(ntry);
    }

    public void recvPack(packHolder pck) {
        packPppOE poe = new packPppOE();
        if (poe.parseHeader(pck)) {
            cntr.drop(pck, counter.reasons.badHdr);
            return;
        }
        ifcP2pOEservNtry ntry = new ifcP2pOEservNtry(this, pck.ETHsrc);
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
        tlv.putStr(pck, packPppOE.typeACnam, "" + hwaddr);
        tlv.putStr(pck, packPppOE.typeSrvNam, serviceCfg);
        switch (poe.cod) {
            case packPppOE.codePadI:
                if (debugger.ifcP2pOEserv) {
                    logger.debug("tx pado");
                }
                packPppOE.updateHeader(pck, packPppOE.codePadO, 0);
                lower.sendPack(pck);
                break;
            case packPppOE.codePadR:
                if (debugger.ifcP2pOEserv) {
                    logger.debug("tx pads");
                }
                ifcP2pOEservNtry old = clnts.add(ntry);
                if (old != null) {
                    ntry = old;
                } else {
                    ntry.sessid = bits.randomW() + 1;
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

class ifcP2pOEservNtry implements ifcDn, Comparator<ifcP2pOEservNtry> {

    public addrMac mac;

    public ifcP2pOEserv lower;

    public counter cntr = new counter();

    public cfgIfc ifc;

    public ifcUp upper = new ifcNull();

    public int sessid;

    public int compare(ifcP2pOEservNtry o1, ifcP2pOEservNtry o2) {
        return mac.compare(o1.mac, o2.mac);
    }

    public ifcP2pOEservNtry(ifcP2pOEserv parent, addrMac addr) {
        lower = parent;
        mac = addr.copyBytes();
    }

    public void startUpper() {
        upper = new ifcNull();
        ifc = lower.clnIfc.cloneStart(this);
    }

    public counter getCounter() {
        return cntr;
    }

    public int getMTUsize() {
        return lower.lower.getMTUsize() - packPppOE.size;
    }

    public long getBandwidth() {
        return lower.lower.getBandwidth();
    }

    public state.states getState() {
        return state.states.up;
    }

    public void closeDn() {
        lower.delClient(this);
        upper.closeUp();
        if (ifc != null) {
            ifc.cloneStop();
        }
    }

    public void flapped() {
        closeDn();
    }

    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
    }

    public addrType getHwAddr() {
        return lower.hwaddr.copyBytes();
    }

    public void setFilter(boolean promisc) {
    }

    public void send2upper(packHolder pck) {
        cntr.rx(pck);
        pck.putStart();
        pck.putByte(0, 0xff);
        pck.putByte(1, 0x03);
        pck.putSkip(2);
        pck.merge2beg();
        upper.recvPack(pck);
    }

    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        pck.merge2beg();
        pck.getSkip(2);
        pck.ETHtrg.setAddr(mac);
        pck.ETHsrc.setAddr(lower.hwaddr);
        packPppOE.updateHeader(pck, packPppOE.codeData, sessid);
        lower.lower.sendPack(pck);
    }

}
