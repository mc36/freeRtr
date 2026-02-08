package org.freertr.ifc;

import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrMac;
import org.freertr.addr.addrType;
import org.freertr.cfg.cfgIfc;
import org.freertr.pack.packHolder;
import org.freertr.pack.packPppOE;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.state;
import org.freertr.enc.encTlv;
import org.freertr.user.userFormat;
import org.freertr.util.bits;

/**
 * ppp over ethernet (rfc2516) protocol client handler
 *
 * @author matecsaba
 */
public class ifcP2pOEclnt implements ifcUp, ifcDn {

    /**
     * last known state
     */
    public state.states lastState = state.states.down;

    /**
     * counter of this interface
     */
    public counter cntr = new counter();

    /**
     * current keepalive interval (0=disabled)
     */
    public int keepaliveInterval = 5000;

    /**
     * server that handler received packets
     */
    public ifcUp upper = new ifcNull();

    /**
     * server that sends our packets
     */
    public ifcDn lower = new ifcNull();

    /**
     * configured service name
     */
    public String serviceCfg = "";

    /**
     * current service name
     */
    public String serviceCur;

    /**
     * current ac name
     */
    public String acName = "";

    /**
     * current ac address
     */
    public addrMac acAddr = new addrMac();

    /**
     * interface to clone
     */
    public cfgIfc clnIfc;

    /**
     * keepalive
     */
    protected ifcP2pOEclntTxKeep keepTimer;

    private encTlv tlv = new encTlv(packPppOE.tlv);

    private addrMac hwAddr = addrMac.getRandom(); // hw address

    private int currState = 0; // saw: 1-pado, 2-pads, 3-conn, 4-wait

    private int roundsWait;

    private byte[] acCookie; // current ac cookie

    private byte[] relayId; // relay id

    private int sessionId = -1; // session id

    /**
     * get show
     *
     * @param l list to append
     */
    public void getShow(userFormat l) {
        l.add(acAddr + "|" + sessionId + "|" + clnIfc.name);
    }

    public counter getCounter() {
        return cntr;
    }

    /**
     * set parent
     *
     * @param parent parent
     */
    public void setParent(ifcDn parent) {
        lower = parent;
        hwAddr = (addrMac) parent.getHwAddr();
    }

    /**
     * close interface
     */
    public void closeUp() {
        sendPADt();
        setState(state.states.close);
        upper.setState(state.states.down);
    }

    public void closeDn() {
        sendPADt();
        setState(state.states.close);
        lower.closeDn();
    }

    public void flapped() {
        sendPADt();
        clearState();
        currState = 4;
        roundsWait = 5;
    }

    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
        setState(lastState);
    }

    public state.states getState() {
        return lastState;
    }

    public void setFilter(boolean promisc) {
    }

    public addrType getHwAddr() {
        return new addrEmpty();
    }

    /**
     * create new instance
     */
    public ifcP2pOEclnt() {
        clearState();
        restartTimer(false);
    }

    /**
     * set state
     *
     * @param stat state
     */
    public void setState(state.states stat) {
        stat = state.toForceable(stat);
        if (checkPeerState(stat)) {
            return;
        }
        restartTimer(false);
    }

    public String toString() {
        return "pppoeC on " + lower;
    }

    public int getMTUsize() {
        return lower.getMTUsize() - packPppOE.size;
    }

    public long getBandwidth() {
        return lower.getBandwidth();
    }

    private void clearState() {
        currState = 0;
        roundsWait = 0;
        serviceCur = serviceCfg;
        acName = "";
        acCookie = null;
        relayId = null;
        sessionId = -1;
    }

    /**
     * check peer state from timers
     *
     * @param force try to force line protocol
     * @return true if state changed
     */
    public boolean checkPeerState(state.states force) {
        state.states stat = state.states.down;
        if (currState == 3) {
            stat = state.states.up;
        }
        if (keepaliveInterval < 1) {
            stat = state.states.up;
        }
        if (state.toForceable(force) == state.states.admin) {
            stat = state.states.admin;
        }
        if (lastState == stat) {
            return false;
        }
        if (stat != state.states.up) {
            clearState();
        }
        lastState = stat;
        if (debugger.ifcP2pOEclnt) {
            logger.debug("line proto=" + state.conv2string(stat));
        }
        if (stat != state.states.up) {
            lower.flapped();
        }
        cntr.stateChange(stat);
        upper.setState(stat);
        return true;
    }

    /**
     * setup timer thread
     *
     * @param shutdown set true to shut down
     */
    public void restartTimer(boolean shutdown) {
        keepTimer = null;
        if (lastState == state.states.admin) {
            return;
        }
        if (shutdown) {
            return;
        }
        if (keepaliveInterval < 1) {
            return;
        }
        keepTimer = new ifcP2pOEclntTxKeep(this);
        keepTimer.start();
    }

    /**
     * send one padi packet
     */
    public void sendPADi() {
        if (debugger.ifcP2pOEclnt) {
            logger.debug("tx padi");
        }
        packHolder pck = new packHolder(true, true);
        pck.clear();
        pck.putStart();
        pck.ETHtrg.setAddr(addrMac.getBroadcast());
        pck.ETHsrc.setAddr(hwAddr);
        tlv.putStr(pck, packPppOE.typeSrvNam, serviceCur);
        packPppOE.updateHeader(pck, packPppOE.codePadI, 0);
        lower.sendPack(pck);
    }

    /**
     * send one padr packet
     */
    public void sendPADr() {
        if (debugger.ifcP2pOEclnt) {
            logger.debug("tx padr");
        }
        packHolder pck = new packHolder(true, true);
        pck.clear();
        pck.putStart();
        pck.ETHtrg.setAddr(acAddr);
        pck.ETHsrc.setAddr(hwAddr);
        tlv.putStr(pck, packPppOE.typeSrvNam, serviceCur);
        if (acCookie != null) {
            tlv.putBytes(pck, packPppOE.typeACcok, acCookie);
        }
        if (relayId != null) {
            tlv.putBytes(pck, packPppOE.typeRlySes, relayId);
        }
        packPppOE.updateHeader(pck, packPppOE.codePadR, 0);
        lower.sendPack(pck);
    }

    /**
     * send one padt packet
     */
    public void sendPADt() {
        if (sessionId < 0) {
            return;
        }
        if (debugger.ifcP2pOEclnt) {
            logger.debug("tx padt");
        }
        packHolder pck = new packHolder(true, true);
        pck.clear();
        pck.putStart();
        pck.ETHtrg.setAddr(acAddr);
        pck.ETHsrc.setAddr(hwAddr);
        packPppOE.updateHeader(pck, packPppOE.codePadT, sessionId);
        lower.sendPack(pck);
    }

    /**
     * send keepalive packet
     */
    public void sendKeepalive() {
        switch (currState) {
            case 0:
                sendPADi();
                break;
            case 1:
                sendPADr();
                break;
            case 3:
                break;
            case 4:
                roundsWait--;
                if (roundsWait > 0) {
                    break;
                }
                clearState();
                break;
            default:
                clearState();
                break;
        }
    }

    /**
     * received packet
     *
     * @param pck packet
     */
    public void recvPack(packHolder pck) {
        packPppOE poe = new packPppOE();
        if (poe.parseHeader(pck)) {
            cntr.drop(pck, counter.reasons.badHdr);
            return;
        }
        if (poe.cod == packPppOE.codeData) {
            if (poe.ses != sessionId) {
                cntr.drop(pck, counter.reasons.notInTab);
                return;
            }
            if (acAddr.compareTo(pck.ETHsrc) != 0) {
                cntr.drop(pck, counter.reasons.badAddr);
                return;
            }
            cntr.rx(pck);
            pck.putStart();
            pck.msbPutW(0, ifcPpp.preamble);
            pck.putSkip(2);
            pck.merge2beg();
            upper.recvPack(pck);
            return;
        }
        if (debugger.ifcP2pOEclnt) {
            logger.debug("rx " + packPppOE.code2string(poe.cod) + " sess=" + poe.ses);
        }
        for (;;) {
            if (tlv.getBytes(pck)) {
                break;
            }
            if (tlv.valTyp == packPppOE.typeEol) {
                break;
            }
            switch (tlv.valTyp) {
                case packPppOE.typeEol:
                    break;
                case packPppOE.typeSrvNam:
                    serviceCur = tlv.getStr();
                    break;
                case packPppOE.typeACnam:
                    acName = tlv.getStr();
                    break;
                case packPppOE.typeACcok:
                    acCookie = tlv.copyBytes();
                    break;
                case packPppOE.typeRlySes:
                    relayId = tlv.copyBytes();
                    break;
                case packPppOE.typeHstUnq:
                case packPppOE.typeVndSpc:
                case packPppOE.typeSrvNm:
                case packPppOE.typeSysErr:
                case packPppOE.typeGenErr:
                    break;
            }
        }
        switch (poe.cod) {
            case packPppOE.codePadO:
                if ((currState & 1) != 0) {
                    return;
                }
                acAddr.setAddr(pck.ETHsrc);
                currState |= 1;
                break;
            case packPppOE.codePadS:
                if ((currState & 2) != 0) {
                    return;
                }
                if (acAddr.compareTo(pck.ETHsrc) != 0) {
                    cntr.drop(pck, counter.reasons.badAddr);
                    return;
                }
                sessionId = poe.ses;
                currState |= 2;
                break;
            case packPppOE.codePadT:
                if (sessionId != poe.ses) {
                    cntr.drop(pck, counter.reasons.badVal);
                    return;
                }
                if (acAddr.compareTo(pck.ETHsrc) != 0) {
                    cntr.drop(pck, counter.reasons.badAddr);
                    return;
                }
                clearState();
                break;
            case packPppOE.codePadR:
            case packPppOE.codePadI:
                break;
            default:
                return;
        }
        checkPeerState(state.states.up);
        sendKeepalive();
    }

    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        if ((lastState != state.states.up) || (sessionId == -1)) {
            cntr.drop(pck, counter.reasons.notUp);
            return;
        }
        pck.merge2beg();
        pck.getSkip(2);
        pck.ETHtrg.setAddr(acAddr);
        pck.ETHsrc.setAddr(hwAddr);
        packPppOE.updateHeader(pck, packPppOE.codeData, sessionId);
        lower.sendPack(pck);
    }

    /**
     * get session info
     *
     * @param mac mac address
     * @return session id, negative if down
     */
    public int getSession(addrMac mac) {
        mac.setAddr(acAddr);
        return sessionId;
    }

}

class ifcP2pOEclntTxKeep implements Runnable {

    ifcP2pOEclnt lower;

    public ifcP2pOEclntTxKeep(ifcP2pOEclnt parent) {
        lower = parent;
    }

    public void start() {
        logger.startThread(this);
    }

    public void run() {
        try {
            for (;;) {
                if (lower.keepTimer != this) {
                    break;
                }
                lower.sendKeepalive();
                lower.checkPeerState(state.states.up);
                bits.sleep(lower.keepaliveInterval);
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}
