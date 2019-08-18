package ifc;

import addr.addrEmpty;
import addr.addrMac;
import addr.addrType;
import java.util.Timer;
import java.util.TimerTask;
import pack.packHolder;
import pack.packPppOE;
import util.counter;
import util.debugger;
import util.logger;
import util.state;
import util.typLenVal;

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
    public int keepaliveInterval = 5;

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
     * dialer interface name
     */
    public String ifcName = "";

    private Timer keepTimer;

    private typLenVal tlv = new typLenVal(packPppOE.tlv);

    private addrMac hwAddr; // hw address

    private int currState = 0; // saw: 1-pado, 2-pads

    private byte[] acCookie; // current ac cookie

    private byte[] relayId; // relay id

    private int sessionId = -1; // session id

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
        setState(state.states.close);
        upper.closeUp();
    }

    public void closeDn() {
        setState(state.states.close);
        lower.closeDn();
    }

    public void flapped() {
        clearState();
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
        try {
            keepTimer.cancel();
        } catch (Exception e) {
        }
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
        keepTimer = new Timer();
        ifcP2pOEclntTxKeep task = new ifcP2pOEclntTxKeep(this);
        keepTimer.schedule(task, 500, keepaliveInterval * 1000);
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
            cntr.rx(pck);
            pck.putStart();
            pck.putByte(0, 0xff);
            pck.putByte(1, 0x03);
            pck.putSkip(2);
            pck.merge2beg();
            upper.recvPack(pck);
            return;
        }
        if (debugger.ifcP2pOEclnt) {
            logger.debug("rx " + packPppOE.code2string(poe.cod) + " sess=" + poe.ses);
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
                sessionId = poe.ses;
                currState |= 2;
                break;
            case packPppOE.codePadT:
                if (sessionId != poe.ses) {
                    break;
                }
                clearState();
                break;
            case packPppOE.codePadR:
            case packPppOE.codePadI:
                break;
            default:
                return;
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

}

class ifcP2pOEclntTxKeep extends TimerTask {

    ifcP2pOEclnt lower;

    public ifcP2pOEclntTxKeep(ifcP2pOEclnt parent) {
        lower = parent;
    }

    public void run() {
        try {
            lower.sendKeepalive();
            lower.checkPeerState(state.states.up);
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}
