package org.freertr.ifc;

import org.freertr.addr.addrMac;
import org.freertr.auth.authResult;
import org.freertr.auth.authenDown;
import org.freertr.auth.authenHead;
import org.freertr.auth.autherDoer;
import org.freertr.auth.autherEap;
import org.freertr.pack.packEapOL;
import org.freertr.pack.packHolder;
import org.freertr.util.bits;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.state;

/**
 * eap over lan (ieee 802.1x) protocol client handler
 *
 * @author matecsaba
 */
public class ifcEapOLclnt implements ifcUp, authenDown {

    /**
     * username to use
     */
    public String username;

    /**
     * password to use
     */
    public String password;

    /**
     * counter of this interface
     */
    public counter cntr = new counter();

    /**
     * server that sends our packets
     */
    public ifcDn lower = new ifcNull();

    /**
     * hardware address
     */
    public addrMac hwaddr = addrMac.getRandom();

    /**
     * keepalive
     */
    protected ifcEapOLclntTxKeep keepTimer;

    private autherDoer doer;

    private int gotData;

    /**
     * setup timer thread
     *
     * @param shutdown set true to shut down
     */
    public void restartTimer(boolean shutdown) {
        keepTimer = null;
        if (shutdown) {
            return;
        }
        keepTimer = new ifcEapOLclntTxKeep(this);
        keepTimer.start();
    }

    public counter getCounter() {
        return cntr;
    }

    public void setParent(ifcDn parent) {
        lower = parent;
        hwaddr = (addrMac) lower.getHwAddr();
    }

    public void closeUp() {
    }

    public void setState(state.states stat) {
    }

    /**
     * send authentication packet
     *
     * @param pck packet
     * @param proto protocol
     * @param code code
     * @param id id
     * @param msg message
     */
    public void sendAuthPack(packHolder pck, int proto, int code, int id, String msg) {
        authenHead cis = new authenHead();
        cis.code = code;
        cis.id = id;
        cis.updatePack(pck);
        packEapOL pckE = new packEapOL();
        pckE.createData(pck);
        pck.ETHsrc.setAddr(hwaddr);
        lower.sendPack(pck);
        if (debugger.ifcEapOLclnt) {
            logger.debug("tx " + msg);
        }
    }

    public void recvAuthPack(String msg) {
        if (debugger.ifcEapOLclnt) {
            logger.debug("rx " + msg);
        }
    }

    /**
     * send timed frame
     */
    public void sendTimer() {
        gotData--;
        if (gotData < 0) {
            clearState();
            packEapOL pckE = new packEapOL();
            packHolder pckB = new packHolder(true, true);
            pckE.createCmd(pckB, packEapOL.typStart);
            pckB.ETHsrc.setAddr(hwaddr);
            lower.sendPack(pckB);
            if (debugger.ifcEapOLclnt) {
                logger.debug("tx " + pckE.dump());
            }
            return;
        }
        doer.sendReq();
        if (doer.working) {
            return;
        }
        if (doer.result.result != authResult.authSuccessful) {
            return;
        }
        gotData = 20;
    }

    private void clearState() {
        doer = new autherEap(this);
        gotData = 0;
        doer.sentUser = username;
        doer.sentPass = password;
    }

    public void recvPack(packHolder pckB) {
        gotData = 10;
        packEapOL pckE = new packEapOL();
        if (pckE.parseHeader(pckB)) {
            cntr.drop(pckB, counter.reasons.badHdr);
        }
        if (debugger.ifcEapOLclnt) {
            logger.debug("rx " + pckE.dump());
        }
        switch (pckE.typ) {
            case packEapOL.typData:
                break;
            default:
                return;
        }
        authenHead cis = new authenHead();
        if (cis.parsePack(pckB)) {
            return;
        }
        if (debugger.ifcEapOLclnt) {
            logger.debug("rx " + cis);
        }
        doer.recvPck(pckB, cis.code, cis.id);
        doer.sendReq();
    }

    /**
     * create new instance
     */
    public ifcEapOLclnt() {
        clearState();
        restartTimer(false);
    }

    public String toString() {
        return "eapolC on " + lower;
    }

}

class ifcEapOLclntTxKeep implements Runnable {

    private ifcEapOLclnt lower;

    public ifcEapOLclntTxKeep(ifcEapOLclnt parent) {
        lower = parent;
    }

    public void start() {
        new Thread(this).start();
    }

    public void run() {
        try {
            for (;;) {
                if (lower.keepTimer != this) {
                    break;
                }
                lower.sendTimer();
                bits.sleep(5000);
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}
