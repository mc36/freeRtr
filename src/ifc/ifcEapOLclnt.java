package ifc;

import addr.addrMac;
import auth.authenDown;
import auth.authenHead;
import auth.autherDoer;
import auth.autherEap;
import java.util.Timer;
import java.util.TimerTask;
import pack.packEapOL;
import pack.packHolder;
import util.counter;
import util.debugger;
import util.logger;
import util.state;

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
    public addrMac hwaddr;

    private Timer keepTimer;

    private autherDoer doer;

    private int gotData;

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
        if (shutdown) {
            return;
        }
        keepTimer = new Timer();
        ifcEapOLclntTxKeep task = new ifcEapOLclntTxKeep(this);
        keepTimer.schedule(task, 500, 5000);
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
        if (!doer.succeed) {
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

class ifcEapOLclntTxKeep extends TimerTask {

    private ifcEapOLclnt lower;

    public ifcEapOLclntTxKeep(ifcEapOLclnt parent) {
        lower = parent;
    }

    public void run() {
        try {
            lower.sendTimer();
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}
