package net.freertr.ifc;

import net.freertr.addr.addrMac;
import net.freertr.auth.authGeneric;
import net.freertr.auth.authenDown;
import net.freertr.auth.authenHead;
import net.freertr.auth.autherDoer;
import net.freertr.auth.autherEap;
import net.freertr.pack.packEapOL;
import net.freertr.pack.packHolder;
import net.freertr.util.counter;
import net.freertr.util.debugger;
import net.freertr.util.logger;
import net.freertr.util.state;

/**
 * eap over lan (ieee 802.1x) protocol server handler
 *
 * @author matecsaba
 */
public class ifcEapOLserv implements ifcUp, authenDown {

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

    /**
     * authenticator to use
     */
    public final authGeneric auther;

    private autherDoer doer;

    /**
     * get counter
     *
     * @return counter
     */
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
        hwaddr = (addrMac) lower.getHwAddr();
    }

    /**
     * close interface
     */
    public void closeUp() {
    }

    /**
     * set state
     *
     * @param stat state
     */
    public void setState(state.states stat) {
    }

    /**
     * received packet
     *
     * @param pckB packet
     */
    public void recvPack(packHolder pckB) {
        packEapOL pckE = new packEapOL();
        if (pckE.parseHeader(pckB)) {
            cntr.drop(pckB, counter.reasons.badHdr);
        }
        if (debugger.ifcEapOLserv) {
            logger.debug("rx " + pckE.dump());
        }
        switch (pckE.typ) {
            case packEapOL.typStart:
                clearState();
                doer.sendReq();
                return;
            case packEapOL.typLogoff:
                clearState();
                return;
            case packEapOL.typData:
                break;
            default:
                return;
        }
        authenHead cis = new authenHead();
        if (cis.parsePack(pckB)) {
            return;
        }
        if (debugger.ifcEapOLserv) {
            logger.debug("rx " + cis);
        }
        doer.recvPck(pckB, cis.code, cis.id);
        doer.sendReq();
    }

    /**
     * create new instance
     *
     * @param auth authenticator to use
     */
    public ifcEapOLserv(authGeneric auth) {
        auther = auth;
        clearState();
    }

    public String toString() {
        return "eapolS on " + lower;
    }

    private void clearState() {
        doer = new autherEap(this);
        doer.authenRem = auther;
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
        if (debugger.ifcEapOLserv) {
            logger.debug("tx " + msg);
        }
    }

    public void recvAuthPack(String msg) {
        if (debugger.ifcEapOLserv) {
            logger.debug("rx " + msg);
        }
    }

}
