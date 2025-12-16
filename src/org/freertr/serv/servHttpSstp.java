package org.freertr.serv;

import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrType;
import org.freertr.cfg.cfgIfc;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcUp;
import org.freertr.pack.packHolder;
import org.freertr.pack.packSstp;
import org.freertr.pipe.pipeSide;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.state;

/**
 * http sstp connection
 *
 * @author matecsaba
 */
public class servHttpSstp implements Runnable, ifcDn {

    /**
     * cloned interface
     */
    protected cfgIfc ifc = null;

    private counter cntr = new counter();

    private final pipeSide pipe;

    private ifcUp upper = new ifcNull();

    private final servHttpConn lower;

    /**
     * create instance
     *
     * @param parent lower layer
     */
    public servHttpSstp(servHttpConn parent) {
        lower = parent;
        pipe = parent.pipe;
    }

    /**
     * do the startup
     */
    protected void doStart() {
        lower.gotKeep = false;
        lower.pipe = null;
        new Thread(this).start();
    }

    public void run() {
        try {
            doWork();
        } catch (Exception e) {
            logger.traceback(e);
        }
        pipe.setClose();
        if (ifc != null) {
            ifc.cloneStop();
        }
    }

    private void doWork() {
        packSstp pckS = new packSstp(pipe);
        packHolder pckB = pckS.recvPack();
        if (pckB == null) {
            return;
        }
        if (pckS.parseCtrl(pckB)) {
            return;
        }
        if (debugger.servHttpTraf) {
            logger.debug("rx " + pckS.dump());
        }
        if (pckS.parseConnReq()) {
            return;
        }
        pckS.fillConnAck();
        pckS.createConnAck(pckB);
        pckS.sendCtrl(pckB);
        if (debugger.servHttpTraf) {
            logger.debug("tx " + pckS.dump());
        }
        ifc = lower.gotHost.allowSstp.cloneStart(this);
        for (;;) {
            if (doRound()) {
                break;
            }
        }
    }

    private boolean doRound() {
        packSstp pckS = new packSstp(pipe);
        packHolder pckB = pckS.recvPack();
        if (pckB == null) {
            return true;
        }
        if (pckS.control) {
            pckS.parseCtrl(pckB);
            if (debugger.servHttpTraf) {
                logger.debug("rx " + pckS.dump());
            }
            return false;
        }
        upper.recvPack(pckB);
        return false;
    }

    public void sendPack(packHolder pck) {
        packSstp pckS = new packSstp(pipe);
        pck.putDefaults();
        pckS.sendData(pck);
    }

    public counter getCounter() {
        return cntr;
    }

    public void closeDn() {
        pipe.setClose();
    }

    public void flapped() {
        pipe.setClose();
    }

    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
    }

    public state.states getState() {
        return state.states.up;
    }

    public void setFilter(boolean promisc) {
    }

    public addrType getHwAddr() {
        return new addrEmpty();
    }

    public int getMTUsize() {
        return 1504;
    }

    public long getBandwidth() {
        return 8000000;
    }

    public String toString() {
        return "sstp";
    }

}
