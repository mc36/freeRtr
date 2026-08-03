package org.freertr.serv;

import org.freertr.addr.addrIP;
import org.freertr.cfg.cfgIfc;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.pack.packHolder;
import org.freertr.prt.prtGre;
import org.freertr.util.bits;
import org.freertr.util.logger;
import org.freertr.util.state;

/**
 * gre handler
 *
 * @author matecsaba
 */
public class servGreConn implements Runnable, Comparable<servGreConn> {

    /**
     * lower layer
     */
    protected final servGre lower;

    /**
     * forwarder to use
     */
    protected final ipFwd fwdCor;

    /**
     * source interface
     */
    protected final ipFwdIface iface;

    /**
     * peed address
     */
    protected final addrIP peer;

    /**
     * interface
     */
    protected cfgIfc acesIfc;

    /**
     * worker to use
     */
    protected prtGre worker;

    /**
     * packet seen
     */
    protected boolean seenPack;

    /**
     * time created
     */
    protected long created;

    /**
     * create instance
     *
     * @param ifc interface to use
     * @param adr peer address
     * @param parent parent to use
     */
    public servGreConn(ipFwdIface ifc, addrIP adr, servGre parent) {
        iface = ifc;
        peer = adr.copyBytes();
        lower = parent;
        fwdCor = lower.srvVrf.getFwd(peer);
        worker = new prtGre(fwdCor);
    }

    public String toString() {
        return "gre with " + peer;
    }

    public int compareTo(servGreConn o) {
        int i = iface.compareTo(o.iface);
        if (i != 0) {
            return i;
        }
        return peer.compareTo(o.peer);
    }

    /**
     * start the process
     */
    public void doStartup() {
        worker.setEndpoints(iface, peer, false);
        worker.sendingFLW = lower.sendingFLW;
        worker.sendingTOS = lower.sendingTOS;
        worker.sendingTTL = lower.sendingTTL;
        worker.sendingDFN = lower.sendingDFN;
        acesIfc = lower.tempIfc.cloneStart(worker);
        worker.setUpper(acesIfc.ethtyp);
        acesIfc.ethtyp.setState(state.states.up);
        created = bits.getTime();
        logger.startThread(this);
    }

    /**
     * stop the process
     */
    public void doStop() {
        acesIfc.cloneStop();
        worker.closeDn();
        lower.conns.del(this);
        seenPack = false;
    }

    /**
     * received a packet
     *
     * @param pck packet to process
     */
    public void doRecv(packHolder pck) {
        seenPack = true;
        worker.recvPack(iface, pck);
    }

    public void run() {
        if (lower.srvCheckAcceptIp(iface, peer, worker)) {
            doStop();
            return;
        }
        for (;;) {
            bits.sleep(lower.timeout);
            if (!seenPack) {
                break;
            }
            seenPack = false;
        }
        doStop();
    }

}
