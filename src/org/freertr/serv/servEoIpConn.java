package org.freertr.serv;

import org.freertr.addr.addrIP;
import org.freertr.clnt.clntEoIp;
import org.freertr.ifc.ifcBridgeIfc;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.pack.packHolder;
import org.freertr.util.bits;

/**
 * eoip handler
 *
 * @author matecsaba
 */
public class servEoIpConn implements Runnable, Comparable<servEoIpConn> {

    /**
     * lower layer
     */
    protected final servEoIp lower;

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
     * worker to use
     */
    protected final clntEoIp worker;

    /**
     * bridge to use
     */
    protected ifcBridgeIfc brdgIfc;

    /**
     * time created
     */
    protected long created;

    /**
     * packet seen
     */
    private boolean seenPack;

    /**
     * create instance
     *
     * @param ifc interface to use
     * @param adr peer address
     * @param parent parent to use
     */
    public servEoIpConn(ipFwdIface ifc, addrIP adr, servEoIp parent) {
        iface = ifc;
        peer = adr.copyBytes();
        lower = parent;
        fwdCor = lower.srvVrf.getFwd(peer);
        worker = new clntEoIp();
    }

    /**
     * get remote address
     *
     * @return address
     */
    public addrIP getRemAddr() {
        return worker.getRemAddr();
    }

    /**
     * get local address
     *
     * @return address
     */
    public addrIP getLocAddr() {
        return worker.getLocAddr();
    }

    /**
     * get forwarder
     *
     * @return forwarder used
     */
    public ipFwd getFwder() {
        return lower.srvVrf.getFwd(peer);
    }

    public int compareTo(servEoIpConn o) {
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
        worker.tunId = lower.tunId;
        worker.setEndpoints(fwdCor, iface, peer);
        worker.sendingFLW = lower.sendingFLW;
        worker.sendingTOS = lower.sendingTOS;
        worker.sendingTTL = lower.sendingTTL;
        worker.sendingDFN = lower.sendingDFN;
        brdgIfc = lower.brdgIfc.bridgeHed.newIface(lower.physInt, true, false);
        worker.setUpper(brdgIfc);
        created = bits.getTime();
        new Thread(this).start();
    }

    /**
     * stop the process
     */
    public void doStop() {
        brdgIfc.closeUp();
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
