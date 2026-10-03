package org.freertr.serv;

import org.freertr.addr.addrIP;
import org.freertr.addr.addrMac;
import org.freertr.addr.addrType;
import org.freertr.clnt.clntSrEth;
import org.freertr.ifc.ifcBridgeIfc;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcUp;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.pack.packHolder;
import org.freertr.util.bits;
import org.freertr.util.counter;
import org.freertr.util.logger;
import org.freertr.util.state;

/**
 * segment routing ethernet handler
 *
 * @author matecsaba
 */
public class servSrEthConn implements Runnable, ifcDn, Comparable<servSrEthConn> {

    private servSrEth lower;

    private ipFwd fwdCor;

    private ipFwdIface iface;

    /**
     * peer address
     */
    protected addrIP peer;

    /**
     * bridge used
     */
    protected ifcBridgeIfc brdgIfc;

    /**
     * creation time
     */
    protected long created;

    private boolean seenPack;

    private counter cntr = new counter();

    /**
     * upper layer
     */
    protected ifcUp upper = new ifcNull();

    /**
     * create instance
     *
     * @param ifc interface
     * @param adr address
     * @param parent lower
     */
    public servSrEthConn(ipFwdIface ifc, addrIP adr, servSrEth parent) {
        iface = ifc;
        peer = adr.copyBytes();
        lower = parent;
        fwdCor = lower.srvVrf.getFwd(peer);
    }

    public String toString() {
        return "sreth with " + peer;
    }

    /**
     * get forwarder
     *
     * @return forwarder used
     */
    public ipFwd getFwder() {
        return lower.srvVrf.getFwd(peer);
    }

    /**
     * get remote address
     *
     * @return address
     */
    public addrIP getRemAddr() {
        return peer.copyBytes();
    }

    /**
     * get local address
     *
     * @return address
     */
    public addrIP getLocAddr() {
        return iface.addr.copyBytes();
    }

    public int compareTo(servSrEthConn o) {
        int i = iface.compareTo(o.iface);
        if (i != 0) {
            return i;
        }
        return peer.compareTo(o.peer);
    }

    /**
     * start work
     */
    protected void doStartup() {
        brdgIfc = lower.brdgIfc.bridgeHed.newIface(lower.physInt, true, false);
        setUpper(brdgIfc);
        created = bits.getTime();
        logger.startThread(this);
    }

    /**
     * process packet
     *
     * @param pck packet
     */
    protected void doRecv(packHolder pck) {
        seenPack = true;
        cntr.rx(pck);
        upper.recvPack(pck);
    }

    /**
     * stop work
     */
    protected void doStop() {
        brdgIfc.closeUp();
        fwdCor.protoDel(lower, iface, peer);
        lower.conns.del(this);
    }

    public void run() {
        if (lower.srvCheckAcceptIp(iface, peer, lower)) {
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

    public void sendPack(packHolder pckBin) {
        pckBin.merge2beg();
        cntr.tx(pckBin);
        pckBin.putDefaults();
        if (lower.sendingTTL >= 0) {
            pckBin.IPttl = lower.sendingTTL;
        }
        if (lower.sendingTOS >= 0) {
            pckBin.IPtos = lower.sendingTOS;
        }
        if (lower.sendingDFN >= 0) {
            pckBin.IPdf = lower.sendingDFN == 1;
        }
        if (lower.sendingFLW >= 0) {
            pckBin.IPid = lower.sendingFLW;
        }
        pckBin.IPprt = clntSrEth.prot;
        pckBin.IPsrc.setAddr(iface.addr);
        pckBin.IPtrg.setAddr(peer);
        fwdCor.protoPack(iface, null, pckBin);
    }

    public addrType getHwAddr() {
        return addrMac.getRandom();
    }

    public void setFilter(boolean promisc) {
    }

    public state.states getState() {
        return state.states.up;
    }

    public void closeDn() {
        doStop();
    }

    public void flapped() {
    }

    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
    }

    public counter getCounter() {
        return cntr;
    }

    public int getMTUsize() {
        return 1400;
    }

    public long getBandwidth() {
        return 8000000;
    }

}
