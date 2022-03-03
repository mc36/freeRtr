package net.freertr.serv;

import java.util.Comparator;
import net.freertr.addr.addrEmpty;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrType;
import net.freertr.cfg.cfgIfc;
import net.freertr.ifc.ifcDn;
import net.freertr.ifc.ifcNull;
import net.freertr.ifc.ifcUp;
import net.freertr.ip.ipFwd;
import net.freertr.pack.packHolder;
import net.freertr.util.counter;
import net.freertr.util.state;

/**
 * gtp server session
 *
 * @author matecsaba
 */
public class servGtpSess implements ifcDn, Comparator<servGtpSess> {

    /**
     * local tunnel id
     */
    public int teidLoc;

    /**
     * data tunnel id
     */
    public int teidDat;

    /**
     * control tunnel id
     */
    public int teidCtr;

    /**
     * data sequence
     */
    public int seqDat;

    /**
     * lower layer
     */
    public servGtpConn lower;

    /**
     * upper layer
     */
    public ifcUp upper = new ifcNull();

    /**
     * cloned interface
     */
    public cfgIfc ifc;

    /**
     * counter
     */
    public counter cntr = new counter();

    /**
     * create instance
     *
     * @param parent lower
     */
    public servGtpSess(servGtpConn parent) {
        lower = parent;
    }

    public String toString() {
        return "amt with " + lower.connD.peerAddr;
    }

    /**
     * get forwarder
     *
     * @return forwarder used
     */
    public ipFwd getFwder() {
        return lower.lower.srvVrf.getFwd(lower.connD.peerAddr);
    }

    /**
     * get local address
     *
     * @return address
     */
    public addrIP getAddrLoc() {
        return lower.connD.iface.addr.copyBytes();
    }

    /**
     * get remote address
     *
     * @return address
     */
    public addrIP getAddrRem() {
        return lower.connD.peerAddr.copyBytes();
    }

    /**
     * get local port number
     *
     * @return session id, 0 if no session
     */
    public int getPortLoc() {
        return lower.connD.portLoc;
    }

    /**
     * get remote port number
     *
     * @return session id, 0 if no session
     */
    public int getPortRem() {
        return lower.connD.portRem;
    }

    public int compare(servGtpSess o1, servGtpSess o2) {
        if (o1.teidLoc < o2.teidLoc) {
            return -1;
        }
        if (o1.teidLoc > o2.teidLoc) {
            return +1;
        }
        return 0;
    }

    /**
     * do startup work
     */
    public void doStartup() {
        upper = new ifcNull();
        ifc = lower.lower.clnIfc.cloneStart(this);
        seqDat = 1;
    }

    public addrType getHwAddr() {
        return new addrEmpty();
    }

    public void setFilter(boolean promisc) {
    }

    public state.states getState() {
        return state.states.up;
    }

    public void closeDn() {
        lower.sesDel(teidLoc);
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

    public counter getCounter() {
        return cntr;
    }

    public int getMTUsize() {
        return 1400;
    }

    public long getBandwidth() {
        return 4000000;
    }

    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        pck.putDefaults();
        lower.sesData(this, pck);
    }

}
