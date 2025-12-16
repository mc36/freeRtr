package org.freertr.serv;

import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrType;
import org.freertr.cfg.cfgIfc;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcUp;
import org.freertr.ip.ipFwd;
import org.freertr.pack.packHolder;
import org.freertr.pack.packL2tp2;
import org.freertr.util.counter;
import org.freertr.util.state;

/**
 * layer two tunneling protocol (rfc2661) session
 *
 * @author matecsaba
 */
public class servL2tp2sess implements ifcDn, Comparable<servL2tp2sess> {

    /**
     * local session id
     */
    protected int sesLoc;

    /**
     * remote session id
     */
    protected int sesRem;

    private ifcUp upper = new ifcNull();

    private counter cntr = new counter();

    private cfgIfc ifc;

    private servL2tp2conn lower;

    /**
     * create instance
     *
     * @param parent parent
     */
    public servL2tp2sess(servL2tp2conn parent) {
        lower = parent;
    }

    public String toString() {
        return lower + "/" + sesLoc;
    }

    public int compareTo(servL2tp2sess o) {
        if (sesLoc < o.sesLoc) {
            return -1;
        }
        if (sesLoc > o.sesLoc) {
            return +1;
        }
        return 0;
    }

    /**
     * start upper layer
     */
    public void doStartup() {
        upper = new ifcNull();
        ifc = lower.lower.clnIfc.cloneStart(this);
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
        lower.enQueue(packL2tp2.createCDN(sesRem, sesLoc));
        lower.sesDel(sesLoc);
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
        return 8000000;
    }

    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        pck.putDefaults();
        lower.sesData(this, pck);
    }

    /**
     * send to upper layer
     *
     * @param pck packet to send
     */
    public void send2upper(packHolder pck) {
        cntr.rx(pck);
        upper.recvPack(pck);
    }

    /**
     * get forwarder
     *
     * @return forwarder used
     */
    public ipFwd getFwder() {
        return lower.lower.srvVrf.getFwd(lower.conn.peerAddr);
    }

    /**
     * get local address
     *
     * @return address
     */
    public addrIP getAddrLoc() {
        return lower.conn.iface.addr.copyBytes();
    }

    /**
     * get remote address
     *
     * @return address
     */
    public addrIP getAddrRem() {
        return lower.conn.peerAddr.copyBytes();
    }

    /**
     * get local port number
     *
     * @return session id, 0 if no session
     */
    public int getPortLoc() {
        return lower.conn.portLoc;
    }

    /**
     * get remote port number
     *
     * @return session id, 0 if no session
     */
    public int getPortRem() {
        return lower.conn.portRem;
    }

    /**
     * get remote session id
     *
     * @return session id, 0 if no session
     */
    public int getSessRem() {
        return sesRem;
    }

    /**
     * get remote tunn id
     *
     * @return session id, 0 if no tunnel
     */
    public int getTunnRem() {
        return lower.tunRem;
    }

}
