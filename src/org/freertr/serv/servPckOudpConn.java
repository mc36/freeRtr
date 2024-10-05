package org.freertr.serv;

import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrType;
import org.freertr.cfg.cfgIfc;
import org.freertr.ifc.ifcBridgeIfc;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcUp;
import org.freertr.ip.ipFwd;
import org.freertr.pack.packHolder;
import org.freertr.prt.prtGenConn;
import org.freertr.util.counter;
import org.freertr.util.state;

/**
 * packet over udp encapsulation handler
 *
 * @author matecsaba
 */
public class servPckOudpConn implements ifcDn, Comparable<servPckOudpConn> {

    private servPckOudp lower;

    /**
     * connection
     */
    protected prtGenConn conn;

    /**
     * counter
     */
    protected counter cntr = new counter();

    /**
     * upper layer
     */
    protected ifcUp upper = new ifcNull();

    /**
     * dialer used
     */
    protected cfgIfc dialIfc;

    /**
     * bridge used
     */
    protected ifcBridgeIfc brdgIfc;

    /**
     * creation time
     */
    protected long created;

    public String toString() {
        return "pckoudp with " + conn.peerAddr;
    }

    /**
     * get forwarder
     *
     * @return forwarder used
     */
    public ipFwd getFwder() {
        return lower.srvVrf.getFwd(conn.peerAddr);
    }

    /**
     * get remote address
     *
     * @return address
     */
    public addrIP getRemAddr() {
        return conn.peerAddr.copyBytes();
    }

    /**
     * get local address
     *
     * @return address
     */
    public addrIP getLocAddr() {
        return conn.iface.addr.copyBytes();
    }

    /**
     * get remote port
     *
     * @return address
     */
    public int getRemPort() {
        return conn.portRem;
    }

    /**
     * get local port
     *
     * @return address
     */
    public int getLocPort() {
        return conn.portLoc;
    }

    /**
     * create instance
     *
     * @param id connection
     * @param parent lower layer
     */
    public servPckOudpConn(prtGenConn id, servPckOudp parent) {
        conn = id;
        lower = parent;
    }

    public int compareTo(servPckOudpConn o) {
        return conn.compareTo(o.conn);
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
        lower.connDel(conn);
        upper.closeUp();
        conn.setClosing();
        if (dialIfc != null) {
            dialIfc.cloneStop();
        }
        if (brdgIfc != null) {
            brdgIfc.closeUp();
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
        pck.merge2beg();
        cntr.tx(pck);
        pck.putDefaults();
        conn.send2net(pck);
    }

}
