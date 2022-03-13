package net.freertr.serv;

import java.util.Comparator;
import net.freertr.addr.addrEmpty;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrType;
import net.freertr.cfg.cfgIfc;
import net.freertr.ifc.ifcBridgeIfc;
import net.freertr.ifc.ifcDn;
import net.freertr.ifc.ifcNull;
import net.freertr.ifc.ifcUp;
import net.freertr.ip.ipFwd;
import net.freertr.pack.packHolder;
import net.freertr.prt.prtGenConn;
import net.freertr.util.counter;
import net.freertr.util.state;

/**
 * packet over udp encapsulation handler
 *
 * @author matecsaba
 */
public class servPckOudpConn implements ifcDn, Comparator<servPckOudpConn> {

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

    public int compare(servPckOudpConn o1, servPckOudpConn o2) {
        return o1.conn.compare(o1.conn, o2.conn);
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
