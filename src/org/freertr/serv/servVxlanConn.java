package org.freertr.serv;

import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrType;
import org.freertr.ifc.ifcBridgeIfc;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcUp;
import org.freertr.ip.ipFwd;
import org.freertr.pack.packHolder;
import org.freertr.pack.packVxlan;
import org.freertr.prt.prtGenConn;
import org.freertr.util.counter;
import org.freertr.util.state;

/**
 * vxlan (rfc7348) handler
 *
 * @author matecsaba
 */
public class servVxlanConn implements ifcDn, Comparable<servVxlanConn> {

    private servVxlan lower;

    /**
     * connection
     */
    protected prtGenConn conn;

    /**
     * bridge used
     */
    protected ifcBridgeIfc brdgIfc;

    /**
     * upper layer
     */
    protected ifcUp upper = new ifcNull();

    /**
     * counter
     */
    protected counter cntr = new counter();

    /**
     * creation time
     */
    protected long created;

    public int compareTo(servVxlanConn o) {
        return conn.compareTo(o.conn);
    }

    public String toString() {
        return lower + " with " + conn.peerAddr;
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
     * get instance
     *
     * @return instance
     */
    public int getInst() {
        return lower.inst;
    }

    /**
     * create instance
     *
     * @param id connection
     * @param parent lower layer
     */
    public servVxlanConn(prtGenConn id, servVxlan parent) {
        conn = id;
        lower = parent;
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
        if (upper != null) {
            upper.closeUp();
            upper = null;
        }
        if (brdgIfc != null) {
            brdgIfc.closeUp();
            brdgIfc = null;
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
        packVxlan vxl = new packVxlan();
        vxl.instance = lower.inst;
        vxl.createHeader(pck);
        pck.merge2beg();
        pck.putDefaults();
        conn.send2net(pck);
    }

}
