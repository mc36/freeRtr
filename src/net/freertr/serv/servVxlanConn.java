package net.freertr.serv;

import java.util.Comparator;
import net.freertr.addr.addrEmpty;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrType;
import net.freertr.ifc.ifcBridgeIfc;
import net.freertr.ifc.ifcDn;
import net.freertr.ifc.ifcNull;
import net.freertr.ifc.ifcUp;
import net.freertr.ip.ipFwd;
import net.freertr.pack.packHolder;
import net.freertr.pack.packVxlan;
import net.freertr.prt.prtGenConn;
import net.freertr.util.counter;
import net.freertr.util.state;

/**
 * vxlan (rfc7348) handler
 *
 * @author matecsaba
 */
public class servVxlanConn implements ifcDn, Comparator<servVxlanConn> {

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

    public int compare(servVxlanConn o1, servVxlanConn o2) {
        return o1.conn.compare(o1.conn, o2.conn);
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
    public addrIP getRemote() {
        return conn.peerAddr.copyBytes();
    }

    /**
     * get local address
     *
     * @return address
     */
    public addrIP getLocal() {
        return conn.iface.addr.copyBytes();
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
