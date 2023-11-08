package net.freertr.serv;

import java.util.Comparator;
import net.freertr.addr.addrEmpty;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrType;
import net.freertr.cfg.cfgIfc;
import net.freertr.ifc.ifcBridgeIfc;
import net.freertr.ifc.ifcDn;
import net.freertr.ifc.ifcNull;
import net.freertr.ifc.ifcPpp;
import net.freertr.ifc.ifcUp;
import net.freertr.ip.ipFwd;
import net.freertr.pack.packHolder;
import net.freertr.pack.packL2tp3;
import net.freertr.pack.packLdpPwe;
import net.freertr.util.counter;
import net.freertr.util.state;

/**
 * layer two tunneling protocol v3 (rfc3931) session
 *
 * @author matecsaba
 */
public class servL2tp3sess implements ifcDn, Comparator<servL2tp3sess> {

    private final servL2tp3conn lower;

    private counter cntr = new counter();

    /**
     * session id local
     */
    protected int sesLoc;

    /**
     * session id remote
     */
    protected int sesRem;

    /**
     * vc id
     */
    protected String vcid;

    /**
     * pseudowire type
     */
    protected int pwType;

    /**
     * upper interface
     */
    protected ifcUp upper = new ifcNull();

    /**
     * bridge interface
     */
    protected ifcBridgeIfc brdgIfc;

    /**
     * dialer interface
     */
    protected cfgIfc dialIfc;

    /**
     * create instance
     *
     * @param parent connection
     */
    public servL2tp3sess(servL2tp3conn parent) {
        lower = parent;
    }

    public String toString() {
        return lower + "/" + vcid;
    }

    public int compare(servL2tp3sess o1, servL2tp3sess o2) {
        if (o1.sesLoc < o2.sesLoc) {
            return -1;
        }
        if (o1.sesLoc > o2.sesLoc) {
            return +1;
        }
        return 0;
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
        lower.enQueue(packL2tp3.createCDN(sesRem, sesLoc));
        lower.session.del(this);
        if (upper != null) {
            upper.closeUp();
            upper = null;
        }
        if (dialIfc != null) {
            dialIfc.cloneStop();
            dialIfc = null;
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
        pck.merge2beg();
        if (pwType == packLdpPwe.pwtPpp) {
            pck.getSkip(2);
        }
        packL2tp3 tx = new packL2tp3();
        tx.ctrl = false;
        tx.sesID = sesRem;
        tx.createHeader(pck);
        cntr.tx(pck);
        pck.putDefaults();
        lower.sendProto(pck);
    }

    public void doRecv(packHolder pck) {
        if (pwType == packLdpPwe.pwtPpp) {
            pck.msbPutW(0, ifcPpp.preamble);
            pck.putSkip(2);
            pck.merge2beg();
        }
        cntr.rx(pck);
        upper.recvPack(pck);
    }

    /**
     * get forwarder
     *
     * @return forwarder used
     */
    public ipFwd getFwder() {
        return lower.lower.srvVrf.getFwd(lower.peer);
    }

    /**
     * get local address
     *
     * @return address
     */
    public addrIP getAddrLoc() {
        return lower.iface.addr.copyBytes();
    }

    /**
     * get remote address
     *
     * @return address
     */
    public addrIP getAddrRem() {
        return lower.peer.copyBytes();
    }

    /**
     * get remote session id
     *
     * @return session id, 0 if no session
     */
    public int getSessRem() {
        return sesRem;
    }

}
