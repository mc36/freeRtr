package org.freertr.serv;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.cfg.cfgBrdg;
import org.freertr.ifc.ifcBridgeIfc;
import org.freertr.ifc.ifcEthTyp;
import org.freertr.spf.spfCalc;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabRoute;
import org.freertr.tab.tabRouteIface;

/**
 * stack forwarder
 *
 * @author matecsaba
 */
public class servStackFwd {

    /**
     * parent
     */
    protected final servStack lower;

    /**
     * create instance
     *
     * @param parent parent
     */
    protected servStackFwd(servStack parent) {
        lower = parent;
    }

    public String toString() {
        return "fwd " + id;
    }

    /**
     * forwarder number
     */
    protected int id;

    /**
     * needed peer
     */
    protected addrIP remote = new addrIP();

    /**
     * interfaces
     */
    protected tabGen<servStackIfc> ifaces = new tabGen<servStackIfc>();

    /**
     * last spf
     */
    protected spfCalc<addrIP> spf;

    /**
     * computed routes
     */
    protected tabRoute<addrIP> routes;

    /**
     * p4lang
     */
    protected servP4lang p4;

    /**
     * openflow
     */
    protected servOpenflow of;

    /**
     * get hardware forwarder
     *
     * @return offload info
     */
    protected String getShGetName() {
        if (p4 != null) {
            return "p4=" + p4.srvName;
        }
        if (of != null) {
            return "of=" + of.srvName;
        }
        return null;
    }

    /**
     * get hardware forwarder
     *
     * @return offload info
     */
    protected String getShGet1liner() {
        if (p4 != null) {
            return p4.getShGenOneLiner();
        }
        if (of != null) {
            return of.getShGenOneLiner();
        }
        return null;
    }

    /**
     * get configuration
     *
     * @param beg text to prepend
     * @param mid text to prepend
     * @param l text to append
     */
    protected void getShowRun(String beg, String mid, List<String> l) {
        if (p4 != null) {
            l.add(beg + mid + "p4lang " + p4.srvName);
        }
        if (of != null) {
            l.add(beg + mid + "openflow " + of.srvName);
        }
        for (int i = 0; i < ifaces.size(); i++) {
            servStackIfc ntry = ifaces.get(i);
            l.add(beg + mid + "backplane " + ntry.pi.name + " " + ntry.metric);
        }
        l.add(beg + mid + "remote " + remote);
    }

    /**
     * find interface
     *
     * @param ifc interface
     * @return true if found
     */
    protected boolean findIfc(cfgBrdg ifc) {
        if (p4 != null) {
            return p4.findIfc(ifc) != null;
        }
        if (of != null) {
            return of.findIfc(ifc) != null;
        }
        return false;
    }

    /**
     * find interface
     *
     * @param ifc interface
     * @return true if found
     */
    protected boolean findIfc(ifcBridgeIfc ifc) {
        if (p4 != null) {
            return p4.findIfc(ifc) != null;
        }
        if (of != null) {
            return of.findIfc(ifc) != null;
        }
        return false;
    }

    /**
     * find interface
     *
     * @param ifc interface
     * @return true if found
     */
    protected boolean findIfc(tabRouteIface ifc) {
        if (p4 != null) {
            return p4.findIfc(ifc) != null;
        }
        if (of != null) {
            return of.findIfc(ifc) != null;
        }
        return false;
    }

    /**
     * find interface
     *
     * @param ifc interface
     * @return true if found
     */
    protected boolean findIfc(ifcEthTyp ifc) {
        if (p4 != null) {
            return p4.findIfc(ifc) != null;
        }
        if (of != null) {
            return of.findIfc(ifc) != null;
        }
        return false;
    }

    /**
     * reindex
     */
    protected void reindex() {
        for (int i = 0; i < ifaces.size(); i++) {
            servStackIfc ntry = ifaces.get(i);
            ntry.id = i;
        }
    }

}
