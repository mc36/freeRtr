package org.freertr.serv;

import org.freertr.addr.addrIP;
import org.freertr.addr.addrMac;
import org.freertr.tab.tabRouteIface;

/**
 * one p4lang neighbor
 *
 * @author matecsaba
 */
public class servP4langNei implements Comparable<servP4langNei> {

    /**
     * peer ip
     */
    protected final addrIP adr;

    /**
     * peer mac
     */
    protected addrMac mac = null;

    /**
     * peer interface
     */
    protected final servP4langIfc iface;

    /**
     * vrf instance
     */
    protected servP4langVrf vrf;

    /**
     * sent ipsec info
     */
    protected String sentIpsec = "";

    /**
     * via neighbor
     */
    protected servP4langNei viaH;

    /**
     * via interface
     */
    protected servP4langIfc viaI;

    /**
     * sent nhop info
     */
    protected int sentIgNhop = -1;

    /**
     * sent interface info
     */
    protected int sentIfc;

    /**
     * sent tunnel info
     */
    protected int sentTun;

    /**
     * neighbor id
     */
    protected int id;

    /**
     * times used
     */
    protected int need;

    /**
     * sent encapsulation
     */
    protected String sentEnc;

    /**
     * create instance
     *
     * @param ifc interface
     * @param per peer ip
     */
    protected servP4langNei(servP4langIfc ifc, addrIP per) {
        adr = per;
        iface = ifc;
    }

    public int compareTo(servP4langNei o) {
        int i = iface.compareTo(o.iface);
        if (i != 0) {
            return i;
        }
        if (iface.cloned != null) {
            return 0;
        }
        if ((iface.ifc.type == tabRouteIface.ifaceType.dialer) || (iface.ifc.type == tabRouteIface.ifaceType.tunnel) || (iface.ifc.type == tabRouteIface.ifaceType.virtppp) || (iface.ifc.type == tabRouteIface.ifaceType.template)) {
            return 0;
        }
        return adr.compareTo(o.adr);
    }

    /**
     * get via interface
     *
     * @return interface
     */
    protected servP4langIfc getVia() {
        servP4langNei via = this;
        for (int i = 0; i < 8; i++) {
            if (via.viaH == null) {
                break;
            }
            via = via.viaH;
        }
        if (via.viaI == null) {
            return via.iface;
        } else {
            return via.viaI;
        }
    }

}
