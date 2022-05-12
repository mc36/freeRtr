package net.freertr.serv;

import java.util.Comparator;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrMac;
import net.freertr.cfg.cfgIfc;

/**
 * one p4lang neighbor
 *
 * @author matecsaba
 */
public class servP4langNei implements Comparator<servP4langNei> {

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

    public int compare(servP4langNei o1, servP4langNei o2) {
        int i = o1.iface.compare(o1.iface, o2.iface);
        if (i != 0) {
            return i;
        }
        if (o1.iface.cloned != null) {
            return 0;
        }
        if ((o1.iface.ifc.type == cfgIfc.ifaceType.dialer) || (o1.iface.ifc.type == cfgIfc.ifaceType.tunnel) || (o1.iface.ifc.type == cfgIfc.ifaceType.virtppp) || (o1.iface.ifc.type == cfgIfc.ifaceType.template)) {
            return 0;
        }
        return o1.adr.compare(o1.adr, o2.adr);
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
