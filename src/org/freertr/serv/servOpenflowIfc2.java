package org.freertr.serv;

import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipIfc;

/**
 * one openflow interface
 *
 * @author matecsaba
 */
class servOpenflowIfc2 implements Comparable<servOpenflowIfc2> {

    /**
     * forwarder interface
     */
    protected ipFwdIface ifc;

    /**
     * backing interface
     */
    protected ipIfc ipi;

    /**
     * ovs interface
     */
    protected servOpenflowIfc1 ifo;

    public int compareTo(servOpenflowIfc2 o) {
        return ifc.compareTo(o.ifc);
    }

    /**
     * create one interface
     *
     * @param iface interface
     * @param ipifc ip interface
     * @param ofifc openflow interface
     */
    public servOpenflowIfc2(ipFwdIface iface, ipIfc ipifc, servOpenflowIfc1 ofifc) {
        ifc = iface;
        ipi = ipifc;
        ifo = ofifc;
    }

}
