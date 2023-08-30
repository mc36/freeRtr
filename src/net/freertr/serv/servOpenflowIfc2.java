package net.freertr.serv;

import java.util.Comparator;
import net.freertr.ip.ipFwdIface;
import net.freertr.ip.ipIfc;

/**
 * one openflow interface
 *
 * @author matecsaba
 */
class servOpenflowIfc2 implements Comparator<servOpenflowIfc2> {

    public ipFwdIface ifc;

    public ipIfc ipi;

    public servOpenflowIfc1 ifo;

    public int compare(servOpenflowIfc2 o1, servOpenflowIfc2 o2) {
        return o1.ifc.compare(o1.ifc, o2.ifc);
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
