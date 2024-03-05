package org.freertr.ifc;

import java.util.Comparator;
import org.freertr.addr.addrIP;

/**
 * bridge group
 *
 * @author matecsaba
 */
public class ifcBridgeGrp implements Comparator<ifcBridgeGrp> {

    /**
     * address
     */
    public final addrIP adr;

    /**
     * time
     */
    public long time;

    /**
     * create address
     *
     * @param addr address
     */
    public ifcBridgeGrp(addrIP addr) {
        adr = addr;
    }

    public int compare(ifcBridgeGrp o1, ifcBridgeGrp o2) {
        return o1.adr.compare(o1.adr, o2.adr);
    }

    public String toString() {
        return "" + adr;
    }

}
