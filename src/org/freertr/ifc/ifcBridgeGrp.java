package org.freertr.ifc;

import org.freertr.addr.addrIP;

/**
 * bridge group
 *
 * @author matecsaba
 */
public class ifcBridgeGrp implements Comparable<ifcBridgeGrp> {

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

    public int compareTo(ifcBridgeGrp o) {
        return adr.compareTo(o.adr);
    }

    public String toString() {
        return "" + adr;
    }

}
