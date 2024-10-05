package org.freertr.ifc;

import org.freertr.addr.addrMac;
import org.freertr.util.bits;
import org.freertr.util.counter;

/**
 * bridge address
 *
 * @author matecsaba
 */
public class ifcBridgeAdr implements Comparable<ifcBridgeAdr> {

    /**
     * address
     */
    public final addrMac adr;

    /**
     * interface
     */
    public ifcBridgeIfc ifc;

    /**
     * static config
     */
    public boolean stat;

    /**
     * time
     */
    public long time;

    /**
     * counter
     */
    public counter cntr;

    /**
     * hardware counter
     */
    public counter hwCntr;

    /**
     * create address
     *
     * @param addr address
     */
    public ifcBridgeAdr(addrMac addr) {
        adr = addr;
    }

    /**
     * copy data
     *
     * @return copy
     */
    public ifcBridgeAdr copyBytes() {
        ifcBridgeAdr n = new ifcBridgeAdr(adr.copyBytes());
        n.ifc = ifc;
        n.time = time;
        n.cntr = cntr;
        n.hwCntr = hwCntr;
        return n;
    }

    public int compareTo(ifcBridgeAdr o) {
        return adr.compareTo(o.adr);
    }

    public String toString() {
        return adr + "|" + ifc.getIfcName() + "|" + stat + "|" + bits.timePast(time) + "|" + cntr.getShHwPsum(hwCntr) + "|" + cntr.getShHwBsum(hwCntr);
    }

}
