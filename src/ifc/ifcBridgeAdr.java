package ifc;

import java.util.Comparator;
import addr.addrMac;
import util.bits;
import util.counter;

/**
 * bridge address
 *
 * @author matecsaba
 */
public class ifcBridgeAdr implements Comparator<ifcBridgeAdr> {

    /**
     * address
     */
    public final addrMac adr;

    /**
     * interface
     */
    public ifcBridgeIfc ifc;

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

    /**
     * compare
     *
     * @param o1 one
     * @param o2 other
     * @return result
     */
    public int compare(ifcBridgeAdr o1, ifcBridgeAdr o2) {
        return o1.adr.compare(o1.adr, o2.adr);
    }

    public String toString() {
        return adr + "|" + ifc.getIfcName() + "|" + bits.timePast(time) + "|" + cntr.getShHwPsum(hwCntr) + "|" + cntr.getShHwBsum(hwCntr);
    }

}
