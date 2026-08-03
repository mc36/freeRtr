package org.freertr.rtr;

import org.freertr.addr.addrIsis;
import org.freertr.addr.addrType;
import org.freertr.util.bits;

/**
 * isis spf address
 *
 * @author matecsaba
 */
public class rtrIsisLevelSpf extends addrType {

    /**
     * size of address
     */
    public final static int size = addrIsis.size + 1;

    /**
     * node address
     */
    private addrIsis adr;

    /**
     * pseudonode id
     */
    private int psn;

    /**
     * create instance
     *
     * @param a node
     * @param p pseudonode
     */
    public rtrIsisLevelSpf(addrIsis a, int p) {
        adr = a;
        psn = p;
        setUp();
    }

    private void setUp() {
        adr.toBuffer(addr, 0);
        addr[addrIsis.size] = (byte) psn;
    }

    public int getSize() {
        return size;
    }

    public rtrIsisLevelSpf copyBytes() {
        return new rtrIsisLevelSpf(adr, psn);
    }

    public String toString() {
        return adr + "." + bits.toHexB(psn);
    }

    /**
     * convert from string
     *
     * @param s string to convert
     * @return true on error, false on success
     */
    public boolean fromString(String s) {
        int i = s.lastIndexOf(".");
        if (i < 0) {
            return true;
        }
        if (adr.fromString(s.substring(0, i))) {
            return true;
        }
        psn = bits.fromHex(s.substring(i + 1, s.length()));
        setUp();
        return false;
    }

}
