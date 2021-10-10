package net.freertr.rtr;

import net.freertr.addr.addrIP;
import net.freertr.addr.addrPrefix;

/**
 * ospfv3 prefix
 *
 * @author matecsaba
 */
public class rtrOspf6pref {

    /**
     * create instance
     */
    public rtrOspf6pref() {
    }

    /**
     * options
     */
    public int option;

    /**
     * metric
     */
    public int metric;

    /**
     * prefix
     */
    public addrPrefix<addrIP> prefix;

    public String toString() {
        return "" + prefix;
    }

}
