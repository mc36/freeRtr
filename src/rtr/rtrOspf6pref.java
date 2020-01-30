package rtr;

import addr.addrIP;
import addr.addrPrefix;

/**
 * ospfv3 prefix
 *
 * @author matecsaba
 */
public class rtrOspf6pref {

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

}
