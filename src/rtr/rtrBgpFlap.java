package rtr;

import addr.addrIP;
import addr.addrPrefix;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

/**
 * bgp4 flap statistic
 *
 * @author matecsaba
 */
public class rtrBgpFlap implements Comparator<rtrBgpFlap> {

    /**
     * prefix
     */
    public addrPrefix<addrIP> prefix;

    /**
     * counter
     */
    public long count;

    /**
     * paths seen
     */
    public List<String> paths = new ArrayList<String>();

    public int compare(rtrBgpFlap o1, rtrBgpFlap o2) {
        return o1.prefix.compare(o1.prefix, o2.prefix);
    }

    public String toString() {
        return prefix + "|" + count;
    }

}
