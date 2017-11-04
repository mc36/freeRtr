package pack;

import addr.addrIP;
import addr.addrPrefix;
import java.util.ArrayList;
import java.util.List;

/**
 * protocol independent multicast (rfc4601) group
 *
 * @author matecsaba
 */
public class packPimGrp {

    /**
     * group number
     */
    public addrPrefix<addrIP> group;

    /**
     * join list
     */
    public List<addrPrefix<addrIP>> joins = new ArrayList<addrPrefix<addrIP>>();

    /**
     * prune list
     */
    public List<addrPrefix<addrIP>> prunes = new ArrayList<addrPrefix<addrIP>>();

    public String toString() {
        String a = "joins=";
        for (int i = 0; i < joins.size(); i++) {
            a += joins.get(i) + " ";
        }
        a += " prunes=";
        for (int i = 0; i < prunes.size(); i++) {
            a += prunes.get(i) + " ";
        }
        return group + " " + a;
    }

}
