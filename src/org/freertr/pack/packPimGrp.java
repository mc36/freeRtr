package org.freertr.pack;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrPrefix;
import org.freertr.tab.tabRouteUtil;

/**
 * protocol independent multicast (rfc4601) group
 *
 * @author matecsaba
 */
public class packPimGrp {

    /**
     * create instance
     */
    public packPimGrp() {
    }

    /**
     * route distinguisher
     */
    public long rd;

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
        String a = " joins=";
        for (int i = 0; i < joins.size(); i++) {
            a += joins.get(i) + " ";
        }
        a += " prunes=";
        for (int i = 0; i < prunes.size(); i++) {
            a += prunes.get(i) + " ";
        }
        return group + " rd=" + tabRouteUtil.rd2string(rd) + a;
    }

}
