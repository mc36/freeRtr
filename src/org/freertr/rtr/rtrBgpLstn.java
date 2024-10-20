package org.freertr.rtr;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.ip.ipFwdIface;
import org.freertr.tab.tabAceslstN;
import org.freertr.tab.tabListing;
import org.freertr.util.cmds;

/**
 * bgp4 listen configuration
 *
 * @author matecsaba
 */
public class rtrBgpLstn implements Comparable<rtrBgpLstn> {

    /**
     * create instance
     */
    public rtrBgpLstn() {
    }

    /**
     * access list to match
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> acl;

    /**
     * template to clone
     */
    public rtrBgpTemp temp;

    /**
     * interface to bound
     */
    public ipFwdIface iface;

    public int compareTo(rtrBgpLstn o) {
        return acl.listName.compareTo(o.acl.listName);
    }

    /**
     * get config
     *
     * @param l list
     * @param beg beginning
     */
    public void getConfig(List<String> l, String beg) {
        l.add(beg + "listen " + acl.listName + " " + temp.tempName);
        l.add(beg + cmds.comment);
    }

}
