package net.freertr.rtr;

import java.util.Comparator;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.ip.ipFwdIface;
import net.freertr.tab.tabAceslstN;
import net.freertr.tab.tabListing;

/**
 * bgp4 listen configuration
 *
 * @author matecsaba
 */
public class rtrBgpLstn implements Comparator<rtrBgpLstn> {

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

    public int compare(rtrBgpLstn o1, rtrBgpLstn o2) {
        return o1.acl.listName.compareTo(o2.acl.listName);
    }

    /**
     * get config
     *
     * @param l list
     * @param beg beginning
     */
    public void getConfig(List<String> l, String beg) {
        l.add(beg + "listen " + acl.listName + " " + temp.tempName);
    }

}
