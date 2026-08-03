package org.freertr.tab;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrPrefix;
import org.freertr.auth.authGeneric;
import org.freertr.pack.packHolder;

/**
 * authentication list entry
 *
 * @author matecsaba
 */
public class tabAuthlstN extends tabListingEntry<addrIP> {

    /**
     * create instance
     */
    public tabAuthlstN() {
    }

    /**
     * authenticator
     */
    public authGeneric auth;

    public String toString() {
        return "sequence " + sequence + " " + auth.autName;
    }

    /**
     * convert to string
     *
     * @param beg beginning
     * @param filter filter mode
     * @return string
     */
    public List<String> usrString(String beg, int filter) {
        List<String> lst = new ArrayList<String>();
        lst.add(beg + this);
        return lst;
    }

    public boolean matches(int afi, int asn, addrPrefix<addrIP> net) {
        return false;
    }

    public boolean matches(int afi, int asn, tabRouteEntry<addrIP> net) {
        return false;
    }

    public boolean matches(packHolder pck) {
        return false;
    }

    public void update(int afi, int asn, tabRouteEntry<addrIP> net) {
    }

}
