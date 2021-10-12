package net.freertr.tab;

import java.util.ArrayList;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrPrefix;
import net.freertr.auth.authGeneric;
import net.freertr.pack.packHolder;

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

    public List<String> usrString(String beg) {
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
