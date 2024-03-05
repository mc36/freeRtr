package org.freertr.tab;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrPrefix;
import org.freertr.auth.authLocal;
import org.freertr.pack.packHolder;

/**
 * represents one script line
 *
 * @author matecsaba
 */
public class tabScrptN extends tabListingEntry<addrIP> {

    /**
     * create instance
     */
    public tabScrptN() {
    }

    /**
     * script line
     */
    public String lin;

    /**
     * convert to string
     *
     * @param beg beginning
     * @param filter filter mode
     * @return string
     */
    public List<String> usrString(String beg, int filter) {
        List<String> l = new ArrayList<String>();
        if ((filter & 0x10000) == 0) {
            l.add(beg + "sequence " + sequence + " " + lin);
        } else {
            l.add(beg + "sequence " + sequence + " " + authLocal.passwdEncode(lin, (filter & 2) != 0));
        }
        return l;
    }

    /**
     * test if matches
     *
     * @param afi address family
     * @param asn as number
     * @param net network
     * @return false on success, true on error
     */
    public boolean matches(int afi, int asn, addrPrefix<addrIP> net) {
        return false;
    }

    /**
     * test if matches
     *
     * @param afi address family
     * @param asn as number
     * @param net network
     * @return false on success, true on error
     */
    public boolean matches(int afi, int asn, tabRouteEntry<addrIP> net) {
        return false;
    }

    /**
     * test if matches
     *
     * @param pck packet
     * @return false on success, true on error
     */
    public boolean matches(packHolder pck) {
        return false;
    }

    /**
     * update entry
     *
     * @param afi address family
     * @param asn as number
     * @param net network
     */
    public void update(int afi, int asn, tabRouteEntry<addrIP> net) {
    }

}
