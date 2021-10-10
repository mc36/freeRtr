package net.freertr.tab;

import java.util.ArrayList;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrPrefix;
import net.freertr.pack.packHolder;

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
     * @return string
     */
    public List<String> usrString(String beg) {
        List<String> l = new ArrayList<String>();
        l.add(beg + "sequence " + sequence + " " + lin);
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
