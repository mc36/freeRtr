package tab;

import addr.addrIP;
import addr.addrPrefix;
import java.util.ArrayList;
import java.util.List;
import pack.packHolder;

/**
 * represents one script line
 *
 * @author matecsaba
 */
public class tabScrptN extends tabListingEntry<addrIP> {

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
     * @param net network
     * @return false on success, true on error
     */
    public boolean matches(int afi, addrPrefix<addrIP> net) {
        return false;
    }

    /**
     * test if matches
     *
     * @param afi address family
     * @param net network
     * @return false on success, true on error
     */
    public boolean matches(int afi, tabRouteEntry<addrIP> net) {
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
     * @param net network
     */
    public void update(int afi, tabRouteEntry<addrIP> net) {
    }

}
