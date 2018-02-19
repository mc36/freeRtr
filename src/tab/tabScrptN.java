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

    public List<String> usrString(String beg) {
        List<String> l = new ArrayList<String>();
        l.add(beg + "sequence " + sequence + " " + lin);
        return l;
    }

    public boolean matches(int afi, addrPrefix<addrIP> net) {
        return false;
    }

    public boolean matches(int afi, tabRouteEntry<addrIP> net) {
        return false;
    }

    public boolean matches(packHolder pck) {
        return false;
    }

    public void update(int afi, tabRouteEntry<addrIP> net) {
    }

}
