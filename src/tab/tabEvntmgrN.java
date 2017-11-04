package tab;

import addr.addrIP;
import addr.addrPrefix;
import java.util.ArrayList;
import java.util.List;
import pack.packHolder;

/**
 * represents one event manager entry
 *
 * @author matecsaba
 */
public class tabEvntmgrN extends tabListingEntry<addrIP> {

    /**
     * tcl command
     */
    public String tcl;

    public List<String> usrString(String beg) {
        List<String> l = new ArrayList<String>();
        l.add(beg + "sequence " + sequence + " tcl " + tcl);
        return l;
    }

    /**
     * convert string to address
     *
     * @param s string to convert
     * @return true if error happened
     */
    public boolean fromString(String s) {
        tcl = s;
        return false;
    }

    public boolean matches(addrPrefix<addrIP> net) {
        return false;
    }

    public boolean matches(tabRouteEntry<addrIP> net) {
        return false;
    }

    public boolean matches(packHolder pck) {
        return false;
    }

    public void update(tabRouteEntry<addrIP> net) {
    }

}
