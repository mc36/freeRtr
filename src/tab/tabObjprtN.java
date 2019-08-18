package tab;

import addr.addrPrefix;
import addr.addrType;
import java.util.ArrayList;
import java.util.List;
import pack.packHolder;

/**
 * represents one port object group entry
 *
 * @param <T> type of address
 * @author matecsaba
 */
public class tabObjprtN<T extends addrType> extends tabListingEntry<T> {

    /**
     * source port
     */
    public tabIntMatcher port;

    /**
     * create new object group entry
     *
     * @param adr empty address to use
     */
    public tabObjprtN(T adr) {
        port = new tabIntMatcher();
    }

    /**
     * convert string to address
     *
     * @param s string to convert
     * @return true if error happened
     */
    public boolean fromString(String s) {
        action = tabPlcmapN.actionType.actPermit;
        if (port.fromString(s)) {
            return true;
        }
        return false;
    }

    public String toString() {
        return "" + port;
    }

    public List<String> usrString(String beg) {
        List<String> l = new ArrayList<String>();
        l.add(beg + "sequence " + sequence + " " + this);
        return l;
    }

    public boolean matches(int afi, addrPrefix<T> net) {
        return false;
    }

    /**
     * test if matches
     *
     * @param afi address family
     * @param net network
     * @return false on success, true on error
     */
    public boolean matches(int afi, tabRouteEntry<T> net) {
        return false;
    }

    public boolean matches(packHolder pck) {
        if (!port.matches(pck.UDPsrc)) {
            return false;
        }
        return true;
    }

    /**
     * update entry
     *
     * @param afi address family
     * @param net network
     */
    public void update(int afi, tabRouteEntry<T> net) {
    }

}
