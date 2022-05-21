package net.freertr.tab;

import java.util.ArrayList;
import java.util.List;
import net.freertr.addr.addrPrefix;
import net.freertr.addr.addrType;
import net.freertr.pack.packHolder;

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
        action = tabListingEntry.actionType.actPermit;
        if (port.fromString(s)) {
            return true;
        }
        return false;
    }

    public String toString() {
        return "" + port;
    }

    /**
     * convert to string
     *
     * @param beg beginning
     * @param filter filter mode
     * @return string
     */
    public List<String> usrString(String beg, int filter) {
        List<String> l = new ArrayList<String>();
        l.add(beg + "sequence " + sequence + " " + this);
        return l;
    }

    public boolean matches(int afi, int asn, addrPrefix<T> net) {
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
    public boolean matches(int afi, int asn, tabRouteEntry<T> net) {
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
     * @param asn as number
     * @param net network
     */
    public void update(int afi, int asn, tabRouteEntry<T> net) {
    }

}
