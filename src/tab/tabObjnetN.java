package tab;

import addr.addrPrefix;
import addr.addrType;
import addr.addrIP;
import addr.addrIPv4;
import addr.addrIPv6;
import java.util.ArrayList;
import java.util.List;
import pack.packHolder;
import util.bits;

/**
 * represents one network object group entry
 *
 * @param <T> type of address
 * @author matecsaba
 */
public class tabObjnetN<T extends addrType> extends tabListingEntry<T> {

    /**
     * source address
     */
    public T addr;

    /**
     * source mask
     */
    public T mask;

    /**
     * create new object group entry
     *
     * @param adr empty address to use
     */
    @SuppressWarnings("unchecked")
    public tabObjnetN(T adr) {
        addr = (T) adr.copyBytes();
        mask = (T) adr.copyBytes();
    }

    /**
     * convert string to address
     *
     * @param ntry object to update
     * @param s string to convert
     * @return true if error happened
     */
    public static boolean fromString(tabObjnetN<addrIP> ntry, String s) {
        ntry.action = tabPlcmapN.actionType.actPermit;
        s = s.trim() + " ";
        int i = s.indexOf(" ");
        String a = s.substring(0, i).trim();
        s = s.substring(i, s.length()).trim();
        if (ntry.addr.fromString(a)) {
            return true;
        }
        if (s.startsWith("/")) {
            if (ntry.addr.isIPv4()) {
                addrPrefix<addrIPv4> prf = new addrPrefix<addrIPv4>(ntry.addr.toIPv4(), bits.str2num(s.substring(1, s.length())));
                ntry.mask.fromIPv4addr(prf.mask);
            } else {
                addrPrefix<addrIPv6> prf = new addrPrefix<addrIPv6>(ntry.addr.toIPv6(), bits.str2num(s.substring(1, s.length())));
                ntry.mask.fromIPv6addr(prf.mask);
            }
        } else if (ntry.mask.fromString(s)) {
            return true;
        }
        ntry.addr.setAnd(ntry.addr, ntry.mask);
        return false;
    }

    /**
     * convert to string
     *
     * @return string
     */
    public String toString() {
        return addr + " " + mask;
    }

    /**
     * convert to string
     *
     * @param beg beginning
     * @return string
     */
    public List<String> usrString(String beg) {
        List<String> l = new ArrayList<String>();
        l.add(beg + "sequence " + sequence + " " + this);
        return l;
    }

    /**
     * test if matches
     *
     * @param afi address family
     * @param net network
     * @return false on success, true on error
     */
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

    /**
     * test if matches
     *
     * @param pck packet
     * @return false on success, true on error
     */
    public boolean matches(packHolder pck) {
        if (!pck.IPsrc.isMatches(addr, mask)) {
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
