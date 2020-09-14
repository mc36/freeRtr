package tab;

import addr.addrPrefix;
import addr.addrType;
import java.util.Comparator;
import java.util.List;
import pack.packHolder;
import util.bits;

/**
 * represents one sequenced listing entry
 *
 * @param <T> class of address
 * @author matecsaba
 */
public abstract class tabListingEntry<T extends addrType> implements Comparator<tabListingEntry<T>> {

    /**
     * action type
     */
    public enum actionType {

        /**
         * forward every packet
         */
        actPermit,
        /**
         * drop every packet
         */
        actDeny,
        /**
         * police to rate
         */
        actPolice,
        /**
         * shape to rate
         */
        actShaper,
        /**
         * bandwidth to rate
         */
        actBndwdth,
        /**
         * priority to rate
         */
        actPriorty

    }

    /**
     * sequence number of this entry
     */
    public int sequence;

    /**
     * description of this class
     */
    public String description = "";

    /**
     * type of action to take true to allow on match, false to deny it
     */
    public actionType action;

    /**
     * packet counter
     */
    public long countPack;

    /**
     * byte counter
     */
    public long countByte;

    /**
     * last matched
     */
    public long lastMatch;

    /**
     * timeout on this entry
     */
    public int timeout;

    /**
     * log matches
     */
    public boolean logMatch;

    /**
     * convert permit/deny to string
     *
     * @param i action
     * @return string
     */
    public static String action2string(actionType i) {
        switch (i) {
            case actPermit:
                return "permit";
            case actDeny:
                return "deny";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * convert string to action
     *
     * @param s string to convert
     * @return action
     */
    public static actionType string2action(String s) {
        if (s.equals("permit")) {
            return actionType.actPermit;
        }
        if (s.equals("deny")) {
            return actionType.actDeny;
        }
        return null;
    }

    public int compare(tabListingEntry<T> o1, tabListingEntry<T> o2) {
        if (o1.sequence < o2.sequence) {
            return -1;
        }
        if (o1.sequence > o2.sequence) {
            return +1;
        }
        return 0;
    }

    /**
     * get counters
     *
     * @return counters
     */
    public String getCounters() {
        return countPack + " packets (" + countByte + " bytes)";
    }

    /**
     * get times
     *
     * @return times
     */
    public String getTimes() {
        return bits.timePast(lastMatch) + " ago, " + bits.timeDump(timeout / 1000) + " timeout";
    }

    /**
     * text representation of this entry
     *
     * @param beg beginning text
     * @return text representation of this entry
     */
    public abstract List<String> usrString(String beg);

    /**
     * test if this entry contains a network
     *
     * @param afi address family
     * @param net network to test
     * @return true if matches, false if not matches
     */
    public abstract boolean matches(int afi, addrPrefix<T> net);

    /**
     * test if this entry contains a network
     *
     * @param afi address family
     * @param net network to test
     * @return true if matches, false if not matches
     */
    public abstract boolean matches(int afi, tabRouteEntry<T> net);

    /**
     * test if this entry matches a packet
     *
     * @param pck packet to test
     * @return true if matches, false if not matches
     */
    public abstract boolean matches(packHolder pck);

    /**
     * update one entry
     *
     * @param afi address family
     * @param net network number
     */
    public abstract void update(int afi, tabRouteEntry<T> net);

}
