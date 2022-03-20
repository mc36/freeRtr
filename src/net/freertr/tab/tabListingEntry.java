package net.freertr.tab;

import java.util.Comparator;
import java.util.List;
import net.freertr.addr.addrPrefix;
import net.freertr.addr.addrType;
import net.freertr.pack.packHolder;
import net.freertr.util.bits;
import net.freertr.util.counter;

/**
 * represents one sequenced listing entry
 *
 * @param <T> class of address
 * @author matecsaba
 */
public abstract class tabListingEntry<T extends addrType> implements Comparator<tabListingEntry<T>> {

    /**
     * create instance
     */
    public tabListingEntry() {
    }

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
         * police to packet rate
         */
        actPps,
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
     * negate action
     *
     * @param act action
     * @return negated
     */
    public static actionType negateAction(actionType act) {
        switch (act) {
            case actDeny:
                return actionType.actPermit;
            case actPermit:
                return actionType.actDeny;
            default:
                return act;
        }
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
    public counter cntr = new counter();

    /**
     * hardware packet counter
     */
    public counter hwCntr;

    /**
     * hardware substract counter
     */
    public counter hwSub;

    /**
     * last matched
     */
    public long lastMatch;

    /**
     * timeout on this entry
     */
    public int timeout;

    /**
     * session limit on this entry
     */
    public int maxSess;

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
     * @param wht what
     * @return counters
     */
    public String getCounters(int wht) {
        return cntr.getShHwStat(hwCntr, hwSub, wht);
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
     * @param asn as number
     * @param net network to test
     * @return true if matches, false if not matches
     */
    public abstract boolean matches(int afi, int asn, addrPrefix<T> net);

    /**
     * test if this entry contains a network
     *
     * @param afi address family
     * @param asn as number
     * @param net network to test
     * @return true if matches, false if not matches
     */
    public abstract boolean matches(int afi, int asn, tabRouteEntry<T> net);

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
     * @param asn as number
     * @param net network number
     */
    public abstract void update(int afi, int asn, tabRouteEntry<T> net);

}
