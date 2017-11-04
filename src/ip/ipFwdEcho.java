package ip;

import java.util.Comparator;

import util.notifier;
import addr.addrIP;

/**
 * stores one echo session
 *
 * @author matecsaba
 */
public class ipFwdEcho implements Comparator<ipFwdEcho> {

    /**
     * id number
     */
    public int echoNum;

    /**
     * notifier to use
     */
    public notifier notif;

    /**
     * source address
     */
    public addrIP src;

    /**
     * target address
     */
    public addrIP trg;

    /**
     * time when entry created
     */
    public long created;

    public int compare(ipFwdEcho o1, ipFwdEcho o2) {
        if (o1.echoNum < o2.echoNum) {
            return -1;
        }
        if (o1.echoNum > o2.echoNum) {
            return +1;
        }
        return 0;
    }

}
