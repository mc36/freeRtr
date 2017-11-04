package rtr;

import addr.addrIP;
import java.util.Comparator;
import util.bits;

/**
 * protocol independent multicast (rfc4601) neighbor
 *
 * @author matecsaba
 */
public class rtrPimNeigh implements Comparator<rtrPimNeigh> {

    /**
     * address of peer
     */
    protected addrIP peer;

    /**
     * last hello
     */
    protected long last;

    /**
     * hold time
     */
    protected int hold;

    /**
     * dr priority
     */
    protected int pri;

    /**
     * uptime
     */
    protected long upTime;

    public int compare(rtrPimNeigh o1, rtrPimNeigh o2) {
        return o1.peer.compare(o1.peer, o2.peer);
    }

    /**
     * get neighbors line
     *
     * @return string
     */
    public String getShNeigh() {
        return peer + "|" + pri + "|" + bits.timePast(upTime);
    }

}
