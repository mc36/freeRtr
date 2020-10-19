package util;

import addr.addrType;

/**
 * spf connection
 *
 * @param <Ta> type of nodes
 * @author matecsaba
 */
public class shrtPthFrstConn<Ta extends addrType> {

    /**
     * metric
     */
    protected int metric;

    /**
     * stub connection
     */
    protected boolean stub;

    /**
     * real hop
     */
    protected boolean realHop;

    /**
     * identifier data
     */
    protected String ident;

    /**
     * target
     */
    protected shrtPthFrstNode<Ta> target;

}
