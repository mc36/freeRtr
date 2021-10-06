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
     * create instance
     */
    public shrtPthFrstConn() {
    }

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
