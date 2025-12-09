package org.freertr.spf;

import org.freertr.addr.addrType;

/**
 * spf connection
 *
 * @param <Ta> type of nodes
 * @author matecsaba
 */
public class spfConn<Ta extends addrType> {

    /**
     * create instance
     */
    public spfConn() {
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
    protected spfNode<Ta> target;

}
