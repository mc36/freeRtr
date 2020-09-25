package util;

import java.util.Comparator;

/**
 * spf connection
 *
 * @param <Ta> type of nodes
 * @author matecsaba
 */
public class shrtPthFrstConn<Ta extends Comparator<? super Ta>> {

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
    protected Object ident;

    /**
     * target
     */
    protected shrtPthFrstNode<Ta> target;

}
