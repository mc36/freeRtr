package util;

import java.util.Comparator;

/**
 * spf connection
 *
 * @param <Ta> type of nodes
 * @author matecsaba
 */
public class shrtPthFrstConn<Ta extends Comparator<? super Ta>> {

    protected int metric;

    protected boolean stub;

    protected boolean realHop;

    protected Object ident;

    protected shrtPthFrstNode<Ta> target;

}
