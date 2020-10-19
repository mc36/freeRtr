package util;

import addr.addrType;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import tab.tabGen;

/**
 * spf node
 *
 * @param <Ta> type of nodes
 * @author matecsaba
 */
public class shrtPthFrstNode<Ta extends addrType> implements Comparator<shrtPthFrstNode<Ta>> {

    /**
     * node id
     */
    protected Ta name;

    /**
     * identifier data
     */
    protected String ident;

    /**
     * reachable
     */
    protected boolean visited;

    /**
     * next hop metric
     */
    protected int nxtMet;

    /**
     * connections
     */
    protected List<shrtPthFrstConn<Ta>> conn = new ArrayList<shrtPthFrstConn<Ta>>();

    /**
     * best uplink
     */
    protected shrtPthFrstRes<Ta> uplink;

    /**
     * uplinks
     */
    protected List<shrtPthFrstRes<Ta>> uplinks;

    /**
     * result
     */
    protected List<shrtPthFrstRes<Ta>> result;

    /**
     * metric
     */
    protected int metric;

    /**
     * segrou base
     */
    protected int srBeg;

    /**
     * segrou index
     */
    protected int srIdx;

    /**
     * bier base
     */
    protected int brBeg;

    /**
     * bier index
     */
    protected int brIdx;

    /**
     * bier nodes behind
     */
    protected tabGen<shrtPthFrstIdx> brLst = new tabGen<shrtPthFrstIdx>();

    /**
     * create new instance
     *
     * @param nam node id
     */
    public shrtPthFrstNode(Ta nam) {
        name = nam;
    }

    public int compare(shrtPthFrstNode<Ta> o1, shrtPthFrstNode<Ta> o2) {
        return o1.name.compare(o1.name, o2.name);
    }

    /**
     * find connection
     *
     * @param peer node id
     * @return connection, null if not found
     */
    protected shrtPthFrstConn<Ta> findConn(shrtPthFrstNode<Ta> peer) {
        for (int i = 0; i < conn.size(); i++) {
            shrtPthFrstConn<Ta> ntry = conn.get(i);
            if (peer.compare(peer, ntry.target) == 0) {
                return ntry;
            }
        }
        return null;
    }

    public String toString() {
        if (ident != null) {
            return "" + ident;
        }
        return "" + name;
    }

}
