package org.freertr.spf;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrType;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabRoute;

/**
 * spf node
 *
 * @param <Ta> type of nodes
 * @author matecsaba
 */
public class spfNode<Ta extends addrType> implements Comparable<spfNode<Ta>> {

    /**
     * node id
     */
    protected Ta name;

    /**
     * stub node
     */
    protected boolean stub;

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
    protected List<spfConn<Ta>> conn = new ArrayList<spfConn<Ta>>();

    /**
     * algorithms
     */
    protected List<Integer> algo = new ArrayList<Integer>();

    /**
     * best uplink
     */
    protected spfResult<Ta> uplink;

    /**
     * uplinks
     */
    protected List<spfResult<Ta>> uplinks;

    /**
     * result
     */
    protected List<spfResult<Ta>> result;

    /**
     * fixed metric prefixes
     */
    protected tabRoute<addrIP> prfFix = new tabRoute<addrIP>("prf");

    /**
     * cumulative metric prefixes
     */
    protected tabRoute<addrIP> prfAdd = new tabRoute<addrIP>("prf");

    /**
     * fixed metric other prefixes
     */
    protected tabRoute<addrIP> othFix = new tabRoute<addrIP>("prf");

    /**
     * cumulative metric other prefixes
     */
    protected tabRoute<addrIP> othAdd = new tabRoute<addrIP>("prf");

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
     * bier subdomain
     */
    protected int brSub;

    /**
     * bier nodes behind
     */
    protected tabGen<spfIndex> brLst = new tabGen<spfIndex>();

    /**
     * create new instance
     *
     * @param nam node id
     */
    public spfNode(Ta nam) {
        name = nam;
    }

    public int compareTo(spfNode<Ta> o) {
        return name.compareTo(o.name);
    }

    /**
     * find connection
     *
     * @param peer node id
     * @param met required metric
     * @return connection, null if not found
     */
    protected spfConn<Ta> findConn(spfNode<Ta> peer, int met) {
        spfConn<Ta> best = null;
        int diff = Integer.MAX_VALUE;
        for (int i = 0; i < conn.size(); i++) {
            spfConn<Ta> ntry = conn.get(i);
            if (peer.compareTo(ntry.target) != 0) {
                continue;
            }
            if (met < 0) {
                return ntry;
            }
            if (met == ntry.metric) {
                return ntry;
            }
            int o = ntry.metric - met;
            if (o < 0) {
                o = -o;
            }
            if (o > diff) {
                continue;
            }
            best = ntry;
            diff = o;
        }
        return best;
    }

    public String toString() {
        if (ident != null) {
            return "" + ident;
        }
        return "" + name;
    }

}
