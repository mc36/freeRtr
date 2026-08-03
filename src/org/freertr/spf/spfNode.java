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
     * other connections
     */
    protected List<spfConn<Ta>> othCon = new ArrayList<spfConn<Ta>>();

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

    public String toString() {
        if (ident != null) {
            return "" + ident;
        }
        return "" + name;
    }

}
