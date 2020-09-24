package util;

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
public class shrtPthFrstNode<Ta extends Comparator<? super Ta>> implements Comparator<shrtPthFrstNode<Ta>> {

    protected Ta name;

    protected boolean visited;

    protected int nxtMet;

    protected List<shrtPthFrstConn<Ta>> conn = new ArrayList<shrtPthFrstConn<Ta>>();

    protected shrtPthFrstUpl<Ta> uplink;

    protected List<shrtPthFrstUpl<Ta>> uplinks;

    protected List<shrtPthFrstUpl<Ta>> result;

    protected int metric;

    protected int srBeg;

    protected int srIdx;

    protected int brBeg;

    protected int brIdx;

    protected tabGen<shrtPthFrstIdx> brLst = new tabGen<shrtPthFrstIdx>();

    public shrtPthFrstNode(Ta nam) {
        name = nam;
    }

    public int compare(shrtPthFrstNode<Ta> o1, shrtPthFrstNode<Ta> o2) {
        return o1.name.compare(o1.name, o2.name);
    }

    public shrtPthFrstConn<Ta> findConn(shrtPthFrstNode<Ta> peer) {
        for (int i = 0; i < conn.size(); i++) {
            shrtPthFrstConn<Ta> ntry = conn.get(i);
            if (peer.compare(peer, ntry.target) == 0) {
                return ntry;
            }
        }
        return null;
    }

    public String toString() {
        return "" + name;
    }

}
