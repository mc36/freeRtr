package net.freertr.util;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrPrefix;
import net.freertr.addr.addrType;

/**
 * spf prefix
 *
 * @param <Ta> type of nodes
 * @author matecsaba
 */
public class shrtPthFrstPfx<Ta extends addrType> implements Comparator<shrtPthFrstPfx<Ta>> {

    /**
     * prefix
     */
    protected final addrPrefix<addrIP> prefix;

    /**
     * nodes
     */
    protected final List<shrtPthFrstNode<Ta>> nodes = new ArrayList<shrtPthFrstNode<Ta>>();

    /**
     * create instance
     *
     * @param pfx prefix
     */
    public shrtPthFrstPfx(addrPrefix<addrIP> pfx) {
        prefix = pfx;
    }

    public String toString() {
        String a = "";
        for (int i = 0; i < nodes.size(); i++) {
            a += " " + nodes.get(i);
        }
        return addrPrefix.ip2str(prefix) + "|" + a;
    }

    public int compare(shrtPthFrstPfx<Ta> o1, shrtPthFrstPfx<Ta> o2) {
        return o1.prefix.compare(o1.prefix, o2.prefix);
    }

}
