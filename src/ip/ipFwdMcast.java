package ip;

import addr.addrIP;
import java.util.Comparator;
import tab.tabGen;

/**
 * stores one multicast group
 *
 * @author matecsaba
 */
public class ipFwdMcast implements Comparator<ipFwdMcast> {

    /**
     * group address
     */
    public addrIP group;

    /**
     * source address, filled with zeroes for anything
     */
    public addrIP source;

    /**
     * route distinguisher
     */
    public long rd;

    /**
     * source interface
     */
    public ipFwdIface iface;

    /**
     * next hop address
     */
    public addrIP upstream;

    /**
     * list of target interfaces
     */
    public tabGen<ipFwdIface> flood = new tabGen<ipFwdIface>();

    /**
     * local processing required
     */
    public boolean local;

    /**
     * configured global group
     */
    public boolean configG;

    /**
     * configured interface group
     */
    public boolean configI;

    /**
     * time when created
     */
    public long created;

    /**
     * local label
     */
    public ipFwdMpmp label;

    /**
     * bier
     */
    public ipFwdBier bier;

    /**
     * mdt vrf
     */
    public ipFwd upsVrf;

    public int compare(ipFwdMcast o1, ipFwdMcast o2) {
        int i = o1.group.compare(o1.group, o2.group);
        if (i != 0) {
            return i;
        }
        return o1.source.compare(o1.source, o2.source);
    }

    public String toString() {
        return source + "," + group;
    }

    /**
     * create new instance
     *
     * @param grp multicast group
     * @param src source address
     */
    public ipFwdMcast(addrIP grp, addrIP src) {
        group = grp.copyBytes();
        source = src.copyBytes();
    }

    /**
     * copy bytes
     *
     * @return copy of record
     */
    public ipFwdMcast copyBytes() {
        ipFwdMcast res = new ipFwdMcast(group, source);
        for (int i = 0; i < flood.size(); i++) {
            res.flood.add(flood.get(i));
        }
        res.rd = rd;
        res.iface = iface;
        res.local = local;
        res.created = created;
        res.label = label;
        res.bier = bier;
        res.upsVrf = upsVrf;
        if (upstream != null) {
            res.upstream = upstream.copyBytes();
        }
        return res;
    }

}
