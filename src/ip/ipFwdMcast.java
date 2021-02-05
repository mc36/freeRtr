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
        if (label != null) {
            res.label = label.copyBytes();
        }
        if (bier != null) {
            res.bier = bier.copyBytes();
        }
        res.upsVrf = upsVrf;
        if (upstream != null) {
            res.upstream = upstream.copyBytes();
        }
        return res;
    }

    /**
     * compare this entry
     *
     * @param o other
     * @return false if equals, true if differs
     */
    public boolean differs(ipFwdMcast o) {
        if (o == null) {
            return true;
        }
        if (local != o.local) {
            return true;
        }
        if (rd != o.rd) {
            return true;
        }
        if (iface != o.iface) {
            return true;
        }
        if (group.compare(group, o.group) != 0) {
            return true;
        }
        if (source.compare(source, o.source) != 0) {
            return true;
        }
        if (label == null) {
            if (o.label != null) {
                return true;
            }
        } else {
            if (label.differs(o.label)) {
                return true;
            }
        }
        if (bier == null) {
            if (o.bier != null) {
                return true;
            }
        } else {
            if (bier.differs(o.bier)) {
                return true;
            }
        }
        if (flood.size() != o.flood.size()) {
            return true;
        }
        for (int i = 0; i < flood.size(); i++) {
            if (flood.get(i) != o.flood.get(i)) {
                return true;
            }
        }
        return false;
    }

}
