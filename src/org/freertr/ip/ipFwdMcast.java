package org.freertr.ip;

import org.freertr.addr.addrIP;
import org.freertr.cfg.cfgAll;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabLabelEntry;
import org.freertr.tab.tabRouteUtil;
import org.freertr.user.userFormat;
import org.freertr.util.bits;
import org.freertr.util.counter;

/**
 * stores one multicast group
 *
 * @author matecsaba
 */
public class ipFwdMcast implements Comparable<ipFwdMcast> {

    /**
     * group address
     */
    public final addrIP group;

    /**
     * source address, filled with zeroes for anything
     */
    public final addrIP source;

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

    /**
     * receive label
     */
    public tabLabelEntry rxLab;

    /**
     * counter
     */
    public counter cntr = new counter();

    /**
     * hardware counter
     */
    public counter hwCntr;

    public int compareTo(ipFwdMcast o) {
        if (rd < o.rd) {
            return -1;
        }
        if (rd > o.rd) {
            return +1;
        }
        int i = group.compareTo(o.group);
        if (i != 0) {
            return i;
        }
        return source.compareTo(o.source);
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
        created = bits.getTime();
    }

    /**
     * copy bytes
     *
     * @return copy of record
     */
    public ipFwdMcast copyBytes() {
        ipFwdMcast res = new ipFwdMcast(group, source);
        for (int i = 0; i < flood.size(); i++) {
            ipFwdIface ntry = flood.get(i);
            if (ntry == null) {
                continue;
            }
            res.flood.add(ntry);
        }
        res.rd = rd;
        res.iface = iface;
        res.local = local;
        res.configG = configG;
        res.configI = configI;
        res.created = created;
        if (label != null) {
            res.label = label.copyBytes();
        }
        if (bier != null) {
            res.bier = bier.copyBytes();
        }
        res.upsVrf = upsVrf;
        res.rxLab = rxLab;
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
        if (group.compareTo(o.group) != 0) {
            return true;
        }
        if (source.compareTo(o.source) != 0) {
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

    /**
     * get show line
     *
     * @return text
     */
    public String getShow() {
        String s = "ifc=" + flood.size();
        if (label != null) {
            s += " label=" + label.neighs.size();
        }
        if (bier != null) {
            s += " bierp=" + bier.peers.size() + " bierf=" + bier.fwds.size();
        }
        if (local) {
            s += " local";
        }
        String a = "";
        if (hwCntr != null) {
            a = "+" + hwCntr.byteTx;
        }
        return source + "|" + group + "|" + iface + "|" + upstream + "|" + s + "|" + cntr.byteTx + a;
    }

    /**
     * get details
     *
     * @param res result
     */
    public void getDump(userFormat res) {
        res.add("source|" + source);
        res.add("group|" + group);
        res.add("rd|" + tabRouteUtil.rd2string(rd));
        res.add("created|" + bits.time2str(cfgAll.timeZoneName, created + cfgAll.timeServerOffset, 3));
        res.add("lasted|" + bits.timePast(created));
        res.add("iface|" + iface);
        res.add("upstream|" + upstream);
        res.add("local|" + local);
        res.add("configG|" + configG);
        res.add("configI|" + configI);
        for (int i = 0; i < flood.size(); i++) {
            res.add("flood iface|" + flood.get(i));
        }
        res.add("label|" + label);
        if (bier != null) {
            bier.getDump(res);
        }
        res.add("counter|" + cntr.getShStat());
        res.add("lastio|" + cntr.getShTraff());
        res.add("hardware counter|" + counter.getShStat(hwCntr));
    }

}
