package org.freertr.ip;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.cfg.cfgAll;
import org.freertr.tab.tabHop;
import org.freertr.tab.tabLabel;
import org.freertr.tab.tabLabelEntry;
import org.freertr.user.userFormat;
import org.freertr.util.bits;

/**
 * stores one traffeng tunnel
 *
 * @author matecsaba
 */
public class ipFwdTrfng implements Comparable<ipFwdTrfng> {

    /**
     * time when created
     */
    public long created;

    /**
     * time of update
     */
    public long updated;

    /**
     * timeout value
     */
    public int timeout;

    /**
     * local label
     */
    public tabLabelEntry locLab;

    /**
     * source is local
     */
    public int srcLoc;

    /**
     * source address
     */
    public addrIP srcAdr;

    /**
     * source id
     */
    public int srcId;

    /**
     * source way next hop
     */
    public addrIP srcHop;

    /**
     * source interface
     */
    public ipFwdIface srcIfc;

    /**
     * target is local
     */
    public boolean trgLoc;

    /**
     * target address
     */
    public addrIP trgAdr;

    /**
     * target id
     */
    public long trgId;

    /**
     * middle targets
     */
    public List<tabHop> midAdrs = new ArrayList<tabHop>();

    /**
     * target way next hop
     */
    public addrIP trgHop;

    /**
     * target interface
     */
    public ipFwdIface trgIfc;

    /**
     * target label
     */
    public int trgLab;

    /**
     * bandwidth
     */
    public float bwdt;

    /**
     * setup priority
     */
    public int priS = 7;

    /**
     * holding priority
     */
    public int priH = 7;

    /**
     * exclude affinity
     */
    public int affE = 0;

    /**
     * include affinity
     */
    public int affI = 0;

    /**
     * must affinity
     */
    public int affM = 0;

    /**
     * record route
     */
    public boolean recRou;

    /**
     * description
     */
    public String descr;

    /**
     * subgroup address
     */
    public addrIP subAdr;

    /**
     * subgroup id
     */
    public int subId;

    /**
     * association type
     */
    public int asocTyp;

    /**
     * association id
     */
    public int asocId;

    /**
     * association global id
     */
    public int asocGlb;

    /**
     * association address
     */
    public addrIP asocAdr;

    public int compareTo(ipFwdTrfng o) {
        if (srcId > o.srcId) {
            return +1;
        }
        if (srcId < o.srcId) {
            return -1;
        }
        if (subId > o.subId) {
            return +1;
        }
        if (subId < o.subId) {
            return -1;
        }
        int i = srcAdr.compareTo(o.srcAdr);
        if (i != 0) {
            return i;
        }
        return subAdr.compareTo(o.subAdr);
    }

    /**
     * create new instance
     *
     * @param adr1 address
     * @param id1 id
     * @param adr2 address
     * @param id2 id
     */
    public ipFwdTrfng(addrIP adr1, int id1, addrIP adr2, int id2) {
        srcAdr = adr1.copyBytes();
        srcId = id1;
        subAdr = adr2.copyBytes();
        subId = id2;
        timeout = 1000;
        created = bits.getTime();
    }

    /**
     * get an instance that could be used to find parent
     *
     * @return parent instance
     */
    public ipFwdTrfng getParent() {
        ipFwdTrfng res = new ipFwdTrfng(srcAdr, srcId, new addrIP(), 0);
        res.descr = descr;
        res.recRou = recRou;
        return res;
    }

    /**
     * get details
     *
     * @param res result
     */
    public void getDump(userFormat res) {
        res.add("source address|" + srcAdr);
        res.add("source id|" + srcId);
        res.add("subgroup address|" + subAdr);
        res.add("subgroup id|" + subId);
        res.add("target address|" + trgAdr);
        res.add("target id|" + trgId);
        String a = "";
        for (int i = 0; i < midAdrs.size(); i++) {
            a += " " + midAdrs.get(i);
        }
        res.add("midpoints|" + a);
        res.add("ingress interface|" + srcIfc);
        res.add("ingress hop|" + srcHop);
        res.add("ingress label|" + locLab);
        res.add("egress interface|" + trgIfc);
        res.add("egress hop|" + trgHop);
        res.add("egress label|" + trgLab);
        res.add("bandwidth|" + bits.bandwidth(((Float) (bwdt * 8)).longValue()));
        res.add("priority|" + priS + " " + priH);
        res.add("affinity|" + affE + " " + affI + " " + affM);
        res.add("record route|" + recRou);
        res.add("description|" + descr);
        res.add("association|" + asocTyp + " " + asocAdr + " " + asocId + " " + asocGlb);
        res.add("timeout|" + bits.timeDump(timeout / 1000));
        res.add("updated|" + bits.timePast(updated));
        res.add("created|" + bits.time2str(cfgAll.timeZoneName, created + cfgAll.timeServerOffset, 3));
        res.add("lasted|" + bits.timePast(created));
        res.add("local|" + srcLoc + " " + trgLoc);
    }

    public String toString() {
        return srcAdr + "|" + srcId + "|" + subAdr + "|" + subId + "|" + trgAdr + "|" + trgId + "|" + descr;
    }

    /**
     * release label
     */
    public void labStop() {
        if (locLab == null) {
            return;
        }
        if (locLab.duplicate != null) {
            if (trgHop != null) {
                locLab.delDupMpls(tabLabelEntry.owner.rsvp, trgHop);
            }
            if (locLab.duplicate.size() > 0) {
                return;
            }
        }
        if (isP2MP()) {
            locLab.clrDupMpls(tabLabelEntry.owner.rsvp);
        }
        locLab.setFwdDrop(tabLabelEntry.owner.rsvp);
        tabLabel.release(locLab, tabLabelEntry.owner.rsvp);
        locLab = null;
    }

    /**
     * check if point to multipoint
     *
     * @return true if p2mp lsp
     */
    public boolean isP2MP() {
        return (subId != 0) || (!subAdr.isEmpty());
    }

}
