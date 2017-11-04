package ip;

import addr.addrIP;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import tab.tabLabel;
import tab.tabLabelNtry;
import util.bits;

/**
 * stores one traffeng tunnel
 *
 * @author matecsaba
 */
public class ipFwdTrfng implements Comparator<ipFwdTrfng> {

    /**
     * time of (re)creation
     */
    public long created;

    /**
     * timeout value
     */
    public int timeout;

    /**
     * local label
     */
    public tabLabelNtry locLab;

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

    public int compare(ipFwdTrfng o1, ipFwdTrfng o2) {
        if (o1.srcId > o2.srcId) {
            return +1;
        }
        if (o1.srcId < o2.srcId) {
            return -1;
        }
        if (o1.subId > o2.subId) {
            return +1;
        }
        if (o1.subId < o2.subId) {
            return -1;
        }
        int i = o1.srcAdr.compare(o1.srcAdr, o2.srcAdr);
        if (i != 0) {
            return i;
        }
        return o1.subAdr.compare(o1.subAdr, o2.subAdr);
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
     * dump this tunnel
     *
     * @return list of lines
     */
    public List<String> dump() {
        List<String> l = new ArrayList<String>();
        l.add("source address = " + srcAdr);
        l.add("source id = " + srcId);
        l.add("subgroup address = " + subAdr);
        l.add("subgroup id = " + subId);
        l.add("target address = " + trgAdr);
        l.add("target id = " + trgId);
        l.add("ingress interface= " + srcIfc);
        l.add("ingress hop = " + srcHop);
        l.add("ingress label = " + locLab);
        l.add("egress interface= " + trgIfc);
        l.add("egress hop = " + trgHop);
        l.add("egress label = " + trgLab);
        l.add("bandwidth = " + bits.bandwidth(((Float) (bwdt * 8)).longValue()));
        l.add("record route = " + recRou);
        l.add("description = " + descr);
        return l;
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
                locLab.clrDupMpls(4, trgHop);
            }
            if (locLab.duplicate.size() > 0) {
                return;
            }
        }
        locLab.setFwdDrop(4);
        tabLabel.release(locLab, 4);
        locLab = null;
    }

}
