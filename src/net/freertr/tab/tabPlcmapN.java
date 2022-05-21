package net.freertr.tab;

import java.util.ArrayList;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrPrefix;
import net.freertr.pack.packHolder;
import net.freertr.util.cmds;

/**
 * represents one policy map entry
 *
 * @author matecsaba
 */
public class tabPlcmapN extends tabListingEntry<addrIP> {

    /**
     * create instance
     */
    public tabPlcmapN() {
    }

    /**
     * length matcher
     */
    public tabIntMatcher lengthMatch = new tabIntMatcher();

    /**
     * ttl matcher
     */
    public tabIntMatcher ttlMatch = new tabIntMatcher();

    /**
     * ttl updater
     */
    public tabIntUpdater ttlSet = new tabIntUpdater();

    /**
     * fragment
     */
    public boolean fragMatch;

    /**
     * flag
     */
    public tabIntMatcher flagMatch = new tabIntMatcher();

    /**
     * ethertype matcher
     */
    public tabIntMatcher ethtypMatch = new tabIntMatcher();

    /**
     * acl matcher
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> aclMatch;

    /**
     * precedence matcher
     */
    public tabIntMatcher precedenceMatch = new tabIntMatcher();

    /**
     * precedence updater
     */
    public tabIntUpdater precedenceSet = new tabIntUpdater();

    /**
     * dscp matcher
     */
    public tabIntMatcher dscpMatch = new tabIntMatcher();

    /**
     * dscp updater
     */
    public tabIntUpdater dscpSet = new tabIntUpdater();

    /**
     * tos matcher
     */
    public tabIntMatcher tosMatch = new tabIntMatcher();

    /**
     * tos updater
     */
    public tabIntUpdater tosSet = new tabIntUpdater();

    /**
     * sgt matcher
     */
    public tabIntMatcher sgtMatch = new tabIntMatcher();

    /**
     * sgt updater
     */
    public tabIntUpdater sgtSet = new tabIntUpdater();

    /**
     * cos matcher
     */
    public tabIntMatcher cosMatch = new tabIntMatcher();

    /**
     * cos updater
     */
    public tabIntUpdater cosSet = new tabIntUpdater();

    /**
     * exp matcher
     */
    public tabIntMatcher expMatch = new tabIntMatcher();

    /**
     * exp updater
     */
    public tabIntUpdater expSet = new tabIntUpdater();

    /**
     * qos matcher
     */
    public tabIntMatcher qosMatch = new tabIntMatcher();

    /**
     * qos updater
     */
    public tabIntUpdater qosSet = new tabIntUpdater();

    /**
     * flow
     */
    public tabIntMatcher flowMatch = new tabIntMatcher();

    /**
     * qos updater
     */
    public tabIntUpdater flowSet = new tabIntUpdater();

    /**
     * byte rate of action
     */
    public long accessRate;

    /**
     * exceed bytes of action
     */
    public long exceedRate;

    /**
     * queue limit
     */
    public int queues;

    /**
     * time interval
     */
    public int interval;

    /**
     * random detect on queue full
     */
    public boolean randomDetect;

    /**
     * child policy
     */
    public tabListing<tabPlcmapN, addrIP> child;

    /**
     * convert string to type
     *
     * @param s string
     * @return type
     */
    public static actionType string2type(String s) {
        if (s.equals("drop")) {
            return actionType.actDeny;
        }
        if (s.equals("transit")) {
            return actionType.actPermit;
        }
        if (s.equals("police")) {
            return actionType.actPolice;
        }
        if (s.equals("pps")) {
            return actionType.actPps;
        }
        if (s.equals("shape")) {
            return actionType.actShaper;
        }
        if (s.equals("bandwidth")) {
            return actionType.actBndwdth;
        }
        if (s.equals("priority")) {
            return actionType.actPriorty;
        }
        return null;
    }

    /**
     * convert type to string
     *
     * @param i type
     * @return string
     */
    public static String type2string(actionType i) {
        switch (i) {
            case actDeny:
                return "drop";
            case actPermit:
                return "transit";
            case actPolice:
                return "police";
            case actPps:
                return "pps";
            case actShaper:
                return "shape";
            case actBndwdth:
                return "bandwidth";
            case actPriorty:
                return "priority";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * convert to string
     *
     * @param beg beginning
     * @param filter filter mode
     * @return string
     */
    public List<String> usrString(String beg, int filter) {
        beg += "sequence " + sequence + " ";
        List<String> l = new ArrayList<String>();
        l.add(beg + "description " + description);
        l.add(beg + "action " + type2string(action));
        if (aclMatch == null) {
            l.add(beg + "no match access-group");
        } else {
            l.add(beg + "match access-group " + aclMatch.listName);
        }
        cmds.cfgLine(l, !fragMatch, beg, "match frag", "");
        l.add(beg + "match flag " + flagMatch);
        l.add(beg + "match flow " + flowMatch);
        l.add(beg + "match length " + lengthMatch);
        l.add(beg + "match ttl " + ttlMatch);
        l.add(beg + "match ethtyp " + ethtypMatch);
        l.add(beg + "match sgt " + sgtMatch);
        l.add(beg + "match cos " + cosMatch);
        l.add(beg + "match exp " + expMatch);
        l.add(beg + "match tos " + tosMatch);
        l.add(beg + "match dscp " + dscpMatch);
        l.add(beg + "match precedence " + precedenceMatch);
        l.add(beg + "match qosgroup " + qosMatch);
        l.add(beg + "set sgt " + sgtSet);
        l.add(beg + "set cos " + cosSet);
        l.add(beg + "set exp " + expSet);
        l.add(beg + "set tos " + tosSet);
        l.add(beg + "set dscp " + dscpSet);
        l.add(beg + "set precedence " + precedenceSet);
        l.add(beg + "set ttl " + ttlSet);
        l.add(beg + "set flow " + flowSet);
        l.add(beg + "set qosgroup " + qosSet);
        l.add(beg + "access-rate " + accessRate * 8);
        l.add(beg + "exceed-rate " + exceedRate * 8);
        l.add(beg + "time-interval " + interval);
        l.add(beg + "queue-limit " + queues);
        cmds.cfgLine(l, !randomDetect, beg, "random-detect", "");
        cmds.cfgLine(l, !logMatch, beg, "log", "");
        if (child == null) {
            l.add(beg + "no service-policy");
        } else {
            l.add(beg + "service-policy " + child.listName);
        }
        return l;
    }

    public boolean matches(int afi, int asn, addrPrefix<addrIP> net) {
        return false;
    }

    public boolean matches(int afi, int asn, tabRouteEntry<addrIP> net) {
        return false;
    }

    public boolean matches(packHolder pck) {
        if (!lengthMatch.matches(pck.dataSize())) {
            return false;
        }
        if (!ttlMatch.matches(pck.IPttl)) {
            return false;
        }
        if (!qosMatch.matches(pck.INTqosGrp)) {
            return false;
        }
        if (!sgtMatch.matches(pck.SGTid)) {
            return false;
        }
        if (!cosMatch.matches(pck.ETHcos)) {
            return false;
        }
        if (!expMatch.matches(pck.MPLSexp)) {
            return false;
        }
        if (!ethtypMatch.matches(pck.ETHtype)) {
            return false;
        }
        if (fragMatch) {
            if (!pck.IPmf && (pck.IPfrg < 1)) {
                return false;
            }
        }
        if (!flagMatch.matches(pck.TCPflg)) {
            return false;
        }
        if (!flowMatch.matches(pck.IPid)) {
            return false;
        }
        if (!tosMatch.matches(pck.IPtos)) {
            return false;
        }
        if (!dscpMatch.matches(pck.IPtos >>> 2)) {
            return false;
        }
        if (!precedenceMatch.matches(pck.IPtos >>> 5)) {
            return false;
        }
        if (aclMatch != null) {
            if (!aclMatch.matches(false, false, pck)) {
                return false;
            }
        }
        return true;
    }

    public void update(int afi, int asn, tabRouteEntry<addrIP> net) {
    }

    /**
     * update tos
     *
     * @param i old tos
     * @return new tos
     */
    public int updateTos(int i) {
        i = tosSet.update(i);
        i = (i & 0x03) | (dscpSet.update(i >>> 2) << 2);
        i = (i & 0x1f) | (precedenceSet.update(i >>> 5) << 5);
        return i;
    }

}
