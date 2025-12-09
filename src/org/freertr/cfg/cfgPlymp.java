package org.freertr.cfg;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabListing;
import org.freertr.tab.tabListingEntry;
import org.freertr.tab.tabPlcmapN;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;

/**
 * policy map configuration
 *
 * @author matecsaba
 */
public class cfgPlymp implements Comparable<cfgPlymp>, cfgGeneric {

    /**
     * name of policymap
     */
    public String name;

    /**
     * current sequence number
     */
    public int seq;

    /**
     * list of policy
     */
    public tabListing<tabPlcmapN, addrIP> plcmap;

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("policy-map .*", cmds.tabulator + "sequence .* description ", null),
        new userFilter("policy-map .*", cmds.tabulator + "sequence .* no match access-group", null),
        new userFilter("policy-map .*", cmds.tabulator + "sequence .* no match frag", null),
        new userFilter("policy-map .*", cmds.tabulator + "sequence .* match flag all", null),
        new userFilter("policy-map .*", cmds.tabulator + "sequence .* match length all", null),
        new userFilter("policy-map .*", cmds.tabulator + "sequence .* match ttl all", null),
        new userFilter("policy-map .*", cmds.tabulator + "sequence .* match ethtyp all", null),
        new userFilter("policy-map .*", cmds.tabulator + "sequence .* match sgt all", null),
        new userFilter("policy-map .*", cmds.tabulator + "sequence .* match cos all", null),
        new userFilter("policy-map .*", cmds.tabulator + "sequence .* match exp all", null),
        new userFilter("policy-map .*", cmds.tabulator + "sequence .* match tos all", null),
        new userFilter("policy-map .*", cmds.tabulator + "sequence .* match dscp all", null),
        new userFilter("policy-map .*", cmds.tabulator + "sequence .* match precedence all", null),
        new userFilter("policy-map .*", cmds.tabulator + "sequence .* match qosgroup all", null),
        new userFilter("policy-map .*", cmds.tabulator + "sequence .* match flow all", null),
        new userFilter("policy-map .*", cmds.tabulator + "sequence .* no set vrf", null),
        new userFilter("policy-map .*", cmds.tabulator + "sequence .* set sgt leave", null),
        new userFilter("policy-map .*", cmds.tabulator + "sequence .* set cos leave", null),
        new userFilter("policy-map .*", cmds.tabulator + "sequence .* set exp leave", null),
        new userFilter("policy-map .*", cmds.tabulator + "sequence .* set tos leave", null),
        new userFilter("policy-map .*", cmds.tabulator + "sequence .* set ttl leave", null),
        new userFilter("policy-map .*", cmds.tabulator + "sequence .* set dscp leave", null),
        new userFilter("policy-map .*", cmds.tabulator + "sequence .* set precedence leave", null),
        new userFilter("policy-map .*", cmds.tabulator + "sequence .* set qosgroup leave", null),
        new userFilter("policy-map .*", cmds.tabulator + "sequence .* set flow leave", null),
        new userFilter("policy-map .*", cmds.tabulator + "sequence .* access-rate 0", null),
        new userFilter("policy-map .*", cmds.tabulator + "sequence .* exceed-rate 0", null),
        new userFilter("policy-map .*", cmds.tabulator + "sequence .* time-interval 0", null),
        new userFilter("policy-map .*", cmds.tabulator + "sequence .* queue-limit 0", null),
        new userFilter("policy-map .*", cmds.tabulator + "sequence .* no random-detect", null),
        new userFilter("policy-map .*", cmds.tabulator + "sequence .* mark-ecn 0", null),
        new userFilter("policy-map .*", cmds.tabulator + "sequence .* no service-policy", null),
        new userFilter("policy-map .*", cmds.tabulator + "sequence .* no log", null)
    };

    /**
     * create new policy map
     *
     * @param s name
     */
    public cfgPlymp(String s) {
        plcmap = new tabListing<tabPlcmapN, addrIP>();
        seq = plcmap.nextseq();
        name = s;
        plcmap.listName = s;
    }

    /**
     * get current entry
     *
     * @return current entry
     */
    public synchronized tabPlcmapN getCurr() {
        tabPlcmapN ntry = new tabPlcmapN();
        ntry.sequence = seq;
        ntry = plcmap.find(ntry);
        if (ntry != null) {
            return ntry;
        }
        ntry = new tabPlcmapN();
        ntry.sequence = seq;
        ntry.action = tabListingEntry.actionType.actPermit;
        plcmap.add(ntry);
        return ntry;
    }

    public int compareTo(cfgPlymp o) {
        return name.toLowerCase().compareTo(o.name.toLowerCase());
    }

    public String getPrompt() {
        return "plcmp";
    }

    public String toString() {
        return name;
    }

    public void getHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "sequence", "sequence number of an entry");
        l.add(null, false, 2, new int[]{1, -1}, "<num>", "sequence number");
        l.add(null, false, 1, new int[]{2, -1}, "reindex", "reindex route map");
        l.add(null, false, 2, new int[]{3, -1}, "[num]", "initial number to start with");
        l.add(null, false, 3, new int[]{-1}, "[num]", "increment number");
        l.add(null, false, 1, new int[]{2}, "action", "set action to do");
        l.add(null, false, 2, new int[]{-1}, "drop", "drop every packet");
        l.add(null, false, 2, new int[]{-1}, "transit", "forward every packet");
        l.add(null, false, 2, new int[]{-1}, "police", "police to rate");
        l.add(null, false, 2, new int[]{-1}, "pps", "police to packet rate");
        l.add(null, false, 2, new int[]{-1}, "shape", "shape to rate");
        l.add(null, false, 2, new int[]{-1}, "bandwidth", "bandwidth to rate");
        l.add(null, false, 2, new int[]{-1}, "priority", "priority to rate");
        l.add(null, false, 1, new int[]{2, -1}, "description", "description of this policy map");
        l.add(null, false, 2, new int[]{2, -1}, "[text]", "text describing this policy map");
        l.add(null, false, 1, new int[]{2}, "rename", "rename this policy map");
        l.add(null, false, 2, new int[]{-1}, "<str>", "set new name");
        l.add(null, false, 1, new int[]{2}, "match", "match values from packet");
        l.add(null, false, 2, new int[]{3}, "length", "match length value");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value to match");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{3}, "ttl", "match ttl value");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value to match");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{3}, "ethtyp", "match ethertype value");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value to match");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{3}, "access-group", "match by access list");
        l.add(null, false, 3, new int[]{-1}, "<name:acl>", "name of access list");
        l.add(null, false, 2, new int[]{3}, "precedence", "match precedence value");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value to match");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{3}, "dscp", "match dscp value");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value to match");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{3}, "tos", "match tos value");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value to match");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{-1}, "frag", "fragmented datagrams");
        l.add(null, false, 2, new int[]{3}, "flag", "tcp flags");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value to match");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{3}, "sgt", "match sgt value");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value to match");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{3}, "cos", "match cos value");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value to match");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{3}, "exp", "match exp value");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value to match");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{3}, "qosgroup", "match qos value");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value to match");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{3}, "flow", "match flow value");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value to match");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 1, new int[]{2}, "set", "set values in packet");
        l.add(null, false, 2, new int[]{3}, "precedence", "set precedence value");
        l.add(null, false, 3, new int[]{-1}, "leave", "leave value unchanged");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value");
        l.add(null, false, 2, new int[]{3}, "dscp", "set dscp value");
        l.add(null, false, 3, new int[]{-1}, "leave", "leave value unchanged");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value");
        l.add(null, false, 2, new int[]{3}, "tos", "set tos value");
        l.add(null, false, 3, new int[]{-1}, "leave", "leave value unchanged");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value");
        l.add(null, false, 2, new int[]{3}, "ttl", "set ttl value");
        l.add(null, false, 3, new int[]{-1}, "leave", "leave value unchanged");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value");
        l.add(null, false, 2, new int[]{3}, "sgt", "set sgt value");
        l.add(null, false, 3, new int[]{-1}, "leave", "leave value unchanged");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value");
        l.add(null, false, 2, new int[]{3}, "cos", "set cos value");
        l.add(null, false, 3, new int[]{-1}, "leave", "leave value unchanged");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value");
        l.add(null, false, 2, new int[]{3}, "exp", "set exp value");
        l.add(null, false, 3, new int[]{-1}, "leave", "leave value unchanged");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value");
        l.add(null, false, 2, new int[]{3}, "qosgroup", "set qosgroup value");
        l.add(null, false, 3, new int[]{-1}, "leave", "leave value unchanged");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value");
        l.add(null, false, 2, new int[]{3}, "flow", "set flow value");
        l.add(null, false, 3, new int[]{-1}, "leave", "leave value unchanged");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value");
        l.add(null, false, 2, new int[]{3}, "vrf", "set vrf table");
        l.add(null, false, 3, new int[]{-1}, "<name:vrf>", "name of vrf");
        l.add(null, false, 1, new int[]{2}, "access-rate", "set access rate of traffic");
        l.add(null, false, 2, new int[]{-1}, "<num>", "bits per second");
        l.add(null, false, 1, new int[]{2}, "exceed-rate", "set exceed rate of traffic");
        l.add(null, false, 2, new int[]{-1}, "<num>", "bits per second");
        l.add(null, false, 1, new int[]{2}, "service-policy", "apply policy map to this traffic");
        l.add(null, false, 2, new int[]{-1}, "<name:pm>", "name of policy map");
        l.add(null, false, 1, new int[]{2}, "queue-limit", "specify queue limit");
        l.add(null, false, 2, new int[]{-1}, "<num>", "number of packets");
        l.add(null, false, 1, new int[]{2}, "time-interval", "specify time interval");
        l.add(null, false, 2, new int[]{-1}, "<num>", "milliseconds");
        l.add(null, false, 1, new int[]{-1}, "random-detect", "set random drop on queue full");
        l.add(null, false, 1, new int[]{2}, "mark-ecn", "mark packets with ecn");
        l.add(null, false, 2, new int[]{-1}, "<num>", "divistor");
        l.add(null, false, 1, new int[]{-1}, "log", "set logging on match");
    }

    public List<String> getShRun(int filter) {
        List<String> l = new ArrayList<String>();
        l.add("policy-map " + name);
        l.addAll(plcmap.dump(cmds.tabulator, filter));
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        if ((filter & 1) == 0) {
            return l;
        }
        return userFilter.filterText(l, defaultF);
    }

    public synchronized void doCfgStr(cmds cmd) {
        String a = cmd.word();
        if (a.equals("sequence")) {
            seq = bits.str2num(cmd.word());
            a = cmd.word();
            if (a.length() < 1) {
                return;
            }
        }
        if (a.equals("action")) {
            tabListingEntry.actionType i = tabPlcmapN.string2type(cmd.word());
            if (i == null) {
                cmd.error("invalid action");
                return;
            }
            tabPlcmapN ntry = getCurr();
            ntry.action = i;
            return;
        }
        if (a.equals("description")) {
            tabPlcmapN ntry = getCurr();
            ntry.description = cmd.getRemaining();
            return;
        }
        if (a.equals("rename")) {
            a = cmd.word();
            cfgPlymp v = cfgAll.plmpFind(a, false);
            if (v != null) {
                cmd.error("already exists");
                return;
            }
            name = a;
            plcmap.listName = a;
            return;
        }
        if (a.equals("access-rate")) {
            tabPlcmapN ntry = getCurr();
            ntry.accessRate = bits.str2long(cmd.word()) / 8;
            return;
        }
        if (a.equals("exceed-rate")) {
            tabPlcmapN ntry = getCurr();
            ntry.exceedRate = bits.str2long(cmd.word()) / 8;
            return;
        }
        if (a.equals("queue-limit")) {
            tabPlcmapN ntry = getCurr();
            ntry.queues = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("time-interval")) {
            tabPlcmapN ntry = getCurr();
            ntry.interval = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("service-policy")) {
            cfgPlymp plc = cfgAll.plmpFind(cmd.word(), false);
            if (plc == null) {
                cmd.error("no such policy");
                return;
            }
            tabPlcmapN ntry = getCurr();
            ntry.child = plc.plcmap;
            return;
        }
        if (a.equals("random-detect")) {
            tabPlcmapN ntry = getCurr();
            ntry.randomDetect = true;
            return;
        }
        if (a.equals("mark-ecn")) {
            tabPlcmapN ntry = getCurr();
            ntry.markEcn = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("log")) {
            tabPlcmapN ntry = getCurr();
            ntry.logMatch = true;
            return;
        }
        if (a.equals("match")) {
            a = cmd.word();
            tabPlcmapN ntry = getCurr();
            if (a.equals("length")) {
                if (ntry.lengthMatch.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("ttl")) {
                if (ntry.ttlMatch.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("ethtyp")) {
                if (ntry.ethtypMatch.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("access-group")) {
                cfgAceslst acl = cfgAll.aclsFind(cmd.word(), false);
                if (acl == null) {
                    cmd.error("no such access list");
                    return;
                }
                ntry.aclMatch = acl.aceslst;
                return;
            }
            if (a.equals("precedence")) {
                if (ntry.precedenceMatch.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("dscp")) {
                if (ntry.dscpMatch.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("tos")) {
                if (ntry.tosMatch.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("flag")) {
                if (ntry.flagMatch.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("frag")) {
                ntry.fragMatch = true;
                return;
            }
            if (a.equals("sgt")) {
                if (ntry.sgtMatch.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("cos")) {
                if (ntry.cosMatch.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("exp")) {
                if (ntry.expMatch.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("qosgroup")) {
                if (ntry.qosMatch.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("flow")) {
                if (ntry.flowMatch.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("set")) {
            a = cmd.word();
            tabPlcmapN ntry = getCurr();
            if (a.equals("vrf")) {
                cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
                if (vrf == null) {
                    cmd.error("no such vrf");
                    return;
                }
                ntry.vrfSet4 = vrf.fwd4;
                ntry.vrfSet6 = vrf.fwd6;
                return;
            }
            if (a.equals("precedence")) {
                if (ntry.precedenceSet.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("dscp")) {
                if (ntry.dscpSet.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("tos")) {
                if (ntry.tosSet.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("ttl")) {
                if (ntry.ttlSet.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("sgt")) {
                if (ntry.sgtSet.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("cos")) {
                if (ntry.cosSet.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("exp")) {
                if (ntry.expSet.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("qosgroup")) {
                if (ntry.qosSet.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("flow")) {
                if (ntry.flowSet.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("reindex")) {
            int i = bits.str2num(cmd.word());
            plcmap.reindex(i, bits.str2num(cmd.word()));
            return;
        }
        if (!a.equals(cmds.negated)) {
            cmd.badCmd();
            return;
        }
        a = cmd.word();
        if (a.equals("description")) {
            tabPlcmapN ntry = getCurr();
            ntry.description = "";
            return;
        }
        if (a.equals("access-rate")) {
            tabPlcmapN ntry = getCurr();
            ntry.accessRate = 0;
            return;
        }
        if (a.equals("exceed-rate")) {
            tabPlcmapN ntry = getCurr();
            ntry.exceedRate = 0;
            return;
        }
        if (a.equals("queue-limit")) {
            tabPlcmapN ntry = getCurr();
            ntry.queues = 0;
            return;
        }
        if (a.equals("time-interval")) {
            tabPlcmapN ntry = getCurr();
            ntry.interval = 0;
            return;
        }
        if (a.equals("service-policy")) {
            tabPlcmapN ntry = getCurr();
            ntry.child = null;
            return;
        }
        if (a.equals("random-detect")) {
            tabPlcmapN ntry = getCurr();
            ntry.randomDetect = false;
            return;
        }
        if (a.equals("mark-ecn")) {
            tabPlcmapN ntry = getCurr();
            ntry.markEcn = 0;
            return;
        }
        if (a.equals("log")) {
            tabPlcmapN ntry = getCurr();
            ntry.logMatch = false;
            return;
        }
        if (a.equals("match")) {
            a = cmd.word();
            tabPlcmapN ntry = getCurr();
            if (a.equals("length")) {
                ntry.lengthMatch.set2always();
                return;
            }
            if (a.equals("ttl")) {
                ntry.ttlMatch.set2always();
                return;
            }
            if (a.equals("ethtyp")) {
                ntry.ethtypMatch.set2always();
                return;
            }
            if (a.equals("access-group")) {
                ntry.aclMatch = null;
                return;
            }
            if (a.equals("precedence")) {
                ntry.precedenceMatch.set2always();
                return;
            }
            if (a.equals("dscp")) {
                ntry.dscpMatch.set2always();
                return;
            }
            if (a.equals("tos")) {
                ntry.tosMatch.set2always();
                return;
            }
            if (a.equals("flag")) {
                ntry.flagMatch.set2always();
                return;
            }
            if (a.equals("frag")) {
                ntry.fragMatch = false;
                return;
            }
            if (a.equals("sgt")) {
                ntry.sgtMatch.set2always();
                return;
            }
            if (a.equals("cos")) {
                ntry.cosMatch.set2always();
                return;
            }
            if (a.equals("exp")) {
                ntry.expMatch.set2always();
                return;
            }
            if (a.equals("qosgroup")) {
                ntry.qosMatch.set2always();
                return;
            }
            if (a.equals("flow")) {
                ntry.flowMatch.set2always();
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("set")) {
            a = cmd.word();
            tabPlcmapN ntry = getCurr();
            if (a.equals("vrf")) {
                ntry.vrfSet4 = null;
                ntry.vrfSet6 = null;
                return;
            }
            if (a.equals("precedence")) {
                ntry.precedenceSet.set2unchange();
                return;
            }
            if (a.equals("dscp")) {
                ntry.dscpSet.set2unchange();
                return;
            }
            if (a.equals("tos")) {
                ntry.tosSet.set2unchange();
                return;
            }
            if (a.equals("ttl")) {
                ntry.ttlSet.set2unchange();
                return;
            }
            if (a.equals("sgt")) {
                ntry.sgtSet.set2unchange();
                return;
            }
            if (a.equals("cos")) {
                ntry.cosSet.set2unchange();
                return;
            }
            if (a.equals("exp")) {
                ntry.expSet.set2unchange();
                return;
            }
            if (a.equals("qosgroup")) {
                ntry.qosSet.set2unchange();
                return;
            }
            if (a.equals("flow")) {
                ntry.flowSet.set2unchange();
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("sequence")) {
            tabPlcmapN ntry = new tabPlcmapN();
            ntry.sequence = bits.str2num(cmd.word());
            if (plcmap.del(ntry)) {
                cmd.error("invalid sequence");
                return;
            }
            return;
        }
        cmd.badCmd();
    }

}
