package org.freertr.tab;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.cfg.cfgAll;
import org.freertr.ifc.ifcUp;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.user.userFormat;
import org.freertr.util.bits;
import org.freertr.util.counter;

/**
 * represents one label entry
 *
 * @author matecsaba
 */
public class tabLabelEntry implements Comparable<tabLabelEntry> {

    /**
     * label owner
     */
    public enum owner {
        /**
         * vrf common
         */
        vrfComm,
        /**
         * vrf unicast
         */
        vrfUni,
        /**
         * mpls pwe
         */
        pwe,
        /**
         * rsvp te
         */
        rsvp,
        /**
         * vrf mp2mp2
         */
        mp2mp,
        /**
         * lsrp sr gb
         */
        lsrpSrgb,
        /**
         * isis sr gb
         */
        isisSrgb,
        /**
         * ospf4 sr gb
         */
        ospf4srgb,
        /**
         * ospf6 sr gb
         */
        ospf6srgb,
        /**
         * bgp evpn pbb
         */
        evpnPbb,
        /**
         * bgp evpn cmac
         */
        evpnCmac,
        /**
         * bgp vpls ve
         */
        vplsVe,
        /**
         * bgp sr gb
         */
        bgpSrgb,
        /**
         * lsrp sr adj
         */
        lsrpAdj,
        /**
         * isis sr adj
         */
        isisAdj,
        /**
         * ospf4 sr adj
         */
        ospf4adj,
        /**
         * ospf6 sr adj
         */
        ospf6adj,
        /**
         * lsrp bier
         */
        lsrpBier,
        /**
         * isis bier
         */
        isisBier,
        /**
         * ospf4 bier
         */
        ospf4bier,
        /**
         * ospf6 bier
         */
        ospf6bier,
        /**
         * bgp bier
         */
        bgpBier,
        /**
         * stack
         */
        stack,
        /**
         * bridge
         */
        bridge,
        /**
         * pvrp sr gb
         */
        pvrpSrgb,
        /**
         * pvrv bier
         */
        pvrpBier,
        /**
         * mcast rx
         */
        mcastRx,
    }

    /**
     * label value
     */
    public final int label;

    /**
     * key value
     */
    protected owner key = null;

    /**
     * indicate that label is working
     */
    protected boolean working;

    /**
     * forwarder instance
     */
    public ipFwd forwarder = null;

    /**
     * interface number
     */
    public ipFwdIface iface;

    /**
     * next hop
     */
    public addrIP nextHop = null;

    /**
     * remote label
     */
    public List<Integer> remoteLab = null;

    /**
     * replication list
     */
    public tabGen<tabLabelDup> duplicate = null;

    /**
     * bier info
     */
    public tabLabelBier bier = null;

    /**
     * need local delivery
     */
    public boolean needLocal;

    /**
     * created
     */
    public long created;

    /**
     * counter
     */
    public counter cntr = new counter();

    /**
     * hardware counter
     */
    public counter hwCntr;

    /**
     * pseudowire interface
     */
    public ifcUp pweIfc = null;

    /**
     * pseudowire bytes to delete
     */
    public int pweDel;

    /**
     * pseudowire bytes to add
     */
    public byte[] pweAdd = null;

    /**
     * create one label
     *
     * @param lab label value
     */
    public tabLabelEntry(int lab) {
        label = lab;
        setFwdDrop(key);
        created = bits.getTime();
    }

    /**
     * get label hash
     *
     * @return xor value
     */
    public int getHashB() {
        return (label ^ (label >>> 8) ^ (label >>> 16)) & 0xff;
    }

    /**
     * get label hash
     *
     * @return xor value
     */
    public int getHashW() {
        return (label ^ (label >>> 16)) & 0xffff;
    }

    /**
     * copy this entry
     *
     * @return copy
     */
    public tabLabelEntry copyBytes() {
        tabLabelEntry n = new tabLabelEntry(label);
        n.key = key;
        n.working = working;
        n.forwarder = forwarder;
        n.iface = iface;
        if (nextHop != null) {
            n.nextHop = nextHop.copyBytes();
        }
        if (remoteLab != null) {
            n.remoteLab = tabLabel.copyLabels(remoteLab);
        }
        if (duplicate != null) {
            n.duplicate = new tabGen<tabLabelDup>();
            for (int i = 0; i < duplicate.size(); i++) {
                tabLabelDup ntry = duplicate.get(i);
                n.duplicate.add(new tabLabelDup(ntry.iface, ntry.hop, ntry.label));
            }
        }
        if (bier != null) {
            n.bier = bier.copyBytes();
        }
        n.needLocal = needLocal;
        n.cntr = cntr;
        n.created = created;
        n.pweIfc = pweIfc;
        n.pweDel = pweDel;
        n.pweAdd = pweAdd;
        return n;
    }

    /**
     * compare this entry
     *
     * @param o other
     * @return false if equals, true if differs
     */
    public boolean differs(tabLabelEntry o) {
        if (o == null) {
            return true;
        }
        if (label != o.label) {
            return true;
        }
        if (key != o.key) {
            return true;
        }
        if (forwarder != o.forwarder) {
            return true;
        }
        if (iface != o.iface) {
            return true;
        }
        if (nextHop == null) {
            if (o.nextHop != null) {
                return true;
            }
        } else {
            if (o.nextHop == null) {
                return true;
            }
            if (nextHop.compareTo(o.nextHop) != 0) {
                return true;
            }
        }
        if (tabRouteUtil.diffIntList(remoteLab, o.remoteLab)) {
            return true;
        }
        if (duplicate == null) {
            if (o.duplicate != null) {
                return true;
            }
        } else {
            if (o.duplicate == null) {
                return true;
            }
            if (duplicate.size() != o.duplicate.size()) {
                return true;
            }
            for (int i = 0; i < duplicate.size(); i++) {
                if (duplicate.get(i).differs(o.duplicate.get(i))) {
                    return true;
                }
            }
        }
        if (bier == null) {
            if (o.bier != null) {
                return true;
            }
        } else {
            if (o.bier == null) {
                return true;
            }
            if (bier.differs(o.bier)) {
                return true;
            }
        }
        if (needLocal != o.needLocal) {
            return true;
        }
        if (pweIfc != o.pweIfc) {
            return true;
        }
        if (pweDel != o.pweDel) {
            return true;
        }
        if (pweAdd != o.pweAdd) {
            return true;
        }
        return false;
    }

    public String toString() {
        return "" + label;
    }

    /**
     * dump this entry
     *
     * @return dump
     */
    public userFormat getShow() {
        userFormat lst = new userFormat("|", "category|value");
        lst.add("label|" + label);
        lst.add("created|" + bits.time2str(cfgAll.timeZoneName, created + cfgAll.timeServerOffset, 3));
        lst.add("lasted|" + bits.timePast(created));
        String s;
        switch (key) {
            case vrfComm:
                s = "vrf common";
                break;
            case vrfUni:
                s = "vrf unicast";
                break;
            case pwe:
                s = "mpls pwe";
                break;
            case rsvp:
                s = "rsvp te";
                break;
            case mp2mp:
                s = "vrf mp2mp";
                break;
            case lsrpSrgb:
                s = "lsrp segrou gb";
                break;
            case isisSrgb:
                s = "isis segrou gb";
                break;
            case ospf4srgb:
                s = "ospf4 segrou gb";
                break;
            case ospf6srgb:
                s = "ospf6 segrou gb";
                break;
            case evpnPbb:
                s = "bgp evpn pbb";
                break;
            case evpnCmac:
                s = "bgp evpn cmac";
                break;
            case vplsVe:
                s = "bgp vpls ve";
                break;
            case bgpSrgb:
                s = "bgp segrou gb";
                break;
            case lsrpAdj:
                s = "lsrp segrou adj";
                break;
            case isisAdj:
                s = "isis segrou adj";
                break;
            case ospf4adj:
                s = "ospf4 segrou adj";
                break;
            case ospf6adj:
                s = "ospf6 segrou adj";
                break;
            case lsrpBier:
                s = "lsrp bier";
                break;
            case isisBier:
                s = "isis bier";
                break;
            case ospf4bier:
                s = "ospf4 bier";
                break;
            case ospf6bier:
                s = "ospf6 bier";
                break;
            case bgpBier:
                s = "bgp bier";
                break;
            case stack:
                s = "stack";
                break;
            case bridge:
                s = "bridge";
                break;
            case pvrpSrgb:
                s = "pvrp segrou gb";
                break;
            case pvrpBier:
                s = "pvrp bier";
                break;
            case mcastRx:
                s="mcast rx";
                break;
            default:
                s = "unknown";
                break;
        }
        lst.add("key|" + key + "-" + s);
        lst.add("working|" + working);
        lst.add("forwarder|" + forwarder);
        lst.add("interface|" + iface);
        lst.add("nexthop|" + nextHop);
        if (remoteLab == null) {
            s = "unlabelled";
        } else {
            s = "";
            for (int i = 0; i < remoteLab.size(); i++) {
                s += " " + remoteLab.get(i);
            }
        }
        lst.add("remote label|" + s);
        lst.add("need local|" + needLocal);
        if (duplicate != null) {
            lst.add("duplicated|" + duplicate.size());
            for (int i = 0; i < duplicate.size(); i++) {
                lst.add("" + duplicate.get(i));
            }
        }
        if (bier != null) {
            List<String> l = bier.getShow(this);
            for (int i = 0; i < l.size(); i++) {
                lst.add(l.get(i));
            }
        }
        lst.add("pwe iface|" + pweIfc);
        lst.add("pwe del|" + pweDel);
        lst.add("pwe add|" + bits.byteDump(pweAdd, 0, -1));
        lst.add("counter|" + cntr.getShStat());
        lst.add("lastio|" + cntr.getShTraff());
        lst.add("hardware counter|" + counter.getShStat(hwCntr));
        return lst;
    }

    /**
     * dump this entry
     *
     * @return dump
     */
    public String getList() {
        String s;
        if (remoteLab == null) {
            s = "unlabelled";
        } else {
            s = "";
            for (int i = 0; i < remoteLab.size(); i++) {
                s += " " + remoteLab.get(i);
            }
        }
        s += "|";
        if (needLocal) {
            s += " local";
        }
        if (duplicate != null) {
            s += " duplicate";
        }
        if (bier != null) {
            s += " bier";
        }
        if (pweIfc != null) {
            s += " pwe";
        }
        String a = "";
        if (hwCntr != null) {
            a = "+" + hwCntr.byteRx;
        }
        return label + "|" + forwarder + "|" + iface + "|" + nextHop + "|" + s + "|" + cntr.byteRx + a;
    }

    public int compareTo(tabLabelEntry o) {
        if (label < o.label) {
            return -1;
        }
        if (label > o.label) {
            return +1;
        }
        return 0;
    }

    /**
     * set forwarding for drop
     *
     * @param ky key to use for deallocation
     */
    public void setFwdDrop(owner ky) {
        if (key != ky) {
            return;
        }
        forwarder = null;
        iface = null;
        nextHop = null;
        remoteLab = null;
        duplicate = null;
        needLocal = false;
    }

    /**
     * set forwarding for common label
     *
     * @param ky key to use for deallocation
     * @param fwd forwarder vrf
     */
    public void setFwdCommon(owner ky, ipFwd fwd) {
        if (key != ky) {
            return;
        }
        forwarder = fwd;
        iface = null;
        nextHop = null;
        remoteLab = null;
        needLocal = true;
    }

    /**
     * set forwarding for route out
     *
     * @param ky key to use for deallocation
     * @param fwd forwarder vrf
     * @param ifc interface id
     * @param hop next hop address
     */
    public void setFwdRoute(owner ky, ipFwd fwd, ipFwdIface ifc, addrIP hop) {
        if (key != ky) {
            return;
        }
        forwarder = fwd;
        iface = ifc;
        nextHop = hop.copyBytes();
        remoteLab = null;
    }

    /**
     * set forwarding for mpls out
     *
     * @param ky key to use for deallocation
     * @param fwd forwarder vrf
     * @param ifc interface id
     * @param hop next hop address
     * @param lab next hop labels
     */
    public void setFwdMpls(owner ky, ipFwd fwd, ipFwdIface ifc, addrIP hop, List<Integer> lab) {
        if (key != ky) {
            return;
        }
        forwarder = fwd;
        iface = ifc;
        nextHop = hop.copyBytes();
        remoteLab = tabLabel.copyLabels(lab);
    }

    /**
     * set duplication for mpls
     *
     * @param ky key to use for deallocation
     * @param fwd forwarder vrf
     * @param ifc interface id
     * @param hop next hop address
     * @param lab next hop labels
     */
    public void addDupMpls(owner ky, ipFwd fwd, ipFwdIface ifc, addrIP hop, List<Integer> lab) {
        if (key != ky) {
            return;
        }
        forwarder = fwd;
        if (duplicate == null) {
            duplicate = new tabGen<tabLabelDup>();
        }
        duplicate.put(new tabLabelDup(ifc, hop, lab));
    }

    /**
     * clear duplication for mpls
     *
     * @param ky key to use for deallocation
     * @param hop next hop address
     */
    public void delDupMpls(owner ky, addrIP hop) {
        if (key != ky) {
            return;
        }
        if (duplicate == null) {
            duplicate = new tabGen<tabLabelDup>();
        }
        tabLabelDup ntry = new tabLabelDup(null, hop, null);
        duplicate.del(ntry);
    }

    /**
     * clear all duplications for mpls
     *
     * @param ky key to use for deallocation
     */
    public void clrDupMpls(owner ky) {
        if (key != ky) {
            return;
        }
        duplicate = new tabGen<tabLabelDup>();
    }

    /**
     * set bier for mpls
     *
     * @param ky key to use for deallocation
     * @param fwd forwarder vrf
     * @param br bier
     */
    public void setBierMpls(owner ky, ipFwd fwd, tabLabelBier br) {
        if (key != ky) {
            return;
        }
        forwarder = fwd;
        bier = br;
    }

    /**
     * set forwarding for pwe out
     *
     * @param ky key to use for deallocation
     * @param fwd forwarder vrf
     * @param ifc interface to use
     * @param del bytes to delete
     * @param add bytes to add
     */
    public void setFwdPwe(owner ky, ipFwd fwd, ifcUp ifc, int del, byte[] add) {
        if (key != ky) {
            return;
        }
        forwarder = fwd;
        pweIfc = ifc;
        pweDel = del;
        if (add == null) {
            pweAdd = null;
        } else {
            pweAdd = new byte[add.length];
            bits.byteCopy(add, 0, pweAdd, 0, add.length);
        }
    }

}
