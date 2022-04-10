package net.freertr.tab;

import java.util.Comparator;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.cfg.cfgAll;
import net.freertr.ifc.ifcUp;
import net.freertr.ip.ipFwd;
import net.freertr.ip.ipFwdIface;
import net.freertr.user.userFormat;
import net.freertr.util.bits;
import net.freertr.util.counter;

/**
 * represents one label entry
 *
 * @author matecsaba
 */
public class tabLabelEntry implements Comparator<tabLabelEntry> {

    /**
     * label value
     */
    public final int label;

    /**
     * key value
     */
    protected int key = 0;

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
        return (label ^ (label >> 8) ^ (label >> 16)) & 0xff;
    }

    /**
     * get label hash
     *
     * @return xor value
     */
    public int getHashW() {
        return (label ^ (label >> 16)) & 0xffff;
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
            if (nextHop.compare(nextHop, o.nextHop) != 0) {
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
            case 1:
                s = "vrf common";
                break;
            case 2:
                s = "vrf unicast";
                break;
            case 3:
                s = "mpls pwe";
                break;
            case 4:
                s = "rsvp te";
                break;
            case 5:
                s = "vrf mp2mp";
                break;
            case 6:
                s = "lsrp segrou gb";
                break;
            case 7:
                s = "isis segrou gb";
                break;
            case 8:
                s = "ospf4 segrou gb";
                break;
            case 9:
                s = "ospf6 segrou gb";
                break;
            case 10:
                s = "bgp evpn pbb";
                break;
            case 11:
                s = "bgp evpn cmac";
                break;
            case 12:
                s = "bgp vpls ve";
                break;
            case 13:
                s = "bgp segrou gb";
                break;
            case 14:
                s = "lsrp segrou adj";
                break;
            case 15:
                s = "isis segrou adj";
                break;
            case 16:
                s = "ospf4 segrou adj";
                break;
            case 17:
                s = "ospf6 segrou adj";
                break;
            case 18:
                s = "lsrp bier";
                break;
            case 19:
                s = "isis bier";
                break;
            case 20:
                s = "ospf4 bier";
                break;
            case 21:
                s = "ospf6 bier";
                break;
            case 22:
                s = "bgp bier";
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

    public int compare(tabLabelEntry o1, tabLabelEntry o2) {
        if (o1.label < o2.label) {
            return -1;
        }
        if (o1.label > o2.label) {
            return +1;
        }
        return 0;
    }

    /**
     * set forwarding for drop
     *
     * @param ky key to use for deallocation
     */
    public void setFwdDrop(int ky) {
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
    public void setFwdCommon(int ky, ipFwd fwd) {
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
    public void setFwdRoute(int ky, ipFwd fwd, ipFwdIface ifc, addrIP hop) {
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
    public void setFwdMpls(int ky, ipFwd fwd, ipFwdIface ifc, addrIP hop, List<Integer> lab) {
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
    public void addDupMpls(int ky, ipFwd fwd, ipFwdIface ifc, addrIP hop, List<Integer> lab) {
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
    public void delDupMpls(int ky, addrIP hop) {
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
    public void clrDupMpls(int ky) {
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
    public void setBierMpls(int ky, ipFwd fwd, tabLabelBier br) {
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
    public void setFwdPwe(int ky, ipFwd fwd, ifcUp ifc, int del, byte[] add) {
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
