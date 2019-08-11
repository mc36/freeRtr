package tab;

import addr.addrIP;
import ifc.ifcUp;
import ip.ipFwd;
import ip.ipFwdIface;
import java.util.Comparator;
import java.util.List;
import user.userFormat;
import util.bits;
import util.counter;

/**
 * represents one label entry
 *
 * @author matecsaba
 */
public class tabLabelNtry implements Comparator<tabLabelNtry> {

    /**
     * label value
     */
    private final int label;

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
     * counter
     */
    public counter cntr = new counter();

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
    public tabLabelNtry(int lab) {
        label = lab;
        setFwdDrop(key);
    }

    /**
     * copy this entry
     *
     * @return copy
     */
    public tabLabelNtry copyBytes() {
        tabLabelNtry n = new tabLabelNtry(label);
        n.key = key;
        n.working = working;
        n.forwarder = forwarder;
        n.iface = iface;
        n.nextHop = nextHop;
        n.remoteLab = remoteLab;
        n.duplicate = duplicate;
        n.bier = bier;
        n.needLocal = needLocal;
        n.cntr = cntr;
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
    public boolean differs(tabLabelNtry o) {
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
        if (nextHop != o.nextHop) {
            return true;
        }
        if (remoteLab != o.remoteLab) {
            return true;
        }
        if (duplicate != o.duplicate) {
            return true;
        }
        if (bier != o.bier) {
            return true;
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
        String s;
        switch (key) {
            case 1:
                s = "vrf common";
                break;
            case 2:
                s = "vrf uniicast";
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
            List<String> l = bier.getShow();
            for (int i = 0; i < l.size(); i++) {
                lst.add(l.get(i));
            }
        }
        lst.add("pwe iface|" + pweIfc);
        lst.add("pwe del|" + pweDel);
        lst.add("pwe add|" + bits.byteDump(pweAdd, 0, -1));
        lst.add("counter|" + cntr.getShStat());
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
        return label + "|" + forwarder + "|" + iface + "|" + nextHop + "|" + s + "|" + cntr.byteRx;
    }

    public int compare(tabLabelNtry o1, tabLabelNtry o2) {
        if (o1.label < o2.label) {
            return -1;
        }
        if (o1.label > o2.label) {
            return +1;
        }
        return 0;
    }

    /**
     * get label value
     *
     * @return label value
     */
    public int getValue() {
        return label;
    }

    /**
     * get working state
     *
     * @return true means working, false means not
     */
    public boolean getWorking() {
        return working;
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
    public void setDupMpls(int ky, ipFwd fwd, ipFwdIface ifc, addrIP hop, List<Integer> lab) {
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
    public void clrDupMpls(int ky, addrIP hop) {
        if (key != ky) {
            return;
        }
        if (duplicate == null) {
            return;
        }
        tabLabelDup ntry = new tabLabelDup(null, hop, null);
        duplicate.del(ntry);
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
