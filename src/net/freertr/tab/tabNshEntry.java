package net.freertr.tab;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import net.freertr.addr.addrMac;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgGeneric;
import net.freertr.cfg.cfgIfc;
import net.freertr.cfg.cfgVrf;
import net.freertr.ifc.ifcEthTyp;
import net.freertr.ip.ipFwd;
import net.freertr.user.userFormat;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.counter;

/**
 * network service header entry
 *
 * @author matecsaba
 */
public class tabNshEntry implements Comparator<tabNshEntry>, cfgGeneric {

    /**
     * service table
     */
    public final static tabGen<tabNshEntry> services = new tabGen<tabNshEntry>();

    /**
     * service path id
     */
    public final int sp;

    /**
     * service index
     */
    public final int si;

    /**
     * counter of this interface
     */
    public counter cntr = new counter();

    /**
     * hardware counter
     */
    public counter hwCntr;

    /**
     * target sp
     */
    public int trgSp;

    /**
     * target si
     */
    public int trgSi;

    /**
     * target interface
     */
    public ifcEthTyp iface;

    /**
     * target address
     */
    public addrMac target;

    /**
     * target ipv4 forwarder
     */
    public ipFwd route4;

    /**
     * target ipv6 forwarder
     */
    public ipFwd route6;

    /**
     * raw packet
     */
    public boolean rawPack;

    /**
     * keep header
     */
    public boolean keepHdr;

    /**
     * create one nsh
     *
     * @param p path id
     * @param i index
     */
    public tabNshEntry(int p, int i) {
        sp = p;
        si = i;
    }

    public String toString() {
        return sp + " " + si;
    }

    public int compare(tabNshEntry o1, tabNshEntry o2) {
        if (o1.sp < o2.sp) {
            return -1;
        }
        if (o1.sp > o2.sp) {
            return +1;
        }
        if (o1.si < o2.si) {
            return -1;
        }
        if (o1.si > o2.si) {
            return +1;
        }
        return 0;
    }

    /**
     * copy entry
     *
     * @return clone
     */
    public tabNshEntry copyBytes() {
        tabNshEntry n = new tabNshEntry(sp, si);
        n.cntr = cntr;
        n.trgSp = trgSp;
        n.trgSi = trgSi;
        n.iface = iface;
        if (target != null) {
            n.target = target;
        }
        n.route4 = route4;
        n.route6 = route6;
        n.rawPack = rawPack;
        n.keepHdr = keepHdr;
        return n;
    }

    /**
     * compare to other entry
     *
     * @param o other
     * @return true if differs, false if equals
     */
    public boolean differs(tabNshEntry o) {
        if (trgSp != o.trgSp) {
            return true;
        }
        if (trgSi != o.trgSi) {
            return true;
        }
        if (iface != o.iface) {
            return true;
        }
        if (target == null) {
            if (o.target != null) {
                return true;
            }
        } else {
            if (o.target == null) {
                return true;
            }
            if (target.compare(target, o.target) != 0) {
                return true;
            }
        }
        if (route4 != o.route4) {
            return true;
        }
        if (route6 != o.route6) {
            return true;
        }
        if (rawPack != o.rawPack) {
            return true;
        }
        if (keepHdr != o.keepHdr) {
            return true;
        }
        return false;
    }

    /**
     * get help text
     *
     * @param l help text
     */
    public void getHelp(userHelping l) {
    }

    private String getCmd() {
        String a = "drop";
        if (iface != null) {
            a = "interface " + iface + " " + target;
        }
        if (route4 != null) {
            a = "route " + route4.cfgName;
        }
        return a;
    }

    /**
     * get configuration
     *
     * @param filter filter
     * @return config
     */
    public List<String> getShRun(int filter) {
        List<String> l = new ArrayList<String>();
        String a = getCmd();
        if ((trgSp != sp) || (trgSi != (si - 1))) {
            a += " switch " + trgSp + " " + trgSi;
        }
        if (rawPack) {
            a += " rawpack";
        }
        if (keepHdr) {
            a += " keephdr";
        }
        l.add("nsh " + sp + " " + si + " " + a);
        l.add(cmds.comment);
        return l;
    }

    /**
     * configure
     *
     * @param cmd command
     */
    public void doCfgStr(cmds cmd) {
        trgSp = sp;
        trgSi = si - 1;
        for (;;) {
            String s = cmd.word();
            if (s.length() < 1) {
                break;
            }
            if (s.equals("interface")) {
                cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
                if (ifc == null) {
                    cmd.error("no such interface");
                    return;
                }
                addrMac adr = new addrMac();
                if (adr.fromString(cmd.word())) {
                    cmd.error("bad address");
                    return;
                }
                iface = ifc.ethtyp;
                target = adr;
                route4 = null;
                route6 = null;
                continue;
            }
            if (s.equals("route")) {
                cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
                if (vrf == null) {
                    cmd.error("no such vrf");
                    return;
                }
                route4 = vrf.fwd4;
                route6 = vrf.fwd6;
                iface = null;
                target = null;
                continue;
            }
            if (s.equals("drop")) {
                route4 = null;
                route6 = null;
                iface = null;
                target = null;
                continue;
            }
            if (s.equals("rawpack")) {
                rawPack = true;
                continue;
            }
            if (s.equals("keephdr")) {
                keepHdr = true;
                continue;
            }
            if (s.equals("switch")) {
                trgSp = bits.str2num(cmd.word());
                trgSi = bits.str2num(cmd.word());
                continue;
            }
        }
    }

    /**
     * get prompt
     *
     * @return prompt
     */
    public String getPrompt() {
        return "nsh";
    }

    /**
     * get sh nsh int output
     *
     * @return list of string
     */
    public static userFormat getShInt() {
        userFormat lst = new userFormat("|", "interface|packet");
        for (int i = 0; i < cfgAll.ifaces.size(); i++) {
            cfgIfc ntry = cfgAll.ifaces.get(i);
            lst.add(ntry.name + "|" + (ntry.nshPack != null));
        }
        return lst;
    }

    /**
     * get sh mpls for output
     *
     * @return list of string
     */
    public static userFormat getShFor() {
        userFormat lst = new userFormat("|", "sp|si|target|bytes");
        for (int i = 0; i < services.size(); i++) {
            tabNshEntry ntry = services.get(i);
            if (ntry == null) {
                continue;
            }
            String a = "";
            if (ntry.hwCntr != null) {
                a = "+" + ntry.hwCntr.byteRx;
            }
            lst.add(ntry.sp + "|" + ntry.si + "|" + ntry.getCmd() + "|" + ntry.cntr.byteRx + a);
        }
        return lst;
    }

    /**
     * dump this entry
     *
     * @return dump
     */
    public userFormat getShow() {
        userFormat lst = new userFormat("|", "category|value");
        lst.add("path|" + sp);
        lst.add("index|" + si);
        lst.add("out path|" + trgSp);
        lst.add("out index|" + trgSi);
        lst.add("iface|" + iface);
        lst.add("target|" + target);
        lst.add("route|" + route4 + " " + route6);
        lst.add("rawpack|" + rawPack);
        lst.add("keephdr|" + keepHdr);
        lst.add("counter|" + cntr.getShStat());
        lst.add("lastio|" + cntr.getShTraff());
        lst.add("hardware counter|" + counter.getShStat(hwCntr));
        return lst;
    }

}
