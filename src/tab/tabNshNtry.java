package tab;

import addr.addrMac;
import cfg.cfgAll;
import cfg.cfgGeneric;
import cfg.cfgIfc;
import cfg.cfgVrf;
import ifc.ifcEthTyp;
import ip.ipFwd;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import user.userFormat;
import user.userHelping;
import util.bits;
import util.cmds;
import util.counter;

/**
 * network service header entry
 *
 * @author matecsaba
 */
public class tabNshNtry implements Comparator<tabNshNtry>, cfgGeneric {

    /**
     * service table
     */
    public final static tabGen<tabNshNtry> services = new tabGen<tabNshNtry>();

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
    public tabNshNtry(int p, int i) {
        sp = p;
        si = i;
    }

    public String toString() {
        return sp + " " + si;
    }

    public int compare(tabNshNtry o1, tabNshNtry o2) {
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
     * get help
     *
     * @return help
     */
    public userHelping getHelp() {
        return new userHelping();
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
    public List<String> getShRun(boolean filter) {
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
            tabNshNtry ntry = services.get(i);
            if (ntry == null) {
                continue;
            }
            lst.add(ntry.sp + "|" + ntry.si + "|" + ntry.getCmd() + "|" + ntry.cntr.byteRx);
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
        return lst;
    }

}
