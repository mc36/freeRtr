package org.freertr.tab;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.pack.packHolder;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.logger;

/**
 * represents one route policy handler
 *
 * @author matecsaba
 */
public class tabRtrplc {

    private tabRtrplc() {
    }

    /**
     * indent table
     *
     * @param lst list to update
     */
    public static void indent(tabListing<tabRtrplcN, addrIP> lst) {
        int o = 0;
        for (int i = 0; i < lst.entries.size(); i++) {
            tabRtrplcN ntry = lst.entries.get(i);
            if (ntry == null) {
                continue;
            }
            ntry.indent = o;
            switch (ntry.doMode) {
                case iff:
                    o++;
                    break;
                case elsif:
                case els:
                    ntry.indent--;
                    break;
                case enif:
                    ntry.indent--;
                    o--;
                    break;
                default:
                    break;
            }
        }
    }

    private static int findEnif(tabListing<tabRtrplcN, addrIP> lst, int pos, int lvl) {
        pos++;
        for (; pos < lst.entries.size(); pos++) {
            tabRtrplcN ntry = lst.entries.get(pos);
            if (ntry == null) {
                continue;
            }
            if (ntry.indent > lvl) {
                continue;
            }
            if (ntry.doMode == tabRtrplcN.doType.enif) {
                break;
            }
        }
        return pos;
    }

    /**
     * update one entry
     *
     * @param afi address family
     * @param asn as number
     * @param net network number
     * @param lst list to use
     * @param copy copy before update
     * @return null if denied, copy otherwise
     */
    public static tabRouteEntry<addrIP> doRpl(int afi, int asn, tabRouteEntry<addrIP> net, tabListing<tabRtrplcN, addrIP> lst, boolean copy) {
        int lvl = 0;
        long tim = bits.getTime();
        packHolder pck = new packHolder(false, false);
        for (int pos = 0; pos < lst.entries.size(); pos++) {
            tabRtrplcN ntry = lst.entries.get(pos);
            if (ntry == null) {
                continue;
            }
            if (ntry.indent > lvl) {
                continue;
            }
            ntry.cntr.rx(pck);
            ntry.lastMatch = tim;
            if (ntry.logMatch) {
                logger.info("list " + lst.listName + " matched at sequence " + ntry.sequence + " on " + net);
            }
            switch (ntry.doMode) {
                case next:
                    continue;
                case description:
                    continue;
                case iff:
                    if (ntry.matches(afi, asn, net)) {
                        lvl++;
                        continue;
                    }
                    pos++;
                    for (; pos < lst.entries.size(); pos++) {
                        for (; pos < lst.entries.size(); pos++) {
                            ntry = lst.entries.get(pos);
                            if (ntry == null) {
                                continue;
                            }
                            if (ntry.indent > lvl) {
                                continue;
                            }
                            break;
                        }
                        ntry = lst.entries.get(pos);
                        if (ntry == null) {
                            break;
                        }
                        boolean stop = false;
                        switch (ntry.doMode) {
                            case elsif:
                                if (ntry.matches(afi, asn, net)) {
                                    lvl++;
                                    stop = true;
                                    break;
                                }
                                break;
                            case els:
                                lvl++;
                                stop = true;
                                break;
                            default:
                                stop = true;
                                break;
                        }
                        if (stop) {
                            break;
                        }
                    }
                    continue;
                case elsif:
                    lvl--;
                    pos = findEnif(lst, pos, lvl);
                    continue;
                case els:
                    lvl--;
                    pos = findEnif(lst, pos, lvl);
                    continue;
                case enif:
                    lvl--;
                    continue;
                case pass:
                    if (copy) {
                        net = net.copyBytes(tabRoute.addType.ecmp);
                        copy = false;
                    }
                    return net;
                case drop:
                    return null;
                case log:
                    logger.info("list " + lst.listName + " matched at sequence " + ntry.sequence + " on " + net);
                    continue;
                case tcl:
                    if (copy) {
                        net = net.copyBytes(tabRoute.addType.ecmp);
                        copy = false;
                    }
                    List<String> scr = new ArrayList<String>();
                    for (; pos < lst.entries.size(); pos++) {
                        ntry = lst.entries.get(pos);
                        if (ntry == null) {
                            continue;
                        }
                        if (ntry.doMode != tabRtrplcN.doType.tcl) {
                            break;
                        }
                        scr.add(ntry.strVal);
                    }
                    pos--;
                    tabRouteUtil.doTcl(afi, asn, net.best, net, scr);
                    continue;
                default:
                    if (copy) {
                        net = net.copyBytes(tabRoute.addType.ecmp);
                        copy = false;
                    }
                    ntry.update(afi, asn, net);
                    continue;
            }
        }
        return null;
    }

    /**
     * convert route map to route policy
     *
     * @param src source
     * @return converted
     */
    public static List<String> convertRm2rpl(List<String> src) {
        src.add("sequence end last");
        List<String> res = new ArrayList<String>();
        List<String> ifs = new ArrayList<String>();
        List<String> act = new ArrayList<String>();
        String mod = "";
        String seq = "beg";
        for (int p = 0; p < src.size(); p++) {
            String a = src.get(p);
            a = a.trim();
            if (!a.startsWith("sequence ")) {
                continue;
            }
            a = a.substring(9, a.length());
            int i = a.indexOf(" ");
            if (i < 0) {
                continue;
            }
            String b = a.substring(0, i);
            a = a.substring(i + 1, a.length());
            if (!b.equals(seq)) {
                seq = b;
                for (i = 0; i < ifs.size(); i++) {
                    res.add(bits.padBeg("", i, cmds.tabulator) + ifs.get(i));
                }
                b = bits.padBeg("", ifs.size(), cmds.tabulator);
                for (i = 0; i < act.size(); i++) {
                    res.add(b + act.get(i));
                }
                res.add(b + mod);
                for (i = ifs.size() - 1; i >= 0; i--) {
                    res.add(bits.padBeg("", i, cmds.tabulator) + "enif");
                }
                ifs = new ArrayList<String>();
                act = new ArrayList<String>();
                mod = "";
            }
            if (a.startsWith("match ")) {
                ifs.add("if " + a.substring(6, a.length()));
                continue;
            }
            if (a.equals("action permit")) {
                mod = "pass";
                continue;
            }
            if (a.equals("action deny")) {
                mod = "drop";
                continue;
            }
            if (a.equals("log")) {
                act.add(a);
                continue;
            }
            if (a.startsWith("tcladd ")) {
                act.add("tcl " + a.substring(7, a.length()));
                continue;
            }
            if (a.startsWith("set ")) {
                act.add(a);
                continue;
            }
            if (a.startsWith("clear ")) {
                act.add(a);
                continue;
            }
        }
        return res;
    }

    /**
     * convert route policy to route map
     *
     * @param src source
     * @return converted
     */
    public static List<String> convertRpl2rm(List<String> src) {
        List<String> res = new ArrayList<String>();
        List<String> ifs = new ArrayList<String>();
        List<String> act = new ArrayList<String>();
        int seq = 0;
        for (int p = 0; p < src.size(); p++) {
            String a = src.get(p);
            a = a.trim();
            if (!a.startsWith("sequence ")) {
                continue;
            }
            a = a.substring(9, a.length());
            int i = a.indexOf(" ");
            if (i < 0) {
                continue;
            }
            a = a.substring(i + 1, a.length()).trim();
            if (a.startsWith("if ")) {
                ifs.add("match " + a.substring(3, a.length()));
                continue;
            }
            if (a.equals("enif")) {
                if (ifs.size() < 1) {
                    continue;
                }
                ifs.remove(ifs.size() - 1);
                continue;
            }
            if (a.equals("elsif")) {
                if (ifs.size() > 0) {
                    ifs.remove(ifs.size() - 1);
                }
                ifs.add("match " + a.substring(3, a.length()));
                continue;
            }
            if (a.equals("else")) {
                if (ifs.size() < 1) {
                    continue;
                }
                ifs.remove(ifs.size() - 1);
                continue;
            }
            if (a.equals("log")) {
                act.add(a);
                continue;
            }
            if (a.startsWith("tcl ")) {
                act.add("tcladd " + a.substring(4, a.length()));
                continue;
            }
            if (a.startsWith("set ")) {
                act.add(a);
                continue;
            }
            if (a.startsWith("clear ")) {
                act.add(a);
                continue;
            }
            String s = null;
            if (a.equals("pass")) {
                s = "permit";
            }
            if (a.equals("drop")) {
                s = "deny";
            }
            if (s == null) {
                continue;
            }
            seq += 10;
            a = "sequence " + seq + " ";
            res.add(a + "action " + s);
            for (i = 0; i < ifs.size(); i++) {
                res.add(a + ifs.get(i));
            }
            for (i = 0; i < act.size(); i++) {
                res.add(a + act.get(i));
            }
            res.add(cmds.comment);
            ifs = new ArrayList<String>();
            act = new ArrayList<String>();
        }
        return res;
    }

}
