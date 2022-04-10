package net.freertr.tab;

import java.util.ArrayList;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.pack.packHolder;
import net.freertr.util.bits;
import net.freertr.util.logger;

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

}
