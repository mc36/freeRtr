package tab;

import addr.addrIP;
import java.util.ArrayList;
import java.util.List;
import util.logger;

/**
 * represents one route policy handler
 *
 * @author matecsaba
 */
public class tabRtrplc {

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
     * @param net network number
     * @param lst list to use
     * @param copy copy before update
     * @return null if denied, copy otherwise
     */
    public static tabRouteEntry<addrIP> doRpl(int afi, tabRouteEntry<addrIP> net, tabListing<tabRtrplcN, addrIP> lst, boolean copy) {
        if (copy) {
            net = net.copyBytes();
        }
        int lvl = 0;
        for (int pos = 0; pos < lst.entries.size(); pos++) {
            tabRtrplcN ntry = lst.entries.get(pos);
            if (ntry == null) {
                continue;
            }
            if (ntry.indent > lvl) {
                continue;
            }
            switch (ntry.doMode) {
                case next:
                    continue;
                case description:
                    continue;
                case iff:
                    if (ntry.matches(afi, net)) {
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
                                if (ntry.matches(afi, net)) {
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
                    ntry.countPack++;
                    if (ntry.logMatch) {
                        logger.info("list " + lst.listName + " matched at sequence " + ntry.sequence + " on " + net);
                    }
                    return net;
                case drop:
                    ntry.countPack++;
                    if (ntry.logMatch) {
                        logger.info("list " + lst.listName + " matched at sequence " + ntry.sequence + " on " + net);
                    }
                    return null;
                case log:
                    ntry.countPack++;
                    logger.info("list " + lst.listName + " matched at sequence " + ntry.sequence + " on " + net);
                    continue;
                case tcl:
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
                    tabRtrmapN.doTcl(afi, net, scr);
                    continue;
                default:
                    ntry.update(afi, net);
                    continue;
            }
        }
        return null;
    }

}
