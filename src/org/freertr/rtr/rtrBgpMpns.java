package org.freertr.rtr;

import org.freertr.addr.addrIP;
import org.freertr.addr.addrPrefix;
import org.freertr.cfg.cfgAll;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabLabel;
import org.freertr.tab.tabLabelEntry;
import org.freertr.tab.tabRoute;
import org.freertr.tab.tabRouteEntry;
import org.freertr.util.bits;

/**
 * bgp4 mpls namespace
 *
 * @author matecsaba
 */
public class rtrBgpMpns {

    private rtrBgpMpns() {
    }

    /**
     * advertise mpns routes
     *
     * @param tab table to update
     * @param attr entry to update
     * @param fwd forwarder to use
     * @param ctx context label
     */
    public static void doAdvertise(tabRoute<addrIP> tab, tabRouteEntry<addrIP> attr, ipFwd fwd, int ctx) {
        attr.best.nextHop = new addrIP();
        attr.best.rouSrc = rtrBgpUtil.peerOriginate;
        for (int i = 0; i < cfgAll.statLabs.size(); i++) {
            tabLabelEntry ntry = cfgAll.statLabs.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.forwarder != fwd) {
                continue;
            }
            addrIP adr = new addrIP();
            bits.msbPutD(adr.getBytes(), 0, ntry.label);
            bits.msbPutD(adr.getBytes(), 4, ctx);
            attr.prefix = new addrPrefix<addrIP>(adr, addrIP.size * 8);
            tab.add(tabRoute.addType.better, attr, true, true);
        }
    }

    /**
     * decode mpns routes
     *
     * @param tab list of routes
     * @param fwd forwarder to use
     * @return labels needed
     */
    public static tabGen<tabLabelEntry> doDecode(tabRoute<addrIP> tab, ipFwd fwd) {
        tabGen<tabLabelEntry> need = new tabGen<tabLabelEntry>();
        for (int i = 0; i < tab.size(); i++) {
            tabRouteEntry<addrIP> ntry = tab.get(i);
            if (ntry.best.rouSrc == rtrBgpUtil.peerOriginate) {
                continue;
            }
            tabRouteEntry<addrIP> rou = fwd.actualU.route(ntry.best.nextHop);
            if (rou == null) {
                continue;
            }
            byte[] buf = ntry.prefix.network.getBytes();
            tabLabelEntry res = new tabLabelEntry(bits.msbGetD(buf, 0));
            res.remoteLab = tabLabel.int2labels(res.label);
            int o = bits.msbGetD(buf, 4);
            if (o != 0) {
                res.remoteLab = tabLabel.prependLabel(res.remoteLab, o);
            }
            res.forwarder = fwd;
            res.iface = (ipFwdIface) rou.best.iface;
            if (rou.best.nextHop == null) {
                res.nextHop = ntry.best.nextHop.copyBytes();
            } else {
                res.nextHop = rou.best.nextHop.copyBytes();
                res.remoteLab = tabLabel.prependLabels(res.remoteLab, rou.best.labelRem);
            }
            need.add(res);
        }
        return need;
    }

    /**
     * install mpns routes
     *
     * @param need list of routes
     * @param done labels done
     */
    public static void doInstall(tabGen<tabLabelEntry> need, tabGen<tabLabelEntry> done) {
        for (int i = 0; i < need.size(); i++) {
            tabLabelEntry ntry = need.get(i);
            tabLabelEntry old = done.find(ntry);
            if (!ntry.differs(old)) {
                continue;
            }
            if (old == null) {
                old = tabLabel.allocateExact(tabLabelEntry.owner.mpns, ntry.label);
            }
            if (old == null) {
                continue;
            }
            old.setFwdMpls(tabLabelEntry.owner.mpns, ntry.forwarder, ntry.iface, ntry.nextHop, ntry.remoteLab);
            done.put(old);
        }
        for (int i = 0; i < done.size(); i++) {
            tabLabelEntry ntry = done.get(i);
            if (need.find(ntry) != null) {
                continue;
            }
            tabLabel.release(ntry, tabLabelEntry.owner.mpns);
            done.del(ntry);
        }
    }

}
