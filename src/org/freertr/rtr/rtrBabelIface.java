package org.freertr.rtr;

import java.util.List;
import org.freertr.addr.addrEui;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrIPv6;
import org.freertr.addr.addrPrefix;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgPrfxlst;
import org.freertr.cfg.cfgRoump;
import org.freertr.cfg.cfgRouplc;
import org.freertr.ip.ipFwdIface;
import org.freertr.pack.packHolder;
import org.freertr.prt.prtGenConn;
import org.freertr.tab.tabListing;
import org.freertr.tab.tabPrfxlstN;
import org.freertr.tab.tabRoute;
import org.freertr.tab.tabRouteAttr;
import org.freertr.tab.tabRouteEntry;
import org.freertr.tab.tabRtrmapN;
import org.freertr.tab.tabRtrplcN;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.enc.encTlv;

/**
 * babel2 interface
 *
 * @author matecsaba
 */
public class rtrBabelIface implements Comparable<rtrBabelIface> {

    /**
     * advertisement interval
     */
    public int updateTimer = 20000;

    /**
     * time to wait between packets
     */
    public int interPackTime = 20;

    /**
     * default distance
     */
    public int distance = 130;

    /**
     * default metric in
     */
    public int metricIn = 100;

    /**
     * default metric out
     */
    public int metricOut = 0;

    /**
     * bfd enabled
     */
    public boolean bfdTrigger;

    /**
     * not advertise routes learned from interface back
     */
    public boolean splitHorizon = true;

    /**
     * advertise default route
     */
    public boolean defOrigin = false;

    /**
     * suppress interface address
     */
    public boolean suppressAddr = false;

    /**
     * unsuppress interface address
     */
    public boolean unsuppressAddr = false;

    /**
     * check neighbor address is connected
     */
    public boolean connectedCheck = true;

    /**
     * ingress prefix list
     */
    public tabListing<tabPrfxlstN, addrIP> prflstIn;

    /**
     * egress prefix list
     */
    public tabListing<tabPrfxlstN, addrIP> prflstOut;

    /**
     * ingress route map
     */
    public tabListing<tabRtrmapN, addrIP> roumapIn;

    /**
     * egress route map
     */
    public tabListing<tabRtrmapN, addrIP> roumapOut;

    /**
     * ingress route policy
     */
    public tabListing<tabRtrplcN, addrIP> roupolIn;

    /**
     * egress route policy
     */
    public tabListing<tabRtrplcN, addrIP> roupolOut;

    /**
     * other enabled
     */
    public boolean otherEna;

    /**
     * other unsuppress interface address
     */
    public boolean othUnsuppAddr = false;

    /**
     * other suppress interface address
     */
    public boolean othSuppAddr = false;

    /**
     * other advertise default route
     */
    public boolean othDefOrg = false;

    /**
     * other default distance
     */
    public int othDist = 130;

    /**
     * ingress prefix list
     */
    public tabListing<tabPrfxlstN, addrIP> oprflstIn;

    /**
     * egress prefix list
     */
    public tabListing<tabPrfxlstN, addrIP> oprflstOut;

    /**
     * ingress route map
     */
    public tabListing<tabRtrmapN, addrIP> oroumapIn;

    /**
     * egress route map
     */
    public tabListing<tabRtrmapN, addrIP> oroumapOut;

    /**
     * ingress route policy
     */
    public tabListing<tabRtrplcN, addrIP> oroupolIn;

    /**
     * egress route policy
     */
    public tabListing<tabRtrplcN, addrIP> oroupolOut;

    /**
     * the interface this works on
     */
    protected final ipFwdIface iface;

    /**
     * the other interface this works on
     */
    protected final ipFwdIface oface;

    /**
     * the udp connection it uses to multicast
     */
    protected prtGenConn conn;

    /**
     * the lower layer
     */
    protected final rtrBabel lower;

    /**
     * sequence number
     */
    protected int seqno;

    /**
     * prefixes advertised on interface
     */
    public tabRoute<addrIP> advert;

    /**
     * other prefixes advertised on interface
     */
    public tabRoute<addrIP> oadvert;

    /**
     * create one instance
     *
     * @param parent the babel protocol
     * @param ifc the ip interface to work on
     * @param oifc the other ip interface to work on
     */
    public rtrBabelIface(rtrBabel parent, ipFwdIface ifc, ipFwdIface oifc) {
        lower = parent;
        iface = ifc;
        oface = oifc;
        seqno = bits.randomW();
        advert = new tabRoute<addrIP>("babel");
        oadvert = new tabRoute<addrIP>("babel");
    }

    /**
     * unregister from udp
     */
    public void unregister2udp() {
        lower.udpCore.listenStop(iface, rtrBabel.port, null, 0);
        conn.setClosing();
    }

    /**
     * register to udp
     */
    public void register2udp() {
        addrIP adr = new addrIP();
        if (iface.addr.isIPv4()) {
            adr.fromString("224.0.0.111");
        } else {
            adr.fromString("ff02::1:6");
        }
        lower.udpCore.packetListen(lower, iface, rtrBabel.port, null, 0, "babel", -1, null, -1, -1);
        conn = lower.udpCore.packetConnect(lower, iface, rtrBabel.port, adr, rtrBabel.port, "babel", -1, null, -1, -1);
        if (conn == null) {
            return;
        }
        conn.timeout = 0;
    }

    public String toString() {
        return "babel on " + iface;
    }

    public int compareTo(rtrBabelIface o) {
        if (iface.ifwNum < o.iface.ifwNum) {
            return -1;
        }
        if (iface.ifwNum > o.iface.ifwNum) {
            return +1;
        }
        return 0;
    }

    /**
     * get configuration
     *
     * @param l list to add
     * @param beg beginning
     */
    public void routerGetConfig(List<String> l, String beg) {
        l.add(cmds.tabulator + beg + "enable");
        cmds.cfgLine(l, !bfdTrigger, cmds.tabulator, beg + "bfd", "");
        cmds.cfgLine(l, !splitHorizon, cmds.tabulator, beg + "split-horizon", "");
        cmds.cfgLine(l, !defOrigin, cmds.tabulator, beg + "default-originate", "");
        cmds.cfgLine(l, !suppressAddr, cmds.tabulator, beg + "suppress-prefix", "");
        cmds.cfgLine(l, !unsuppressAddr, cmds.tabulator, beg + "unsuppress-prefix", "");
        cmds.cfgLine(l, !connectedCheck, cmds.tabulator, beg + "verify-source", "");
        l.add(cmds.tabulator + beg + "distance " + distance);
        l.add(cmds.tabulator + beg + "metric-in " + metricIn);
        l.add(cmds.tabulator + beg + "metric-out " + metricOut);
        l.add(cmds.tabulator + beg + "packet-timer " + interPackTime);
        l.add(cmds.tabulator + beg + "update-timer " + updateTimer);
        cmds.cfgLine(l, prflstIn == null, cmds.tabulator, beg + "prefix-list-in", "" + prflstIn);
        cmds.cfgLine(l, prflstOut == null, cmds.tabulator, beg + "prefix-list-out", "" + prflstOut);
        cmds.cfgLine(l, roumapIn == null, cmds.tabulator, beg + "route-map-in", "" + roumapIn);
        cmds.cfgLine(l, roumapOut == null, cmds.tabulator, beg + "route-map-out", "" + roumapOut);
        cmds.cfgLine(l, roupolIn == null, cmds.tabulator, beg + "route-policy-in", "" + roupolIn);
        cmds.cfgLine(l, roupolOut == null, cmds.tabulator, beg + "route-policy-out", "" + roupolOut);
        cmds.cfgLine(l, !otherEna, cmds.tabulator, beg + "other-enable", "");
        cmds.cfgLine(l, !othDefOrg, cmds.tabulator, beg + "other-default-originate", "");
        cmds.cfgLine(l, !othSuppAddr, cmds.tabulator, beg + "other-suppress-prefix", "");
        cmds.cfgLine(l, !othUnsuppAddr, cmds.tabulator, beg + "other-unsuppress-prefix", "");
        l.add(cmds.tabulator + beg + "other-distance " + othDist);
        cmds.cfgLine(l, oprflstIn == null, cmds.tabulator, beg + "other-prefix-list-in", "" + oprflstIn);
        cmds.cfgLine(l, oprflstOut == null, cmds.tabulator, beg + "other-prefix-list-out", "" + oprflstOut);
        cmds.cfgLine(l, oroumapIn == null, cmds.tabulator, beg + "other-route-map-in", "" + oroumapIn);
        cmds.cfgLine(l, oroumapOut == null, cmds.tabulator, beg + "other-route-map-out", "" + oroumapOut);
        cmds.cfgLine(l, oroupolIn == null, cmds.tabulator, beg + "other-route-policy-in", "" + oroupolIn);
        cmds.cfgLine(l, oroupolOut == null, cmds.tabulator, beg + "other-route-policy-out", "" + oroupolOut);
    }

    /**
     * do configuration
     *
     * @param a command
     * @param cmd parameters
     */
    public void routerDoConfig(String a, cmds cmd) {
        if (a.equals("bfd")) {
            bfdTrigger = true;
            return;
        }
        if (a.equals("default-originate")) {
            defOrigin = true;
            return;
        }
        if (a.equals("suppress-prefix")) {
            suppressAddr = true;
            return;
        }
        if (a.equals("unsuppress-prefix")) {
            unsuppressAddr = true;
            return;
        }
        if (a.equals("verify-source")) {
            connectedCheck = true;
            return;
        }
        if (a.equals("split-horizon")) {
            splitHorizon = true;
            return;
        }
        if (a.equals("other-enable")) {
            otherEna = oface != null;
            return;
        }
        if (a.equals("other-default-originate")) {
            othDefOrg = true;
            return;
        }
        if (a.equals("other-suppress-prefix")) {
            othSuppAddr = true;
            return;
        }
        if (a.equals("other-unsuppress-prefix")) {
            othUnsuppAddr = true;
            return;
        }
        if (a.equals("other-distance")) {
            othDist = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("other-prefix-list-in")) {
            cfgPrfxlst ntry = cfgAll.prfxFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such prefix list");
                return;
            }
            oprflstIn = ntry.prflst;
            return;
        }
        if (a.equals("other-prefix-list-out")) {
            cfgPrfxlst ntry = cfgAll.prfxFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such prefix list");
                return;
            }
            oprflstOut = ntry.prflst;
            return;
        }
        if (a.equals("other-route-map-in")) {
            cfgRoump ntry = cfgAll.rtmpFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such route map");
                return;
            }
            oroumapIn = ntry.roumap;
            return;
        }
        if (a.equals("other-route-map-out")) {
            cfgRoump ntry = cfgAll.rtmpFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such route map");
                return;
            }
            oroumapOut = ntry.roumap;
            return;
        }
        if (a.equals("other-route-policy-in")) {
            cfgRouplc ntry = cfgAll.rtplFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such route policy");
                return;
            }
            oroupolIn = ntry.rouplc;
            return;
        }
        if (a.equals("other-route-policy-out")) {
            cfgRouplc ntry = cfgAll.rtplFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such route policy");
                return;
            }
            oroupolOut = ntry.rouplc;
            return;
        }
        if (a.equals("distance")) {
            distance = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("metric-in")) {
            metricIn = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("metric-out")) {
            metricOut = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("packet-timer")) {
            interPackTime = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("update-timer")) {
            updateTimer = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("prefix-list-in")) {
            cfgPrfxlst ntry = cfgAll.prfxFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such prefix list");
                return;
            }
            prflstIn = ntry.prflst;
            return;
        }
        if (a.equals("prefix-list-out")) {
            cfgPrfxlst ntry = cfgAll.prfxFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such prefix list");
                return;
            }
            prflstOut = ntry.prflst;
            return;
        }
        if (a.equals("route-map-in")) {
            cfgRoump ntry = cfgAll.rtmpFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such route map");
                return;
            }
            roumapIn = ntry.roumap;
            return;
        }
        if (a.equals("route-map-out")) {
            cfgRoump ntry = cfgAll.rtmpFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such route map");
                return;
            }
            roumapOut = ntry.roumap;
            return;
        }
        if (a.equals("route-policy-in")) {
            cfgRouplc ntry = cfgAll.rtplFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such route policy");
                return;
            }
            roupolIn = ntry.rouplc;
            return;
        }
        if (a.equals("route-policy-out")) {
            cfgRouplc ntry = cfgAll.rtplFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such route policy");
                return;
            }
            roupolOut = ntry.rouplc;
            return;
        }
        cmd.badCmd();
    }

    /**
     * undo configuration
     *
     * @param a command
     * @param cmd parameters
     */
    public void routerUnConfig(String a, cmds cmd) {
        if (a.equals("bfd")) {
            bfdTrigger = false;
            return;
        }
        if (a.equals("default-originate")) {
            defOrigin = false;
            return;
        }
        if (a.equals("suppress-prefix")) {
            suppressAddr = false;
            return;
        }
        if (a.equals("unsuppress-prefix")) {
            unsuppressAddr = false;
            return;
        }
        if (a.equals("verify-source")) {
            connectedCheck = false;
            return;
        }
        if (a.equals("split-horizon")) {
            splitHorizon = false;
            return;
        }
        if (a.equals("other-enable")) {
            otherEna = false;
            return;
        }
        if (a.equals("other-default-originate")) {
            othDefOrg = false;
            return;
        }
        if (a.equals("other-suppress-prefix")) {
            othSuppAddr = false;
            return;
        }
        if (a.equals("other-unsuppress-prefix")) {
            othUnsuppAddr = false;
            return;
        }
        if (a.equals("other-prefix-list-in")) {
            oprflstIn = null;
            return;
        }
        if (a.equals("other-prefix-list-out")) {
            oprflstOut = null;
            return;
        }
        if (a.equals("other-route-map-in")) {
            oroumapIn = null;
            return;
        }
        if (a.equals("other-route-map-out")) {
            oroumapOut = null;
            return;
        }
        if (a.equals("other-route-policy-in")) {
            oroupolIn = null;
            return;
        }
        if (a.equals("other-route-policy-out")) {
            oroupolOut = null;
            return;
        }
        if (a.equals("prefix-list-in")) {
            prflstIn = null;
            return;
        }
        if (a.equals("prefix-list-out")) {
            prflstOut = null;
            return;
        }
        if (a.equals("route-map-in")) {
            roumapIn = null;
            return;
        }
        if (a.equals("route-map-out")) {
            roumapOut = null;
            return;
        }
        if (a.equals("route-policy-in")) {
            roupolIn = null;
            return;
        }
        if (a.equals("route-policy-out")) {
            roupolOut = null;
            return;
        }
        cmd.badCmd();
    }

    /**
     * get help text
     *
     * @param l list to update
     */
    public static void routerGetHelp(userHelp l) {
        l.add(null, false, 4, new int[]{-1}, "enable", "enable/disable babel updates");
        l.add(null, false, 4, new int[]{-1}, "bfd", "enable bfd triggered down");
        l.add(null, false, 4, new int[]{-1}, "default-originate", "send default route to peer");
        l.add(null, false, 4, new int[]{-1}, "suppress-prefix", "do not advertise interface");
        l.add(null, false, 4, new int[]{-1}, "unsuppress-prefix", "do advertise interface");
        l.add(null, false, 4, new int[]{-1}, "verify-source", "check source address of updates");
        l.add(null, false, 4, new int[]{-1}, "split-horizon", "dont advertise back on rx interface");
        l.add(null, false, 4, new int[]{5}, "distance", "administrative distance of routes");
        l.add(null, false, 5, new int[]{-1}, "<num>", "set administrative distance");
        l.add(null, false, 4, new int[]{5}, "metric-in", "interface incoming metric");
        l.add(null, false, 5, new int[]{-1}, "<num>", "metric");
        l.add(null, false, 4, new int[]{5}, "metric-out", "interface outgoing metric");
        l.add(null, false, 5, new int[]{-1}, "<num>", "metric");
        l.add(null, false, 4, new int[]{5}, "packet-timer", "inter packet gap time");
        l.add(null, false, 5, new int[]{-1}, "<num>", "time in ms");
        l.add(null, false, 4, new int[]{5}, "update-timer", "time between updates");
        l.add(null, false, 5, new int[]{-1}, "<num>", "time in ms");
        l.add(null, false, 4, new int[]{5}, "route-map-in", "process prefixes in ingress updates");
        l.add(null, false, 5, new int[]{-1}, "<name:rm>", "name of route map");
        l.add(null, false, 4, new int[]{5}, "route-map-out", "process prefixes in egress updates");
        l.add(null, false, 5, new int[]{-1}, "<name:rm>", "name of route map");
        l.add(null, false, 4, new int[]{5}, "route-policy-in", "process prefixes in ingress updates");
        l.add(null, false, 5, new int[]{-1}, "<name:rpl>", "name of route policy");
        l.add(null, false, 4, new int[]{5}, "route-policy-out", "process prefixes in egress updates");
        l.add(null, false, 5, new int[]{-1}, "<name:rpl>", "name of route policy");
        l.add(null, false, 4, new int[]{5}, "prefix-list-in", "filter prefixes in ingress updates");
        l.add(null, false, 5, new int[]{-1}, "<name:pl>", "name of prefix list");
        l.add(null, false, 4, new int[]{5}, "prefix-list-out", "filter prefixes in egress updates");
        l.add(null, false, 5, new int[]{-1}, "<name:pl>", "name of prefix list");
        l.add(null, false, 4, new int[]{-1}, "other-enable", "enable other protocol processing");
        l.add(null, false, 4, new int[]{-1}, "other-default-originate", "send other default route to peer");
        l.add(null, false, 4, new int[]{-1}, "other-suppress-prefix", "do not advertise other interface");
        l.add(null, false, 4, new int[]{-1}, "other-unsuppress-prefix", "do advertise other interface");
        l.add(null, false, 4, new int[]{5}, "other-distance", "administrative distance of other routes");
        l.add(null, false, 5, new int[]{-1}, "<num>", "set administrative distance");
        l.add(null, false, 4, new int[]{5}, "other-metric-in", "other interface incoming metric");
        l.add(null, false, 5, new int[]{-1}, "<num>", "metric");
        l.add(null, false, 4, new int[]{5}, "other-metric-out", "other interface outgoing metric");
        l.add(null, false, 5, new int[]{-1}, "<num>", "metric");
        l.add(null, false, 4, new int[]{5}, "other-route-map-in", "process other prefixes in ingress updates");
        l.add(null, false, 5, new int[]{-1}, "<name:rm>", "name of route map");
        l.add(null, false, 4, new int[]{5}, "other-route-map-out", "process other prefixes in egress updates");
        l.add(null, false, 5, new int[]{-1}, "<name:rm>", "name of route map");
        l.add(null, false, 4, new int[]{5}, "other-route-policy-in", "process other prefixes in ingress updates");
        l.add(null, false, 5, new int[]{-1}, "<name:rpl>", "name of route policy");
        l.add(null, false, 4, new int[]{5}, "other-route-policy-out", "process other prefixes in egress updates");
        l.add(null, false, 5, new int[]{-1}, "<name:rpl>", "name of route policy");
        l.add(null, false, 4, new int[]{5}, "other-prefix-list-in", "filter other prefixes in ingress updates");
        l.add(null, false, 5, new int[]{-1}, "<name:pl>", "name of prefix list");
        l.add(null, false, 4, new int[]{5}, "other-prefix-list-out", "filter other prefixes in egress updates");
        l.add(null, false, 5, new int[]{-1}, "<name:pl>", "name of prefix list");
    }

    private void createBabelHeader(packHolder pck) {
        pck.merge2beg();
        pck.putByte(0, rtrBabel.magic); // magic
        pck.putByte(1, rtrBabel.version); // version
        pck.msbPutW(2, pck.dataSize()); // length
        pck.putSkip(rtrBabel.size); // size of header
        pck.merge2beg();
    }

    private void createBabelHello(packHolder pck) {
        encTlv tlv = rtrBabel.getTlv();
        bits.msbPutW(tlv.valDat, 0, 0); // reserved
        bits.msbPutW(tlv.valDat, 2, seqno); // sequence number
        bits.msbPutW(tlv.valDat, 4, updateTimer / 10); // interval
        tlv.putBytes(pck, rtrBabel.tlvHello, 6, tlv.valDat);
    }

    private void createBabelIhu(rtrBabelNeigh ntry, packHolder pck) {
        encTlv tlv = rtrBabel.getTlv();
        bits.putByte(tlv.valDat, 1, 0); // reserved
        bits.msbPutW(tlv.valDat, 2, metricIn); // rxcost
        bits.msbPutW(tlv.valDat, 4, updateTimer / 10); // interval
        int i;
        if (ntry.conn.peerAddr.isIPv4()) {
            bits.putByte(tlv.valDat, 0, 1); // ipv4
            ntry.conn.peerAddr.toIPv4().toBuffer(tlv.valDat, 6); // address
            i = 10;
        } else {
            addrIPv6 a6 = ntry.conn.peerAddr.toIPv6();
            if (a6.isLinkLocal()) {
                bits.putByte(tlv.valDat, 0, 3); // ipv6ll
                addrEui ae = new addrEui();
                ae.fromIPv6(a6);
                ae.toBuffer(tlv.valDat, 6); // address
                i = 14;
            } else {
                bits.putByte(tlv.valDat, 0, 2); // ipv6
                a6.toBuffer(tlv.valDat, 6); // address
                i = 22;
            }
        }
        tlv.putBytes(pck, rtrBabel.tlvIhu, i, tlv.valDat);
    }

    private void createBabelNhop(addrIP adr, packHolder pck) {
        encTlv tlv = rtrBabel.getTlv();
        bits.putByte(tlv.valDat, 1, 0); // reserved
        int i;
        if (adr.isIPv4()) {
            bits.putByte(tlv.valDat, 0, 1); // ipv4
            adr.toIPv4().toBuffer(tlv.valDat, 2); // address
            i = 6;
        } else {
            addrIPv6 a6 = adr.toIPv6();
            if (a6.isLinkLocal()) {
                bits.putByte(tlv.valDat, 0, 3); // ipv6ll
                addrEui ae = new addrEui();
                ae.fromIPv6(a6);
                ae.toBuffer(tlv.valDat, 2); // address
                i = 10;
            } else {
                bits.putByte(tlv.valDat, 0, 2); // ipv6
                a6.toBuffer(tlv.valDat, 2); // address
                i = 18;
            }
        }
        tlv.putBytes(pck, rtrBabel.tlvNxtHop, i, tlv.valDat);
    }

    private void createBabelUpdate(tabRouteEntry<addrIP> ntry, addrEui last, packHolder pck) {
        if ((ntry.best.metric + metricOut) >= 0xffff) {
            return;
        }
        encTlv tlv = rtrBabel.getTlv();
        if (ntry.best.aggrRtr == null) {
            ntry.best.aggrRtr = new addrIP();
            ntry.best.aggrRtr.fromIPv6addr(lower.routerID.toIPv6(null));
            ntry.best.aggrAs = lower.seqno;
        }
        addrEui ae = new addrEui();
        ae.fromIPv6(ntry.best.aggrRtr.toIPv6());
        if (ae.compareTo(last) != 0) {
            bits.msbPutW(tlv.valDat, 0, 0); // reserved
            ae.toBuffer(tlv.valDat, 2); // address
            tlv.putBytes(pck, rtrBabel.tlvRtrId, 10, tlv.valDat);
            last.setAddr(ae);
        }
        bits.putByte(tlv.valDat, 1, 0); // flags
        bits.putByte(tlv.valDat, 3, 0); // omitted
        bits.msbPutW(tlv.valDat, 4, updateTimer / 10); // interval
        bits.msbPutW(tlv.valDat, 6, ntry.best.aggrAs); // seqno
        bits.msbPutW(tlv.valDat, 8, ntry.best.metric + metricOut); // metric
        int i;
        if (ntry.prefix.network.isIPv4()) {
            bits.putByte(tlv.valDat, 0, 1); // ipv4
            addrPrefix<addrIPv4> a4 = addrPrefix.ip2ip4(ntry.prefix);
            a4.network.toBuffer(tlv.valDat, 10); // address
            i = a4.maskLen;
        } else {
            bits.putByte(tlv.valDat, 0, 2); // ipv6
            addrPrefix<addrIPv6> a6 = addrPrefix.ip2ip6(ntry.prefix);
            a6.network.toBuffer(tlv.valDat, 10); // address
            i = a6.maskLen;
        }
        bits.putByte(tlv.valDat, 2, i); // prefix length
        i = (i + 7) / 8;
        tlv.putBytes(pck, rtrBabel.tlvUpdate, 10 + i, tlv.valDat);
        if (debugger.rtrBabelTraf) {
            logger.debug("txnet " + ntry);
        }
    }

    /**
     * send one update over a connection
     *
     * @param conn connection to use
     */
    protected void sendOutUpdates(prtGenConn conn) {
        seqno = (seqno + 1) & 0xffff;
        packHolder pck = new packHolder(true, true);
        tabRoute<addrIP> tab1 = new tabRoute<addrIP>("copy");
        if (defOrigin) {
            if (iface.addr.isIPv4()) {
                tab1.add(tabRoute.addType.better, addrPrefix.ip4toIP(addrPrefix.defaultRoute4()), new addrIP());
            } else {
                tab1.add(tabRoute.addType.better, addrPrefix.ip6toIP(addrPrefix.defaultRoute6()), new addrIP());
            }
        }
        tab1.mergeFrom(tabRoute.addType.better, lower.routerComputedU, tabRouteAttr.distanLim);
        if (splitHorizon) {
            tab1.delIface(conn.iface);
        }
        tab1.mergeFrom(tabRoute.addType.better, lower.routerRedistedU, tabRouteAttr.distanLim);
        tabRoute<addrIP> tab2 = new tabRoute<addrIP>("copy");
        tabRoute.addUpdatedTable(tabRoute.addType.better, rtrBgpUtil.sfiUnicast, 0, tab2, tab1, true, roumapOut, roupolOut, prflstOut);
        advert = tab2;
        tab1 = new tabRoute<addrIP>("copy");
        if (otherEna) {
            if (othDefOrg) {
                if (oface.addr.isIPv4()) {
                    tab1.add(tabRoute.addType.better, addrPrefix.ip4toIP(addrPrefix.defaultRoute4()), new addrIP());
                } else {
                    tab1.add(tabRoute.addType.better, addrPrefix.ip6toIP(addrPrefix.defaultRoute6()), new addrIP());
                }
            }
            tab1.mergeFrom(tabRoute.addType.better, lower.other.routerComputedU, tabRouteAttr.distanLim);
            if (splitHorizon) {
                tab1.delIface(oface);
            }
            tab1.mergeFrom(tabRoute.addType.better, lower.other.routerRedistedU, tabRouteAttr.distanLim);
            tab2 = new tabRoute<addrIP>("copy");
            tabRoute.addUpdatedTable(tabRoute.addType.better, rtrBgpUtil.sfiUnicast, 0, tab2, tab1, true, oroumapOut, oroupolOut, oprflstOut);
            oadvert = tab2;
        }
        if (debugger.rtrBabelTraf) {
            logger.debug("tx " + conn);
        }
        createBabelHello(pck);
        int entries = 1;
        for (int i = 0; i < lower.neighs.size(); i++) {
            rtrBabelNeigh ntry = lower.neighs.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.iface.conn.iface.ifwNum != conn.iface.ifwNum) {
                continue;
            }
            createBabelIhu(ntry, pck);
            entries++;
            if (pck.headSize() < 512) {
                continue;
            }
            createBabelHeader(pck);
            conn.send2net(pck);
            pck.clear();
            entries = 0;
            bits.sleep(interPackTime);
        }
        addrEui last = new addrEui();
        for (int i = 0; i < advert.size(); i++) {
            tabRouteEntry<addrIP> ntry = advert.get(i);
            if (ntry == null) {
                continue;
            }
            createBabelUpdate(ntry, last, pck);
            entries++;
            if (pck.headSize() < 512) {
                continue;
            }
            createBabelHeader(pck);
            conn.send2net(pck);
            pck.clear();
            last = new addrEui();
            entries = 0;
            bits.sleep(interPackTime);
        }
        if (otherEna) {
            createBabelNhop(oface.addr, pck);
        }
        for (int i = 0; i < oadvert.size(); i++) {
            tabRouteEntry<addrIP> ntry = oadvert.get(i);
            if (ntry == null) {
                continue;
            }
            createBabelUpdate(ntry, last, pck);
            entries++;
            if (pck.headSize() < 512) {
                continue;
            }
            createBabelHeader(pck);
            conn.send2net(pck);
            pck.clear();
            createBabelNhop(oface.addr, pck);
            last = new addrEui();
            entries = 0;
            bits.sleep(interPackTime);
        }
        if (entries > 0) {
            createBabelHeader(pck);
            conn.send2net(pck);
        }
    }

    /**
     * do some work round
     */
    public synchronized void doWork() {
        if (conn == null) {
            return;
        }
        conn.workInterval = updateTimer;
        conn.timeout = 0;
        sendOutUpdates(conn);
    }

}
