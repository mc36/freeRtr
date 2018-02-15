package rtr;

import addr.addrEui;
import addr.addrIP;
import addr.addrIPv4;
import addr.addrIPv6;
import addr.addrPrefix;
import cfg.cfgAll;
import cfg.cfgPrfxlst;
import cfg.cfgRoump;
import cfg.cfgRouplc;
import ip.ipFwdIface;
import java.util.Comparator;
import java.util.List;
import pack.packHolder;
import prt.prtGenConn;
import tab.tabListing;
import tab.tabPrfxlstN;
import tab.tabRoute;
import tab.tabRouteEntry;
import tab.tabRtrmapN;
import tab.tabRtrplcN;
import user.userHelping;
import util.bits;
import util.cmds;
import util.debugger;
import util.logger;
import util.typLenVal;

/**
 * babel2 interface
 *
 * @author matecsaba
 */
public class rtrBabelIface implements Comparator<rtrBabelIface> {

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
     * the interface this works on
     */
    protected final ipFwdIface iface;

    /**
     * the udp connection it uses to multicast
     */
    protected prtGenConn conn;

    /**
     * the lower layer
     */
    protected rtrBabel lower;

    /**
     * sequence number
     */
    protected int seqno;

    /**
     * prefixes advertised on interface
     */
    public tabRoute<addrIP> advert;

    /**
     * create one instance
     *
     * @param parent the babel protocol
     * @param ifc the ip interface to work on
     */
    public rtrBabelIface(rtrBabel parent, ipFwdIface ifc) {
        lower = parent;
        iface = ifc;
        seqno = bits.randomW();
        advert = new tabRoute<addrIP>("babel");
    }

    /**
     * unregister from udp
     */
    public void unregister2udp() {
        lower.udpCore.listenStop(iface, rtrBabel.port, null, 0, 0);
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
        lower.udpCore.packetListen(lower, iface, rtrBabel.port, null, 0, 0, "babel", null, -1);
        conn = lower.udpCore.packetConnect(lower, iface, rtrBabel.port, adr, rtrBabel.port, "babel", null, -1);
        if (conn == null) {
            return;
        }
        conn.timeout = 0;
    }

    public String toString() {
        return "babel on " + iface;
    }

    public int compare(rtrBabelIface o1, rtrBabelIface o2) {
        if (o1.iface.ifwNum < o2.iface.ifwNum) {
            return -1;
        }
        if (o1.iface.ifwNum > o2.iface.ifwNum) {
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
        if (a.equals("split-horizon")) {
            splitHorizon = true;
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
        if (a.equals("split-horizon")) {
            splitHorizon = false;
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
    public static void routerGetHelp(userHelping l) {
        l.add("4 .         enable                  enable/disable babel updates");
        l.add("4 .         bfd                     enable bfd triggered down");
        l.add("4 .         default-originate       send default route to peer");
        l.add("4 .         suppress-prefix         do not advertise interface");
        l.add("4 .         split-horizon           dont advertise back on rx interface");
        l.add("4 5         distance                administrative distance of routes");
        l.add("5 .           <num>                 set administrative distance");
        l.add("4 5         metric-in               interface incoming metric");
        l.add("5 .           <num>                 metric");
        l.add("4 5         metric-out              interface outgoing metric");
        l.add("5 .           <num>                 metric");
        l.add("4 5         packet-timer            inter packet gap time");
        l.add("5 .           <num>                 time in ms");
        l.add("4 5         update-timer            time between updates");
        l.add("5 .           <num>                 time in ms");
        l.add("4 5         route-map-in            process prefixes in ingress updates");
        l.add("5 .           <name>                name of route map");
        l.add("4 5         route-map-out           process prefixes in egress updates");
        l.add("5 .           <name>                name of route map");
        l.add("4 5         route-policy-in         process prefixes in ingress updates");
        l.add("5 .           <name>                name of route policy");
        l.add("4 5         route-policy-out        process prefixes in egress updates");
        l.add("5 .           <name>                name of route policy");
        l.add("4 5         prefix-list-in          filter prefixes in ingress updates");
        l.add("5 .           <name>                name of prefix list");
        l.add("4 5         prefix-list-out         filter prefixes in egress updates");
        l.add("5 .           <name>                name of prefix list");
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
        typLenVal tlv = rtrBabel.getTlv();
        bits.msbPutW(tlv.valDat, 0, 0); // reserved
        bits.msbPutW(tlv.valDat, 2, seqno); // sequence number
        bits.msbPutW(tlv.valDat, 4, updateTimer / 10); // interval
        tlv.putBytes(pck, rtrBabel.tlvHello, 6, tlv.valDat);
    }

    private void createBabelIhu(rtrBabelNeigh ntry, packHolder pck) {
        typLenVal tlv = rtrBabel.getTlv();
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

    private void createBabelUpdate(tabRouteEntry<addrIP> ntry, addrEui last, packHolder pck) {
        typLenVal tlv = rtrBabel.getTlv();
        if (ntry.aggrRtr == null) {
            ntry.aggrRtr = new addrIP();
            ntry.aggrRtr.fromIPv6addr(lower.routerID.toIPv6());
            ntry.aggrAs = lower.seqno;
        }
        addrEui ae = new addrEui();
        ae.fromIPv6(ntry.aggrRtr.toIPv6());
        if (ae.compare(ae, last) != 0) {
            bits.msbPutW(tlv.valDat, 0, 0); // reserved
            ae.toBuffer(tlv.valDat, 2); // address
            tlv.putBytes(pck, rtrBabel.tlvRtrId, 10, tlv.valDat);
            last.setAddr(ae);
        }
        bits.putByte(tlv.valDat, 1, 0); // flags
        bits.putByte(tlv.valDat, 3, 0); // omitted
        bits.msbPutW(tlv.valDat, 4, updateTimer / 10); // interval
        bits.msbPutW(tlv.valDat, 6, ntry.aggrAs); // seqno
        bits.msbPutW(tlv.valDat, 8, ntry.metric + metricOut); // metric
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
        tab1.mergeFrom(tabRoute.addType.better, lower.routerComputedU, null, true, tabRouteEntry.distanLim);
        if (splitHorizon) {
            tab1.delIface(conn.iface);
        }
        tab1.mergeFrom(tabRoute.addType.better, lower.routerRedistedU, null, true, tabRouteEntry.distanLim);
        tabRoute<addrIP> tab2 = new tabRoute<addrIP>("copy");
        tabRoute.addUpdatedTable(tabRoute.addType.better, rtrBgpUtil.safiUnicast, tab2, tab1, roumapOut, roupolOut, prflstOut);
        advert = tab2;
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
        for (int i = 0; i < tab2.size(); i++) {
            tabRouteEntry<addrIP> ntry = tab2.get(i);
            if (ntry == null) {
                continue;
            }
            if ((ntry.metric + metricOut) >= 0xffff) {
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
