package net.freertr.rtr;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrIPv4;
import net.freertr.addr.addrIPv6;
import net.freertr.addr.addrMac;
import net.freertr.addr.addrPrefix;
import net.freertr.cfg.cfgBrdg;
import net.freertr.cfg.cfgIfc;
import net.freertr.clnt.clntSrEth;
import net.freertr.clnt.clntVxlan;
import net.freertr.ifc.ifcBridgeRtr;
import net.freertr.ifc.ifcDot1ah;
import net.freertr.ifc.ifcEther;
import net.freertr.ip.ipMpls;
import net.freertr.pack.packHolder;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabLabel;
import net.freertr.tab.tabLabelEntry;
import net.freertr.tab.tabRoute;
import net.freertr.tab.tabRouteEntry;
import net.freertr.tab.tabRtrmapN;
import net.freertr.util.bits;
import net.freertr.util.debugger;
import net.freertr.util.logger;

/**
 * bgp4 evpn
 *
 * @author matecsaba
 */
public class rtrBgpEvpn implements ifcBridgeRtr, Comparator<rtrBgpEvpn> {

    /**
     * encapsulation type
     */
    public enum encapType {

        /**
         * pbb
         */
        pbb,
        /**
         * vxlan
         */
        vxlan,
        /**
         * cmac
         */
        cmac,
        /**
         * vpws
         */
        vpws
    }

    /**
     * id number
     */
    public int id;

    /**
     * encapsulation
     */
    public encapType encap;

    /**
     * bridge to use
     */
    public cfgBrdg bridge;

    /**
     * backbone mac
     */
    public addrMac bbmac;

    /**
     * broadcast mac
     */
    public addrMac bcmac;

    /**
     * source interface
     */
    public cfgIfc iface;

    /**
     * srv6 interface
     */
    public cfgIfc srv6;

    /**
     * set true if advertised
     */
    public boolean adverted;

    /**
     * peers
     */
    protected tabGen<rtrBgpEvpnPeer> peers = new tabGen<rtrBgpEvpnPeer>();

    /**
     * label
     */
    protected tabLabelEntry label;

    /**
     * cmac receiver
     */
    protected rtrBgpEvpnCmac cmacr;

    /**
     * vpws receiver
     */
    protected rtrBgpEvpnVpws vpwsr;

    /**
     * upper layer
     */
    protected rtrBgp parent;

    /**
     * create new instance
     *
     * @param p parent to use
     */
    public rtrBgpEvpn(rtrBgp p) {
        parent = p;
    }

    public int compare(rtrBgpEvpn o1, rtrBgpEvpn o2) {
        if (o1.id < o2.id) {
            return -1;
        }
        if (o1.id > o2.id) {
            return +1;
        }
        return 0;
    }

    private rtrBgpEvpnPeer findPeer(addrIP adr) {
        for (int i = 0; i < peers.size(); i++) {
            rtrBgpEvpnPeer ntry = peers.get(i);
            if (adr.compare(adr, ntry.peer) != 0) {
                continue;
            }
            return ntry;
        }
        return null;
    }

    private int convLab(tabLabelEntry label) {
        return (label.label << 4) | 1;
    }

    /**
     * bridge changed
     */
    public void bridgeChanged() {
        if (encap == encapType.pbb) {
            return;
        }
        if (debugger.rtrBgpFull) {
            logger.debug("bridge changed");
        }
        parent.needFull.add(1);
        parent.compute.wakeup();
    }

    /**
     * generate configuration
     *
     * @param l list to append
     * @param beg beginning
     */
    public void getConfig(List<String> l, String beg) {
        beg = beg + "afi-evpn " + id + " ";
        l.add(beg + "bridge-group " + bridge.name);
        if (srv6 != null) {
            l.add(beg + "srv6 " + srv6.name);
        }
        l.add(beg + "bmac " + bbmac);
        String a = "";
        switch (encap) {
            case pbb:
                a = "pbb";
                break;
            case vxlan:
                a = "vxlan";
                break;
            case cmac:
                a = "cmac";
                break;
            case vpws:
                a = "vpws";
                break;
        }
        l.add(beg + "encapsulation " + a);
        if (iface != null) {
            l.add(beg + "update-source " + iface.name);
        }
    }

    private void putPmsi(tabRouteEntry<addrIP> ntry, int lab) {
        ntry.best.pmsiTyp = 6;
        ntry.best.pmsiLab = lab;
        if (ntry.best.nextHop.isIPv4()) {
            ntry.best.pmsiTun = ntry.best.nextHop.toIPv4().getBytes();
        } else {
            ntry.best.pmsiTun = ntry.best.nextHop.toIPv6().getBytes();
        }
    }

    /**
     * advertise this evpn
     *
     * @param tab table to use
     */
    protected void doAdvertise(tabRoute<addrIP> tab) {
        adverted = false;
        if (id == 0) {
            return;
        }
        if (bridge == null) {
            return;
        }
        if (bridge.bridgeHed.rd == 0) {
            return;
        }
        if (iface == null) {
            return;
        }
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        ntry.prefix = rtrBgpUtil.defaultRoute(parent.afiUni);
        ntry.prefix.maskLen = addrIP.size * 8;
        ntry.prefix.mask.fillBytes(0xff);
        if (ntry.prefix.network.isIPv4()) {
            addrIPv4 adr = iface.addr4;
            if (adr == null) {
                return;
            }
            ntry.best.nextHop = new addrIP();
            ntry.best.nextHop.fromIPv4addr(adr);
        } else {
            addrIPv6 adr = iface.addr6;
            if (adr == null) {
                return;
            }
            ntry.best.nextHop = new addrIP();
            ntry.best.nextHop.fromIPv6addr(adr);
        }
        ntry.rouDst = bridge.bridgeHed.rd;
        ntry.best.extComm = new ArrayList<Long>();
        ntry.best.extComm.add(tabRtrmapN.rt2comm(bridge.bridgeHed.rtExp));
        ntry.best.rouSrc = rtrBgpUtil.peerOriginate;
        byte[] buf = new byte[addrIP.size];
        ntry.prefix.wildcard.fromBuf(buf, 0);
        ntry.prefix.broadcast.fromBuf(buf, 0);
        switch (encap) {
            case pbb:
                buf[0] = 2; // mac advertisement
                bbmac.toBuffer(buf, 10);
                ntry.prefix.network.fromBuf(buf, 0);
                if (!ipMpls.putSrv6prefix(ntry, srv6, parent.evpnUni)) {
                    ntry.best.evpnLab = convLab(ntry.best.labelLoc);
                } else {
                    ntry.best.evpnLab = convLab(parent.evpnUni);
                }
                tab.add(tabRoute.addType.better, ntry, true, true);
                buf = new byte[addrIP.size];
                buf[0] = 3; // inclusive multicast
                bits.msbPutD(buf, 2, id);
                ntry.prefix.network.fromBuf(buf, 0);
                ntry.prefix.broadcast = ntry.best.nextHop.copyBytes();
                if (!ipMpls.putSrv6prefix(ntry, srv6, parent.evpnMul)) {
                    putPmsi(ntry, convLab(ntry.best.labelLoc));
                } else {
                    putPmsi(ntry, convLab(parent.evpnMul));
                }
                tab.add(tabRoute.addType.better, ntry, true, true);
                adverted = true;
                break;
            case vxlan:
                buf[0] = 2; // mac advertisement
                ntry.best.extComm.add(tabRtrmapN.tuntyp2comm(8));
                ntry.best.evpnLab = id;
                List<addrMac> cmac = bridge.bridgeHed.getMacList();
                for (int i = 0; i < cmac.size(); i++) {
                    cmac.get(i).toBuffer(buf, 10);
                    ntry.prefix.network.fromBuf(buf, 0);
                    tab.add(tabRoute.addType.better, ntry, true, true);
                }
                buf = new byte[addrIP.size];
                buf[0] = 3; // inclusive multicast
                ntry.prefix.network.fromBuf(buf, 0);
                ntry.prefix.broadcast = ntry.best.nextHop.copyBytes();
                putPmsi(ntry, id);
                tab.add(tabRoute.addType.better, ntry, true, true);
                adverted = true;
                break;
            case cmac:
                if (label == null) {
                    label = tabLabel.allocate(11);
                    if (label == null) {
                        break;
                    }
                    cmacr = new rtrBgpEvpnCmac(this);
                    label.setFwdPwe(11, parent.fwdCore, cmacr, 0, null);
                }
                buf[0] = 2; // mac advertisement
                if (!ipMpls.putSrv6prefix(ntry, srv6, label)) {
                    ntry.best.evpnLab = convLab(ntry.best.labelLoc);
                } else {
                    ntry.best.evpnLab = convLab(label);
                }
                bits.msbPutD(buf, 2, id);
                cmac = bridge.bridgeHed.getMacList();
                for (int i = 0; i < cmac.size(); i++) {
                    cmac.get(i).toBuffer(buf, 10);
                    ntry.prefix.network.fromBuf(buf, 0);
                    tab.add(tabRoute.addType.better, ntry, true, true);
                }
                buf = new byte[addrIP.size];
                buf[0] = 3; // inclusive multicast
                bits.msbPutD(buf, 2, id);
                ntry.prefix.network.fromBuf(buf, 0);
                ntry.prefix.broadcast = ntry.best.nextHop.copyBytes();
                if (!ipMpls.putSrv6prefix(ntry, srv6, label)) {
                    putPmsi(ntry, convLab(ntry.best.labelLoc));
                } else {
                    putPmsi(ntry, convLab(label));
                }
                tab.add(tabRoute.addType.better, ntry, true, true);
                adverted = true;
                break;
            case vpws:
                if (label == null) {
                    label = tabLabel.allocate(11);
                    if (label == null) {
                        break;
                    }
                    vpwsr = new rtrBgpEvpnVpws(this);
                    label.setFwdPwe(11, parent.fwdCore, vpwsr, 0, null);
                }
                buf[0] = 1; // eth advertisement
                if (!ipMpls.putSrv6prefix(ntry, srv6, label)) {
                    ntry.best.evpnLab = convLab(ntry.best.labelLoc);
                } else {
                    ntry.best.evpnLab = convLab(label);
                }
                bits.msbPutD(buf, 12, id);
                ntry.prefix.network.fromBuf(buf, 0);
                tab.add(tabRoute.addType.better, ntry, true, true);
                adverted = true;
                break;
        }
    }

    /**
     * read peers in this evpn
     *
     * @param cmp computed routes
     */
    protected void doPeers(tabRoute<addrIP> cmp) {
        for (int i = 0; i < peers.size(); i++) {
            peers.get(i).needed = 0;
        }
        long rt = tabRtrmapN.rt2comm(bridge.bridgeHed.rtImp);
        byte[] buf = new byte[addrIP.size];
        for (int i = 0; i < cmp.size(); i++) {
            tabRouteEntry<addrIP> ntry = cmp.get(i);
            if (ntry.best.rouSrc == rtrBgpUtil.peerOriginate) {
                continue;
            }
            if (ntry.best.extComm == null) {
                continue;
            }
            if (rtrBgpUtil.findLongList(ntry.best.extComm, rt) < 0) {
                continue;
            }
            rtrBgpEvpnPeer per;
            rtrBgpEvpnPeer old = null;
            ntry.prefix.network.toBuffer(buf, 0);
            int eti = bits.msbGetD(buf, 2);
            switch (buf[0]) {
                case 1: // eth advertisement
                    if (encap != encapType.vpws) {
                        continue;
                    }
                    if (eti != 0) {
                        continue;
                    }
                    if (bits.msbGetD(buf, 12) != id) {
                        continue;
                    }
                    per = new rtrBgpEvpnPeer(this);
                    per.bbmac = new addrMac();
                    old = findPeer(ntry.best.nextHop);
                    if (old == null) {
                        peers.add(per);
                    } else {
                        per = old;
                    }
                    per.needed |= 1;
                    per.peer = ntry.best.nextHop.copyBytes();
                    per.labUni = ntry.best.evpnLab >>> 4;
                    if (ntry.best.segrouPrf != null) {
                        per.srv6uni = ntry.best.segrouPrf.copyBytes();
                    }
                    break;
                case 2: // mac advertisement
                    switch (encap) {
                        case pbb:
                            if (eti != 0) {
                                continue;
                            }
                            break;
                        case vxlan:
                            if (eti != 0) {
                                continue;
                            }
                            break;
                        case cmac:
                            if (eti != id) {
                                continue;
                            }
                            break;
                        case vpws:
                            continue;
                    }
                    per = new rtrBgpEvpnPeer(this);
                    per.bbmac = new addrMac();
                    per.bbmac.fromBuf(buf, 10);
                    switch (encap) {
                        case pbb:
                            old = peers.find(per);
                            break;
                        case vxlan:
                            old = findPeer(ntry.best.nextHop);
                            break;
                        case cmac:
                            old = peers.find(per);
                            break;
                        case vpws:
                            continue;
                    }
                    if (old == null) {
                        peers.add(per);
                    } else {
                        per = old;
                    }
                    per.needed |= 1;
                    per.peer = ntry.best.nextHop.copyBytes();
                    per.labUni = ntry.best.evpnLab >>> 4;
                    if (ntry.best.segrouPrf != null) {
                        per.srv6uni = ntry.best.segrouPrf.copyBytes();
                    }
                    break;
                case 3: // inclusive multicast
                    if (ntry.best.pmsiTun == null) {
                        continue;
                    }
                    if (ntry.best.pmsiTyp != 6) {
                        continue;
                    }
                    switch (encap) {
                        case pbb:
                            if (eti != id) {
                                continue;
                            }
                            break;
                        case vxlan:
                            if (eti != 0) {
                                continue;
                            }
                            break;
                        case cmac:
                            if (eti != id) {
                                continue;
                            }
                            break;
                        case vpws:
                            continue;
                    }
                    old = findPeer(ntry.best.nextHop);
                    if (old == null) {
                        per = new rtrBgpEvpnPeer(this);
                        per.bbmac = addrMac.getRandom();
                        per.peer = ntry.best.nextHop.copyBytes();
                        peers.add(per);
                    } else {
                        per = old;
                    }
                    per.needed |= 2;
                    per.labMul = ntry.best.pmsiLab >>> 4;
                    if (ntry.best.segrouPrf != null) {
                        per.srv6mul = ntry.best.segrouPrf.copyBytes();
                    }
                    break;
            }
        }
        for (int i = peers.size() - 1; i >= 0; i--) {
            rtrBgpEvpnPeer ntry = peers.get(i);
            boolean ned = false;
            switch (encap) {
                case pbb:
                    ned = ntry.needed == 3;
                    break;
                case vxlan:
                    ned = ntry.needed != 0;
                    break;
                case cmac:
                    ned = ntry.needed != 0;
                    break;
                case vpws:
                    ned = ntry.needed != 0;
                    break;
            }
            if (!ned) {
                peers.del(ntry);
                if (ntry.brdg == null) {
                    continue;
                }
                if (debugger.rtrBgpEvnt) {
                    logger.debug("stop " + ntry);
                }
                ntry.doStop();
                continue;
            }
            if (ntry.brdg != null) {
                continue;
            }
            if (debugger.rtrBgpEvnt) {
                logger.debug("start " + ntry);
            }
            switch (encap) {
                case pbb:
                    ntry.brdg = bridge.bridgeHed.newIface(false, false, false);
                    ntry.setUpper(ntry.brdg);
                    break;
                case vxlan:
                    ntry.vxlan = new clntVxlan();
                    ntry.vxlan.target = "" + ntry.peer;
                    ntry.vxlan.vrf = parent.vrfCore;
                    ntry.vxlan.srcIfc = iface;
                    ntry.vxlan.inst = id;
                    ntry.vxlan.wildcard = true;
                    ntry.brdg = bridge.bridgeHed.newIface(false, true, false);
                    ntry.vxlan.setUpper(ntry.brdg);
                    ntry.vxlan.workStart();
                    break;
                case cmac:
                    ntry.brdg = bridge.bridgeHed.newIface(false, false, false);
                    ntry.setUpper(ntry.brdg);
                    break;
                case vpws:
                    ntry.brdg = bridge.bridgeHed.newIface(false, false, false);
                    ntry.setUpper(ntry.brdg);
                    break;
            }
        }
    }

    /**
     * stop this evpn
     */
    public void doStop() {
        if (debugger.rtrBgpEvnt) {
            logger.debug("stop " + id);
        }
        bridge.bridgeHed.macRouter = null;
        if (label != null) {
            tabLabel.release(label, 11);
        }
        for (int i = 0; i < peers.size(); i++) {
            rtrBgpEvpnPeer ntry = peers.get(i);
            ntry.doStop();
        }
    }

    /**
     * send one packet
     *
     * @param per peer to use
     * @param pck packet to send
     */
    protected void sendPack(rtrBgpEvpnPeer per, packHolder pck) {
        switch (encap) {
            case pbb:
                pck.ETHvlan = id;
                new ifcDot1ah().createHeader(pck);
                pck.ETHsrc.setAddr(bbmac);
                if (pck.ETHtrg.isFloodable()) {
                    pck.ETHtrg.setAddr(bcmac);
                } else {
                    pck.ETHtrg.setAddr(per.bbmac);
                }
                ifcEther.createETHheader(pck, false);
                pck.msbPutD(0, 0);
                pck.putSkip(4);
                pck.merge2beg();
                doSendPack(per, pck);
                break;
            case vxlan:
                break;
            case cmac:
                ifcEther.createETHheader(pck, false);
                doSendPack(per, pck);
                break;
            case vpws:
                ifcEther.createETHheader(pck, false);
                doSendPack(per, pck);
                break;
        }
    }

    private void doSendPack(rtrBgpEvpnPeer per, packHolder pck) {
        boolean flood = pck.ETHtrg.isFloodable();
        if (encap == encapType.vpws) {
            flood = false;
        }
        addrIP srv;
        if (flood) {
            pck.MPLSlabel = per.labMul;
            srv = per.srv6mul;
        } else {
            pck.MPLSlabel = per.labUni;
            srv = per.srv6uni;
        }
        if (pck.MPLSlabel == 0) {
            return;
        }
        if (srv == null) {
            ipMpls.beginMPLSfields(pck, false);
            ipMpls.createMPLSheader(pck);
            parent.fwdCore.mplsTxPack(per.peer, pck, false);
            return;
        }
        pck.putDefaults();
        pck.IPtrg.setAddr(srv);
        pck.IPsrc.setAddr(srv);
        pck.IPprt = clntSrEth.prot;
        ipMpls.beginMPLSfields(pck, false);
        parent.vrfCore.fwd6.protoPack(iface.fwdIf6, null, pck);
    }

    /**
     * get peer list
     *
     * @param tab list to append
     */
    public void getPeerList(tabRoute<addrIP> tab) {
        for (int i = 0; i < peers.size(); i++) {
            rtrBgpEvpnPeer nei = peers.get(i);
            if (nei == null) {
                continue;
            }
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = new addrPrefix<addrIP>(nei.peer, addrIP.size * 8);
            tabRoute.addUpdatedEntry(tabRoute.addType.better, tab, rtrBgpUtil.sfiUnicast, 0, ntry, true, null, null, parent.routerAutoMesh);
        }
    }

}
