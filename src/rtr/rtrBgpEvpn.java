package rtr;

import addr.addrIP;
import addr.addrIPv4;
import addr.addrIPv6;
import addr.addrMac;
import addr.addrPrefix;
import cfg.cfgBrdg;
import cfg.cfgIfc;
import clnt.clntVxlan;
import ifc.ifcBridgeRtr;
import ifc.ifcDot1ah;
import ifc.ifcEther;
import ip.ipMpls;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import pack.packHolder;
import tab.tabGen;
import tab.tabLabel;
import tab.tabLabelNtry;
import tab.tabRoute;
import tab.tabRouteEntry;
import tab.tabRtrmapN;
import util.bits;
import util.debugger;
import util.logger;

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
        cmac
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
    protected tabLabelNtry label;

    /**
     * cmac receiver
     */
    protected rtrBgpEvpnCmac cmacr;

    private rtrBgp parent;

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

    private int convLab(tabLabelNtry label) {
        return (label.getValue() << 4) | 1;
    }

    /**
     * bridge changed
     */
    public void bridgeChanged() {
        if (encap == encapType.pbb) {
            return;
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
        }
        l.add(beg + "encapsulation " + a);
        if (iface != null) {
            l.add(beg + "update-source " + iface.name);
        }
    }

    private void putPmsi(tabRouteEntry<addrIP> ntry, int lab) {
        ntry.pmsiTyp = 6;
        ntry.pmsiLab = lab;
        if (ntry.nextHop.isIPv4()) {
            ntry.pmsiTun = ntry.nextHop.toIPv4().getBytes();
        } else {
            ntry.pmsiTun = ntry.nextHop.toIPv6().getBytes();
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
        if (bridge.bridgeHed.rd == 0) {
            return;
        }
        if (bridge == null) {
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
            ntry.nextHop = new addrIP();
            ntry.nextHop.fromIPv4addr(adr);
        } else {
            addrIPv6 adr = iface.addr6;
            if (adr == null) {
                return;
            }
            ntry.nextHop = new addrIP();
            ntry.nextHop.fromIPv6addr(adr);
        }
        ntry.rouDst = bridge.bridgeHed.rd;
        ntry.extComm = new ArrayList<Long>();
        ntry.extComm.add(tabRtrmapN.rt2comm(bridge.bridgeHed.rtExp));
        ntry.rouSrc = rtrBgpUtil.peerOriginate;
        byte[] buf = new byte[addrIP.size];
        ntry.prefix.wildcard.fromBuf(buf, 0);
        ntry.prefix.broadcast.fromBuf(buf, 0);
        switch (encap) {
            case pbb:
                buf[0] = 2; // mac advertisement
                bbmac.toBuffer(buf, 10);
                ntry.prefix.network.fromBuf(buf, 0);
                ntry.evpnLab = convLab(parent.evpnUni);
                tab.add(tabRoute.addType.better, ntry, true, true);
                buf = new byte[addrIP.size];
                buf[0] = 3; // inclusive multicast
                bits.msbPutD(buf, 2, id);
                ntry.prefix.network.fromBuf(buf, 0);
                ntry.prefix.broadcast = ntry.nextHop.copyBytes();
                putPmsi(ntry, convLab(parent.evpnMul));
                tab.add(tabRoute.addType.better, ntry, true, true);
                adverted = true;
                break;
            case vxlan:
                buf[0] = 2; // mac advertisement
                ntry.extComm.add(tabRtrmapN.tuntyp2comm(8));
                ntry.evpnLab = id;
                List<addrMac> cmac = bridge.bridgeHed.getMacList();
                for (int i = 0; i < cmac.size(); i++) {
                    cmac.get(i).toBuffer(buf, 10);
                    ntry.prefix.network.fromBuf(buf, 0);
                    tab.add(tabRoute.addType.better, ntry, true, true);
                }
                buf = new byte[addrIP.size];
                buf[0] = 3; // inclusive multicast
                ntry.prefix.network.fromBuf(buf, 0);
                ntry.prefix.broadcast = ntry.nextHop.copyBytes();
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
                ntry.evpnLab = convLab(label);
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
                ntry.prefix.broadcast = ntry.nextHop.copyBytes();
                putPmsi(ntry, convLab(label));
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
            if (ntry.rouSrc == rtrBgpUtil.peerOriginate) {
                continue;
            }
            if (ntry.extComm == null) {
                continue;
            }
            if (rtrBgpUtil.findLongList(ntry.extComm, rt) < 0) {
                continue;
            }
            rtrBgpEvpnPeer per;
            ntry.prefix.network.toBuffer(buf, 0);
            int eti = bits.msbGetD(buf, 2);
            switch (buf[0]) {
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
                    }
                    per = new rtrBgpEvpnPeer(this);
                    per.bbmac = new addrMac();
                    per.bbmac.fromBuf(buf, 10);
                    rtrBgpEvpnPeer old = null;
                    switch (encap) {
                        case pbb:
                            old = peers.find(per);
                            break;
                        case vxlan:
                            old = findPeer(ntry.nextHop);
                            break;
                        case cmac:
                            old = peers.find(per);
                            break;
                    }
                    if (old == null) {
                        peers.add(per);
                    } else {
                        per = old;
                    }
                    per.needed |= 1;
                    per.peer = ntry.nextHop.copyBytes();
                    per.labUni = ntry.evpnLab >>> 4;
                    break;
                case 3: // inclusive multicast
                    if (ntry.pmsiTun == null) {
                        continue;
                    }
                    if (ntry.pmsiTyp != 6) {
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
                    }
                    old = findPeer(ntry.nextHop);
                    if (old == null) {
                        per = new rtrBgpEvpnPeer(this);
                        per.bbmac = addrMac.getRandom();
                        per.peer = ntry.nextHop.copyBytes();
                        peers.add(per);
                    } else {
                        per = old;
                    }
                    per.needed |= 2;
                    per.labMul = ntry.pmsiLab >>> 4;
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
                ifcDot1ah.createHeader(pck);
                pck.ETHsrc.setAddr(bbmac);
                boolean flood = pck.ETHtrg.isFloodable();
                if (flood) {
                    pck.ETHtrg.setAddr(bcmac);
                } else {
                    pck.ETHtrg.setAddr(per.bbmac);
                }
                ifcEther.createETHheader(pck, false);
                pck.msbPutD(0, 0);
                pck.putSkip(4);
                pck.merge2beg();
                ipMpls.beginMPLSfields(pck, false);
                if (flood) {
                    pck.MPLSlabel = per.labMul;
                } else {
                    pck.MPLSlabel = per.labUni;
                }
                ipMpls.createMPLSheader(pck);
                parent.fwdCore.mplsTxPack(per.peer, pck, false);
                break;
            case vxlan:
                break;
            case cmac:
                flood = pck.ETHtrg.isFloodable();
                ifcEther.createETHheader(pck, false);
                ipMpls.beginMPLSfields(pck, false);
                if (flood) {
                    pck.MPLSlabel = per.labMul;
                } else {
                    pck.MPLSlabel = per.labUni;
                }
                if (pck.MPLSlabel == 0) {
                    break;
                }
                ipMpls.createMPLSheader(pck);
                parent.fwdCore.mplsTxPack(per.peer, pck, false);
                break;
        }
    }

    /**
     * get peer list
     *
     * @param tab list to append
     */
    public void getPeerList(tabRoute<addrIP> tab) {
        for (int i = 0; i < peers.size(); i++) {
            rtrBgpEvpnPeer nei = peers.get(i);
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = new addrPrefix<addrIP>(nei.peer, addrIP.size * 8);
            tabRoute.addUpdatedEntry(tabRoute.addType.better, tab, rtrBgpUtil.safiUnicast, ntry, null, null, parent.routerAutoMesh);
        }
    }

}
