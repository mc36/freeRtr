package org.freertr.rtr;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrPrefix;
import org.freertr.addr.addrType;
import org.freertr.cfg.cfgBrdg;
import org.freertr.cfg.cfgIfc;
import org.freertr.clnt.clntMplsPwe;
import org.freertr.ifc.ifcBridgeIfc;
import org.freertr.pack.packLdpPwe;
import org.freertr.tab.tabRoute;
import org.freertr.tab.tabRouteEntry;
import org.freertr.tab.tabRouteUtil;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.debugger;
import org.freertr.util.logger;

/**
 * bgp4 mspw
 *
 * @author matecsaba
 */
public class rtrBgpMspw implements Comparable<rtrBgpMspw> {

    /**
     * id number
     */
    public long id;

    /**
     * bridge to use
     */
    public cfgBrdg bridge;

    /**
     * source interface
     */
    public cfgIfc iface;

    /**
     * control word
     */
    public boolean ctrlWrd;

    /**
     * set true if advertised
     */
    public boolean adverted;

    /**
     * neighbor address
     */
    public addrIP remAdr;

    /**
     * neighbor address
     */
    public long remId;

    /**
     * aggregate at, 0=none, 1=node, 2=global
     */
    public int aggr;

    private final rtrBgp parent;

    /**
     * pwe client
     */
    protected clntMplsPwe clnt;

    /**
     * bridge interface
     */
    protected ifcBridgeIfc brdg;

    private addrIP last = null;

    /**
     * create new instance
     *
     * @param p parent to use
     */
    public rtrBgpMspw(rtrBgp p) {
        parent = p;
        aggr = 1;
    }

    public int compareTo(rtrBgpMspw o) {
        if (id < o.id) {
            return -1;
        }
        if (id > o.id) {
            return +1;
        }
        return 0;
    }

    /**
     * generate configuration
     *
     * @param l list to append
     * @param beg1 beginning
     */
    public void getConfig(List<String> l, String beg1) {
        String beg2 = beg1 + "afi-mspw " + tabRouteUtil.rd2string(id) + " ";
        l.add(beg2 + "bridge-group " + bridge.number);
        if (remAdr != null) {
            l.add(beg2 + "remote " + remAdr + " " + tabRouteUtil.rd2string(remId));
        }
        if (ctrlWrd) {
            l.add(beg2 + "control-word");
        }
        String a = null;
        switch (aggr) {
            case 0:
                a = "none";
                break;
            case 1:
                a = "acid";
                break;
            case 2:
                a = "prefix";
                break;
            default:
                a = "unknown=" + aggr;
                break;
        }
        if (aggr != 1) {
            l.add(beg2 + "partial " + a);
        }
        if (iface != null) {
            l.add(beg2 + "update-source " + iface.name);
        }
        l.add(beg1 + cmds.comment);
    }

    /**
     * advertise this mspw
     *
     * @param freshly currently computing
     */
    protected void doAdvertise(tabRoute<addrIP>[] freshly) {
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
        if (remAdr == null) {
            return;
        }
        addrType src = null;
        if (remAdr.isIPv4()) {
            src = iface.addr4;
        } else {
            src = iface.addr6;
        }
        if (src == null) {
            return;
        }
        byte[] buf = createAddr(src, id, aggr);
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        ntry.best.extComm = new ArrayList<Long>();
        ntry.prefix = new addrPrefix<addrIP>(new addrIP(), buf.length * 8);
        ntry.prefix.network.fromBuf(buf, 0);
        ntry.prefix.broadcast.fromBuf(buf, 16);
        ntry.best.extComm.add(tabRouteUtil.rt2comm(bridge.bridgeHed.rtExp));
        ntry.best.rouSrc = rtrBgpUtil.peerOriginate;
        ntry.rouDst = bridge.bridgeHed.rd;
        freshly[rtrBgpParam.idxMspw].add(tabRoute.addType.better, ntry, true, true);
        ntry = new tabRouteEntry<addrIP>();
        ntry.prefix = tabRouteUtil.extcomm2rtfilter(parent.localAs, tabRouteUtil.rt2comm(bridge.bridgeHed.rtImp));
        ntry.best.rouSrc = rtrBgpUtil.peerOriginate;
        freshly[rtrBgpParam.idxRtf].add(tabRoute.addType.always, ntry, false, true);
        adverted = true;
    }

    private static byte[] createAddr(addrType src, long id, int agr) {
        byte[] buf = new byte[addrIP.size * 2];
        int len = 1;
        bits.msbPutD(buf, len, (int) (id >>> 32)); // global
        len += 4;
        if (agr < 2) {
            src.toBuffer(buf, len);
            len += src.getSize();
        }
        if (agr < 1) {
            bits.msbPutD(buf, len, (int) id); // ac id
            len += 4;
        }
        buf[0] = (byte) (len - 1);
        return buf;
    }

    /**
     * read peers in this mspw
     */
    protected void doPeers() {
        if (remAdr == null) {
            doStop();
            return;
        }
        if (iface == null) {
            doStop();
            return;
        }
        if (bridge == null) {
            doStop();
            return;
        }
        byte[] ned;
        if (remAdr.isIPv4()) {
            ned = createAddr(remAdr.toIPv4(), remId, 0);
        } else {
            ned = createAddr(remAdr.toIPv6(), remId, 0);
        }
        byte[] got = new byte[ned.length];
        addrIP per = null;
        int len = 0;
        for (int i = 0; i < parent.computd[rtrBgpParam.idxMspw].size(); i++) {
            tabRouteEntry<addrIP> ntry = parent.computd[rtrBgpParam.idxMspw].get(i);
            if (ntry.best.rouSrc == rtrBgpUtil.peerOriginate) {
                continue;
            }
            ntry.prefix.network.toBuffer(got, 0);
            ntry.prefix.broadcast.toBuffer(got, 16);
            if (got[0] > ned[0]) {
                continue;
            }
            if (bits.byteComp(ned, 1, got, 1, got[0]) != 0) {
                continue;
            }
            if (len > got[0]) {
                continue;
            }
            len = got[0];
            per = ntry.best.nextHop.copyBytes();
        }
        if (per == null) {
            doStop();
            return;
        }
        if (last != null) {
            if (last.compareTo(per) == 0) {
                return;
            }
        }
        doStop();
        if (debugger.rtrBgpEvnt) {
            logger.debug("start " + per);
        }
        last = per;
        clnt = new clntMplsPwe();
        clnt.pwType = packLdpPwe.pwtEthPort;
        clnt.pwMtu = bridge.bridgeHed.getMTUsize();
        clnt.target = "" + per;
        clnt.vrf = parent.vrfCore;
        clnt.srcIfc = iface;
        clnt.vcid = 0;
        clnt.srcI = id;
        clnt.trgI = remId;
        clnt.ctrlWrd = ctrlWrd;
        clnt.general = true;
        clnt.descr = null;
        brdg = bridge.bridgeHed.newIface(false, true, false);
        clnt.setUpper(brdg);
        clnt.workStart();
    }

    /**
     * stop this mspw
     */
    public void doStop() {
        if (clnt == null) {
            return;
        }
        if (debugger.rtrBgpEvnt) {
            logger.debug("stop " + tabRouteUtil.rd2string(id) + " " + last);
        }
        clnt.workStop();
        brdg.closeUp();
        clnt = null;
        brdg = null;
        last = null;
    }

    /**
     * get peer list
     *
     * @param tab list to append
     */
    public void getPeerList(tabRoute<addrIP> tab) {
        if (clnt == null) {
            return;
        }
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        ntry.prefix = new addrPrefix<addrIP>(last, addrIP.size * 8);
        tabRoute.addUpdatedEntry(tabRoute.addType.better, tab, rtrBgpUtil.sfiUnicast, 0, ntry, true, null, null, parent.routerAutoMesh);
    }

}
