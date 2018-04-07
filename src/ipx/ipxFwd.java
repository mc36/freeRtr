package ipx;

import addr.addrIpx;
import addr.addrPrefix;
import ifc.ifcEthTyp;
import java.util.List;
import pack.packHolder;
import tab.tabGen;
import tab.tabRoute;
import tab.tabRouteEntry;
import util.cmds;
import util.counter;
import util.logger;
import util.notifier;
import util.state;

/**
 * does internetwork packet exchange forwarding
 *
 * @author matecsaba
 */
public class ipxFwd implements Runnable {

    /**
     * size of header
     */
    public static final int size = 30;

    /**
     * name of routing table
     */
    public final String vrfName;

    /**
     * list of current interfaces
     */
    public final tabGen<ipxIface> ifaces;

    /**
     * the configured static route table
     */
    public final tabGen<tabRouteEntry<addrIpx>> staticR;

    /**
     * the computed connected table
     */
    public tabRoute<addrIpx> connedR;

    /**
     * the computed routing table
     */
    public tabRoute<addrIpx> actualR;

    /**
     * time when recompute automatically
     */
    public int untriggeredRecomputation = 240 * 1000;

    private notifier triggerUpdate;

    private int nextIfaceNumber = 10;

    /**
     * the constructor of vrf
     *
     * @param nam name of this vrf
     */
    public ipxFwd(String nam) {
        vrfName = nam;
        ifaces = new tabGen<ipxIface>();
        staticR = new tabGen<tabRouteEntry<addrIpx>>();
        triggerUpdate = new notifier();
        updateEverything();
        new Thread(this).start();
    }

    /**
     * stop this routing table completely
     */
    public void stopThisVrf() {
        untriggeredRecomputation = -1;
        triggerUpdate.wakeup();
        for (int i = ifaces.size() - 1; i >= 0; i--) {
            ipxIface ifc = ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            ifaceDel(ifc);
        }
    }

    /**
     * get running configuration
     *
     * @param l list to append
     */
    public void getShRun(List<String> l) {
        for (int i = 0; i < staticR.size(); i++) {
            tabRouteEntry<addrIpx> prf = staticR.get(i);
            l.add("ipx route " + vrfName + " " + prf.prefix.network + " " + prf.prefix.mask + " " + prf.nextHop);
        }
    }

    private void updateEverything() {
        tabRoute<addrIpx> tabC = new tabRoute<addrIpx>("connected");
        tabC.defDist = 0;
        tabC.defMetr = 0;
        tabC.defRouTyp = tabRouteEntry.routeType.conn;
        tabRoute<addrIpx> tabA = new tabRoute<addrIpx>("locals");
        tabA.defDist = 0;
        tabA.defMetr = 1;
        tabA.defRouTyp = tabRouteEntry.routeType.local;
        for (int i = 0; i < ifaces.size(); i++) {
            ipxIface ifc = ifaces.get(i);
            if (!ifc.ready) {
                continue;
            }
            tabRouteEntry<addrIpx> prf = tabC.add(tabRoute.addType.always, ifc.network, null);
            prf.iface = ifc;
            prf = tabA.add(tabRoute.addType.always, new addrPrefix<addrIpx>(ifc.addr, ifc.addr.maxBits()), null);
            prf.iface = ifc;
            prf.rouTyp = tabRouteEntry.routeType.local;
        }
        tabA.mergeFrom(tabRoute.addType.better, tabC, null, true, tabRouteEntry.distanLim);
        for (int i = 0; i < staticR.size(); i++) {
            tabRouteEntry<addrIpx> ntry = staticR.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.distance >= tabRouteEntry.distanMax) {
                continue;
            }
            tabRouteEntry<addrIpx> nh = tabC.route(ntry.nextHop);
            if (nh == null) {
                continue;
            }
            tabRouteEntry<addrIpx> imp = ntry.copyBytes();
            imp.iface = nh.iface;
            tabA.add(tabRoute.addType.better, imp, false, true);
        }
        connedR = tabC;
        actualR = tabA;
    }

    public void run() {
        try {
            for (;;) {
                if (triggerUpdate.misleep(untriggeredRecomputation) > 0) {
                    logger.debug("too fast table updates");
                }
                if (untriggeredRecomputation <= 0) {
                    break;
                }
                updateEverything();
            }
            untriggeredRecomputation -= 1;
        } catch (Exception e) {
            logger.exception(e);
        }
    }

    /**
     * del static route
     *
     * @param rou route
     */
    public void staticDel(tabRouteEntry<addrIpx> rou) {
        rou = staticR.del(rou);
        triggerUpdate.wakeup();
    }

    /**
     * add static route
     *
     * @param rou route
     */
    public void staticAdd(tabRouteEntry<addrIpx> rou) {
        staticR.add(rou);
        triggerUpdate.wakeup();
    }

    /**
     * parse static route
     *
     * @param cmd command to read
     * @return parsed route entry
     */
    public static tabRouteEntry<addrIpx> staticParse(cmds cmd) {
        tabRouteEntry<addrIpx> ntry = new tabRouteEntry<addrIpx>();
        addrIpx adr = new addrIpx();
        if (adr.fromString(cmd.word())) {
            return null;
        }
        addrIpx msk = new addrIpx();
        if (msk.fromString(cmd.word())) {
            return null;
        }
        ntry.prefix = new addrPrefix<addrIpx>(adr, msk.toNetmask());
        if (adr.fromString(cmd.word())) {
            return null;
        }
        ntry.nextHop = adr;
        return ntry;
    }

    /**
     * add one interface
     *
     * @param lower interface to add
     * @return interface handler
     */
    public ipxIface ifaceAdd(ifcEthTyp lower) {
        ipxIface ntry;
        for (;;) {
            nextIfaceNumber = ((nextIfaceNumber + 1) & 0x3fffffff);
            ntry = new ipxIface(nextIfaceNumber + 10000, this, lower);
            ntry.addr = new addrIpx();
            ntry.network = new addrPrefix<addrIpx>(ntry.addr, ntry.addr.maxBits());
            ntry.ready = true;
            if (ifaces.add(ntry) == null) {
                break;
            }
        }
        triggerUpdate.wakeup();
        return ntry;
    }

    /**
     * delete one interface
     *
     * @param ifc interface handler
     */
    public void ifaceDel(ipxIface ifc) {
        ifc = ifaces.del(ifc);
        if (ifc == null) {
            return;
        }
        ifc.ready = false;
        triggerUpdate.wakeup();
    }

    /**
     * change interface state
     *
     * @param ifc interface handler
     * @param stat new status of interface
     */
    public void ifaceState(ipxIface ifc, state.states stat) {
        ifc.cntr.stateChange(stat);
        boolean st = (stat == state.states.up);
        if (ifc.ready == st) {
            return;
        }
        ifc.ready = st;
        triggerUpdate.wakeup();
    }

    /**
     * change interface address
     *
     * @param ifc interface handler
     * @param addr new address
     */
    public void ifaceAddr(ipxIface ifc, addrIpx addr) {
        ifc.addr = addr.copyBytes();
        ifc.network = new addrPrefix<addrIpx>(ifc.addr, 32);
        triggerUpdate.wakeup();
    }

    /**
     * parse one packet
     *
     * @param pck packet to process
     * @param src source address
     * @param dst target address
     * @return true on error, false on success
     */
    public static boolean parseIPXheader(packHolder pck, addrIpx src, addrIpx dst) {
        int len = pck.msbGetW(2); // packet length
        if (pck.dataSize() < len) {
            return true;
        }
        pck.setDataSize(len);
        pck.IPttl = pck.getByte(4); // transport control
        pck.IPprt = pck.getByte(5); // packet type
        pck.getAddr(dst, 6); // destination
        pck.UDPtrg = pck.msbGetW(16); // dst port
        pck.getAddr(src, 18); // source
        pck.UDPsrc = pck.msbGetW(28); // src port
        pck.IPsiz = size;
        return false;
    }

    /**
     * update one packet
     *
     * @param pck packet to process
     * @param ttl time to live
     */
    public static void updateIPXheader(packHolder pck, int ttl) {
        pck.unMergeBytes(size);
        pck.putSkip(-size);
        if (ttl != -1) {
            pck.putByte(4, ttl); // time to live
            pck.IPttl = ttl;
        }
        pck.putSkip(size);
        pck.mergeHeader(-1, pck.headSize() - size);
    }

    /**
     * interface signals that it got a packet
     *
     * @param lower interface handler
     * @param pck packet to process
     */
    public void ifacePack(ipxIface lower, packHolder pck) {
        if (lower == null) {
            return;
        }
        if (!lower.ready) {
            lower.cntr.drop(pck, counter.reasons.notUp);
            return;
        }
        pck.putStart();
        addrIpx dst = new addrIpx();
        addrIpx src = new addrIpx();
        if (parseIPXheader(pck, src, dst)) {
            lower.cntr.drop(pck, counter.reasons.badHdr);
            return;
        }
        pck.INTiface = lower.ifwNum;
        pck.INTupper = 0;
        int net = dst.getNet();
        if ((net == 0) || (net == -1)) {
            return;
        }
        tabRouteEntry<addrIpx> prf = actualR.route(dst);
        if (prf == null) {
            return;
        }
        if (prf.iface == null) {
            return;
        }
        ipxIface txIfc = (ipxIface) prf.iface;
        if (pck.INTiface == txIfc.ifwNum) {
            return;
        }
        if (pck.IPttl >= 16) {
            return;
        }
        updateIPXheader(pck, pck.IPttl + 1);
        if (prf.rouTyp == tabRouteEntry.routeType.conn) {
            pck.ETHtrg.setAddr(dst.getMac());
        } else {
            pck.ETHtrg.setAddr(prf.nextHop.getMac());
        }
        pck.ETHsrc.setAddr(txIfc.hwaddr);
        txIfc.sendPack(pck);
    }

}
