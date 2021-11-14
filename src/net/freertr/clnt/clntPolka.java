package net.freertr.clnt;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import net.freertr.addr.addrEmpty;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrType;
import net.freertr.cry.cryPoly;
import net.freertr.ifc.ifcDn;
import net.freertr.ifc.ifcNull;
import net.freertr.ifc.ifcPolka;
import net.freertr.ifc.ifcUp;
import net.freertr.ip.ipFwd;
import net.freertr.ip.ipFwdIface;
import net.freertr.ip.ipFwdTab;
import net.freertr.ip.ipMpls;
import net.freertr.pack.packHolder;
import net.freertr.tab.tabLabel;
import net.freertr.tab.tabRouteEntry;
import net.freertr.user.userFormat;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.counter;
import net.freertr.util.debugger;
import net.freertr.util.logger;
import net.freertr.util.notifier;
import net.freertr.util.state;

/**
 * polka tunnel client
 *
 * @author matecsaba
 */
public class clntPolka implements Runnable, ifcDn {

    /**
     * create instance
     */
    public clntPolka() {
    }

    /**
     * upper layer
     */
    public ifcUp upper = new ifcNull();

    /**
     * forwarder
     */
    public ipFwd fwdCor;

    /**
     * source interface
     */
    public ipFwdIface fwdIfc = null;

    /**
     * target
     */
    public addrIP target;

    /**
     * ttl value
     */
    public int ttl = 255;

    /**
     * verify encoding
     */
    public boolean verify = false;

    /**
     * counter
     */
    public counter cntr = new counter();

    private addrIP[] targets = new addrIP[1];

    private addrIP nextHop = new addrIP();

    private ipFwdIface nextIfc;

    private byte[] routeid = null;

    private boolean working = false;

    private notifier notif = new notifier();

    public String toString() {
        return "polka to " + target;
    }

    /**
     * get hw address
     *
     * @return hw address
     */
    public addrType getHwAddr() {
        return new addrEmpty();
    }

    /**
     * set filter
     *
     * @param promisc promiscous mode
     */
    public void setFilter(boolean promisc) {
    }

    /**
     * get state
     *
     * @return state
     */
    public state.states getState() {
        if (routeid == null) {
            return state.states.down;
        } else {
            return state.states.up;
        }
    }

    /**
     * close interface
     */
    public void closeDn() {
        clearState();
    }

    /**
     * flap interface
     */
    public void flapped() {
        clearState();
    }

    /**
     * set upper layer
     *
     * @param server upper layer
     */
    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
    }

    /**
     * get counter
     *
     * @return counter
     */
    public counter getCounter() {
        return cntr;
    }

    /**
     * get mtu size
     *
     * @return mtu size
     */
    public int getMTUsize() {
        return 1500;
    }

    /**
     * get bandwidth
     *
     * @return bandwidth
     */
    public long getBandwidth() {
        return 8000000;
    }

    /**
     * send packet
     *
     * @param pck packet
     */
    public void sendPack(packHolder pck) {
        if (routeid == null) {
            return;
        }
        pck.NSHmdv = routeid;
        pck.IPprt = pck.msbGetW(0);
        pck.getSkip(2);
        cntr.tx(pck);
        if (ttl >= 0) {
            pck.NSHttl = ttl;
        } else {
            pck.NSHttl = pck.IPttl;
        }
        nextIfc.lower.sendPolka(pck, nextHop);
    }

    /**
     * get resulting route
     *
     * @param src source to use
     * @return route, null if no suitable
     */
    public tabRouteEntry<addrIP> getResultRoute(tabRouteEntry<addrIP> src) {
        if (routeid == null) {
            return null;
        }
        src.best.nextHop = nextHop.copyBytes();
        src.best.iface = nextIfc;
        src.best.labelRem = tabLabel.int2labels(ipMpls.labelImp);
        src.best.attribVal = routeid;
        return src;
    }

    /**
     * set targets
     *
     * @param s targets
     */
    public void setTargets(String s) {
        if (s == null) {
            s = "";
        }
        List<addrIP> trgs = new ArrayList<addrIP>();
        cmds c = new cmds("adrs", s);
        for (;;) {
            s = c.word();
            if (s.length() < 1) {
                break;
            }
            addrIP a = new addrIP();
            if (a.fromString(s)) {
                continue;
            }
            trgs.add(a);
        }
        setTargets(trgs);
    }

    /**
     * set targets
     *
     * @param trg targets
     */
    public void setTargets(List<addrIP> trg) {
        clearState();
        addrIP[] ts = new addrIP[trg.size()];
        for (int i = 0; i < ts.length; i++) {
            ts[i] = trg.get(i).copyBytes();
        }
        routeid = null;
        targets = ts;
        notif.wakeup();
    }

    /**
     * get targets
     *
     * @return targets
     */
    public String getTargets() {
        if (targets.length < 1) {
            return null;
        }
        String s = "";
        for (int i = 0; i < targets.length; i++) {
            s += " " + targets[i];
        }
        return s.trim();
    }

    /**
     * start connection
     */
    public void workStart() {
        if (debugger.clntPolkaTraf) {
            logger.debug("starting work");
        }
        working = true;
        new Thread(this).start();
    }

    /**
     * stop connection
     */
    public void workStop() {
        if (debugger.clntPolkaTraf) {
            logger.debug("stopping work");
        }
        working = false;
        clearState();
    }

    public void run() {
        for (;;) {
            if (!working) {
                break;
            }
            try {
                workDoer();
            } catch (Exception e) {
                logger.traceback(e);
            }
            notif.sleep(5000);
        }
    }

    private void clearState() {
        routeid = null;
        upper.setState(state.states.down);
    }

    private void workDoer() {
        ipFwdIface ifc = fwdIfc;
        if (ifc == null) {
            ifc = ipFwdTab.findSendingIface(fwdCor, target);
        }
        if (ifc == null) {
            return;
        }
        int[] ids = new int[targets.length + 1];
        tabRouteEntry<addrIP> prev = fwdCor.actualU.route(target);
        if (prev == null) {
            if (debugger.clntPolkaTraf) {
                logger.debug("no route for " + target);
            }
            clearState();
            return;
        }
        for (int i = targets.length - 1; i >= 0; i--) {
            tabRouteEntry<addrIP> ntry = fwdCor.actualU.route(targets[i]);
            if (ntry == null) {
                if (debugger.clntPolkaTraf) {
                    logger.debug("no route for " + targets[i]);
                }
                clearState();
                return;
            }
            if (prev.best.segrouIdx < 1) {
                if (debugger.clntPolkaTraf) {
                    logger.debug("no index for " + prev);
                }
                clearState();
                return;
            }
            ids[i + 1] = prev.best.segrouIdx;
            prev = ntry;
        }
        ids[0] = prev.best.segrouIdx;
        nextHop = prev.best.nextHop.copyBytes();
        nextIfc = (ipFwdIface) prev.best.iface;
        ifcPolka plk = nextIfc.lower.getPolka();
        if (plk == null) {
            if (debugger.clntPolkaTraf) {
                logger.debug("polka not enabled for " + nextIfc);
            }
            clearState();
            return;
        }
        try {
            routeid = ifcPolka.encodeRouteId(plk.coeffs, ids);
        } catch (Exception e) {
            if (debugger.clntPolkaTraf) {
                logger.debug("error encoding routeid for " + target);
            }
            return;
        }
        upper.setState(state.states.up);
        if (!verify) {
            return;
        }
        try {
            doOneVerify(ids, ifcPolka.decodeRouteIdPoly(plk.coeffs, routeid), "poly");
            doOneVerify(ids, ifcPolka.decodeRouteIdCrc(plk.coeffs, routeid), "crc");
        } catch (Exception e) {
            if (debugger.clntPolkaTraf) {
                logger.debug("error decoding routeid for " + target);
            }
            return;
        }
    }

    private void doOneVerify(int[] ids, int[] dec, String mod) {
        boolean good = true;
        for (int i = 0; i < ids.length; i++) {
            if (dec[ids[i]] != ifcPolka.routeNextValue(ids, i)) {
                good = false;
            }
        }
        if (good) {
            return;
        }
        if (debugger.clntPolkaTraf) {
            logger.debug("bad routeid for " + target + " with " + mod);
        }

    }

    /**
     * get routeid show
     *
     * @return show
     */
    public userFormat getShRoute() {
        if (routeid == null) {
            return null;
        }
        userFormat l = new userFormat("|", "mode|routeid");
        l.add("hex|" + bits.byteDump(routeid, 0, -1));
        l.add("poly|" + new cryPoly(new BigInteger(routeid)));
        return l;
    }

    /**
     * get decoded show
     *
     * @return show
     */
    public userFormat getShDecode() {
        if (routeid == null) {
            return null;
        }
        userFormat l = new userFormat("|", "index|coeff|poly|crc|equal");
        ifcPolka plk = nextIfc.lower.getPolka();
        int[] pol = ifcPolka.decodeRouteIdPoly(plk.coeffs, routeid);
        int[] crc = ifcPolka.decodeRouteIdCrc(plk.coeffs, routeid);
        for (int i = 0; i < plk.coeffs.length; i++) {
            l.add(i + "|" + bits.toHexD(plk.coeffs[i].intCoeff()) + "|" + pol[i] + "|" + crc[i] + "|" + (pol[i] == crc[i]));
        }
        return l;
    }

}
