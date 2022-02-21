package net.freertr.clnt;

import java.util.Comparator;
import net.freertr.addr.addrEmpty;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrType;
import net.freertr.ifc.ifcDn;
import net.freertr.ifc.ifcMpolka;
import net.freertr.ifc.ifcNull;
import net.freertr.ifc.ifcPolka;
import net.freertr.ifc.ifcUp;
import net.freertr.ip.ipFwd;
import net.freertr.ip.ipFwdIface;
import net.freertr.ip.ipMpls;
import net.freertr.pack.packHolder;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabIndex;
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
 * mpolka tunnel client
 *
 * @author matecsaba
 */
public class clntMpolka implements Runnable, ifcDn {

    /**
     * create instance
     */
    public clntMpolka() {
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

    private clntMpolkaTrg[] targets = new clntMpolkaTrg[1];

    private clntMpolkaOut[] outputs = null;

    private boolean working = false;

    private notifier notif = new notifier();

    public String toString() {
        return "mpolka";
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
        if (outputs == null) {
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
        if (outputs == null) {
            return;
        }
        pck.IPprt = pck.msbGetW(0);
        pck.getSkip(2);
        cntr.tx(pck);
        if (ttl >= 0) {
            pck.NSHttl = ttl;
        } else {
            pck.NSHttl = pck.IPttl;
        }
        for (int i = 0; i < outputs.length; i++) {
            pck.NSHmdv = outputs[i].rou;
            outputs[i].ifc.lower.sendMpolka(pck.copyBytes(true, true), outputs[i].hop);
        }
    }

    /**
     * get resulting route
     *
     * @param src source to use
     * @return route, null if no suitable
     */
    public tabRouteEntry<addrIP> getResultRoute(tabRouteEntry<addrIP> src) {
        if (outputs == null) {
            return null;
        }
        if (outputs.length < 1) {
            return null;
        }
        src.best.nextHop = outputs[0].hop.copyBytes();
        src.best.iface = outputs[0].ifc;
        src.best.labelRem = tabLabel.int2labels(ipMpls.labelImp);
        src.best.attribVal = outputs[0].rou;
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
        tabGen<clntMpolkaTrg> trgs = new tabGen<clntMpolkaTrg>();
        cmds c = new cmds("adrs", s);
        for (;;) {
            s = c.word();
            if (s.length() < 1) {
                break;
            }
            clntMpolkaTrg ntry = new clntMpolkaTrg();
            ntry.node = new addrIP();
            ntry.node.fromString(s);
            for (;;) {
                s = c.word();
                if (s.length() < 1) {
                    break;
                }
                if (s.equals(",")) {
                    break;
                }
                addrIP a = new addrIP();
                if (s.equals("-")) {
                    if (a.fromString(c.word())) {
                        continue;
                    }
                    ntry.through = a;
                    continue;
                }
                if (a.fromString(s)) {
                    continue;
                }
                ntry.peers.add(a);
            }
            trgs.add(ntry);
        }
        setTargets(trgs);
    }

    /**
     * set targets
     *
     * @param trg targets
     */
    public void setTargets(tabGen<clntMpolkaTrg> trg) {
        clearState();
        clntMpolkaTrg[] ts = new clntMpolkaTrg[trg.size()];
        for (int i = 0; i < ts.length; i++) {
            ts[i] = trg.get(i);
        }
        outputs = null;
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
        if (debugger.clntMpolkaTraf) {
            logger.debug("starting work");
        }
        working = true;
        new Thread(this).start();
    }

    /**
     * stop connection
     */
    public void workStop() {
        if (debugger.clntMpolkaTraf) {
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
        outputs = null;
        upper.setState(state.states.down);
    }

    private void workDoer() {
        tabGen<clntMpolkaOut> outs = new tabGen<clntMpolkaOut>();
        for (int o = 0; o < targets.length; o++) {
            addrIP adr = targets[o].node;
            tabRouteEntry<addrIP> rou = fwdCor.actualU.route(adr);
            if (rou == null) {
                if (debugger.clntMpolkaTraf) {
                    logger.debug("no route for " + adr);
                }
                continue;
            }
            if (rou.best.segrouIdx < 1) {
                if (debugger.clntMpolkaTraf) {
                    logger.debug("no index for " + rou);
                }
                continue;
            }
            tabIndex<addrIP> idx = fwdCor.actualIU.find(new tabIndex<addrIP>(rou.best.segrouIdx, null));
            if (idx == null) {
                if (debugger.clntMpolkaTraf) {
                    logger.debug("no srindex for " + rou);
                }
                continue;
            }
            if (idx.neighs == null) {
                if (debugger.clntMpolkaTraf) {
                    logger.debug("no srneigh for " + rou);
                }
                continue;
            }
            if (targets[o].through != null) {
                adr = targets[o].through;
                rou = fwdCor.actualU.route(adr);
                if (rou == null) {
                    if (debugger.clntMpolkaTraf) {
                        logger.debug("no route for " + adr);
                    }
                    continue;
                }
            }
            clntMpolkaOut ntry = new clntMpolkaOut(rou.best.nextHop.copyBytes());
            ntry.ifc = (ipFwdIface) rou.best.iface;
            ntry.plk = ntry.ifc.lower.getMpolka();
            if (ntry.plk == null) {
                if (debugger.clntMpolkaTraf) {
                    logger.debug("polka not enabled for " + ntry.ifc);
                }
                continue;
            }
            int neis = 0;
            for (int i = 0; i < targets[o].peers.size(); i++) {
                adr = targets[o].peers.get(i);
                if (adr.compare(adr, targets[o].node) == 0) {
                    neis |= 1;
                    continue;
                }
                rou = fwdCor.actualU.route(adr);
                if (rou == null) {
                    if (debugger.clntMpolkaTraf) {
                        logger.debug("no route for " + adr);
                    }
                    continue;
                }
                if (rou.best.segrouIdx < 1) {
                    if (debugger.clntMpolkaTraf) {
                        logger.debug("no index for " + rou);
                    }
                    continue;
                }
                int p = idx.neighs.index(new tabIndex<addrIP>(rou.best.segrouIdx, null));
                if (p < 0) {
                    if (debugger.clntMpolkaTraf) {
                        logger.debug("no neighbor for " + rou);
                    }
                    continue;
                }
                p++;
                neis |= 1 << p;
            }
            if (neis == 0) {
                if (debugger.clntMpolkaTraf) {
                    logger.debug("no outputs for " + targets[o]);
                }
                continue;
            }
            idx = new tabIndex<addrIP>(idx.index, null);
            idx.bitmap = neis;
            ntry.nei.add(idx);
            ntry = outs.add(ntry);
            if (ntry == null) {
                continue;
            }
            ntry.nei.add(idx);
        }
        if (outs.size() < 1) {
            if (debugger.clntMpolkaTraf) {
                logger.debug("no outputs");
            }
            clearState();
            return;
        }
        clntMpolkaOut[] out = new clntMpolkaOut[outs.size()];
        for (int i = 0; i < out.length; i++) {
            out[i] = outs.get(i);
            try {
                out[i].rou = ifcMpolka.encodeRouteId(out[i].plk.coeffs, out[i].nei);
            } catch (Exception e) {
                if (debugger.clntMpolkaTraf) {
                    logger.debug("error encoding routeid for " + out[i].hop);
                }
                clearState();
                return;
            }
        }
        outputs = out;
        upper.setState(state.states.up);
        if (!verify) {
            return;
        }
        for (int i = 0; i < out.length; i++) {
            try {
                doOneVerify(out[i].nei, ifcPolka.decodeRouteIdPoly(out[i].plk.coeffs, out[i].rou), "poly");
                doOneVerify(out[i].nei, ifcPolka.decodeRouteIdCrc(out[i].plk.coeffs, out[i].rou), "crc");
            } catch (Exception e) {
                if (debugger.clntMpolkaTraf) {
                    logger.debug("error decoding routeid for " + out[i].hop);
                }
                return;
            }
        }
    }

    private void doOneVerify(tabGen<tabIndex<addrIP>> ids, int[] dec, String mod) {
        boolean good = true;
        for (int i = 0; i < ids.size(); i++) {
            tabIndex<addrIP> c = ids.get(i);
            if (dec[c.index] != c.bitmap) {
                good = false;
            }
        }
        if (good) {
            return;
        }
        if (debugger.clntMpolkaTraf) {
            logger.debug("bad routeid with " + mod);
        }
    }

    /**
     * get routeid show
     *
     * @return show
     */
    public userFormat getShRoute() {
        if (outputs == null) {
            return null;
        }
        userFormat l = new userFormat("|", "iface|hop|routeid");
        for (int i = 0; i < outputs.length; i++) {
            l.add(outputs[i].ifc + "|" + outputs[i].hop + "|" + bits.byteDump(outputs[i].rou, 0, -1));
        }
        return l;
    }

    /**
     * get decoded show
     *
     * @return show
     */
    public userFormat getShDecode() {
        if (outputs == null) {
            return null;
        }
        userFormat l = new userFormat("|", "index|coeff|poly|crc|equal");
        for (int o = 0; o < outputs.length; o++) {
            ifcMpolka plk = outputs[o].ifc.lower.getMpolka();
            int[] pol = ifcPolka.decodeRouteIdPoly(plk.coeffs, outputs[o].rou);
            int[] crc = ifcPolka.decodeRouteIdCrc(plk.coeffs, outputs[o].rou);
            for (int i = 0; i < plk.coeffs.length; i++) {
                l.add(i + "|" + bits.toHexD(plk.coeffs[i].intCoeff()) + "|" + pol[i] + "|" + crc[i] + "|" + (pol[i] == crc[i]));
            }
        }
        return l;
    }

}

class clntMpolkaTrg implements Comparator<clntMpolkaTrg> {

    public addrIP node;

    public addrIP through;

    public tabGen<addrIP> peers = new tabGen<addrIP>();

    public String toString() {
        String a = "" + node;
        if (through != null) {
            a += " - " + through;
        }
        for (int i = 0; i < peers.size(); i++) {
            a += " " + peers.get(i);
        }
        return a + " ,";
    }

    public int compare(clntMpolkaTrg o1, clntMpolkaTrg o2) {
        int i = o1.node.compare(o1.node, o2.node);
        if (i != 0) {
            return i;
        }
        if ((o1.through == null) && (o2.through == null)) {
            return 0;
        }
        if (o1.through == null) {
            return -1;
        }
        if (o2.through == null) {
            return +1;
        }
        return o1.through.compare(o1.through, o2.through);
    }

}

class clntMpolkaOut implements Comparator<clntMpolkaOut> {

    public final addrIP hop;

    public ipFwdIface ifc;

    public ifcMpolka plk;

    public tabGen<tabIndex<addrIP>> nei;

    public byte[] rou;

    public clntMpolkaOut(addrIP h) {
        hop = h;
        nei = new tabGen<tabIndex<addrIP>>();
    }

    public int compare(clntMpolkaOut o1, clntMpolkaOut o2) {
        return o1.hop.compare(o1.hop, o2.hop);
    }

}
