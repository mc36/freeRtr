package net.freertr.clnt;

import net.freertr.addr.addrEmpty;
import net.freertr.addr.addrType;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgIfc;
import net.freertr.ifc.ifcDn;
import net.freertr.ifc.ifcEthTyp;
import net.freertr.ifc.ifcNull;
import net.freertr.ifc.ifcUp;
import net.freertr.pack.packHolder;
import net.freertr.tab.tabIntMatcher;
import net.freertr.util.cmds;
import net.freertr.util.counter;
import net.freertr.util.state;

/**
 * exp bundle
 *
 * @author matecsaba
 */
public class clntMplsExp implements ifcDn {

    /**
     * create instance
     */
    public clntMplsExp() {
    }

    /**
     * upper layer
     */
    public ifcUp upper = new ifcNull();

    /**
     * experimental value, -1 means maps out
     */
    public int expr = -1;

    /**
     * entrp[y value, -1 means maps out
     */
    public int entr = -1;

    /**
     * ttl value
     */
    public int ttl = 255;

    /**
     * counter
     */
    public counter cntr = new counter();

    private final ifcEthTyp[] lowers = new ifcEthTyp[8];

    private final cfgIfc[] ifaces = new cfgIfc[8];

    public String toString() {
        return "expbundle over " + getTargets();
    }

    /**
     * send packet
     *
     * @param pck packet
     */
    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        ifcEthTyp ntry = lowers[pck.MPLSexp & 7];
        if (ntry == null) {
            cntr.drop(pck, counter.reasons.noIface);
            return;
        }
        if (expr >= 0) {
            pck.MPLSexp = expr;
        }
        if (entr > 0) {
            pck.MPLSrnd = entr;
        }
        if (ttl >= 0) {
            pck.MPLSttl = ttl;
        }
        ntry.doTxPack(pck);
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
        return state.states.up;
    }

    /**
     * close interface
     */
    public void closeDn() {
    }

    /**
     * flap interface
     */
    public void flapped() {
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
     * start connection
     */
    public void workStart() {
    }

    /**
     * stop connection
     */
    public void workStop() {
    }

    /**
     * set targets
     *
     * @param trgs targets
     */
    public void setTargets(String trgs) {
        for (int i = 0; i < lowers.length; i++) {
            lowers[i] = null;
        }
        cmds cmd = new cmds("trg", trgs);
        for (;;) {
            tabIntMatcher mtch = new tabIntMatcher();
            String a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            int i = a.indexOf(":");
            if (i < 0) {
                continue;
            }
            String b = a.substring(i + 1, a.length());
            a = a.substring(0, i);
            if (mtch.fromString(a)) {
                continue;
            }
            cfgIfc iface = cfgAll.ifcFind(b, 0);
            if (iface == null) {
                continue;
            }
            mtch.rangeMax++;
            for (i = mtch.rangeMin; i < mtch.rangeMax; i++) {
                ifaces[i & 7] = iface;
                lowers[i & 7] = iface.ethtyp;
            }
        }
    }

    /**
     * get targets
     *
     * @return targets
     */
    public String getTargets() {
        String a = "";
        for (int i = 0; i < lowers.length; i++) {
            if (lowers[i] == null) {
                continue;
            }
            a += " " + i + ":" + lowers[i];
        }
        return a.trim();
    }

}
