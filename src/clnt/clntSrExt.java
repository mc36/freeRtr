package clnt;

import addr.addrEmpty;
import addr.addrIP;
import addr.addrType;
import ifc.ifcDn;
import ifc.ifcNull;
import ifc.ifcUp;
import ip.ipCorSrh;
import ip.ipFwd;
import java.util.ArrayList;
import java.util.List;
import pack.packHolder;
import util.cmds;
import util.counter;
import util.state;

/**
 * sr over srh tunnel client
 *
 * @author matecsaba
 */
public class clntSrExt implements ifcDn {

    /**
     * upper layer
     */
    public ifcUp upper = new ifcNull();

    /**
     * forwarder
     */
    public ipFwd fwdCor;

    /**
     * target
     */
    public addrIP target;

    /**
     * tos value, -1 means maps out
     */
    public int tos = -1;

    /**
     * ttl value
     */
    public int ttl = 255;

    /**
     * counter
     */
    public counter cntr = new counter();

    private addrIP[] targets = new addrIP[1];

    public String toString() {
        return "srext to " + target;
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
     * send packet
     *
     * @param pck packet
     */
    public void sendPack(packHolder pck) {
        pck.getSkip(2);
        cntr.tx(pck);
        if (ttl >= 0) {
            pck.IPttl = ttl;
        }
        if (tos >= 0) {
            pck.IPtos = tos;
        }
        ipCorSrh.createHeader(pck, targets);
        fwdCor.updateIPheader(pck, pck.IPsrc, pck.IPtrg, pck.IPprt, pck.IPttl, pck.IPtos, pck.dataSize() - pck.IPsiz);
        fwdCor.mplsTxPack(pck.IPtrg, pck, false);
    }

    /**
     * set targets
     *
     * @param s targets
     */
    public void setTargets(String s) {
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
        addrIP[] ts = new addrIP[trg.size() + 1];
        for (int i = 0; i < (ts.length - 1); i++) {
            ts[i] = trg.get(i).copyBytes();
        }
        ts[ts.length - 1] = target.copyBytes();
        targets = ts;
    }

    /**
     * get targets
     *
     * @return targets
     */
    public String getTargets() {
        String s = "";
        for (int i = 0; i < (targets.length - 1); i++) {
            s += " " + targets[i];
        }
        return s.trim();
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

}
