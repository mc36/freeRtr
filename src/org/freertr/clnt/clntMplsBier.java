package org.freertr.clnt;

import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrType;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcUp;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdBier;
import org.freertr.pack.packHolder;
import org.freertr.tab.tabGen;
import org.freertr.util.cmds;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.notifier;
import org.freertr.util.state;

/**
 * mpls bier tunnel client
 *
 * @author matecsaba
 */
public class clntMplsBier implements Runnable, ifcDn {

    /**
     * create instance
     */
    public clntMplsBier() {
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
     * source id
     */
    public int srcId = 0;

    /**
     * experimental value, -1 means maps out
     */
    public int expr = -1;

    /**
     * entropy value, -1 means maps out
     */
    public int entr = -1;

    /**
     * marking value, -1 means maps out
     */
    public int mark = -1;

    /**
     * ttl value
     */
    public int ttl = 255;

    /**
     * counter
     */
    public counter cntr = new counter();

    private boolean working = false;

    private ipFwdBier bier;

    private tabGen<addrIP> targets = new tabGen<addrIP>();

    private notifier notif1 = new notifier();

    private notifier notif2 = new notifier();

    public String toString() {
        return "bier to " + getTargets();
    }

    /**
     * get hw address
     *
     * @return address
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
     * set upper
     *
     * @param server upper
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
        cntr.tx(pck);
        if (expr >= 0) {
            pck.MPLSexp = expr;
        }
        if (entr > 0) {
            pck.MPLSntr = entr;
        }
        if (mark > 0) {
            pck.MPLSmrk = mark;
        }
        if (ttl >= 0) {
            pck.MPLSttl = ttl;
        }
        pck.ETHtype = pck.msbGetW(0);
        pck.getSkip(2);
        bier.sendPack(pck);
    }

    /**
     * set targets
     *
     * @param s targets
     */
    public void setTargets(String s) {
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
            targets.add(a);
        }
    }

    /**
     * get targets
     *
     * @return targets
     */
    public String getTargets() {
        String s = "";
        for (int i = 0; i < targets.size(); i++) {
            s += " " + targets.get(i);
        }
        return s.trim();
    }

    /**
     * start connection
     */
    public void workStart() {
        if (debugger.clntMplsBierTraf) {
            logger.debug("starting work");
        }
        working = true;
        bier = new ipFwdBier(srcId);
        for (int i = 0; i < targets.size(); i++) {
            bier.addPeer(fwdCor, targets.get(i), 0, -1);
        }
        new Thread(this).start();
    }

    /**
     * wait until setup complete
     *
     * @param tim time to wait
     */
    public void wait4setup(int tim) {
        notif2.misleep(tim);
    }

    /**
     * stop connection
     */
    public void workStop() {
        if (debugger.clntMplsBierTraf) {
            logger.debug("stop work");
        }
        working = false;
        notif1.wakeup();
    }

    public void run() {
        for (;;) {
            if (!working) {
                break;
            }
            try {
                bier.updatePeers();
            } catch (Exception e) {
                logger.traceback(e);
            }
            notif1.sleep(10000);
        }
    }

}
