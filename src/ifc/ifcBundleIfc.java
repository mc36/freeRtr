package ifc;

import java.util.Comparator;
import pack.packHolder;
import util.counter;
import util.debugger;
import util.logger;
import util.state;

/**
 * bundle interface handler
 *
 * @author matecsaba
 */
public class ifcBundleIfc implements ifcUp, Comparator<ifcBundleIfc> {

    /**
     * bundling interface number
     */
    public int ifcNum;

    /**
     * bundling interface priority
     */
    public int priority;

    /**
     * last state of this interface
     */
    public state.states stated = state.states.up;

    /**
     * bundle handler
     */
    public ifcBundle lowerBu;

    /**
     * interface handler, null means uninitialized
     */
    public ifcDn lowerIf;

    /**
     * bytes will sent in interval
     */
    public int byteNeed;

    /**
     * bytes received in interval
     */
    public int byteRcvd;

    /**
     * bytes reported in average
     */
    public int byteAvrg;

    /**
     * bytes reported in intervals
     */
    public int[] byteRprt;

    /**
     * bytes reported next in buffer
     */
    public int byteNext;

    /**
     * last time packet arrived
     */
    public long lastRx;

    private counter cntr = new counter();

    /**
     * get counter
     *
     * @return counter
     */
    public counter getCounter() {
        return cntr;
    }

    /**
     * set state
     *
     * @param stat state
     */
    public void setState(state.states stat) {
        stated = state.toUsable(stat);
        cntr.stateChange(stated);
        lowerBu.propagateState();
    }

    /**
     * creatws new interface
     *
     * @param parent interface handler
     */
    public ifcBundleIfc(ifcBundle parent) {
        lowerBu = parent;
    }

    /**
     * close interface
     */
    public void closeUp() {
        lowerBu.delIface(ifcNum);
    }

    /**
     * set parent
     *
     * @param parent parent
     */
    public void setParent(ifcDn parent) {
        lowerIf = parent;
        parent.setFilter(lowerBu.promiscous);
        lowerBu.propagateState();
    }

    /**
     * received packet
     *
     * @param pck packet
     */
    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        if (debugger.ifcBundleTraf) {
            logger.debug("rx " + ifcNum);
        }
        if (stated != state.states.up) {
            cntr.drop(pck, counter.reasons.notUp);
            return;
        }
        pck.putStart();
        byteRcvd += pck.dataSize();
        lowerBu.send2upper(this, pck);
    }

    /**
     * send one packet over this interface
     *
     * @param pck packet to send
     */
    protected void doTxPack(packHolder pck) {
        cntr.tx(pck);
        if (debugger.ifcBundleTraf) {
            logger.debug("tx " + ifcNum);
        }
        if (stated != state.states.up) {
            cntr.drop(pck, counter.reasons.notUp);
            return;
        }
        if (lowerIf == null) {
            cntr.drop(pck, counter.reasons.notInTab);
            return;
        }
        lowerIf.sendPack(pck);
    }

    /**
     * get show output
     *
     * @return line
     */
    public String getShow() {
        return lowerIf + "|" + state.conv2string(stated) + "|" + byteNeed + "|" + byteAvrg + "|" + priority;
    }

    public String toString() {
        return "" + lowerIf;
    }

    public int compare(ifcBundleIfc v1, ifcBundleIfc v2) {
        if (v1.ifcNum < v2.ifcNum) {
            return -1;
        }
        if (v1.ifcNum > v2.ifcNum) {
            return +1;
        }
        return 0;
    }

}
