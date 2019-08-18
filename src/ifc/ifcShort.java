package ifc;

import pack.packHolder;
import util.counter;
import util.state;

/**
 * loop back all traffic
 *
 * @author matecsaba
 */
public class ifcShort implements ifcUp {

    private ifcDn lower = new ifcNull();

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
     * close this interface
     */
    public void closeUp() {
    }

    /**
     * set state of interface
     *
     * @param stat new state
     */
    public void setState(state.states stat) {
        cntr.stateChange(stat);
    }

    /**
     * set worker interface
     *
     * @param parent worker interface
     */
    public void setParent(ifcDn parent) {
        lower = parent;
    }

    /**
     * this interface got a packet for processing
     *
     * @param pck packet needs to parsed
     */
    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        pck.putStart();
        byte[] hwa = lower.getHwAddr().getBytes();
        pck.putFill(0, hwa.length, 0xff);
        pck.putCopy(hwa, 0, hwa.length, hwa.length);
        pck.putSkip(hwa.length * 2);
        cntr.tx(pck);
        lower.sendPack(pck);
    }

    public String toString() {
        return "loop on " + lower;
    }

}
