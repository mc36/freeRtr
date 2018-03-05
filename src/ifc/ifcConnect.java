package ifc;

import pack.packHolder;
import util.counter;
import util.state;

/**
 * connect two interfaces
 *
 * @author matecsaba
 */
public class ifcConnect {

    private ifcConnectWorker s1;

    private ifcConnectWorker s2;

    /**
     * create new instance
     */
    public ifcConnect() {
        s1 = new ifcConnectWorker();
        s2 = new ifcConnectWorker();
        s1.other = s2;
        s2.other = s1;
    }

    /**
     * get side one
     *
     * @return interface handler
     */
    public ifcUp getSide1() {
        return s1;
    }

    /**
     * get side two
     *
     * @return interface handler
     */
    public ifcUp getSide2() {
        return s2;
    }

    /**
     * set filter
     *
     * @param prom pormiscous
     */
    public void setPromiscous(boolean prom) {
        s1.lower.setFilter(prom);
        s2.lower.setFilter(prom);
    }

}

class ifcConnectWorker implements ifcUp {

    public ifcDn lower = new ifcNull();

    public ifcConnectWorker other;

    public counter cntr = new counter();

    public void setParent(ifcDn parent) {
        lower = parent;
    }

    public void setState(state.states stat) {
    }

    public void closeUp() {
    }

    public counter getCounter() {
        return cntr;
    }

    public void recvPack(packHolder pck) {
        other.lower.sendPack(pck);
    }

}
