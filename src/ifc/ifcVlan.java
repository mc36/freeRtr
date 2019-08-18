package ifc;

import addr.addrType;
import pack.packHolder;
import util.counter;
import util.state;

/**
 * vlan trunk handler
 *
 * @author matecsaba
 */
public abstract class ifcVlan implements ifcUp {

    /**
     * state of lower
     */
    protected state.states lastState = state.states.up;

    /**
     * lower layer
     */
    protected ifcDn lower = new ifcNull();

    /**
     * promiscous mode set
     */
    protected boolean promiscous = false;

    /**
     * packet counter
     */
    protected counter cntr = new counter();

    /**
     * get counter
     *
     * @return counter
     */
    public counter getCounter() {
        return cntr;
    }

    /**
     * set parent
     *
     * @param parent parent
     */
    public void setParent(ifcDn parent) {
        lower = parent;
    }

    /**
     * register to ethertype
     *
     * @param ethtyp where to register
     */
    public abstract void reg2ethTyp(ifcEthTyp ethtyp);

    /**
     * unregister from ethertype
     *
     * @param ethtyp where from register
     */
    public abstract void unreg2ethTyp(ifcEthTyp ethtyp);

    /**
     * add one vlan handler
     *
     * @param vl vlan id
     * @param ifc interface handler that should be notified when packet arrives
     * @return vlan handler
     */
    public abstract ifcDn addVlan(int vl, ifcUp ifc);

    /**
     * update one vlan handler
     *
     * @param vl vlan id
     * @param ifc interface handler that should be notified when packet arrives
     * @return vlan handler
     */
    public abstract ifcDn updateVlan(int vl, ifcUp ifc);

    /**
     * delete vlan handler
     *
     * @param vl vlan id
     * @return true if error happened
     */
    public abstract ifcUp delVlan(int vl);

    /**
     * get hardware address
     *
     * @return hw address
     */
    protected addrType vlnHwAddr() {
        return lower.getHwAddr();
    }

    /**
     * get state of interface
     *
     * @return state of line protocol
     */
    protected state.states vlnState() {
        return lastState;
    }

    /**
     * get bandwidth
     *
     * @return bandwidth
     */
    protected long vlnBandwidth() {
        return lower.getBandwidth();
    }

    /**
     * send one packet to the network
     *
     * @param pck packet to send
     */
    protected void vlnTxPack(packHolder pck) {
        cntr.tx(pck);
        if (lastState != state.states.up) {
            cntr.drop(pck, counter.reasons.notUp);
            return;
        }
        lower.sendPack(pck);
    }

}
