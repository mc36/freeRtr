package ifc;

import pack.packHolder;
import util.counter;
import util.state;

/**
 * incoming packet handler header
 *
 * @author matecsaba
 */
public interface ifcUp {

    /**
     * this interface got a packet for processing
     *
     * @param pck packet needs to parsed
     */
    public void recvPack(packHolder pck);

    /**
     * set worker interface
     *
     * @param parent worker interface
     */
    public void setParent(ifcDn parent);

    /**
     * set state of interface
     *
     * @param stat new state
     */
    public void setState(state.states stat);

    /**
     * close this interface
     */
    public void closeUp();

    /**
     * get interface counter
     *
     * @return the counter
     */
    public counter getCounter();

}
