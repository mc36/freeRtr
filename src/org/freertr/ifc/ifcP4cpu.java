package org.freertr.ifc;

import org.freertr.pack.packHolder;
import org.freertr.util.debugger;
import org.freertr.util.logger;

/**
 * p4lang cpuport mux
 *
 * @author matecsaba
 */
public class ifcP4cpu extends ifcVlan {

    /**
     * size of header
     */
    public final static int size = 4;

    /**
     * parse header
     *
     * @param pck packet to parse
     * @return false on success, true on error
     */
    public boolean parseHeader(packHolder pck) {
        ifcEther.createETHheader(pck, false);
        pck.ETHvlan = pck.msbGetD(0) & 0x7fffffff;
        pck.getSkip(size);
        ifcEther.parseETHheader(pck, false);
        if (debugger.ifcP4cpuTraf) {
            logger.debug("rx vlan=" + pck.ETHvlan);
        }
        return false;
    }

    /**
     * create header
     *
     * @param pck packet to update
     */
    public void createHeader(packHolder pck) {
        if (debugger.ifcP4cpuTraf) {
            logger.debug("tx vlan=" + pck.ETHvlan);
        }
        pck.merge2beg();
        ifcEther.createETHheader(pck, false);
        pck.msbPutD(0, pck.ETHvlan);
        pck.putSkip(size);
        pck.merge2beg();
        ifcEther.parseETHheader(pck, false);
    }

    /**
     * convert to string
     *
     * @return string
     */
    public String toString() {
        return "p4cpu on " + lower;
    }

    /**
     * register ethertype
     *
     * @param ethtyp handler
     */
    public void reg2ethTyp(ifcEthTyp ethtyp) {
        cntr.dropper = ethtyp.getCounter();
        ethtyp.addET(-1, "p4cpu", this);
        ethtyp.updateET(-1, this);
    }

    /**
     * unregister ethertype
     *
     * @param ethtyp handler
     */
    public void unreg2ethTyp(ifcEthTyp ethtyp) {
        vLans.clear();
        ethtyp.delET(-1);
    }

    /**
     * create new multiplexer
     */
    public ifcP4cpu() {
        if (debugger.ifcP4cpuTraf) {
            logger.debug("started");
        }
    }

    /**
     * get size of mtu
     *
     * @return mtu size
     */
    public int remainingMtu() {
        return lower.getMTUsize() - size;
    }

}
