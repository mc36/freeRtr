package net.freertr.ifc;

import net.freertr.pack.packHolder;
import net.freertr.util.debugger;
import net.freertr.util.logger;

/**
 * qinq2 protocol
 *
 * @author matecsaba
 */
public class ifcQinq2 extends ifcVlan {

    /**
     * ethertype of these packets
     */
    public final static int type = 0x9200;

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
        if (pck.msbGetW(0) != type) {
            return true;
        }
        int i = pck.msbGetW(2); // vlan tag
        pck.ETHvlan = i & 0xfff; // id
        pck.ETHcos = (i >>> 13) & 7; // cos
        pck.getSkip(size);
        if (debugger.ifcQinq2Traf) {
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
        if (debugger.ifcQinq2Traf) {
            logger.debug("tx vlan=" + pck.ETHvlan);
        }
        pck.msbPutW(0, type); // ether type
        pck.msbPutW(2, (pck.ETHvlan & 0xfff) | ((pck.ETHcos & 7) << 13)); // vlan tag
        pck.putSkip(size);
        pck.merge2beg();
    }

    /**
     * convert to string
     *
     * @return string
     */
    public String toString() {
        return "qinq2 on " + lower;
    }

    /**
     * register ethertype
     *
     * @param ethtyp handler
     */
    public void reg2ethTyp(ifcEthTyp ethtyp) {
        cntr.dropper = ethtyp.getCounter();
        ethtyp.addET(type, "qinq2", this);
        ethtyp.updateET(type, this);
    }

    /**
     * unregister ethertype
     *
     * @param ethtyp handler
     */
    public void unreg2ethTyp(ifcEthTyp ethtyp) {
        vLans.clear();
        ethtyp.delET(type);
    }

    /**
     * create new multiplexer
     */
    public ifcQinq2() {
        if (debugger.ifcQinq2Traf) {
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
